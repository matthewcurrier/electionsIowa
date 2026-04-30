# R/parse_scott_2020_pdf.R
#
# Parsing helpers for the Scott County 2020 General Election PDF.
# The SOS published this county as a PDF rather than an xlsx. pdfplumber
# (Python) is used for table extraction via {reticulate}; all subsequent
# transformation is pure R / purrr.
#
# PDF layout (consistent across all 50 contests, all 199 pages):
#
#   • Contest-start pages contain "SCOTT COUNTY ELECTION CANVASS SUMMARY"
#     followed by "General Election - 2020" followed by the contest name.
#   • Continuation pages repeat only the candidate header row.
#   • Every page yields exactly one pdfplumber table.
#
#   Table header row:
#     [empty, empty, cand1, cand2, …, "Write-in", "Undervotes", "Overvotes", "Total"]
#     The last THREE columns (Undervotes, Overvotes, Total) are ALWAYS excluded.
#     Everything from col 3 onward up to that boundary is a candidate / response.
#
#   Data rows:
#     [precinct_or_empty, vote_type, val1, val2, …, undervotes, overvotes, total]
#     When col 1 is empty the precinct name carries forward from the previous row.
#     Rows where vote_type == "Total" or precinct == "Total" are summary rows → DROPPED.

library(reticulate)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)

# ── pdfplumber import ──────────────────────────────────────────────────────────

# Lazily cached module reference — import once per session.
.pdfplumber_module <- NULL

#' Report which Python reticulate is using and whether pdfplumber is available.
#'
#' Call this once at the top of an interactive session if anything looks
#' wrong. It prints the Python binary path, version, and package status
#' without modifying any state.
check_python_env <- function() {
  message("Python binary : ", reticulate::py_config()$python)
  message("Python version: ", reticulate::py_config()$version)

  has_pkg <- reticulate::py_module_available("pdfplumber")
  message("pdfplumber    : ", if (has_pkg) "installed" else "NOT FOUND")

  invisible(has_pkg)
}

#' Import pdfplumber via reticulate, installing it automatically if absent.
#'
#' On first call the function checks whether pdfplumber is importable.
#' If not, it installs it into the active Python environment with
#' `reticulate::py_install()` before importing.  Subsequent calls in the
#' same session return the cached module reference immediately.
#'
#' If auto-install fails (e.g. no network, locked environment) the error
#' from reticulate is surfaced with an additional hint to run
#' `check_python_env()` for diagnostics.
import_pdfplumber <- function() {
  if (!is.null(.pdfplumber_module)) {
    return(invisible(.pdfplumber_module))
  }

  if (!reticulate::py_module_available("pdfplumber")) {
    message(
      "pdfplumber not found — installing into the active Python environment…"
    )
    tryCatch(
      reticulate::py_install("pdfplumber", pip = TRUE),
      error = function(e) {
        stop(
          "Auto-install of pdfplumber failed: ",
          conditionMessage(e),
          "\n\n",
          "Diagnostics:\n",
          "  reticulate::py_config()        — shows which Python is active\n",
          "  check_python_env()             — pdfplumber availability check\n",
          "  reticulate::use_python(...)    — switch Python if needed\n",
          "  reticulate::py_install('pdfplumber', pip = TRUE)  — manual install\n",
          call. = FALSE
        )
      }
    )
  }

  .pdfplumber_module <<- reticulate::import("pdfplumber", delay_load = FALSE)
  invisible(.pdfplumber_module)
}

# ── Pure-R transformation helpers (fully unit-testable without reticulate) ─────

#' Normalise a raw candidate name that may contain embedded newlines.
#'
#' pdfplumber joins multi-line cell text with "\n". We collapse those
#' and squish all internal whitespace to a single space.
#' "Write-\nin" → "Write-in"; "Joseph\nR. Biden…" → "Joseph R. Biden…"
#'
#' @param name Character scalar.
#' @return Cleaned character scalar.
normalise_candidate_name <- function(name) {
  name |>
    str_replace_all("\n", " ") |>
    str_replace_all("-\\s+", "-") |> # re-join hyphenated line-breaks
    str_squish()
}

#' Extract candidate names from a pdfplumber header row (character vector).
#'
#' The header is always structured as:
#'   [empty, empty, cand…, "Undervotes", "Overvotes", "Total"]
#' so we take columns 3 through (n - 3) inclusive (1-indexed).
#'
#' @param header_row Character vector, the first row of a pdfplumber table.
#' @return Character vector of normalised candidate names.
extract_candidates_from_header <- function(header_row) {
  n <- length(header_row)
  header_row[seq(3L, n - 3L)] |> map_chr(normalise_candidate_name)
}

#' Fill down a precinct-name vector, carrying non-empty values forward.
#'
#' Uses `purrr::accumulate` to propagate the most recent non-blank
#' precinct label through rows where col 1 is empty (continuation rows).
#'
#' Wide contest tables (e.g. President) wrap long precinct names across
#' multiple lines inside the cell, so pdfplumber joins them with "\n".
#' We normalise those before the fill-down so every stored value is a
#' clean single-line string.
#'
#' @param raw_col Character vector; may contain NA or empty strings.
#' @return Character vector the same length as `raw_col`, no NAs or blanks.
fill_down_precincts <- function(raw_col) {
  cleaned <- raw_col |>
    as.character() |>
    replace_na("") |>
    str_replace_all("\n", " ") |>
    str_squish()

  accumulate(cleaned, function(prev, curr) {
    if (nchar(curr) > 0L) curr else prev
  })
}

#' Parse all data rows from one contest's pdfplumber tables into long format.
#'
#' This function is intentionally free of PDF / reticulate calls so it can
#' be unit-tested with synthetic fixtures.
#'
#' @param rows     List of character vectors. The first element is the header
#'                 row; subsequent elements are data rows.
#' @param office   Contest / office title (character scalar).
#' @param county   County label. Defaults to "scott".
#'
#' @return A tibble with columns:
#'   precinct, candidate, value, vote_type, office,
#'   election_type, election_year, county
parse_contest_rows <- function(rows, office, county = "scott") {
  if (length(rows) < 2L) {
    return(tibble())
  }

  header <- as.character(rows[[1]])
  candidates <- extract_candidates_from_header(header)
  n_cols <- length(header)

  # Candidate value columns: same range used for header extraction (1-indexed)
  cand_idx <- seq(3L, n_cols - 3L)

  # Bind all data rows into a matrix for vectorised processing
  data_rows <- rows[-1L] |> map(as.character)

  precinct_raw <- map_chr(data_rows, 1L)
  vote_type_raw <- map_chr(data_rows, 2L)

  # Carry-forward precinct names
  precinct_filled <- fill_down_precincts(precinct_raw)

  # Normalise vote type (may contain "\n" on wide-table pages)
  vote_type_clean <- vote_type_raw |>
    replace_na("") |>
    str_replace_all("\n", " ") |>
    str_squish()

  # Keep only Election Day and Absentee rows;
  # drop Total subtotals and the grand-total row (precinct == "Total")
  keep <- (vote_type_clean %in% c("Election Day", "Absentee")) &
    (precinct_filled != "Total")

  if (!any(keep)) {
    return(tibble())
  }

  kept_precincts <- precinct_filled[keep]
  kept_vote_types <- vote_type_clean[keep]
  kept_rows <- data_rows[keep]

  # Pivot: one row per (precinct × candidate × vote_type)
  map2_dfr(
    seq_along(kept_rows),
    kept_rows,
    function(i, row) {
      raw_vals <- row[cand_idx]
      parsed <- as.integer(str_remove_all(replace_na(raw_vals, "0"), ","))

      tibble(
        precinct = kept_precincts[[i]],
        candidate = candidates,
        value = parsed,
        vote_type = kept_vote_types[[i]],
        office = office,
        election_type = "General",
        election_year = 2020L,
        county = county
      )
    }
  )
}

# ── Contest-boundary detection ─────────────────────────────────────────────────

#' Identify which pages start a new contest and extract the office name.
#'
#' A contest-start page contains "SCOTT COUNTY ELECTION CANVASS SUMMARY".
#' The office name is the text on the line immediately after
#' "General Election - 2020".
#'
#' @param page_texts Character vector, one element per PDF page.
#' @return A tibble with columns `page_index` (1-based) and `office`.
find_contest_starts <- function(page_texts) {
  is_start <- str_detect(
    page_texts,
    fixed("SCOTT COUNTY ELECTION CANVASS SUMMARY")
  )

  keep_pages <- which(is_start)

  tibble(
    page_index = keep_pages,
    office = map_chr(page_texts[keep_pages], function(txt) {
      m <- str_match(txt, "General Election - 2020\n(.+)")
      if (is.na(m[1, 1])) NA_character_ else str_squish(m[1, 2])
    })
  )
}

#' Assign every page to a contest by filling down contest starts.
#'
#' @param n_pages     Total page count (integer).
#' @param starts      Tibble from `find_contest_starts()`.
#' @return A tibble with columns `page_index` and `office` for every page.
assign_pages_to_contests <- function(n_pages, starts) {
  all_pages <- tibble(page_index = seq_len(n_pages))

  all_pages |>
    left_join(starts, by = "page_index") |>
    mutate(
      office = accumulate(office, function(prev, curr) {
        if (!is.na(curr)) curr else prev
      })
    ) |>
    filter(!is.na(office))
}

# ── Top-level entry point ──────────────────────────────────────────────────────

#' Parse the Scott County 2020 General Election PDF into long format.
#'
#' @param pdf_path Path to `scott.pdf`.
#' @return A long-format tibble with the same schema produced by
#'   `parse_county_file_2020()` in `parse_2020_general.R`.
parse_scott_pdf <- function(pdf_path) {
  pb <- import_pdfplumber()

  pdf <- pb$open(pdf_path)
  on.exit(pdf$close(), add = TRUE)

  pages <- pdf$pages
  n_pages <- length(pages)

  message("  Scott County PDF: ", n_pages, " pages")

  # Extract text for every page (used only for contest-boundary detection)
  page_texts <- map_chr(pages, function(p) p$extract_text() %||% "")

  # Extract pdfplumber tables for every page (list of list-of-rows)
  message("  Extracting tables from all pages…")
  page_tables <- map(pages, function(p) {
    tbls <- p$extract_tables()
    if (length(tbls) == 0L) NULL else tbls[[1L]] # always 1 table per page
  })

  # Map pages to contests
  starts <- find_contest_starts(page_texts)
  page_contest <- assign_pages_to_contests(n_pages, starts)

  # Group pages by contest, preserving contest order
  contest_groups <- page_contest |>
    group_by(office) |>
    group_split()

  message("  Parsing ", length(contest_groups), " contests…")

  map_dfr(contest_groups, function(grp) {
    office <- grp$office[[1L]]
    page_idx <- grp$page_index # 1-based

    # Collect all rows from every page for this contest.
    # The header repeats on continuation pages — keep only the FIRST occurrence
    # as canonical; for subsequent pages skip their header row.
    all_rows <- imap(page_idx, function(pg, i) {
      tbl <- page_tables[[pg]]
      if (is.null(tbl)) {
        return(NULL)
      }
      if (i == 1L) tbl else tbl[-1L] # strip repeated header on continuations
    }) |>
      compact() |>
      list_flatten(name_spec = "{inner}")

    parse_contest_rows(all_rows, office = office)
  })
}
