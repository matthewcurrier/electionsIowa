# R/parse_2020_general.R
#
# Parsing helpers for 2020 Iowa General Election county xlsx files.
# Each county is stored as a separate xlsx where every numbered sheet
# represents one contest. Sheet layout (1-indexed rows):
#
#   Row 1 : Office / contest name  (col 1 only)
#   Row 2 : Candidate names        (col 3, 6, 9, … — every 3rd from col 3)
#   Row 3 : Sub-headers            ("Registered Voters", then per-candidate
#                                   "Election Day" / "Absentee" / "Total Votes")
#   Row 4 : First precinct data row
#   Row n : "Total:" summary row   ← EXCLUDED from output
#
# The county name is derived from the xlsx filename (without extension).

library(readxl)
library(dplyr)
library(purrr)
library(tibble)
library(fs)

SKIP_SHEETS_2020 <- c("Table of Contents", "Registered Voters")

# ── Sheet-level parser ─────────────────────────────────────────────────────────

#' Parse one contest sheet into long format.
#'
#' @param raw   A data frame read with `col_names = FALSE` (no column names).
#' @param county Character. County label derived from the source filename.
#'
#' @return A tibble with columns:
#'   precinct, candidate, value, vote_type, office,
#'   election_type, election_year, county
parse_sheet_2020 <- function(raw, county) {
  office <- as.character(raw[[1, 1]])

  # Row 2 holds candidate names; non-NA entries at col index >= 3 are candidates.
  row2 <- unlist(raw[2, ])
  candidate_start_cols <- which(!is.na(row2) & seq_along(row2) >= 3)
  candidates <- as.character(row2[candidate_start_cols])

  # Rows 4 through (n - 1): skip the 3-row header block and the final Total row.
  data_rows <- raw[4:(nrow(raw) - 1), ]
  precincts <- as.character(data_rows[[1]])

  map2_dfr(candidates, candidate_start_cols, function(candidate, start_col) {
    bind_rows(
      tibble(
        precinct = precincts,
        candidate = candidate,
        value = as.integer(data_rows[[start_col]]),
        vote_type = "Election Day",
        office = office,
        election_type = "General",
        election_year = 2020L,
        county = county
      ),
      tibble(
        precinct = precincts,
        candidate = candidate,
        value = as.integer(data_rows[[start_col + 1L]]),
        vote_type = "Absentee",
        office = office,
        election_type = "General",
        election_year = 2020L,
        county = county
      )
    )
  })
}

# ── File-level parser ──────────────────────────────────────────────────────────

#' Parse all contest sheets in one county xlsx file.
#'
#' @param file_path Path to a county `.xlsx` file.
#'
#' @return A long-format tibble for every contest in the county, with the
#'   same schema as `parse_sheet_2020()`.
parse_county_file_2020 <- function(file_path) {
  county <- path_ext_remove(path_file(file_path))
  all_sheets <- excel_sheets(file_path)
  contest_sheets <- setdiff(all_sheets, SKIP_SHEETS_2020)

  map_dfr(contest_sheets, function(sheet) {
    raw <- read_excel(file_path, sheet = sheet, col_names = FALSE)
    parse_sheet_2020(raw, county)
  })
}
