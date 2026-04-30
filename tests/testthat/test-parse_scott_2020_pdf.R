# tests/testthat/test-parse_scott_2020_pdf.R
#
# Unit tests for the pure-R helpers in R/parse_scott_2020_pdf.R.
# All tests use synthetic fixtures — no PDF or reticulate required.
#
# Run with: testthat::test_file("tests/testthat/test-parse_scott_2020_pdf.R")

library(testthat)
library(dplyr)
library(tibble)
library(purrr)
library(stringr)

source(here::here("R", "parse_scott_2020_pdf.R"))

# ── Fixtures ───────────────────────────────────────────────────────────────────

# Mimics a pdfplumber header row from a wide contest (e.g. President).
# pdfplumber joins multi-line cell text with "\n".
make_header <- function() {
  list(
    "", "",
    "Joseph\nR. Biden\nand Kamala\nD. Harris,\nDEM",
    "Donald\nJ. Trump\nand Michael\nR. Pence,\nREP",
    "Write-\nin",
    "Undervotes", "Overvotes", "Total"
  )
}

# Full table fixture: header + 2 precincts x 3 row-types.
# Total subtotal rows and the grand-total row must be dropped by the parser.
make_table_rows <- function() {
  list(
    make_header(),
    list("(P1) Alpha Township", "Election Day", "100", "200", "5", "3", "0", "308"),
    list("",                    "Absentee",     "50",  "80",  "2", "1", "0", "133"),
    list("",                    "Total",        "150", "280", "7", "4", "0", "441"),
    list("(P2) Beta City",      "Election Day", "30",  "70",  "0", "5", "0", "105"),
    list("",                    "Absentee",     "20",  "40",  "1", "2", "0",  "63"),
    list("",                    "Total",        "50",  "110", "1", "7", "0", "168"),
    list("Total",               NULL,           "200", "390", "8", "11","0", "609")
  )
}

# ── normalise_candidate_name ───────────────────────────────────────────────────

test_that("normalise_candidate_name collapses embedded newlines", {
  expect_equal(
    normalise_candidate_name("Joseph\nR. Biden\nand Kamala"),
    "Joseph R. Biden and Kamala"
  )
})

test_that("normalise_candidate_name repairs hyphenated line-breaks", {
  expect_equal(normalise_candidate_name("Write-\nin"), "Write-in")
})

test_that("normalise_candidate_name squishes extra whitespace", {
  expect_equal(normalise_candidate_name("  Some   Name  "), "Some Name")
})

# ── extract_candidates_from_header ────────────────────────────────────────────

test_that("extract_candidates_from_header excludes the last 3 sentinel columns", {
  result <- extract_candidates_from_header(as.character(make_header()))
  expect_equal(length(result), 3L)
})

test_that("extract_candidates_from_header normalises the Write-in hyphen break", {
  result <- extract_candidates_from_header(as.character(make_header()))
  expect_equal(result[[3]], "Write-in")
})

test_that("extract_candidates_from_header normalises multi-line candidate names", {
  result <- extract_candidates_from_header(as.character(make_header()))
  expect_true(str_starts(result[[1]], "Joseph R. Biden"))
})

# ── fill_down_precincts ────────────────────────────────────────────────────────

test_that("fill_down_precincts carries non-empty values forward", {
  result <- fill_down_precincts(c("Alpha", "", "", "Beta", ""))
  expect_equal(result, c("Alpha", "Alpha", "Alpha", "Beta", "Beta"))
})

test_that("fill_down_precincts handles NA inputs", {
  result <- fill_down_precincts(c(NA, "Gamma", NA))
  expect_equal(result, c("", "Gamma", "Gamma"))
})

test_that("fill_down_precincts normalises embedded newlines in wide-table precinct names", {
  result <- fill_down_precincts(c("(AG) Allen's\nGrove\nTownship", "", ""))
  expect_equal(result, rep("(AG) Allen's Grove Township", 3L))
})

# ── parse_contest_rows ─────────────────────────────────────────────────────────

test_that("parse_contest_rows returns the expected column names", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  expect_named(
    result,
    c("precinct", "candidate", "value", "vote_type",
      "office", "election_type", "election_year", "county")
  )
})

test_that("parse_contest_rows drops Total subtotal rows", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  expect_false("Total" %in% result$vote_type)
})

test_that("parse_contest_rows drops the grand-total precinct row", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  expect_false("Total" %in% result$precinct)
})

test_that("parse_contest_rows produces only Election Day and Absentee vote types", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  expect_setequal(unique(result$vote_type), c("Election Day", "Absentee"))
})

test_that("parse_contest_rows detects all candidates correctly", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  expect_setequal(
    unique(result$candidate),
    c("Joseph R. Biden and Kamala D. Harris, DEM",
      "Donald J. Trump and Michael R. Pence, REP",
      "Write-in")
  )
})

test_that("parse_contest_rows fills down precinct names for Absentee rows", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  abs_precincts <- result |>
    filter(vote_type == "Absentee") |>
    pull(precinct) |>
    unique()
  expect_true("(P1) Alpha Township" %in% abs_precincts)
})

test_that("parse_contest_rows strips commas from vote counts", {
  rows <- make_table_rows()
  rows[[2]][[3]] <- "1,234"
  result <- parse_contest_rows(rows, office = "President ( 1)")
  val <- result |>
    filter(
      precinct  == "(P1) Alpha Township",
      candidate == "Joseph R. Biden and Kamala D. Harris, DEM",
      vote_type == "Election Day"
    ) |>
    pull(value)
  expect_equal(val, 1234L)
})

test_that("parse_contest_rows stamps correct static metadata on every row", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  expect_true(all(result$election_type == "General"))
  expect_true(all(result$election_year == 2020L))
  expect_true(all(result$county        == "scott"))
  expect_true(all(result$office        == "President ( 1)"))
})

test_that("parse_contest_rows produces the correct row count", {
  result <- parse_contest_rows(make_table_rows(), office = "President ( 1)")
  # 2 precincts x 2 vote types x 3 candidates = 12 rows
  expect_equal(nrow(result), 12L)
})

test_that("parse_contest_rows returns an empty tibble when given only a header", {
  result <- parse_contest_rows(list(make_header()), office = "test")
  expect_equal(nrow(result), 0L)
})

# ── find_contest_starts ────────────────────────────────────────────────────────

test_that("find_contest_starts identifies contest-start pages by their header text", {
  texts <- c(
    "Intro page, no county header",
    "SCOTT COUNTY ELECTION CANVASS SUMMARY\nGeneral Election - 2020\nPresident and Vice President\nMore",
    "Continuation page",
    "SCOTT COUNTY ELECTION CANVASS SUMMARY\nGeneral Election - 2020\nUnited States Senator\nMore"
  )
  result <- find_contest_starts(texts)
  expect_equal(nrow(result), 2L)
  expect_equal(result$page_index, c(2L, 4L))
  expect_equal(result$office, c("President and Vice President", "United States Senator"))
})

test_that("find_contest_starts returns zero rows when no contest headers present", {
  result <- find_contest_starts(c("plain text", "more plain text"))
  expect_equal(nrow(result), 0L)
})

# ── assign_pages_to_contests ───────────────────────────────────────────────────

test_that("assign_pages_to_contests propagates contest name to continuation pages", {
  starts <- tibble(page_index = c(2L, 5L), office = c("Contest A", "Contest B"))
  result <- assign_pages_to_contests(n_pages = 6L, starts = starts)
  expect_equal(result$office[result$page_index == 3L], "Contest A")
  expect_equal(result$office[result$page_index == 4L], "Contest A")
  expect_equal(result$office[result$page_index == 5L], "Contest B")
  expect_equal(result$office[result$page_index == 6L], "Contest B")
})

test_that("assign_pages_to_contests excludes pages before the first contest start", {
  starts <- tibble(page_index = 2L, office = "Contest A")
  result <- assign_pages_to_contests(n_pages = 4L, starts = starts)
  expect_false(1L %in% result$page_index)
})
