# tests/testthat/test-parse_2020_general.R
#
# Unit tests for the 2020 Iowa general election parsing helpers.
# Run with: testthat::test_file("tests/testthat/test-parse_2020_general.R")
# or:       devtools::test()

library(testthat)
library(dplyr)
library(tibble)
library(purrr)

source(here::here("R", "parse_2020_general.R"))

# ── Synthetic fixture ──────────────────────────────────────────────────────────
# Mirrors the exact layout of a real 2020 county contest sheet (no col names).
#
#   Row 1 : office
#   Row 2 : NaN | NaN | CandA | NaN | NaN | CandB | NaN | NaN
#   Row 3 : NaN | Reg Voters | Election Day | Absentee | Total Votes | ...
#   Row 4 : Precinct 1 data
#   Row 5 : Precinct 2 data
#   Row 6 : Total: summary  ← must be excluded

make_raw_sheet <- function() {
  tibble(
    `...1` = c("President ( 1)", NA,                  NA,               "Precinct 1", "Precinct 2", "Total:"),
    `...2` = c(NA,               NA,                  "Reg Voters",      100L,         200L,          300L),
    `...3` = c(NA,               "Alice",             "Election Day",     10L,          20L,           30L),
    `...4` = c(NA,               NA,                  "Absentee",          5L,           8L,           13L),
    `...5` = c(NA,               NA,                  "Total Votes",      15L,          28L,           43L),
    `...6` = c(NA,               "Bob",               "Election Day",      7L,          12L,           19L),
    `...7` = c(NA,               NA,                  "Absentee",          3L,           6L,            9L),
    `...8` = c(NA,               NA,                  "Total Votes",      10L,          18L,           28L)
  )
}

# ── Column contract ────────────────────────────────────────────────────────────

test_that("parse_sheet_2020 returns the expected columns", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  expect_named(
    result,
    c("precinct", "candidate", "value", "vote_type",
      "office", "election_type", "election_year", "county")
  )
})

# ── Total: row exclusion ───────────────────────────────────────────────────────

test_that("parse_sheet_2020 excludes the Total: summary row", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")
  expect_false("Total:" %in% result$precinct)
})

# ── Candidate detection ────────────────────────────────────────────────────────

test_that("parse_sheet_2020 detects all candidates in row 2", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")
  expect_setequal(unique(result$candidate), c("Alice", "Bob"))
})

# ── Vote type splitting ────────────────────────────────────────────────────────

test_that("parse_sheet_2020 produces exactly Election Day and Absentee vote types", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")
  expect_setequal(unique(result$vote_type), c("Election Day", "Absentee"))
})

# ── Value accuracy ─────────────────────────────────────────────────────────────

test_that("parse_sheet_2020 extracts correct Election Day value for Alice / Precinct 1", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  val <- result |>
    filter(candidate == "Alice", vote_type == "Election Day", precinct == "Precinct 1") |>
    pull(value)

  expect_equal(val, 10L)
})

test_that("parse_sheet_2020 extracts correct Absentee value for Alice / Precinct 2", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  val <- result |>
    filter(candidate == "Alice", vote_type == "Absentee", precinct == "Precinct 2") |>
    pull(value)

  expect_equal(val, 8L)
})

test_that("parse_sheet_2020 extracts correct Election Day value for Bob / Precinct 2", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  val <- result |>
    filter(candidate == "Bob", vote_type == "Election Day", precinct == "Precinct 2") |>
    pull(value)

  expect_equal(val, 12L)
})

test_that("parse_sheet_2020 extracts correct Absentee value for Bob / Precinct 1", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  val <- result |>
    filter(candidate == "Bob", vote_type == "Absentee", precinct == "Precinct 1") |>
    pull(value)

  expect_equal(val, 3L)
})

# ── Static metadata ────────────────────────────────────────────────────────────

test_that("parse_sheet_2020 stamps correct static metadata on every row", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  expect_true(all(result$election_type == "General"))
  expect_true(all(result$election_year == 2020L))
  expect_true(all(result$county        == "testcounty"))
  expect_true(all(result$office        == "President ( 1)"))
})

# ── Row count ─────────────────────────────────────────────────────────────────

test_that("parse_sheet_2020 returns the right number of rows", {
  result <- parse_sheet_2020(make_raw_sheet(), county = "testcounty")

  # 2 candidates × 2 vote types × 2 precincts = 8 rows
  expect_equal(nrow(result), 8L)
})

# ── Integration: parse_county_file_2020 with a real sample file ───────────────

test_that("parse_county_file_2020 works on adair.xlsx sample and skips non-contest sheets", {
  skip_if_not(
    file.exists(here::here("inst", "data", "2020", "general", "adair.xlsx")),
    "adair.xlsx not present in inst/data/2020/general — skipping integration test"
  )

  result <- parse_county_file_2020(
    here::here("inst", "data", "2020", "general", "adair.xlsx")
  )

  expect_true(nrow(result) > 0)
  expect_true(all(result$county == "adair"))
  expect_false("Table of Contents" %in% result$office)
  expect_false("Registered Voters" %in% result$office)
})
