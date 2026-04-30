# 01_munge_2024-general.R
#
# Reads the 2024 Iowa General Election results xlsx and writes a single
# long-format parquet file to inst/parquet/.

library(readxl)
library(dplyr)
library(purrr)
library(arrow)
library(here)
library(fs)

input_file <- here(
  "inst",
  "data",
  "2024",
  "IA_2024_general-election-results.xlsx"
)
output_file <- here(
  "inst",
  "parquet",
  "IA_2024_general-election-results_long.parquet"
)

ELECTION_TYPE <- "General"
ELECTION_YEAR <- 2024L

skip_sheets <- c("Table of Contents", "Registered Voters")
all_sheets <- excel_sheets(input_file)
contest_sheets <- setdiff(all_sheets, skip_sheets)

parse_sheet <- function(sheet_name) {
  raw <- read_excel(input_file, sheet = sheet_name, col_names = FALSE)

  office <- as.character(raw[[1, 1]])

  row2 <- unlist(raw[2, ])
  candidate_start_cols <- which(!is.na(row2) & seq_along(row2) >= 3)
  candidates <- as.character(row2[candidate_start_cols])

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
        election_type = ELECTION_TYPE,
        election_year = ELECTION_YEAR
      ),
      tibble(
        precinct = precincts,
        candidate = candidate,
        value = as.integer(data_rows[[start_col + 1L]]),
        vote_type = "Absentee",
        office = office,
        election_type = ELECTION_TYPE,
        election_year = ELECTION_YEAR
      )
    )
  })
}

message("Processing ", length(contest_sheets), " contest sheets...")

results_gen_2024 <- map_dfr(contest_sheets, function(sheet) {
  message("  Sheet: ", sheet)
  parse_sheet(sheet)
})

message("Total rows: ", nrow(results_gen_2024))

results_gen_2024 <- results_gen_2024 |>
  rename(county = precinct) |>
  mutate(precinct = NA_character_)

dir_create(path_dir(output_file))
write_parquet(results_gen_2024, output_file)
message("Saved to: ", output_file)
