# 02_munge_2020-general.R
#
# Reads all per-county xlsx files from inst/data/2020/general/ and produces
# a single long-format parquet file that can later be bound with the 2024 data.
#
# Output schema (matches 01_munge_2024-general.R, plus `county`):
#   precinct      <chr>  Precinct label as printed in the source file
#   candidate     <chr>  Candidate name (or "Write-in")
#   value         <int>  Vote count
#   vote_type     <chr>  "Election Day" or "Absentee"
#   office        <chr>  Contest / office title
#   election_type <chr>  "General"
#   election_year <int>  2020
#   county        <chr>  Lowercase county key derived from filename
#
# Notes:
#   • Scott County is excluded — the SOS published only a PDF for 2020.
#     See scripts/parse_scott_2020_pdf.R for the separate PDF pipeline.
#   • "Total:" summary rows are dropped; only precinct-level rows are retained.
#   • "Total Votes" columns (every 3rd column starting from col 5) are dropped;
#     they are derivable as Election Day + Absentee.

library(dplyr)
library(purrr)
library(arrow)
library(here)
library(fs)

source(here("R", "parse_2020_general.R"))

# ── Paths ──────────────────────────────────────────────────────────────────────

input_dir <- here("inst", "data", "2020", "general")
output_file <- here(
  "inst",
  "parquet",
  "IA_2020_general-election-results_long.parquet"
)

# ── Discover county files ──────────────────────────────────────────────────────

xlsx_files <- dir_ls(input_dir, glob = "*.xlsx")

if (length(xlsx_files) == 0) {
  stop("No .xlsx files found in: ", input_dir)
}

message(
  "Found ",
  length(xlsx_files),
  " county xlsx file(s).\n",
  "Note: Scott County (PDF only) is handled separately and excluded here."
)

# ── Parse all counties ─────────────────────────────────────────────────────────

results_gen_2020 <- map_dfr(xlsx_files, function(f) {
  message("  Processing: ", path_file(f))
  parse_county_file_2020(f)
})

# ── Validate ───────────────────────────────────────────────────────────────────

stopifnot(
  "election_year column must be 2020 throughout" = all(
    results_gen_2020$election_year == 2020L
  ),
  "vote_type must be only Election Day or Absentee" = all(
    results_gen_2020$vote_type %in% c("Election Day", "Absentee")
  ),
  "No NA values allowed in key identifier columns" = !anyNA(results_gen_2020[c(
    "precinct",
    "candidate",
    "vote_type",
    "office",
    "county"
  )])
)

message("Total rows: ", nrow(results_gen_2020))
message("Counties processed: ", n_distinct(results_gen_2020$county))
message("Contests found: ", n_distinct(results_gen_2020$office))

# ── Write parquet ──────────────────────────────────────────────────────────────

dir_create(path_dir(output_file))
write_parquet(results_gen_2020, output_file)
message("Saved to: ", output_file)
