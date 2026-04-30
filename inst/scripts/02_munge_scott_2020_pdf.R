# 02_munge_scott_2020_pdf.R
#
# Parses the Scott County 2020 General Election PDF and writes a long-format
# parquet file with the same schema as 02_munge_2020-general.R so both
# outputs can be bound with a simple arrow::open_dataset() or bind_rows().
#
# Prerequisites:
#   pip install pdfplumber       (Python package used via reticulate)
#
# Output:
#   inst/data/2020/IA_2020_scott-general-election-results_long.parquet
#
# Notes:
#   • "Undervotes", "Overvotes", and "Total" columns are intentionally dropped.
#   • Precinct-level "Total" subtotal rows are dropped; only Election Day and
#     Absentee rows are retained.
#   • The PDF does not include a United States Representative contest for Scott
#     County; those races appear to have been omitted from the SOS publication.

library(arrow)
library(here)
library(fs)

source(here("R", "parse_scott_2020_pdf.R"))

# ── Paths ──────────────────────────────────────────────────────────────────────

input_file <- here("inst", "data", "2020", "general", "scott.pdf")
output_file <- here(
  "inst",
  "parquet",
  "IA_2020_scott-general-election-results_long.parquet"
)

if (!file_exists(input_file)) {
  stop(
    "Scott County PDF not found at: ",
    input_file,
    "\n",
    "Download it from the Iowa SOS 2020 general election results page and ",
    "place it alongside the other county xlsx files."
  )
}

# ── Python / pdfplumber preflight ──────────────────────────────────────────────
# Reports which Python is active and installs pdfplumber if absent.
# If this block errors, run check_python_env() and reticulate::py_config()
# to diagnose which Python environment reticulate has selected.

check_python_env()

# ── Parse ──────────────────────────────────────────────────────────────────────

message("Parsing Scott County PDF…")
results_scott_2020 <- parse_scott_pdf(input_file)

# ── Validate ───────────────────────────────────────────────────────────────────

stopifnot(
  "election_year must be 2020 throughout" = all(
    results_scott_2020$election_year == 2020L
  ),
  "vote_type must be only Election Day or Absentee" = all(
    results_scott_2020$vote_type %in% c("Election Day", "Absentee")
  ),
  "county must be 'scott' throughout" = all(
    results_scott_2020$county == "scott"
  ),
  "No NA values in key identifier columns" = !anyNA(results_scott_2020[c(
    "precinct",
    "candidate",
    "vote_type",
    "office",
    "county"
  )])
)

message("Total rows   : ", nrow(results_scott_2020))
message("Contests     : ", dplyr::n_distinct(results_scott_2020$office))
message("Precincts    : ", dplyr::n_distinct(results_scott_2020$precinct))

# ── Write parquet ──────────────────────────────────────────────────────────────

dir_create(path_dir(output_file))
write_parquet(results_scott_2020, output_file)
message("Saved to: ", output_file)
