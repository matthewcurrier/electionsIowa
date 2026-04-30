library(arrow)
library(dplyr)
library(here)

df <- bind_rows(
  read_parquet(here("inst", "parquet", "IA_2020_general-election-results_long.parquet")),
  read_parquet(here("inst", "parquet", "IA_2020_scott-general-election-results_long.parquet")),
  read_parquet(here("inst", "parquet", "IA_2024_general-election-results_long.parquet"))
)
