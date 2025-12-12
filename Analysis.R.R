# ----------------------------
# 0. Clean workspace 
# ----------------------------
rm(list = ls())

# ----------------------------
# 1. Load libraries
# ----------------------------
library(tidyverse)   # dplyr, tidyr, stringr, etc.
library(readxl)      # read_excel

# ----------------------------
# 2. Load dataset (Excel)
# ----------------------------
file_path <- "us-tmt-fast500-2017-winners-rankings.xlsx"
df_raw <- read_excel(file_path)

# Quick preview
message("Data preview (first 6 rows):")
print(head(df_raw))

# ----------------------------
# 3. Standardise column names
# ----------------------------
names(df_raw) <- names(df_raw) %>%
  str_replace_all("\\s+", "_") %>%
  str_replace_all("\\.", "_") %>%
  toupper()

message("Standardized column names:")
print(colnames(df_raw))
# ----------------------------
# 4. Select & rename expected columns (adapted to your file)
# ----------------------------
expected_cols <- c("RANK","COMPANY_NAME","PRIMARY_INDUSTRY","GROWTH","CITY","PROV_","CEO_NAME")
missing_cols <- setdiff(expected_cols, colnames(df_raw))
if (length(missing_cols) > 0) {
  stop("Expected columns missing from the Excel file: ", paste(missing_cols, collapse = ", "))
}

df <- df_raw %>%
  select(
    Rank = RANK,
    Company = COMPANY_NAME,
    Industry = PRIMARY_INDUSTRY,
    Growth_raw = GROWTH,
    City = CITY,
    State = PROV_,     # matches your file
    CEO = CEO_NAME
  )




