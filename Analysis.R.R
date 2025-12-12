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


