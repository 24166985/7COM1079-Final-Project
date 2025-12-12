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
# ----------------------------
# 5. Clean Growth: remove commas, percent signs, convert to numeric
# ----------------------------
df <- df %>%
  mutate(
    Growth_raw = as.character(Growth_raw),
    Growth_clean = Growth_raw %>%
      str_replace_all(",", "") %>%
      str_replace_all("%", "") %>%
      str_trim(),
    Growth = as.numeric(Growth_clean)
  )

# Report any conversion issues
if (any(is.na(df$Growth))) {
  warning("Some Growth values could not be converted to numeric. Showing first examples:")
  print(df %>% filter(is.na(Growth)) %>% select(Company, Growth_raw) %>% head())
}

# ----------------------------
# 6. Create cleaned industry categories (Software vs Biotech/Pharma)
# ----------------------------
df <- df %>%
  mutate(
    Industry_clean = case_when(
      str_to_lower(Industry) == "software" ~ "Software",
      str_detect(str_to_lower(Industry), "biotech") |
        str_detect(str_to_lower(Industry), "pharm") ~ "Biotech_Pharma",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Industry_clean), !is.na(Growth))

# Force factor level order so tests are consistent
df$Industry_clean <- factor(df$Industry_clean, levels = c("Software", "Biotech_Pharma"))

message("Counts by industry (filtered):")
print(table(df$Industry_clean))
# ----------------------------
# 7. Descriptive statistics (EDA table)
# ----------------------------
eda_summary <- df %>%
  group_by(Industry_clean) %>%
  summarise(
    n = n(),
    mean_growth = mean(Growth, na.rm = TRUE),
    median_growth = median(Growth, na.rm = TRUE),
    sd_growth = sd(Growth, na.rm = TRUE),
    iqr_growth = IQR(Growth, na.rm = TRUE),
    min_growth = min(Growth, na.rm = TRUE),
    max_growth = max(Growth, na.rm = TRUE)
  ) %>% arrange(Industry_clean)

message("Descriptive statistics by industry:")
print(eda_summary)

message("Overall summary of Growth:")
print(summary(df$Growth))

# Save the eda summary to CSV for easy copy/paste into report if needed
write_csv(eda_summary, "eda_summary_by_industry.csv")
# ----------------------------
# 8. EDA Plots (Base R) - Save PNG files
# ----------------------------

# Boxplot (main plot)
boxplot(Growth ~ Industry_clean, data = df,
        main = "Growth Rate (%) by Industry",
        xlab = "Industry",
        ylab = "Growth Rate (%)",
        col = c("lightblue", "lightgreen"),
        notch = FALSE)   # notch turned off ??? prevents warning

# Histograms per group
par(mfrow=c(1,2))
hist(df$Growth[df$Industry_clean=="Software"],
     main="Software: Growth Distribution",
     xlab="Growth (%)", breaks=20, col="lightblue")
hist(df$Growth[df$Industry_clean=="Biotech_Pharma"],
     main="Biotech/Pharma: Growth Distribution",
     xlab="Growth (%)", breaks=20, col="lightgreen")
par(mfrow=c(1,1))

# QQ plots per group
par(mfrow=c(1,2))
if (sum(df$Industry_clean == "Software") > 2) {
  qqnorm(df$Growth[df$Industry_clean=="Software"], main="QQ-plot: Software Growth")
  qqline(df$Growth[df$Industry_clean=="Software"])
} else {
  plot.new(); title("QQ-plot: Software (not enough points)")
}
if (sum(df$Industry_clean == "Biotech_Pharma") > 2) {
  qqnorm(df$Growth[df$Industry_clean=="Biotech_Pharma"], main="QQ-plot: Biotech/Pharma Growth")
  qqline(df$Growth[df$Industry_clean=="Biotech_Pharma"])
} else {
  plot.new(); title("QQ-plot: Biotech/Pharma (not enough points)")
}
par(mfrow=c(1,1))

# Jitter/stripchart for individual observations
stripchart(Growth ~ Industry_clean, data=df, vertical = TRUE, method = "jitter",
           pch = 19, col = c("blue","darkgreen"), main="Individual Growth Observations by Industry",
           xlab="Industry", ylab="Growth (%)")

# ----------------------------
# 9. Assumption checks (numerical)
# ----------------------------

growth_software <- df$Growth[df$Industry_clean == "Software"]
growth_biotech <- df$Growth[df$Industry_clean == "Biotech_Pharma"]

# Ensure n's exist
n1 <- length(growth_software)
n2 <- length(growth_biotech)

# Shapiro-Wilk normality tests (only valid if n between 3 and 5000)
shapiro_software <- if (n1 >= 3 && n1 <= 5000) shapiro.test(growth_software) else list(p.value=NA)
shapiro_biotech <- if (n2 >= 3 && n2 <= 5000) shapiro.test(growth_biotech) else list(p.value=NA)

message("Shapiro-Wilk normality test p-values:")
message("Software p = ", signif(shapiro_software$p.value,4))
message("Biotech p = ", signif(shapiro_biotech$p.value,4))

# Variance equality test 
var_test <- tryCatch({
  if (n1 > 1 && n2 > 1) var.test(growth_software, growth_biotech) else stop("Not enough observations for var.test")
}, error = function(e) {
  message("var.test() failed or not applicable: ", e$message)
  return(NULL)
})

if (!is.null(var_test)) {
  message("F-test p-value (variances): ", signif(var_test$p.value,4))
} else {
  message("Variance test not available; proceeding with caution.")
}

# ----------------------------
# 10. Choose & run inferential test
# ----------------------------
alpha <- 0.05
# Decide if parametric t-test is okay: both Shapiro p > alpha and var_test p > alpha
vartest_ok <- !is.null(var_test) && (var_test$p.value > alpha)
shapiro_ok <- (!is.na(shapiro_software$p.value) && shapiro_software$p.value > alpha) &&
  (!is.na(shapiro_biotech$p.value) && shapiro_biotech$p.value > alpha)

use_t_test <- shapiro_ok && vartest_ok

if (use_t_test) {
  message("Using Welch two-sample t-test (parametric).")
  # Welch's t-test does not assume equal variances; we could use var.equal=TRUE if var_test indicated equality.
  t_res <- t.test(Growth ~ Industry_clean, data = df, var.equal = FALSE)
  test_name <- "t_test"
  test_output <- t_res
} else {
  message("Using Wilcoxon rank-sum test (non-parametric).")
  wilcox_res <- wilcox.test(Growth ~ Industry_clean, data = df, exact = FALSE)
  test_name <- "wilcox"
  test_output <- wilcox_res
}

print("Inferential test result:")
print(test_output)








