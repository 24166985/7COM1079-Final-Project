library(readxl)
library(dplyr)
library(ggplot2)
library(car)
# Final code for analysis in report

# Load dataset
data <- read_excel("us-tmt-fast500-2017-winners-ranking.xlsx")

# Clean Growth column
data$Growth_raw <- as.numeric(gsub("[,%]", "", data$GROWTH))

# Create log-transformed column in main data frame
data$logGrowth <- log10(data$Growth_raw + 1)

# Filter only the two industries we compare
data_sub <- data %>% 
  filter(`PRIMARY INDUSTRY` %in% c("Biotechnology/pharmaceutical", "Software"))

# Levene test for equal variances
leveneTest(logGrowth ~ `PRIMARY INDUSTRY`, data = data_sub)

# Welch t-test on log-transformed values
t_test <- t.test(logGrowth ~ `PRIMARY INDUSTRY`, 
                 data = data_sub,
                 var.equal = FALSE)

print(t_test)

# Raw histogram
png("Figure1_raw_hist.png", width=800, height=500)
hist(data_sub$Growth_raw[data_sub$`PRIMARY INDUSTRY` == "Biotechnology/pharmaceutical"],
     col=rgb(0,0,1,0.5), xlim=c(0,60000), main="Raw Growth Distribution",
     xlab="Growth (%)")
hist(data_sub$Growth_raw[data_sub$`PRIMARY INDUSTRY` == "Software"],
     col=rgb(0,1,0,0.5), add=TRUE)
legend("topright", legend=c("Biotech","Software"), 
       fill=c(rgb(0,0,1,0.5),rgb(0,1,0,0.5)))
dev.off()

# Log histogram
png("Figure2_log_hist.png", width=800, height=500)
hist(data_sub$logGrowth[data_sub$`PRIMARY INDUSTRY` == "Biotechnology/pharmaceutical"],
     col=rgb(0,0,1,0.5), main="Log Growth Distribution",
     xlab="Log10(Growth+1)")
hist(data_sub$logGrowth[data_sub$`PRIMARY INDUSTRY` == "Software"],
     col=rgb(0,1,0,0.5), add=TRUE)
legend("topright", legend=c("Biotech","Software"),
       fill=c(rgb(0,0,1,0.5),rgb(0,1,0,0.5)))
dev.off()
