# =============================================================================
# File:          linear_mixed_effects_model.R
# Description:   Fit a linear mixed-effects model to longitudinal relative 
#                abundance data with time as fixed effect and subject as random 
#                intercept.
# Dependencies:  lme4, lmerTest, ggplot2
# Input:         CSV file with columns: 'abundance' (numeric), 'time' (numeric),
#                'subject' (identifier)
# Output:        Saved output files in 'output/' directory:
#                - model_summary.txt
#                - time_coefficient.csv
#                - confidence_intervals.txt
# Usage:         Run interactively; select data file when prompted.
# =============================================================================

# install required packages
install.packages("lme4")
install.packages("lmerTest")
install.packages("ggplot2")

# Load required packages
library(lme4)
library(lmerTest)
library(ggplot2)

# Read data
data <- read.csv(file.choose())
data$time    <- as.numeric(data$time)
data$subject <- as.factor(data$subject)

# Fit model
model <- lmer(
  abundance ~ time + (1 | subject),
  data = data,
  na.action = na.omit
)


# --- Save all outputs to a single combined text file ---
dir.create("output", showWarnings = FALSE)  # Create output folder if not exists

output_file <- "output/combined_results.txt"

# Open connection to write all output sequentially
sink(output_file)

# 1. Full model summary
cat("=== Model Summary ===\n\n")
print(summary(model))
cat("\n\n")

# 2. Time coefficient (estimate, SE, t, p)
cat("=== Time Coefficient ===\n\n")
time_coef <- coef(summary(model))["time", ]
time_df <- as.data.frame(t(time_coef))
rownames(time_df) <- "time"
print(time_df)
cat("\n\n")

# 3. 95% Confidence Intervals
cat("=== 95% Confidence Intervals (profile method) ===\n\n")
ci <- confint(model, parm = "beta_", method = "profile")
print(ci)
cat("\n")

# Close sink
sink()

# Optional: print confirmation
cat("\nAll results saved to 'output/LMM_results.txt'.\n")
