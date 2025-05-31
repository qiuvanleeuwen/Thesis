library(dplyr)
library(ggplot2)

# 1. Running CodeSPAML.R for data processing
tryCatch({
  source(file.path("CodeSPAML.R"), echo = TRUE)
  print("Stap 1 voltooid.")
}, error = function(e) {
  print(paste("Error running CodeSPAML.R:", e$message))
  stop("Script stops due to an error in CodeSPAML.R")
})

# 2. Do not run the API.R, since this will take too long. Instead read the following four CSV files that are already made with API.R and go to step 3.
results_association1 <- read.csv("results_association1.csv")
results_association2 <- read.csv("results_association2.csv")
results_feature_based1 <- read.csv("results_feature_based1.csv")
results_feature_based2 <- read.csv("results_feature_based2.csv")

# 3. Running AssociationCorrelation.R to make an average result CSV file for only association-based prompt and calculate the correlations
tryCatch({
  source(file.path("AssociationCorrelation.R"), echo = TRUE)
  print("Stap 1 voltooid.")
}, error = function(e) {
  print(paste("Error running AssociationCorrelation.R:", e$message))
  stop("Script stops due to an error in AssociationCorrelation.R")
})

# 4. Running FeatureBasedCorrelation.R to make an average result CSV file for only feature-based prompt and calculate the correlations
tryCatch({
  source(file.path("FeatureBasedCorrelation.R"), echo = TRUE)
  print("Stap 1 voltooid.")
}, error = function(e) {
  print(paste("Error running FeatureBasedCorrelation.R:", e$message))
  stop("Script stops due to an error in FeatureBasedCorrelation.R")
})

# 5. Running Priming.R to create two CSV files with four predictors per prompt
tryCatch({
  source(file.path("Priming.R"), echo = TRUE)
  print("Stap 1 voltooid.")
}, error = function(e) {
  print(paste("Error running Priming.R:", e$message))
  stop("Script stops due to an error in Priming.R")
})

# 6. Running Predictors.R to create a CSV file with all eight predictors from both prompts and the priming effect by Buchanan et al.
tryCatch({
  source(file.path("Predictors.R"), echo = TRUE)
  print("Stap 1 voltooid.")
}, error = function(e) {
  print(paste("Error running Predictors.R:", e$message))
  stop("Script stops due to an error in Predictors.R")
})

# 7. Running MRA.R to create the scatter plots for all eight predictors vs. priming effect by Buchanan et al.
tryCatch({
  source(file.path("MRA.R"), echo = TRUE)
  print("Stap 1 voltooid.")
}, error = function(e) {
  print(paste("Error running MRA.R:", e$message))
  stop("Script stops due to an error in MRA.R")
})