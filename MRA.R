library(ggplot2)
library(dplyr)
library(tidyr)

input_data <- read.csv("PrimingPredictors.csv")

dependent_variable <- "zRT_Priming_Effect"
all_predictors <- c(
  "RelatedCos", "DifferenceCos", "A.RelatedAPI", "A.DifferenceAPI", "Fb.RelatedAPI", "Fb.DifferenceAPI"
)

# Calculate Descriptive statistics for zRT_Priming_Effect
if (dependent_variable %in% names(input_data)) {
  desc_zRT <- input_data %>%
    summarise(
      Variable = dependent_variable,
      Observations    = sum(!is.na(!!sym(dependent_variable))),
      Mean     = mean(!!sym(dependent_variable), na.rm = TRUE),
      SD       = sd(!!sym(dependent_variable), na.rm = TRUE),
      Min      = min(!!sym(dependent_variable), na.rm = TRUE),
      Max      = max(!!sym(dependent_variable), na.rm = TRUE)
    )
  print(desc_zRT)
  if(nrow(desc_zRT) > 0){
    print(paste0("For ", dependent_variable, ": Mean = ", round(desc_zRT$Mean, 3),
                 ", SD = ", round(desc_zRT$SD, 3),
                 ", Min = ", round(desc_zRT$Min, 3),
                 ", Max = ", round(desc_zRT$Max, 3),
                 ", based on ", desc_zRT$N_Obs, " target words/observations."))
  }
  
}

# Calculate Descriptive statistics for the six predictors
existing_predictors <- intersect(all_predictors, names(input_data))

if (length(existing_predictors) > 0) {
  desc_predictors_table <- input_data %>%
    select(all_of(existing_predictors)) %>%
    summarise(
      across(
        everything(),
        list(
          Observations = ~sum(!is.na(.)),
          Mean  = ~mean(., na.rm = TRUE),
          SD    = ~sd(., na.rm = TRUE),
          Min   = ~min(., na.rm = TRUE),
          Max   = ~max(., na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Predictor", "Statistic"),
      names_sep = "__"
    ) %>%
    pivot_wider(
      names_from = Statistic,
      values_from = value
    )
  print(desc_predictors_table)
}

# Make six individual plots per predictor in a new map
output_individual <- "individual_scatter_plots"
for (predictor_name in all_predictors) {
  plot_data_single <- input_data %>%  # Select Y and current X
    select(Y = all_of(dependent_variable), X = all_of(predictor_name))
  correlation_value <- cor(plot_data_single$X, plot_data_single$Y, use = "complete.obs")
  correlation_text <- paste("Pearson correlation =", round(correlation_value, 5))
  p_individual <- ggplot(plot_data_single, aes(x = X, y = Y)) +  # Create the ggplot object
    geom_point(color = "blue", shape = 18, size = 2.5, alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
    labs(title = paste(dependent_variable, "vs.", predictor_name), subtitle = correlation_text, x = predictor_name, y = dependent_variable) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  output_filename_individual <- file.path(output_individual, paste0("scatter_", predictor_name, ".png"))  # Save the plot as a png file
  ggsave(output_filename_individual, plot = p_individual, width = 6, height = 4, dpi = 300)
}

# Make 2 plots for only all related plots and only all difference plots in a new map
output_grouped <- "grouped_plots"
create_grouped_plot <- function(data, predictors_group, group_title, filename) {
  plot_data_long <- data %>%  # Reshape data from wide to long format for grouping
    select(all_of(dependent_variable), all_of(predictors_group)) %>%
    pivot_longer(cols = all_of(predictors_group), names_to = "PredictorName", values_to = "PredictorValue") %>%
    rename(OutcomeValue = !!sym(dependent_variable))
  if(nrow(plot_data_long) == 0) { return(NULL) }
  
  p_grouped <- ggplot(plot_data_long, aes(x = PredictorValue, y = OutcomeValue)) +
    geom_point(alpha = 0.3, size = 1, color="blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +
    facet_wrap(~ PredictorName, scales = "free_x") +  # Create separate plots for each predictor, allow different x-scales
    labs(title = paste(dependent_variable, "vs.", group_title, "Predictors"), x = "Predictor Value", y = dependent_variable) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, face = "bold"), strip.text = element_text(face="bold", size=9), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(filename, plot = p_grouped, width = 8, height = 7, dpi = 300)
}

# Show three Related Predictors
related_predictors <- c("RelatedCos", "A.RelatedAPI", "Fb.RelatedAPI")
output_related_grouped <- file.path(output_grouped, "all_related_predictors.png")
create_grouped_plot(input_data, related_predictors, "Related", output_related_grouped)

# Show three Difference Predictors
difference_predictors <- c("DifferenceCos", "A.DifferenceAPI", "Fb.DifferenceAPI")
output_difference_grouped <- file.path(output_grouped, "all_difference_predictors.png")
create_grouped_plot(input_data, difference_predictors, "Difference Score", output_difference_grouped)

# Make 2 plots where all three related predictors are merged together and all three difference predictors
output_merge <- "merged_plots"
create_merged_plot <- function(data, predictors_group, group_title, filename) {
  plot_data_long <- data %>%
    select(all_of(dependent_variable), all_of(predictors_group)) %>%
    pivot_longer(cols = all_of(predictors_group), names_to = "PredictorName", values_to = "PredictorValue") %>%
    rename(OutcomeValue = !!sym(dependent_variable)) %>%
    filter(!is.na(OutcomeValue) & !is.na(PredictorValue))

# Give every predictor in a plot a different color, form and line
  p_merge <- ggplot(plot_data_long, aes(x = PredictorValue, y = OutcomeValue, color = PredictorName, shape = PredictorName)) +
    geom_point(alpha = 0.5, size = 2) + 
    geom_smooth(method = "lm", se = FALSE, aes(linetype = PredictorName), linewidth = 1) +
    labs(
      title = paste(dependent_variable, "vs. All four", group_title, "Predictors"),
      x = "Predictor Value",
      y = dependent_variable,
      color = "Predictor",
      shape = "Predictor",
      linetype = "Predictor"
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "right"
    )
  ggsave(filename, plot = p_merge, width = 8, height = 6, dpi = 300)
}

# Merge Related Predictors
output_related_merged <- file.path(output_merge, "merged_related_predictors.png")
create_merged_plot(input_data, related_predictors, "Related", output_related_merged)

# Merge Difference Predictors
output_difference_merged <- file.path(output_merge, "merged_difference_predictors.png")
create_merged_plot(input_data, difference_predictors, "Difference Score", output_difference_merged)

# Make formula for lm function
predictors_formula_part <- paste(paste0("", all_predictors, ""), collapse = " + ")
formula_str <- paste0("", dependent_variable, " ~ ", predictors_formula_part)
model_formula <- as.formula(formula_str)
print(paste("Used formula:", formula_str))

# Use standard lm function to plot
model <- lm(model_formula, data = input_data)
summary_model <- summary(model)
summary(model)
plot(model)

# Add column with the 6 predictor names
coefficients_matrix <- summary_model$coefficients
coefficients_df <- as.data.frame(coefficients_matrix)
coefficients_df <- coefficients_df %>%
  tibble::rownames_to_column(var = "Predictor") # Add column called 'Predictor'

write.csv(coefficients_df, "multiple_regression_table.csv", row.names = FALSE)