# ASSOCIATION
results_association1 <- read.csv("results_association1.csv")
results_association2 <- read.csv("results_association2.csv")
results_association1$pair <- as.character(results_association1$pair)
results_association2$pair <- as.character(results_association2$pair)

# Combine dataframes
combined_results <- rbind(results_association1, results_association2)
ordered_pairs <- results_association1 %>%
  distinct(pair)

# Calculate average API_similarity and cosine_similarity per pair
average_results <- combined_results %>%
  group_by(pair, type) %>% # Group by both pair and type
  summarize(mean_cosine_similarity = mean(cosine_similarity, na.rm = TRUE),
            mean_API_similarity = mean(API_similarity, na.rm = TRUE),
            .groups = 'drop')

average_results <- ordered_pairs %>%
  left_join(average_results, by = "pair")

write.csv(average_results, "average_results_association.csv", row.names = FALSE)

# Calculate the correlation between cosine_similarity and API_similarity for average
correlation <- cor(average_results$mean_cosine_similarity, as.numeric(average_results$mean_API_similarity), use = "complete.obs")
print(paste("Correlation between cosine_similarity and API_similarity:", correlation))

# Calculate the correlation between cosine_similarity and API_similarity for run 1 and 2
correlation <- cor(results_association1$cosine_similarity, as.numeric(results_association1$API_similarity), use = "complete.obs")
print(paste("Correlation between cosine_similarity and API_similarity for association run 1:", correlation))
correlation <- cor(results_association2$cosine_similarity, as.numeric(results_association2$API_similarity), use = "complete.obs")
print(paste("Correlation between cosine_similarity and API_similarity for association run 2:", correlation))

# Dataframe run 1 and run 2
df_run1_scores <- results_association1 %>%
  select(
    pair, 
    API_similarity_run1 = API_similarity,
    cosine_similarity_run1 = cosine_similarity
  )

df_run2_scores <- results_association2 %>%
  select(
    pair, 
    API_similarity_run2 = API_similarity,
    cosine_similarity_run2 = cosine_similarity
  )
merged_runs_scores <- merge(df_run1_scores, df_run2_scores, by = "pair")

# Calculate the correlation between run 1 en run 2 for the API_similarity
correlation_api_run1_vs_run2 <- cor(as.numeric(merged_runs_scores$API_similarity_run1), as.numeric(merged_runs_scores$API_similarity_run2), use = "complete.obs")
print(paste("Correlation between API_similarity of run 1 and run 2:", correlation_api_run1_vs_run2))

# Calculate the correlation between run 1 en run 2 for the cosine_similarity
# cosine_similarity zou al numeriek moeten zijn, maar as.numeric() kan geen kwaad.
correlation_cosine_run1_vs_run2 <- cor(as.numeric(merged_runs_scores$cosine_similarity_run1), as.numeric(merged_runs_scores$cosine_similarity_run2), use = "complete.obs")
print(paste("Correlation between cosine_similarity of run 1 and run 2:", correlation_cosine_run1_vs_run2))

# Calculate the correlation for related pairs for average
average_results_related <- average_results %>% filter(type == "related")
correlation_related <- cor(as.numeric(average_results_related$mean_cosine_similarity), as.numeric(average_results_related$mean_API_similarity), use = "complete.obs")
print(paste("Correlation related pairs:", correlation_related))

# Calculate the correlation for unrelated pairs for average
average_results_unrelated <- average_results %>% filter(type == "unrelated")
correlation_unrelated <- cor(as.numeric(average_results_unrelated$mean_cosine_similarity), as.numeric(average_results_unrelated$mean_API_similarity), use = "complete.obs")
print(paste("Correlation unrelated pairs:", correlation_unrelated))