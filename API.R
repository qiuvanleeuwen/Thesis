library(openai)
library(glue)
library(dplyr)
library(tidyr)

Sys.setenv(
  OPENAI_API_KEY = '' # Insert own API key
)

# Define the filenames for the four individual output CSV files.
filename_prompt_association_run1 <- "results_association1.csv"
filename_prompt_association_run2 <- "results_association2.csv"
filename_prompt_feature_run1     <- "results_feature_based1.csv"
filename_prompt_feature_run2     <- "results_feature_based1.csv"

get_embedding <- function(word) {
  tryCatch({
    answer <- create_embedding(
      model = "text-embedding-3-small",
      input = word
    )
    embedding <- answer$data$embedding[[1]]
    return(embedding)
  }, error = function(e) {
    cat("Error getting embedding for word:", word, "\n")
    print(e)
    return(NA) 
  }
  )
}

# Calculate the cosine similarity between two embedding vectors
cosine_similarity <- function(vec1, vec2) {
  if(any(is.na(vec1)) || any(is.na(vec2)) || length(vec1) == 1 || length(vec2) == 1) {
    return(NA_real_)
  }
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

source("CodeSPAML.R")
if (is.list(en_SPAML)) {
  en_SPAML <- en_SPAML[[1]]
}

# Filter and prepare word pairs from en_SPAML
word_pairs <- en_SPAML %>%
  filter(!is.na(pair) & pair != "NA") %>% 
  filter(tolower(class) == "word" & tolower(type) != "nonword") %>% 
  select(word, which, pair, type, class) %>%
  distinct(pair, .keep_all = TRUE) 

# Ensure pair columns are character type
en_SPAML$pair <- as.character(en_SPAML$pair)
word_pairs$pair <- as.character(word_pairs$pair)

# Separate the pair column into cue and 'target' words
test_pairs <- word_pairs %>%
  separate(pair, into = c("cue", "target"), sep = "-", remove = FALSE, extra = "merge", fill = "right") %>%
  filter(!is.na(target)) %>% 
  select(cue, target, type, class, pair)
test_pairs$pair <- as.character(test_pairs$pair)
test_pairs <- head(test_pairs, 2) # Test

for (i in 1:nrow(test_pairs_processed)) {
  cue_word <- test_pairs_processed$cue[i]
  target_word <- test_pairs_processed$target[i]
  
  # Get embeddings for cue and target words
  cue_emb <- get_embedding(cue_word)
  target_emb <- get_embedding(target_word)
  
  # Calculate and store cosine similarity
  test_pairs_processed$cosine_similarity_val[i] <- cosine_similarity(cue_emb, target_emb)
}
# '{cue_word}' and '{target_word}' are placeholders that will be filled dynamically
prompts_list <- list(
  "association" = "In this study we want to investigate the degree to which English nouns can be considered related. We will present you with two nouns: {cue_word} and {target_word}. Words are related if they co-occur in similar situations and derive their meaning from the context in which the word occurs. Your task is to rate the relatedness of a word pair based on how often these two words are used together in everyday language. Use a numerical rating from 0 – 1 with 3 decimals. A rating of 0 means the pair has no possible relation/association in everyday language. A rating of 1 means that the pair has the highest possible degree of relation/association. Evaluate relatedness solely in regard to the co-occurrence patterns and not on meaning of a word. For example, the word pair cold - hot should have a high relatedness of 0.879, since they co-occur in similar situations. However, frog - square should have a low relatedness of 0.113, since they are not used in similar situations. Only return a numeric value, nothing else.",
  "feature" = "In this study we want to investigate the degree to which English nouns can be considered related. We will present you with two nouns: {cue_word} and {target_word}. Words must have shared semantic features of words to be considered related. The relationship is based on shared properties, attributes and categories (e.g., 'has four legs', 'is a vehicle', 'is edible'). Explicitly ignore word associations and co-occurrence in language (e.g., do not rate 'hot' and 'cold’' high just because they are often used in daily language). Focus solely on overlapping features. Use a numerical rating from 0 – 1 with 3 decimals. A rating of 0 means the pair has no possible relation in semantic features. A rating of 1 means that the pair has the highest possible degree of relation in semantic features. For example, the word pair cat - dog should have a high relatedness of 0.887, since their features are similar (animal, pet, furry, has four legs). However, cat - bus should have a low relatedness of 0.112, since they share very few features. Only return a numeric value, nothing else."
)

n_runs_per_prompt <- 2 

# Initialize DataFrame
results_final <- data.frame(
  pair = character(),
  type = character(),
  prompt_name = character(),
  run_number = integer(),
  cosine_similarity = numeric(),
  API_similarity = character(),
  stringsAsFactors = FALSE
)

# Loop over each defined prompt
for (p_name in names(prompts_list)) {
  current_prompt_template <- prompts_list[[p_name]] # Get the current prompt template
  
  for (run_n in 1:n_runs_per_prompt) {   # Loop for the specified number of runs per prompt
    for (i in 1:nrow(test_pairs_processed)) {     # Loop over each word pair
      
      # Extract data for the current pair
      cue_word <- test_pairs_processed$cue[i]
      target_word <- test_pairs_processed$target[i]
      original_pair_id <- test_pairs_processed$pair[i] 
      pair_type <- test_pairs_processed$type[i]
      cos_sim <- test_pairs_processed$cosine_similarity_val[i]
    
      prompt_content <- glue(current_prompt_template) 
      
      api_sim_content <- NA_character_ # Initialize API similarity content as NA
      tryCatch({
        answer <- create_chat_completion(
          model = "gpt-4o-mini", 
          temperature = 1,  # Set temperature for variability (otherwise it would be rounded up or down)
          messages = list(
            list(
              "role" = "system",
              "content" = "Your task is to evaluate the relatedness of two English nouns, called a cue and a target, to give a numerical rating from 0 to 1 with 3 decimal places, where 0 indicates no relation and 1 indicates the highest possible degree of relatedness."
            ),
            list( # User message containing the specific prompt
              "role" = "user",
              "content" = prompt_content
            )
          )
        )
        api_sim_content <- answer$choices$message.content
      })
      
      # Add the results for the current pair, prompt, and run to the main results dataframe
      results_final <- rbind(results_final, data.frame(
        pair = original_pair_id,
        type = pair_type,
        prompt_name = p_name,
        run_number = run_n,
        cosine_similarity = cos_sim,
        API_similarity = api_sim_content,
        stringsAsFactors = FALSE
      ))
    }
  }
}

save_filtered_results <- function(main_df, target_prompt_name, target_run_number, output_filename) {
  
  # Filter the main dataframe for the specific prompt and run
  filtered_df <- main_df %>%
    filter(prompt_name == target_prompt_name & run_number == target_run_number)
  
  # Select only the desired columns for the individual output files
  if (nrow(filtered_df) > 0) {
    df_to_save <- filtered_df %>%
      select(pair, type, cosine_similarity, API_similarity) 

    write.csv(df_to_save, output_filename, row.names = FALSE)
  }
}

save_filtered_results(results_final, "association", 1, filename_prompt_association_run1)
save_filtered_results(results_final, "association", 2, filename_prompt_association_run2) 
save_filtered_results(results_final, "feature", 1, filename_prompt_feature_run1)
save_filtered_results(results_final, "feature", 2, filename_prompt_feature_run2)