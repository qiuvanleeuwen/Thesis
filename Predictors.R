library(dplyr)
library(tidyr)    

original_word_order <- unique(processedData$word)
processedData <- processedData %>% mutate(word = as.character(word), type = as.character(type))
predictor <- predictor %>% mutate(word = as.character(word), type = as.character(type))

# Calculate primingData
primingData <- left_join(processedData, predictor, by = c("word", "type")) %>%
  group_by(word, type) %>%
  filter(type != "nonword") %>%
  summarize(zRTMean = mean(zRT, na.rm = TRUE), en_cosine = first(en_cosine), .groups = "drop") %>%
  filter(!is.na(word) & !is.na(type)) %>%
  pivot_wider(names_from = type, values_from = c(zRTMean, en_cosine)) %>%
  mutate(zRT_Priming_Effect = zRTMean_unrelated - zRTMean_related)

# Make columns for association-based
predictors_assoc_raw <- read.csv("predictors_priming_association.csv")
predictors_assoc <- predictors_assoc_raw %>%
  mutate(pair = as.character(pair), type = as.character(type)) %>%
  filter(type == "related") %>%
  separate(pair, into = c("cue", "target"), sep = "-", remove = FALSE, extra = "merge", fill = "right") %>%
  filter(!is.na(target) & target != "") %>%
  select(
    target,
    RelatedCos = Only_Related_cosine,
    DifferenceCos = DifferenceScore_cosine,
    A.RelatedAPI = Only_Related_API,
    A.DifferenceAPI = DifferenceScore_API
  ) %>%
  distinct(target, .keep_all = TRUE)

# Make columns for feature-based
predictors_feat_raw <- read.csv("predictors_priming_feature_based.csv")
predictors_feat <- predictors_feat_raw %>%
  mutate(pair = as.character(pair), type = as.character(type)) %>%
  filter(type == "related") %>%
  separate(pair, into = c("cue", "target"), sep = "-", remove = FALSE, extra = "merge", fill = "right") %>%
  filter(!is.na(target) & target != "") %>%
  select(
    target,
    Fb.RelatedAPI = Only_Related_API,
    Fb.DifferenceAPI = DifferenceScore_API
  ) %>%
  distinct(target, .keep_all = TRUE)

# Merge all predictors
primingData$word <- as.character(primingData$word)
predictors_assoc$target <- as.character(predictors_assoc$target)
predictors_feat$target <- as.character(predictors_feat$target)
primingData_merged <- left_join(primingData, predictors_assoc, by = c("word" = "target"))
primingData_merged <- left_join(primingData_merged, predictors_feat, by = c("word" = "target"))

write.csv(primingData_merged, "PrimingPredictors.csv", row.names = FALSE)