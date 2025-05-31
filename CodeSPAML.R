library(dplyr)
library(tidyr)

setwd("/Users/qiuvanleeuwen/Documents/Thesis/RStudio")
source("Functions multiverse.R")

en_SPAML <- read.csv("en_SPAML.csv")
en_SPAML <- list(en_SPAML)

processedData <- en_SPAML %>% decision1(exclBelow18 = TRUE) %>% 
  decision2(exclNonNative = TRUE) %>% 
  decision4(exclBelow100trials = TRUE) %>% 
  decision5(exclSameResp = TRUE) %>% 
  decision6(exclAltResp = TRUE) %>% 
  decision9(exclError = TRUE) %>% 
  decision11(exclNegRTs = TRUE) %>%
  decision16(excl160ms = TRUE) %>% 
  decision17(excl3000ms = TRUE) %>% 
  decision18(exclPart3SD = TRUE) %>% 
  removeNonwords() %>% 
  removeFillerwords() %>% 
  ztransform() 

processedData <- data.frame(unlist(processedData, recursive = FALSE))

predictor <- read.csv("en_words.csv") %>%
  select(en_target, type, en_cosine) %>%
  rename(word = en_target)

primingData <- left_join(processedData, predictor, by = c("word", "type")) %>% 
  group_by(word, type) %>% 
  filter(type != "nonword") %>% 
  summarize(zRTMean = mean(zRT), en_cosine = first(en_cosine), .groups = "drop") %>%
  pivot_wider(names_from = type, values_from = c(zRTMean, en_cosine))

# Extract unique words for stimulus pairs
unique_words <- unique(processedData$word)
print(paste("Number of unique words:", length(unique_words)))
