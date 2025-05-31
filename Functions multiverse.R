decision1 <- function(data, exclBelow18 = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclBelow18){
    returnData <- c(returnData, lapply(data, subset, age >= 18))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclBelow18 & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision2 <- function(data, exclNonNative = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclNonNative){
    returnData <- c(returnData, lapply(data, subset, native_match))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclNonNative & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision3 <- function(data, exclSilverman = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclSilverman){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$duration, df$observation)
      pValuesSilverman <- sapply(dataParticipant, function(x) try(silverman.test(x, k = 1, R = 500)@p_value)) #the try function is used because the silverman.test function gives an error when there are too few observations for a participant
      keepParticipants <- names(pValuesSilverman)[pValuesSilverman > 0.05]
      subset <- subset(df, observation %in% keepParticipants)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclSilverman & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision4 <- function(data, exclBelow100trials = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclBelow100trials){
    
    returnData <- c(returnData, lapply(data, function(df){
      trialsCount <- table(df$observation)
      keepParticipants <- names(trialsCount[trialsCount > 99])
      subset <- subset(df, observation %in% keepParticipants)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclBelow100trials & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision5 <- function(data, exclSameResp = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclSameResp){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$response, df$observation)
      uniqueResponses <- sapply(dataParticipant, function(x) length(unique(x[!is.na(x)])))
      removeParticipants <- names(uniqueResponses)[uniqueResponses == 1]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclSameResp & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision6 <- function(data, exclAltResp = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclAltResp){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$response, df$observation)
      propSame <- sapply(dataParticipant, function(x) sum(x==lag(x), na.rm = T)/(length(x)-1)) #proportion of responses that are the same as the response to the previous trial per participant; if this is zero participants always switched between responses (or they never gave any response)
      removeParticipants <- names(propSame)[propSame == 0]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclAltResp & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision7 <- function(data, excl10 = FALSE, excl20 = FALSE, excl40 = FALSE, excl70 = FALSE, excl3SD = FALSE, exclMAD = FALSE, excl25NW = FALSE, excl25W = FALSE, excl30WNW = FALSE, exclProptest = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (excl10){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$correct, df$observation)
      propIncorrect <- sapply(dataParticipant, function(x) 1-mean(x, na.rm = TRUE))
      removeParticipants <- names(propIncorrect)[propIncorrect > .10]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl20){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$correct, df$observation)
      propIncorrect <- sapply(dataParticipant, function(x) 1-mean(x, na.rm = TRUE))
      removeParticipants <- names(propIncorrect)[propIncorrect > .20]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl40){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$correct, df$observation)
      propIncorrect <- sapply(dataParticipant, function(x) 1-mean(x, na.rm = TRUE))
      removeParticipants <- names(propIncorrect)[propIncorrect > .40]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl70){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$correct, df$observation)
      propIncorrectTimeout <- sapply(dataParticipant, function(x) (sum(!x, na.rm = TRUE) + sum(is.na(x)))/length(x))
      removeParticipants <- names(propIncorrectTimeout)[propIncorrectTimeout > .70]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl3SD){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$correct, df$observation)
      propCorrect <- sapply(dataParticipant, function(x) mean(x, na.rm = TRUE))
      removeParticipants <- names(propCorrect)[propCorrect < (mean(propCorrect, na.rm  = TRUE) - 3*sd(propCorrect, na.rm = TRUE))]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (exclMAD){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$correct, df$observation)
      propCorrect <- sapply(dataParticipant, function(x) mean(x, na.rm = TRUE))
      medianAcc <- median(propCorrect, na.rm = TRUE)
      madAcc <- mad(propCorrect, na.rm = TRUE)
      removeParticipants <- names(propCorrect)[propCorrect < medianAcc - 3*madAcc | propCorrect > medianAcc + 3*madAcc]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl25NW){
    returnData <- c(returnData, lapply(data, function(df){
      dfNW <- subset(df, class == "nonword") #only retain nonwords
      dataParticipant <- split(dfNW$correct, dfNW$observation)
      propIncorrect <- sapply(dataParticipant, function(x) 1-mean(x, na.rm = TRUE))
      removeParticipants <- names(propIncorrect)[propIncorrect > .25]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl25W){
    returnData <- c(returnData, lapply(data, function(df){
      dfW <- subset(df, class == "word") #only retain words
      dataParticipant <- split(dfW$correct, dfW$observation)
      propIncorrect <- sapply(dataParticipant, function(x) 1-mean(x, na.rm = TRUE))
      removeParticipants <- names(propIncorrect)[propIncorrect > .25]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (excl30WNW){
    returnData <- c(returnData, lapply(data, function(df){
      dfW <- subset(df, class == "word") #only retain words
      dataParticipantW <- split(dfW$correct, dfW$observation)
      propIncorrectW <- sapply(dataParticipantW, function(x) 1-mean(x, na.rm = TRUE))
      dfNW <- subset(df, class == "nonword") #only retain nonwords
      dataParticipantNW <- split(dfNW$correct, dfNW$observation)
      propIncorrectNW <- sapply(dataParticipantNW, function(x) 1-mean(x, na.rm = TRUE))
      removeParticipants <- c(names(propIncorrectW)[propIncorrectW > .30], names(propIncorrectNW)[propIncorrectNW > .30])
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (exclProptest){
    returnData <- c(returnData, lapply(data, function(df){
      dfW <- subset(df, class == "word" & !is.na(correct)) #only retain words and trials with a response
      dataParticipantW <- split(dfW$correct, dfW$observation)
      pValueW <- sapply(dataParticipantW, function(x) prop.test(sum(x), n = length(x), alternative = "greater")$p.value)
      dfNW <- subset(df, class == "nonword" & !is.na(correct)) #only retain nonwords and trials with a response
      dataParticipantNW <- split(dfNW$correct, dfNW$observation)
      pValueNW <- sapply(dataParticipantNW, function(x) prop.test(sum(x), n = length(x), alternative = "greater")$p.value)
      removeParticipants <- c(names(pValueW)[pValueW > .05], names(pValueNW)[pValueNW > .05])
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!excl10 & !excl20 & !excl40 & !excl70 & !excl3SD & !exclMAD & !excl25NW & !excl25W & !excl30WNW & !exclProptest & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision8 <- function(data, excl25 = FALSE, excl50 = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (excl25){
    returnData <- c(returnData, lapply(data, function(df){
      dataItem <- split(df$correct, df$word)
      propIncorrect <- sapply(dataItem, function(x) 1-mean(x, na.rm = TRUE))
      removeItems <- names(propIncorrect)[propIncorrect > .25]
      subset <- subset(df, !word %in% removeItems)
      return(subset)}))
  }
  
  if (excl50){
    returnData <- c(returnData, lapply(data, function(df){
      dataItem <- split(df$correct, df$word)
      propIncorrect <- sapply(dataItem, function(x) 1-mean(x, na.rm = TRUE))
      removeItems <- names(propIncorrect)[propIncorrect > .50]
      subset <- subset(df, !word %in% removeItems)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!excl25 & !excl50 & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision9 <- function(data, exclError = FALSE, exclErrorPrevTrial = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclError){
    returnData <- c(returnData, lapply(data, subset, correct))
  }
  
  if (exclErrorPrevTrial){
    returnData <- c(returnData, lapply(data, subset, correct & (lag(correct) | observation != lag(observation) | block != lag(block) | is.na(lag(correct))))) #first value for the lag function returns NA at the very first trial because there is no previous observation, and also when there is no response on the previous trial, in which case the response should not be excluded (only when there is an error on the previous trial) 
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclError &  !exclErrorPrevTrial & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision10 <- function(data, exclFirstTrial = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclFirstTrial){
    returnData <- c(returnData, lapply(data, subset, !trial_code %in% c(seq(1, 351, 50))))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclFirstTrial & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision11 <- function(data, exclNegRTs = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclNegRTs){
    returnData <- c(returnData, lapply(data, subset, duration >= 0))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclNegRTs & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision12 <- function(data, excl25250 = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (excl25250){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$duration, df$observation)
      propFaster250 <- sapply(dataParticipant, function(x) sum(x < 250, na.rm = TRUE)/length(x)) #proportion of trials with duration faster than 250 ms
      removeParticipants <- names(propFaster250)[propFaster250 > .25]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  if (keepRegardless){
      returnData <- c(returnData, data)
  }
    
  if (!excl25250 & !keepRegardless){
      print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision13 <- function(data, exclTO50 = FALSE, keepRegardless = FALSE){
  returnData <- list()    
  
  if (exclTO50){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$duration, df$observation)
      propTO <- sapply(dataParticipant, function(x) sum(x >= 3000, na.rm = TRUE)/length(x)) #proportion of timeouts, operationalized as trials with a duration equal to or above 3000 ms
      removeParticipants <- names(propTO)[propTO > .50] 
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  ##this choice was not phrased correctly (should have been 3*SD above the mean), we decided to not include this option, but the code is included below
  #if (exclTO3SD){
  #  returnData <- c(returnData, lapply(data, function(df){
  #    dataParticipant <- split(df$duration, df$observation)
  #    propTO <- sapply(dataParticipant, function(x) sum(x >= 3000, na.rm = TRUE)/length(x)) #proportion of timeouts, operationalized as trials with a duration equal to or above 3000 ms
  #    if (mean(propTO, na.rm  = TRUE) == 0){ #this part was added later to account for the situation wherein RTs above 1000ms, 1500ms, and so on are removed before running this function
  #      removeParticipants <- "nobody"
  #    }
  #    else {
  #      removeParticipants <- names(propTO)[propTO <= (mean(propTO, na.rm  = TRUE) - 3*sd(propTO, na.rm = TRUE))] 
  #    }
  #    subset <- subset(df, !observation %in% removeParticipants)
  #    return(subset)}))
  #}

  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclTO50 & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision14 <- function(data, exclPart2SD = FALSE, exclPart2.5SD = FALSE, exclPartMAD = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclPart2SD){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$duration, df$observation)
      meanDuration <- sapply(dataParticipant, function(x) mean(x, na.rm = TRUE))
      removeParticipants <- names(meanDuration)[meanDuration > mean(meanDuration, na.rm = TRUE) + 2*sd(meanDuration, na.rm = TRUE)]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (exclPart2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$duration, df$observation)
      meanDuration <- sapply(dataParticipant, function(x) mean(x, na.rm = TRUE))
      removeParticipants <- names(meanDuration)[meanDuration > mean(meanDuration, na.rm = TRUE) + 2.5*sd(meanDuration, na.rm = TRUE)]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (exclPartMAD){
    returnData <- c(returnData, lapply(data, function(df){
      dataParticipant <- split(df$duration, df$observation)
      meanDuration <- sapply(dataParticipant, function(x) mean(x, na.rm = TRUE))
      medianDuration <- median(meanDuration, na.rm = TRUE)
      madDuration <- mad(meanDuration, na.rm = TRUE)
      removeParticipants <- names(meanDuration)[meanDuration < medianDuration - 3*madDuration | meanDuration > medianDuration + 3*madDuration]
      subset <- subset(df, !observation %in% removeParticipants)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclPart2SD & !exclPart2.5SD & !exclPartMAD & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision15 <- function(data, exclIt2.5SD = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclIt2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dataItem <- split(df$duration, df$word)
      meanDuration <- sapply(dataItem, function(x) mean(x, na.rm = TRUE))
      removeItems <- names(meanDuration)[meanDuration > mean(meanDuration, na.rm = TRUE) + 2.5*sd(meanDuration, na.rm = TRUE)]
      subset <- subset(df, !word %in% removeItems)
      return(subset)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclIt2.5SD & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision16 <- function(data, excl50ms = FALSE, excl100ms = FALSE, excl150ms = FALSE, excl160ms = FALSE, excl200ms = FALSE, excl250ms = FALSE, excl300ms = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (excl50ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 50))
  }
  
  if (excl100ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 100))
  }
  
  if (excl150ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 150))
  }
  
  if (excl160ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 160))
  }
  
  if (excl200ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 200))
  }
  
  if (excl250ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 250))
  }

  if (excl300ms){
    returnData <- c(returnData, lapply(data, subset, duration >= 300))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!excl50ms & !excl100ms & !excl150ms & !excl160ms & !excl200ms & !excl250ms  & !excl300ms & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision17 <- function(data, excl1000ms = FALSE, excl1500ms = FALSE, excl1600ms = FALSE, excl2000ms = FALSE, excl2500ms = FALSE, excl3000ms = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (excl1000ms){
    returnData <- c(returnData, lapply(data, subset, duration <= 1000))
  }
  
  if (excl1500ms){
    returnData <- c(returnData, lapply(data, subset, duration <= 1500))
  }
  
  if (excl1600ms){
    returnData <- c(returnData, lapply(data, subset, duration <= 1600))
  }
  
  if (excl2000ms){
    returnData <- c(returnData, lapply(data, subset, duration <= 2000))
  }
  
  if (excl2500ms){
    returnData <- c(returnData, lapply(data, subset, duration <= 2500))
  }
  
  if (excl3000ms){
    returnData <- c(returnData, lapply(data, subset, duration <= 3000))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!excl1000ms & !excl1500ms & !excl1600ms & !excl2000ms & !excl2500ms & !excl3000ms & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

decision18 <- function(data, exclTrial5perc = FALSE, exclTrial2SD = FALSE, exclTrial2.5SD = FALSE, exclTrial3SD = FALSE, winsTrial3SD = FALSE, exclTrialIQR = FALSE, exclIt2.5SD = FALSE, exclIt3SD = FALSE, winsIt2SD = FALSE, exclPart5perc = FALSE, exclPart2SD = FALSE, exclPart2.5SD = FALSE, exclPart3SD = FALSE, exclPart5SD = FALSE, winsPart2SD = FALSE, winsPart2.5SD = FALSE, exclCond2.5SD = FALSE, exclCondIQR = FALSE, winsCond2SD = FALSE, exclItCond2.5SD = FALSE, exclPartCond2SD = FALSE, exclPartCond2.5SD = FALSE, exclPartCond3SD = FALSE, exclPartBlock3SD = FALSE, keepRegardless = FALSE){
  returnData <- list()
  
  if (exclTrial5perc){
    returnData <- c(returnData, lapply(data, subset, duration > quantile(duration, probs = .05, na.rm = TRUE) & duration < quantile(duration, probs = .95, na.rm = TRUE)))
  }
  
  if (exclTrial2SD){
    returnData <- c(returnData, lapply(data, subset, duration > mean(duration, na.rm = TRUE) - 2*sd(duration, na.rm = TRUE) & duration < mean(duration, na.rm = TRUE) + 2*sd(duration, na.rm = TRUE)))
  }
  
  if (exclTrial2.5SD){
    returnData <- c(returnData, lapply(data, subset, duration > mean(duration, na.rm = TRUE) - 2.5*sd(duration, na.rm = TRUE) & duration < mean(duration, na.rm = TRUE) + 2.5*sd(duration, na.rm = TRUE)))
  }
  
  if (exclTrial3SD){
    returnData <- c(returnData, lapply(data, subset, duration > mean(duration, na.rm = TRUE) - 3*sd(duration, na.rm = TRUE) & duration < mean(duration, na.rm = TRUE) + 3*sd(duration, na.rm = TRUE)))
  }
  
  if (winsTrial3SD){
    returnData <- c(returnData, lapply(data, function(df){
      upperCutoff <- mean(df$duration, na.rm = TRUE) + 3*sd(df$duration, na.rm = TRUE)
      lowerCutoff <- mean(df$duration, na.rm = TRUE) - 3*sd(df$duration, na.rm = TRUE)
      df$duration[df$duration >= upperCutoff] <- upperCutoff
      df$duration[df$duration <= lowerCutoff] <- lowerCutoff
      return(df)}))
  }
  
  if (exclTrialIQR){
    returnData <- c(returnData, lapply(data, subset, duration <= quantile(duration, probs = .75, na.rm = TRUE) + 3*IQR(duration, na.rm = TRUE)))
  }
  
  if (exclIt2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(word) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2.5 * sdDuration & duration < meanDuration + 2.5 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclIt3SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(word) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 3 * sdDuration & duration < meanDuration + 3 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (winsIt2SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(word) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE),
          upperCutoff = meanDuration + 2 * sdDuration,
          lowerCutoff = meanDuration - 2 * sdDuration,
          duration = ifelse(duration >= upperCutoff, upperCutoff, ifelse(duration <= lowerCutoff, lowerCutoff, duration))
        ) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration, -upperCutoff, -lowerCutoff)
      return(dfClean)}))
  }
  
  if (exclPart5perc){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          fastest5perc = quantile(duration, probs = .05, na.rm = TRUE),
          slowest5perc = quantile(duration, probs = .95, na.rm = TRUE)
        ) %>%
        filter(duration > fastest5perc & duration < slowest5perc) %>%
        ungroup() %>%
        select(-fastest5perc, -slowest5perc)
      return(dfClean)}))
  }
  
  if (exclPart2SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2 * sdDuration & duration < meanDuration + 2 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPart2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2.5 * sdDuration & duration < meanDuration + 2.5 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPart3SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 3 * sdDuration & duration < meanDuration + 3 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPart5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 5 * sdDuration & duration < meanDuration + 5 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (winsPart2SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE),
          upperCutoff = meanDuration + 2 * sdDuration,
          lowerCutoff = meanDuration - 2 * sdDuration,
          duration = ifelse(duration >= upperCutoff, upperCutoff, ifelse(duration <= lowerCutoff, lowerCutoff, duration))
        ) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration, -upperCutoff, -lowerCutoff)
      return(dfClean)}))
  }
  
  if (winsPart2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE),
          upperCutoff = meanDuration + 2.5 * sdDuration,
          lowerCutoff = meanDuration - 2.5 * sdDuration,
          duration = ifelse(duration >= upperCutoff, upperCutoff, ifelse(duration <= lowerCutoff, lowerCutoff, duration))
        ) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration, -upperCutoff, -lowerCutoff)
      return(dfClean)}))
  }
  
  if (exclCond2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(type) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2.5 * sdDuration & duration < meanDuration + 2.5 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclCondIQR){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(type) %>%
        mutate(
          Q3 = quantile(duration, probs = .75, na.rm = TRUE),
          IQR = IQR(duration, na.rm = TRUE)
        ) %>%
        filter(duration < Q3 + 1.5*IQR) %>%
        ungroup() %>%
        select(-Q3, -IQR)
      return(dfClean)}))
  }
  
  if (winsCond2SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(type) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE),
          upperCutoff = meanDuration + 2 * sdDuration,
          lowerCutoff = meanDuration - 2 * sdDuration,
          duration = ifelse(duration >= upperCutoff, upperCutoff, ifelse(duration <= lowerCutoff, lowerCutoff, duration))
        ) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration, -upperCutoff, -lowerCutoff)
      return(dfClean)}))
  }
  
  if (exclItCond2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(word, type) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2.5 * sdDuration & duration < meanDuration + 2.5 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPartCond2SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation, type) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2 * sdDuration & duration < meanDuration + 2 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPartCond2.5SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation, type) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 2.5 * sdDuration & duration < meanDuration + 2.5 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPartCond3SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation, type) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 3 * sdDuration & duration < meanDuration + 3 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (exclPartBlock3SD){
    returnData <- c(returnData, lapply(data, function(df){
      dfClean <- df %>%
        group_by(observation, block) %>%
        mutate(
          meanDuration = mean(duration, na.rm = TRUE),
          sdDuration = sd(duration, na.rm = TRUE)
        ) %>%
        filter(duration > meanDuration - 3 * sdDuration & duration < meanDuration + 3 * sdDuration) %>%
        ungroup() %>%
        select(-meanDuration, -sdDuration)
      return(dfClean)}))
  }
  
  if (keepRegardless){
    returnData <- c(returnData, data)
  }
  
  if (!exclTrial5perc & !exclTrial2SD & !exclTrial2.5SD & !exclTrial3SD & !winsTrial3SD & !exclTrialIQR & !exclIt2.5SD & !exclIt3SD & !winsIt2SD & !exclPart5perc & !exclPart2SD & !exclPart2.5SD & !exclPart3SD & !exclPart5SD & !winsPart2SD & !winsPart2.5SD & !exclCond2.5SD & !exclCondIQR & !winsCond2SD & !exclItCond2.5SD & !exclPartCond2SD & !exclPartCond2.5SD & !exclPartCond3SD & !exclPartBlock3SD & !keepRegardless){
    print("Set at least one of the processing options to TRUE. Now you just get an empty list")
  }
  return(returnData)
}

removeNonwords <- decision19 <- function(data){
  returnData <- lapply(data, subset, class != "nonword")
  return(returnData)
} 

removeFillerwords <- decision20  <- function(data){
  returnData <- lapply(data, subset, class == "nonword" | which == "target")
  return(returnData)
}

ztransform <- decision21  <- function(data){
  returnData <- lapply(data, function(df) {
    df$zRT <- ave(df$duration, df$observation, FUN = scale)
    return(df)
  })
  return(returnData)
}

itemPriming <- decision22 <- function(data){
  returnPriming <- lapply(data, function(df) {
    itemMeans <- aggregate(zRT ~ target_id + type, df, mean)
    itemDiff <- itemMeans[itemMeans$type == "unrelated", ]$zRT - itemMeans[itemMeans$type == "related", ]$zRT
    itemDiff <- data.frame(target_id = unique(itemMeans$target_id), itemPriming = itemDiff)
    return(itemDiff)
  })
  return(returnPriming)
}

#matches item-level priming effects across languages
#necessary because item exclusions could result in different dimensions of the item-level priming effects
matchItems <- function(list1, list2){
  list1 <- lapply(list1, as.data.frame)
  list2 <- lapply(list2, as.data.frame)
  
  returnMatchItems <- Map(function(x, y) {
    full_join(x, y, by = "target_id") %>%
      select(-target_id)
  }, list1, list2)
  return(returnMatchItems)
}

correlations <- function(priming){
  correlation <- unlist(lapply(priming, function(x){cor(x[[1]], x[[2]], use = "complete.obs")}))
  p_value <- unlist(lapply(priming, function(x){cor.test(x[[1]], x[[2]], use = "complete.obs", alternative = "greater")$p.value}))
  return(data.frame(correlation, p_value))
}  

# Function to transform the question labels to decisions in the multiverse wherein each refers to a decision function and the actual choice one makes (the number after the underscore)
processLabels <- function(x) {
  parts <- strsplit(x, "_")[[1]]
  parts[1] <- ifelse(as.numeric(substr(parts[1], 2, nchar(parts[1]))) < 37, ceiling(as.numeric(substr(parts[1], 2, nchar(parts[1])))/2), as.numeric(substr(parts[1], 2, nchar(parts[1])))-18)
  parts[1] <- paste0("decision", parts[1])
  paste(parts, collapse = "_")
}

# Function to check whether the last two steps indeed correspond with calculating priming effect and correlating them. If not the suggested analysis pipeline does not make sense
checkLastTwo <- function(list) {
  # Extract the last two elements
  lastTwo <- tail(list, 2)
  
  # Check if they match the right steps
  matches <- (lastTwo[1] == "decision22") & (lastTwo[2] == "decision23")
  return(matches)
}

path <- function(data, fcall) {
  parts <- strsplit(fcall, "_")[[1]]
  funcName <- parts[1]
  argIndex <- as.numeric(parts[2])
  func <- get(funcName) #gets the function itself
  args <- formals(func) #gets the arguments of the function
  argName <- names(args)[argIndex + 1] #gets the name of the argument that needs to be set to true; this is always the argument + 1 because the first argument of all decision functions involves the data
  if (!is.na(argIndex)){args[[argName]] <- TRUE} #some functions that don't involve a decision (from decision19 onwards), only have the data as an argument, hence this if statement
  args_mod <- c(list(data), args[-1]) #remove the first argument from each function (data)
  do.call(func, args_mod)
}

pipeline <- function(paths){
  en_data <- reduce(paths, path, .init = en_SPAML)
  de_data <- reduce(paths, path, .init = de_SPAML)
  correlation <- matchItems(en_data, de_data) %>% correlations() %>% unlist()
  return(correlation)
}

stackedBar <- function(prop, main = "Proportion Bar", col = hcl.colors(length(prop), palette = "mint"), labels = NULL, posLegend = c(0, 0), margins = c(2.5, 2, 1, 10) + 0.1) {
  # Plot area setup without axis labels and box
  par(mar = margins) 
  plot(1, type = "n", xlim = c(0, 1), ylim = c(0.33, 0.65), xaxt = 'n', yaxt = 'n', xlab = "", ylab = "", main = main, bty = "n", cex.main = 0.8)
  
  # Starting point for the first rectangle
  start <- 0
  
  # Draw each segment
  for (i in seq_along(prop)) {
    rect(start, 0.45, start + prop[i], 0.65, col = col[i], border = "grey")
    start <- start + prop[i]  # Update start for the next segment
  }
  
  rect(0, 0.45, 1, 0.65, border = "darkgrey", lwd = 1)
  
  # Add a legend if labels are provided
  if (!is.null(labels)) {
    par(xpd = TRUE)
    legend("topright", inset = posLegend, legend = labels, fill = col, title = "", cex = 0.8, bty = "n")
    par(xpd = FALSE)
  }
}

removeLeadingZero <- function(x) {
  sapply(x, function(v) printp(v, digits = 2))
}

#old function to calculate correlations, didn't take into account that dimensions could differ because of differential item exclusions
#correlations <- function(de_data, en_data){
#  correlation <- mapply(cor, de_data, en_data)
#  p_value <- mapply(function(x, y) {
#    test_result <- cor.test(x, y)
#    return(test_result$p.value)
#  }, de_data, en_data)
#  return(data.frame(correlation, p_value))
#}
