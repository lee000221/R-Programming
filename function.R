library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(lubridate)
library(readr)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(caret)
library(missForest)
library(mice)
library(gbm)
library(randomForest)
library(reshape2)
library(rpart)
library(rpart.plot)
library(knitr)
library(scales)
library(e1071)
library(patchwork)
library(GGally)

# function
extract_near_ktm_lrt <- function(text) {
  pattern <- "\\bNear KTM/LRT\\b"
  if (!is.na(text)) {
    if (str_detect(text, pattern)) {
      return('yes')
    } else {
      return('no')
    }
  } else {
    return(text)
  }
}

extractInputOutput <- function(data, output_column_name) {
  # Extract output data
  output_data <- data[[output_column_name]]
  
  # Extract input data
  input_data <- data[, !names(data) %in% output_column_name]
  
  # Return input and output data
  return(list(input_data = input_data, output_data = output_data))
}

numericalImputation <- function(X_num, strategy = "mode") {
  # Define the imputation method
  if (tolower(strategy) == "mode") {
    imputer_num <- function(x) {
      if (is.numeric(x)) {
        x[is.na(x)] <- as.numeric(names(table(x))[which.max(table(x))])
      }
      return(x)
    }
  } else {
    stop("Invalid imputation strategy.")
  }
  
  # Perform imputation
  X_num_imputed <- as.data.frame(lapply(X_num, imputer_num))
  return(X_num_imputed)
}

standardizerData <- function(data) {
  standardized_data <- scale(data)
  standardized_data <- as.data.frame(standardized_data)
  colnames(standardized_data) <- colnames(data)
  rownames(standardized_data) <- rownames(data)
  return(standardized_data)
}


calculate_metrics <- function(y_true, y_pred) {
  mae <- mean(abs(y_true - y_pred))
  r2 <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  return(list(R2_score = format(r2, digits = 4), MAE_score = format(mae, digits = 4)))
}

classification_results_metrics <- function(confusion_matrices) {
  num_classes <- nrow(confusion_matrices[[1]])
  num_models <- length(confusion_matrices)
  
  accuracy <- numeric(num_models)
  precision <- matrix(0, nrow = num_models, ncol = num_classes)
  recall <- matrix(0, nrow = num_models, ncol = num_classes)
  f1_score <- matrix(0, nrow = num_models, ncol = num_classes)
  
  for (i in 1:num_models) {
    confusion_matrix <- confusion_matrices[[i]]
    
    # Calculate accuracy
    accuracy[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    # Calculate precision, recall, and F1 score for each class
    for (j in 1:num_classes) {
      true_positives <- confusion_matrix[j, j]
      false_positives <- sum(confusion_matrix[-j, j])
      false_negatives <- sum(confusion_matrix[j, -j])
      
      precision[i, j] <- true_positives / (true_positives + false_positives)
      recall[i, j] <- true_positives / (true_positives + false_negatives)
      f1_score[i, j] <- 2 * (precision[i, j] * recall[i, j]) / (precision[i, j] + recall[i, j])
    }
  }
  
  # Create a matrix with the results
  result_matrix <- cbind(accuracy, precision, recall, f1_score)
  
  
  
  colnames(result_matrix) <- c("Accuracy", paste0("Precision ", 1:num_classes),
                               paste0("Recall ", 1:num_classes), paste0("F1 Score ", 1:num_classes))
  # Format the results with 2 decimals and as percentages
  result_matrix <- round(result_matrix, 4)
  rownames(result_matrix) <- c("DT", "NB", "RF")
  
  return(result_matrix)
}

create_pie_chart <- function(data, variable) {
  counts <- table(data[[variable]])
  colors <- rainbow(length(counts))
  total <- sum(counts, na.rm = TRUE)
  
  percentages <- ifelse(is.na(names(counts)), NA, counts / total)
  percentages <- replace(percentages, is.na(percentages), 0)
  
  pie(counts,
      labels = NA, # Exclude labels within the pie chart
      col = colors,
      main = paste(variable, "Distribution"))
  
  legend("topright", legend = paste(names(counts), " (", percent(percentages), ")"),
         fill = colors, title = NULL,
         cex = 0.7, xpd = TRUE, inset = c(-0.1,0))
}










