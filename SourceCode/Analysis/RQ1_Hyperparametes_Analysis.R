library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

args <- commandArgs(TRUE)
require(lsr)
require(foreign)
require(MASS)
require(bootES)
library(ggplot2)
library(plyr)
library(reshape2)
library(scales)
library(RColorBrewer)
require(beanplot)
library(crop)
library(directlabels)
library(grid)
require(effsize)
library("stringr")
library(data.table)

targetMetricOptions <- c("Recall")
CountAllReadResultsRows <- 0
basepath = "C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results"


tl <- c(5)
t = tl

# Define hard-coded hyperparameter names for each classifier
hyperparameter_names <- list(
  "naive_bayes" = c("laplace", "usekernel", "adjust"),
  "pda" = c("lambda"),
  "multinom" = c("decay"),
  "gbm" = c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode"),
  "lda2" = c("dimen"),
  "rpart2" = c("maxdepth"),
  "glmboost" = c("mstop", "prune"),
  "rotationForest" = c("K", "L"),
  "C5.0" = c("trials", "mode", "winnow"),
  "mlpWeightDecay" = c("size", "decay"),
  "mlp" = c("size"),
  "LMT" = c("iter"),
  "knn" = c("k"),
  "svmRadial" = c("sigma", "C"),
  "kknn" = c("kmax", "distance", "kernel"),
  "J48" = c("C", "M"),
  "svmLinear" = c("C"),
  "JRip" = c("Numopt", "NumFolds", "MinWeights"),
  "rf" = c("mtry"),
  "LogitBoost" = c("nIter"),
  "nnet" = c("size", "decay"),
  "avNNet" = c("size", "decay", "bag"),
  "AdaBoost.M1" = c("mfinal", "maxdepth", "coeflearn"),
  "rpart" = c("cp"),
  "RRF" = c("mtry", "coefReg", "coefImp"),
  "ranger" = c("mtry", "splitrule", "size"),
  "ada" = c("iter", "maxdepth", "nu"),
  "AdaBag" = c("mfinal", "maxdepth")
)

all_hyperparameter_changes <- data.frame(Hyperparameter = character(),
                                         Algorithm = character(),
                                         ExpMode = character(),
                                         ChangeCount = integer(),
                                         TotalCount = integer(),
                                         Percentage = numeric(),
                                         stringsAsFactors = FALSE)

most_common_values <- data.frame(Hyperparameter = character(),
                                 Algorithm = character(),
                                 ExpMode = character(),
                                 MostCommonValue = character(),
                                 FrequencyPer = character(),
                                 stringsAsFactors = FALSE)

# Function to calculate frequency percentage
calculate_frequency_percentage <- function(value, total_count, values) {
  freq <- sum(values == value)
  freq_percentage <- (freq / total_count) * 100
  return(paste0(round(freq_percentage, 1), "%"))
}

for (targetMetric in targetMetricOptions) {
  enableBootstrap <- "boot"
  
  expModeOptions <- c("Exp1", "Exp4")
  tuneOptions <- c("none", "grid")
  
  for (expModeSelection in expModeOptions) {
    cat("################################ (Metric ", targetMetric, ")###################################### \n")
    cat("--- Start Loading for Experiment Mode : (", expModeSelection, ") \n")
    
    classifiers <- data.frame("Models" = c("naive_bayes", "pda", "multinom", "gbm", "lda2", "rpart2", "glmboost", "rotationForest", "C5.0", "mlpWeightDecay", "mlp", "LMT", "knn", "svmRadial", "kknn", "J48", "svmLinear", "JRip", "rf", "LogitBoost", "nnet", "avNNet", "AdaBoost.M1", "rpart", "RRF", "ranger", "ada", "AdaBag"))
    
    for (iClassifier in 1:nrow(classifiers)) {
      readFileSuccessfully <- FALSE
      mlalgo <- as.character(classifiers[iClassifier, "Models"])
      
      finalPerALLFinalNone <- NULL
      finalPerALLFinalOptimized <- NULL
      
      for (tuningMethod in tuneOptions) {
        cat("+++ Start Loading for Classifier: ", mlalgo, " and Tuning Method: (", tuningMethod, ") \n")
        
        metricGoal <- ifelse(targetMetric %in% c("ROC", "Recall"), targetMetric, NULL)
        TestMetricName <- ifelse(targetMetric == "Recall", "TestROC", "TestROC")
        
        basepath <- "C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results"
        resultsDataFolder <- paste("/Results_", expModeSelection, "_Params/", sep = "")
        
        perfFilePathFinalModel <- paste(
          basepath,
          "/Results_", expModeSelection, "_Params/ReleaseBased_manualGrid_param_", metricGoal, "_", mlalgo, "_bootstrap_", enableBootstrap, "_tuningMethod_", 
          tuningMethod, "_", t, "_", expModeSelection, ".csv", sep = ""
        )
        
        tryCatch({
          # Read the CSV file without headers
          allPerResults <- read.csv(perfFilePathFinalModel, sep = ",", header = FALSE, stringsAsFactors = FALSE)
          readFileSuccessfully <- TRUE
        }, error = function(err) {
          cat("An error occurred:", conditionMessage(err), "\n")
          readFileSuccessfully <- FALSE
        })
        
        if (!readFileSuccessfully) {
          cat("*** Error, Skipping Reading file for : ", mlalgo, "\n")
          next
        }
        
        # Use hard-coded hyperparameter names for column names
        if (mlalgo %in% names(hyperparameter_names)) {
          numHyperparams <- length(hyperparameter_names[[mlalgo]])
          colNames <- c(hyperparameter_names[[mlalgo]], "Dataset", "Classifier")
          colnames(allPerResults) <- colNames
        } else {
          cat("Error: Hyperparameter names not defined for classifier:", mlalgo, "\n")
          next
        }
        
        # Remove duplicate rows, keeping only the last occurrence based on "Dataset" and "Classifier"
        allPerResults <- allPerResults %>%
          group_by(Dataset, Classifier) %>%
          slice_tail(n = 1) %>%
          ungroup()
        
        if (tuningMethod == "none") {
          finalPerALLFinalNone <- allPerResults
        } else if (tuningMethod == "grid") {
          finalPerALLFinalOptimized <- allPerResults
        } else if (tuningMethod == "random") {
          finalPerALLFinalOptimized <- allPerResults
        }
        
      }
      
      if (!is.null(finalPerALLFinalNone) & !is.null(finalPerALLFinalOptimized)) {
        # Extract hyperparameter columns based on predefined names
        hyperparam_columns_none <- hyperparameter_names[[mlalgo]]
        hyperparam_columns_grid <- paste0(hyperparam_columns_none, "_grid")
        
        # Extract common columns for Dataset and Classifier
        dataset_classifier_columns <- c("Dataset", "Classifier")
        
        # Merge the data on Dataset and Classifier
        mergedData <- merge(finalPerALLFinalNone, finalPerALLFinalOptimized, by = dataset_classifier_columns, suffixes = c("_none", "_grid"))
        
        # Calculate the total number of observations for this classifier
        total_observations <- nrow(mergedData)
        
        # Calculate the number of changes for each hyperparameter and the percentage
        for (hp_col_none in hyperparam_columns_none) {
          # Derive the corresponding grid column name
          hp_col_grid <- paste0(hp_col_none, "_grid")
          
          if (hp_col_grid %in% colnames(mergedData)) {
            # Create full column names with suffixes
            none_col_name <- paste0(hp_col_none, "_none")
            grid_col_name <- hp_col_grid
            
            # Convert columns to character for comparison
            none_values <- as.character(mergedData[[none_col_name]])
            grid_values <- as.character(mergedData[[grid_col_name]])
            
            # Calculate the number of differences for this hyperparameter
            differences_count <- sum(none_values != grid_values)
            
            # Calculate the percentage of changes
            percentage_changes <- (differences_count / total_observations) * 100
            
            # Append to the data frame
            all_hyperparameter_changes <- rbind(all_hyperparameter_changes,
                                                data.frame(Hyperparameter = hp_col_none,
                                                           Algorithm = mlalgo,
                                                           ExpMode = expModeSelection,
                                                           ChangeCount = differences_count,
                                                           TotalCount = total_observations,
                                                           Percentage = percentage_changes,
                                                           stringsAsFactors = FALSE))
          } else {
            cat("Warning: No corresponding grid column for:", hp_col_none, "\n")
          }
        }
        
        # Calculate most common values and their frequency percentage
        for (hp_col in hyperparam_columns_none) {
          if (hp_col %in% colnames(finalPerALLFinalOptimized)) {
            # Extract the values
            hp_values <- finalPerALLFinalOptimized[[hp_col]]
            
            # Find the most common value
            most_common_value <- names(sort(table(hp_values), decreasing = TRUE))[1]
            
            # Calculate the frequency percentage
            freq_percentage <- calculate_frequency_percentage(most_common_value, total_observations, hp_values)
            
            # Append to the most_common_values data frame
            most_common_values <- rbind(most_common_values,
                                        data.frame(Hyperparameter = hp_col,
                                                   Algorithm = mlalgo,
                                                   ExpMode = expModeSelection,
                                                   MostCommonValue = most_common_value,
                                                   FrequencyPer = freq_percentage,
                                                   stringsAsFactors = FALSE))
          }
        }
      }
    }
  }
}

# Combine MostCommonValue and FrequencyPer into a single column
#most_common_values <- most_common_values %>%
#  mutate(MostCommonValueWithFrequency = paste(MostCommonValue, 
 #                                             paste0("(", FrequencyPer, ")"),
  #                                            sep = " ")) %>%
  #select(-MostCommonValue, -FrequencyPer) # Remove old columns

most_common_values <- most_common_values %>%
  mutate(MostCommonValueWithFrequency = paste(MostCommonValue, 
                                              paste0("(", FrequencyPer, ")"),
                                              sep = " ")) %>%
  dplyr::select(-MostCommonValue, -FrequencyPer) # Use dplyr::select to avoid conflicts



# Combine hyperparameter changes with most common values
combined_results <- merge(all_hyperparameter_changes, most_common_values, by = c("Hyperparameter", "Algorithm", "ExpMode"))

combined_results <- combined_results %>%
  mutate(across(ends_with("Percentage"), ~ paste0(round(.x, 1), "%")))

round_if_numeric <- function(x, digits = 2) {
  # Try to convert to numeric
  num_value <- as.numeric(x)
  
  # Check if conversion was successful
  if (!is.na(num_value)) {
    # Return rounded numeric value as character
    return(as.character(round(num_value, digits)))
  } else {
    # Return original value if not numeric
    return(x)
  }
}


round_if_numeric <- function(x, digits = 1) {
  # Try to convert to numeric
  num_value <- as.numeric(x)
  
  # Check if conversion was successful
  if (!is.na(num_value)) {
    # Return rounded numeric value as character
    return(as.character(round(num_value, digits)))
  } else {
    # Return original value if not numeric
    return(x)
  }
}

round_any <- function(x, digits = 1) {
  # Try to convert to numeric
  num_value <- as.numeric(x)
  

    return(round(num_value, digits))

}

# Apply rounding to columns that end with "MostCommonValueWithFrequency"
combined_results <- combined_results %>%
  mutate(across(ends_with("MostCommonValueWithFrequency"), ~ sapply(.x, round_if_numeric)))



combined_results <- combined_results %>%
  mutate(across(ends_with("Percentage"), ~ paste0(round_any(.x, 1), "%")))

combined_results <- combined_results %>%
  mutate(Percentage = paste0(round(Percentage, 1), "%") )


# Apply rounding to columns that end with "MostCommonValueWithFrequency"
combined_results <- combined_results %>%
  mutate(across(ends_with("MostCommonValueWithFrequency"), ~ sapply(.x, round_if_numeric)))

# Reshape the data to include percentages for each ExpMode
reshaped_data <- combined_results %>%
  pivot_wider(names_from = ExpMode, values_from = c(ChangeCount, TotalCount, Percentage, MostCommonValueWithFrequency), names_sep = "_") %>%
  arrange(Algorithm, Hyperparameter)

# Generate column names for LaTeX table
col_names <- c("Hyperparameter", "Algorithm",
               paste0("ChangeCount_", c("IVDP", "CVDP")),
               paste0("TotalCount_", c("IVDP", "CVDP")),
               paste0("Percentage_", c("IVDP", "CVDP")),
               paste0("CommonValue(%)_", c("IVDP", "CVDP")))

# Generate LaTeX table
latex_table <- kable(reshaped_data, format = "latex", booktabs = TRUE, 
                     caption = "Hyperparameter Changes and Most Common Values by Algorithm and Experiment Mode",
                     col.names = col_names)

# Print the LaTeX table

cat(latex_table)


reshaped_data <- dplyr::select(reshaped_data, -TotalCount_Exp1, -TotalCount_Exp4)
# Display the updated data
print(reshaped_data)





# Generate column names for LaTeX table
col_names <- c("Hyperparameter", "Algorithm",
               paste0("ChangeCount_", c("IVDP", "CVDP")),
              # paste0("TotalCount_", c("IVDP", "CVDP")),
               paste0("ChangeFreqency_", c("IVDP", "CVDP")),
               paste0("CommonValue(%)_", c("IVDP", "CVDP")))

# Generate LaTeX table
latex_table <- kable(reshaped_data, format = "latex", booktabs = TRUE, 
                     caption = "Hyperparameter Changes and Most Common Values by Algorithm and Experiment Mode",
                     col.names = col_names)

# Print the LaTeX table

cat(latex_table)