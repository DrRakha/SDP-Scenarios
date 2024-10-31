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

# Define the classifiers
 
 
# Define the classifiers

targetMetricOptions <- c("ROC")
#classifiers <- data.frame("Models" = c("naive_bayes", "pda", "multinom", "gbm", "lda2", "rpart2", "glmboost", "rotationForest", "C5.0", "mlpWeightDecay", "mlp", "LMT", "knn", "svmRadial", "kknn", "J48", "svmLinear", "JRip", "rf", "LogitBoost", "nnet", "avNNet", "AdaBoost.M1", "rpart", "RRF", "ranger", "ada", "AdaBag"))
classifiers <- data.frame("Models" = c("naive_bayes", "pda", "multinom", "gbm", "lda2", "rpart2", "glmboost", "rotationForest", "C5.0", "mlpWeightDecay", "mlp", "LMT", "knn", "svmRadial", "kknn", "J48", "svmLinear", "JRip", "rf", "LogitBoost", "nnet", "avNNet", "AdaBoost.M1", "rpart", "RRF", "ranger", "ada", "AdaBag"))


#classifiers <- data.frame("Models" = c("mlp", "nnet", "knn", "C5.0", "mlpWeightDecay", "RRF", "rotationForest"))




expModeOptions <- c("Exp1", "Exp4")
tuningMethods <- c("none", "grid")

# Define the data path
datapath <- "C:/TMU/postdoc-TMU/SDP-Runs"
pathToSave = "C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results/Results_Figures" 
# Load dataset names
trainingDataFolder <- paste("/Datasets_TrainingOneReleaseTest_", expModeOptions[1], "/", sep = "")
optiFiles <- list.files(paste(datapath, trainingDataFolder, sep = ""))
optiFiles <- optiFiles[grepl("^.*\\.csv", optiFiles)]
datasets <- gsub(".csv", "", optiFiles)


included_datasetsNASA <- c("MC", "ar3", "ar4", "ar5", "ar6", "PC")
included_datasetsPROMISE <- c("ant", "camel", "forrest", "ivy", "jedit", "xalan", 
                       "poi", "prop", "log4j", "lucene", "synapse", 
                       "pbeans", "velocity", "xerces")
included_datasetsEclipse <- c("eclipse")
included_datasetsApache <- c("artemis", "mng", "openjpa", "qpid")



filtered_datasetsNASA  <- datasets[grep(paste(included_datasetsNASA , collapse = "|"), datasets)]
filtered_datasetsPROMISE  <- datasets[grep(paste(included_datasetsPROMISE , collapse = "|"), datasets)]
filtered_datasetsEclipse <- datasets[grep(paste(included_datasetsEclipse , collapse = "|"), datasets)]
filtered_datasetsApache  <- datasets[grep(paste(included_datasetsApache , collapse = "|"), datasets)]


# Initialize data frames to store results
variable_importance_data <- data.frame()

# Initialize counters for files successfully read
total_files <- 0
successful_reads <- 0

# Loop over datasets, experiment modes, classifiers, and tuning methods
for (dataset in datasets) {
  for (expModeSelection in expModeOptions) {
    for (iClassifier in 1:nrow(classifiers)) {
      mlalgo <- as.character(classifiers[iClassifier, "Models"])
      
      for (tuningMethod in tuningMethods) {
        # Define file path
       # file_path <- paste0("C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results/Results_", expModeSelection, "_VarImp/ReleaseBased_manualGrid_varImpor_Accuracy_", dataset, "_", mlalgo, "_bootstrap_boot_tuningMethod_", tuningMethod, "_", expModeSelection, ".csv")
        file_path <- paste0("C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results/Results_", expModeSelection, "_VarImp/ReleaseBased_manualGrid_varImpor_",targetMetricOptions,"_", dataset, "_", mlalgo, "_bootstrap_boot_tuningMethod_", tuningMethod, "_5_", expModeSelection, ".csv")
        total_files <- total_files + 1
        
        tryCatch({
          var_importance <- read.csv(file_path, sep = ",", header = FALSE, stringsAsFactors = FALSE, skip = 1)[, c(1, 2)]
          colnames(var_importance) <- c("Variable", "Importance")
          
          # Add Experiment Mode, Classifier, Dataset, and Tuning Method to the data
          if (dataset %in% filtered_datasetsNASA) {
            var_importance$Variable <- paste0("1_" ,var_importance$Variable)
          }
          
          if (dataset %in% filtered_datasetsPROMISE) {
            var_importance$Variable <- paste0("2_" ,var_importance$Variable)
          }
          
          if (dataset %in% filtered_datasetsEclipse) {
            var_importance$Variable <- paste0("3_" ,var_importance$Variable)
          }
          
          if (dataset %in% filtered_datasetsApache) {
            var_importance$Variable <- paste0("4_" ,var_importance$Variable)
          }
 
          var_importance$ExpMode <- expModeSelection
          var_importance$Classifier <- mlalgo
          var_importance$Dataset <- dataset
          var_importance$TuningMethod <- tuningMethod
          
          # Combine data
          variable_importance_data <- rbind(variable_importance_data, var_importance)
          
          successful_reads <- successful_reads + 1
        }, error = function(err) {
          cat("An error occurred with file:", file_path, "\n", conditionMessage(err), "\n")
        })
      }
    }
  }
}

# Reshape the data to compare "none" and "grid" tuning methods across Exp1 and Exp4
importance_wide <- reshape2::dcast(variable_importance_data, Variable + Classifier + Dataset ~ ExpMode + TuningMethod, value.var = "Importance")

# Compute the differences between "grid" and "none" for each experiment mode and rename them
importance_wide$IVDP <- with(importance_wide, Exp1_grid - Exp1_none)
importance_wide$CVDP <- with(importance_wide, Exp4_grid - Exp4_none)


importance_wide <- variable_importance_data %>%
  pivot_wider(names_from = c(  "ExpMode", "TuningMethod"), values_from = Importance)


importance_wide <- importance_wide %>%
  mutate(IVDP = Exp1_grid - Exp1_none,
         CVDP = Exp4_grid - Exp4_none)


# Reshape for plotting
difference_long <- importance_wide %>%
  pivot_longer(cols = c(IVDP, CVDP), names_to = "ExpMode", values_to = "Difference")



# Initialize a list to store results
results_list <- list()

# Loop through each variable
# Loop over each variable
for (var in unique(difference_long$Variable)) {
  data_ivdp <- subset(difference_long, Variable == var & ExpMode == "IVDP")$Difference
  data_cvdp <- subset(difference_long, Variable == var & ExpMode == "CVDP")$Difference
  
  print(length(data_ivdp))
  
  if (length(data_ivdp) > 0 && length(data_cvdp) > 0) {
    # Perform Wilcoxon test
    stTest <- wilcox.test(data_ivdp, data_cvdp,alternative = "g", paired = FALSE)
    
    # Perform Cliff's delta
    Cliff <- cliff.delta(data_ivdp, data_cvdp)
    
    # Significance levels
    if (stTest$p.value <= 0.05 && stTest$p.value > 0.01) {
      startVal <- "*"
    } else if (stTest$p.value <= 0.01 && stTest$p.value > 0.001) {
      startVal <- "**"
    } else if (stTest$p.value <= 0.001) {
      startVal <- "***"
    } else {
      startVal <- "(-)"
    }
    
 
    
    if(Cliff$magnitude=="small")
    {
      EffSizeShort = "S"
    } else if(Cliff$magnitude=="medium")
    {
      
      EffSizeShort = "M"
    } else if(Cliff$magnitude=="large")
    {
      
      EffSizeShort = "L"
    }  else  
    {
      
      EffSizeShort = "N"
    }
    
    
    # Store results
    results_list[[var]] <- c(Variable = var, 
                             Wilcox_p = stTest$p.value,
                             Cliff_magnitude = Cliff$estimate,
                             Significance = startVal,
                             EffectSize = EffSizeShort)
  }
}


# Convert list to data frame
results_df <- as.data.frame(do.call(rbind, results_list), stringsAsFactors = FALSE)
results_df$Wilcox_p <- as.numeric(results_df$Wilcox_p)
results_df$Cliff_magnitude <- as.numeric(results_df$Cliff_magnitude)



# Generate facet labels
results_df <- results_df %>%
  mutate(FacetLabel = paste0(
    Variable, "\n", 
    round(abs(Cliff_magnitude), 2), "(", EffectSize, ")", Significance
  ))

# Convert to named vector for facet labels
facet_labels_vector <- setNames(tolower(results_df$FacetLabel), results_df$Variable)

plot_data <- merge(difference_long, results_df, by = "Variable")

 

ggplot(plot_data, aes(x = factor(ExpMode, levels = c("IVDP", "CVDP")), y = Difference, fill = ExpMode)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("IVDP" = "lightblue", "CVDP" = "gray")) +
  facet_wrap(~ Variable, scales = "free", ncol = 9, labeller = labeller(Variable = facet_labels_vector)) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Distribution of Importance Differences",
       x = "SDP Scenario",
       y = "Difference in Importance") +
  theme_minimal() +
  theme(plot.margin = margin(b = 30, l = 10,t = 1), 
        plot.tag.position = c(0.52, -0.05), 
        plot.tag = element_text(size = 6),  
        axis.text.x = element_text(face = "bold", size = 7, angle = 90, vjust = 0.5, hjust = 1), 
        axis.title.y = element_text(size = 13),
        strip.text = element_text(size = 10), 
        panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.position = "none")

 

dev.copy2pdf(width=14.52,height=19.2 , file=paste(pathToSave,"/",targetMetricOptions,"VarriableImportanceDelta.pdf",sep=''));
dev.off();

 


plot_data_dt <- as.data.table(plot_data)
# Calculate maximum differences for each Variable and ExpMode
max_diff_dt <- plot_data_dt[, .(MaxDiff = max(Difference, na.rm = TRUE)), by = .(Variable, ExpMode)]
# Calculate maximum differences for each Variable and ExpMode using aggregate






max_diff_base <- aggregate(Difference ~ Variable + ExpMode, data = plot_data, FUN = max, na.rm = TRUE)
# Rename the resulting column
colnames(max_diff_base)[colnames(max_diff_base) == "Difference"] <- "MaxDiff"

# Print the result
print(max_diff_base)

 # Filter median_diff to include only IVDP
ivdp_diff <- max_diff_base %>%
  filter(ExpMode == "IVDP")


ranked_variables_ivdp <- ivdp_diff %>%
  arrange(MaxDiff) %>%
  pull(Variable)


# Filter and rank variables for CVDP in descending order
cvdp_diff <- max_diff_base %>%
  filter(ExpMode == "CVDP") %>%
  arrange(MaxDiff)

ranked_variables_cvdp <- cvdp_diff %>% pull(Variable)

 
 
 ggplot(ivdp_diff, aes(x = factor(Variable, levels = ranked_variables_ivdp), y = MaxDiff, fill = ExpMode)) +
   # geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bars for max value
   geom_jitter(data = plot_data %>% filter(ExpMode == "IVDP"),
               aes(x = factor(Variable, levels = ranked_variables_ivdp), y = Difference),
               width = 0.2, height = 0, color = "#6FC0DB", fill = "lightblue", shape = 21, stroke = 0.5, alpha = 0.7) +  # Scatter plot of all Differences
   geom_point(data = ivdp_diff, aes(x = factor(Variable, levels = ranked_variables_ivdp), y = MaxDiff),
              color = "lightblue", size = 2, shape = 21, fill = "red") +  # Red point for max value
   scale_fill_manual(values = c("IVDP" = "lightblue")) +
   coord_flip() +  # Flip coordinates to make it horizontal
   scale_y_continuous(position = "right", limits = c(-500, 1500)) +  # Move y-axis to the right and limit range +  # Move y-axis to the right
   labs(title = "IVDP Max Differences and Distribution",
        x = "Feature Name (Ranked by Max Difference)",
        y = "Varriable Importance Difference") +
   theme(plot.margin = margin(b = 30, l = 10,t = 1,r = 1), 
         plot.tag.position = c(0.52, -0.05), 
         plot.tag = element_text(size = 8),  
         axis.text.x = element_text(face = "bold", size = 10, angle = 90, vjust = 0.5, hjust = 1), 
         axis.title.y = element_text(size = 17),
         strip.text = element_text(size = 14), 
         panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1),
         plot.title = element_text(hjust = 0.5)) +  # Center the title
   theme(legend.position = "none")
 
 
dev.copy2pdf(width=9.52,height=18.2 , file=paste(pathToSave,"/",targetMetricOptions,"VarriableImportanceIVDPMax.pdf",sep=''));
dev.off();

 

 

ggplot(cvdp_diff, aes(x = factor(Variable, levels = ranked_variables_cvdp), y = MaxDiff, fill = ExpMode)) +
 # geom_bar(stat = "identity", position = "dodge", color = "black") +  # Bars for max value
  geom_jitter(data = plot_data %>% filter(ExpMode == "CVDP"),
              aes(x = factor(Variable, levels = ranked_variables_cvdp), y = Difference),
      width = 0.2, height = 0, color = "darkgray", fill = "gray", shape = 21, stroke = 0.5, alpha = 0.7) +# Scatter plot of all Differences
  geom_point(data = cvdp_diff, aes(x = factor(Variable, levels = ranked_variables_cvdp), y = MaxDiff),
             color = "darkgray", size = 2, shape = 21, fill = "red") +  # Red point for max value
  scale_fill_manual(values = c("CVDP" = "gray")) +
  coord_flip() +  # Flip coordinates to make it horizontal
  scale_y_continuous(position = "right", limits = c(-500, 1500))  +  # Move y-axis to the right
  labs(title = "CVDP Max Differences and Distribution",
       x = "Feature Name (Ranked by Max Difference)",
       y = "Varriable Importance Difference") +
  theme(plot.margin = margin(b = 30, l = 10,t = 1,r = 1), 
        plot.tag.position = c(0.52, -0.05), 
        plot.tag = element_text(size = 8),  
        axis.text.x = element_text(face = "bold", size = 10, angle = 90, vjust = 0.5, hjust = 1), 
        axis.title.y = element_text(size = 17),
        strip.text = element_text(size = 14), 
        panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1),
        plot.title = element_text(hjust = 0.5)) +  # Center the title
  theme(legend.position = "none")
 

dev.copy2pdf(width=9.52,height=18.2 , file=paste(pathToSave,"/",targetMetricOptions,"VarriableImportanceCVDPMax.pdf",sep=''));
dev.off();




 


 

# Print the summary of files read
cat("\nSummary:\n")
cat("Total files attempted:", total_files, "\n")
cat("Successful reads:", successful_reads, "\n")