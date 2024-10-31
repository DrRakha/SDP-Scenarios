## Dr. Mohamed Sami Rakha.
## Post-Doctoral Fellow 
## Canada
## Results - Script
## Loading the libraries
## TSE Publication
## This R-script file includes the code that is used to generate Figures 
args <- commandArgs(TRUE)
require(lsr)
require(foreign)
require(MASS)
require(bootES)
library(ggplot2)
library(plyr) # might be not needed here anyway it is a must-have package I think in R 
library(reshape2) # to "melt" your dataset
library (scales) # it has a "rescale" function which is needed in heatmaps 
library(RColorBrewer) # for convenience of heatmap colors, it reflects your mood sometimes
require(beanplot)
library(plyr) # For the desc() function
library(ggplot2)
library(crop)
library(directlabels)
library(scales)
library(grid)
require(effsize)
library("stringr")
library(reshape2)
library(dplyr)
require(data.table)


set.seed(895)





#targetMetricOptions<- c("ROC","Recall","F")
#targetMetricOptions<- c("ROC" ,"Recall" ,"Precision" ,"F" ,"Accuracy")
targetMetricOptions<- c("ROC" ,"Recall" ,"Precision" ,"F" ,"Accuracy")
allpercentage_negative <- NULL
Allmerged_all_data <- NULL
EffectSizeAll <- ""
CountAllReadResultsRows <- 0

for ( targetMetric in (targetMetricOptions)) {

#targetMetric <- "F" #1-126
enableBootstrap <- "boot"
 
# Exp1 or Exp4
# Exp1 : one release in training - one release testing
# Exp4 : multiple releases in training - one release testing  
expMode <- "Exp4"
enableIVDP_one_release <- TRUE

###########################Start Loading Results Loop ############################
#################################################################################

expModeOptions<- c("Exp4","Exp1")
tuneOptions<- c("none","grid","random")


for ( expModeSelection in (expModeOptions)) {
  cat("################################ (Metric ",targetMetric,  " )###################################### \n")
  cat("--- Start Loading for Experiment Mode : (",expModeSelection, ") \n")
  
for ( tuningMethod in (tuneOptions)) {
   
  cat("+++ Start Loading for Tuning Method : (",tuningMethod, ") \n")
 
  
if (is.na(targetMetric))
{
  targetMetric = "ROC"
}

if (is.na(enableBootstrap))
{
  enableBootstrap = "boot"
  
}
 

if (is.na(tuningMethod))
{
  tuningMethod = "none"
}

if (is.na(expMode))
{
  expMode <- "Exp1"
}

metricGoal = NULL
if (targetMetric == "ROC")
{
  metricGoal = "ROC"
  TestMetricName <- "TestROC"
  
} else if (targetMetric == "Recall")
{
  metricGoal = "Recall"
  TestMetricName <- "TestROC"
  
  
} else{
  metricGoal = targetMetric
  
}

tl <- c(5)
t = tl

datapath = "C:/TMU/postdoc-TMU/SDP-Runs"
pathToSave = "C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results/Results_Figures" 

trainingDataFolder <- paste("/Datasets_TrainingOneReleaseTest_",expModeSelection,"/",  sep = "" )

optiFiles <-
  list.files(paste(datapath, trainingDataFolder, sep = ""))

optiFiles = optiFiles[grepl("^.*\\.csv", optiFiles)]
length(optiFiles)

datasetName = gsub(".csv", "", optiFiles)
basepath = "C:/TMU/postdoc-TMU/SDP-Analysis/Runs_Results"
#classifiers <-  c("naive_bayes", "pda", "gamboost", "gbm" , "glm", "rpart2", "glmboost")
classifiers <-  data.frame ("Models" =  c("naive_bayes", "pda", "multinom", "gbm" , "lda2", "rpart2", "glmboost","rotationForest","C5.0","mlpWeightDecay", "mlp","LMT","knn","svmRadial","kknn","J48","svmLinear","JRip","rf","LogitBoost", "nnet","avNNet","AdaBoost.M1", "rpart","RRF","ranger","ada","AdaBag"))
#classifiers <-  data.frame ("Models" =  c("naive_bayes", "pda", "multinom", "gbm" , "lda2", "rpart2", "glmboost","rotationForest","C5.0","LMT","knn","svmRadial","kknn","J48","svmLinear","JRip","rf","LogitBoost", "nnet","avNNet","AdaBoost.M1", "rpart","RRF","ranger","ada","AdaBag"))
 
#includedReleaseTraining <-  c("log4j-1.2.csv",    "forrest-0.8.csv",  "ant-1.5.csv" ,     "ar4.csv",          "ant-1.7.csv",      "lucene-2.4.csv",   "ant-1.6.csv",      "xerces-1.4.csv",  "ar6.csv",          "xalan-2.6.csv",    "ar5.csv",          "xalan-2.7.csv" ,   "poi-2.5.csv",      "velocity-1.6.csv",   "PC4.csv",    "ivy-2.0.csv",    "poi-3.0.csv" ,     "jedit-4.2.csv" ,   "jedit-4.1.csv" , "jedit-4.3.csv" ,   "camel-1.6.csv",         "camel-1.4.csv" , "prop-6.csv" ,   "prop-5.csv"    ,   "prop-4.csv"   ,      "eclipse-3.0.csv",  "synapse-1.2.csv",  "prop-3.csv")


#length(includedReleaseTraining)
 
resultsDataFolder <- paste("/Results_",expModeSelection,"/",  sep = "" )

optiFiles <-
  list.files(paste(basepath, resultsDataFolder, sep = ""))
 

maxPerdatasetIterationsALL <- NULL
OtherNonMaxPredatasetIterationsALL <- NULL
finalPerALLFinal <- NULL


for ( iClassifier in 1:nrow(classifiers)) {
 
  readFileSuccesfully <- FALSE
  mlalgo=as.character(classifiers[iClassifier,"Models"]) 
  perfFilePathFinalModel <- paste(
    basepath,
    "/Results_",expModeSelection,"/ReleaseBased_", "results_perf_",   metricGoal, "_", mlalgo, "_bootstrap_",  enableBootstrap, "_tuningMethod_", 
    tuningMethod,   "_",  t,"_",expModeSelection,  ".csv", sep = "" )
  
  
  tryCatch({
    
    allPerResults <- read.csv(perfFilePathFinalModel,sep = ",",header = TRUE,stringsAsFactors=FALSE)
    readFileSuccesfully <- TRUE
  
  }, error = function(err) {
    cat("An error occurred:", conditionMessage(err), "\n")
    
    print(mlalgo)
    readFileSuccesfully <- FALSE
    

    # stop("Error: da")
  })
 

  if(!readFileSuccesfully)
  {
    cat("*** Error, Skipping Reading file for : ",mlalgo, "\n")
    next
  }
 
  #For plots display Only
  allPerResults$Model[allPerResults$Model == "rotationForest"] <- "RotationRF"
  allPerResults$Model[allPerResults$Model == "AdaBoost.M1"] <- "AdaBoost"
  allPerResults$Model[allPerResults$Model == "ada"] <- "Ada"
  allPerResults$Model[allPerResults$Model == "mlp"] <- "MLP"
  allPerResults$Model[allPerResults$Model == "mlpWeightDecay"] <- "MLP_WD"
  allPerResults$Model[allPerResults$Model == "ranger"] <- "Ranger"
  allPerResults$Model[allPerResults$Model == "rf"] <- "RF"
  allPerResults$Model[allPerResults$Model == "rpart"] <- "RPart"
 
  
  
  
  #print(mlalgo)
 # print(nrow(allPerResults))
 
  if(!is.null(finalPerALLFinal))
  {
    finalPerALLFinal <- rbind (finalPerALLFinal, allPerResults)
  }
  else
  {
    finalPerALLFinal <- allPerResults
  }
  
}
 

CountAllReadResultsRows <- CountAllReadResultsRows + nrow(finalPerALLFinal)
print(paste(" Total Read Count   ", CountAllReadResultsRows, sep = ""))
  
if(tuningMethod=="grid" | tuningMethod=="random")
{
  
  if(expModeSelection=="Exp4") 
  {
  finalPerALLFinalOptimizedExp4    <- finalPerALLFinal 
  
  cat("Tuned Loaded Exp4: ",tuningMethod, " total = ",nrow(finalPerALLFinalOptimizedExp4), "\n")
  
  }
  else
  {
  finalPerALLFinalOptimizedExp1    <- finalPerALLFinal 
  
  cat("Tuned Loaded Exp1: ",tuningMethod, " total = ",nrow(finalPerALLFinalOptimizedExp1), "\n")
  }
  
} else if(tuningMethod=="none")
{
  
  
  if(expModeSelection=="Exp4") 
  {
  finalPerALLFinalNoneExp4    <- finalPerALLFinal 
  cat("Non-Tuned Loaded Exp4: ",tuningMethod, " total = ",nrow(finalPerALLFinalNoneExp4), "\n")
  }
  else
  {
   finalPerALLFinalNoneExp1    <- finalPerALLFinal 
   cat("Non-Tuned Loaded Exp1: ",tuningMethod, " total = ",nrow(finalPerALLFinalNoneExp1), "\n")
  }
  
}

 

}




########################### Done Loading Results Loop ############################
#################################################################################

 
# Exp1 or Exp4
# Exp1 : one release in training - one release testing
# Exp4 : multiple releases in training - one release testing  

cat("Target Metric : ",targetMetric, "\n")
cat("Experiment : ",expModeSelection, "\n")


}



finalPerALLFinalOptimized = NULL
finalPerALLFinalNone = NULL

if(expMode=="Exp4") 
{
  finalPerALLFinalOptimized <- finalPerALLFinalOptimizedExp4
  finalPerALLFinalNone  <- finalPerALLFinalNoneExp4   
  nrow(finalPerALLFinalOptimizedExp4)
  nrow(finalPerALLFinalNoneExp4)
  
} else{
  finalPerALLFinalOptimized <- finalPerALLFinalOptimizedExp1
  finalPerALLFinalNone  <- finalPerALLFinalNoneExp1  
  nrow(finalPerALLFinalOptimizedExp1)
  nrow(finalPerALLFinalNoneExp1)
}


cat("Tuned  finalPerALLFinalOptimized: ",nrow(finalPerALLFinalOptimized), "\n")
cat("Non-Tuned finalPerALLFinalNone: ",nrow(finalPerALLFinalNone), "\n")

########################### RQ1 Loading Results Loop ############################
#################################################################################
 
# If you want the count of unique values
num_unique_datasets <- length(unique(finalPerALLFinalNone$DataSet))
print(unique(finalPerALLFinalNone$DataSet))
print(num_unique_datasets)
num_unique_models <- length(unique(finalPerALLFinalNone$Model))
print(unique(finalPerALLFinalNone$Model))
print(num_unique_models)


# Create a new column in finalPerALLFinalOptimizedExp1 by combining X, Y, and Z


if(targetMetric=="ROC")
{
  IVDPtunnedMetric <- "TrainROC_Tunned"
  IVDPnonTunnedMetric <- "TrainROC_NonTunned"
  
  CVDPtunnedMetric <- "TestROC_Tunned"
  CVDPnonTunnedMetric <- "TestROC_NonTunned"
  
  IVDPtunnedMetricExp1 <- "TrainROC_Tunned_Exp1"
  IVDPnonTunnedMetricExp1 <- "TrainROC_NonTunned_Exp1"
  
  YLabelMetric <- "AUC"
  
  
} else if(targetMetric=="Recall")
{
  IVDPtunnedMetric <- "TrainRecall_Tunned"
  IVDPnonTunnedMetric <- "TrainRecall_NonTunned"
  
  IVDPtunnedMetricExp1 <- "TrainRecall_Tunned_Exp1"
  IVDPnonTunnedMetricExp1 <- "TrainRecall_NonTunned_Exp1"
  
  CVDPtunnedMetric <- "TestRecall_Tunned"
  CVDPnonTunnedMetric <- "TestRecall_NonTunned"
  
  YLabelMetric <- "Recall Rate"
  
} else if(targetMetric=="Accuracy")
{
  
  IVDPtunnedMetric <- "TrainAccuracy_Tunned"
  IVDPnonTunnedMetric <- "TrainAccuracy_NonTunned"
  
  IVDPtunnedMetricExp1 <- "TrainAccuracy_Tunned_Exp1"
  IVDPnonTunnedMetricExp1 <- "TrainAccuracy_NonTunned_Exp1"
  
  
  CVDPtunnedMetric <- "TestAccuracy_Tunned"
  CVDPnonTunnedMetric <- "TestAccuracy_NonTunned"
  
  YLabelMetric <- "Accuracy"
  
} else if(targetMetric=="F")
{
  
  IVDPtunnedMetric <- "TrainF_Tunned"
  IVDPnonTunnedMetric <- "TrainF_NonTunned"
  
  IVDPtunnedMetricExp1 <- "TrainF_Tunned_Exp1"
  IVDPnonTunnedMetricExp1 <- "TrainF_NonTunned_Exp1"
  
  CVDPtunnedMetric <- "TestF1_Tunned"
  CVDPnonTunnedMetric <- "TestF1_NonTunned"
  YLabelMetric <- "F-Measure"
 
  
  
} else if(targetMetric=="Precision")
{
  
  IVDPtunnedMetric <- "TrainPrecision_Tunned"
  IVDPnonTunnedMetric <- "TrainPrecision_NonTunned"
  
  IVDPtunnedMetricExp1 <- "TrainPrecision_Tunned_Exp1"
  IVDPnonTunnedMetricExp1 <- "TrainPrecision_NonTunned_Exp1"
  
  CVDPtunnedMetric <- "TestPrecision_Tunned"
  CVDPnonTunnedMetric <- "TestPrecision_NonTunned"
  YLabelMetric <- "Precision"
  
  
  
}




#All data with Hyperparameter tunning
finalPerALLFinalOptimized$merge_key <- paste(finalPerALLFinalOptimized$DataSet, finalPerALLFinalOptimized$Model, sep = "-")
 #All data without Hyperparameter tunning
finalPerALLFinalNone$merge_key <- paste(finalPerALLFinalNone$DataSet, finalPerALLFinalNone$Model, sep = "-")

cat("Tuned  finalPerALLFinalOptimized: ",nrow(finalPerALLFinalOptimized), "\n")
cat("Non-Tuned finalPerALLFinalNone: ",nrow(finalPerALLFinalNone), "\n")


# Merge the data frames based on the new merge_key column
merged_all_data <- merge(finalPerALLFinalOptimized, finalPerALLFinalNone, suffixes = c("_Tunned", "_NonTunned"), by = "merge_key", all = FALSE)

nrow(merged_all_data)

## IVDP
 

#non_numeric_rows <- which(!grepl("^\\d+\\.?\\d*$", merged_all_data[, IVDPtunnedMetric]) )


#merged_all_data[non_numeric_rows, ]

#merged_all_data[,IVDPtunnedMetric]

if (enableIVDP_one_release)
{
  
  finalPerALLFinalOptimizedExp1$merge_key <- paste(finalPerALLFinalOptimizedExp1$DataSet, finalPerALLFinalOptimizedExp1$Model, sep = "-")
  #All data without Hyperparameter tunning
  finalPerALLFinalNoneExp1$merge_key <- paste(finalPerALLFinalNoneExp1$DataSet, finalPerALLFinalNoneExp1$Model, sep = "-")
  
  merged_all_dataExp1 <- merge(finalPerALLFinalOptimizedExp1, finalPerALLFinalNoneExp1, suffixes = c("_Tunned", "_NonTunned"), by = "merge_key", all = FALSE)
  nrow(merged_all_dataExp1)
 
  
  merged_all_dataExp1 <- merge(merged_all_data, merged_all_dataExp1, suffixes = c("", "_Exp1"), by = "merge_key", all = FALSE)
  nrow(merged_all_dataExp1)
  
  
}


### Calculating the percentage
# Add a small positive number to the divisor to avoid Infinity/NaN percentages
# This value could bias the percentages, therefore leaving it zero
epsilon <- 1e-6  # You can adjust this value based on your needs



if (enableIVDP_one_release)
{
  
  merged_all_dataExp1$IVDPMetric_Diff = as.numeric(merged_all_dataExp1[,IVDPtunnedMetricExp1])- as.numeric(merged_all_dataExp1[, IVDPnonTunnedMetricExp1])
  merged_all_dataExp1$IVDP_RelativeImprovPercentage <- ((merged_all_dataExp1$IVDPMetric_Diff)/(as.numeric(merged_all_dataExp1[, IVDPnonTunnedMetricExp1])+epsilon)) * 100
  nrow(merged_all_dataExp1)
  
  
  zero_value_rows <- merged_all_dataExp1[merged_all_dataExp1[, IVDPnonTunnedMetricExp1] == 0, ]
  
  which(merged_all_dataExp1[, IVDPnonTunnedMetricExp1] == 0 )
  
  merged_all_data<- merged_all_dataExp1
  
  nrow(merged_all_data)
  
  
} else
{
  
  merged_all_data$IVDPMetric_Diff = merged_all_data[,IVDPtunnedMetric]- merged_all_data[, IVDPnonTunnedMetric]
  merged_all_data$IVDP_RelativeImprovPercentage <- (merged_all_data$IVDPMetric_Diff/(merged_all_data[, IVDPnonTunnedMetric]+epsilon))* 100
  
  #which(merged_all_data[, IVDPnonTunnedMetric] == 0 )
  
  #merged_all_data[which(merged_all_data[, IVDPnonTunnedMetric] == 0 ), ]
  
  nrow(merged_all_data)
  
}

 
## CVDP
merged_all_data$CVDPMetric_Diff <- merged_all_data[,CVDPtunnedMetric]- merged_all_data[, CVDPnonTunnedMetric]
merged_all_data$CVDP_RelativeImprovPercentage <-  (merged_all_data$CVDPMetric_Diff/(merged_all_data[, CVDPnonTunnedMetric]+epsilon)) * 100

## This check drop some values if needed - Clean NaN / INF percentages for accurate comparison 
cat("Total number of rows compared before cleaning NaNs/Inf: ",nrow(merged_all_data), "\n")

#merged_all_data <- merged_all_data[is.finite(merged_all_data[, "CVDP_RelativeImprovPercentage"]), ]
#merged_all_data <- merged_all_data[is.finite(merged_all_data[, "IVDP_RelativeImprovPercentage"]), ]

summary(merged_all_data$CVDP_RelativeImprovPercentage)
summary(merged_all_data$IVDP_RelativeImprovPercentage)


print(nrow(merged_all_data))
stTest=wilcox.test(merged_all_data$IVDP_RelativeImprovPercentage, merged_all_data$CVDP_RelativeImprovPercentage,alternative="g", paired = FALSE)
print(stTest)
Eff_debug1=cliff.delta(merged_all_data$IVDP_RelativeImprovPercentage, merged_all_data$CVDP_RelativeImprovPercentage, paired = FALSE)
print(Eff_debug1)

cat("Total number of rows compared: ",nrow(merged_all_data), "\n")
 

 
 

#length(positive_percentageMAX)
par(mar = c(4.1, 4.1, 4.1, 1.1))

#merged_all_data <- merged_all_data[is.finite(merged_all_data[, "CVDP_RelativeImprovPercentage"]), ]
#merged_all_data <- merged_all_data[is.finite(merged_all_data[, "IVDP_RelativeImprovPercentage"]), ]

summary(merged_all_data[, "IVDP_RelativeImprovPercentage"] )
summary(merged_all_data[, "CVDP_RelativeImprovPercentage"] )


nrow(merged_all_data)


IVDPtunnedMetric
CVDPtunnedMetric

# Assuming your data frame is named 'merged_all_data' - CVDP
    max_indicesCVDP <- tapply(
    seq_len(nrow(merged_all_data)), 
    merged_all_data$DataSet_Tunned, 
    function(x) {
      acc_values <- merged_all_data[x,CVDPtunnedMetric]
      if (length(acc_values) == 0 || all(is.na(acc_values))) {
        return(NA)
      }
      x[which.max(acc_values)]
    }
    )
    
    merged_all_data$Model_Tunned
      

    
    
    max_indicesCVDP_df <- as.data.frame(max_indicesCVDP)  
  # Subset the data based on the found indices
  best_model_subset <- merged_all_data[max_indicesCVDP_df$max_indicesCVDP, c("Model_Tunned", "DataSet_Tunned", CVDPtunnedMetric)]
 
  # Remove ".csv" from each dataset name
  best_model_subset$DataSet_Tunned <- sub(".csv$", "", best_model_subset$DataSet_Tunned)
  
  model_frequencyCVDP <- table(best_model_subset$Model_Tunned)
  model_percentageCVDP <- prop.table(model_frequencyCVDP) * 100
  
  # Assuming your data frame is named 'merged_all_data' - IVDP
  max_indicesIVDP <- tapply(
    seq_len(nrow(merged_all_data)), 
    merged_all_data$DataSet_Tunned, 
    function(x) {
      acc_values <- merged_all_data[x,IVDPtunnedMetric]
      if (length(acc_values) == 0 || all(is.na(acc_values))) {
        return(NA)
      }
      x[which.max(acc_values)]
    }
  )
  # Subset the data based on the found indices
  max_indicesIVDP_df <- as.data.frame(max_indicesIVDP)  
  best_model_subset <- merged_all_data[max_indicesIVDP_df$max_indicesIVDP, c("Model_Tunned", "DataSet_Tunned", IVDPtunnedMetric)]
  
  # Remove ".csv" from each dataset name
  best_model_subset$DataSet_Tunned <- sub(".csv$", "", best_model_subset$DataSet_Tunned)
  model_frequencyIVDP <- table(best_model_subset$Model_Tunned)
  model_percentageIVDP <- prop.table(model_frequencyIVDP) * 100
   
 
 length(best_model_subset$Model_Tunned)
 merged_all_data$Model_Tunned
 
 nrow(merged_all_data)
 
 cat("RQ1 Matched IVDP - CVDP data size: ", nrow(merged_all_data), "\n")
 
 
length(merged_all_data[, "CVDP_RelativeImprovPercentage"] )

# Plot data 
plot_data <- merged_all_data[merged_all_data[, "IVDP_RelativeImprovPercentage"] < 100 & merged_all_data[,"IVDP_RelativeImprovPercentage"] > -50 & merged_all_data[, "CVDP_RelativeImprovPercentage"] < 100 & merged_all_data[,"CVDP_RelativeImprovPercentage"] > -50 , ]

nrow(plot_data)


par(  mar = c(2, 5, 0, 0) + 0.1, oma = c(0, 0, 0, 0) )



  

if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
{
  startVal="*";
}else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
{
  startVal="*";
}else if(stTest$p.value<=0.001)
{
  startVal="*";
}else
{
  
  startVal="(-)"
  
}

if(Eff_debug1$magnitude=="small")
{
  EffSizeShort = "S"
} else if(Eff_debug1$magnitude=="medium")
{
  
  EffSizeShort = "M"
} else if(Eff_debug1$magnitude=="large")
{
  
  EffSizeShort = "L"
}  else  
{
  
  EffSizeShort = "N"
}


specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
# par(xpd=TRUE)
# legend('bottomleft',bty="n",inset=c(0.17,-0.101), legend = "Effect Size =")
effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",EffSizeShort,") ",startVal,sep='')

EffectSizeAll =paste(EffectSizeAll, effVal, "    \t         ",sep='')
# legend('bottomleft',bty="n",inset=c(0.48,-0.101), legend = effVal)
# legend("topright", cex = 0.75 , text.width=0.11,   legend = c("IVDP", "CVDP"), fill = c("lightblue", "lightgray"))

options(encoding = "UTF-8")

# dev.copy2pdf(width=4.32,height=4.5 , file=paste(pathToSave,"/",expMode,targetMetric,"vsExp1",enableIVDP_one_release,"beanplotPerfImpactPercentage.pdf",sep=''));
# dev.off();



## Same figure but in ggplot
   ggplot(merged_all_data, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  #geom_point(aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage), position = position_jitter(width = 0.01)) +
  geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
  #geom_point(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), position = position_jitter(width = 0.01)) +
  scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
  ylim(-20, 40) +
  labs(
    x = NULL,
    y = paste(YLabelMetric, " Impact (%)"),
    title = NULL
  ) +
  labs(tag = paste("Effect Size = ", effVal ,sep='')) +     
  theme_minimal() +
  theme(plot.margin = margin(b = 30, l=10), plot.tag.position = c(0.56, -0.06), plot.tag = element_text(size = 14),  axis.text.x = element_text(face = "bold", size = 16), axis.title.y = element_text( size = 20)
        ,axis.text.y = element_text(size = 14)) 
 





# Save the ggplot to a file
ggsave(paste(pathToSave,"/",expMode,targetMetric,"vsExp1",enableIVDP_one_release,"_",tuningMethod,"_ggplotPerfImpactPercentage.pdf",sep=''), plot = last_plot(), device = "pdf", width = 5, height = 5.2, units = "in", dpi = 300)






merged_all_data$GroupMetric <- targetMetric
Allmerged_all_data<-rbind(Allmerged_all_data,merged_all_data)


###################################################################################

## drop out NAs/Infs
cleaned_merged_all_data <- merged_all_data[is.finite(merged_all_data[, "CVDP_RelativeImprovPercentage"]), ]
cleaned_merged_all_data <- cleaned_merged_all_data[is.finite(cleaned_merged_all_data[, "IVDP_RelativeImprovPercentage"]), ]



max(cleaned_merged_all_data$IVDP_RelativeImprovPercentage)
max(cleaned_merged_all_data$CVDP_RelativeImprovPercentage)




percentage_negative <- NULL
total_count <- length(cleaned_merged_all_data[, "IVDP_RelativeImprovPercentage"])  # Total number of values

negative_count <- sum(cleaned_merged_all_data[, "IVDP_RelativeImprovPercentage"] < 0)  # Number of negative values
percentage_negative$Impact <- (negative_count / total_count) * 100
percentage_negative$Scenario <- "IVDP"
percentage_negative$metric <- targetMetric

allpercentage_negative<- rbind(allpercentage_negative, percentage_negative) 
total_count <- length(cleaned_merged_all_data[, "CVDP_RelativeImprovPercentage"])  # Total number of values
negative_count <- sum(cleaned_merged_all_data[, "CVDP_RelativeImprovPercentage"] < 0)  # Number of negative values


percentage_negative <- NULL
percentage_negative$Impact <- (negative_count / total_count) * 100
percentage_negative$Scenario <- "CVDP"
percentage_negative$metric <- targetMetric
allpercentage_negative<- rbind(allpercentage_negative,percentage_negative)
 
# 
# maxrow <- merged_all_data[merged_all_data$IVDP_RelativeImprovPercentage == max(merged_all_data$IVDP_RelativeImprovPercentage),]
# 
# maxrow$TrainROC_Tunned
# maxrow$TrainROC_NonTunned
# maxrow$merge_key
# 
# 
# 
# maxrow <- merged_all_data[merged_all_data$CVDP_RelativeImprovPercentage == max(merged_all_data$CVDP_RelativeImprovPercentage),]
# 
# maxrow$TestROC_Tunned
# maxrow$TestROC_NonTunned
# maxrow$merge_key
# 


}


##Step1


###### Starting All Metrics Performance Impact in One Figure
print(CountAllReadResultsRows)
cat("Total read data: ",  CountAllReadResultsRows , "\n")
#@@WORK
 

par(mgp = c(1, 0.8, 0), mar = c(3, 3, 1, 2) + 0.1, oma = c(0, 0, 0, 0), inner = c(0, 0, 0, 0))


Allmerged_all_data$GroupMetric <- factor(Allmerged_all_data$GroupMetric, levels = c("ROC", "Recall","Precision", "F", "Accuracy"))

# Define a custom labeller function to change the label for "F" metric


facet_labels <- c("ROC" = "AUC", "Recall" = "Recall", "Precision" = "Precision", "F" = "F-Measure", "Accuracy" = "Accuracy")





#Allmerged_all_data <- Allmerged_all_data[is.finite(Allmerged_all_data[, "CVDP_RelativeImprovPercentage"]), ]
#Allmerged_all_data <- Allmerged_all_data[is.finite(Allmerged_all_data[, "IVDP_RelativeImprovPercentage"]), ]


summary(Allmerged_all_data$IVDP_RelativeImprovPercentage)
summary(Allmerged_all_data$CVDP_RelativeImprovPercentage)
# 110.18

ggplot(Allmerged_all_data, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
  geom_boxplot(fill = "lightblue", outlier.shape = NA) +
  geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
  scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
  #ylim(-20, 40) +
  coord_cartesian(ylim =  c(-30, 70)) +
  labs(
    x = NULL,
    y = paste("Performance Impact (%) "),
    title = NULL
  ) +
  labs(tag = paste("Effect Size =   ", EffectSizeAll ,sep='')) +     
  theme_minimal() +
  theme(plot.margin = margin(b = 30, l=10), plot.tag.position = c(0.52, -0.05), plot.tag = element_text(size = 8),  axis.text.x = element_text(face = "bold", size = 9, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 13)
        ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
  facet_wrap(~ GroupMetric, scales = "fixed", ncol = 5, labeller = labeller(GroupMetric = as_labeller(facet_labels)))


 


# Save the ggplot to a file
ggsave(paste(pathToSave,"/",expMode,targetMetric,"vsExp1",enableIVDP_one_release,"_",tuningMethod,"_ggplotAllMetricsImpactPercentageTSE.pdf",sep=''), plot = last_plot(), device = "pdf", width = 5, height = 3.2, units = "in", dpi = 300)





###### Starting Histogram

percentageData <- as.data.frame(allpercentage_negative)

percentageData$Impact<- as.numeric(percentageData$Impact)

percentageData$metric <- factor(percentageData$metric, levels = c("ROC", "Recall", "Precision", "F", "Accuracy"))
percentageData$Scenario <- factor(percentageData$Scenario, levels = c("IVDP", "CVDP"))


par(mgp = c(2, 0.8, 0), mar = c(3, 3, 4, 2) + 0.1, oma = c(0, 0, 0, 0), inner = c(0, 0, 0, 0))


ggplot(percentageData, aes(x = metric, y = Impact, fill = Scenario)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single")) +
  labs(x = "", y = "Negative Impact Percentage (%)") +
  scale_fill_manual(values = c("IVDP" = "lightblue", "CVDP" = "lightgray"), limits = c("IVDP", "CVDP")) +
  scale_x_discrete(labels = c(ROC = "AUC", Recall = "Recall", Precision = "Precision", F = "F-Measure", Accuracy="Accuracy"))+ theme_minimal() +theme(legend.position = "top", plot.margin = margin(b = 0, l=10) ,  axis.text.x = element_text(face = "bold", size = 9), axis.title.y = element_text( size = 13)
                                                                                                             ,axis.text.y = element_text(size = 10))
 

 


options(encoding = "UTF-8")

dev.copy2pdf(width=4.52,height=3.2 , file=paste(pathToSave,"/NegativebarfImpactPercentage.pdf",sep=''));
dev.off();




##########################################

# 
# data <- as.data.frame(allpercentage_negative)
# 
# data$Impact<- as.numeric(data$Impact)
# 
# ggplot(data, aes(x = metric, y =  Impact, fill = context)) +
#   geom_col(position = position_dodge2(width = 0.8, preserve = "single")) +
#   labs(title = "Grouped Bar Plot Example", x = "metric", y = "Values") +
#   scale_fill_manual(values = c("IVDP" = "lightblue", "CVDP" = "lightgray"))
# 
#  
# 
# 
# your_data <- data.frame(
#   Impact = c(4.40806, 24.05542, 4.456094, 28.44037, 3.84216, 22.11838),
#   context = c("IVDP", "CVDP", "IVDP", "CVDP", "IVDP", "CVDP"),
#   metric = c("Recall", "Recall", "FMeasure", "FMeasure", "ROC", "ROC")
# )
# 
# # Ensure non-negative values for "Impact"
# your_data$Impact <- pmax(0, your_data$Impact)

#your_data$metric <- factor(your_data$metric, levels = c("ROC", "Recall", "FMeasure"))
#your_data$context <- factor(your_data$context, levels = c("IVDP", "CVDP"))

# Create the bar plot


################################################


# CVDP Ranks
nrow(merged_all_data)

rank_indicesCVDP <- tapply(
  seq_len(nrow(merged_all_data)), 
  merged_all_data$DataSet_Tunned, 
  function(x) {
    acc_values <- merged_all_data[x, CVDPtunnedMetric]
    model_names <- merged_all_data[x, "Model_Tunned"]
    
    if (length(acc_values) == 0 || all(is.na(acc_values))) {
      return(data.frame(Model_Tunned = rep(NA, length(x)), Rank = rep(NA, length(x))))
    }
    
    rank_values <- rank(-acc_values, ties.method = "min")  # Use rank with descending order
    
    result_df <- data.frame(Model_Tunned = model_names, CVDPRank = rank_values, Row_Index = x)
    return(result_df)
  }
)


nrow(rank_indicesCVDP)


# Assuming you want to merge based on the "Row_Index" column
row_indices <- as.integer(row.names(merged_all_data))
# Add row indices to merged_all_data
merged_all_data$Row_Index <- row_indices
# Continue with your merging process
merged_data_with_ranks <- merge(merged_all_data, do.call(rbind, rank_indicesCVDP), by = "Row_Index")

# IVDP Ranks
# rank_indicesIVDP <- tapply(
#   seq_len(nrow(merged_all_data)), 
#   merged_all_data$DataSet_Tunned, 
#   function(x) {
#     acc_values <- merged_all_data[x, IVDPtunnedMetric]
#     model_names <- merged_all_data[x, "Model_Tunned"]
#     
#     if (length(acc_values) == 0 || all(is.na(acc_values))) {
#       return(data.frame(Model_Tunned = rep(NA, length(x)), Rank = rep(NA, length(x))))
#     }
#     
#     rank_values <- rank(-acc_values, ties.method = "min")  # Use rank with descending order
#     
#     result_df <- data.frame(Model_Tunned = model_names, IVDPRank = rank_values, Row_Index = x)
#     return(result_df)
#   }
# )

if (enableIVDP_one_release)
{
  
  # IVDP Ranks
  rank_indicesIVDP <- tapply(
    seq_len(nrow(merged_all_data)), 
    merged_all_data$DataSet_Tunned, 
    function(x) {
      acc_values <- merged_all_data[x, IVDPtunnedMetricExp1]
      model_names <- merged_all_data[x, "Model_Tunned"]
      acc_values <- as.numeric(acc_values)
      if (length(acc_values) == 0 || all(is.na(acc_values))) {
        return(data.frame(Model_Tunned = rep(NA, length(x)), Rank = rep(NA, length(x))))
      }
    
      rank_values <- rank(-acc_values, ties.method = "min")  # Use rank with descending order
      
      result_df <- data.frame(Model_Tunned = model_names, IVDPRank = rank_values, Row_Index = x)
      return(result_df)
    }
  )
  

} else {
  # IVDP Ranks
  rank_indicesIVDP <- tapply(
    seq_len(nrow(merged_all_data)), 
    merged_all_data$DataSet_Tunned, 
    function(x) {
      acc_values <- merged_all_data[x, IVDPtunnedMetric]
      model_names <- merged_all_data[x, "Model_Tunned"]
      
      if (length(acc_values) == 0 || all(is.na(acc_values))) {
        return(data.frame(Model_Tunned = rep(NA, length(x)), Rank = rep(NA, length(x))))
      }
      
      rank_values <- rank(-acc_values, ties.method = "min")  # Use rank with descending order
      
      result_df <- data.frame(Model_Tunned = model_names, IVDPRank = rank_values, Row_Index = x)
      return(result_df)
    }
  )
  
  
}


nrow(rank_indicesIVDP)
# Continue with your merging process
merged_data_with_ranks <- merge(merged_data_with_ranks, do.call(rbind, rank_indicesIVDP), by = "Row_Index")

# Assuming you have a factor variable that you want to use for faceting, for example, Group

#Strange Error here
#plot_data$Group <- factor(c(rep("IVDP", length(plot_data$IVDP_RelativeImprovPercentage)),
                           # rep("CVDP", length(plot_data$CVDP_RelativeImprovPercentage))))

# Using facet argument to create facetted beanplots

merged_data_with_ranks$IVDP_RelativeImprovPercentage
merged_data_with_ranks$Model_Tunned
merged_data_with_ranks$IVDP_RelativeImprovPercentage

nrow(merged_data_with_ranks)

#merged_data_with_ranks$Group <- factor(c(rep("IVDP", length(merged_data_with_ranks$IVDP_RelativeImprovPercentage)),
 #                           rep("CVDP", length(merged_data_with_ranks$CVDP_RelativeImprovPercentage))))


# Assuming every row has values for both "IVDP" and "CVDP"
#merged_data_with_ranks$Group <- factor(interaction(merged_data_with_ranks$IVDP_RelativeImprovPercentage, merged_data_with_ranks$CVDP_RelativeImprovPercentage))


  merged_data_with_ranks$Model_Tunned

    merged_data_with_ranks$Model_Tunned <- factor(merged_data_with_ranks$Model_Tunned, levels = unique(merged_data_with_ranks$Model_Tunned))
    levels(merged_data_with_ranks$Model_Tunned)
    
    
    nrow(merged_data_with_ranks)
    
    
    unique(merged_data_with_ranks$Model_Tunned)
   
    #target_models <- c( "ada", "AdaBag","rf")
    #subset_data <- subset(merged_data_with_ranks, Model_Tunned %in% target_models)
    subset_data <- merged_data_with_ranks
    

    # selectedModelData <- subset(subset_data, Model_Tunned == "MLP")
    # nrow(selectedModelData)
    # 
    # stTest <- wilcox.test(selectedModelData$IVDP_RelativeImprovPercentage, 
    #                       selectedModelData$CVDP_RelativeImprovPercentage,
    #                       alternative = "g", paired = FALSE)
    # 
    # # Calculate effect size (Cliff's delta)
    # Eff_debug1 <- cliff.delta(selectedModelData$IVDP_RelativeImprovPercentage, 
    #                           selectedModelData$CVDP_RelativeImprovPercentage,
    #                           paired = FALSE)
    # summary(selectedModelData$IVDP_RelativeImprovPercentage)
    # summary(selectedModelData$CVDP_RelativeImprovPercentage)
    # 
    # write.csv(selectedModelData$IVDP_RelativeImprovPercentage, paste(pathToSave,"/MLP_IVDP_RelativeImprovPercentage.csv",sep=''), row.names=FALSE)
    # write.csv(selectedModelData$CVDP_RelativeImprovPercentage,  paste(pathToSave,"/MLP_CVDP_RelativeImprovPercentage.csv",sep=''), row.names=FALSE)
    # 
    # 
    # df <- bind_rows(
    #   tibble(performance_impact = selectedModelData$CVDP_RelativeImprovPercentage, schema = "CVDP"), 
    #   tibble(performance_impact = selectedModelData$IVDP_RelativeImprovPercentage, schema = "IVDP")
    # )
    # 
    # df %>%
    #   ggplot(aes(x=schema, y=performance_impact)) + 
    #   geom_boxplot(outlier.shape = NA) +
    #   coord_cartesian(ylim =  c(-50, 200)) 
    # 
    # 
    # ncol(selectedModelData)
    # selectedModelData2 <- selectedModelData[, c("IVDP_RelativeImprovPercentage", "CVDP_RelativeImprovPercentage", "Model_Tunned")]
    # summary(selectedModelData2$IVDP_RelativeImprovPercentage)
    # summary(selectedModelData2$CVDP_RelativeImprovPercentage)
    # 
    # data_clean <- selectedModelData2[is.finite(selectedModelData2$IVDP_RelativeImprovPercentage) & is.finite(selectedModelData2$CVDP_RelativeImprovPercentage), ]
    # 
    # nrow(selectedModelData2)
    # nrow(data_clean)
    # 
    # ggplot(data_clean) +
    #   geom_boxplot( aes(x = factor("IVDP"), y =data_clean$IVDP_RelativeImprovPercentage), fill = "lightblue",outlier.shape = NA) +
    #   geom_boxplot(aes(x = factor("CVDP"), y = data_clean$CVDP_RelativeImprovPercentage), fill = "lightgray",outlier.shape = NA) +
    #   labs(
    #     x = NULL,
    #     y =  paste(YLabelMetric, " Impact (%) "),
    #     title = NULL
    #   ) +   coord_cartesian(ylim =  c(-20, 100)) 
    # 
    # 
    # library(tidyr)
    # # Reshape the data into long format
    # long_data <- pivot_longer(selectedModelData, 
    #                           cols = c(IVDP_RelativeImprovPercentage, CVDP_RelativeImprovPercentage),
    #                           names_to = "Model_Type", 
    #                           values_to = "Relative_Improvement_Percentage")
    # 
    # # Plot the data
    # 
    # # Select columns by name
    # 
    # # Or select columns by index
    # subset_data <- main_dataframe[, c(1, 3, 5)]
    # 
    # 
    # ggplot(long_data) +
    #   geom_boxplot(aes(x = Model_Type, y = Relative_Improvement_Percentage, fill = Model_Type)) +
    #   ylim(-20, 40) +
    #   labs(
    #     x = NULL,
    #     y = "Relative Improvement Percentage",
    #     title = NULL
    #   ) +
    #   scale_fill_manual(values = c("IVDP" = "lightblue", "CVDP" = "lightgray"))
    # 
    # 
    # +
    #   theme_minimal() +
    #   theme(strip.text = element_text(size = 12),plot.margin = margin(b = 10, l=10), plot.tag.position = c(0.50, -0.07), plot.tag = element_text(size = 9),  axis.text.x = element_text(face = "bold", size = 12, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 18),axis.text.y = element_text(size = 14)
    #         ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
    #   facet_wrap(~ Model_Tunned, scales = "fixed", ncol = 14, labeller = custom_labeller)
    # 
    # 
    # ggplot(selectedModelData, aes(x=schema, y=performance_impact)) + 
    #   geom_boxplot(outlier.shape = NA) +
    #   coord_cartesian(ylim =  c(-50, 200)) 
    # 
    # #rebattle
    
    
    
    target_models <- unique(merged_data_with_ranks$Model_Tunned)
    length(subset_data$IVDP_RelativeImprovPercentage)
    length(subset_data$CVDP_RelativeImprovPercentage)
    
 
    # Check for missing values
    missing_values <- sum(is.na(subset_data$IVDP_RelativeImprovPercentage) | is.na(subset_data$CVDP_RelativeImprovPercentage))
    
    # Set the levels of Model_Tunned
   # subset_data$Model_Tunned <- factor(subset_data$Model_Tunned, levels = target_models)
    levels(subset_data$Model_Tunned)

    wilcox_results <- list()
    effect_sizes <- list()
    # Get unique levels of Model_Tunned
    model_levels <- unique(subset_data$Model_Tunned)
    subset_data$Model_Tunned <- factor(subset_data$Model_Tunned, levels = target_models)
    #subset_data$Model_Tunned<-levels(c( "ada", "AdaBag"))
 
    
    nrow(subset_data)
    ncol(subset_data)
    
    # Loop through each level of Model_Tunned
    for (model_level in model_levels) {
      # Subset data for the current model level
      subset_model_data = subset(subset_data, Model_Tunned %in% model_level)
      
       
      
      length(subset_model_data$Model_Tunned)
      
      # Perform Wilcoxon signed-rank test
      stTest <- wilcox.test(subset_model_data$IVDP_RelativeImprovPercentage, 
                            subset_model_data$CVDP_RelativeImprovPercentage,
                            alternative = "g", paired = FALSE)
      
      # Calculate effect size (Cliff's delta)
      Eff_debug1 <- cliff.delta(subset_model_data$IVDP_RelativeImprovPercentage, 
                                subset_model_data$CVDP_RelativeImprovPercentage,
                                paired = FALSE)
      # Store results in lists
      wilcox_results[[model_level]] <- stTest
      effect_sizes[[model_level]] <- Eff_debug1
    }
    
    
    # summary(subset(subset_data, Model_Tunned %in% "ada")$IVDP_RelativeImprovPercentage)
      summary(subset(subset_data, Model_Tunned %in% "MLP")$CVDP_RelativeImprovPercentage)
    # summary(subset(subset_data, Model_Tunned %in% "rf")$IVDP_RelativeImprovPercentage)
    # 
    # summary(subset(subset_data, Model_Tunned %in% "ada")$CVDP_RelativeImprovPercentage)
    # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$CVDP_RelativeImprovPercentage)
    # summary(subset(subset_data, Model_Tunned %in% "rf")$CVDP_RelativeImprovPercentage)
    # 
    
    
    par(mgp = c(2, 0.8, 0), mar = c(9, 3, 1, 0) + 0.1, oma = c(0, 0, 0, 0), outer = c(0, 0, 0, 0))
 
  # Set x-axis labels
 # axis(side = 1, at = 1:length(levels(subset_data$Model_Tunned)), labels = levels(subset_data$Model_Tunned), srt = -90, cex.axis = 0.8)
  
 
    specify_decimal <- function(x, k) format(round(x, k), nsmall=k)
    #par(xpd=TRUE)
    
    
  #  legend('bottomleft',bty="n",inset=c(-0.05,-0.471), legend = "Effect Size =", cex = 1)
    
    stepPosition <- 0.02
    for (model_level in model_levels) {
      stTest <- wilcox_results[[model_level]] 
      Eff_debug1 <- effect_sizes[[model_level]]
      
      if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
      {
        startVal="*";
      }else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
      {
        startVal="*";
      }else if(stTest$p.value<=0.001)
      {
        startVal="*";
      }else
      {
        
        startVal="(-)"
        
      }
      
      
      if(Eff_debug1$magnitude=="small")
      {
        EffSizeShort = "S"
      } else if(Eff_debug1$magnitude=="medium")
      {
        
        EffSizeShort = "M"
      } else if(Eff_debug1$magnitude=="large")
      {
        
        EffSizeShort = "L"
      }  else  
      {
        
        EffSizeShort = "N"
      }
      
      
      #effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",Eff_debug1$magnitude,") ",startVal,sep='')
      effVal=paste(" (",EffSizeShort,") ",startVal,sep='')
    #  legend('bottomleft',bty="n",inset=c(stepPosition,-0.471), legend = effVal, cex = 0.95)
      
      stepPosition= stepPosition+0.0447
      
    }
    
    par(cex = 1.7)
  #  legend("topright", cex = 0.6 , text.width=0.59,   legend = c("IVDP", "CVDP"), fill = c("lightblue", "lightgray"), text.font = 6)
 
  
 # dev.copy2pdf(width=15,height=5.5 , file=paste(pathToSave,"/",targetMetric,"AllModelsImpactPercentage.pdf",sep=''));
  #dev.off();
  
  
  
  
  
  
  effVal<- ""
  
  facet_labels <- vector("list", length(model_levels))
  i <- 1
  for (model_level in model_levels) {
    stTest <- wilcox_results[[model_level]] 
    Eff_debug1 <- effect_sizes[[model_level]]
    
 
    if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.001)
    {
      startVal="*";
    }else
    {
      
      startVal="(-)"
      
    }
    
    
    if(Eff_debug1$magnitude=="small")
    {
      EffSizeShort = "S"
    } else if(Eff_debug1$magnitude=="medium")
    {
      
      EffSizeShort = "M"
    } else if(Eff_debug1$magnitude=="large")
    {
      
      EffSizeShort = "L"
    }  else  
    {
      
      EffSizeShort = "N"
    }
 
    facet_labels[[i]] <- paste(model_level," \n ",(specify_decimal(abs(Eff_debug1$estimate),2)),"(",EffSizeShort,")",startVal, " ",sep='')
    
    effVal=paste(effVal, (specify_decimal(abs(Eff_debug1$estimate),2)),"(",EffSizeShort,")",startVal, " ",sep='')
    
    i <- i +1
    #effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",Eff_debug1$magnitude,") ",startVal,sep='')
    #effVal=paste(" (",EffSizeShort,") ",startVal,sep='')
  }
  
  facet_labels <- setNames(facet_labels, model_levels)
  
  # Define custom labeling function
  custom_labeller <- function(variable, value) {
    return(facet_labels[value])
  }
  
  
  symlog_trans <- function(base = 10, thr = 1, scale = 1){
    trans <- function(x)
      ifelse(abs(x) < thr, x, sign(x) * 
               (thr + scale * suppressWarnings(log(sign(x) * x / thr, base))))
    
    inv <- function(x)
      ifelse(abs(x) < thr, x, sign(x) * 
               base^((sign(x) * x - thr) / scale) * thr)
    
    breaks <- function(x){
      sgn <- sign(x[which.max(abs(x))])
      if(all(abs(x) < thr))
        pretty_breaks()(x)
      else if(prod(x) >= 0){
        if(min(abs(x)) < thr)
          sgn * unique(c(pretty_breaks()(c(min(abs(x)), thr)),
                         log_breaks(base)(c(max(abs(x)), thr))))
        else
          sgn * log_breaks(base)(sgn * x)
      } else {
        if(min(abs(x)) < thr)
          unique(c(sgn * log_breaks()(c(max(abs(x)), thr)),
                   pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
        else
          unique(c(-log_breaks(base)(c(thr, -x[1])),
                   pretty_breaks()(c(-thr, thr)),
                   log_breaks(base)(c(thr, x[2]))))
      }
    }
    trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
  }
  
  ggplot(subset_data, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add horizontal line at y = 0
    scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
    scale_y_continuous(trans = symlog_trans())+
    coord_cartesian(ylim =  c(-15, 200))  +
    labs(
      x = NULL,
      y =  paste(" Symlog Scale : ", YLabelMetric, " Impact (%) "),
      title = NULL
    ) +
    theme_minimal() +
    theme(strip.text = element_text(size = 12),plot.margin = margin(b = 10, l=10), plot.tag.position = c(0.50, -0.07), plot.tag = element_text(size = 9),  axis.text.x = element_text(face = "bold", size = 12, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 18),axis.text.y = element_text(size = 8)
          ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
    facet_wrap(~ Model_Tunned, scales = "fixed", ncol = 14, labeller = custom_labeller)
  #step2
  
  # Save the ggplot to a file
  ggsave(paste(pathToSave,"/",expMode,targetMetric,"vsExp1",enableIVDP_one_release,"ggplotAllModels.pdf",sep=''), plot = last_plot(), device = "pdf", width = 15, height = 9.2, units = "in", dpi = 300)
  
  
  
  
## RQ2  
  
##############################################################
  ############ Scott Knott ESD
###############################################################  
# 
# library(ScottKnottESD)
# summary(subset_data)
# nrow(subset_data)
# final_df <- as.data.frame(t(subset_data))
# # Select specific columns using subset
#  twocol_data <- subset(subset_data, select = c(IVDP_RelativeImprovPercentage, Model_Tunned))
#  
#  subset_data
#  
#  
#  
#  colnames(twocol_data)
#  # Get unique model names
#  unique_models <- unique(twocol_data$Model_Tunned)
#  
#  # Pre-calculate the total count for each model
#  model_counts <- table(twocol_data$Model_Tunned)
#  
#  
#  # Create a new data frame with columns dynamically named
#  new_data <- data.frame(setNames(replicate(length(unique_models), numeric(0), simplify = FALSE), unique_models))
#  
#  # Changing the shape of the data to ML columns and rows performance impact
#  for (i in 1:length(unique_models)) {
#    model_name <- unique_models[i]
#    selected_data <- subset(twocol_data, Model_Tunned == model_name)
#    for (j in 1:model_counts[model_name]) {
#      value <- selected_data$IVDP_RelativeImprovPercentage[j]
#      # Fill in the values based on the count for each model
#      new_data[j, as.character(model_name)] <- value
#    }
#    # Increment the current row counter
#    #current_row <- current_row + model_counts[model_name]
#  }
#  # Using Non-Parametric ScottKnott ESD test
#  sk <- sk_esd(new_data, version="np")
#  
#  #plot(sk)
#  
# 
#  sk$groups
#  
#  sk$nms
#  
#  
#  sk$sig.level
#  sk$m.inf
#  
#  
# # #########################
#  twocol_data <- subset(subset_data, select = c(CVDP_RelativeImprovPercentage, Model_Tunned))
#  
#  
#  
#  colnames(twocol_data)
#  # Get unique model names unique_models <- unique(twocol_data$Model_Tunned)
# 
#  # Pre-calculate the total count for each model model_counts <-
#  table(twocol_data$Model_Tunned)
# 
#  # Create a new data frame with columns dynamically named new_data <-
#  data.frame(setNames(replicate(length(unique_models), numeric(0), simplify =
#  FALSE), unique_models))
# 
#  for (i in 1:length(unique_models)) {
#    model_name <- unique_models[i]
#    selected_data <- subset(twocol_data, Model_Tunned == model_name)
#    for (j in 1:model_counts[model_name]) {
#      value <- selected_data$CVDP_RelativeImprovPercentage[j]
#      # Fill in the values based on the count for each model
#      new_data[j, as.character(model_name)] <- value
#    }
#    # Increment the current row counter
#    #current_row <- current_row + model_counts[model_name]
#  }
#  
#  sk <- sk_esd(new_data, version="np")
#  
#  plot(sk)
#  
#  
#  sk
#  
#  sk$groups
#  
#  sk$nms
#  
#  
#  sk$sig.level
#    sk$m.inf
#  
#  nrow(sk$m.inf)
 
#   
  ##### RQ3 
  
   
  
  # Calculate the median
  
  
  if(targetMetric == "ROC")
  {
    print("am here -- ROC")
    ROCsubset_data <- subset_data
    
    
  } else if (targetMetric == "F")
  {
    print("am here -- F")
    Fsubset_data <- subset_data
    
  } else if (targetMetric == "Recall")
  {
    print("am here -- Recall")
    nrow(subset_data)
    Recallsubset_data <- subset_data
    
  } else if(targetMetric == "Precision")
  {
    print("am here -- Precision")
    Precisionsubset_data = subset_data
    
  } else if(targetMetric == "Accuracy")
  {
    print("am here -- Accuracy")
    Accuracysubset_data = subset_data
    
  }
    
  
  #RQ3
  #ROCsubset_data
  #Fsubset_data
  #Recallsubset_data
  #Accuracysubset_data
  
  
  
  ROCsubset_data$Group <- "AUC"
  Fsubset_data$Group <- "F-Measure"
  Recallsubset_data$Group <- "Recall"
  Precisionsubset_data$Group <- "Precision"
  Accuracysubset_data$Group <- "Accuracy"
  
  colnames(ROCsubset_data)
  # Step 1: Calculate the median of IVDP_RelativeImprovPercentage for each dataset
  
  
  
  ROCsubset_data$DataSet_NonTunned <- gsub("\\.csv$", "", ROCsubset_data$DataSet_NonTunned)
  
  # Step 1: Calculate the median of IVDP_RelativeImprovPercentage for each dataset
  dataset_medians <- aggregate(IVDP_RelativeImprovPercentage ~ DataSet_NonTunned, 
                               data = ROCsubset_data, FUN = median, na.rm = TRUE)
  summary(dataset_medians$IVDP_RelativeImprovPercentage)
  # Get the NRowTrainData_Tunned values
  nrow_train_data <- aggregate(NRowTrainData_Tunned ~ DataSet_NonTunned, 
                               data = ROCsubset_data, FUN = unique)  # Assuming there's one unique value per dataset
  
  # Combine the values into a data frame
  table_data <- data.frame(
    Dataset = dataset_medians$DataSet_NonTunned,
    NRowTrainData_Tunned = nrow_train_data$NRowTrainData_Tunned,
    Median_IVDP = dataset_medians$IVDP_RelativeImprovPercentage
  )
  
  # Sort the table by Median_IVDP
  
  # Sort the table by Median_IVDP
  table_data <- table_data[order(table_data$Median_IVDP), ]
  
  # Reshape the data to a horizontal format
  horizontal_table <- data.frame(
    Metric = c("NRowTrainData_Tunned", "Median_IVDP"),
    t(as.matrix(table_data[, -1]))  # Transpose the dataset values
  )
  
  # Set the column names
  colnames(horizontal_table) <- c("Metric", as.character(table_data$Dataset))
  
  
  library(xtable)
  
  # Create LaTeX table
  latex_table <- xtable(table_data, caption = "Summary of Dataset Performance Metrics", 
                        label = "tab:dataset_summary")
  # Print the table in LaTeX format
  print(latex_table, type = "latex", include.rownames = FALSE)
 
  
  
  ROCsubset_data$DataSet_NonTunned <- gsub("\\.csv$", "", ROCsubset_data$DataSet_NonTunned)
  
  # Step 1: Calculate the median of CVDP_RelativeImprovPercentage for each dataset
  dataset_medians <- aggregate(CVDP_RelativeImprovPercentage ~ DataSet_NonTunned, 
                               data = ROCsubset_data, FUN = median, na.rm = TRUE)
  summary(dataset_medians$CVDP_RelativeImprovPercentage)
  # Get the NRowTrainData_Tunned values
  nrow_train_data <- aggregate(NRowTrainData_Tunned ~ DataSet_NonTunned, 
                               data = ROCsubset_data, FUN = unique)  # Assuming there's one unique value per dataset
  
  # Combine the values into a data frame
  table_data <- data.frame(
    Dataset = dataset_medians$DataSet_NonTunned,
    NRowTrainData_Tunned = nrow_train_data$NRowTrainData_Tunned,
    Median_CVDP = dataset_medians$CVDP_RelativeImprovPercentage
  )
  
  # Sort the table by Median_CVDP
  
  # Sort the table by Median_CVDP
  table_data <- table_data[order(table_data$Median_CVDP), ]
  
  # Reshape the data to a horizontal format
  horizontal_table <- data.frame(
    Metric = c("NRowTrainData_Tunned", "Median_CVDP"),
    t(as.matrix(table_data[, -1]))  # Transpose the dataset values
  )
  
  # Set the column names
  colnames(horizontal_table) <- c("Metric", as.character(table_data$Dataset))
  
  
  library(xtable)
  
  # Create LaTeX table
  latex_table <- xtable(table_data, caption = "Summary of Dataset Performance Metrics", 
                        label = "tab:dataset_summary")
  # Print the table in LaTeX format
  print(latex_table, type = "latex", include.rownames = FALSE)
  
   
  
  
   
  
  # Combine the dataframes
  
  ncol(ROCsubset_data)
  ncol(Fsubset_data)
  ncol(Recallsubset_data)
  ncol(Precisionsubset_data)
  ncol(Accuracysubset_data)
  
  unique(ROCsubset_data$Model_Tunned)
  unique(Fsubset_data$Model_Tunned)
  unique(Recallsubset_data$Model_Tunned)
  unique(Accuracysubset_data$Model_Tunned)
  
  nrow(Fsubset_data)
  nrow(ROCsubset_data)
  nrow(Recallsubset_data)
  nrow(Precisionsubset_data)
  nrow(Accuracysubset_data)
  
  # Function to return the data points corresponding to the small quantile
  get_small_quantile_data <- function(data, column) {
    small_quantile <- quantile(data[[column]], probs = 1/3)
    subset(data, data[[column]] <= small_quantile)
  }
  
  # Function to return the data points corresponding to the large quantile
  get_large_quantile_data <- function(data, column) {
    large_quantile <- quantile(data[[column]], probs = 2/3)
    subset(data, data[[column]] >= large_quantile)
  }
  
   
  small_quantile_ROC <- get_small_quantile_data(ROCsubset_data, "NRowTrainData_Tunned")
  large_quantile_ROC <- get_large_quantile_data(ROCsubset_data, "NRowTrainData_Tunned")
  
  
  min(large_quantile_ROC$NRowTrainData_Tunned)
  max(large_quantile_ROC$NRowTrainData_Tunned)
  
  min(small_quantile_ROC$NRowTrainData_Tunned)
  max(small_quantile_ROC$NRowTrainData_Tunned)
  
  nrow(small_quantile_ROC)
  nrow(large_quantile_ROC)   
  
  count(small_quantile_ROC$DataSet_Tunned)
  count(large_quantile_ROC$DataSet_Tunned)
  
 
  small_quantile_Recall <- get_small_quantile_data(Recallsubset_data, "NRowTrainData_Tunned")
  large_quantile_Recall <- get_large_quantile_data(Recallsubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_Recall)
  nrow(large_quantile_Recall)   
  
  #small_quantile_Recall <- small_quantile_Recall[is.finite(small_quantile_Recall[, "CVDP_RelativeImprovPercentage"]), ]
  #small_quantile_Recall <- small_quantile_Recall[is.finite(small_quantile_Recall[, "IVDP_RelativeImprovPercentage"]), ]
  
  nrow(small_quantile_Recall)
  nrow(large_quantile_Recall)   
  
 
  #large_quantile_Recall <- large_quantile_Recall[is.finite(large_quantile_Recall[, "CVDP_RelativeImprovPercentage"]), ]
  #large_quantile_Recall <- large_quantile_Recall[is.finite(large_quantile_Recall[, "IVDP_RelativeImprovPercentage"]), ]
 
  stTest <- wilcox.test(large_quantile_Recall$IVDP_RelativeImprovPercentage, 
                        large_quantile_Recall$CVDP_RelativeImprovPercentage,
                        alternative = "g", paired = FALSE)
  
  # Calculate effect size (Cliff's delta)
  Eff_debug1 <- cliff.delta(large_quantile_Recall$IVDP_RelativeImprovPercentage, 
                            large_quantile_Recall$CVDP_RelativeImprovPercentage,
                            paired = FALSE)
  
  
  small_quantile_Precision <- get_small_quantile_data(Precisionsubset_data, "NRowTrainData_Tunned")
  large_quantile_Precision <- get_large_quantile_data(Precisionsubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_Precision)
  nrow(large_quantile_Precision)
  
  
  
  #small_quantile_Precision <- small_quantile_Precision[is.finite(small_quantile_Precision[, "CVDP_RelativeImprovPercentage"]), ]
  #small_quantile_Precision <- small_quantile_Precision[is.finite(small_quantile_Precision[, "IVDP_RelativeImprovPercentage"]), ]
 
  #large_quantile_Precision <- large_quantile_Precision[is.finite(large_quantile_Precision[, "CVDP_RelativeImprovPercentage"]), ]
  #large_quantile_Precision <- large_quantile_Precision[is.finite(large_quantile_Precision[, "IVDP_RelativeImprovPercentage"]), ]
  
 
  # Store results in lists
 
  small_quantile_F <- get_small_quantile_data(Fsubset_data, "NRowTrainData_Tunned")
  large_quantile_F <- get_large_quantile_data(Fsubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_F)
  nrow(large_quantile_F)   
 
  
  small_quantile_Accuracy <- get_small_quantile_data(Accuracysubset_data, "NRowTrainData_Tunned")
  large_quantile_Accuracy <- get_large_quantile_data(Accuracysubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_Accuracy)
  nrow(large_quantile_Accuracy)   
 
  
  ####################################################################
  
  
  Large_datasets_above_median <- rbind(large_quantile_ROC,  large_quantile_Recall, large_quantile_Precision, large_quantile_F, large_quantile_Accuracy)
  Small_datasets_below_median <- rbind(small_quantile_ROC,  small_quantile_Recall, small_quantile_Precision, small_quantile_F, small_quantile_Accuracy)
  
  
  Large_datasets_above_median$Group <- factor(Large_datasets_above_median$Group, levels = c("AUC", "Recall","Precision", "F-Measure", "Accuracy"))
  Small_datasets_below_median$Group <- factor(Small_datasets_below_median$Group, levels = c("AUC", "Recall","Precision", "F-Measure", "Accuracy"))
  
 
  Small_datasets_below_median$DataSet_NonTunned
  unique(Small_datasets_below_median$DataSet_Tunned)
  length(unique(Small_datasets_below_median$DataSet_Tunned))
  unique(Large_datasets_above_median$DataSet_Tunned)
  length(unique(Large_datasets_above_median$DataSet_Tunned))
 
  
  
  nrow(Fsubset_data)
  nrow(ROCsubset_data)
  nrow(Recallsubset_data)
  
  nrow(Small_datasets_below_median)
  nrow(Large_datasets_above_median)
  
  
  
  levels(Small_datasets_below_median$Model_Tunned)
  
  wilcox_results <- list()
  effect_sizes <- list()
  # Get unique levels of Model_Tunned
  
  
  
  Group_levels <- unique(Small_datasets_below_median$Group)
  
    #subset_data$Model_Tunned<-levels(c( "ada", "AdaBag"))
  
 
 
  # Loop through each level of Model_Tunned
  for (group_level in Group_levels) {
    # Subset data for the current model level
    combined_group_data = subset(Small_datasets_below_median, Group %in% group_level)
    
    
    
    length(combined_group_data$Model_Tunned)
    
    # Perform Wilcoxon signed-rank test
    stTest <- wilcox.test(combined_group_data$IVDP_RelativeImprovPercentage, 
                          combined_group_data$CVDP_RelativeImprovPercentage,
                          alternative = "g", paired = FALSE)
    
    # Calculate effect size (Cliff's delta)
    Eff_debug1 <- cliff.delta(combined_group_data$IVDP_RelativeImprovPercentage, 
                              combined_group_data$CVDP_RelativeImprovPercentage,
                              paired = FALSE)
    # Store results in lists
    wilcox_results[[group_level]] <- stTest
    effect_sizes[[group_level]] <- Eff_debug1
  }
  
  
  # summary(subset(subset_data, Model_Tunned %in% "ada")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$IVDP_RelativeImprovPercentage)
  # 
  # summary(subset(subset_data, Model_Tunned %in% "ada")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$CVDP_RelativeImprovPercentage)
  # 
  
  effVal<- ""
  for (group_level in Group_levels) {
    stTest <- wilcox_results[[group_level]] 
    Eff_debug1 <- effect_sizes[[group_level]]
    

    
    if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.001)
    {
      startVal="*";
    }else
    {
      
      startVal="(-)"
      
    }
    
    
    if(Eff_debug1$magnitude=="small")
    {
      EffSizeShort = "S"
    } else if(Eff_debug1$magnitude=="medium")
    {
      
      EffSizeShort = "M"
    } else if(Eff_debug1$magnitude=="large")
    {
      
      EffSizeShort = "L"
    }  else  
    {
      
      EffSizeShort = "N"
    }
    
    
    #effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",Eff_debug1$magnitude,") ",startVal,sep='')
    #effVal=paste(" (",EffSizeShort,") ",startVal,sep='')
   # legend('bottomleft',bty="n",inset=c(stepPosition,-0.171), legend = effVal, cex = 0.95)
    effVal=paste(effVal, (specify_decimal(abs(Eff_debug1$estimate),2))," (",EffSizeShort,") ",startVal,   "    \t         ",sep='')
    
    stepPosition= stepPosition+0.3047
    
    
  }
  
  
  par(mgp = c(2, 0.8, 0), mar = c(6, 3, 1, 2) + 0.1, oma = c(0, 0, 0, 0), inner = c(0, 0, 0, 0))
  
  
   
  
  ggplot(Small_datasets_below_median, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
    scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
    #ylim(-20, 40) +
    coord_cartesian(ylim =  c(-30, 70)) +
    labs(
      x = NULL,
      y = paste("", " Performance Impact (%) "),
      title = NULL
    ) +
    labs(tag = paste("Effect Size =   ", effVal ,sep='')) +     
    theme_minimal() +
    theme(plot.margin = margin(b = 30, l=10), plot.tag.position = c(0.52, -0.05), plot.tag = element_text(size = 8),  axis.text.x = element_text(face = "bold", size = 9, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 13)
          ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
    facet_wrap(~ Group, scales = "fixed", ncol = 5)
 
  # Save the ggplot to a file
  ggsave(paste(pathToSave,"/",expMode,"vsExp1",enableIVDP_one_release,"ggplotSmallDatasetsImpactPercentage.pdf",sep=''), plot = last_plot(), device = "pdf", width = 5, height = 3.2, units = "in", dpi = 300)
  
  
  
   
  
  
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  
  
  
  wilcox_results <- list()
  effect_sizes <- list()
  # Get unique levels of Model_Tunned
  
  
  
  Group_levels <- unique(Large_datasets_above_median$Group)
  Large_datasets_above_median$Group <- factor(Large_datasets_above_median$Group, levels = Group_levels)
  #subset_data$Model_Tunned<-levels(c( "ada", "AdaBag"))
  
  
  
  # Loop through each level of Model_Tunned
  for (group_level in Group_levels) {
    # Subset data for the current model level
    combined_group_data = subset(Large_datasets_above_median, Group %in% group_level)
    
    
    
    length(combined_group_data$Model_Tunned)
    
    # Perform Wilcoxon signed-rank test
    stTest <- wilcox.test(combined_group_data$IVDP_RelativeImprovPercentage, 
                          combined_group_data$CVDP_RelativeImprovPercentage,
                          alternative = "g", paired = FALSE)
    
    # Calculate effect size (Cliff's delta)
    Eff_debug1 <- cliff.delta(combined_group_data$IVDP_RelativeImprovPercentage, 
                              combined_group_data$CVDP_RelativeImprovPercentage,
                              paired = FALSE)
    # Store results in lists
    wilcox_results[[group_level]] <- stTest
    effect_sizes[[group_level]] <- Eff_debug1
  }
  
  
  # summary(subset(subset_data, Model_Tunned %in% "ada")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$IVDP_RelativeImprovPercentage)
  # 
  # summary(subset(subset_data, Model_Tunned %in% "ada")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$CVDP_RelativeImprovPercentage)
  # 
  
  
  
  effVal<- ""
  for (group_level in Group_levels) {
    stTest <- wilcox_results[[group_level]] 
    Eff_debug1 <- effect_sizes[[group_level]]
    

    if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.001)
    {
      startVal="*";
    }else
    {
      
      startVal="(-)"
      
    }
    
    
    if(Eff_debug1$magnitude=="small")
    {
      EffSizeShort = "S"
    } else if(Eff_debug1$magnitude=="medium")
    {
      
      EffSizeShort = "M"
    } else if(Eff_debug1$magnitude=="large")
    {
      
      EffSizeShort = "L"
    }  else  
    {
      
      EffSizeShort = "N"
    }
    
    
    #effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",Eff_debug1$magnitude,") ",startVal,sep='')
    #effVal=paste(" (",EffSizeShort,") ",startVal,sep='')
    effVal=paste(effVal, (specify_decimal(abs(Eff_debug1$estimate),2))," (",EffSizeShort,") ",startVal,   "    \t         ",sep='')
    
    stepPosition= stepPosition+0.3047
    
    
  }
  
  
  par(mgp = c(2, 0.8, 0), mar = c(6, 3, 1, 2) + 0.1, oma = c(0, 0, 0, 0), inner = c(0, 0, 0, 0))
  
  

  ggplot(Large_datasets_above_median, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
    scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
    #ylim(-20, 40) +
    coord_cartesian(ylim =  c(-30, 70)) +
    labs(
      x = NULL,
      y = paste("", " Performance Impact (%) "),
      title = NULL
    ) +
    labs(tag = paste("Effect Size =   ", effVal ,sep='')) +     
    theme_minimal() +
    theme(plot.margin = margin(b = 30, l=10), plot.tag.position =  c(0.52, -0.05), plot.tag = element_text(size = 8),  axis.text.x = element_text(face = "bold", size = 9, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 13)
          ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
    facet_wrap(~ Group, scales = "fixed", ncol = 5)
 
  # Save the ggplot to a file
  ggsave(paste(pathToSave,"/",expMode,"vsExp1",enableIVDP_one_release,"ggplotLargeDatasetsImpactPercentage.pdf",sep=''), plot = last_plot(), device = "pdf", width = 5, height = 3.2, units = "in", dpi = 300)
  
  
   
  
### RQ3 Median
  
  # 
  # # Function to return the data points corresponding to the small quantile
  # get_small_quantile_data <- function(data, column) {
  #   small_quantile <- quantile(data[[column]], probs = 1/3)
  #   subset(data, data[[column]] <= small_quantile)
  # }
  # 
  # # Function to return the data points corresponding to the large quantile
  # get_large_quantile_data <- function(data, column) {
  #   large_quantile <- quantile(data[[column]], probs = 2/3)
  #   subset(data, data[[column]] >= large_quantile)
  # }
  # 
  
  # Function to return the data points corresponding to the smaller than or equal to the median
  get_smaller_than_median_data <- function(data, column) {
    median_value <- median(data[[column]])
    subset(data, data[[column]] <= median_value)
  }
  
  # Function to return the data points corresponding to the larger than or equal to the median
  get_larger_than_median_data <- function(data, column) {
    median_value <- median(data[[column]])
    subset(data, data[[column]] > median_value)
  }
  
  
  
  
  small_quantile_ROC <- get_smaller_than_median_data(ROCsubset_data, "NRowTrainData_Tunned")
  large_quantile_ROC <- get_larger_than_median_data(ROCsubset_data, "NRowTrainData_Tunned")
  
  
  min(large_quantile_ROC$NRowTrainData_Tunned)
  max(large_quantile_ROC$NRowTrainData_Tunned)
  
  min(small_quantile_ROC$NRowTrainData_Tunned)
  max(small_quantile_ROC$NRowTrainData_Tunned)
  
  count(unique(small_quantile_ROC$DataSet_Tunned))
  count(unique(large_quantile_ROC$DataSet_Tunned))
  
  nrow(small_quantile_ROC)
  nrow(large_quantile_ROC)   
  
  
  small_quantile_Recall <- get_smaller_than_median_data(Recallsubset_data, "NRowTrainData_Tunned")
  large_quantile_Recall <- get_larger_than_median_data(Recallsubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_Recall)
  nrow(large_quantile_Recall)   
  
  #small_quantile_Recall <- small_quantile_Recall[is.finite(small_quantile_Recall[, "CVDP_RelativeImprovPercentage"]), ]
  #small_quantile_Recall <- small_quantile_Recall[is.finite(small_quantile_Recall[, "IVDP_RelativeImprovPercentage"]), ]
  
  nrow(small_quantile_Recall)
  nrow(large_quantile_Recall)   
  
  
  #large_quantile_Recall <- large_quantile_Recall[is.finite(large_quantile_Recall[, "CVDP_RelativeImprovPercentage"]), ]
  #large_quantile_Recall <- large_quantile_Recall[is.finite(large_quantile_Recall[, "IVDP_RelativeImprovPercentage"]), ]
  
  stTest <- wilcox.test(large_quantile_Recall$IVDP_RelativeImprovPercentage, 
                        large_quantile_Recall$CVDP_RelativeImprovPercentage,
                        alternative = "g", paired = FALSE)
  
  # Calculate effect size (Cliff's delta)
  Eff_debug1 <- cliff.delta(large_quantile_Recall$IVDP_RelativeImprovPercentage, 
                            large_quantile_Recall$CVDP_RelativeImprovPercentage,
                            paired = FALSE)
  
  
  small_quantile_Precision <- get_smaller_than_median_data(Precisionsubset_data, "NRowTrainData_Tunned")
  large_quantile_Precision <- get_larger_than_median_data(Precisionsubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_Precision)
  nrow(large_quantile_Precision)
  
  
  
  #small_quantile_Precision <- small_quantile_Precision[is.finite(small_quantile_Precision[, "CVDP_RelativeImprovPercentage"]), ]
  #small_quantile_Precision <- small_quantile_Precision[is.finite(small_quantile_Precision[, "IVDP_RelativeImprovPercentage"]), ]
  
  #large_quantile_Precision <- large_quantile_Precision[is.finite(large_quantile_Precision[, "CVDP_RelativeImprovPercentage"]), ]
  #large_quantile_Precision <- large_quantile_Precision[is.finite(large_quantile_Precision[, "IVDP_RelativeImprovPercentage"]), ]
  
  
  # Store results in lists
  
  small_quantile_F <- get_smaller_than_median_data(Fsubset_data, "NRowTrainData_Tunned")
  large_quantile_F <- get_larger_than_median_data(Fsubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_F)
  nrow(large_quantile_F)   
  
  
  small_quantile_Accuracy <- get_smaller_than_median_data(Accuracysubset_data, "NRowTrainData_Tunned")
  large_quantile_Accuracy <- get_larger_than_median_data(Accuracysubset_data, "NRowTrainData_Tunned")
  nrow(small_quantile_Accuracy)
  nrow(large_quantile_Accuracy)   
  
  
  ####################################################################
  
  
  Large_datasets_above_median <- rbind(large_quantile_ROC,  large_quantile_Recall, large_quantile_Precision, large_quantile_F, large_quantile_Accuracy)
  Small_datasets_below_median <- rbind(small_quantile_ROC,  small_quantile_Recall, small_quantile_Precision, small_quantile_F, small_quantile_Accuracy)
  
  
  Large_datasets_above_median$Group <- factor(Large_datasets_above_median$Group, levels = c("AUC", "Recall","Precision", "F-Measure", "Accuracy"))
  Small_datasets_below_median$Group <- factor(Small_datasets_below_median$Group, levels = c("AUC", "Recall","Precision", "F-Measure", "Accuracy"))
  
  
  Small_datasets_below_median$DataSet_NonTunned
  unique(Small_datasets_below_median$DataSet_Tunned)
  length(unique(Small_datasets_below_median$DataSet_Tunned))
  unique(Large_datasets_above_median$DataSet_Tunned)
  length(unique(Large_datasets_above_median$DataSet_Tunned))
  
  
  
  nrow(Fsubset_data)
  nrow(ROCsubset_data)
  nrow(Recallsubset_data)
  
  nrow(Small_datasets_below_median)
  nrow(Large_datasets_above_median)
  
  
  
  levels(Small_datasets_below_median$Model_Tunned)
  
  wilcox_results <- list()
  effect_sizes <- list()
  # Get unique levels of Model_Tunned
  
  
  
  Group_levels <- unique(Small_datasets_below_median$Group)
  
  #subset_data$Model_Tunned<-levels(c( "ada", "AdaBag"))
  
  
  
  # Loop through each level of Model_Tunned
  for (group_level in Group_levels) {
    # Subset data for the current model level
    combined_group_data = subset(Small_datasets_below_median, Group %in% group_level)
    
    
    
    length(combined_group_data$Model_Tunned)
    
    # Perform Wilcoxon signed-rank test
    stTest <- wilcox.test(combined_group_data$IVDP_RelativeImprovPercentage, 
                          combined_group_data$CVDP_RelativeImprovPercentage,
                          alternative = "g", paired = FALSE)
    
    # Calculate effect size (Cliff's delta)
    Eff_debug1 <- cliff.delta(combined_group_data$IVDP_RelativeImprovPercentage, 
                              combined_group_data$CVDP_RelativeImprovPercentage,
                              paired = FALSE)
    # Store results in lists
    wilcox_results[[group_level]] <- stTest
    effect_sizes[[group_level]] <- Eff_debug1
  }
  
  
  # summary(subset(subset_data, Model_Tunned %in% "ada")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$IVDP_RelativeImprovPercentage)
  # 
  # summary(subset(subset_data, Model_Tunned %in% "ada")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$CVDP_RelativeImprovPercentage)
  # 
  
  effVal<- ""
  for (group_level in Group_levels) {
    stTest <- wilcox_results[[group_level]] 
    Eff_debug1 <- effect_sizes[[group_level]]
    
    
    
    if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.001)
    {
      startVal="*";
    }else
    {
      
      startVal="(-)"
      
    }
    
    
    if(Eff_debug1$magnitude=="small")
    {
      EffSizeShort = "S"
    } else if(Eff_debug1$magnitude=="medium")
    {
      
      EffSizeShort = "M"
    } else if(Eff_debug1$magnitude=="large")
    {
      
      EffSizeShort = "L"
    }  else  
    {
      
      EffSizeShort = "N"
    }
    
    
    #effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",Eff_debug1$magnitude,") ",startVal,sep='')
    #effVal=paste(" (",EffSizeShort,") ",startVal,sep='')
    # legend('bottomleft',bty="n",inset=c(stepPosition,-0.171), legend = effVal, cex = 0.95)
    effVal=paste(effVal, (specify_decimal(abs(Eff_debug1$estimate),2))," (",EffSizeShort,") ",startVal,   "    \t         ",sep='')
    
    stepPosition= stepPosition+0.3047
    
    
  }
  
  
  par(mgp = c(2, 0.8, 0), mar = c(6, 3, 1, 2) + 0.1, oma = c(0, 0, 0, 0), inner = c(0, 0, 0, 0))
  
  
  
  
  ggplot(Small_datasets_below_median, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
    scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
    #ylim(-20, 40) +
    coord_cartesian(ylim =  c(-30, 70)) +
    labs(
      x = NULL,
      y = paste("", " Performance Impact (%) "),
      title = NULL
    ) +
    labs(tag = paste("Effect Size =   ", effVal ,sep='')) +     
    theme_minimal() +
    theme(plot.margin = margin(b = 30, l=10), plot.tag.position = c(0.52, -0.05), plot.tag = element_text(size = 8),  axis.text.x = element_text(face = "bold", size = 9, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 13)
          ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
    facet_wrap(~ Group, scales = "fixed", ncol = 5)
  
  # Save the ggplot to a file
  ggsave(paste(pathToSave,"/",expMode,"vsExp1",enableIVDP_one_release,"ggplotMedianSmallDatasetsImpactPercentage.pdf",sep=''), plot = last_plot(), device = "pdf", width = 5, height = 3.2, units = "in", dpi = 300)
  
  
  
  
  
  
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  #####################################################################
  
  
  
  wilcox_results <- list()
  effect_sizes <- list()
  # Get unique levels of Model_Tunned
  
  
  
  Group_levels <- unique(Large_datasets_above_median$Group)
  Large_datasets_above_median$Group <- factor(Large_datasets_above_median$Group, levels = Group_levels)
  #subset_data$Model_Tunned<-levels(c( "ada", "AdaBag"))
  
  
  
  # Loop through each level of Model_Tunned
  for (group_level in Group_levels) {
    # Subset data for the current model level
    combined_group_data = subset(Large_datasets_above_median, Group %in% group_level)
    
    
    
    length(combined_group_data$Model_Tunned)
    
    # Perform Wilcoxon signed-rank test
    stTest <- wilcox.test(combined_group_data$IVDP_RelativeImprovPercentage, 
                          combined_group_data$CVDP_RelativeImprovPercentage,
                          alternative = "g", paired = FALSE)
    
    # Calculate effect size (Cliff's delta)
    Eff_debug1 <- cliff.delta(combined_group_data$IVDP_RelativeImprovPercentage, 
                              combined_group_data$CVDP_RelativeImprovPercentage,
                              paired = FALSE)
    # Store results in lists
    wilcox_results[[group_level]] <- stTest
    effect_sizes[[group_level]] <- Eff_debug1
  }
  
  
  # summary(subset(subset_data, Model_Tunned %in% "ada")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$IVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$IVDP_RelativeImprovPercentage)
  # 
  # summary(subset(subset_data, Model_Tunned %in% "ada")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "AdaBag")$CVDP_RelativeImprovPercentage)
  # summary(subset(subset_data, Model_Tunned %in% "rf")$CVDP_RelativeImprovPercentage)
  # 
  
  
  
  effVal<- ""
  for (group_level in Group_levels) {
    stTest <- wilcox_results[[group_level]] 
    Eff_debug1 <- effect_sizes[[group_level]]
    
    
    if(stTest$p.value<=0.05 &&stTest$p.value>0.01 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.01 &&stTest$p.value>0.001 )
    {
      startVal="*";
    }else if(stTest$p.value<=0.001)
    {
      startVal="*";
    }else
    {
      
      startVal="(-)"
      
    }
    
    
    if(Eff_debug1$magnitude=="small")
    {
      EffSizeShort = "S"
    } else if(Eff_debug1$magnitude=="medium")
    {
      
      EffSizeShort = "M"
    } else if(Eff_debug1$magnitude=="large")
    {
      
      EffSizeShort = "L"
    }  else  
    {
      
      EffSizeShort = "N"
    }
    
    
    #effVal=paste((specify_decimal(abs(Eff_debug1$estimate),2))," (",Eff_debug1$magnitude,") ",startVal,sep='')
    #effVal=paste(" (",EffSizeShort,") ",startVal,sep='')
    effVal=paste(effVal, (specify_decimal(abs(Eff_debug1$estimate),2))," (",EffSizeShort,") ",startVal,   "    \t         ",sep='')
    
    stepPosition= stepPosition+0.3047
    
    
  }
  
  
  par(mgp = c(2, 0.8, 0), mar = c(6, 3, 1, 2) + 0.1, oma = c(0, 0, 0, 0), inner = c(0, 0, 0, 0))
  
  
  
  ggplot(Large_datasets_above_median, aes(x = factor("IVDP"), y = IVDP_RelativeImprovPercentage)) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_boxplot(aes(x = factor("CVDP"), y = CVDP_RelativeImprovPercentage), fill = "lightgray", outlier.shape = NA) +
    scale_x_discrete(limits = rev(levels(factor(c("IVDP", "CVDP"))))) +  # Reverse factor levels
    #ylim(-20, 40) +
    coord_cartesian(ylim =  c(-30, 70)) +
    labs(
      x = NULL,
      y = paste("", " Performance Impact (%) "),
      title = NULL
    ) +
    labs(tag = paste("Effect Size =   ", effVal ,sep='')) +     
    theme_minimal() +
    theme(plot.margin = margin(b = 30, l=10), plot.tag.position =  c(0.52, -0.05), plot.tag = element_text(size = 8),  axis.text.x = element_text(face = "bold", size = 9, angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text( size = 13)
          ,     panel.border = element_rect(color = "#CCCCCC", fill = NA, size = 1) ) +
    facet_wrap(~ Group, scales = "fixed", ncol = 5)
  
  # Save the ggplot to a file
  ggsave(paste(pathToSave,"/",expMode,"vsExp1",enableIVDP_one_release,"ggplotMedianLargeDatasetsImpactPercentage.pdf",sep=''), plot = last_plot(), device = "pdf", width = 5, height = 3.2, units = "in", dpi = 300)
  
  
  
  
   
  
   