# Mohamed Sami Rakha
# Post-doctoral

library(caret)
library(GA)
library(kernlab)
library(gbm)
library(foreach)
library(ROCR)
library(filelock)
library(doParallel)
library(mltools)
library("future")
library(pROC)
library(doMC)
# Calculate the AUC using the pROC package
options(java.parameters = "-Xmx5g")
library(parallel) 
library(mlbench)
#out-of-sample  Generate BootStrap Training IDXs
getTrainIndices <- function(data, iterations) {
  training.indices <- list()
  tmp.indices <- list()
  for (j in 1:iterations) {
    # Generate bootstrap training samples with replacement
    tmp.indices[[j]] <- sample(nrow(data), replace = TRUE)
  }
  training.indices <- tmp.indices
}


getPredictionValues <- function(mlModel, testData) {
  predictions <- NULL
  
  predictions <- tryCatch(
    predict(mlModel, testData[,-ncol(testData)], type = "prob"),
    error = function(e) {
      #  message("Prediction structure does not have T lables:", conditionMessage(e))
      NULL  # Return a value or placeholder in case of error
    }
  )
  
  if (is.null(predictions)) {
    predictions <- tryCatch(
      predict(mlModel, testData[,-ncol(testData)], type = "response")$predictions,
      error = function(e) {
        # message("Prediction structure does not have T lables:", conditionMessage(e))
        NULL  # Return a value or placeholder in case of error
      }
    )
    
  }
  
  if (is.null(predictions)) {
    predictions <- tryCatch(
      #Get Probabilities for T-class (1- False Class Probabilities)
      1 - predict(mlModel, testData[,-ncol(testData)], type = "raw"),
      error = function(e) {
        #  message("Prediction structure does not have T lables:", conditionMessage(e))
        NULL  # Return a value or placeholder in case of error
      }
    )
  }
  
  if (is.null(predictions)) {
    predictions <- tryCatch(
      #Get Probabilities for T-class (1- False Class Probabilities)
      predict(mlModel, testData[,-ncol(testData)], type = "posterior"),
      error = function(e) {
        #  message("Prediction structure does not have T lables:", conditionMessage(e))
        NULL  # Return a value or placeholder in case of error
      }
    )
    
  }
  
  if (is.null(predictions)) {
    predictions <- tryCatch(
      #Get Probabilities for T-class (1- False Class Probabilities)
      predict(mlModel, testData[,-ncol(testData)], type = "response"),
      error = function(e) {
        # message("Prediction structure does not have T lables:", conditionMessage(e))
        predictions  # Return a value or placeholder in case of error
      }
    )
    
  }
  
  # Return a value (optional)
  return(predictions)
}

getTrueClassPredictions <- function(predictions) {
  predTureClass <- NULL
  
  predTureClass <- tryCatch(
    predictions[, "T"],
    error = function(e) {
      # message("Prediction structure does not have T lables:", conditionMessage(e))
      NULL  # Return a value or placeholder in case of error
    }
  )
  
  # Check if the result is NULL (indicating an error occurred)
  if (is.null(predTureClass)) {
    predTureClass <- tryCatch(
      predictions$prob[, 1],
      error = function(e) {
        # message("Prediction structure does not have prob lables:", conditionMessage(e))
        NULL  # Return a value or placeholder in case of error
      }
    )
    
  }
  
  if (is.null(predTureClass)) {
    predTureClass <- tryCatch(
      predictions$posterior[, "T"],
      error = function(e) {
        # message("Prediction structure does not have prob lables:", conditionMessage(e))
        NULL  # Return a value or placeholder in case of error
      }
    )
    
  }
  
  
  if (is.null(predTureClass)) {
    predTureClass <- tryCatch(
      predictions[, 1],
      error = function(e) {
        # message("Prediction structure does not have prob lables:", conditionMessage(e))
        predictions  # Return a value or placeholder in case of error
      }
    )
    
  }
  
  return(predTureClass)
  
}

getAUCValue <- function(predTureClass, labels) {
  
  pred <- prediction(predTureClass, labels)
  
  AUCResults <- NA
  tryCatch({
    # Step 4: Calculate TPR and FPR
    perf <- performance(pred, measure = "tpr", x.measure = "fpr")
    # Step 5: Calculate AUC
    auc <- performance(pred, measure = "auc")
    AUCResults<-auc@y.values[[1]]
  }, error = function(e) {
    # Handle error, if any
    AUCResults <- NA
    cat("Error occurred while calculating AUCResults:\n")
    print(e)
  })
  
  return(AUCResults)
}



getPredClasses <- function(predTureClass) {
  
  predictionsClasses <-
    factor(ifelse(predTureClass >= 0.500000000, "T", "F"))
  
  predictionsClasses <-
    factor(predictionsClasses, levels = c("T", "F"))
  
  return(predictionsClasses)
  
}


perfColomnNames <-   c(
  "TestROC", 
  "TestMCC",
  "TestPrecision", 
  "TestRecall",
  "TestF1", 
  "TestKappa",
  "TestAccuracy",
  "TestSpecificity",
  "TrainAccuracy",
  "TrainKappa",
  "TrainROC",
  "TrainSens",
  "TrainSpec",
  "TrainAUC",
  "TrainPrecision",
  "TrainRecall",
  "TrainF",
  "DataSet",
  "Model",
  "iteration",
  "RunTime",
  "NRowTrainData",
  "NRowTestData",
  "countTotalChecks",
  "nonEqualROCValues",
  "nonEqualAccuracyValues",
  "countNullModels"
)



isResultsAlreadyGenerated <- function(filename, dataset, mlalgo)  {
  
  
  if (file.exists(
    paste(filename, sep = "" )  )) 
  { 
    ReleaseBased_Results = read.csv(file =  paste(filename, sep = ""   ) , 
                                    header = FALSE
    )
    
    
    newReleaseColNames <-
      c(
        perfColomnNames
      )
    colnames(ReleaseBased_Results) <- newReleaseColNames
    checkRowReleaseBased_Results = subset(
      ReleaseBased_Results,
      as.character(ReleaseBased_Results$DataSet) == as.character(dataset) &
        as.character(ReleaseBased_Results$Model) == as.character(mlalgo)
    )
    numberOfRows = nrow(checkRowReleaseBased_Results)
    
    if (numberOfRows > 0)
    {
      return (TRUE)
    } 
    else
    {
      return (FALSE)
    }
    
  } else
  {
    headers <- paste(perfColomnNames, collapse = ",")
    write(
      headers,
      file = paste(filename,   sep = "" ),
      append = FALSE
    )
    return (FALSE)
  }
  
}



toUseCores = 0
if (detectCores() >= 8)
{
  toUseCores = 4
  
} else{
  toUseCores = 4
}
print("detectCores")
detectCores()
print("toUseCores")
toUseCores
no_cores <- detectCores()





print(paste("number of cores to cluster ",no_cores,"+++ < Cores",  sep = " " ))

cores_2_use <- floor(detectCores())
#cl <- makeCluster(cores_2_use, outfile = "")
#doParallel::registerDoParallel(cl)
#getDoParWorkers()
library("future")

library(doParallel)
# create the cluster for caret to use
#cl <- makePSOCKcluster(no_cores)
#registerDoParallel(cl)
#cl <- makeCluster(5)
#registerDoParallel(cl)
registerDoMC(cores = 1)



set.seed(895)
basepath = "/home/sami123/projects/def-miranska/sami123/SDP-Runs"
#basepath = "."


#print(optiFiles)  ## list all files in path


#classifiers <-  c("kknn")
#Working


# This is the orginal one
#classifiers <-  c("rotationForest","C5.0","mlpWeightDecay", "mlp","LMT","knn","svmRadial","kknn","J48","svmLinear","multinom","JRip","rf","rbf","LogitBoost", "nnet","avNNet","AdaBoost.M1" ,"rpart","RRF","ranger","ada","AdaBag")
#to add  ("naive_bayes", "pda", "gamboost", "gbm", "xgbTree","glm","rpart2","glmboost", "bagFDA", "xgbDART")

#classifiers <-  c("naive_bayes", "pda", "gamboost", "gbm", "xgbTree", "glm", "rpart2", "glmboost", "bagFDA", "xgbDART")



#TSE PAPER 28 ones
#classifiers <-  c("treebag","naive_bayes", "pda", "gamboost", "gbm" , "glm", "rpart2", "glmboost","rotationForest","C5.0","mlpWeightDecay", "mlp","LMT","knn","svmRadial","kknn","J48","svmLinear","JRip","rf","LogitBoost", "nnet","avNNet","AdaBoost.M1", "rpart","RRF","ranger","ada","AdaBag","rbf", "lda","multinom")



#classifiers <-  c("AdaBag","ada","treebag","J48","avNNet","JRip") #shorter
#classifiers <-  c("rotationForest","C5.0","mlpWeightDecay", "mlp","LMT","knn","svmRadial","kknn","J48","svmLinear","multinom","JRip","rf","rbf","LogitBoost", "nnet","avNNet","AdaBoost.M1" ,"rpart","RRF","ranger","ada","AdaBag")

#classifiers <-  c("rpart2")


#failedClassifiers <-  c("ORFlog", "pda","gamboost", "gbm", "xgbTree","fda","pcaNNet","glm","naive_bayes","extraTrees","rotationForest","gbm_h2o","xgbDART",dwdPoly", "deepboost", "cforest", "rpart2", "glmboost", "bartMachine","adaboost","bagFDA")


#

# ALL Possible
#classifiers <-  c("lm","qda","pls","svmPoly","glmnet","gam","bayesglm","ctree","BstLm","ctree2", "adaboost","AdaBag","bagFDA","bartMachine","ada","glmboost","rpart2","cforest","deepboost","dwdPoly","xgbDART","gbm_h2o","ranger","rotationForest","extraTrees","RRF",  "naive_bayes", "lda","rpart","treebag","glm","C5.0","AdaBoost.M1","avNNet","pcaNNet","nnet","fda","mlpWeightDecay","mlp","LMT","LogitBoost","knn","xgbTree","gbm","rbf","svmRadial","gamboost","rf","JRip","multinom"   , "pda", "svmLinear","J48","ORFlog","parRF", "kknn")
classifiers <-  c("naive_bayes", "pda", "multinom", "gbm" , "lda2", "rpart2", "glmboost","rotationForest","C5.0","mlpWeightDecay", "mlp","LMT","knn","svmRadial","kknn","J48","svmLinear","JRip","rf","LogitBoost", "nnet","avNNet","AdaBoost.M1", "rpart","RRF","ranger","ada","AdaBag")



#classifiers <-  c("adaboost","AdaBag","bagFDA","bartMachine","ada","glmboost","rpart2","cforest","deepboost","dwdPoly","xgbDART","gbm_h2o","ranger","rotationForest","extraTrees","RRF",  "naive_bayes", "lda","rpart","treebag","glm","C5.0","AdaBoost.M1","avNNet","pcaNNet","nnet","fda","mlpWeightDecay","mlp","LMT","LogitBoost","knn","xgbTree","gbm","rbf","svmRadial","gamboost","rf","JRip","multinom"   , "pda", "svmLinear","J48","ORFlog","parRF", "kknn")
#classifiers <-  c("rf")


args <- commandArgs(TRUE)
#myjob <- args[1] #1-126
targetMetric <- args[1] #1-126
#enableBootstrap <- args[3]
#tuningMethod <- args[4]#1-126

# Exp1 or Exp4
# Exp1 : one release in training - one release testing
# Exp4 : multiple releases in training - one release testing  
expMode <- args[2]

myjob <- args[3] #1-126

myjob <- as.numeric(myjob)
  # Replace this with your actual JobId value
# Calculate the index based on JobId
#index <- ((myjob - 1) %% length(classifiers)) + 1
# Select the classifier
#selectedClassifier <- classifiers[index]
#classifiers <- selectedClassifier


if (is.na(targetMetric))
{
  targetMetric = "ROC"
}

#if (is.na(enableBootstrap))
#{
  enableBootstrap = "boot"
  
#}

#if (is.na(tuningMethod))
#{
 # tuningMethod = "none"
#}


if (is.na(expMode))
{
  expMode <- "Exp1"
}



trainingDataFolder <- paste("/Datasets_TrainingOneReleaseTest_",expMode,"/",  sep = "" )

optiFiles <-
  list.files(paste(basepath, trainingDataFolder, sep = ""))

optiFiles = optiFiles[grepl("^.*\\.csv", optiFiles)]
length(optiFiles)



job.grid <- expand.grid(ds = optiFiles, ml = classifiers)



 
#tuneOptions<- c("grid", "none","random")

tuneOptions<- c( "grid", "random", "none")
 
 
totalJobs = length(optiFiles) * length(classifiers)

for ( tuningMethodLoop in (tuneOptions)) {

tuningMethod <- tuningMethodLoop
 


#1-126
metricGoal = NULL
if (targetMetric == "ROC")
{
  metricGoal = "ROC"
  
} else{
  metricGoal = targetMetric
  
}


#totalJobs -- 


 
 



#totalJobs -- 
for (currentJob in myjob:myjob) {

  result <- tryCatch({

  run.config <- job.grid[as.integer(currentJob),]
  dataset <- run.config[, 1]
  mlalgo <- run.config[, 2]
  datasetName = gsub(".csv", "", dataset)
  tl <- c(5)
  t = tl
  
  
  perfFilePathFinalModel <- paste(
    basepath,
    "/Results_",expMode,"/ReleaseBased_", "results_perf_",   metricGoal, "_", mlalgo, "_bootstrap_",  enableBootstrap, "_tuningMethod_", 
    tuningMethod,   "_",  t,"_",expMode,  ".csv", sep = "" )
  
  
  perfFilePathIterationModels <- paste(basepath, "/Results_",expMode,"_Iterations/ReleaseBased_", "results_perf_allIterationModels_", metricGoal, "_", mlalgo,
                                       "_bootstrap_", enableBootstrap, "_tuningMethod_", tuningMethod, "_", t,"_",expMode, ".csv",
                                       sep = ""
  )
  
  
  paramsFilePathSave <- paste(basepath, "/Results_",expMode,"_Params/ReleaseBased_",  "manualGrid_param_", metricGoal, "_", mlalgo, "_bootstrap_",  enableBootstrap, 
                              "_tuningMethod_",  tuningMethod,   "_", t,"_", expMode,".csv",  sep = "" )
  
  
  varImportFilePathSave  <- paste(basepath, "/Results_",expMode,"_VarImp/ReleaseBased_",  "manualGrid_varImpor_", metricGoal, "_",datasetName,"_", mlalgo, "_bootstrap_",  enableBootstrap, 
              "_tuningMethod_",  tuningMethod,   "_", t,"_", expMode,".csv",  sep = "" )
			  
  savedModelsFilePathSave  <- paste(basepath, "/Results_",expMode,"_SavedModels/ReleaseBased_",  "manualGrid_trainedModels_", metricGoal, "_",datasetName,"_", mlalgo, "_bootstrap_",  enableBootstrap, 
              "_tuningMethod_",  tuningMethod,   "_", t,"_", expMode,".rds",  sep = "" )
  
  
 
  
  
  if(isResultsAlreadyGenerated (perfFilePathFinalModel, dataset[1], mlalgo[1]))
  {
    
    cat("Already generated at ", perfFilePathFinalModel, "\n")
    next
  }
  
 # cl <- makePSOCKcluster(no_cores)
#registerDoParallel(cl)
 # cl <- makeClusterPSOCK(cores_2_use, master=nsl(Sys.info()['nodename']), revtunnel = TRUE, outfile = "", verbose = TRUE)
  
  #######################################################################################################################################   
  ###########################################Custom Fit Function to store models #########################################################
  #######################################################################################################################################   
  
  # Create an environment to store the model objects
  saved_models_env <- new.env()
  
  # Counter
  # Initialize a counter variable
  model_counter <- 0
  
  # Copy all model structure info from existing model type
  cust.mdl <- getModelInfo(mlalgo, regex = FALSE)[[1]]
  
  # Override fit function so that we can save the iteration
  cust.mdl$fit <-
    function(x = x,
             y = y,
             wts = wts,
             param = param,
             lev = lev,
             last = last,
             classProbs = classProbs,
             ...) {
      # Dont save the final pass (dont train the final model across the entire training set)
      
      # Increment the model counter
      model_counter <<- model_counter + 1
      
      # Fit the model
      fit.obj <- NULL
      
      fit.obj <- tryCatch(
        getModelInfo(mlalgo, regex = FALSE)[[1]]$fit(x, y, wts, param, lev, last, classProbs, ...),
        error = function(e) {
          #  message("Prediction structure does not have T lables:", conditionMessage(e))
          # Return a value or placeholder in case of error
          model_id <- paste0(model_counter)
          saved_models_env[[model_id]] <- NULL
          stop(e)
          
        }
      )
      
      
      #fit.obj <-
      # getModelInfo(mlalgo, regex = FALSE)[[1]]$fit(x, y, wts, param, lev, last, classProbs, ...)
      
      # Create an object with data to save and save it
      
      
      
      if (!last)
      {
        fit.data <- list(
          resample = rownames(x),
          mdl = fit.obj,
          #x, y, wts,
          param = param,
          lev = lev,
          last = last,
          classProbs = classProbs,
          other = list(...)
        )
        
        # Create a string representing the tuning params
        param.str <- paste(lapply(1:ncol(param), function(x) {
          paste0(names(param)[x], param[1, x])
        }), collapse = "-")
        # print("In function")
        
        # Add the model object to the environment using a unique identifier
        
        
        model_id <- paste0(model_counter)
        saved_models_env[[model_id]] <- fit.data
      }
      
      return (fit.obj)
    }
  
  #######################################################################################################################################   
  #######################################################################################################################################          
  
  
  #######################################################################################################################################         
  #####################################################Loading and preparing the training release data ################################## 
  #######################################################################################################################################
  trainingDataForRelease <-
    list.files(
      paste(
        basepath,
        trainingDataFolder,
        datasetName,
        "Training",
        sep = ""
      )
    )
  
  trainingData = NULL
  
  for (currentDataset in trainingDataForRelease)
  {
    trainingRelease <-
      read.csv(
        paste(
          basepath,
          trainingDataFolder,
          datasetName,
          "Training/",
          currentDataset,
          sep = ""
        ),
        sep = ",",
        header = TRUE
      )
    trainingRelease[, colnames(trainingRelease)[ncol(trainingRelease)]] <-
      as.factor(ifelse(trainingRelease[, colnames(trainingRelease)[ncol(trainingRelease)]] == "TRUE", "T", "F"))
    #Not Needed..Remove First column, (a Text column in all datasets....)
    #trainingRelease <- trainingRelease[,-1]
    transformLog <- function(y) {
      y <- log1p(y)
    }
    indep.log <-
      apply(trainingRelease[,-ncol(trainingRelease)], 2, function(x) {
        !(min(x) < 0)
      })
    indep.log <- names(indep.log[which(indep.log == TRUE)])
    trainingRelease[, indep.log] <-
      data.frame(apply(trainingRelease[, indep.log], 2, transformLog))
    trainingData = rbind(trainingData, trainingRelease)
    print(
      paste(
        "Adding Training Data... ",
        " -   ",
        currentDataset,
        " for testing release ",
        datasetName,
        "- current size: ",
        nrow(trainingRelease),
        " - Cumulative Training Size ",
        nrow(trainingData),
        sep = ""
      )
    )
    
  }
  
  
  trainingData[, ncol(trainingData)] <-
    factor(trainingData[, ncol(trainingData)], levels = c("T", "F"))
  #print(trainingData$bug)
  
  
  responeYTrainingRelease <-
    trainingData[, colnames(trainingData)[ncol(trainingData)]] #This is the response (Bug or No Bug)
  
  #######################################################################################################################################   
  #######################################################################################################################################  
  
  
  #######################################################################################################################################         
  #####################################################Loading and preparing the test release data ###################################### 
  #######################################################################################################################################
  testedRelease <-
    read.csv(
      paste(
        basepath,
        trainingDataFolder,
        dataset,
        sep = ""
      ),
      sep = ",",
      header = TRUE
    )
  testedRelease[, colnames(testedRelease)[ncol(testedRelease)]] <-
    as.factor(ifelse(testedRelease[, colnames(testedRelease)[ncol(testedRelease)]] == "TRUE", "T", "F"))
  responeYRelease <-
    testedRelease[, colnames(testedRelease)[ncol(testedRelease)]] #This is the response (Bug or No Bug)
  
  #Not Needed..Remove First column, (a Text column in all datasets....)
  #testedRelease <- testedRelease[,-1]
  transformLog <- function(y) {
    y <- log1p(y)
  }
  indep.log <-
    apply(testedRelease[,-ncol(testedRelease)], 2, function(x) {
      !(min(x) < 0)
    })
  indep.log <- names(indep.log[which(indep.log == TRUE)])
  testedRelease[, indep.log] <-
    data.frame(apply(testedRelease[, indep.log], 2, transformLog))
  
  testedRelease[, ncol(testedRelease)] <-
    factor(testedRelease[, ncol(testedRelease)], levels = c("T", "F"))
  # print(testedRelease$bug)
  #######################################################################################################################################         
  ####################################################################################################################################### 
  
  warning("checking condition")
  warning(paste("Working on the dataset ", dataset , ", and algo  ", mlalgo, sep = " "))
  # Model
  print(
    paste(
      "Model Type: ",
      mlalgo,
      " - ",
      " Dataset: ",
      dataset,
      "_ TuneLength: ",
      t,
      " BootStrap:100",
      sep = ""
    )
  )
  
  #######################################################################################################################################                 
  #############################################Setting the space of hyper-parameters for each ML Algorithm #############################         
  #######################################################################################################################################         
  # The search space for parameters.
  grid.value <- NULL
  default.value <- c()

  if (mlalgo == "C5.0") {
    grid.value <-
      expand.grid(
        trials = c(1, 10, 20, 40),
        model = c("rules", "tree") ,
        winnow = c(TRUE, FALSE)
      )
    
    default.value <- data.frame(model = "rules", winnow = FALSE,trials = 1)
    
  } else if (mlalgo == "AdaBoost.M1") {
    grid.value <-
      expand.grid(
        mfinal = c(50, 100, 150, 200, 250)  ,
        maxdepth = c(1, 2, 3, 4, 5),
        coeflearn = "Breiman"
      )
    
    default.value <- data.frame(mfinal=50, maxdepth = 1,coeflearn="Breiman")
    
  } else if (mlalgo == "avNNet") {
    grid.value <-
      expand.grid(
        size = c(1, 3, 5, 7, 9) ,
        decay = c(0, 0.0001, 0.001, 0.01, 0.1) ,
        bag = c(TRUE, FALSE)
      )
    
	## Changed default of Decay from 0 to 0.001 to avoid run errors
    default.value <- data.frame(size = 1, decay = 0.001, bag =FALSE)
    
  } else if (mlalgo == "pcaNNet") {
    grid.value <-
      expand.grid(size = c(1, 3, 5, 7, 9),
                  decay = c(0, 0.0001, 0.001, 0.01, 0.1))
    
    default.value <- data.frame(size = 1, decay = 0)
    
  } else if (mlalgo == "nnet") {
    grid.value <-
      expand.grid(size = c(1, 3, 5, 7, 9),
                  decay = c(0, 0.0001, 0.001, 0.01, 0.1))
    
    default.value <- data.frame(size = 1, decay = 0)
    
  } else if (mlalgo == "fda") {
    grid.value <-
      expand.grid(degree = 1,
                  nprune = c(10, 20, 30, 40, 50))
    
    default.value <- data.frame(degree = 1, nprune = 10)
    
  } else if (mlalgo == "mlpWeightDecay") {
    grid.value <-
      expand.grid(size = c(1, 3, 5, 7, 9),
                  decay = c(0, 0.0001, 0.001, 0.01, 0.1))
 
    default.value <- data.frame(size = 1, decay = 0)
    
  } else if (mlalgo == "mlp") {
    grid.value <- expand.grid(size = c(1, 3, 5, 7, 9))
    
    default.value <- data.frame(size = 1)
    
  } else if (mlalgo == "LMT") {
    grid.value <- expand.grid(iter = c(1, 21, 41, 61, 81))
    
    default.value <- data.frame(iter = 1)
    
  } else if (mlalgo == "gpls") {
    grid.value <- expand.grid(K.prov = c(1, 2, 3, 4, 5))
    
    default.value <- data.frame(K.prov = 1)
    
  } else if (mlalgo == "LogitBoost") {
    grid.value <- expand.grid(nIter = c(11, 21, 31, 41, 51))
    
    default.value <- data.frame(nIter = 11)
    
  } else if (mlalgo == "knn") {
    grid.value <- expand.grid(k = c(1, 5, 9, 13, 17))
    
    default.value <- data.frame(k = 1)
    
    
  } else if (mlalgo == "xgbTree") {
    grid.value <-
      expand.grid(
        nrounds = c(50, 100, 150, 200, 250),
        eta = 0.3,
        max_depth = c(1, 2, 3, 4, 5),
        gamma = 0,
        colsample_bytree = 0.8,
        min_child_weight = 1,
        subsample = 0.5
      )
    default.value <- data.frame(nrounds = 100,eta=0.3,max_depth=1,gamma=0, colsample_bytree=0.8, min_child_weight=1, subsample=0.5)
    
  } else if (mlalgo == "gbm") {
    grid.value <-
      expand.grid(
        n.trees = c(50, 100, 150, 200, 250),
        interaction.depth = c(1, 2, 3, 4, 5),
        shrinkage = 0.1,
        n.minobsinnode = 10
      )
    default.value <- data.frame(n.trees = 100, interaction.depth = 1, shrinkage = 0.1, n.minobsinnode=10)
    
    
  }  else if (mlalgo == "nb") {
    grid.value <- expand.grid(
      fL = 0,
      usekernel = c(TRUE, FALSE),
      adjust = 0
    )
    default.value <- data.frame(fL= 0, usekernel = FALSE,adjust=0)
    
  }  else if (mlalgo == "rbf") {
    grid.value <- expand.grid(size = c(11, 13, 15, 17, 19))
    
    default.value <- data.frame(size=11)
    
    
  } else if (mlalgo == "svmRadial") {
    grid.value <-
      expand.grid(C = c(0.25, 0.5, 1, 2, 4),
                  sigma = c(0.1, 0.3, 0.5, 0.7, 0.9))
    
    default.value <- data.frame(C = 1, sigma = 0.5)
    
  } else if (mlalgo == "gamboost") {
    grid.value <-
      expand.grid(mstop = c(50, 100, 150, 200, 250),
                  prune = c("yes", "no"))
    
    default.value <- data.frame(mstop=50,prune="no")
    
  } else if (mlalgo == "rf") {
    grid.value <- expand.grid(mtry = c(10, 20, 30, 40, 50))
    
    default.value <- data.frame(mtry=10)
    
  } else if (mlalgo == "JRip") {
    grid.value <-
      expand.grid(
        NumOpt = c(1, 2, 3, 4, 5)  ,
        NumFolds = 3,
        MinWeights = 2
      )
    
    default.value <- data.frame(NumOpt=2,NumFolds=3,MinWeights=2)
    
  }  else if (mlalgo == "multinom") {
    grid.value <- expand.grid(decay = c(0, 0.0001, 0.001, 0.01, 0.1))
    
    default.value <- data.frame(decay = 0)
    
  }  else if (mlalgo == "pda") {
    grid.value <- expand.grid(lambda = c(1, 2, 3, 4, 5))
    
    default.value <- data.frame(lambda = 1)
    
  } else if (mlalgo == "svmLinear") {
    ## This one need reLook- missing parameter
	## We using tune =5 instead, single value in the Kla paper
    ##  grid.value <- expand.grid(C = 1)
    
	grid.value <- expand.grid(C = c(1, 10, 20, 30, 40, 50) )
	
    default.value <- data.frame(C = 1)
    
  } else if (mlalgo == "J48") {
    ## This one need reLook- missing parameter
	## We using tune =5 instead, single value in the Kla paper
    #grid.value <- expand.grid(C = 0.25, M = 2)
    
    default.value <- data.frame(C = 0.25,M=2)
    
  } else if (mlalgo == "rpart") {
    #cart
    grid.value  <-
      expand.grid(cp = c(0.0001, 0.001, 0.01, 0.1, 0.5))
    
    default.value  <-expand.grid(cp=c(0.01))
    
  } else if (mlalgo == "naive_bayes") {
    #cart
  #  grid.value  <-
   #   expand.grid(
    #    laplace = 0,
    #    usekernel = c(FALSE, TRUE),
    #    adjust = 0
    #  )
    default.value  <-expand.grid(laplace=0, usekernel=FALSE,adjust=0 )
  }
  
  #######################################################################################################################################        
  ####################################################################################################################################### 
  
  
  # Get the test index
  set.seed(895)
  # 100 Iterations - BootStrap
  trainIDx = getTrainIndices(trainingData, 100)
  #out-of-sample Bootstrap ( get test IDXs)
  test_inds <- lapply(trainIDx, function(x) {
    inds <- setdiff(1:nrow(trainingData), x)
    return(sample(inds))
  })
  
  # Optimized Setting
  MySummary  <- function(data,
                         lev = NULL,
                         model = NULL) {
    a1 <- defaultSummary(data, lev, model)
    b1 <- twoClassSummary(data, lev, model)
    c1 <- prSummary(data, lev, model)
    out <- c(a1, b1, c1)
    out
  }
  
  warning("Working on the model now!... ")
  t = 5
  set.seed(895)
  #Start time before training 
  start.time <- proc.time()
  
  
  
  #######################################################################################################################################
  ###########################################Starting Training based on tuning and validation settings ##################################
  #######################################################################################################################################       
  #Change1
  warning("Starting to learn model now!")
  model.trainedModel<- NULL
  
  if (tuningMethod == "none")
  {
    if (enableBootstrap == "none")
    {
      
      
      warning("Tuning OFF -  Bootstrap OFF ")
      print("Tuning OFF -  Bootstrap OFF")
      #Tuning OFF
      #Bootstrap OFF
      
      #This model is 
      #saved_models_env <- new.env()
      
      # Counter
      # Initialize a counter variable
      # No models are trained
      model_counter <- 0
      number_of_resample_iteration <- 1
      
      model.trainedModel <-
        train(
          x = trainingData[,-ncol(trainingData)],
          y = responeYTrainingRelease,
          method = cust.mdl,
          tuneGrid = default.value,
          tuneLength = 1,
		 # importance = "impurity",
          #default value is 3
          metric = targetMetric,
          trControl = trainControl(
            method = "none" ,
            classProbs = TRUE,
			savePredictions = TRUE,
            summaryFunction = MySummary,
            allowParallel = FALSE, verbose = FALSE
          )
        )
      
    } else
    {
      warning("Tuning OFF -  Bootstrap ON ")
      print("Tuning OFF -  Bootstrap ON")
      #Tuning OFF
      #Bootstrap ON
      
      # Create an environment to store the model objects
      #saved_models_env <- new.env()
      
      # Counter
      # Initialize a counter variable
      model_counter <- 0
      number_of_resample_iteration <- 100
      # Copy all model structure info from existing model type
      
      model.trainedModel <-
        train(
          x = trainingData[, -ncol(trainingData)],
          y = responeYTrainingRelease,
          method = cust.mdl,
          metric = targetMetric,
          maximize = TRUE,
          tuneGrid = default.value,
          tuneLength = 1,
		  #importance = "impurity",
          #default value is 3
          trControl = trainControl(
            method = enableBootstrap ,
            number = number_of_resample_iteration,
            index = trainIDx,indexOut = test_inds ,
            returnResamp = "all",
            classProbs = TRUE,
            savePredictions = TRUE,
            summaryFunction = MySummary,
            allowParallel = FALSE , verbose = FALSE
          )
        )
      
      
    }
    
    
  } else
  {
    if (enableBootstrap == "none")
    {
      ### Cannot have this case
      ### Error: Only one model should be specified in tuneGrid with no resampling
      warning("Tuning ON -  Bootstrap OFF ")
      print("Tuning ON -  Bootstrap OFF")
      #Tuning ON
      #Bootstrap OFF
      number_of_resample_iteration <- 1
      model.trainedModel <-
        train(
          x = trainingData[,-ncol(trainingData)],
          y = responeYTrainingRelease,
          method = cust.mdl,
          metric = targetMetric,
          maximize = TRUE,
          tuneGrid = grid.value,
          tuneLength = 5,
		  #importance = "impurity",
          #nthread = 4,
          #verbose = FALSE,
          trControl = trainControl(
            method = "none" ,
            search = tuningMethod,
            classProbs = TRUE,
			savePredictions = TRUE,
            summaryFunction = MySummary,
            allowParallel = FALSE , verbose = FALSE
          )
        )
      
    } else
    {
      
      warning("Tuning ON -  Bootstrap ON ")
      print("Tuning ON -  Bootstrap ON")
      #Tuning ON
      #Bootstrap ON
      #saved_models_env <- new.env()
      #7amada
      # Counter
      # Initialize a counter variable
      model_counter <- 0
      number_of_resample_iteration <- 100
      nrow(trainingData)
      model.trainedModel <-
        train(
          x = trainingData[, -ncol(trainingData)],
          y = responeYTrainingRelease,
          method = cust.mdl,
          metric = targetMetric,
          maximize = TRUE,
          tuneGrid = grid.value,
          tuneLength = 5,
		  #importance = "impurity",
          #nthread = 4,
          #verbose = FALSE,
          trControl = trainControl(
            method = "boot" ,
            search = tuningMethod,
            number = number_of_resample_iteration ,
            index = trainIDx,indexOut = test_inds ,
            returnResamp = "all",
            classProbs = TRUE,
			savePredictions = TRUE,
            summaryFunction = MySummary,
            allowParallel = FALSE , verbose = FALSE
          )
        )
      
	  model.trainedModel
	 # print(model.trainedModel)
      
    }
    
  }
  
  warning("done with training model <---")
  
    
  if(isResultsAlreadyGenerated (perfFilePathFinalModel, dataset[1], mlalgo[1]))
  {
    
    cat("Already generated at ", perfFilePathFinalModel, "\n")
    next
  }
  
  
  #####################################################################################################################################                
  ############################ Testing and saving the results of all iteration models resulted from train function  ###################      
  
  
  #################################################### Check iteration models are valid ################################################
  #######################################################################################################################################        
  
  
  set.seed(895)
  num_models <- model_counter
  num_models <- length(ls(envir = saved_models_env))
  number_of_tunning <- num_models/number_of_resample_iteration
  
  
  number_of_indx_out <- length(model.trainedModel$control$indexOut)
  
  nonEqualValues <- 0
  nonEqualROCValues <- 0
  nonEqualAccuracyValues <- 0
  equalROCValues <- 0
  equalAccuracyValues <- 0
  
  countIteration <- 0
  countModels <- 0
  countTotalChecks <- 0
  countNullModels <- 0
  iterationModelPerf <- NULL
  allIterationsModelPerf <- NULL
  
  warning("done with training model <---")
  cat("Number of models ", num_models, "\n")
  cat("Number of models as model_counter ", model_counter, "\n")
  
  for (countModels in 1:num_models) {
     warning("Iteration modeling one by one <---")
    model_data <- saved_models_env[[as.character(countModels)]]
    
    if(all(is.na(model_data)) ||  is.null(model_data))
    {
      
      countNullModels <- countNullModels + 1
      
      next
    }
    
    training_resample_indxes <- numeric_values <- as.numeric(gsub("\\..*$", "", gsub("^X", "", model_data$resample)))
    # Get the testing indexes that are not in the training indexes
    testing_resample_indxes <- setdiff(1:nrow(trainingData), training_resample_indxes)
    
    countIteration <- 0
    for (indexMatch in 1:number_of_indx_out)
    {
      if (identical(sort(testing_resample_indxes), sort(model.trainedModel$control$indexOut[[indexMatch]])))
      {
        countIteration <-indexMatch 
        break
      }
      
    }
    
    #test_inds
    
    if (countIteration==0)   {
      nonEqualValues <- nonEqualValues +1
     # message("Test sample indexes are not identical. countIteration--> ",countIteration, "- countModel--> ",countModels)
      #countIteration = countIteration + 1
      # Probably some resample model has failed while building - rare case
      #next
      #stop("Script execution halted, faild to validate iteration models samples.")
      
      #Skip to the next re-sample
      next
      
    } 
    
    
    predictions <- getPredictionValues(model_data$mdl, trainingData[model.trainedModel$control$indexOut[[countIteration]],])
    
    labels <- trainingData[model.trainedModel$control$indexOut[[countIteration]], ncol(trainingData)]
    
    
    predTureClass <- getTrueClassPredictions(predictions)
    
    
    AUCResults <- getAUCValue(predTureClass, labels)
    
    predictionsClasses <- getPredClasses(predTureClass)
    
    resampleReTestingResults <-
      confusionMatrix(data = predictionsClasses,
                      reference = trainingData[model.trainedModel$control$indexOut[[countIteration]], ncol(trainingData)],
                      mode = "prec_recall")
    
    
    
    
    if(!is.na(model.trainedModel$resample[countIteration,"ROC"]))
    {
      ROCdiff=model.trainedModel$resample[countIteration,"ROC"] - AUCResults
      
      if(is.na(ROCdiff)|| round(ROCdiff,5) !=0)
      {
       # message("ROC is not identical.")
        nonEqualROCValues <- nonEqualROCValues +1
       # cat("Model ID:", countIteration, "\n")
       # cat("Accuracy:", resampleReTestingResults$overall["Accuracy"], "\n")
       # cat("Kappa:", resampleReTestingResults$overall["Kappa"], "\n")
       # cat("ROC:", ifelse(!is.na(AUCResults) , AUCResults , NA), "\n")
      #  cat("Precision:", resampleReTestingResults$byClass["Precision"], "\n")
      #  cat("Recall:", resampleReTestingResults$byClass["Recall"], "\n")
       # cat("F1-score:", resampleReTestingResults$byClass["F1"], "\n\n")
       # print(model.trainedModel$resample[countIteration, ])
        #  stop("Script execution halted, faild to validate iteration models samples (ROC is not identical).")
      } else
      {
        equalROCValues <- equalROCValues +1 
      }
    } 
    
    
    if(!is.na (model.trainedModel$resample[countIteration,"Accuracy"]))
    {
      if(!round(model.trainedModel$resample[countIteration,"Accuracy"],2)== round(resampleReTestingResults$overall["Accuracy"],2))
      {
        #message("Accuracy is not identical.")
        nonEqualAccuracyValues <- nonEqualAccuracyValues +1
       # cat("Model ID:", countIteration, "\n")
       # cat("Accuracy:", resampleReTestingResults$overall["Accuracy"], "\n")
       # cat("Kappa:", resampleReTestingResults$overall["Kappa"], "\n")
       # cat("ROC:", ifelse(!is.na(AUCResults) , AUCResults , NA), "\n")
        #cat("Precision:", resampleReTestingResults$byClass["Precision"], "\n")
       # cat("Recall:", resampleReTestingResults$byClass["Recall"], "\n")
       # cat("F1-score:", resampleReTestingResults$byClass["F1"], "\n\n")
       # print(model.trainedModel$resample[countIteration, ])
        # stop("Script execution halted, faild to validate iteration models samples (Accuracy is not identical).")
      }
      else
      {
        
        equalAccuracyValues <- equalAccuracyValues +1
      }
    }
    
    
    ## Saving results 
    
    
    colnames(testedRelease)
    
    predictionsNewRelease <- getPredictionValues(model_data$mdl, testedRelease)
    
    
    if(is.null(predictionsNewRelease))
    {
      
      trainingColNames <- colnames( trainingData)
      predictionsNewRelease <- getPredictionValues(model_data$mdl, testedRelease[, trainingColNames])
      
      
    }
    
    # Calculate the performance metrics resulted from the testing
    
    predTureClassNewRelease <- getTrueClassPredictions(predictionsNewRelease)
    testLabelsNewRelease <- testedRelease[, ncol(testedRelease)]
    AUCResultsNewRelease <- getAUCValue(predTureClassNewRelease, testLabelsNewRelease)
    predictionsClassesNewRelease <- getPredClasses(predTureClassNewRelease)
    
    newReleaseResults <-
      confusionMatrix(data = predictionsClassesNewRelease,
                      reference = testedRelease[, ncol(testedRelease)],
                      mode = "prec_recall")
    
    MCCResultsNewRelease <-
      mcc(predictionsClassesNewRelease, testedRelease[, ncol(testedRelease)])
    
    
    iterationModelPerf <-
      paste(
        AUCResultsNewRelease,
        MCCResultsNewRelease,
        newReleaseResults$byClass["Precision"],
        newReleaseResults$byClass["Recall"],
        newReleaseResults$byClass["F1"],
        newReleaseResults$overall["Kappa"],
        newReleaseResults$overall["Accuracy"],
        newReleaseResults$byClass["Specificity"],
        ifelse(!is.na(AUCResults), AUCResults , NA),
        resampleReTestingResults$overall["Accuracy"],
        resampleReTestingResults$overall["Kappa"], 
        resampleReTestingResults$byClass["Precision"], 
        resampleReTestingResults$byClass["Recall"], 
        resampleReTestingResults$byClass["F1"],  
        resampleReTestingResults$byClass["Specificity"],
        model.trainedModel$resample[countIteration, "ROC"],
        model.trainedModel$resample[countIteration, "Precision"],
        model.trainedModel$resample[countIteration, "Recall"],
        model.trainedModel$resample[countIteration, "Kappa"],
        model.trainedModel$resample[countIteration, "AUC"],
        model.trainedModel$resample[countIteration, "Accuracy"],
        model.trainedModel$resample[countIteration, "Sens"],
        model.trainedModel$resample[countIteration, "Spec"],
        model.trainedModel$resample[countIteration, "F"],
        model.trainedModel$resample[countIteration, "mtry"],
        model.trainedModel$resample[countIteration, "Resample"],
        dataset,
        mlalgo,
        t,
        "elapsed",
        nrow(trainingData),
        nrow(testedRelease),
        countIteration,
        countModels,
        sep = ","
      )
    # Aggregating all the results performance
	cat("row binding iterations \n")
    allIterationsModelPerf = rbind(allIterationsModelPerf, iterationModelPerf)
    countTotalChecks <- countTotalChecks +1
    
    
  }
  
  
  

  
  #####################################################################################################################################   
  ##############################################Testing the best model and saving results###########################################
  duration = proc.time() - start.time
  
  if(isResultsAlreadyGenerated (perfFilePathFinalModel, dataset[1], mlalgo[1]))
  {
    cat("Already generated at ", perfFilePathFinalModel, "\n")
    next
    
  }  
  
  # Get the best model based on the specified metric
  
  predictions <- getPredictionValues(model.trainedModel$finalModel, testedRelease)
  
  # Calculate the performance metrics resulted from the testing
  
  if(is.null(predictions))
  {
    
    trainingColNames <- colnames( trainingData)
    predictions <- getPredictionValues(model.trainedModel$finalModel, testedRelease[, trainingColNames])
    
    
  }
  
  
  predTureClass <- getTrueClassPredictions(predictions)
  
  
  testLabels <- testedRelease[, ncol(testedRelease)]
  AUCResults <- getAUCValue(predTureClass, testLabels)
  predictionsClasses <- getPredClasses(predTureClass)
  
  AllOtherResults <-
    confusionMatrix(data = predictionsClasses,
                    reference = testedRelease[, ncol(testedRelease)],
                    mode = "prec_recall")
  
  MCCResults <-
    mcc(predictionsClasses, testedRelease[, ncol(testedRelease)])
  duration = proc.time() - start.time
  
  print(" TOtal Time")
  print(duration)
  
  #Storing the trained Optimized model on the file system.
   saveRDS(
     model.trainedModel,
     file = savedModelsFilePathSave
     )
   
  
  
  #Storing the best parameters
  modelPerf = NULL
  modelParam = NULL
  
  
  if (enableBootstrap == "none")
  {
    # No training performance to store.
    modelPerf <-
      paste(
        AUCResults,
        MCCResults,
        AllOtherResults$byClass["Precision"],
        AllOtherResults$byClass["Recall"],
        AllOtherResults$byClass["F1"],
        AllOtherResults$overall["Kappa"],
        AllOtherResults$overall["Accuracy"],
        AllOtherResults$byClass["Specificity"],
        "TrainAccuracy" ,
        "TrainKappa" ,
        "TrainROC" ,
        "TrainSens" ,
        "TrainSpec" ,
        "TrainAUC" ,
        "TrainPrecision" ,
        "TrainRecall" ,
        "TrainF",
        dataset,
        mlalgo,
        t,
        duration["elapsed"],
        nrow(trainingData),
        nrow(testedRelease),
        countTotalChecks,
        nonEqualROCValues,
        nonEqualAccuracyValues,
        countNullModels,
        sep = ","
      )
  } else {
    modelPerf <-
      paste(
        AUCResults,
        MCCResults,
        AllOtherResults$byClass["Precision"],
        AllOtherResults$byClass["Recall"],
        AllOtherResults$byClass["F1"],
        AllOtherResults$overall["Kappa"],
        AllOtherResults$overall["Accuracy"],
        AllOtherResults$byClass["Specificity"],
        getTrainPerf(model.trainedModel)[, "TrainAccuracy"],
        getTrainPerf(model.trainedModel)[, "TrainKappa"],
        getTrainPerf(model.trainedModel)[, "TrainROC"],
        getTrainPerf(model.trainedModel)[, "TrainSens"],
        getTrainPerf(model.trainedModel)[, "TrainSpec"],
        getTrainPerf(model.trainedModel)[, "TrainAUC"],
        getTrainPerf(model.trainedModel)[, "TrainPrecision"],
        getTrainPerf(model.trainedModel)[, "TrainRecall"],
        getTrainPerf(model.trainedModel)[, "TrainF"],
        dataset,
        mlalgo,
        t,
        duration["elapsed"],
        nrow(trainingData),
        nrow(testedRelease),
        countTotalChecks,
        nonEqualROCValues,
        nonEqualAccuracyValues,
        countNullModels,
        sep = ","
      )
  }
  
  
  if(isResultsAlreadyGenerated (perfFilePathFinalModel, dataset[1], mlalgo[1]))
  {
    cat("Already generated at ", perfFilePathFinalModel, "\n")
    next
    
  }  
  

 
    
  file.lock =lock(perfFilePathFinalModel, exclusive = TRUE, timeout = 59900)

   #  write(
   #   modelPerf,
   #  file = perfFilePathFinalModel,
   # append = TRUE
   #)

  # Open the file for appending
  file_conn <- file(perfFilePathFinalModel, "a")
    if(isResultsAlreadyGenerated (perfFilePathFinalModel, dataset[1], mlalgo[1]))
  {
    cat("Already generated at ", perfFilePathFinalModel, "\n")
	  # Flush the data to the file
	close(file_conn)
	unlock(file.lock)
    next
    
  }  
  
  
  
    
   importance <- varImp(model.trainedModel, scale = FALSE)
    file.lockImportFilePathSave =lock(varImportFilePathSave, exclusive = TRUE, timeout = 9900)
    write.table(
      importance$importance,
      file = varImportFilePathSave,
      append = FALSE,
      col.names = TRUE,
      row.names = TRUE,
      sep = ","
    )
    unlock(file.lockImportFilePathSave)
  
  modelParam <- as.data.frame(model.trainedModel$bestTune[1,])
  modelParam[["ds"]] <- dataset
  modelParam[["ml"]] <- mlalgo
  #Storing the trained Optimized model on the file system.
  
  file.lockparamsFilePathSave =lock(paramsFilePathSave, exclusive = TRUE, timeout = 49900)
	if (!file.exists(paramsFilePathSave)) {
	  # File does not exist, write data with headers
	  write.table(
		modelParam,
		file = paramsFilePathSave,
		append = FALSE,  # Not appending, so headers will be written
		col.names = TRUE,
		row.names = FALSE,
		sep = ","
	  )
	} else {
	  # File exists, append data without headers
	  write.table(
		modelParam,
		file = paramsFilePathSave,
		append = TRUE,
		col.names = FALSE,  # Do not write column names again
		row.names = FALSE,
		sep = ","
	  )
	}
  unlock(file.lockparamsFilePathSave)
  
 

  
  
  #Storing a log
  #  write(myjob,file = paste(basepath,"/Runs_Log/RQ2/grid/ReleaseBased_","default_job_log.log",sep = ""),append = TRUE)
  
  
 
  cat(" Trying to save iterations \n")
  #append the results of all iteration models to a file
	  if (!is.null(allIterationsModelPerf))
	  {
		
		if (!file.exists(perfFilePathIterationModels
		)) {
		  modelIterationPerfHeaders <- paste(
			"Test_ROC",
			"Test_MCC",
			"Test_Precision",
			"Test_Recall",
			"Test_F1",
			"Test_Kappa",
			"Test_Accuracy",
			"Test_Specificity",
			"sampleReTestingROC",
			"sampleReTestingAccuracy",
			"sampleReTestingKappa", 
			"sampleReTestingPrecision", 
			"sampleReTestingRecall", 
			"sampleReTestingF1",  
			"sampleReTestingSpecificity",
			"Validate-ROC",
			"Validate-Precision",
			"Validate-Recall",
			"Validate-Kappa",
			"Validate-AUC",
			"Validate-Accuracy" ,
			"Validate-Sens",
			"Validate-Spec" ,
			"Validate-F" ,
			"mtry" ,
			"Resample" ,
			"dataset",
			"mlalgo",
			"t",
			"duration-elapsed",
			"nrow-trainingData",
			"nrow-testedRelease",
			"countIteration",
			"countModels",
			sep = ","
		  )
		  
		  file.lockIterationModels =lock(perfFilePathIterationModels, exclusive = TRUE, timeout = 49900)
		  
		  
		   write(
			 modelIterationPerfHeaders,
			 file = perfFilePathIterationModels,
			 append = FALSE
		  )
		  # Open the file for appending

		  file_conn <- file(perfFilePathIterationModels, "a")
		  
		  # Write your data frame to the file using cat
		  cat(modelIterationPerfHeaders, "\n", file = file_conn)
		  
		  # Flush the data to the file
		  flush(file_conn)
		  
		  # Close the file connection
		  close(file_conn)
				


		  unlock(file.lockIterationModels)
		  
		}
		
		## Save the performance metrics of each model iteration.
		cat("Yes saving iterations \n")
		file.lockIterationModels =lock(perfFilePathIterationModels, exclusive = TRUE, timeout = 49900)
		
		  write(
		   allIterationsModelPerf,
			file = perfFilePathIterationModels,
		  append = TRUE
		)


		 



		unlock(file.lockIterationModels)
		
	  }



       if(isResultsAlreadyGenerated (perfFilePathFinalModel, dataset[1], mlalgo[1]))
  {
    cat("Already generated at ", perfFilePathFinalModel, "\n")
	  # Flush the data to the file
	close(file_conn)
	unlock(file.lock)
    next
    
  }  
  
  # Write your data frame to the file using cat
  cat(modelPerf, "\n", file = file_conn)

  # Flush the data to the file
  flush(file_conn)

  # Close the file connection
  close(file_conn)

  unlock(file.lock)
  
  
  cat("Samples inmatches: ",nonEqualValues, "\n")
  cat("Total Checks Counts: ",countTotalChecks, "\n")
  cat("-- Performance ROC inmatches: ",nonEqualROCValues, "\n")
  cat("++ Performance ROC matches: ",equalROCValues, "\n")
  cat("-- Performance Accuracy inmatches: ",nonEqualAccuracyValues, "\n")
  cat("++ Performance Accuracy matches: ",equalAccuracyValues, "\n")
  cat(".. Count Null Iteration Models: ",countNullModels, "\n")
  
  

  
  }, error = function(e) {
    # Handle the error by printing a message
    message("An error occurred:", conditionMessage(e))
    # Return a default value
    NULL
  })

}


}
 
   # stopCluster(cl)
    #registerDoSEQ()
# stopCluster(cl)

#}
