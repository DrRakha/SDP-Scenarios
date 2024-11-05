# Contrasting-the-Hyperparameter-Tuning-Impact-Across-Software-Defect-Prediction-Scenarios

### Replication Package for [Research Paper Title]

## Overview

This repository contains the replication package for the research paper, _"Contrasting the Hyperparameter Tuning Impact Across Software Defect Prediction Scenarios,"_ authored by Mohamed Sami Rakha, Andriy Miranskyy, and Daniel Alencar da Costa. This package provides the necessary materials to replicate the results and experiments presented in the paper.

## Abstract

Software defect prediction (SDP) plays a crucial role in delivering high-quality software, helping teams optimize their quality assurance efforts to improve final product quality. Recent research shows that applying hyperparameter tuning can enhance prediction performance for specific SDP scenarios (e.g., predicting defects in future versions). However, the tuning impact may vary across different SDP scenarios. Comparing these impacts across scenarios is essential for improving the robustness, generalizability, and practicality of SDP modeling.

In this study, we contrast the impact of hyperparameter tuning across two critical SDP scenarios:
1. **Inner Version Defect Prediction (IVDP)**
2. **Cross Version Defect Prediction (CVDP)**

Key differences between these scenarios include the prediction scope and the evaluation setups used. Our experiments apply 28 machine learning algorithms, 45 post-release software datasets, two tuning algorithms, and five optimization metrics. Statistical analyses reveal significant differences in SDP performance impact, both overall and per algorithm, highlighting that smaller projects are more susceptible to larger performance differences. These results emphasize the importance of considering SDP scenarios when anticipating performance gains from hyperparameter tuning.

## Contents

- **`data/`**: Contains the datasets used in experiments.
- **`scripts/`**: Includes scripts for data processing, model training, and evaluation.
- **`results/`**: Stores output files and experiment results.
- **`README.md`**: This document, providing an overview and usage instructions.

## Prerequisites

Before running the code, ensure you have the following:

- **R** version 4.3.2 (2023-10-31 ucrt)
- Required R libraries (listed in each R script header)
- **RStudio** (recommended for convenience)
- Any additional tools or software noted in the paper

## Installation

Clone the repository to your local machine:

```bash
git clone https://github.com/YourUsername/Contrasting-the-Hyperparameter-Tuning-Impact-Across-Software-Defect-Prediction-Scenarios.git
 ```

## Running Experiments

This package supports two primary SDP evaluation setups:

- **Exp1**: Inner Version Defect Prediction (IVDP)
- **Exp4**: Cross Version Defect Prediction (CVDP)

### Dataset Paths for Each SDP Scenario

- **IVDP**: `data/Exp1/Datasets_TrainingOneReleaseTest_Exp1`
- **CVDP**: `data/Exp4/Datasets_TrainingOneReleaseTest_Exp4`

### Running Hyperparameter Tuning

To run hyperparameter tuning for a specific scenario, use the following R script:

- **Script**: `SourceCode/Run_PerModel_Runs_Save_Models.R`

Example command:

```bash
Rscript SourceCode/Run_PerModel_Runs_Save_Models.R <Optimization Metric> <SDP Scenario> <Job ID>
# Example
Rscript SourceCode/Run_PerModel_Runs_Save_Models.R Recall Exp1 1
```



<!-- Study Abstract -->

Software defect prediction (SDP) is crucial for delivering high-quality software products. The SDP activities help software teams better utilize their software quality assurance efforts, improving the quality of the final product.
Recent research has indicated that prediction performance improvements in SDP are achievable by applying hyperparameter tuning to a particular SDP scenario (e.g., predicting defects for a future version). However, the positive impact resulting from the hyperparameter tuning step may differ based on the targeted SDP scenario. Comparing the impact of hyperparameter tuning across two SDP scenarios is necessary to provide comprehensive insights and enhance the robustness, generalizability, and, eventually, the practicality of SDP modeling for quality assurance. 

Therefore, in this study, we contrast the impact of hyperparameter tuning across two pivotal and consecutive SDP scenarios: (1) Inner Version Defect Prediction (IVDP) and (2) Cross Version Defect Prediction (CVDP). The main distinctions between the two scenarios lie in the scope of defect prediction and the selected evaluation setups. This study's experiments use common evaluation setups, 28 machine learning (ML) algorithms, 45 post-release software datasets, two tunning algorithms, and five optimization metrics. We apply statistical analytics to compare the SDP performance impact differences by investigating the overall impact, the single ML algorithm impact, and variations across different software project sizes. 

The results indicate that the SDP gains within the IVDP scenario are significantly larger than those within the CVDP scenario. The results reveal that asserting performance gains for up to 22 ML algorithms may not hold across multiple SDP scenarios. Furthermore, we found that small software projects are more susceptible to larger differences in performance impacts. Overall, the study findings recommend software engineering researchers and practitioners to consider the effect of the selected SDP scenario when expecting performance gains from hyperparameter tuning. 

Contents
data/: Directory containing the data files used in the experiments.
scripts/: Directory containing the code scripts for data processing, model training, and evaluation.
results/: Directory for output files and results generated from the experiments.
README.md: This file, providing an overview and instructions.
Prerequisites
Before running the code, ensure that you have the following software installed:

R version 4.3.2 (2023-10-31 ucrt)
Required R libraries (listed in the header of each R script)
R-Studio is recommended
Any other software or tools specified in the paper
Installation
Clone the repository:

#Running Experiments 
There are two mean evaluation setups: 
Exp1: Inner Version Defect Prediction (IVDP) 
Exp4: Cross Version Defect Prediction (CVDP)

#The Datasets path per SDP scenario:
IVDP: Data/Exp1/Datasets_TrainingOneReleaseTest_Exp1
CVDP: Data/Exp4/Datasets_TrainingOneReleaseTest_Exp4

#R-Script to run hyperparameter tuning 
SourceCode/Run_PerModel_Runs_Save_Models.R

Following is an example of running this script: 

Rscript  SourceCode/Run_PerModel_Runs_Save_Models.R  <Optimization Metric> <SDP Scenario> <Job ID>
Rscript  SourceCode/Run_PerModel_Runs_Save_Models.R  Recall Exp1 1


#Scripts to generate figures: 
To generate the main figures and analysis in the paper, please use the following script:
SourceCode/Analysis/Main_Figures_Generation.R




To generate the analysis related to hyperparameter changes, please use the following script:
SourceCode/Analysis/RQ1_Hyperparametes_Analysis.R

To generate the analysis related to the feature importance differences, please use the following script: 
SourceCode/Analysis/RQ2_VarriableImportance.R
 

Notes
Ensure that you have the necessary permissions to access and use the data.
The scripts assume that the working directory is the root of the repository.
Contact
For any questions or issues, please contact Dr.Mohamed Rakha at rakha@torontomu.ca.

Citation
If you use this replication package in your research, please cite the original paper as follows:

scss

<!-- Copy code -->
[Mohamed Sami Rakha, Andriy Miranskyy Member, Daniel Alencar da Costa]. (2024). [Contrasting the Hyperparameter Tuning Impact
Across Software Defect Prediction Scenarios]. [Journal/Conference]. [DOI or URL]
