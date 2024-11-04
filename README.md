# Contrasting-the-Hyperparameter-Tuning-Impact-Across-Software-Defect-Prediction-Scenarios
Replication Package for [Research Paper Title]
Overview
This repository contains the replication package for the research paper titled "Contrasting the Hyperparameter Tuning Impact
Across Software Defect Prediction Scenarios" by Mohamed Sami Rakha, Andriy Miranskyy Member, and Daniel Alencar da Costa. The purpose of this package is to allow others to replicate the results and experiments described in the paper.


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
