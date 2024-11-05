# Contrasting-the-Hyperparameter-Tuning-Impact-Across-Software-Defect-Prediction-Scenarios

### Replication Package for [Research Paper Title]

## Overview

This repository contains the replication package for the research paper, _"Contrasting the Hyperparameter Tuning Impact Across Software Defect Prediction Scenarios,"_ authored by Mohamed Sami Rakha, Andriy Miranskyy, and Daniel Alencar da Costa. This package provides the necessary materials to replicate the results and experiments presented in the paper.

## Abstract

Software defect prediction (SDP) plays a crucial role in delivering high-quality software, helping teams optimize their quality assurance efforts to improve final product quality. Recent research shows that applying hyperparameter tuning can enhance prediction performance for specific SDP scenarios (e.g., predicting defects in future versions). However, the tuning impact may vary across different SDP scenarios. Comparing these impacts across scenarios is essential for improving the robustness, generalizability, and practicality of SDP modeling.

In this study, we contrast the impact of hyperparameter tuning across two critical SDP scenarios:
1. **Inner Version Defect Prediction (IVDP)**
2. **Cross Version Defect Prediction (CVDP)**

Key differences between these scenarios include the prediction scope and the evaluation setups used. Our experiments apply 28 machine learning algorithms, 53 post-release software datasets, two tuning algorithms, and five optimization metrics. Statistical analyses reveal significant differences in SDP performance impact, both overall and per algorithm, highlighting that smaller projects are more susceptible to larger performance differences. These results emphasize the importance of considering SDP scenarios when anticipating performance gains from hyperparameter tuning.

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


### Generating Figures and Analysis

To generate figures and perform analysis as described in the paper, use these scripts:

1. **Main Figures**: `SourceCode/Analysis/Main_Figures_Generation.R`
2. **Hyperparameter Analysis**: `SourceCode/Analysis/RQ1_Hyperparametes_Analysis.R`
3. **Feature Importance Analysis**: `SourceCode/Analysis/RQ2_VarriableImportance.R`

## Notes

- Ensure you have appropriate permissions to access and use the data.
- Scripts assume the working directory is the repository root.

## Contact

For questions or assistance, please reach out to Dr. Mohamed Rakha at [rakha@torontomu.ca](mailto:rakha@torontomu.ca).

## Citation

If you use this replication package in your research, please cite the original paper as follows:



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
