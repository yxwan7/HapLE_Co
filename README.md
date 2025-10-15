# Are Older Adults in China Living Longer Happy Years?

**A Cohort-Based Multistate Analysis, 2002–2018**

---

## 📋 Overview

This repository contains the replication code for the research paper investigating whether older adults in China are experiencing longer happy years of life. Using data from the Chinese Longitudinal Healthy Longevity Survey (CLHLS) spanning 2002–2018, this study employs multistate life table methods to analyze happiness-adjusted life expectancy (HapLE) across different cohorts and sociodemographic groups.

## 🎯 Research Objectives

- Estimate happiness-adjusted life expectancy (HapLE) among Chinese older adults
- Compare cohort differences in HapLE trajectories
- Examine HapLE variations by:
  - Sex
  - Education level
  - Urban/rural residence
- Assess trends in healthy aging and subjective well-being over time

## 📁 Repository Structure

```
├── R/                          # R scripts for data processing and visualization
│   ├── 0_1_data_process_HapLE.R       # Data cleaning and preparation
│   ├── 0_1_ipw_diagnostics.R          # Inverse probability weighting diagnostics
│   ├── 1_1_table_descriptive.R        # Descriptive statistics
│   ├── 1_2_table_HapLE_sex.R          # HapLE by sex
│   ├── 1_3_table_HapLE_edu.R          # HapLE by education
│   ├── 1_4_table_HapLE_urban.R        # HapLE by urban/rural
│   ├── 2_1_plot_lexis.R               # Lexis diagram visualization
│   ├── 2_2_plot_stack_LE_sex.R        # Stacked LE plot by sex
│   ├── 2_3_plot_stack_LE_edu.R        # Stacked LE plot by education
│   └── 2_4_plot_stack_LE_urban.R      # Stacked LE plot by urban/rural
│
└── SPACE/                      # SPACE software files for multistate life table analysis
    └── 68_73/                  # Analysis for 68-73 age cohort
        ├── sex/                # Analysis by sex
        ├── sex_all/            # Overall sex-stratified analysis
        ├── sex+edu/            # Analysis by sex and education
        └── sex+urban/          # Analysis by sex and urban/rural
            ├── MSLT_RADxCOV_M.sas     # Multistate life table - Radix covariate (Male)
            ├── MSLT_RADxCOV_S.sas     # Multistate life table - Radix covariate (Sex)
            ├── MSLT_SIMxCOV_M.sas     # Multistate life table - Simulation covariate (Male)
            ├── MSLT_SIMxCOV_S.sas     # Multistate life table - Simulation covariate (Sex)
            ├── SPACE_macro.sas        # SPACE macro definitions
            ├── SPACE_MAIN.sas         # Main SPACE analysis script
            └── SPACE_Modules.sas      # SPACE module components
```

## 🔧 Requirements

### R Dependencies
```r
# Data manipulation and visualization
tidyverse
haven

# Additional packages (install as needed)
# The specific versions used in the analysis can be found in each script
```

### SAS Software
- SAS 9.4 or later
- SPACE (Stochastic Population Analysis for Complex Events) software
  - Used for multistate life table calculations

## 📊 Data Source

This study uses data from the **Chinese Longitudinal Healthy Longevity Survey (CLHLS)**:
- Survey waves: 2002, 2005, 2008, 2011, 2014, 2018
- Study population: Adults aged 65 and older
- Sample coverage: 22 provinces in China

**Note**: Due to data use agreements, the raw CLHLS data files are not included in this repository. Researchers interested in replicating the analysis should:
1. Apply for CLHLS data access at: https://sites.duke.edu/centerforaging/programs/chinese-longitudinal-healthy-longevity-survey-clhls/
2. Place the data file in a folder named `clhls data/` in the project root directory
3. Expected file: `clhls_2002_2018_longitudinal_dataset_released_version1.sav`

## 🚀 Usage

### Step 1: Data Preparation
```r
# Run the data processing script
source("R/0_1_data_process_HapLE.R")

# This will:
# - Load CLHLS data
# - Create happiness states (happy/unhappy)
# - Generate sociodemographic variables
# - Prepare longitudinal datasets
```

### Step 2: Descriptive Analysis
```r
# Generate descriptive statistics
source("R/1_1_table_descriptive.R")

# Generate HapLE tables by subgroups
source("R/1_2_table_HapLE_sex.R")
source("R/1_3_table_HapLE_edu.R")
source("R/1_4_table_HapLE_urban.R")
```

### Step 3: Multistate Life Table Analysis
Navigate to the appropriate SPACE folder and run the SAS scripts:
```sas
/* Example: Analysis by sex */
%include "SPACE/68_73/sex/SPACE_MAIN.sas";
```

### Step 4: Visualization
```r
# Create visualizations
source("R/2_1_plot_lexis.R")          # Lexis diagram
source("R/2_2_plot_stack_LE_sex.R")   # Stacked LE by sex
source("R/2_3_plot_stack_LE_edu.R")   # Stacked LE by education
source("R/2_4_plot_stack_LE_urban.R") # Stacked LE by urban/rural
```

## 📈 Key Measures

### Happiness Definition
- **Happy**: Self-reported happiness levels 1-2 (very happy/happy)
- **Unhappy**: Self-reported happiness levels 3-5 (so-so/unhappy/very unhappy)

### Life Expectancy Components
- **Total Life Expectancy (LE)**: Expected years of life remaining
- **Happy Life Expectancy (HapLE)**: Expected years lived in happy state
- **Unhappy Life Expectancy**: Expected years lived in unhappy state

### Sociodemographic Stratifications
- **Sex**: Male, Female
- **Education**: Literate, Illiterate
- **Residence**: Urban, Rural

## 📝 Analytical Methods

1. **Multistate Life Table Analysis**: Using SPACE software to estimate transition probabilities and state-specific life expectancies
2. **Inverse Probability Weighting (IPW)**: To address potential selection bias from attrition
3. **Cohort Comparison**: Comparing 2002 and 2008 cohorts to assess temporal trends
4. **Subgroup Analysis**: Stratified analyses by sex, education, and urban/rural residence

## 📄 Citation

If you use this code or findings from this research, please cite:

```
[Author(s)]. (Year). Are Older Adults in China Living Longer Happy Years? 
A Cohort-Based Multistate Analysis, 2002–2018. [Journal Name], [Volume(Issue)], [Page range].
```

## 👥 Authors

[Add author names and affiliations]

## 📧 Contact

For questions or issues related to this code, please:
- Open an issue in this repository
- Contact: [Add contact email]

## 📜 License

[Add license information]

## 🙏 Acknowledgments

This research uses data from the Chinese Longitudinal Healthy Longevity Survey (CLHLS), which is supported by funds from the U.S. National Institutes on Aging (NIA), China Natural Science Foundation, China Social Science Foundation, and UNFPA.

---

**Last Updated**: October 2025

