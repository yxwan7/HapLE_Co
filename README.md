# Are Older Adults in China Living Longer Happy Years?

**A Cohort-Based Multistate Analysis, 2002–2018**

---

## 📋 Overview

This repository contains the replication code for the paper *Are Older Adults in China Living Longer Happy Years? A Cohort-Based Multistate Analysis, 2002–2018*


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
    └── 68_73/                  # Analysis for 68-73 age range (Omit other age ranges)
        ├── sex/                # Analysis by sex
        ├── sex_all/            # Overall sex-stratified analysis
        ├── sex+edu/            # Analysis by sex and education
        └── sex+urban/          # Analysis by sex and urban/rural
            ├── MSLT_RADxCOV_M.sas
            ├── MSLT_RADxCOV_S.sas
            ├── MSLT_SIMxCOV_M.sas
            ├── MSLT_SIMxCOV_S.sas
            ├── SPACE_macro.sas
            ├── SPACE_MAIN.sas
            └── SPACE_Modules.sas
```

## 📒 Note

The SPACE program (Stochastic Population Analysis for Complex Events) is used for multistate life table analysis in this study. For more information about the SPACE program, please visit: https://sites.utexas.edu/space/

**Reference:**

Cai, L., Hayward, M. D., Saito, Y., Lubitz, J., Hagedorn, A., & Crimmins, E. (2010). Estimation of multi-state life table functions and their variability from complex survey data using the SPACE program. *Demographic Research*, 22(6), 129–158. https://doi.org/10.4054/DemRes.2010.22.6
