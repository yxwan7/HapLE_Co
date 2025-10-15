# ==============================================================================
# TITLE:      Data process
# ==============================================================================

#-----------------------------------------------------
# 1. Load Required Packages
#-----------------------------------------------------
# clean environment
rm(list = ls())

# load packages
library(tidyverse)
library(haven)

#-----------------------------------------------------
# 2. Load and Process 2002–2008 Data
#-----------------------------------------------------
clhls2002_2008 <- read_sav(
  "clhls data/clhls_2002_2018_longitudinal_dataset_released_version1.sav"
)

#------ Process 2002 Wave ------
d_2002 <- clhls2002_2008 %>%
  mutate(
    age = as.numeric(trueage),
    wgt = as.numeric(w_2002),
    sex = if_else(a1 == 2, 2, 1),
    wave = 1,
    psu = id,
    strata = 1
  ) %>%
  ## SES
  mutate(
    edu = case_when(f1 == 0 ~ 2, f1 > 0 & f1 < 30 ~ 1, TRUE ~ -1),
    urban = case_when(
      residenc == 3 ~ 2,
      residenc == 2 ~ 1,
      residenc == 1 ~ 1,
      TRUE ~ -1
    )
  ) %>%
  ## state
  ### Happiness (H)
  #### 3 states
  mutate(H = case_when(
    b11 >= 1 & b11 <= 2 ~ 1,
    b11 >= 3 & b11 <= 5 ~ 2,
    TRUE ~ -1
  )) %>%
  #### 4 states
  mutate(H4 = case_when(
    b11 >= 1 & b11 <= 2 ~ 1,
    b11 == 3 ~ 2,
    b11 >= 4 & b11 <= 5 ~ 3,
    TRUE ~ -1
  )) %>%
  ### ADL status (A)
  mutate(A = case_when(
    e1 == 1 & e2 == 1 & e3 == 1 & e4 == 1 & e5 == 1 & e6 == 1 ~ 1,
    e1 == 2 | e2 == 2 | e3 == 2 | e4 == 2 | e5 == 2 | e6 == 2 ~ 2,
    e1 == 3 | e2 == 3 | e3 == 3 | e4 == 3 | e5 == 3 | e6 == 3 ~ 2,
    TRUE ~ -1
  )) %>%
  ### filter
  filter(is.na(wgt) == F, H != -1, A != -1, edu != -1, urban != -1) %>%
  select(id, age, sex, wgt, psu, strata, H, H4, A, wave, edu, urban)


#------ Process 2005 Wave ------
d_2005 <- clhls2002_2008 %>%
  mutate(
    age = case_when(
      dth02_05 == 1 & d5vyear == 9999 ~ as.numeric(2005 - v_bthyr),
      dth02_05 == 1 & is.na(d5vyear) == TRUE ~ as.numeric(2005 - v_bthyr),
      dth02_05 == 1 & d5vyear != 9999 ~ as.numeric(d5vyear - v_bthyr),
      TRUE ~ as.numeric(vage_5)
    ),
    wgt = as.numeric(w_2002),
    sex = if_else(a1 == 2, 2, 1),
    wave = 2,
    psu = id,
    strata = 1
  ) %>%
  ## SES
  mutate(
    edu = case_when(f1 == 0 ~ 2, f1 > 0 & f1 < 30 ~ 1, TRUE ~ -1),
    urban = case_when(
      residenc == 3 ~ 2,
      residenc == 2 ~ 1,
      residenc == 1 ~ 1,
      TRUE ~ -1
    )
  ) %>%
  ## state
  ### Happiness (H)
  #### 3 states
  mutate(H = case_when(
    b11_5 >= 1 & b11_5 <= 2 ~ 1,
    b11_5 >= 3 & b11_5 <= 5 ~ 2,
    dth02_05 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  #### 4 states
  mutate(H4 = case_when(
    b11_5 >= 1 & b11_5 <= 2 ~ 1,
    b11_5 == 3 ~ 2,
    b11_5 >= 4 & b11_5 <= 5 ~ 3,
    dth02_05 == 1 ~ 4,
    TRUE ~ -1
  )) %>%
  ### ADL status (A)
  mutate(A = case_when(
    e1_5 == 1 & e2_5 == 1 & e3_5 == 1 & e4_5 == 1 & e5_5 == 1 & e6_5 == 1 ~ 1,
    e1_5 == 2 | e2_5 == 2 | e3_5 == 2 | e4_5 == 2 | e5_5 == 2 | e6_5 == 2 ~ 2,
    e1_5 == 3 | e2_5 == 3 | e3_5 == 3 | e4_5 == 3 | e5_5 == 3 | e6_5 == 3 ~ 2,
    dth02_05 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  ### filter
  filter(is.na(wgt) == F, H != -1, edu != -1, urban != -1) %>%
  select(id, age, sex, wgt, psu, strata, H, H4, A, wave, edu, urban)

#------ Process 2008 Wave ------
d_2008 <- clhls2002_2008 %>%
  mutate(
    age = case_when(
      dth05_08 == 1 & d8vyear == 9999 ~ as.numeric(2008 - v_bthyr),
      dth05_08 == 1 & is.na(d8vyear) == TRUE ~ as.numeric(2008 - v_bthyr),
      dth05_08 == 1 & d8vyear != 9999 ~ as.numeric(d8vyear - v_bthyr),
      TRUE ~ as.numeric(vage_8)
    ),
    wgt = as.numeric(w_2002),
    sex = if_else(a1 == 2, 2, 1),
    wave = 3,
    psu = id,
    strata = 1
  ) %>%
  ## SES
  mutate(
    edu = case_when(f1 == 0 ~ 2, f1 > 0 & f1 < 30 ~ 1, TRUE ~ -1),
    urban = case_when(
      residenc == 3 ~ 2,
      residenc == 2 ~ 1,
      residenc == 1 ~ 1,
      TRUE ~ -1
    )
  ) %>%
  ## state
  ### Happiness (H)
  #### 3 states
  mutate(H = case_when(
    b11_8 >= 1 & b11_8 <= 2 ~ 1,
    b11_8 >= 3 & b11_8 <= 5 ~ 2,
    dth05_08 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  #### 4 states
  mutate(H4 = case_when(
    b11_8 >= 1 & b11_8 <= 2 ~ 1,
    b11_8 == 3 ~ 2,
    b11_8 >= 4 & b11_8 <= 5 ~ 3,
    dth05_08 == 1 ~ 4,
    TRUE ~ -1
  )) %>%
  ### ADL status (A)
  mutate(A = case_when(
    e1_8 == 1 & e2_8 == 1 & e3_8 == 1 & e4_8 == 1 & e5_8 == 1 & e6_8 == 1 ~ 1,
    e1_8 == 2 | e2_8 == 2 | e3_8 == 2 | e4_8 == 2 | e5_8 == 2 | e6_8 == 2 ~ 2,
    e1_8 == 3 | e2_8 == 3 | e3_8 == 3 | e4_8 == 3 | e5_8 == 3 | e6_8 == 3 ~ 2,
    dth05_08 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  ### filter
  filter(is.na(wgt) == F, H != -1, edu != -1, urban != -1) %>%
  select(id, age, sex, wgt, psu, strata, H, H4, A, wave, edu, urban)

# Combine 2002, 2005, 2008 data
d0 <- d_2002 %>%
  bind_rows(d_2005) %>%
  bind_rows(d_2008)

# Add age group
d1 <- d0 %>%
  left_join(clhls2002_2008 %>%
    select(id, trueage) %>%
    mutate(
      age_group = case_when(
        trueage >= 65 & trueage <= 70 ~ "65-70",
        trueage >= 71 & trueage <= 76 ~ "71-76",
        trueage >= 77 & trueage <= 82 ~ "77-82",
        trueage >= 83 & trueage <= 88 ~ "83-88",
        trueage >= 89 & trueage <= 94 ~ "89-94",
        trueage >= 95 & trueage <= 100 ~ "95-100",
        TRUE ~ "100+"
      )
    ) %>% select(id, age_group), by = "id") %>%
  mutate(cohort = 1) %>%
  group_by(id, cohort, age_group) %>%
  filter(
    any(wave == 1),
    any(wave > 1),
    n() > 1
  ) %>%
  ungroup()

#-----------------------------------------------------
# 3. Load and Process 2011–2018 Data
#-----------------------------------------------------

clhls2011_2018 <- read_sav(
  "clhls data/clhls_2011_2018_longitudinal_dataset_released_version1.sav"
)

#------ Process 2011 Wave ------
d_2011 <- clhls2011_2018 %>%
  mutate(
    age = as.numeric(trueage),
    wgt = as.numeric(w_2011),
    sex = if_else(a1 == 2, 2, 1),
    wave = 4,
    psu = id,
    strata = 1
  ) %>%
  ## SES
  mutate(
    edu = case_when(f1 == 0 ~ 2, f1 > 0 & f1 < 30 ~ 1, TRUE ~ -1),
    urban = case_when(
      residenc == 3 ~ 2,
      residenc == 2 ~ 1,
      residenc == 1 ~ 1,
      TRUE ~ -1
    )
  ) %>%
  ## state
  ### Happiness (H)
  #### 3 states
  mutate(H = case_when(
    b11 >= 1 & b11 <= 2 ~ 1,
    b11 >= 3 & b11 <= 5 ~ 2,
    TRUE ~ -1
  )) %>%
  #### 4 states
  mutate(H4 = case_when(
    b11 >= 1 & b11 <= 2 ~ 1,
    b11 == 3 ~ 2,
    b11 >= 4 & b11 <= 5 ~ 3,
    TRUE ~ -1
  )) %>%
  ### ADL status (A)
  mutate(A = case_when(
    e1 == 1 & e2 == 1 & e3 == 1 & e4 == 1 & e5 == 1 & e6 == 1 ~ 1,
    e1 == 2 | e2 == 2 | e3 == 2 | e4 == 2 | e5 == 2 | e6 == 2 ~ 2,
    e1 == 3 | e2 == 3 | e3 == 3 | e4 == 3 | e5 == 3 | e6 == 3 ~ 2,
    TRUE ~ -1
  )) %>%
  ### filter
  filter(is.na(wgt) == F, H != -1, A != -1, edu != -1, urban != -1) %>%
  select(id, age, sex, wgt, psu, strata, H, H4, A, wave, edu, urban)

#------ Process 2014 Wave ------
d_2014 <- clhls2011_2018 %>%
  mutate(
    age = if_else(dth11_14 == 1, as.numeric(d14age), as.numeric(trueage_14)),
    wgt = as.numeric(w_2011),
    sex = if_else(a1 == 2, 2, 1),
    wave = 5,
    psu = id,
    strata = 1
  ) %>%
  ## SES
  mutate(
    edu = case_when(f1 == 0 ~ 2, f1 > 0 & f1 < 30 ~ 1, TRUE ~ -1),
    urban = case_when(
      residenc == 3 ~ 2,
      residenc == 2 ~ 1,
      residenc == 1 ~ 1,
      TRUE ~ -1
    )
  ) %>%
  ## state
  ### Happiness (H)
  #### 3 states
  mutate(H = case_when(
    b11_14 >= 1 & b11_14 <= 2 ~ 1,
    b11_14 >= 3 & b11_14 <= 5 ~ 2,
    dth11_14 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  #### 4 states
  mutate(H4 = case_when(
    b11_14 >= 1 & b11_14 <= 2 ~ 1,
    b11_14 == 3 ~ 2,
    b11_14 >= 4 & b11_14 <= 5 ~ 3,
    dth11_14 == 1 ~ 4,
    TRUE ~ -1
  )) %>%
  ### ADL status (A)
  mutate(A = case_when(
    e1_14 == 1 & e2_14 == 1 & e3_14 == 1 & e4_14 == 1 & e5_14 == 1 & e6_14 == 1 ~ 1,
    e1_14 == 2 | e2_14 == 2 | e3_14 == 2 | e4_14 == 2 | e5_14 == 2 | e6_14 == 2 ~ 2,
    e1_14 == 3 | e2_14 == 3 | e3_14 == 3 | e4_14 == 3 | e5_14 == 3 | e6_14 == 3 ~ 2,
    dth11_14 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  ### filter
  filter(is.na(wgt) == F, H != -1, edu != -1, urban != -1) %>%
  select(id, age, sex, wgt, psu, strata, H, H4, A, wave, edu, urban)

#------ Process 2018 Wave ------
d_2018 <- clhls2011_2018 %>%
  mutate(
    age = case_when(
      dth14_18 == 1 ~ as.numeric(dage),
      ### adjust age for special cases
      id == 41354412 ~ as.numeric(trueage_14 + yearin_18 - yearin_14),
      TRUE ~ as.numeric(trueage_18)
    ),
    wgt = as.numeric(w_2011),
    sex = if_else(a1 == 2, 2, 1),
    wave = 6,
    psu = id,
    strata = 1
  ) %>%
  ## SES
  mutate(
    edu = case_when(f1 == 0 ~ 2, f1 > 0 & f1 < 30 ~ 1, TRUE ~ -1),
    urban = case_when(
      residenc == 3 ~ 2,
      residenc == 2 ~ 1,
      residenc == 1 ~ 1,
      TRUE ~ -1
    )
  ) %>%
  ## state
  ### Happiness (H)
  #### 3 states
  mutate(H = case_when(
    b11_18 >= 1 & b11_18 <= 2 ~ 1,
    b11_18 >= 3 & b11_18 <= 5 ~ 2,
    dth14_18 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  #### 4 states
  mutate(H4 = case_when(
    b11_18 >= 1 & b11_18 <= 2 ~ 1,
    b11_18 == 3 ~ 2,
    b11_18 >= 4 & b11_18 <= 5 ~ 3,
    dth14_18 == 1 ~ 4,
    TRUE ~ -1
  )) %>%
  ### ADL status (A)
  mutate(A = case_when(
    e1_18 == 1 & e2_18 == 1 & e3_18 == 1 & e4_18 == 1 & e5_18 == 1 & e6_18 == 1 ~ 1,
    e1_18 == 2 | e2_18 == 2 | e3_18 == 2 | e4_18 == 2 | e5_18 == 2 | e6_18 == 2 ~ 2,
    e1_18 == 3 | e2_18 == 3 | e3_18 == 3 | e4_18 == 3 | e5_18 == 3 | e6_18 == 3 ~ 2,
    dth14_18 == 1 ~ 3,
    TRUE ~ -1
  )) %>%
  ### filter
  filter(is.na(wgt) == F, H != -1, edu != -1, urban != -1) %>%
  select(id, age, sex, wgt, psu, strata, H, H4, A, wave, edu, urban)

# merge data
d0_2 <- d_2011 %>%
  bind_rows(d_2014) %>%
  bind_rows(d_2018)

# Add age group
d1_2 <- d0_2 %>%
  left_join(clhls2011_2018 %>%
    select(id, trueage) %>%
    mutate(
      age_group = case_when(
        trueage >= 65 & trueage <= 70 ~ "65-70",
        trueage >= 71 & trueage <= 76 ~ "71-76",
        trueage >= 77 & trueage <= 82 ~ "77-82",
        trueage >= 83 & trueage <= 88 ~ "83-88",
        trueage >= 89 & trueage <= 94 ~ "89-94",
        trueage >= 95 & trueage <= 100 ~ "95-100",
        TRUE ~ "100+"
      )
    ) %>% select(id, age_group), by = "id") %>%
  mutate(cohort = 2) %>%
  group_by(id, cohort, age_group) %>%
  filter(
    any(wave == 4),
    any(wave > 4),
    n() > 1
  ) %>%
  ungroup()
#-----------------------------------------------------
# 4. Combine All Cohorts and Define Follow-Up Group
#-----------------------------------------------------
d_all <- d1 %>%
  bind_rows(d1_2)

#-----------------------------------------------------
# 5. Estimate Inverse Probability Weights (IPW)
#-----------------------------------------------------
# Extract unique IDs grouped by age_group and cohort from d_all
unique_ids <- d_all %>%
  distinct(cohort, age_group, id)


# Create full baseline cohort data with "lost" indicator
d_full_cohort1 <- d_2002 %>%
  mutate(lost = if_else(id %in% (unique_ids %>% filter(cohort == 1))$id, 0, 1)) %>%
  mutate(
    cohort = 1,
    age_group = case_when(
      age >= 65 & age <= 70 ~ "65-70",
      age >= 71 & age <= 76 ~ "71-76",
      age >= 77 & age <= 82 ~ "77-82",
      age >= 83 & age <= 88 ~ "83-88",
      age >= 89 & age <= 94 ~ "89-94",
      age >= 95 & age <= 100 ~ "95-100",
      TRUE ~ "100+"
    )
  )

# Cohort 2
d_full_cohort2 <- d_2011 %>%
  mutate(lost = if_else(id %in% (unique_ids %>% filter(cohort == 2))$id, 0, 1)) %>%
  mutate(
    cohort = 2,
    age_group = case_when(
      age >= 65 & age <= 70 ~ "65-70",
      age >= 71 & age <= 76 ~ "71-76",
      age >= 77 & age <= 82 ~ "77-82",
      age >= 83 & age <= 88 ~ "83-88",
      age >= 89 & age <= 94 ~ "89-94",
      age >= 95 & age <= 100 ~ "95-100",
      TRUE ~ "100+"
    )
  )

# Combine the data from both cohorts
d_full_all <- bind_rows(d_full_cohort1, d_full_cohort2) %>%
  filter(!is.na(age_group))

# Ensure variables for the model are factors
d_full_all <- d_full_all %>%
  mutate(
    sex = factor(sex),
    urban = factor(urban),
    edu = factor(edu),
    A = factor(A),
    H = factor(H),
    lost = factor(lost)
  )

# Modify loop to store diagnostic information
ipw_results_final_weight <- list()
ipw_results_for_diag <- list()
ps_models_list <- list() # Store PS models for diagnostics

# Get all unique combinations of cohort and age group
groups <- d_full_all %>%
  distinct(cohort, age_group)

# Loop through each cohort × age_group combination
for (i in 1:nrow(groups)) {
  this_cohort <- groups$cohort[i]
  this_age_group <- groups$age_group[i]

  # Subset data for the current group
  d_sub <- d_full_all %>%
    filter(cohort == this_cohort, age_group == this_age_group)

  if (nrow(d_sub) >= 30 && n_distinct(d_sub$lost) == 2 && sum(d_sub$lost == 1) > 5 && sum(d_sub$lost == 0) > 5) {
    ps_model <- NULL
    ps_pred <- NA_real_

    tryCatch(
      {
        # Fit logistic regression to predict probability of loss to follow-up
        ps_model <- glm(
          lost ~ age + sex + urban + edu + A + H,
          data = d_sub,
          family = binomial(link = "logit")
        )
        # Predict propensity score
        ps_pred <- predict(ps_model, newdata = d_sub, type = "response")
      },
      error = function(e) {
        warning(paste(
          "PS model failed for cohort", this_cohort,
          "age_group", this_age_group, ":", e$message
        ))
      }
    )

    if (!is.null(ps_model) && !any(is.na(ps_pred))) {
      d_sub$ps <- ps_pred
      d_sub$ps_clipped <- pmin(pmax(d_sub$ps, 1e-6), 1 - 1e-6)

      d_sub <- d_sub %>%
        mutate(ipw_raw = case_when(
          lost == 0 ~ 1 / (1 - ps_clipped),
          lost == 1 ~ 1 / ps_clipped,
          TRUE ~ NA_real_
        ))

      # Store model for diagnostics
      group_key <- paste0("cohort", this_cohort, "_", this_age_group)
      ps_models_list[[group_key]] <- list(
        model = ps_model,
        data = d_sub,
        cohort = this_cohort,
        age_group = this_age_group
      )

      ipw_results_for_diag[[group_key]] <- d_sub

      d_sub_not_lost <- d_sub %>% filter(lost == 0)

      if (nrow(d_sub_not_lost) > 0) {
        upper_cutoff <- quantile(d_sub_not_lost$ipw_raw, 0.95, na.rm = TRUE)
        lower_cutoff <- quantile(d_sub_not_lost$ipw_raw, 0.05, na.rm = TRUE)

        d_sub_not_lost <- d_sub_not_lost %>%
          mutate(ipw = pmin(pmax(ipw_raw, lower_cutoff), upper_cutoff))

        d_sub_not_lost <- d_sub_not_lost %>%
          mutate(weight = ipw * wgt)

        ipw_results_final_weight[[paste0("cohort", this_cohort, "_", this_age_group)]] <- d_sub_not_lost %>%
          select(id, age_group, cohort, weight)
      }
    } else {
      warning(paste("Skipping weight calculation due to PS model failure for cohort", this_cohort, "age_group", this_age_group))
      ipw_results_for_diag[[paste0("cohort", this_cohort, "_", this_age_group)]] <- d_sub
    }
  } else {
    warning(paste("Skipping PS model due to insufficient data/variation for cohort", this_cohort, "age_group", this_age_group))
    ipw_results_for_diag[[paste0("cohort", this_cohort, "_", this_age_group)]] <- d_sub
  }
}

# Combine data for diagnostics
d_diag_data <- bind_rows(ipw_results_for_diag)

# Combine all group results into one dataframe
d_ipw <- bind_rows(ipw_results_final_weight)

#-----------------------------------------------------
# 6. Export Final Weighted Data by Age Group
#-----------------------------------------------------
d_final <- d_all %>%
  left_join(d_ipw, by = c("id", "cohort", "age_group"))

d_68_73 <- d_final %>%
  filter(age_group == "65-70")

d_74_79 <- d_final %>%
  filter(age_group == "71-76")

d_80_85 <- d_final %>%
  filter(age_group == "77-82")

d_86_91 <- d_final %>%
  filter(age_group == "83-88")

d_92_97 <- d_final %>%
  filter(age_group == "89-94")

d_98_103 <- d_final %>%
  filter(age_group == "95-100")

# Write csv
write_csv(d_68_73, "space input/HapLE_68_73.csv")
write_csv(d_74_79, "space input/HapLE_74_79.csv")
write_csv(d_80_85, "space input/HapLE_80_85.csv")
write_csv(d_86_91, "space input/HapLE_86_91.csv")
write_csv(d_92_97, "space input/HapLE_92_97.csv")
write_csv(d_98_103, "space input/HapLE_98_103.csv")
write_csv(d_final, "space input/HapLE_all.csv")
