# ==============================================================================
# TITLE:      IPW Diagnostics for Publication
# ==============================================================================

#-----------------------------------------------------
# 1. Load Required Packages
#-----------------------------------------------------

# load packages
library(tidyverse)
library(cobalt)
library(pROC)
library(ResourceSelection)

# Note: Run 0_1_data_process_HapLE.R first to generate required objects
# source("R code/0_1_data_process_HapLE.R")

#-----------------------------------------------------
# 2. Load Pre-computed IPW Data
#-----------------------------------------------------

## Check if required objects exist in environment
required_objects <- c(
  "d_diag_data", "ps_models_list", "ipw_results_final_weight",
  "ipw_results_for_diag", "groups"
)

missing_objects <- c()
for (obj in required_objects) {
  if (!exists(obj)) {
    missing_objects <- c(missing_objects, obj)
  }
}

if (length(missing_objects) > 0) {
  stop(paste(
    "Missing required objects:", paste(missing_objects, collapse = ", "),
    "\nPlease run the main data processing script first."
  ))
}


#-----------------------------------------------------
# 3. Propensity Score Model Performance Evaluation
#-----------------------------------------------------
# Begin PS model evaluation

## Initialize model performance storage
model_performance <- data.frame(
  cohort = numeric(),
  age_group = character(),
  n_total = numeric(),
  n_lost = numeric(),
  prop_lost = numeric(),
  auc = numeric(),
  hl_pvalue = numeric(),
  model_converged = logical(),
  most_important_var = character(),
  stringsAsFactors = FALSE
)

## Evaluate each PS model
for (group_name in names(ps_models_list)) {
  model_info <- ps_models_list[[group_name]]
  ps_model <- model_info$model
  data_sub <- model_info$data

  ## Basic model statistics
  n_total <- nrow(data_sub)
  n_lost <- sum(data_sub$lost == 1)
  prop_lost <- n_lost / n_total

  ## Model convergence
  converged <- ps_model$converged

  ## Calculate AUC (C-statistic)
  auc_val <- NA
  tryCatch(
    {
      roc_obj <- roc(data_sub$lost, data_sub$ps, quiet = TRUE)
      auc_val <- as.numeric(auc(roc_obj))
    },
    error = function(e) {
      warning(paste("AUC calculation failed for", group_name, ":", e$message))
    }
  )

  ## Hosmer-Lemeshow test
  hl_pvalue <- NA
  tryCatch(
    {
      hl_test <- hoslem.test(as.numeric(data_sub$lost) - 1, data_sub$ps)
      hl_pvalue <- hl_test$p.value
    },
    error = function(e) {
      warning(paste("Hosmer-Lemeshow test failed for", group_name, ":", e$message))
    }
  )

  ## Variable importance (coefficient magnitudes)
  most_important_var <- NA
  coef_summary <- summary(ps_model)$coefficients
  if (nrow(coef_summary) > 1) {
    coef_importance <- abs(coef_summary[-1, "Estimate"])
    most_important_var <- names(coef_importance)[which.max(coef_importance)]
  }

  ## Store results
  model_performance <- rbind(model_performance, data.frame(
    cohort = model_info$cohort,
    age_group = model_info$age_group,
    n_total = n_total,
    n_lost = n_lost,
    prop_lost = prop_lost,
    auc = auc_val,
    hl_pvalue = hl_pvalue,
    model_converged = converged,
    most_important_var = most_important_var,
    stringsAsFactors = FALSE
  ))
}


#-----------------------------------------------------
# 4. Covariate Balance Assessment
#-----------------------------------------------------

## Define covariate formula
covariate_formula <- formula(lost ~ age + sex + urban + edu + A + H)

## Get data for balance assessment
diag_data_valid_ipw <- d_diag_data %>% filter(!is.na(ipw_raw))

## Overall balance check
balance_raw_ipw <- NULL
if (nrow(diag_data_valid_ipw) > 0 && n_distinct(diag_data_valid_ipw$lost) == 2) {
  ## Unweighted balance
  balance_unweighted <- bal.tab(
    covariate_formula,
    data = diag_data_valid_ipw,
    method = "matching",
    s.d.denom = "pooled",
    abs = TRUE,
    disp = c("means"),
    stats = c("mean.diffs")
  )

  ## Weighted balance (raw IPW)
  balance_raw_ipw <- bal.tab(
    covariate_formula,
    data = diag_data_valid_ipw,
    weights = "ipw_raw",
    method = "weighting",
    s.d.denom = "pooled",
    abs = TRUE,
    disp = c("means"),
    stats = c("mean.diffs")
  )
}

## Group-specific balance assessment
group_balance_summary <- data.frame(
  cohort = numeric(),
  age_group = character(),
  sample_size = numeric(),
  n_lost = numeric(),
  prop_lost = numeric(),
  max_smd_before = numeric(),
  max_smd_after = numeric(),
  balance_achieved = character(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(groups)) {
  this_cohort <- groups$cohort[i]
  this_age_group <- groups$age_group[i]

  d_group <- d_diag_data %>%
    filter(cohort == this_cohort, age_group == this_age_group, !is.na(ipw_raw))

  if (nrow(d_group) > 0 && n_distinct(d_group$lost) == 2) {
    ## Unweighted balance for this group
    balance_group_unweighted <- bal.tab(
      covariate_formula,
      data = d_group,
      method = "matching",
      s.d.denom = "pooled",
      abs = TRUE,
      disp = c("means"),
      stats = c("mean.diffs")
    )

    ## Weighted balance for this group
    balance_group_weighted <- bal.tab(
      covariate_formula,
      data = d_group,
      weights = "ipw_raw",
      method = "weighting",
      s.d.denom = "pooled",
      abs = TRUE,
      disp = c("means"),
      stats = c("mean.diffs")
    )

    ## Extract maximum SMDs
    max_smd_before <- max(abs(balance_group_unweighted$Balance$Diff.Un), na.rm = TRUE)
    max_smd_after <- max(abs(balance_group_weighted$Balance$Diff.Adj), na.rm = TRUE)

    ## Sample characteristics
    n_total <- nrow(d_group)
    n_lost <- sum(d_group$lost == 1)
    prop_lost <- n_lost / n_total

    ## Add to summary table
    group_balance_summary <- rbind(group_balance_summary, data.frame(
      cohort = this_cohort,
      age_group = this_age_group,
      sample_size = n_total,
      n_lost = n_lost,
      prop_lost = prop_lost,
      max_smd_before = max_smd_before,
      max_smd_after = max_smd_after,
      balance_achieved = ifelse(max_smd_after < 0.1, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
  }
}


#-----------------------------------------------------
# 5. Weight Distribution and Effective Sample Size
#-----------------------------------------------------

## Raw IPW weight distribution (non-lost cases only)
ipw_weight_summary <- diag_data_valid_ipw %>%
  filter(lost == 0) %>%
  summarise(
    n = n(),
    mean_ipw = mean(ipw_raw, na.rm = TRUE),
    median_ipw = median(ipw_raw, na.rm = TRUE),
    min_ipw = min(ipw_raw, na.rm = TRUE),
    max_ipw = max(ipw_raw, na.rm = TRUE),
    q95_ipw = quantile(ipw_raw, 0.95, na.rm = TRUE),
    sd_ipw = sd(ipw_raw, na.rm = TRUE),
    n_extreme = sum(ipw_raw > 10, na.rm = TRUE)
  )

## Truncated IPW data for effective sample size calculation
d_truncated_ipw <- bind_rows(ipw_results_final_weight) %>%
  left_join(d_diag_data %>% select(id, wgt), by = "id", relationship = "many-to-many") %>%
  mutate(ipw_truncated = weight / wgt) %>%
  filter(!is.na(ipw_truncated))

## Effective sample size calculation
eff_sample_size <- NULL
if (nrow(d_truncated_ipw) > 0) {
  eff_sample_size <- d_truncated_ipw %>%
    summarise(
      total_n = n(),
      sum_weights = sum(weight, na.rm = TRUE),
      sum_weights_sq = sum(weight^2, na.rm = TRUE),
      eff_sample_size = (sum_weights^2) / sum_weights_sq,
      design_effect = total_n / eff_sample_size,
      efficiency_pct = (eff_sample_size / total_n) * 100
    )
}


#-----------------------------------------------------
# 6. Generate Publication-Ready Tables
#-----------------------------------------------------

## Table 1: Overall covariate balance (before vs after IPW)
balance_summary_table <- NULL
if (!is.null(balance_raw_ipw)) {
  balance_summary_table <- data.frame(
    Variable = c("Age (years)", "Female sex", "Rural residence", "No education", "ADL dependency", "Unhappiness"),
    Unweighted_SMD = round(abs(balance_unweighted$Balance$Diff.Un), 4),
    Weighted_SMD = round(abs(balance_raw_ipw$Balance$Diff.Adj), 4),
    Balance_Achieved = ifelse(abs(balance_raw_ipw$Balance$Diff.Adj) < 0.1, "Yes", "No"),
    stringsAsFactors = FALSE
  )
}

## Table 2: PS model performance for publication
ps_performance_table <- model_performance %>%
  mutate(
    N_Lost_Pct = paste0(n_lost, " (", round(prop_lost * 100, 1), "%)"),
    AUC = round(auc, 3),
    HL_pvalue = round(hl_pvalue, 4),
    Model_Quality = case_when(
      auc >= 0.7 ~ "Good",
      auc >= 0.6 ~ "Fair",
      auc < 0.6 ~ "Poor",
      TRUE ~ "Unknown"
    ),
    Calibration = ifelse(hl_pvalue >= 0.05, "Good", "Poor")
  ) %>%
  select(cohort, age_group, n_total, N_Lost_Pct, AUC, Model_Quality, HL_pvalue, Calibration) %>%
  arrange(cohort, age_group)

## Table 3: Effective sample size summary
ess_summary_table <- NULL
if (!is.null(eff_sample_size)) {
  ess_summary_table <- data.frame(
    Metric = c("Original Sample Size", "Sum of Weights", "Effective Sample Size", "Design Effect", "Efficiency (%)"),
    Value = c(
      eff_sample_size$total_n,
      round(eff_sample_size$sum_weights, 0),
      round(eff_sample_size$eff_sample_size, 0),
      round(eff_sample_size$design_effect, 2),
      round(eff_sample_size$efficiency_pct, 1)
    ),
    stringsAsFactors = FALSE
  )
}

#-----------------------------------------------------
# 7. Export Results
#-----------------------------------------------------

## Export core diagnostic tables
write_csv(model_performance, "R output/0_1_ps_model_performance.csv")

write_csv(group_balance_summary, "R output/0_2_balance_groups.csv")

if (!is.null(balance_summary_table)) {
  write_csv(balance_summary_table, "R output/0_3_balance_overall.csv")
}

if (!is.null(ess_summary_table)) {
  write_csv(ess_summary_table, "R output/0_4_effective_sample_size.csv")
}

if (!is.null(ps_performance_table)) {
  write_csv(ps_performance_table, "R output/0_5_ps_performance_pub.csv")
}

write_csv(ipw_weight_summary, "R output/0_6_weight_distribution.csv")
