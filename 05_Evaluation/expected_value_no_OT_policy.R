# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# NO OVERTIME POLICY ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)


# Load Data
path_train            <- "00_Data/telco_train.xlsx"
path_test             <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl       <- read_excel(path_train, sheet = 1)
test_raw_tbl        <- read_excel(path_test, sheet = 1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)
test_readable_tbl  <- process_hr_data_readable(test_raw_tbl, definitions_raw_tbl)

# ML Preprocessing Recipe 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
    step_zv(all_predictors()) %>%
    step_num2factor(JobLevel, StockOptionLevel) %>%
    prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl  <- bake(recipe_obj, new_data = test_readable_tbl)

# 2. Models ----

h2o.init()

# Replace this with your model!!! (or rerun h2o.automl)
automl_leader <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_0_AutoML_20181202_142146")

automl_leader

#3. Expected Value ----

# 3.1 Calculating Expected Value With OT ----

source("00_Scripts/assess_attrition.R")

# MonthlyIncome : Used later as a way to cost the policy change
# OverTime : Needed to Determine whether or not the employee is working OT

predictions_with_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(EmployeeNumber, MonthlyIncome, OverTime)
  )

predictions_with_OT_tbl 

# Attrition Cost : Cost incurred when an employee leaves 
# Attrition Cost is only occurred if employee leaves. The 'Yes' column is the likelihood 
# of leaving. When combined, we can get an expected attrition cost 

# Baseline: Do nothing results in no additional costs (e.g no cost incurred from reduction in OT) 
# Expected Cost = Yes * (Total Potential Cost) + No * (Policy change cost)

# For Baseline : EV equation simplifies
# Expected Cost = Yes *  (Attrition Cost)


ev_with_OT_tbl <- predictions_with_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n = 1,
      salary = MonthlyIncome * 12,
      net_revenue_per_employee = 250000
    )
  ) %>%
  mutate(
    cost_of_policy_change = 0
  ) %>%
  mutate(
    expected_attrition_cost = 
      Yes * (attrition_cost + cost_of_policy_change) +
      No  * (cost_of_policy_change)
  )

ev_with_OT_tbl

total_ev_with_OT_tbl <- ev_with_OT_tbl %>%
  summarise(
    total_expected_attrition_cost_0 = sum(expected_attrition_cost)
  )

total_ev_with_OT_tbl

# 3.2 Calculating Expected Value Without OT ----

test_without_OT_tbl <- test_tbl %>%
  mutate(OverTime = fct_recode(OverTime, "No" = "Yes"))

test_without_OT_tbl

# Adjusting Overtime had a pretty big impact on likelihood of leaving 
# ProTip : When making changes to variables, include the initial state and new state

predictions_without_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_without_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(EmployeeNumber, MonthlyIncome, OverTime),
    test_without_OT_tbl %>%
      select(OverTime)
  ) %>%
  rename(
    OverTime_0 = OverTime,
    OverTime_1 = OverTime1
  )

predictions_without_OT_tbl

# Policy Change: The goal is to update the calculation to reflect a new policy based on your
# realistic parameters you and your organisation are potentially experiencing

# ProTip : Policy change doesn't need to be perfect at first! 
# Focus first on getting set up algo set up correctly 
# Optimization and Sensitivity analysis will get us to prediction

avg_overtime_pct <- 0.10


ev_without_OT_tbl<- predictions_without_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n = 1,
      salary = MonthlyIncome * 12,
      net_revenue_per_employee = 250000
    )
  ) %>%
  mutate(
    cost_of_policy_change = case_when(
      OverTime_0 == "Yes" & OverTime_1 == "No" ~ avg_overtime_pct * attrition_cost,
      TRUE ~ 0
    )
  ) %>%
  mutate(
    expected_attrition_cost = 
      Yes * (attrition_cost + cost_of_policy_change) +
      No  * (cost_of_policy_change)
  )

ev_without_OT_tbl

total_ev_without_OT_tbl <- ev_without_OT_tbl %>%
  summarise(
    total_expected_attrition_cost_1 = sum(expected_attrition_cost)
  )

# Cost is going down ( 3119258 to 2700079)
total_ev_without_OT_tbl

# 3.3 Savings Calculation ----

# We can potentially save 419 K for the organization

# To Annualize Multiply by 6.7
# To annualize the savings, multiply the factor of total observations to test observations 
# (train + test) / test = (1250 + 220) / 220 = 6.7 
# Annualizing : $414k X 6.7 = $2.8M 

bind_cols(
  total_ev_with_OT_tbl,
  total_ev_without_OT_tbl
) %>%
  mutate(
    savings = total_expected_attrition_cost_0 -  total_expected_attrition_cost_1,
    pct_savings = savings / total_expected_attrition_cost_0
  )
