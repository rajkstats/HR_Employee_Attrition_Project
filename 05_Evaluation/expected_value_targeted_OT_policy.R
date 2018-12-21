# EVALUATION: EXPECTED VALUE OF POLICY CHANGE ----
# TARGETED OVERTIME POLICY ----

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


# Target by Threshold : If threshold = 30%, only employees with prob_leave > =0.30 are targeted 

# 3. Primer: Working With Threshold & Rates ----

performance_h2o <- automl_leader %>%
  h2o.performance(newdata = as.h2o(test_tbl))

performance_h2o %>%
  h2o.confusionMatrix()

# Most Critical : Don't want to miss FNs. Typically cost is higher for FNs.
# h2o.metric() : Returns the classifier performance

rates_by_threshold_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble()

# tnr & fpr are related : They total to 100%
# tpr & fnr are related : They total to 100%

rates_by_threshold_tbl %>%
  glimpse()

# ProTip: F1 is the optimal balance between Precision & Recall.
# However, the threshold @Max F1 is not typically the optimal 
# value for the business case because FNs are typically more costly than FPs. 

#ProTip: Functions like max() and min() often return multiple values
# Tack on slice(1) after filter to return only one row 

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter( f1 == max(f1)) %>%
  slice(1)  

# gather() : wide to long, which is necessary for ggplot groups
# fct_reorder2() : Used when plotting x and y axis features to control legend 
# geom_point(): Adds a layer with scatter plot style points to the plot
# geom_smooth() : Adds a smoother line (loess by default). A great way to quickly get a trend line. 
# Can change the argument to "lm" to get a straight line trend as an option

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  gather(key = key, value = value, tnr:tpr, factor_key = TRUE) %>%
  mutate(key = fct_reorder2(key, threshold, value)) %>%
  ggplot(aes(threshold, value, color = key)) +
  geom_point() + 
  geom_smooth() +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "right") +
  labs(
    title = "Expected Rates",
    y = "Value",
    x = "Threshold"
  )

# Group 1 :  tnr & fpr properties
# 1) Probability of correctly classifying a negative 
# 2) When actual is negative, we just sometimes classify it as positive
# 3) tnr +  fpr = 1


# Group 2:  tpr & fnr properties
# 1) Probability of correctly classifying a positive
# 2) When actual is positive, we just sometimes classify it as negative
# 3) tpr +  fnr = 1

# Cost Tradeoff
# In a perfect world, our model would never make mistakes & we could target TPs perfectly
# We do not live in a perfect world, FPs and FNs results in costs

# Expected Rates : Fundamentally, expected rates are the probability of getting the model
# prediction correct or incorrect at a given threshold


# 4. Expected Value ----

# 4.1 Calculating Expected Value With OT ----

source("00_Scripts/assess_attrition.R")

predictions_with_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(EmployeeNumber, MonthlyIncome, OverTime)
  )

predictions_with_OT_tbl 


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



# 4.2 Calculating Expected Value With Targeted OT ----

max_f1_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1)) %>%
  slice(1)

max_f1_tbl

tnr <- max_f1_tbl$tnr
fnr <- max_f1_tbl$fnr
fpr <- max_f1_tbl$fpr
tpr <- max_f1_tbl$tpr

threshold <- max_f1_tbl$threshold

# Anyone with Yes > = 0.26 and OverTime = Yes had their OT toggled to No 

test_targeted_OT_tbl <- test_tbl %>%
  add_column(Yes = predictions_with_OT_tbl$Yes) %>%
  mutate(
    OverTime = case_when(
      Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)) ,
      TRUE ~ OverTime
     )
  ) %>%
  select(-Yes)

test_targeted_OT_tbl

# Running h2o.predict() on the modified data will give us the probabilities
# of attrition = Yes implementing the policy change

predictions_targeted_OT_tbl <- automl_leader %>%
  h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(EmployeeNumber, MonthlyIncome, OverTime),
    test_targeted_OT_tbl %>%
      select(OverTime)
  ) %>%
  rename(
    OverTime_0 = OverTime,
    OverTime_1 = OverTime1
  )

predictions_targeted_OT_tbl

avg_overtime_pct <- 0.10

# Cost / Benefit : These are local values that we adjust by employee
# that account for TN, TP, FN & FP

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>%
  mutate(
    attrition_cost = calculate_attrition_cost(
      n = 1,
      salary = MonthlyIncome * 12,
      net_revenue_per_employee = 250000
    )
  ) %>%
  mutate(
    cost_of_policy_change = case_when(
      OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
      TRUE ~ 0
    )
  ) %>%
  mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No *  (tnr * cb_tn + fpr * cb_fp)
  )


# cb_tn = cost of policy change
# This is the cost of an employee staying 
# Controlled by the cost_of_policy_change logic

# cb_fp = cost of policy change
# This is the cost of predicting leave when employee stays
# Controlled by the cost_of_policy_change logic

# cb_tp = cost of policy change + attrition_cost 
# This is the cost of an employee leaving  
# Controlled by both the attrition_cost & cost_of_policy_change logic

# cb_fn = cost of policy change + attrition_cost 
# This is the cost of employee leaving even though we predict to stay
# Controlled by both the attrition_cost & cost_of_policy_change logic

ev_targeted_OT_tbl

total_ev_targeted_OT_tbl <- ev_targeted_OT_tbl %>%
  summarise(total_expected_attrition_cost_1 = sum(expected_attrition_cost))

total_ev_targeted_OT_tbl

# 4.3 Savings Calculation ----

# pct goes up to 15% from 13% ,  So targted OT makes more sense

savings_tbl <- bind_cols(
  total_ev_with_OT_tbl,
  total_ev_targeted_OT_tbl
) %>%
  mutate(
    savings = total_expected_attrition_cost_0 -  total_expected_attrition_cost_1,
    pct_savings = savings / total_expected_attrition_cost_0
  )

savings_tbl

# 5. Optimizing By Threshold ----



# 5.1 Create calculate_savings_by_threshold() ----

# calculate_savings_by_threshold() : A function that calculates & returns the savings when the user provides data,
# h2o model, threshold and expected rates (tnr, fpr, fnr, tpr)

# Default: The function is set up for a "No OT Policy" because the threshold is so low
# anyone with OT gets converted to No OT

data <-  test_tbl
h2o_model <- automl_leader

calculate_savings_by_threshold <- function(data, h2o_model, threshold = 0, 
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
  
  data_0_tbl <- as.tibble(data)
  
  # 4. Expected Value
  
  # 4.1 Calculating Expected Value with OT
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
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
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value with Targeted OT
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)) ,
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes)
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1
    )
  
  avg_overtime_pct <- 0.10
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = 250000
      )
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No *  (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  # 4.3 Savings Calculation
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 -  total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )

return(savings_tbl$savings)
    
}

# Threshold @ Max F1

rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1))


max_f1_savings <- calculate_savings_by_threshold(test_tbl, automl_leader, 
                               threshold = max_f1_tbl$threshold,
                               tnr = max_f1_tbl$tnr,
                               fnr = max_f1_tbl$fnr,
                               fpr = max_f1_tbl$fpr,
                               tpr = max_f1_tbl$tpr)

# No OT Policy

test_tbl %>%
  calculate_savings_by_threshold(automl_leader, threshold = 0,
                                 tnr = 0, fnr = 0, tpr = 1, fpr = 1)
 

# Do Nothing Policy

test_tbl %>%
  calculate_savings_by_threshold(automl_leader, threshold = 1,
                                 tnr = 1, fnr = 1, tpr = 0, fpr = 0)

# 5.2 Optimization ----


smpl <-  seq(1, 220, length.out = 20) %>% round(digits = 0)

# Why Sample ? 
# When we perform an iterative process, this often takes a lot of time 
# We can sample the indices to reduce the number of iterations from 220 to 20
# which reduces our iteration time by a factor of 11x 

# pmap_dbl() : One of the pmap functions that use list inputs (".|") arguments, 
# which allow for any number of inputs to be mapped
# pmap_dbl version returns a single numeric value as opposed pmap() that returns a list by default
# .l arg : Stotres a list of values that map the function arguments to the columns in data frame

# partial() : Used to partially fill a function with static arguments (I call this preloading the function ) 
# This is very useful for mapping when arguments like "data" never change during the iteration 


partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)

rates_by_threshold_optimized_tbl <- rates_by_threshold_tbl %>%
  select(threshold, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(
    savings = pmap_dbl(.l = list(
      threshold = threshold,
       tnr = tnr,
       fnr = fnr,
       fpr = fpr,
       tpr = tpr
    ),
    .f = partial(calculate_savings_by_threshold, data = test_tbl, h2o_model = automl_leader)
    )
  )

rates_by_threshold_optimized_tbl

# Thresh @ F1 Max: Default threshold for most classification algorithms 
# Not optimized for business case
# F1 treats False positives & False negatives equally. They are not
# FN's are in many cases 3X+ more costly to organization

# ProTip : Separate ggplot2 steps with comments to make layers more readable  

# annotate() : allows us to generate elements (text, labels, rectangles etc) 
# using cartesian coordinates  


rates_by_threshold_optimized_tbl %>%
  ggplot(aes(threshold, savings)) +
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  
  # Optimal Point 
 geom_point(shape = 21, size = 5, color = palette_light()[[3]],
            data = rates_by_threshold_optimized_tbl %>%
              filter(savings == max(savings))) +
  geom_label(aes(label = scales::dollar(savings)), 
             vjust = -1, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(savings == max(savings))) +
  # F1 Max
  geom_vline(xintercept = max_f1_tbl$threshold,
             color = palette_light()[[5]], size = 2) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings),
           x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -1,
           color = palette_light()[[1]]) +
  
  # No OT Policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == min(threshold))) +
  geom_label(aes(label = scales::dollar(savings)), 
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == min(threshold))) +
  
  # Do Nothing
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == max(threshold))) +
  geom_label(aes(label = scales::dollar(round(savings,0))), 
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl %>%
               filter(threshold == max(threshold))) +
  
  # Aesthestics 
  theme_tq() +
  expand_limits(x = c(-.1, 1.1), y = 8e5) + 
  scale_x_continuous(labels  = scales::percent,
                     breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Optimization Results : Expected Savings Maximized At 13.3% ",
       x = "Threshold (%)", 
       y = "Savings")

# Explaining Results

# Case 1 : "No OT Policy", Threshold = 0,  Anyone With OT Targeted
# Analysis of Test Set (15% of Total Population)
# $419k / 0.15 = $2.8M per year savings

# Case 2 : Optimization 
# Threshold @ Max Savings 
# Target employees by weighted analysis of cost of FN and cost of FP

# Case 3: Max F1 Score 
# Threshold @ Max F1
# Target employees by balancing FN's and FP's

# FN's are more costly than FPs

# FP - We predict leaves but stays
# We target and reduce OT for 10-30% of attrition cost


# FN - We predict employee stays but actually leaves 
# We fail to target and do not reduce OT for 100% of attrition cost

# FNs vs FPs
# FN = $100 k , FP = $30K
# FN/FP = 3X more costly
# When FN's = 3X FP's , lower threshold
# Lower the threshold from 28% to 13%  : 
# Catch the people that have 13% or more probability of quitting

# Expected Savings Increases 13.9% ((549 - 482) / 482 * 100)
# $67k Savings / 0.15 = $446k per year additional savings for full population

# Case 4: Do Nothing Policy
# Threshold = 1 
# Threshold is so high that no one is targeted (No one has 100% probability of leaving)

# Targeted Approach :
# Targeting employees with Yes > = 13.3% will maximize expected savings
# Big Savings : Targeting employees with Yes > = 13.3% , Increased savings by 13.9% versus F1


# 6 Sensitivity Analysis ----

# 6.1 Create calculate_savings_by_threshold_2() ----


# Modelling Assumptions: 
# You will never have perfect information and therefore assumptions must be made
# Test you assumptions: Even though you need to test some assumptions, you can still try different values
# to test your assumptions. This is why we do Sensitivity Analysis.


data <-  test_tbl
h2o_model <- automl_leader



calculate_savings_by_threshold_2 <- function(data, h2o_model, threshold = 0, 
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                           avg_overtime_pct = 0.10,
                                           net_revenue_per_employee = 250000) {
  
  data_0_tbl <- as.tibble(data)
  
# ProTip : For minor function modifications, use comments with 4 dashes to indicate 
# what changed (# explanation ----)  
  
  # 4. Expected Value
  
  # 4.1 Calculating Expected Value with OT
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        # changed in _2 -----
        net_revenue_per_employee = net_revenue_per_employee
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
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value with Targeted OT
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)) ,
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes) 
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1
    )
  
# ProTip : Traceability and readability is more important than the correct programming
  
  avg_overtime_pct <- avg_overtime_pct # changed in _2 ----
  
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        # changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee
      )
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
      )
    ) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No *  (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  # 4.3 Savings Calculation
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 -  total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )
  
  return(savings_tbl$savings)
  
}


test_tbl %>%
  calculate_savings_by_threshold_2(automl_leader, avg_overtime_pct = 0.10,
                                   net_revenue_per_employee = 250000)
# Savings are reduced
test_tbl %>%
  calculate_savings_by_threshold_2(automl_leader, avg_overtime_pct = 0.15,
                                 net_revenue_per_employee = 300000)


# 6.2 Sensitivity Analysis ----

# Classifier Calibration : This combination of threshold and expected rates settings has our classifier
# calibrated to optimum FN / FP Ratio for max savings 
# If cost / benefit change, settings need to be recalibrated (re-optimized)

max_savings_rates_tbl <- rates_by_threshold_optimized_tbl %>%
  filter(savings == max(savings))

max_savings_rates_tbl

calculate_savings_by_threshold_2(
  data = test_tbl,
  h2o_model = automl_leader,
  threshold = max_savings_rates_tbl$threshold,
  tnr = max_savings_rates_tbl$tnr,
  fnr =  max_savings_rates_tbl$fnr,
  fpr = max_savings_rates_tbl$fpr,
  tpr =  max_savings_rates_tbl$tpr
)

# Preloaded Function : Preloads the calibrated settings that optimize threshold & 
# maximize expected savings 

calculate_savings_by_threshold_2_preloaded <- partial(
  calculate_savings_by_threshold_2,
  # Function Arguments
  data = test_tbl,
  h2o_model = automl_leader,
  threshold = max_savings_rates_tbl$threshold,
  tnr = max_savings_rates_tbl$tnr,
  fnr =  max_savings_rates_tbl$fnr,
  fpr = max_savings_rates_tbl$fpr,
  tpr =  max_savings_rates_tbl$tpr
  
)


calculate_savings_by_threshold_2_preloaded(
  avg_overtime_pct = 0.10,
  net_revenue_per_employee = 250000)

# Multiple Combinations: We have two inputs that we are simultaneously going to change:
# 1. Average OverTime Percent
# 2. Net Revenue Per Employee

# Start by creating a list() of possible values for the inputs going from best to worst case

# Average OverTime : If an employee works 100% they essentially double their hours. We 
# don't expect this to be average. Rather, it's likely that the worst case is 30% 
# or roughly 30% of 40 = 12 hours per week

# Net Revenue per Employee: On an income statement, take the gross revenue minus Costs of
# Goods sold to get Net Revenue. They spread this out across every employee to get an 
# estimate of their financial value to organization.

# Worst Case : we believe $200,000  for lowest NRPE
# Best Case: we believe $400,000 for highest NRPE

# purr:cross_df() : From a list() of variables with elements, produces all the combinations of
# a list elements. Very useful for grid search and sensitivity analysis

# Multiple variables with cross_df() : we can use this with many variables as we want!
# Try 3 or 4 different sequences to see what happens

# map() or map2_dbl(): Could be used as well, but are less flexible. Can only be 
# used for 2 variable mappings

sensitivity_tbl <- list(
  avg_overtime_pct = seq(0.05, 0.30, by = 0.05), 
  net_revenue_per_employee = seq(200000, 400000, by = 50000)
) %>%
  cross_df() %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        avg_overtime_pct = avg_overtime_pct,
        net_revenue_per_employee = net_revenue_per_employee
      ),
      .f = calculate_savings_by_threshold_2_preloaded
    )
  )

sensitivity_tbl

# ProTip: Heat Maps are a great way to show how two variables interact with a 3rd (the target)

# geom_tile(): Make a rectangular grid of tiles that are useful in creating heat maps

# scale_fill_gradient2(): We can use two gradient transitions by specifying a low, mid
# high argument along with a midpoint to specify the middle transition point

sensitivity_tbl %>%
  ggplot(aes(avg_overtime_pct, net_revenue_per_employee)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
  theme_tq() +
  theme(legend.position = "none") +
  scale_fill_gradient2(
    low = palette_light()[[2]],
    mid = "white",
    high = palette_light()[[1]],
    midpoint = 0
  ) + 
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0.05, 0.30, by = 0.05)
  ) +
  scale_y_continuous(
    labels = scales::dollar
  ) +
  labs(
    title = "Profitability Heatmap : Expected Savings Sensitivity Analysis ",
    subtitle = "How sensitive is savings to net revenue per employee and average overtime percentage ?",
    x = "Average Overtime Percentage",
    y = "Net Revenue Per Employee"
  )

# Challenge : People with no stock options are leaving ----

# Part 1 : Find optimal threshold ----

avg_overtime_pct <- 0.10
net_revenue_per_employee <- 250000
stock_option_cost <- 5000

data <- test_tbl 
h2o_model <- automl_leader

calculate_savings_by_threshold_3 <- function(data, h2o_model, threshold = 0, 
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000,
                                             stock_option_cost = 5000) {
  
  data_0_tbl <- as.tibble(data)
  
  # 4. Expected Value
  
  # 4.1 Calculating Expected Value with OT
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      # Changed in _3 ---- 
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        # changed in _2 -----
        net_revenue_per_employee = net_revenue_per_employee
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
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value with Targeted OT & Stock Option Policy
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)) ,
        TRUE ~ OverTime
      )
    ) %>%
    # Changed in _3 ----
    mutate(
      StockOptionLevel = case_when(
        Yes >= threshold & StockOptionLevel == 0 
        ~ factor("1", levels = levels(data_0_tbl$StockOptionLevel)) ,
        TRUE ~ StockOptionLevel
      )
    ) %>%
    select(-Yes) 
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    # Changed in _3 ----
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel),
      data_1_tbl %>%
        select(OverTime, StockOptionLevel)
    ) %>%
    rename(
      OverTime_0 = OverTime,
      OverTime_1 = OverTime1,
      # Changed in _3 ----
      StockOptionLevel_0 = StockOptionLevel,
      StockOptionLevel_1 = StockOptionLevel1   
    )
  

  avg_overtime_pct <- avg_overtime_pct # changed in _2 ----
  stock_option_cost <- stock_option_cost # changed in _3 ----
  
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        # changed in _2 ----
        net_revenue_per_employee = net_revenue_per_employee
      )
    ) %>%
    # Changed in _3 ----
    # cost OT
    mutate(
      cost_OT = case_when(
        OverTime_0 == "Yes" & OverTime_1 == "No"
        ~  avg_overtime_pct * MonthlyIncome * 12,
        TRUE ~ 0
      )
    ) %>%
    # cost Stock Option
    mutate(
      cost_SO = case_when(
        StockOptionLevel_1 == "1" & StockOptionLevel_0 == "0"
        ~ stock_option_cost,
        TRUE ~ 0
      )
    ) %>%
    mutate(cost_of_policy_change = cost_OT + cost_SO) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_tp = cost_of_policy_change + attrition_cost,
      cb_fn = cost_of_policy_change + attrition_cost,
      expected_attrition_cost = 
        Yes * (tpr * cb_tp + fnr * cb_fn) +
        No *  (tnr * cb_tn + fpr * cb_fp)
    )
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  # 4.3 Savings Calculation
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 -  total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )
  
  return(savings_tbl$savings)
  
}


max_f1_tbl <- rates_by_threshold_tbl %>%
  select(threshold, f1, tnr:tpr) %>%
  filter(f1 == max(f1))

max_f1_savings <- calculate_savings_by_threshold_3(
  data = test_tbl,
  h2o_model = automl_leader,
  threshold = max_f1_tbl$threshold,
  tnr = max_f1_tbl$tnr,
  fnr =  max_f1_tbl$fnr,
  fpr = max_f1_tbl$fpr,
  tpr =  max_f1_tbl$tpr,
  avg_overtime_pct = 0.10,
  net_revenue_per_employee = 250000,
  stock_option_cost = 5000
)


# Optimisation
smpl <- seq(1, 220, length.out = 20) %>% round(digits = 0)
  
calculate_savings_by_threshold_3_preloaded <- partial(
  calculate_savings_by_threshold_3,
  # Function Arguments
  data = test_tbl,
  h2o_model = automl_leader,
  avg_overtime_pct = 0.10,
  net_revenue_per_employee = 250000,
  stock_option_cost = 5000)

rates_by_threshold_optimized_tbl_3 <- rates_by_threshold_tbl %>%
  select(threshold, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(
    savings = pmap_dbl(.l = list(
      threshold = threshold,
      tnr = tnr,
      fnr = fnr,
      fpr = fpr,
      tpr = tpr
    ),
    .f = calculate_savings_by_threshold_3_preloaded
    )
  )

rates_by_threshold_optimized_tbl_3

rates_by_threshold_optimized_tbl_3 %>%
  filter(savings == max(savings))
 


rates_by_threshold_optimized_tbl_3 %>%
  ggplot(aes(threshold, savings)) +
  
  #Vlines
  geom_vline(xintercept = max_f1_tbl$threshold, 
             color = palette_light()[[5]] , size = 2) +
  geom_vline(aes(xintercept = threshold), 
             color = palette_light()[[3]] , size = 2,
            data =  rates_by_threshold_optimized_tbl_3 %>%
              filter(savings ==  max(savings))
  ) +
  #Points
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  
  #F1 Max
  annotate(geom = "label", label = scales::dollar(max_f1_savings),
           x = max_f1_tbl$threshold, y = max_f1_savings, vjust = -1,
           color = palette_light()[[1]]) +
  
  # Optimal Point 
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl_3 %>%
               filter(savings == max(savings))) +
  
  geom_label(aes(label = scales::dollar(savings)), 
             vjust = -2, color = palette_light()[[3]],
             data = rates_by_threshold_optimized_tbl_3 %>%
               filter(savings == max(savings))) +

  # No OT Policy
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl_3 %>%
               filter(threshold == min(threshold))) +
  geom_label(aes(label = scales::dollar(savings)), 
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl_3 %>%
               filter(threshold == min(threshold))) +
  
  # Do Nothing
  geom_point(shape = 21, size = 5, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl_3 %>%
               filter(threshold == max(threshold))) +
  geom_label(aes(label = scales::dollar(round(savings,0))), 
             vjust = -1, color = palette_light()[[2]],
             data = rates_by_threshold_optimized_tbl_3 %>%
               filter(threshold == max(threshold))) +
  
  # Aesthestics 
  theme_tq() +
  expand_limits(x = c(-.1, 1.1), y = 12e5) + 
  scale_x_continuous(labels  = scales::percent,
                     breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Optimization Results : Expected Savings Maximized At 26.7% ",
       x = "Threshold (%)", 
       y = "Savings")



# Part 2 : Perform sensitivity analysis at optimal threshold -----

net_revenue_per_employee <- 250000
avg_overtime_pct <- seq(0.05, 0.30, by = 0.05)
stock_option_cost <- seq(5000, 25000, by = 5000)

  
max_savings_rates_tbl_3 <- rates_by_threshold_optimized_tbl_3 %>%
  filter(savings == max(savings))

max_savings_rates_tbl_3


calculate_savings_by_threshold_3_preloaded <- partial(
  calculate_savings_by_threshold_3,
  # Function Arguments
  data = test_tbl,
  h2o_model = automl_leader,
  threshold = max_savings_rates_tbl$threshold,
  tnr = max_savings_rates_tbl_3$tnr,
  fnr =  max_savings_rates_tbl_3$fnr,
  fpr = max_savings_rates_tbl_3$fpr,
  tpr =  max_savings_rates_tbl_3$tpr
  
)


calculate_savings_by_threshold_3_preloaded(
  avg_overtime_pct = 0.10,
  net_revenue_per_employee = 250000,
  stock_option_cost = 5000)  


sensitivity_tbl_3 <- list(
  avg_overtime_pct  = seq(0.05, 0.30, by = 0.05),
  net_revenue_per_employee = 250000,
  stock_option_cost = seq(5000, 25000, by = 5000)
) %>%
  cross_df() %>%
  mutate(
    savings = pmap_dbl(
      .l = list(
        avg_overtime_pct = avg_overtime_pct,
        net_revenue_per_employee = net_revenue_per_employee,
        stock_option_cost = stock_option_cost
      ),
      .f = calculate_savings_by_threshold_3_preloaded
    )
  )

sensitivity_tbl_3


sensitivity_tbl_3 %>%
  ggplot(aes(avg_overtime_pct, stock_option_cost)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
  theme_tq() +
  theme(legend.position = "none") +
  scale_fill_gradient2(
    low = palette_light()[[2]],
    mid = "white",
    high = palette_light()[[1]],
    midpoint = 0
  ) + 
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0.05, 0.30, by = 0.05)
  ) +
  scale_y_continuous(
    labels = scales::dollar,
    breaks = seq(5000, 25000, by = 5000)
  ) +
  labs(
    title = "Profitability Heatmap : Expected Savings Sensitivity Analysis ",
    subtitle = "How sensitive is savings to stock options cost and average overtime percentage ?",
    x = "Average Overtime Percentage",
    y = "Average Stock Options Cost"
  )


# As longs as average overtime percentage is less than 18% and avg stocks options cost less than
# $20,000 , we are break even that is profit.
