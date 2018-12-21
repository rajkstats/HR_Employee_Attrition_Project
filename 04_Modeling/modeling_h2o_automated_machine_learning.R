# H2O MODELING -----

# Load Libraries
library(h2o)
library(glue)
library(cowplot)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(fs)

# 1. Setup ----


#Load Data
path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet=1)
test_raw_tbl <- read_excel(path_test, sheet=1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Processing Pipeline
source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_tbl = definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_tbl = definitions_raw_tbl)

# H2O AutoMl Algorithm
# Low Maintenance Algorithm
# Handles factor and numeric data (Data needs to be in readable format)
# Performs preprocessing internally
# Not Necessary: Transformations

# ML Preprocessing 

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(JobLevel, StockOptionLevel) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)


# 2. Modeling ----

h2o.init()


split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)

train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)

# outcome /response /target
y <- "Attrition"
x <- setdiff(names(train_h2o),y)

# Key Concepts
# Training Frame : Used to develop model
# Validation Frame : Used to tune hyperparameters via grid search
# Leaderboard Frame: Test set completely held out from model training & tuning
# Use max_run_time_secs to minimize modeling time initially. Once results look promising
# increase the run time to get more models with highly tuned parameters

automl_models_h2o <- h2o.automl(
  x = x,
  y = y,
  training_frame =   train_h2o,
  validation_frame = valid_h2o,
  leaderboard_frame = test_h2o,
  max_runtime_secs = 30,
  nfolds = 5
)

typeof(automl_models_h2o) # S4 class

# slotNames() : Returns the names of slots in an S4 class object
slotNames(automl_models_h2o)

# Info: S4 objects use the @ symbol to select slots. Slots are like entries in a list
# Leaderboard Frame: leaderboard is summary of models produced by H2o AutoML
# TRUE Performance measure: leaderboard performance (AUC, logloss) is representative of unseen data
automl_models_h2o@leaderboard

# Training Results: These are the results during the modeling process, which are not representative of new data
# Validation Results: These are the results during the tuning process, which are not representative of new data  
automl_models_h2o@leader

# IMPORTANT : None of these performance outputs are on hold out data. The leaderboard is what you need to use to
# compare models

# h2o.getModel() : Connects to a model given a model id

h2o.getModel('DeepLearning_0_AutoML_20181202_142146')

# slice():  select row or rows by position
automl_models_h2o@leaderboard %>%
  as.tibble() %>%
  slice(1) %>%
  pull(model_id) %>%
  h2o.getModel()

# message() : Generates the message that is printed to screen while function runs
# verbose : Many functions include a "verbose" argument that can toggle on/off display of
# informative information. This can be a good practice in function development if a user may need
# information about the function

extract_h2o_model_name_by_position <- function(h2o_leaderboard, n = 1, verbose = TRUE){
  
 model_name <-  h2o_leaderboard %>%
   as.tibble() %>%
   slice(1) %>%
   pull(model_id)
 
 if(verbose) message(model_name)
 
 return(model_name)
 
}
  
automl_models_h2o@leaderboard %>%
  extract_h2o_model_name_by_position(2) %>%
  h2o.getModel()


# Saving and Loading H2o Models

h2o.getModel("GLM_grid_0_AutoML_20181202_142146_model_0") %>%
  h2o.saveModel(path = "04_Modeling/h20_models/")

h2o.getModel("StackedEnsemble_BestOfFamily_0_AutoML_20181202_142146") %>%
  h2o.saveModel(path = "04_Modeling/h20_models/")

h2o.getModel("DeepLearning_0_AutoML_20181202_142146") %>%
  h2o.saveModel(path = "04_Modeling/h20_models/")

deeplearning_h2o <- h2o.loadModel("04_Modeling/h20_models/DeepLearning_0_AutoML_20181202_142146")

# Making Predictions

stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_0_AutoML_20181202_142146")
stacked_ensemble_h2o

# Binary Classifications
# H2O predictions are 3 columns :
# 1 - Class Prediction
# 2 - 1st Class probability
# 3 - 2nd Class probability

predictions <- h2o.predict(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(predictions) # environment vbl

# Saving it as a tibble
predictions_tbl <- predictions %>% as.tibble()


deeplearning_h2o

# ?h2o.deeplearning()
# Printing parameters used to create model
deeplearning_h2o@allparameters

# Cross Validation 
# What is Cross Validation
# Why use cross validation ?
# How K fold Cross validation works in H2o ?
# K-fold cross validation helps in model development
# Models have parameters that affect prediction performance
# Instead of one tests, multiple tests
# End results is Parameter selections based on model performance stability
# Selected Model Generalizes better
# Lets say k =5 folds 
# Same modelling approach applied to all 5 folds 
# Parameters are same, models may not be same 
# AUC is then generated for all 5 folds for model effectiveness
# After the process is completed with all sets of cross validation sets , then auc is avergaed
# This process can again be performed with new set of modelling parameters with concept called Grid Search (Iterative Hyperparameter Tuning)

# For every model you can get cross validation except stack ensembles
h2o.cross_validation_models(deeplearning_h2o)

# h2o.auc() - Rereives area under curve (AUC) for a classifier. If passing an h2O Model, can use the xval argument
# to retreive the average cross validation AUC.

h2o.auc(deeplearning_h2o, train = T, valid = T, xval = T)

# Grid Search : Creating a "grid" or matrix of numerous combination of parameters. Then run the model with all combination
# to see which returns the best results

# Hyperparameters : " A fancy name for parameters"
# Hyperparameter space : A grid of parameter value combinations

# 3. Visualizing the Leaderboard  ----

# AUC : It's a way of measuring the peroformance of a binary classifier by comparing the 
# False Positive Rate (FPR x-axis) to True positive rate (TPR y-axis)

# LogLoss : Logarithmic loss. Measures the peroformance of a classifier by comparing the
# class probability to actual value (1 or 0)

# H2O Leaderboard Metrics Visualization

# rownames_to_column() : Add rownames of a dataframe to a column

#ProTip: To take advantage of ggplot2 color and group aesthetics, we need data in long format
# Long format has each faceting values stacked on top of each other, making the data frame long 
# rather than wide

data_transformed <- automl_models_h2o@leaderboard %>%
  as.tibble() %>% 
  select(model_id,auc,logloss) %>%
  mutate(model_type = str_split(model_id, '_', simplify = TRUE)[,1]) %>%
  slice(1:10) %>%
  rownames_to_column() %>%
  mutate(
    model_id = as_factor(model_id) %>% reorder(auc),
    model_type = as_factor(model_type)
  ) %>%
  gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>%
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())

# ProTip: Remember for facets to work properly data must be in long format

data_transformed %>%
  ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label  = round(value, 2), hjust = "inward")) +
  facet_wrap(~ key, scale = "free_x") +
  theme_tq() +
  scale_color_tq() +
  labs(title = "H2O leaderboard Metrics",
       subtitle = paste0("Ordered by : auc"),
       y = "Model Position, Model ID" , x = "")
      
h2o_leaderboard <- automl_models_h2o@leaderboard 
 
plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"),
                                 n_max = 20, size = 4, include_lbl = TRUE) {
  
  #Setup input 
  order_by <- tolower(order_by[[1]])
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    select(model_id,auc,logloss) %>%
    mutate(model_type = str_split(model_id, '_', simplify = TRUE)[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ",as.character(model_id)) %>% as_factor())
  
  
  # Transformation 
  if(order_by == "auc" ) {
    
    data_transformed_tbl <- leaderboard_tbl %>%
    slice(1:n_max) %>%
      mutate(
        model_id = as_factor(model_id) %>% reorder(auc),
        model_type = as_factor(model_type)
      ) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname), factor_key = T) 
  } 
  else if(order_by == "logloss" ){
    data_transformed_tbl <- leaderboard_tbl %>%
      slice(1:n_max) %>%
      mutate(
        model_id = as_factor(model_id) %>% reorder(logloss),
        model_type = as_factor(model_type)
      ) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname), factor_key = T)
       } else{
    stop(paste0(" order_by = '", order_by,"' is not a permitted option. "))
       }

  g <- data_transformed_tbl %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = 3) +
    facet_wrap(~ key, scale = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(title = "H2O leaderboard Metrics",
         subtitle = paste0("Ordered by : auc"),
         y = "Model Position, Model ID" , x = "")
  
  if(include_lbl) g <- g +  geom_label(aes(label  = round(value, 2), hjust = "inward")) 

return(g)
    
}  

automl_models_h2o@leaderboard %>%
  plot_h2o_leaderboard(order_by = "logloss")

automl_models_h2o@leaderboard %>%
  plot_h2o_leaderboard(order_by = "auc")
  
# 4. Assessing Performance ----
stacked_ensemble_h2o <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_0_AutoML_20181202_142146")
deeplearning_h2o <- h2o.loadModel("04_Modeling/h20_models/DeepLearning_0_AutoML_20181202_142146")
glm_h2o <- h2o.loadModel("04_Modeling/h20_models/GLM_grid_0_AutoML_20181202_142146_model_0")  


performance_h2o <- h2o.performance(stacked_ensemble_h2o, newdata = as.h2o(test_tbl))

typeof(performance_h2o) # has slots S4 type

performance_h2o %>% slotNames()

performance_h2o@metrics

# Classifier Summary metrics 
# AUC: Stands for Area under Curve, referring to ROC (Receiver Operating Characterstics plot). 
# This measures true positive rate (TPR) vs false positive rate (FPR)

# AUC is one of the main performance metrics that data scientists use to select a model. However,
# it's not always the best. Logloss is also a good metric. 

# Caution : train, val, xval arguments only work for models (not performance objects)

h2o.auc(stacked_ensemble_h2o, train = T, valid = T, xval = T)

h2o.auc(performance_h2o)

# Gini coefficient :  AUC = (Ginicoeff + 1) /2 
# Rarely used in comparing the models

h2o.giniCoef(performance_h2o)

# logloss: a metric that measures the class probability from the model 
# against the actual value in binary format (0,1)
h2o.logloss(performance_h2o) # 0.29

# Logloss compares the prediction class prob to 1/0 actual value computing the mean error
# This is a great way to measure the true performance of a classifier


# Confusion Matrix 
# ProTip : Learn to read a confusion matrix. Focus on understanding the threshold, Precision & Recall
# These are critical to business analysis
# Top Row : Prediction(P) 
# Left column : Actual (A)

# threshold : Value that determines which class probability is 0 or 1 (i.e employee stays or leaves) 

h2o.confusionMatrix(stacked_ensemble_h2o)

h2o.confusionMatrix(performance_h2o)

# Quadrants contains: 
# True Negative (P=N, A=N)
# False Positive (P=Y, A=N)
# False  Negative (P=N, A=Y)
# True Positive (P=Y, A=Y)

# h2o.metric() : Converts a performance obj to series of metrics (e.g precision , recall, f1 etc) 
# that vary by threshold

# Precision vs Recall Graph ----

# Following is on test set
performance_tbl <- performance_h2o %>%
  h2o.metric() %>%
  as.tibble()


# Important measures that vary by threshold:

# F1: Optimal balance between precision and recall. Typically the threshold that maximises F1
# is used as threshold / cutoff for turning class probability into 0/1. 
# However this is not always the best case, An expected value optimization is required when 
# costs of false positives and false negatives are known

# Precision : Measures false positives (e.g predicted to leave but actually stayed )
# Recall : Measures false negatives (e.g predicted to stay but actually left)

# true positives (tps), true negatives (tns), false positives (fps), and false negatives (fns) 
# Often converted into rates to understand cost  / benefit of classifier
# Rates are included as tpr, tnr, fpr and fnr

# Precision = TP / (TP +FP)  # 25 /(25+16) = 0.6097
# In other words, it detects how frequently your algorithm over-picks the Yes class

# Precision in business context : Precision indicates how often we incorrectly say people will leave
# when they actually will stay

# Recall =  TP / (FN +FP) # 25 /(25+11) = 0.69444
# In other words, it provides a metric for under-picking the Yes class

# ProTip : Recall is typically more important than Precision in the business context. We would rather give
# up some false positives (inadeverently target stayers) to gain false negatives (accurately predict quiters)

# Recall in Business context : Recall indicates how often we miss people that will leave by 
# incorrectly predicting they will stay

# F1 = 2 *(precision * recall) / (precision + recall) #  2 * (0.60 * 0.69) / (0.60+0.69) = 0.64
# In other words, it provides a metric for balancing  precision vs recall

# Max F1 Thresholds : Threshold that optimizes the balance between precision and recall

performance_tbl %>%
  filter(f1 == max(f1))

# While F1 optimizes between precision and recall, it's not always the best choice for threshold. why ?

# Because there are different costs associated with false negatives and false positives.
# Typically false negatives costs the company more. This is where Expected Value (EV) comes into play.

# h2o.find_threshold_by_max_metric() : User supplies a metric to maximize (e.g f1) and the function
# returns the threshold that maximizes the metric

performance_tbl %>%
  ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) + 
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(performance_h2o, "f1")) +
  theme_tq() +
  labs(title = 'Precision vs Recall', y = "value")
  
# ROC Plot ----

path <- "04_Modeling/h20_models/DeepLearning_0_AutoML_20181202_142146"

load_model_performance_metrics <- function(path, test_tbl){
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))

  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc)
  
} 

load_model_performance_metrics(path,test_tbl)

# fs + purr

model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h20_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl

# ROC Curve : pits the True Positive Rate (TPR, y-axis) against the False positive Rates (FPR, x-axis)
# TPR : Rate at which people are correctly identified as leaving
# FPR : Rate at which people that stay are incorrectly identified as leaving 

# ProTip: Converting Numeric to Factor requires an intermdediate step of converting to character

model_metrics_tbl %>%
  mutate( 
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc = auc %>% round(3) %>% as.character() %>% as_factor()
    ) %>%
  ggplot(aes(fpr, tpr, color = path, linetype =auc)) +
  geom_line(size = 1) +
  theme_tq() + 
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(title = "ROC Plot" , subtitle = "Performance of 3 Top Performing Models")


# Perfect model at (0,1)
# Average Model with no predictive power line at 45 degree
# For a classifier, a perfect model has an AUC of 1 and 
# average model with no predictive power has an AUC of 0.5


# Precision vs Recall

# Turn precision / recall tradeoff into performance 

load_model_performance_metrics <- function(path, test_tbl){
  
  model_h2o <- h2o.loadModel(path)
  perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
  
  perf_h2o %>%
    h2o.metric() %>%
    as.tibble() %>%
    mutate(auc = h2o.auc(perf_h2o)) %>%
    select(tpr, fpr, auc, precision, recall)
} 


model_metrics_tbl <- fs::dir_info(path = "04_Modeling/h20_models/") %>%
  select(path) %>%
  mutate(metrics = map(path, load_model_performance_metrics, test_tbl)) %>%
  unnest()

model_metrics_tbl


model_metrics_tbl %>%
  mutate( 
    path = str_split(path, pattern = "/", simplify = T)[,3] %>% as_factor(),
    auc = auc %>% round(3) %>% as.character() %>% as_factor()
  ) %>%
  ggplot(aes(recall, precision, color = path, linetype =auc)) +
  geom_line(size = 1) +
  theme_tq() + 
  scale_color_tq() +
  theme(legend.direction = "vertical") +
  labs(title = "Precision vs Recall Plot" , subtitle = "Performance of 3 Top Performing Models")

# Models have better balance between FP's and FN's
# Models have a less precison for given recall

# Business Application : 

# FN are what we typically most  care about. Recall indicates that susceptibility to FN's  
# (lower recall, more susceptible)

# In other words, we want to accurately predict which employees will leave (lower FN's) at the expense
# of over predicting employees to leave (FP's) 

# Precision vs recall curve shows us which models will give up less FP's 
# as we optimize the thresholds for FN's


# Gain & Lift

# Emphasizes How much model improve results

ranked_predictions_tbl <- predictions_tbl %>%
  bind_cols(test_tbl) %>%
  select(predict:Yes, Attrition) %>%
  arrange(desc(Yes))

# Gain / Lift  : In first 10, we have 90% accuracy  (9 of 10 people)
# Without model, we'd only expect the global attrition rate for first 10 (16% or 1.6 people)
# If total expected quiters is 220 x 0.16 = 35
# Gain : If 35 people expected to quit, we gained 9 of 35 or 25.7% in first 10 cases
# Lift : If the expectation is 1.6 people, we beat the expectation by 9/1.6 = 5.6X in first 10 cases

# Executive Communication : Gain & Lift can help communicate modeling results in terms everyone
# understands and cares about..RESULTS !

# Class Probability Response : Yes is the class probability for churn, or how likely the employee is to churn
# Attrition is the Response, or what actually happened

# Ranked Predictions : By ranking by class probability of Yes, we assess the models ability to truly detect 
# someone that is leaving

# Grouping By Likelihood : Grouping into cohorts of most likely to least likely groups is as the heart of
# Gain/Lift chart. When we do this, we can immediately show that if candidate has high prob of leaving, 
# how likely they are to leave

# ntile: Thinking of grouping by ntile as splitting up a continuious variable into n buckets
# This allows us to group the group the Response (Attrition) based on ntile column

# Cumulative Gain: Technically this is cumulative gain, because we cumulatively sum the pct_responses 

calculated_gain_lift_tbl <- ranked_predictions_tbl %>%
  mutate(ntile = ntile(Yes, n = 10)) %>%
  group_by(ntile) %>%
  summarise(
    cases = n(),
    responses = sum(Attrition == "Yes")
  ) %>%
  arrange(desc(ntile)) %>%
  mutate(group = row_number()) %>%
  select(group, cases, responses) %>%
  mutate(
    cumulative_responses = cumsum(responses),
    pct_responses        =  responses/sum(responses),
    gain                 =  cumsum(pct_responses),
    cumulative_pct_cases =  cumsum(cases)/sum(cases),
    lift                 =  gain/cumulative_pct_cases,
    gain_baseline        =  cumulative_pct_cases,
    lift_baseline        =  gain_baseline / cumulative_pct_cases
  )

# 10th Decile: This group has the highest class probabilities for leaving
# 18 of 22 actually left

# Gain : Think of this as what we gained using the model
# For example, if we focused on first 2 groups , we gain ability to target 69.4% of our quiters using our model

# Lift : Think of this as multiplier between what we gained divided by what we expected to gain with no model
# For example, if we focused on the first two decile groups, we gained ability to target 69.4% of our quiters 
# but we only expected to be able to target 20% of the quiters in 2 decile groups

# lift2 = 69.4% /20% = 3.47x
# Meaning model is 3.47x better targeting than random


# ProTip : Cumulative gain baseline is always equal to cumulative ntile percentage
# ProTip : Cumulative lift baseline is always equal to 1


 calculated_gain_lift_tbl

gain_lift_tbl <- performance_h2o %>%
   h2o.gainsLift() %>%
   as.tibble()

# Group: H2o groups into 16 ntiles with the first being the most likely to be in the minority (Yes) class
# 16 N-tiles 
# 220/ 16 n-tiles = 13.75 people / group
# 16 n-tiles gives great resolution with about 14 people in each group

gain_lift_tbl %>%
  glimpse()

# Cumulative Capture Rate : This is "cumulative percentage gain" that we will use
# Cumulative Lift : This is the "cumulative lift" that we will use

# Formatting Data for ggplot() and gather() : If we want to take advantage of ggplot's color and group aesthetics
# or facet_wrap() function, we need to have data stacked on top of each other by grouping variables
# gather function enables this


gain_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("lift")) %>%
  mutate(baseline = cumulative_data_fraction) %>%
  rename(gain = cumulative_capture_rate) %>%
  gather(key = key, value = value, gain, baseline)


gain_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() + 
  labs(
    title = "Gain Chart",
    x =  "Cumulative Data Fraction",
    y =  "Gain"
  )

# Let's use a marketing example
# Goal : Send marketing communications to have customers sign up 
# Is targeting everyone the right strategy ? 

# Randomly targeting 25% of potential customers should yield 25% of potential positive responses
# Strategically targeting 25% of high probability customers should yield  69% of potential positive responses

# Always explain in terms of what they are interested in : 
# More customers
# Less churn
# Better quality 
# Reduced lead times
# Anything they care about



lift_transformed_tbl <- gain_lift_tbl %>%
  select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift) %>%
  select(-contains("capture")) %>%
  mutate(baseline = 1) %>%
  rename(lift = cumulative_lift) %>%
  gather(key = key, value = value, lift, baseline)


lift_transformed_tbl %>%
  ggplot(aes(x = cumulative_data_fraction, y = value, color = key)) +
  geom_line(size = 1.5) +
  theme_tq() +
  scale_color_tq() + 
  labs(
    title = "Lift Chart",
    x =  "Cumulative Data Fraction",
    y =  "Lift"
  )


# Gained From : 25% (Baseline)
#          To : 69% (Model)  

# Gain & Lift Go hand in hand : the two charts work together to show the results of using the modeling approach
# versus just targeting people at random

# Lifted From : 1x (Baseline)
#          To : 3x (Model)  

# Lift is a Multiplier 
# How many positive responses would you get at random X lift = How many you would get with the model

# how does lift apply to Attrition ? 
# Example: Providing stock options strategically to high performers that are igh risk.
# Could strategically focus on those in the high risk category that are working time

# Stock options Assignment would be 3x more effective with the model


# Example : Communicating to Executive
# If 35 people are expected to quit, we can obtain 69% by targeting the top 25% of high risk people 
# This reduces costs by 1/3rd versus random selection because we only need to offer stock options to
# high risk candidates


# 5. Performace Visualization ----

# Model Metrics Dashboard : Evaluate multiple models to see strengths and weakness

h2o_leaderboard <- automl_models_h2o@leaderboard
newdata <- test_tbl
order_by <- "auc"
max_models <- 4 
size <- 1

plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc","logloss"), 
                                 max_models = 3, size = 1.5){
  
  #Input 
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as.tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as.tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)
  
  h2o.no_progress() # turns off progress bars while we step down function
  
  # 1. Model Metrics
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as.tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
 } 
  
#ProTip: mutate() + map() is a powerful way of enabling rowwise iteration in a tidy way. 
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest() %>%
    mutate(
      model_id = as_factor(model_id) %>%
        fct_reorder(!! order_by_expr, .desc =  ifelse(order_by == "auc", TRUE, FALSE
        )),
      auc = auc %>%
        round(3) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>%
        round(4) %>%
        as.character() %>%
        as_factor() %>%
        fct_reorder(as.numeric(model_id))
   )
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    theme_tq() + 
    scale_color_tq() +
    labs(title = "ROC" , x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical") 
    
    # 1B. Precision vs Recall
    
    p2 <- model_metrics_tbl %>%
      ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
      geom_line(size = size) +
      theme_tq() + 
      scale_color_tq() +
      labs(title = "Precision vs Recall" , x = "Recall", y = "Precision") +
      theme(legend.position = "none") 
    
    # 2. Gain / Lift
     
      get_gain_lift <- function(model_id, test_tbl) {
        
        model_h2o <- h2o.getModel(model_id)
        perf_h2o <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
        
        perf_h2o %>%
          h2o.gainsLift() %>%
          as.tibble() %>%
          select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
      }  
    
    gain_lift_tbl <- leaderboard_tbl %>%
      mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
      unnest() %>%
      mutate(
        model_id = as_factor(model_id) %>%
          fct_reorder(!! order_by_expr, .desc =  ifelse(order_by == "auc", TRUE, FALSE)),
        auc = auc %>%
          round(3) %>%
          as.character() %>%
          as_factor() %>%
          fct_reorder(as.numeric(model_id)),
        logloss = logloss %>%
          round(4) %>%
          as.character() %>%
          as_factor() %>%
          fct_reorder(as.numeric(model_id))
      ) %>%
      rename(
        gain = cumulative_capture_rate,
        lift = cumulative_lift
      )
    
    # 2A. Gain Plot
    #geom_segment() : draws a line b/w 2 points
    #expand_limits(): Used to expand the plot window which is useful if elements are getting cut-off 
    
    
    
    p3 <- gain_lift_tbl %>%
      ggplot(aes_string(x = "cumulative_data_fraction", "gain", 
                        color = "model_id", linetype = order_by)) +
      geom_line(size = size) +
      geom_segment(x = 0, y = 0, xend = 1,  yend = 1, 
                   color = "black", size  = size )+
      theme_tq() +
      scale_color_tq() +
      expand_limits(x = c(0,1), y = c(0,1)) +
      labs(title = "Gain Chart",
        x =  "Cumulative Data Fraction", y =  "Gain"
      ) +
      theme(legend.position = "none") 
      
  
    # 2B. Lift Plot
    
    
    p4 <- gain_lift_tbl %>%
      ggplot(aes_string(x = "cumulative_data_fraction", "lift", color = "model_id", linetype = order_by)) +
      geom_line(size = size) +
      geom_segment(x = 0, y = 1, xend = 1,  yend = 1, 
                   color = "black", size  = size )+
      theme_tq() +
      scale_color_tq() + 
      expand_limits(x = c(0,1), y = c(0,1) ) +
      labs(
        title = "Lift Chart",
        x =  "Cumulative Data Fraction",
        y =  "Lift"
      ) +
      theme(legend.position = "none") 
    
    # Combine using cowplot
    p_legend <- get_legend(p1)
    p1 <- p1 + theme(legend.position = "none") 
    
    p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2)
    
    #cowplot::ggdraw : Sets up a drawing layer
    #cowplot::draw_label : Draw a text or mathematical expression on ggdraw layer or on a ggplot object
    
    p_title <- ggdraw() +
      draw_label("H2O Model Metrics ", size = 18, fontface = "bold", 
                 colour =  palette_light()[[1]])
    
    # glue() : A better method of pasting strings (than the paste0 function) Use braces to intermix
    #code with text 
    p_subtitle <- ggdraw() +
      draw_label(glue("Ordered by  {toupper(order_by)} "), size = 10, fontface = "bold", 
                 colour =  palette_light()[[1]])    
  
    ret <- plot_grid(p_title, p_subtitle, p , p_legend, 
                     ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
    
    h2o.show_progress()
    
    return(ret)
}

automl_models_h2o@leaderboard %>%
  plot_h2o_performance(newdata = test_tbl, order_by = "auc", 
                       size = 1, max_models = 4)

