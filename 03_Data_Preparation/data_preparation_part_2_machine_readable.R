# Data Preparation ----
# Machine Readable ----

# Setup ----
rm(list = ls())
# Loading libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)
library(ggplot2)
library(recipes)

#Load Data
path_train <- "00_Data/telco_train.xlsx"
path_test <- "00_Data/telco_test.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet=1)
test_raw_tbl <- read_excel(path_test, sheet=1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

source("00_Scripts/data_processing_pipeline.R")
train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_tbl = definitions_raw_tbl)
test_readable_tbl <- process_hr_data_readable(test_raw_tbl, definitions_tbl = definitions_raw_tbl)

# Plot Faceted Histogram function ----
# Inspect Feature Distributions

# Gathering Data: When plotting in ggplot2, data must be in long format to make facets. The
# gather() function can be used to get into long format

#ProTip: Changing to character with as.character() then factor with as.factor() arranges factors alphabetically

data <- train_raw_tbl

plot_hist_facet <- function(data, bins = 10, ncol = 5, 
                            fct_reorder = FALSE, fct_rev = FALSE,
                            fill = palette_light()[[3]],
                            color = "white", scale = "free") {
  
  data_factored <- data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE)
  
  if(fct_reorder) {
    data_factored <- data_factored %>%
      mutate(key = as.character(key) %>% as.factor())
  }
  
  if(fct_rev){
    data_factored <- data_factored %>%
      mutate(key = fct_rev(key))
  }
  
  g <- data_factored %>%
    ggplot(aes(x = value, group = key )) +
    geom_histogram(bins = bins, fill = fill , color = color,
                   ) +
    facet_wrap(~ key, ncol = ncol, scale = scale) +
    theme_tq()

  
  return(g)
  
}

# everything () : Used in select() to select every remaining variable (feature) that has 
# not been selected in a dataframe. A great way to reorder columns by selecting specific columns and
# then using everything() to get the remaining ones


train_raw_tbl %>%
  select(Attrition, everything()) %>%
  plot_hist_facet(bins = 10, ncol = 5, fct_rev = F)

# ProTip: Correlation Analysis is a great way to determine if you are getting good features prior to modeling. 

# Data Preprocessing with recipes ----

# Plan : Correlation Analysis 
# 1. Zero Variance Features ----
# Note : We will use histogram as a way to see what transformations we need to make

# ~ is used to separate the outcomes from predictors. Using a (.) is R shorthand telling a function to select 
#  all variables except Attrition as the predictors 
recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors())

recipe_obj

# prep() : prepares the recipe performing any preliminary calculations and essentially figuring out what to do
# Note prep() doesn't transform the data. It just figures out what transformations need to be performed

# bake() : Performs the transformation on new data. Once a recipe has instructions (steps) and has been 
# prepared, data can be transformed using the bake() function 

recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl)

# 3-part process
# 1. Create the instructions with recipes and steps
# 2. prepare the recipe
# 3. bake the new data


# 2. Transformations ----

# skewness() : Computes the skewness of a univariate distribution. Highly skewed features have either 
# high positive or high negative values depending on the direction of skew . 

# factor_key =TRUE to preserve the order of variables

skewed_feature_names <- train_readable_tbl %>%
  select_if(is.numeric) %>%
  map_df(skewness) %>%
  gather(factor_key = TRUE) %>%
  arrange(desc(value)) %>%
  filter(value >= 0.8) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>%
  select(skewed_feature_names) %>%
  plot_hist_facet()

# removing features which needs to be converted to factors and should not be transformed
!skewed_feature_names %in% c("JobLevel", "StockOptionLevel") 

skewed_feature_names <- train_readable_tbl %>%
  select_if(is.numeric) %>%
  map_df(skewness) %>%
  gather(factor_key = TRUE) %>%
  arrange(desc(value)) %>%
  filter(value >= 0.8) %>%
  filter(!key  %in% c("JobLevel", "StockOptionLevel")) %>%
  pull(key) %>%
  as.character()

skewed_feature_names

factor_names <-  c("JobLevel", "StockOptionLevel")

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names)

recipe_obj %>%
  prep() %>%
  bake(train_readable_tbl) %>%
  select(skewed_feature_names) %>%
  plot_hist_facet()
    

# 3. Center/Scaling ----

# Converts numeric data into different scales to be on same scale
# Algorithms that require feature scaling : Kmeans, DeepLearning, PCA, SVMs
# Can't Remember Which Algo needs it ? When in doubt, center and scale. It won't typically hurt your predictions

train_readable_tbl %>%
  select_if(is.numeric) %>%
  plot_hist_facet()

#ProTip: Make sure you always center before you scale
# all_numeric() : special selector for recipes steps that select only numeric features in dataset

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

recipe_obj$steps[[4]] # before prep (mean is NULL)

prepared_recipe <- recipe_obj %>% prep()

prepared_recipe$steps[[4]] # 4 step is centering

prepared_recipe %>%
  bake(new_data = train_readable_tbl) %>%
  select_if(is.numeric) %>%
  plot_hist_facet()

# 4. Dummy Variables ----

# Dummy variables : Expanding categorical features into multiple columns of 0's and 1's
# If factor has 3 levels, the feature is expanded into 2 columns (1 less than the number of levels)

recipe_obj <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

recipe_obj

recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl) %>%
  select(contains("JobRole")) %>%
  plot_hist_facet()


# all_nominal() : special selector function for recipes steps that selects only categorical data

dummied_recipe_obj  <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal())

dummied_recipe_obj

dummied_recipe_obj %>%
  prep() %>%
  bake(new_data = train_readable_tbl) %>%
  select(contains("JobRole")) %>%
  plot_hist_facet(ncol = 3)

recipe_obj  <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal())

# Final Recipe ----

recipe_obj  <- recipe(Attrition ~ ., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_num2factor(factor_names) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal()) %>%
  prep()

recipe_obj

train_tbl <- bake(recipe_obj, new_data = train_readable_tbl)
train_tbl %>% glimpse()

test_tbl <- bake(recipe_obj, new_data = test_readable_tbl)
test_tbl %>% glimpse()


# tidy(recipe_obj)
# bake() : Applys a recipe object to a dataset. This is the function that DOES the
# transforming using a recipe that has been setup with steps and prepares with prep()


# Correlation Analysis ----
# Correlation analysis only works with numeric data. You will get an error if you try to run a 
# correlation on a factor or character data type

data <- train_tbl

#ProTip: quo() and enquo() do the exact same thing. The only difference is that quo() is
# used outside of functions and enquo() is used inside
feature_expr <- quo(Attrition_Yes)

# cor() : Returns a square correlation data frame , correlating every column in data frame against all of others.
# Return is square meaning no. of rows  = no. of columns
# use = "pairwise.complete.obs" (Argument of cor(). No the default, but is almost always what you want to do). If you 
# use = "everything" (default) then you run the risk of getting errors from missing values

# Correlation Data Frame Properties :
# Is square (e.g 66 x 66)
# Has 1 running down diagonal
# Is triangularly inverted meaning dim(row,col) = (1,2) = (2,1) 

get_cor <- function(data, target, use = "pairwise.complete.obs",
                     fct_reorder = FALSE, fct_rev = FALSE){
    
  feature_exp <- enquo(feature_expr)
  feature_name <- quo_name(feature_exp)
  
  data_cor <-  data %>%
    mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    cor(use = use) %>%
    as.tibble() %>%
    mutate(feature = names(.)) %>%
    select(feature, !! feature_exp) %>%
    filter(!(feature == feature_name)) %>%
    mutate_if(is.character, as_factor)

# fct_reorder: reorders a factor by another column
# Info: fct_reorder()  chnages the level of factors but does not rearrange the data frame 
# Tack on arrange() by factored columns to get in appropriate order

# Info : We do this factor level reordering for plotting function, plot_cor()
  
  if(fct_reorder){
    data_cor <- data_cor %>%
      mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
      arrange(feature)
  }
  
  if(fct_rev){
    data_cor <- data_cor %>%
      mutate(feature = fct_rev(feature)) %>%
      arrange(feature)
  }
  
return(data_cor)  
}

train_tbl %>%
  get_cor(Attrition_Yes, fct_reorder = T, fct_rev = T )
  

data <- train_tbl
feature_expr <- quo(Attrition_Yes)


plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]){

  
  feature_expr <- enquo(target)
  feature_name <- quo_name(feature_expr)

# str(get_cor())    
  data_cor <-  data %>%
      get_cor(!! feature_expr, fct_reorder = fct_reorder , fct_rev = fct_rev) %>%
      mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
      mutate(Correlation = case_when(
        (!! feature_expr) >=0 ~ "Positive",
        TRUE                  ~ "Negative") %>% as.factor())
  


      g <- data_cor %>%
        ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
        geom_point(aes(color = Correlation), size = size) +
        geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
        geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
        expand_limits(x = c(-1,1)) +
        theme_tq() +
        scale_color_manual(values = c(color_neg, color_pos))
      
      
    if(include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
    
    return(g)    
}

#names(train_tbl)

train_tbl %>% 
  select(Attrition_Yes,contains("JobRole")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# Variables that contribute most to attrition lies at the bottom end

# Correlation Evaluation ----

# Explore features by Category 

# 1. Descriptive Features : Age, gender, marital status
train_tbl %>% 
  select(Attrition_Yes, Age, contains("Gender"), contains("MaritalStatus"), 
         NumCompaniesWorked, contains("Over18"), DistanceFromHome) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# 2. Employment Features : Department, job role , job level 
train_tbl %>% 
  select(Attrition_Yes, contains("employee"), contains("department"), contains("job") ) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# 3. Compensation Features : DailyRate ,HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel 
train_tbl %>% 
  select(Attrition_Yes, contains("income"), contains("rate"), contains("salary"), contains("stock") ) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# 4. Survey Results : EnvironmentSatisfaction, JobSatisfaction , RelationshipSatisfaction, WorkLifeBalance
train_tbl %>% 
  select(Attrition_Yes, contains("satisfaction"), contains("life") ) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# 5. Performance Data : JobInvolvement, PerformanceRating 
train_tbl %>% 
  select(Attrition_Yes, contains("performance"), contains("involvement")) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# 6. Work - Life Features : BusinessTravel , OverTime
train_tbl %>% 
  select(Attrition_Yes, contains("overtime"), contains("travel")) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)

# 7. Training & Education : Education, EducationField , TrainingTimesLastYear
train_tbl %>% 
  select(Attrition_Yes, contains("training"), contains("education")) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)


# 8. Time - Based Features: TotalWorkingYears , YearsAtCompany , YearsInCurrentRole , YearsSinceLastPromotion, YearsWithCurrManager 
train_tbl %>% 
  select(Attrition_Yes, contains("years")) %>% 
plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = F)


# ProTip: Spend your time on collecting data on good features. Correlation Analysis helps with this !




# Notes ----
# Impute : The act of filling in missing values within features. Common methods include: 
# - filling by recency (tidyr::fill)
# - filling by similarity (knn impute)

# Outlier Handling : Outliers are extremes. Sometimes outliers should be removed if
# clearly an error in data collection . Otherwise, extreme events may be reality and possibly should be kept
# Outliers are unfortunately very difficult to deal with

# Recommendation : Don't remove unless necessary. Rather, select algorithm that is more resistant to outliers

# Zero Variance Features : Features that have no variance, and therefore lend nothing the predictive quality of model

# Transformation : chnages the data to remove skew (e.g log), stabilize variance (e,g Box Cox) 
# or make stationary (e.g difference for time series)

# Normality: When the distribution of data has bell shaped curve. This is a requirement for linear models
# that depend on correlation (e.g correlation analysis, linear regression, logistic regression etc)
# Non-linear models (e.g random forest, decision trees etc) can handle non-normal distributions because these depend more on
# how the data can be segregated from other data

# Skewness : Data Often is skewed ,  which is when the majority of observations lie within a relatively
# small range, but the minority have a very wide spread . Data that is skewed is often to have a "fat" or long tail

# Discretization : Act of making continuous variable discrete. Think of turning a variable like
# age into cohorts of less than 20 years, 20-30, 30-40 etc.

# Caution: Discretization can hurt correlations. It's often best not to discretize unless there is a specific
# need to do so (like explaining the difference between what "millenials" do versus "Gen X'ers")

# Normalization : Not to be confused with removing skew! Normalization is getting the data onto a consistent scale. 
# some machine learning algorithms (e.g PCA, KNN, Deep learning etc) depend on the data to all have same scale.

# Dummy variables: Turning categorical data into separate columns of zeros and ones. This is important for machine learning
# algorithms to detect patterns in unordered data

# Interaction Variables: When two features have a relationship to each other they are said to "interact". An example is when the ratio of 
# height and weight of a person is related to obesity. Height and weight are said to interact

# Multivariate Transformations : Examples include PCA, which is a dimensionality reduction algorithm. It's useful in cases where the
# data is very wide and can be suseptible to overfitting. PCA allows us to get in the essence of data by reducing the width into principal components



