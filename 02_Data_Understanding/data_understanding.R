# Data Understanding ----

# Loading libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(skimr)
library(GGally)

#Load Data
path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet=1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

definitions_raw_tbl

train_raw_tbl

# glimpse() : Visualizing a tibble with columns going length-wise (transposed) to more 
# easily view the data

glimpse(train_raw_tbl)

# Feature Categories
# Descriptive Features : Age,DistanceFromHome, Gender , MaritalStatus, NumCompaniesWorked, Over18 
# Employment Features : Department, EmployeeCount, EmployeeNumber , JobInvolvement , JobRole, JobSatisfaction
# Compensation Features : DailyRate ,  HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel 
# Survey Results : EnvironmentSatisfaction, JobSatisfaction , RelationshipSatisfaction, WorkLifeBalance
# Performance Data : JobInvolvement, PerformanceRating 
# Work - Life Features : BusinessTravel , OverTime
# Training & Education : Education, EducationField , TrainingTimesLastYear
# Time - Based Features: TotalWorkingYears , YearsAtCompany , YearsInCurrentRole , YearsSinceLastPromotion, YearsWithCurrManager 

# ProTip :Break down data collection activities into strategic areas

# Exploratory Data Analysis (EDA) ----
# Step 1 : Data summarization ----

#skim() : returns summary by data type . This include missing values and number of unique features for categorical data
# For numeric data , it returns histograms and quantiles.

# ProTip : Separating your data by datatype (e. numeric vs categorical) is a great way to investigate properties of data
#ProTip : Histograms are extremely useful for analyzing numeric data (outliers, skew, should it be a factor ?) and more

#ProTip: If number of unique categorical features is large, consider creating an "other" category
#Info: A categorical feature that has only one unique level offers no value to modeling

skim(train_raw_tbl)


# Character Data Type

#select_if() : select only columns matching a function . Typically used with a data type selector function.
# For ex: can select only character by passing the function  is.character 

train_raw_tbl %>% 
  select_if(is.character) %>% 
  glimpse()

#map() : Iterate over a list. When used with a data frame (or tibble) , iterates over the columns. When 
# used with a data frame inside mutate(), iterate over rows

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(unique)

# Anonymous Tidy Function :Can make anonymous function  that are not pre-defined (hence anonymous). They 
# begin with tilde ~ and take .  as  an argument

# table :Convert character or factor data into counts. Can take single or multiple categorical inputs and 
# cross - tabulate counts

#prop.table() : Modifies the output of table to proportions

train_raw_tbl %>% 
  select_if(is.character) %>% 
  map(~ table(.) %>% prop.table())

# Discrete Features : Some numeric features may not be continuous and are actually categorical. 
# These are called discrete features because they have defined levels even though they are stored as numeric.
# They typically should be converted to categorical data types (i.e factor)

# Numeric Data

train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map(~ unique(.) %>% length())

# map_df() : Works exactly like map() except attempts to convert the list output to a dataframe
# gather() : collect columns in wide format and converts to long format with the column names 
# as variables in "key" column

train_raw_tbl %>% 
  select_if(is.numeric) %>% 
  map_df(~ unique(.) %>% length()) %>%
  gather() %>% 
  arrange(value) %>%
  filter(value <= 10)


# Info : Numeric Varibales that are lower in levels are likely to be discrete 
#  Numeric Varibales that are higher in levels are likely to be continuous


# GGally
# ggpairs contains three sections that compares features : 
# Diagonal - Contain density for continuous and counts / proportions as bars for discrete 
# lower triangle - Contains histogram for numeric categorical pairs , scatter for numeric-numeric pairs
# and bars for categorical-categorical pairs
# upper traingle - box-plot for numeric - categorical pairs , correlation value for numeric-numeric pairs
# bars for categorical - categorical pairs 


# Step 2 : Data Visualization ----
train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  ggpairs()

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  ggpairs(aes(color = Attrition), lower = "blank", legend = 1, 
          diag = list(continuous = wrap("densityDiag", alpha = 0.5))) +
  theme(legend.position = "bottom")

plot_ggpairs <- function(data, color = NULL, density_alpha = 0.5) {
  
  color_expr <- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) {
    
    g <- data %>%
      ggpairs(lower = "blank") 
    
  } else {
    
    color_name <- quo_name(color_expr)
    
    g <- data %>%
      ggpairs(mapping = aes_string(color = color_name), 
              lower = "blank", legend = 1,
              diag = list(continuous = wrap("densityDiag", 
                                            alpha = density_alpha))) +
      theme(legend.position = "bottom")
  }
  
  return(g)
  
}
  

train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  plot_ggpairs(color = Attrition)


# Explore features by Category 

# 1. Descriptive Features : Age, gender, marital status
train_raw_tbl %>% 
  select(Attrition, Age, Gender, MaritalStatus, NumCompaniesWorked, Over18, DistanceFromHome) %>% 
  plot_ggpairs(color = Attrition)

# 2. Employment Features : Department, job role , job level 
train_raw_tbl %>% 
  select(Attrition, contains("employee"), contains("department"), contains("job") ) %>% 
  plot_ggpairs(color = Attrition)

# 3. Compensation Features : DailyRate ,HourlyRate, MonthlyIncome, MonthlyRate, PercentSalaryHike, StockOptionLevel 
train_raw_tbl %>% 
  select(Attrition, contains("income"), contains("rate"), contains("salary"), contains("stock") ) %>% 
  plot_ggpairs(color = Attrition)

# 4. Survey Results : EnvironmentSatisfaction, JobSatisfaction , RelationshipSatisfaction, WorkLifeBalance
train_raw_tbl %>% 
  select(Attrition, contains("satisfaction"), contains("life") ) %>% 
  plot_ggpairs(color = Attrition)

# 5. Performance Data : JobInvolvement, PerformanceRating 
train_raw_tbl %>% 
  select(Attrition, contains("performance"), contains("involvement")) %>% 
  plot_ggpairs(color = Attrition)

# 6. Work - Life Features : BusinessTravel , OverTime
train_raw_tbl %>% 
  select(Attrition, contains("overtime"), contains("travel")) %>% 
  plot_ggpairs(color = Attrition)

# 7. Training & Education : Education, EducationField , TrainingTimesLastYear
train_raw_tbl %>% 
  select(Attrition, contains("training"), contains("education")) %>% 
  plot_ggpairs(color = Attrition)


# 8. Time - Based Features: TotalWorkingYears , YearsAtCompany , YearsInCurrentRole , YearsSinceLastPromotion, YearsWithCurrManager 
train_raw_tbl %>% 
  select(Attrition, contains("years")) %>% 
  plot_ggpairs(color = Attrition)





# DataExplorer package to produce a nice html report for eda
# which(apply(train_raw_tbl, 2, var)==0)
# (9)EmployeeCount , (27)StandardHours
#Removing columns other than numeric
#nums <- unlist(lapply(train_raw_tbl, is.numeric)) 
#train<-train_raw_tbl[ , nums]
#Removing columns with constant variance which are EmployeeCount,StandardHours 
# we are left with only 24 columns out of 35
#train<-train[ , apply(train, 2, var) != 0]
#create_report(train)








