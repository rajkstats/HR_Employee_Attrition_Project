# RECOMMENDATION ALGORITHM ----

# 1.0 Setup ----

# Libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(recipes)    # Make sure v0.1.3 or laters is installed. If not restart & install.packages("recipes") to update.


# Data
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



# 2.0 Correlation Analysis - Machine Readable ----
source("00_Scripts/plot_cor.R")

# 2.1 Recipes ----

# Numeric Features need to be binned to compare cohorts within population
# Discretization : Process of converting numeric data to categorical (factors) via binning

# Example "Age" : 
# Bin_1 (low) : 0-29 
# Bin_2 (low2med) : 30-36 
# Bin_3 (med2high) : 37-49 
# Bin_4 (high) : 50+


train_readable_tbl %>% glimpse()

factor_names <- c("JobLevel", "StockOptionLevel")

# Recipe
# step_dummy(): one_hot = TRUE : Enables one hot encoding which provides a column for every 
# category when dummying versus the default of one less column than the number of categories
# This is very beneficial for correlation analysis interpretability
 

recipe_obj <- recipe(Attrition ~ . , data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_num2factor(factor_names) %>%
  step_discretize(all_numeric(), options = list(min_unique = 1)) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()
    
recipe_obj

train_corr_tbl <- bake(recipe_obj, new_data = train_readable_tbl)

# Discretized Output : All are binary features 0's and 1's only

train_corr_tbl %>% glimpse()

# tidy() : A context specific function that converts non-data-frame objects to dataframes
# (or more specifically tibble) From the borom package. 
# recipes has a special implementation of tidy

tidy(recipe_obj)

# Running tidy without argument : Returns the overall hierarchy of all steps

# Returns the step-level details that define the step strategy

tidy(recipe_obj, number = 3)


# 2.2 Correlation Visualization ----

# Manipulate Data

train_corr_tbl %>%
  glimpse()

cor_level <- 0.06

correlation_results_tbl <- train_corr_tbl %>%
  select(-Attrition_No) %>%
  get_cor(Attrition_Yes, fct_reorder = TRUE, fct_rev = TRUE) %>%
  filter(abs(Attrition_Yes) >= cor_level) %>%
  mutate(
    relationship = case_when(
      Attrition_Yes > 0 ~ "Supports",
      TRUE ~ "Contradicts"
    )
  ) %>%
  mutate(feature_text = as.character(feature)) %>%
  separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>%
  mutate(feature_base = as_factor(feature_base) %>% fct_rev())

#correlation_results_tbl %>%
#  mutate(level = as.numeric(feature_base))

# Greater levels : Are placed at the top of Y-axis of a ggplot, and each successive 
# level is ordered descending

correlation_results_tbl 

# Create Visualisation

length_unique_groups <- correlation_results_tbl %>%
  pull(feature_base) %>% 
  unique() %>%
  length()

correlation_results_tbl %>%
  ggplot(aes(Attrition_Yes, feature_base, color = relationship)) +
  geom_point() + 
  geom_label(aes(label = feature), vjust = -0.5) +
  expand_limits(x = c(-0.3, 0.3), y = c(1,length_unique_groups + 1)) +
  theme_tq() +
  scale_color_tq() +
  labs(
    title = "Correlation Analysis : Recommendation Strategy Development",
    subtitle =  "Discretizing feature to help identify a strategy"
    )

# 3.0 Recommendation Strategy Development Worksheet ----

# ProTip : Strategy groups are great for combining interconnected strategies
# using critical thinking !


# 4.0 Recommendation Algorithm Development ----

# 4.1 Personal Development (Mentorship, Education) ----

# YearsAtCompany
# YAC - High  - Likely to stay & YAC - Low - Likely to leave
# Tie promotion if low to advance faster / Mentor if YAC is low

# TotalWorkingYears	
# TWY - High  - More likely to stay & TWY - Low - More likely to leave	
# Tie low TWY to training & formation / mentorship activities

# YearsInCurrentRole	
# More Time in current role realted to lower attrition	
# Incentivize specialize or promote / Mentorship Role

# JobInvolvement	 
# High JI - Likely to stay , low JI - Likely to leave	
# Create  a personal development program if low / Seek leadership Role

# JobSatisfaction	
# Low JS - more likely to leave / High JS - more likely to stay	
# Low : Create  a personal development plan / High - Mentorship Roles	

# PerformanceRating
# Low Personal Development Program / High Seek Leadership or Mentorship Roles  

# Some people make great mentors, some people are great leaders. Which are which ?
# Mentors: Tend to be very knowledgeable, experienced and have good satisfaction metrics
# Leaders: Tend to be very involved, and have good performance metrics

# Good Better Best Approach

# (Worst Case) Create Personal Development Plan: Job Involvement, JobSatisfaction, PerformanceRating 

# (Better Case) Promote Training & Formation: YearsAtCompany, TotalWorkingYears	

# (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction

# (Best Case 2) Seek Leadership Role : JobInvolvement, JobSatisfaction, PerformanceRating


# ProTip : If a strategy returns very few people, it should be investigated. It may be overly aggressive

# Implement Strategy into code 
train_readable_tbl %>%
  select(YearsAtCompany, TotalWorkingYears, YearsInCurrentRole,
         JobInvolvement, JobSatisfaction, PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    personal_development_strategy =  case_when(
      
      # (Worst Case) Create Personal Development Plan: Job Involvement, JobSatisfaction, PerformanceRating 
      PerformanceRating == 1 | 
        JobSatisfaction == 1 |
        JobInvolvement  <= 2      ~  "Create Personal Development Plan",
      # (Better Case) Promote Training & Formation: YearsAtCompany, TotalWorkingYears	
      YearsAtCompany < 3 |
        TotalWorkingYears < 6    ~  "Promote Training & Formation" ,
      
      # (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
      (YearsInCurrentRole > 3 |  YearsAtCompany >= 5) & 
        PerformanceRating >= 3 & 
        JobSatisfaction == 4  ~  "Seek Mentorship Role",
      
      
      # (Best Case 2) Seek Leadership Role : JobInvolvement, JobSatisfaction, PerformanceRating
       JobInvolvement >= 3 & 
        JobSatisfaction >= 3 &
        PerformanceRating>= 3  ~ "Seek Leadership Role",
      
      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  ) 
#%>% pull(personal_development_strategy) %>%
#  table()

# Progressive Targeting : The logic based system uses an if-style evaluation progressively isolating 
# the most concerning groups to the least concerning groups

# ProTip : We are not striving perfection at first. This will come with iteration and decision maker
# feedback before and during employment

train_readable_tbl %>%
  pull(JobInvolvement) %>%
  levels()

train_readable_tbl %>%
  pull(PerformanceRating) %>%
  levels()

# str_detect() : Detects the presence of a pattern with a string. Very useful inside the filter() function 
# When working with character data

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "YearsAtCompany"))

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "TotalWorking"))

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "YearsInCurr"))



# 4.2 Professional Development (Promotion Readiness) ----

# JobLevel
# Employees with  Job level 1 are leaving / Job level 2 are staying	
# Promote faster for high performers

# YearsAtCompany	
# YAC - High  - Likely to stay & YAC - Low - Likely to leave	
# Tie promotion if low to advance faster / Mentor if YAC is low

# YearsInCurrentRole	
# More Time in current role realted to lower attrition	
# Incentivize specialize or promote

# Additional Features
# JobInvolvement - Important for promotion readiness, incentivize involvment for leaders and early promotion 
# JobSatisfaction	- Important for specialization, incentivizes satisfaction for mentors
# PerformanceRating - Important for any promotion

# Good Better Best Approach

#  Ready for Rotation: YearsInCurrentRole,  JobSatisfaction (LOW)

#  Ready for Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#  Ready for Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#  Ready for Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#  Ready for Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating

#  Incentivize Specialization : YearsInCurrentRole, JobSatisfaction, PerformanceRating

# Implement Strategy into code 
train_readable_tbl %>%
  select(JobLevel, YearsInCurrentRole,
         JobInvolvement, JobSatisfaction, PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    professional_development_strategy =  case_when(
      
      #  Ready for Rotation: YearsInCurrentRole,  JobSatisfaction (LOW)
      YearsInCurrentRole >= 2 &
        JobSatisfaction <= 2                 ~"Ready for Rotation",
      
      #  Ready for Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 1 &
        YearsInCurrentRole >=2 &
        JobInvolvement >=3 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Ready for Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 2 &
        YearsInCurrentRole >=2 &
        JobInvolvement >=4 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Ready for Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 3 &
        YearsInCurrentRole >=3 &
        JobInvolvement >=4 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Ready for Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 4 &
        YearsInCurrentRole >=4 &
        JobInvolvement >=4 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Incentivize Specialization : YearsInCurrentRole, JobSatisfaction, PerformanceRating
      YearsInCurrentRole >= 4 & 
        JobSatisfaction >= 4 &
        PerformanceRating >= 3         ~ "Incentivize Specialization",

      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  ) 
#%>% pull(professional_development_strategy) %>%
#  table()

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "YearsInCurr"))


# 4.3 Work Environment Strategy ----

# OverTime	
# Employees with high OT are leaving 	
# Reduce OverTime - work life balance 


# EnvironmentSatisfaction	
# Employees with low environment satisfaction are more likely to leave	
# Improve the workplace environment - review job assignment after a period of time in current role

# WorkLifeBalance	
# Bad worklife balance is more likely to leave	
# Improve the worklife balance

# BusinessTravel	
# More Business Travel - more likely to leave , Less BT - more likely to stay	
# Reduce Business Travel where possible

# DistanceFromHome	
# High DistanceFromHome - More likely to leave
# Monitor work life balance  - Monitor Business Travel

# Additional Features
#  YearsInCurrentRole - Important for reviewing a job assignment is to give a sufficient time in a role (min 2 years)
#  JobInvolvement - Not included, but important in keeping environment satisfaction (Target Medium & low) 

# Good Better Best Approach
# Improve Work - Life Balance : OverTime, WorkLifeBalance	
# Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance	
# Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole 
# Promote Job Engagement: JobInvolvement


# Implement Strategy into code 
train_readable_tbl %>%
  select(OverTime, EnvironmentSatisfaction, WorkLifeBalance, BusinessTravel,
         DistanceFromHome, YearsInCurrentRole, JobInvolvement) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(
    work_environment_strategy =  case_when(
      
      # Improve Work - Life Balance : OverTime, WorkLifeBalance
      OverTime == 2 | 
        WorkLifeBalance == 1                 ~ "Improve Work-Life Balance",
      
      # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance	
      ( BusinessTravel == 3|
          DistanceFromHome >= 10) &
        WorkLifeBalance == 2                 ~ "Monitor Business Travel",
      
      
      # Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole 
      EnvironmentSatisfaction == 1 &
        YearsInCurrentRole >= 2             ~ "Review Job Assignment",
     
       # Promote Job Engagement: JobInvolvement
      JobInvolvement <= 2   ~ "Promote Job Engagement",
      
      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  ) %>% count( work_environment_strategy)

train_readable_tbl %>%
  pull(OverTime) %>%
  levels()

train_readable_tbl %>%
  pull(WorkLifeBalance) %>%
  levels()

train_readable_tbl %>%
  pull(BusinessTravel) %>%
  levels()

tidy(recipe_obj, number = 3) %>%
  filter(str_detect(terms, "Distance"))

# 5.0 Recommendation Function ----

data <- train_readable_tbl

data %>% 
  select(EmployeeNumber)
  
employee_number <- 19

recommend_strategies <- function(data, employee_number){
  
  data %>%
    filter(EmployeeNumber == employee_number) %>%
    mutate_if(is.factor, as.numeric) %>%
    
    # Personal Development Strategy 
    mutate(
      personal_development_strategy =  case_when(
        
        # (Worst Case) Create Personal Development Plan: Job Involvement, JobSatisfaction, PerformanceRating 
        PerformanceRating == 1 | 
          JobSatisfaction == 1 |
          JobInvolvement  <= 2      ~  "Create Personal Development Plan",
        # (Better Case) Promote Training & Formation: YearsAtCompany, TotalWorkingYears	
        YearsAtCompany < 3 |
          TotalWorkingYears < 6    ~  "Promote Training & Formation" ,
        
        # (Best Case 1) Seek Mentorship Role: YearsInCurrentRole, YearsAtCompany, PerformanceRating, JobSatisfaction
        (YearsInCurrentRole > 3 |  YearsAtCompany >= 5) & 
          PerformanceRating >= 3 & 
          JobSatisfaction == 4  ~  "Seek Mentorship Role",
        
        
        # (Best Case 2) Seek Leadership Role : JobInvolvement, JobSatisfaction, PerformanceRating
        JobInvolvement >= 3 & 
          JobSatisfaction >= 3 &
          PerformanceRating>= 3  ~ "Seek Leadership Role",
        
        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    # select(EmployeeNumber, personal_development_strategy)
  
  
    # Professional Development Strategy
  mutate(
    professional_development_strategy =  case_when(
      
      #  Ready for Rotation: YearsInCurrentRole,  JobSatisfaction (LOW)
      YearsInCurrentRole >= 2 &
        JobSatisfaction <= 2                 ~"Ready for Rotation",
      
      #  Ready for Promotion Level 2: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 1 &
        YearsInCurrentRole >=2 &
        JobInvolvement >=3 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Ready for Promotion Level 3: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 2 &
        YearsInCurrentRole >=2 &
        JobInvolvement >=4 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Ready for Promotion Level 4: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 3 &
        YearsInCurrentRole >=3 &
        JobInvolvement >=4 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Ready for Promotion Level 5: JobLevel, YearsInCurrentRole, JobInvolvement, PerformanceRating
      JobLevel == 4 &
        YearsInCurrentRole >=4 &
        JobInvolvement >=4 &
        PerformanceRating >=3                 ~ "Ready for Promotion",   
      
      #  Incentivize Specialization : YearsInCurrentRole, JobSatisfaction, PerformanceRating
      YearsInCurrentRole >= 4 & 
        JobSatisfaction >= 4 &
        PerformanceRating >= 3         ~ "Incentivize Specialization",
      
      # Catch All
      TRUE ~ "Retain and Maintain"
    )
  ) %>%
  #select(EmployeeNumber, personal_development_strategy,professional_development_strategy)  
    
   # Work-Environment Strategy
    mutate(
      work_environment_strategy =  case_when(
        
        # Improve Work - Life Balance : OverTime, WorkLifeBalance
        OverTime == 2 | 
          WorkLifeBalance == 1                 ~ "Improve Work-Life Balance",
        
        # Monitor Business Travel: BusinessTravel, DistanceFromHome, WorkLifeBalance	
        ( BusinessTravel == 3|
            DistanceFromHome >= 10) &
          WorkLifeBalance == 2                 ~ "Monitor Business Travel",
        
        
        # Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole 
        EnvironmentSatisfaction == 1 &
          YearsInCurrentRole >= 2             ~ "Review Job Assignment",
        
        # Promote Job Engagement: JobInvolvement
        JobInvolvement <= 2   ~ "Promote Job Engagement",
        
        # Catch All
        TRUE ~ "Retain and Maintain"
      )
    ) %>%
    select(EmployeeNumber, personal_development_strategy,professional_development_strategy, work_environment_strategy)
}

train_readable_tbl %>%
  select(EmployeeNumber) 

train_readable_tbl %>%
  recommend_strategies(1)

train_readable_tbl %>%
  recommend_strategies(2)

train_readable_tbl %>%
  recommend_strategies(4)

test_readable_tbl %>%
  select(EmployeeNumber) 

test_readable_tbl %>%
  recommend_strategies(228)