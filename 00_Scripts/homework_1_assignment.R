# HOMEWORK 1 ----

# Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

# Source Scripts ----
source("00_Scripts/assess_attrition.R")

# Data ----
path_train     <- "00_Data/telco_train.xlsx"
train_raw_tbl  <- read_excel(path_train, sheet = 1)

dept_jobrole_tbl <- train_raw_tbl %>%
    select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

kpi_industry_turnover_pct <- 0.088

# Productivity Cost by Role ----

productivity_cost_by_role_tbl <- read_excel("00_Data/productivity_cost_by_role.xlsx")
productivity_cost_by_role_tbl



# Q1: Which Job Role has the highest total cost of attrition? ----
dept_jobrole_productivity_tbl <- dept_job_role_tbl %>% 
  
  count(Department,JobRole, Attrition) %>% 
  
  count_to_pct(Department,JobRole)%>%
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  
  left_join(productivity_cost_by_role_tbl,by=c("Department","JobRole")) %>%
  
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,salary=Salary_Average ,
                                                     net_revenue_per_employee=Revenue_Average))

dept_jobrole_productivity_tbl %>%
  plot_attrition(Department, JobRole, .value = cost_of_attrition)

# Q2: What is the total cost of attrition for the Research & Development: Research Scientist job role? ----

dept_jobrole_productivity_tbl %>%
  plot_attrition(Department, JobRole, .value = cost_of_attrition, units = "M")

# Q3: What percentage do the top four Job Roles account for in terms of the total cost of attrition? ----
dept_jobrole_productivity_tbl %>% 
  arrange(desc(cost_of_attrition)) %>%
  mutate(row_num = row_number()) %>% 
  mutate(is_top_4 = case_when(
    row_num <= 4 ~ "Yes",
    TRUE ~ "No"
  )) %>%
  
# Summarize cost by top 4 
  group_by(is_top_4) %>%
  summarize(
    total_attrition_cost = sum(cost_of_attrition)
  ) %>%
  ungroup() %>%
  #Calculate percentage of total 
  mutate(total_attrition_pct = total_attrition_cost / sum(total_attrition_cost) )

#ProTip:  Visualizing the cumulative percentages by group is a good way to show executives where the biggest problem areas are!

# Alternative Method
dept_jobrole_productivity_tbl %>%
  #Rank
  arrange(desc(cost_of_attrition)) %>%
  mutate(cumulative_attrition = cumsum(cost_of_attrition) ) %>% 
  mutate(cumulative_percent = cumulative_attrition/ sum(cost_of_attrition) ) %>% 
  select(Department , JobRole, n , cost_of_attrition:cumulative_percent)
  


# Q4. Which Department has the highest total cost of attrition? ----
dept_productivity_tbl <- dept_job_role_tbl %>% 
  
  count(Department,JobRole, Attrition) %>% 
  
  count_to_pct(Department,JobRole)%>%
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = kpi_industry_turnover_pct) %>%
  
  left_join(productivity_cost_by_role_tbl,by=c("Department","JobRole")) %>%
  
  group_by(Department) %>% 
  summarise(n= sum(n),
            Salary_Average = sum(Salary_Average),
            Revenue_Average = sum(Revenue_Average)) %>% 
  
  #Attrition cost
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,salary=Salary_Average ,
                                                     net_revenue_per_employee=Revenue_Average))

dept_productivity_tbl %>%
  plot_attrition(Department, .value = cost_of_attrition)

# Q5: What percentage does the top Department account for in terms of the total cost of attrition? ----
dept_productivity_tbl %>% 
  count_to_pct(col = cost_of_attrition) %>%
  arrange(desc(pct))

