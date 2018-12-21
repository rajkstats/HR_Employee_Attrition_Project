#BUSINESS UNDERSTANDING -----

# Loading libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)

#Read Data
path_train <- "00_Data/telco_train.xlsx"
train_raw_tbl<-read_excel(path_train,sheet = 1)
colnames(train_raw_tbl)

# In realistic situation , we may not have the data on all the columns listed
#Subsetting data
dept_job_role_tbl <- train_raw_tbl %>% select(EmployeeNumber,Department,JobRole,Attrition)

#Pro Tip : Always append your variables with class descriptor (e.g "tbl") to keep variables organized

dept_job_role_tbl

# 1. Business Science Problem Framework ----
# 1A. View Business as Machine ----
# BSU's : Department and Job Roles
# Define Objectives : Retain High Performers
# Assess Outcomes : TBD
dept_job_role_tbl %>% group_by(Attrition) %>% summarize(n=n())

# Applying ungroup operation to see if there are any groups
#group -> perform grouped operations -> ungroup
dept_job_role_tbl %>% group_by(Attrition) %>% summarize(n=n()) %>% ungroup()

dept_job_role_tbl %>% 
  group_by(Attrition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  mutate(pct=n/sum(n))

# 1B. Understand the Drivers ----
# Investigation Objectives : 16 % Attrition
# Synthesize Outcomes : High counts and High Percentages
# Hypothesize Drivers : Job Role and department

# Job role and Department are cohort (Group within a population that often has specific sub-population trends)
# Percentages created are within a department not overall percentage , that's the reason we grouped again by Department

# Department ----
dept_job_role_tbl %>% 
  
  group_by(Department,Attrition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  group_by(Department) %>%
  mutate(pct=n/sum(n))

# Looking at the Results it shows that, there may be something going on by department since we observe different percentages of 
# attrition within different departments

#Pro Tip : Separate actions to make related code steps clear & to emphasize your workflow



# Job Role ----
# Hierarchial Relationships : Categories can often have sub-categories 
# Sub-Categories : Identifying sub-categories within cohorts can even further help in defining meaningful relationships 
# Tidy Data: Has one row per observation, and one column per feature 

dept_job_role_tbl %>% 
  
  group_by(Department,JobRole, Attrition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  group_by(Department,JobRole) %>%
  mutate(pct=n/sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% "Yes" )

# We observe high counts and high percentages by Job Role and Departments (For ex: Sales (High Counts))

# 1C. Measure the Drivers ----

# Collect information on employee attrition : ongoing 
# Develop KPI's : Industry KPIs : 8.8%
# Google : 8.8% may be conservative compared to the Bureau of Labor Statistics. For our purposes, consider this as a conservative KPI
# that indicates a major problem if exceeded


# case_when : 
# Replaces if else statments in mutate
# Benefit: we are able to have multiple cases that are clearly specified, not NESTED
#steps are evaluated in order. Always end with TRUE~ + value that you want items not meeting the  criteria(s) above to get

dept_job_role_tbl %>% 
  
  group_by(Department,JobRole, Attrition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  group_by(Department,JobRole) %>%
  mutate(pct=n/sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% "Yes" ) %>%
  arrange(desc(pct)) %>%
  mutate (
    above_industry_avg =  case_when(
      pct> 0.088 ~ "Yes" ,
      TRUE ~ "No"
    )
  )

# 1D. Uncover Problems and opportunities ----
calculate_attrition_cost<- function (
  #Employee  
  n                  = 1,
  salary             = 80000,
  
  
  #Direct Costs
  separation_cost          = 500,
  vacancy_cost             = 10000,
  acquisition_cost         = 4900,
  placement_cost           = 3500, 
  
  
  #Productivity Costs
  net_revenue_per_employee = 250000, 
  workdays_per_year        = 240,
  workdays_position_open   = 40,
  workdays_onboarding      = 60, 
  onboarding_efficiency    = 0.50
  
){
  
  #Direct Costs
  direct_cost <- sum(separation_cost, vacancy_cost, acquisition_cost, placement_cost)
  
  #Lost Productivity Costs
  productivity_cost <- net_revenue_per_employee /workdays_per_year *
    (workdays_position_open + workdays_onboarding * onboarding_efficiency)
  
  # Savings of salary and Benefits (Cost Reduction)
  salary_benefit_reduction <- salary / workdays_per_year * workdays_position_open
  
  #Estimated Turnover per Employee 
  cost_per_employee <- direct_cost + productivity_cost - salary_benefit_reduction
  
  # Total Cost of Employee Turnover 
  total_cost <- n * cost_per_employee
  
  return(total_cost)
}

#Per Person
calculate_attrition_cost()
# 78483.33

#For 200 employees 
calculate_attrition_cost(200)
#15696667 (15.7 million)

#Calculating cost by JobRole ----

dept_job_role_tbl %>% 
  
  group_by(Department,JobRole, Attrition) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  
  group_by(Department,JobRole) %>%
  mutate(pct=n/sum(n)) %>%
  ungroup() %>%
  
  filter(Attrition %in% "Yes" ) %>%
  arrange(desc(pct)) %>%
  mutate (
    above_industry_avg =  case_when(
      pct> 0.088 ~ "Yes" ,
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,80000))

#ProTip : Keep flexibility in mind !. Assess your code and create flexible functions to streamline your workflow

# Workflow of Attrition ----

dept_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  
  count_to_pct(Department, JobRole)%>%
  
  filter(Attrition %in% "Yes" ) %>%
  arrange(desc(pct)) %>%
  mutate (
    above_industry_avg =  case_when(
      pct> 0.088 ~ "Yes" ,
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,80000))

#Info: Tidy Eval is a programming language built into dplyr and the tidyverse packages
# ... : enable passing multiple, un-named arguments to a function 
# Because dots are not pre-selected , user can flexibly add variables and function will adapt ! 

#ProTip: The first argument of "tidy" function is always data

#quos : It captures the expressions that remian unevaluated by R 
#enquo : Same as quos, used when there is only single argument 

count_to_pct <-  function(data,...,col=n){
      
    grouping_vars_expr <- quos(...) #quos(Department, JobRole)
    col_expr <- enquo(col)  
    
    ret <- data %>%
              group_by(!!! grouping_vars_expr) %>%
              mutate(pct = (!! col_expr) /sum(!! col_expr)) %>%
              ungroup()
    
    return(ret)
}



dept_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  
  count_to_pct(Department, JobRole)%>%
  
  filter(Attrition %in% "Yes" ) %>%
  arrange(desc(pct)) %>%
  mutate (
    above_industry_avg =  case_when(
      pct> 0.088 ~ "Yes" ,
      TRUE ~ "No"
    )
  ) %>% 
  
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,80000))

#Info: Function inputs in text (e.g attrition_value = "Yes") format that don't require tidy Eval
#ProTip: Use paranthesis to give TidyEval evaluations priority 

assess_attrition <- function(data, attrition_col, attrition_value, baseline_pct) {
      
      attrition_col_expr <- enquo(attrition_col)
        
      data %>%
            filter((!! attrition_col_expr) %in% attrition_value ) %>%
            arrange(desc(pct)) %>%
            mutate (
                above_industry_avg =  case_when(
                pct> baseline_pct ~ "Yes" ,
                TRUE ~ "No"
            )
          )
            
}

dept_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  
  count_to_pct(Department, JobRole)%>%
  
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
  
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,80000))


# Visualization of Attrition cost ----

#ProTip: For ggplot2 visualizations, its best to process text to a data manipulation step before viz step
#ProTip: For ggplot2 visualizations, factors are used to order categorical variables (e.g non-numeric axis)
#Info: Factors are numeric (e.g 1,2,3..) with labels that are printed (e.g "Small", "Medium", "Large")
# because they are numeric, they are easily reordered and the ordering can be modified by changing the hidden numeric value

#fct_reorder(): Reorders a factor numeric values by the magnitude of a different numeric variable 
#pull() : Extract the values from a column
#level(): Extract the levels of values in a column
#format(): formats a numeric value specifying the number of decimal places using the digits argument

# Info: 
# ggplot2 viz process begins with  a blank canvas that progressively has layers added
# aes(): Maps the column names to ggplot() aesthetic variables such as x,y,color,fill...
# geom_segment(): Builds a line using x,y,xend and yend arguments
# palette_light() : A list of colors in hex format that work well together for business plotting
# geom_point : Makes a scatter plot, but also useful for making the point on lollipop plots
# scale_x_continuous(): Formats the x axis, Note scale_y_contnuous() formats y axis as well
# geom_label(): add labels to point within the plot
# theme_tq(): A general theme that adjust background , faceting palette, legend position , and more. It 
# works great for making "business-ready plots"
# scale_size(): Adjust the max and min size of elements to prevent large/small values from 
# becoming too large or small
# labs() : Adjusts the title, subtitle, x and y axis labels, legend labels and others
# themes : Adjusts the appearance of many elements individually. Many present themes (e.g theme_tq())
# take care of most appearance actions for you. Taking on theme() at the end will make any fine-tuned
# adjustments to your plot

#ProTip: Use size, color, and fill aesthetics to emphasize important features


dept_job_role_tbl %>% 
  
  count(Department, JobRole, Attrition) %>% 
  count_to_pct(Department, JobRole)%>%
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,80000)
  ) %>% 
  
  #Data Manipulation
  mutate(name = str_c(Department, JobRole, sep = ": ") %>% as_factor()) %>% 
  mutate(name = fct_reorder(name,cost_of_attrition)) %>% 
  #pull(name) %>%
  #levels()
  mutate(cost_text = str_c("$", format(cost_of_attrition / 1e6 , digits=2), "M" ,sep = "")) %>% 
  
  #Plotting
  ggplot(aes(x = cost_of_attrition, y = name )) +
  geom_segment(aes(xend = 0, yend = name ), color = palette_light()[[1]]) + 
  geom_point(aes(size = cost_of_attrition), color = palette_light()[[1]]) + 
  scale_x_continuous(labels = scales::dollar) + 
  geom_label(aes(label = cost_text, size = cost_of_attrition),
             hjust="inward",color= palette_light()[[1]]) +
  theme_tq() + 
  scale_size(range = c(3,5)) + 
  labs(title = "Estimated Cost of Attrition : By Dept and Job Role " ,
       y="", x = "Cost of Attrition") + 
  theme(legend.position = "none")
  

#ProTip: 
#Functions can be built with flexibility to adapt to different inputs (e.g Department or Job Roles)
#Functions can streamline your workflow so you can complete important actions quickly
#fct_reorder : will order the function by .value argument
#fct_rev : will rev the order of y axis
#rlang::sym() : Turns a single character string into an expression (e.g a column name). expression is typically captured 
# in enquo() or quos() to delay evaluation
# quo_name() :  Turns an expression into a character string , opposite of rlang::sym()
# switch() : Takes an argument and based on that argument value will change the return following a predefined logic.
# Similar to nested series of if-statements

#Info: A function factory is a factory that takes a function to produce function 
# dollar_format(): A function factory that is used to make various text monetary formats


 
plot_attrition <- function(data,...,.value,
                           fct_reorder = TRUE,
                           fct_rev = FALSE,
                           include_lbl = TRUE,
                           color = palette_light()[[1]],
                           units = c("0", "K", "M")) {
  
  
  #Inputs
  group_vars_expr <- quos(...)
  if(length(group_vars_expr)==0)
    group_vars_expr <- quos(rlang::sym(colnames(data)[[1]]))
  
  value_expr <- enquo(.value)
  value_name <- quo_name(value_expr)
  
  units_val <- switch(units[[1]],
                      "M" = 1e6,
                      "K" = 1e3,
                      "0" = 1) 
  if(units[[1]] == "0") units <- ""
  
  #Data Manipulation
  usd <- scales::dollar_format(prefix = "$" , largest_with_cents = 1e3)
  
  data_manipulated <- data %>%
    mutate(name = str_c(!!! group_vars_expr, sep = ": ") %>% as_factor()) %>%
    mutate(value_text = str_c(usd(!! value_expr / units_val ), units[[1]] , sep = ""))
  
  if(fct_reorder){
    data_manipulated <-  data_manipulated %>% 
      mutate(name = forcats::fct_reorder(name, !! value_expr)) %>% 
      arrange(name)
  }
    
  if(fct_rev){
    data_manipulated <-  data_manipulated %>% 
      mutate(name = forcats::fct_rev(name)) %>% 
      arrange(name)
  }
  
  
  #Visualization
  
  g <- data_manipulated  %>%
    ggplot(aes_string(x = value_name, y = "name")) +
    geom_segment(aes(xend = 0, yend = name), color = color) +
    geom_point(aes_string(size = value_name), color = color) +
    scale_x_continuous(labels = scales::dollar) +
    theme_tq() +
    scale_size(range = c(3, 5)) +
    theme(legend.position = "none")
  
  
  if(include_lbl){
    g <- g + 
    geom_label(
      aes_string(label = "value_text", size = value_name),
      hjust = "inward",
      color = color
    )
  } 
  
  return(g)

}



dept_job_role_tbl %>% 
  
  count(JobRole, Attrition) %>% 
  count_to_pct(JobRole)%>%
  assess_attrition(Attrition, attrition_value = "Yes", baseline_pct = 0.088) %>% 
  mutate(cost_of_attrition= calculate_attrition_cost(n=n,80000)
  ) %>% 
  
  plot_attrition(JobRole, .value = cost_of_attrition, units= "M") +
  labs(title = "Estimated Cost of Attrition By Job Role",
       x =  "Cost of Attrition" ,
       y="",
       subtitle = "Looks like Sales Executive and Laboratory Technician are biggest sales driver of cost")
  