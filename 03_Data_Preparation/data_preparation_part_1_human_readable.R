# Data Preparation ----
# Human Readable ----

# Loading libraries
library(readxl)
library(tidyverse)
library(tidyquant)
library(stringr)
library(forcats)

#Load Data
path_train <- "00_Data/telco_train.xlsx"
path_data_definitions <- "00_Data/telco_data_definitions.xlsx"

train_raw_tbl <- read_excel(path_train, sheet=1)
definitions_raw_tbl <- read_excel(path_data_definitions, sheet = 1, col_names = FALSE)

# Tidying the data ----

train_raw_tbl %>% glimpse() 

# X_1 = Feature Name , X_2 = Feature Code + Feature Description
# These are actually multiple non-tidy datasets stored in one file 
View(definitions_raw_tbl)

# fill() : Replace missing values (NAs) with the closest entry 
# ( previous if .direction = "down" or next if .direction = "up")

# separate() : Turns a single character column into multiple columns using 'sep' argument 

definitions_tbl <- definitions_raw_tbl %>% 
  fill(X__1,.direction = "down") %>%
  filter(!is.na(X__2)) %>%
  separate(X__2, into = c("key", "value"), sep = " '", remove = TRUE) %>%
  rename(column_name = X__1) %>%
  mutate(key = as.numeric(key)) %>%
  mutate(value = value %>% str_replace(pattern = "'",replacement = ""))

definitions_tbl

# Multiple datasets stored in one data frame. These needs to be integrated 
# separately into training and test sets

# split() : splits a dataframe into multiple dataframes contained within a list
# supply column name as a vetcor (meaning .$column_name)

# map(): Applies functions to each element in a vector or list

# forcats::as_factor() : Differs from base::as_factor() . Creates factor in the order
# in which they appear as opposed to alphabetically

definitions_list <- definitions_tbl %>%
  split(.$column_name) %>%
  map(~ select(., -column_name)) %>%
  map(~ mutate(., value = as_factor(value)))
 
definitions_list

#definitions_list[[1]]  

for(i in seq_along(definitions_list)) {
  
  list_name <- names(definitions_list)[i]
  colnames(definitions_list[[i]]) <- c(list_name,paste0(list_name,"_value"))
  
}
  
definitions_list

# Goal : Iteratively join the dataframes within the definitions list with main data frame (training data)

# list() : Create a list that contains objects . Lists are a great way to store objects that relate to each 
# other. They can also be iterated over using various functions in the purr package

#ProTip : Use lists to collect objects that need to be iterated over. Use purr functions to iterate.

# reduce() : Iteratively applies a user specified function to successive binary sets of objects. 
# For example : a three element vector would have a function applied to first two elements and that output
# would then have the function applied with the third element

# one_of() : select helper for programmatically selecting variables
# set_names() : convenient way to automate processing of names in a  vector, list or dataframes 

data_merged_tbl <- list(HR_Data = train_raw_tbl) %>%
  append(definitions_list, after = 1) %>%
  reduce(left_join) %>%
  select(-one_of(names(definitions_list))) %>%
  set_names(str_replace_all(names(.),pattern = "_value",replacement = "")) %>%
  select(sort(names(.)))

data_merged_tbl %>%
  glimpse()


data_merged_tbl %>%
  select_if(is.character) %>%
  glimpse()

# non-travel should be lowest , then travel rarely and then travel frequently
data_merged_tbl %>%
  distinct(BusinessTravel)

# fct_relevel(): Allows moving of factor levels, which helps with getting factors 
# in right order

data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  select_if(is.factor) %>%
  map(levels)

data_processed_tbl <- data_merged_tbl %>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely","Travel_Frequently"),
    MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced") 
  )

data_processed_tbl %>%
  select_if(is.factor) %>%
  map(levels)

# Processing Pipeline -----
definitions_raw_tbl -> definitions_tbl
train_raw_tbl -> data

process_hr_data_readable <- function(data, definitions_tbl) {
  
definitions_list <- definitions_tbl %>%
    fill(X__1, .direction = "down") %>%
    filter(!is.na(X__2)) %>%
    separate(X__2,into = c("key", "value"),sep = " '",remove = TRUE) %>%
    rename(column_name = X__1) %>%
    mutate(key = as.numeric(key)) %>%
    mutate(value = value %>% str_replace(pattern = "'", replacement = "")) %>%
    split(.$column_name) %>%
    map( ~ select(.,-column_name)) %>%
    map( ~ mutate(., value = as_factor(value)))
  
  for (i in seq_along(definitions_list)) {
    list_name <- names(definitions_list)[i]
    colnames(definitions_list[[i]]) <-c(list_name, paste0(list_name, "_value"))
  }
  
  data_merged_tbl <- list(HR_Data = data) %>%
    append(definitions_list, after = 1) %>%
    reduce(left_join) %>%
    select(-one_of(names(definitions_list))) %>%
    set_names(str_replace_all(names(.), pattern = "_value", replacement = "")) %>%
    select(sort(names(.))) %>%
    mutate_if(is.character, as.factor) %>%
    mutate(
      BusinessTravel = BusinessTravel %>% fct_relevel("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
      MaritalStatus  = MaritalStatus %>% fct_relevel("Single", "Married", "Divorced")
    )
  
  return(data_merged_tbl)
  
} 



process_hr_data_readable(train_raw_tbl,definitions_tbl = definitions_raw_tbl) %>%
  glimpse()
