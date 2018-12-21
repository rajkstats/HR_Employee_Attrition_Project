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

# Data Processing Pipeline 
source("00_Scripts/data_processing_pipeline.R")

train_readable_tbl <- process_hr_data_readable(train_raw_tbl, definitions_raw_tbl)

#Education 

# Before
train_raw_tbl %>%
  ggplot(aes(Education)) +
  geom_bar()
  
# After
train_readable_tbl %>%
  ggplot(aes(Education)) +
  geom_bar()


# Business Travel 

# Before
train_raw_tbl %>%
  ggplot(aes(BusinessTravel)) +
  geom_bar()

# After
train_readable_tbl %>%
  ggplot(aes(BusinessTravel)) +
  geom_bar()
