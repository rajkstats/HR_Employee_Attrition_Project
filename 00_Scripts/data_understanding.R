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
