# LIME FEATURE EXPLANATION ----

# 1. Setup ----

# Load Libraries 

library(h2o)
library(recipes)
library(readxl)
library(tidyverse)
library(tidyquant)
library(lime)

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

automl_leader <- h2o.loadModel("04_Modeling/h20_models/StackedEnsemble_BestOfFamily_0_AutoML_20181202_142146")

automl_leader

# Resources : http://uc-r.github.io/lime
# https://www.business-science.io/business/2018/06/25/lime-local-feature-interpretation.html
# Lime Vignettes : https://cran.r-project.org/web/packages/lime/vignettes/Understanding_lime.html
# https://arxiv.org/abs/1602.04938 / https://arxiv.org/pdf/1602.04938.pdf

# For models outside H2o :http://www.business-science.io/business/2017/11/28/customer_churn_analysis_keras.html

# 3. LIME ----

# 3.1 Making Predictions ----

predictions_tbl <- automl_leader %>% 
  h2o.predict(newdata = as.h2o(test_tbl)) %>%
  as.tibble() %>%
  bind_cols(
    test_tbl %>%
      select(Attrition, EmployeeNumber)
  )

predictions_tbl

test_tbl %>%
  slice(5) %>%
  glimpse()

# Why LIME ?

# Used to determine which features contribute to prediction (& by how much) for a single observation (i.e local)

# 3.2 Single Explanation ----

# Remove the Target Feature : H2O Model does not use the Attrition Column within prediction set

# Explainer : The "recipe"  for creating the explanation. It contains the ML model & feature distributions (bins)
# for the training data

# lime() : Creates an "explainer" from training and model object. The returned object contains the ML model
# and the feature distributions of training data

# LIME Steps
# 1. Build explainer with lime()
# 2. Create an explanation  with explain()


explainer <- train_tbl %>%
  select(-Attrition) %>%
  lime(
    model           =  automl_leader,
    bin_continuous  =  TRUE,
    n_bins          =  4, 
    quantile_bins   = TRUE
    )

explainer

# ProTip: h2o, keras and caret R packages have been integrated to lime 
# If you ever need to use an unintegrated package, you can do so by creating special functions
# model_type(), predict_model()

# ProTip: Use bin_continuous to bin the features. It makes it easy to detect what causes the continuous feature
# to have a high feature weight explanation

# ProTip: Usually, n_bins to tell how many bins you want. Usually 4 or 5 is sufficient to describe a continuous feature

# ProTip: Use quantile_bins to tell how to distribute observations with the bins. If True, cuts will be selected
# to evenly distribute the total observations within each of bins. 

# lime arguments : 1st argument is model. Passes model to explain the function
# 2nd set of arguments are user selections for  : bin_cotinuous, n_bins, quantile_bins, use_density

# bin_cuts : Stores the cuts for every feature that is continuous (numeric in feature_type)
# If bin_cuts = 4, bin_cuts will have 5 cuts. The bins are then the observations that fall between the cuts

explanation <- test_tbl %>%
  slice(5) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer       = explainer,
    n_labels        =  1,
    n_features      =  8,
    n_permutations  =  5000,
    kernel_width    =  1
    )

explanation

# ProTip: Use lime::explain() since explain() is a common function used in other packages. You will get errors
# if incorrect explain() function is used.

# LIME Algorithm: 
# 1. Given an observation, permute it to create replicated feature data with slight value modifications
# 2. Compute similarity distance measure between original observation and permuted observations
# 3. Apply selected machine learning model to predict outcomes of permuted data
# 4. Select m number of features to best describe predicted outcomes
# 5. Fit a simple model to permuted data, explaining the complex model outcome with m features from
# the permuted data weighted by its similarity to original observation
# 6. Use the resulting feature weights to explain local behavior


# Kernel_width : Affects the lime linear model fit (R-squared value) and therefore should be tuned
# to make sure you get the best explanations


explanation %>%
  as.tibble() %>%
  select(feature:prediction)


# feature_weight :  Magnitude indicates importance
# + / - indicates support or contradict  

#Resource : https://www.business-science.io/business/2018/06/25/lime-local-feature-interpretation.html
# Does LIME center all the features prior to fitting a linear model as well? : https://github.com/thomasp85/lime/issues/56

plot_features(explanation = explanation, ncol = 1)

# 3.3 Multiple Explanations ----

explanation <- test_tbl %>%
  slice(1:20) %>%
  select(-Attrition) %>%
  lime::explain(
    explainer       = explainer,
    n_labels        =  1,
    n_features      =  8,
    n_permutations  =  5000,
    kernel_width    =  1
  )

explanation %>%
  as.tibble()

# Messy
plot_features(explanation = explanation, ncol = 4)

# Tidy
plot_explanations(explanation)

# Challenge ---- 
# Get your custom plot_features() function to scale the multiple cases
# Use the tidyquant theme colors so plots match all of the plots we've made so far

# 4. Challenge Solution ----

# 4.1 Recreating plot_features()  ----

explanation %>%
  as.tibble()

case_1 <- explanation %>%
  filter(case == 1)

case_1 %>%
  plot_features()

library(glue)

# Transformation

data_transformed <- case_1 %>%
  as.tibble() %>%
  mutate(
    feature_desc = as_factor(feature_desc) %>%
      fct_reorder(abs(feature_weight), .desc = FALSE),
    key = ifelse( feature_weight > 0, "Supports", "Contradicts") %>%
      fct_relevel("Supports"),
    case_text = glue("Case : {case}"),
    label_text = glue("Label : {label}"),
    prob_text  = glue("Probability: {round(label_prob, 2)}"),
    r2_text    = glue("Explanation Fit : {model_r2 %>% round(2)}")
  ) %>%
  select(feature_desc, feature_weight, key, case_text:r2_text)

data_transformed

# You can use geom_col() / geom_bar(stat = 'identity') which basically sends the data as it is
# instead of summarising

data_transformed %>%
  ggplot(aes(feature_desc, feature_weight, fill = key)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_tq() +
  scale_fill_tq() +
  labs(y = "Weight" , x = "Feature") +
  facet_wrap(~ case_text + label_text + prob_text + r2_text, 
             ncol =1 , scales = "free")

plot_features_tq <- function(explanation, ncol) {
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      feature_desc = as_factor(feature_desc) %>%
        fct_reorder(abs(feature_weight), .desc = FALSE),
      key = ifelse( feature_weight > 0, "Supports", "Contradicts") %>%
        fct_relevel("Supports"),
      case_text = glue("Case : {case}"),
      label_text = glue("Label : {label}"),
      prob_text  = glue("Probability: {round(label_prob, 2)}"),
      r2_text    = glue("Explanation Fit : {model_r2 %>% round(2)}")
    ) %>%
    select(feature_desc, feature_weight, key, case_text:r2_text)
  
  
  data_transformed %>%
    ggplot(aes(feature_desc, feature_weight, fill = key)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_tq() +
    scale_fill_tq() +
    labs(y = "Weight" , x = "Feature") +
    facet_wrap(~ case_text + label_text + prob_text + r2_text, 
               ncol = ncol , scales = "free")
  
} 



explanation %>%
  filter(case %in% 1) %>%
  plot_features_tq(ncol = 2)

# BS custom plot
explanation %>%
  filter(case %in% 1:6) %>%
  plot_features_tq(ncol = 2)

#Thomas Peterson Lime plot
explanation %>%
  filter(case %in% 1:6) %>%
  plot_features(ncol = 2)

# 4.2 Recreating plot explanation ----

explanation %>%
  as.tibble()

plot_explanations(explanation)

data_transformed <- explanation %>%
  as.tibble() %>%
  mutate(
    case = as_factor(case),
   order_1 = rank(feature)
  ) %>%
  #select(case, feature, feature_value, order_1) %>%
  #arrange(order_1)
  group_by(feature) %>%
  mutate(
    order_2 = rank(feature_value)
  ) %>%
  ungroup() %>%
  #select(case, feature, feature_value, order_1, order_2) %>%
  #arrange(order_1,order_2)
  mutate(
    order = order_1 * 1000 + order_2
  ) %>%
  #select(case, feature, feature_value, order_1, order_2, order) %>%
  #arrange(order)
  mutate(
    feature_desc  = as.factor(feature_desc) %>%
      fct_reorder(order, .desc = T)
  ) %>%
  select(case, feature_desc, feature_weight, label)


data_transformed %>%
  ggplot(aes(case, feature_desc)) +
  geom_tile(aes(fill = feature_weight)) + 
  facet_wrap(~ label) +
  theme_tq() + 
  scale_fill_gradient2(low = palette_light()[[2]],
                       mid = "white", 
                       high = palette_light()[[1]]) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) + 
  labs(y = "Feature", x = "Case", 
       fill  = glue("Feature 
                    Weight"))


plot_explanations_tq <- function(explanation) {
  
  
  data_transformed <- explanation %>%
    as.tibble() %>%
    mutate(
      case = as_factor(case),
      order_1 = rank(feature)
    ) %>%
    group_by(feature) %>%
    mutate(
      order_2 = rank(feature_value)
    ) %>%
    ungroup() %>%
    mutate(
      order = order_1 * 1000 + order_2
    ) %>%
    mutate(
      feature_desc  = as.factor(feature_desc) %>%
        fct_reorder(order, .desc = T)
    ) %>%
    select(case, feature_desc, feature_weight, label)
  
  
  data_transformed %>%
    ggplot(aes(case, feature_desc)) +
    geom_tile(aes(fill = feature_weight)) + 
    facet_wrap(~ label) +
    theme_tq() + 
    scale_fill_gradient2(low = palette_light()[[2]],
                         mid = "white", 
                         high = palette_light()[[1]]) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    ) + 
    labs(y = "Feature", x = "Case", 
         fill  = glue("Feature 
                      Weight"))
  
}
  
plot_explanations(explanation)
  
plot_explanations_tq(explanation)

