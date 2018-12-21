# Grid Search  & CV ----

deeplearning_h2o <- h2o.loadModel("04_Modeling/h20_models/DeepLearning_0_AutoML_20181202_142146")

# This gives performance on training data
deeplearning_h2o


# Lets look at performance of test data (AUC = 0.84 implies 84 percent accuracy)
test_tbl
h2o.performance(deeplearning_h2o, newdata = as.h2o(test_tbl))

# Grid Search 
# Two Methods : 
# 1. Cartesian Grid Search
# 2. Random Grid Search

# Resource : https://www.h2o.ai/blog/h2o-gbm-tuning-tutorial-for-r/

#?h2o.grid()
# Deep learning models are highly tunable
# deeplearning_h2o@allparameters (epochs, hidden) can be a part of hyperparams

# Cartesian Grid Search: Generate combinations of hyperparameters. If one hyperparameter contains
# 3 states and another contains 3 states, then a total of 3 x 3 = 9  models will be tested


deeplearning_grid_01 <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "deeplearning_grid_01",
  
  #h2o.deeplearning()
  x = x,
  y = y,
  training_frame = train_h2o,
  validation_frame = valid_h2o,
  nfolds = 5,
  hyper_params = list(
    hidden = list( c(10, 10, 10), c(50, 20, 10), c(20, 20, 20) ),
    epochs = c(10, 50, 100) 
  )
)

# Ordered on logloss
deeplearning_grid_01

h2o.getGrid(grid_id = "deeplearning_grid_01" , sort_by = "auc", decreasing = T)

deeplearning_grid_01_model_1 <- h2o.getModel("deeplearning_grid_01_model_0")

# Overfitting : we can tell model is overfitting because of huge difference between training AUC
# and validation / cross validation AUC

deeplearning_grid_01_model_1 %>% 
  h2o.auc(train = T, valid = T, xval = T)

# ProTip: Grid Search can help with highly tunable models (e.g GBM, Deep learning)
# We just decreased accuracy on test set from 92% to 87%  (AUC 89% to 86%)

deeplearning_grid_01_model_1 %>%
  h2o.performance(newdata = as.h2o(test_tbl))

#ProTip: However, further improvements should be taken to make sure overfitting is not an issue. 
# We want model that generalize to new data





  