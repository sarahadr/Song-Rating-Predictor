# Song-Rating-Predictor
R Script of the process underwent in testing out different models to predict a song's rating. Best model was determined be the lowest RMSE score. 
Created a boosted tree predictor for song ratings based on a 5-time cross-validated training set after developing the base model from a simple linear regression.
Utilized packages including tidyverse, dplyr, gbm, rpart, caret, and lubridate in the process of creating the final model. Performed feature selection functions to determine which to include including hybrid stepwise function and lasso regression. 
Transformed variables (primarily non-numeric) in order for them to be compatible with the model. Best model was chosen based on the lowest root mean squared error generated from the validation dataset. 
