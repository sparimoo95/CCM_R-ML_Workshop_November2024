## CCM Machine Learning in R Workshop: Supervised Learning - Logistic Regression

# 01. Logistic Regression: Heart Disease ---------------------------------------

## GOAL: predict whether someone will develop heart disease (TenYearCHD)

# 0. set seed for reproducibility
set.seed(66)

# 1. split up the dataset into a training and test set
split_heart = sample.split(prepped_heart_df$TenYearCHD, .5)

train_heart = prepped_heart_df[split_heart, ]
test_heart  = prepped_heart_df[!split_heart, ]

# 2. Build a linear model on the training set only - many ways to do this as well
heart_glm = glm(TenYearCHD ~ ., family = "binomial", data = train_heart)
summary(heart_glm)

# 3. Apply the model to the test set and generate the predictions
heart_glm_pred = predict(heart_glm, newdata = test_heart, type = "response")
heart_glm_pred

# 4. Classification accuracy
# if predicted value is greater than 0.5, then set the prediction to "1" i.e., will develop heart disease 
heart_glm_perf = ifelse(heart_glm_pred > 0.5, 1, 0)

# create confusion matrix from model's performance and compare it to the test df 
confusionMatrix(factor(heart_glm_perf), 
                factor(test_heart$TenYearCHD), 
                positive = as.character(1))

# 5 Evaluate variable importance using the varImp function from the caret package
heart_glm_importances <- varImp(heart_glm) %>%
  as.data.frame() %>% 
  rownames_to_column() %>%
  dplyr::rename(predictor = rowname) %>% 
  arrange(desc(Overall)) %>% # arrange variables according to their importance
  top_n(5) %>% # select top 5 most important variables
  # clean up the predictor naming for plotting
  mutate(predictor = as.factor(predictor))

levels(heart_glm_importances$predictor) = c("Age", "HS Education", "Blood Glucose", "Male Sex", "Systolic BP")

# 6. Visualize most important variables
ggplot(heart_glm_importances, 
       aes(x = predictor, y = Overall, fill = predictor)) +
  geom_col(show.legend = F, color = "black") + 
  labs(x = "Variable", y = "Estimate") +
  scale_fill_brewer(palette = 'Set3') + 
  theme_classic(base_size = 24) 

# 7. Look at the AUC and plot the ROC curve
heart_glm_roc = roc(test_heart$TenYearCHD, heart_glm_pred)
heart_glm_roc
plot(heart_glm_roc)

## CHALLENGE: how does the AUC change with a different training/test split?

#
# 2. Random Forest - Classification --------------------------------------------------------

set.seed(66)
heart_rf = randomForest(TenYearCHD ~ ., data = train_heart)
predict(heart_rf, newdata = test_heart)
heart_rf_pred = predict(heart_rf, newdata = test_heart, type = 'response')
head(heart_rf_pred)

# plot variable importance
varImpPlot(heart_rf, 
           main = "Variable Importance for Predicting Heart Disease", 
           bg = "brown")

##

#
# 3. Random Forest - Regression --------------------------------------------------------

## GOAL: predict cholesterol levels

# 0. set seed for reproducibility
set.seed(66)

# 1. split up the dataset into a training and test set
split_heart_rf = sample.split(prepped_heart_df$sysBP, .8)

train_heart_rf = prepped_heart_df[split_heart, ]
test_heart_rf  = prepped_heart_df[!split_heart, ]

# 2. Build a random forest model to predict cholesterol levels
heart_rf_chol = randomForest(totChol ~ ., data = train_heart_rf)
heart_rf_chol

heart_rf_chol_pred = predict(heart_rf_chol, newdata = test_heart_rf, 'response')
head(heart_rf_chol_pred)

# plot variable importance
varImpPlot(heart_rf_chol, 
           main = "Variable Importance for Predicting Cholesterol Levels", 
           bg = "brown")


