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
  top_n(5) # select top 5 most important variables

# 6. Visualize most important variables

ggplot(heart_glm_importances, aes(x = predictor, y = Overall, fill = predictor)) +
  geom_col(show.legend = F, color = "black") + 
  labs(x = "Variable", y = "Estimate") +
  scale_fill_brewer(palette = 'Set3') + 
  theme_minimal()

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
varImpPlot(heart_rf)

##

# fit a random forest model using caret
heart_rf_caret <- train(as.factor(TenYearCHD) ~ ., 
                        data = train_heart, 
                        method = "ranger")
heart_rf_caret

# apply model to test set to generate predictions
heart_rf_caret_pred <- predict(heart_rf_caret, test_heart)
# compare predicted outcome and true outcome
confusionMatrix(heart_rf_caret_pred, as.factor(test_heart$TenYearCHD))


# random forest with cross-validation
set.seed(66)
heart_rf_caret_cv <- train(as.factor(TenYearCHD) ~ ., 
                        data = train_heart, 
                        method = "ranger",
                        trControl = trainControl(method = "cv", number = 5))
heart_rf_caret_cv
heart_rf_caret_cv_pred <- predict(heart_rf_caret_cv, test_heart)
confusionMatrix(heart_rf_caret_cv_pred, as.factor(test_heart$TenYearCHD))

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
varImpPlot(heart_rf_chol)

# 3. Tabulaheart_rf# 3. Tabulate values of BMI, then predict heart disease for all of them

grid_set = data.frame(sysBP = seq(80, 300, by = 10))
grid_pred = data.frame(grid_set, heart_rf_pred = predict(heart_rf, newdata = grid_set))

ggplot(data = prepped_heart_df, aes(x = sysBP, y=totChol, col = split_heart)) + 
  geom_point() +  
  geom_point(data = grid_pred, aes(x = sysBP, y=heart_rf_pred), col = 'blue', pch = 1) +
  scale_color_manual(values = c("TRUE" = "deepskyblue", "FALSE" = "orange")) + 
  theme_bw() + 
  geom_smooth(method = 'lm', col = 'magenta', se = 0) + 
  geom_smooth(method = 'lm', se = 0)

