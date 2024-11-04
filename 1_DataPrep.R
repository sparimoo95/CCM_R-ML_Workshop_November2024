## CCM Machine Learning in R Workshop: Data Preparation

setwd("/Users/shireenparimoo/Documents/Teaching/R Workshop - November 2024/data/")

#
## 00. Load libraries ----------------------------------------------------

# install.packages("tidyverse", "caret", "pROC", "caTools", "randomForest", "kohonen", "recipes", "tidymodels", "factoextra", "rpart.plot")
library(tidyverse) # we will use this library, which comes with its own syntax
library(pROC) # plot ROC curves and calculate area under the curve (AUC)
library(caret) # specify models
library(caTools) # split dataset into training and test sets 
library(e1071) # svm
library(randomForest) # random forest model
library(tidymodels)
library(kohonen) # wine dataset
library(factoextra) # k means viz
library(corrplot) # correlation plot
library(rpart.plot) # random forest plots
# library(recipes) # preprocess data for clustering

#
## 01. Import data ------------------------------------------------------

raw_heart_df <- read.csv("framingham.csv", header = TRUE, sep = ",")
# raw_beer_df <- read.csv("beer_profile_and_ratings.csv", header = TRUE, sep = ",")
# data(wines)
# raw_wines_df <- as.data.frame(wines) # features
# wine_classes_df <- as.data.frame(vintages) # labels

#

# 02. Prepare and explore the data ----------------------------------------------------

# Framingham Heart Dataset ------------------------------------------------

str(raw_heart_df) # prints out the structure of the dataframe

# prepare the dataset for logistic regression
prepped_heart_df <- raw_heart_df %>%  # this is a pipe; it allows you to perform a sequence of actions on a single object
  # now let's first change some of the variables to factors and numerics
  # the mutate_at function will take a vector of the column names you want to change to factor 
  # and apply the `factor` function to those variables in the `prepped_df_tidy` dataframe
  mutate_at(c("male", "education", "currentSmoker", "BPMeds", "prevalentStroke", "prevalentHyp", "diabetes", "TenYearCHD"), 
            as.factor) %>% 
  mutate_at(c("age", "cigsPerDay", "totChol", "sysBP", "diaBP", "heartRate", "glucose"),
            as.numeric) %>% 
  # let's also change the values in the male and education columns to make them more descriptive
  # and convert them to factors all at once
  mutate(sex = as.factor(ifelse(male == 1, "M", "F")),                              
         education = as.factor(case_when(education == 1 ~ "< High School",
                                         education == 2 ~ "High School Graduate",
                                         education == 3 ~ "Some College",
                                         education == 4 ~ "College Graduate"))) %>% 
  # remove rows with missing values
  na.omit() %>% 
  # remove redundant column "male"
  select(-male)

str(prepped_heart_df)

# prepare the dataset for clustering i.e., only keep numeric/continuous variables
prepped_heart_df_cluster <- prepped_heart_df %>% 
  select(c("age", "cigsPerDay", "totChol", "sysBP", "diaBP", "heartRate", "glucose")) %>% 
  # scale and center the numeric variables; contrast code the categorical variables
  scale() %>%
  # convert to data frame
  as.data.frame() %>% 
  # remove rows with NAs
  na.omit()

# look at correlations within the clustering dataset
corrplot(cor(prepped_heart_df_cluster))

#

# # Wines Dataset ------------------------------------------------
# 
# # combine the wine and vintages datasets and do some renaming
# prepped_wines_df <- cbind(raw_wines_df, vintages) %>% 
#   # rename some of the columns so there are no spaces in the names
#   dplyr::rename(malic_acid = `malic acid`,
#                 ash_alkalinity = `ash alkalinity`,
#                 total_phenols = `tot. phenols`,
#                 non_flav_phenols = `non-flav. phenols`,
#                 color_intensity = `col. int.`,
#                 color_hue = `col. hue`) 
# 
# # normalize the wines dataset for clustering (after removing the vintages column)
# norm_wines_df <- as.data.frame(scale(prepped_wines_df %>% select(-vintages)))
# 
# # look at correlations in the clustering dataset
# corrplot(cor(norm_wines_df))
# 
# #
