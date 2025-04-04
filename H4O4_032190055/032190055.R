library(tidyverse)
library(recipes)
library(visdat)
library(VIM)          # KNN imputation i??in
library(rpart)        # Tree-based imputation i??in
library(caret)        # Ekstra destek i??in

data <- read.csv("D:/ames.csv")  # Dosya ad?? sizde farkl?? olabilir

vis_miss(data) # visdat paketinden

rec_mean <- recipe(~ ., data = data) %>%
  step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_impute_mode(all_nominal(), -all_outcomes()) %>%
  prep()

data_mean <- bake(rec_mean, new_data = NULL)

data_knn <- kNN(data, k = 5)

library(mice)
imp_tree <- mice(data, method = "cart", m = 1)
data_tree <- complete(imp_tree)

# Histogram ??rne??i
ggplot(data_mean, aes(x = Gr_Liv_Area)) + 
  geom_histogram()

# Boxplot ??rne??i
ggplot(data_knn, aes(y = Sale_Price)) + 
  geom_boxplot()


ggplot(data_mean, aes(x = Gr.Liv.Area)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Gr.Liv.Area Histogram??", x = "Gr.Liv.Area", y = "Frekans")


ggplot(data_knn, aes(x = Gr.Liv.Area)) + 
  geom_histogram(bins = 30, fill = "lightgreen", color = "black")

