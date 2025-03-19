# "mtcars" Veri Setinde Basit Rastgele B??lme
# Gerekli paket
install.packages("rsample")
library(rsample)

# Reproducible sonu??lar i??in
set.seed(123)

# mtcars veri seti
data("mtcars")

# Veriyi %60 e??itim ve %40 test olarak b??lme
split <- initial_split(mtcars, prop = 0.6)
train_data <- training(split)
test_data <- testing(split)

# E??itim ve test setlerindeki "mpg" ortalamalar??
mean_train <- mean(train_data$mpg)
mean_test <- mean(test_data$mpg)

# Sonu??lar?? raporlama
cat("E??itim setindeki MPG ortalamas??:", mean_train, "\n")
cat("Test setindeki MPG ortalamas??:", mean_test, "\n")


# "PimaIndiansDiabetes" Veri Setinde Stratified Sampling
# Gerekli paketler
install.packages("mlbench")
install.packages("rsample")
library(mlbench)
library(rsample)

# PimaIndiansDiabetes veri setini y??kle
data("PimaIndiansDiabetes")

# Veriyi inceleyelim
str(PimaIndiansDiabetes)

# Reproducible sonu??lar i??in
set.seed(123)

# Stratified sampling ile veri b??lme (%70 e??itim, %30 test)
split <- initial_split(PimaIndiansDiabetes, prop = 0.7, strata = "diabetes")
train_data <- training(split)
test_data <- testing(split)

# Orijinal veri setindeki diabetes da????l??m??
orig_dist <- prop.table(table(PimaIndiansDiabetes$diabetes))

# E??itim setindeki diabetes da????l??m??
train_dist <- prop.table(table(train_data$diabetes))

# Test setindeki diabetes da????l??m??
test_dist <- prop.table(table(test_data$diabetes))

# Sonu??lar?? raporlama
cat("Orijinal Veri Seti Da????l??m??:\n")
print(orig_dist)
cat("\nE??itim Seti Da????l??m??:\n")
print(train_dist)
cat("\nTest Seti Da????l??m??:\n")
print(test_dist)


# "diamonds" Veri Setinde Cross Validation ve RMSE Hesab??
# Gerekli paketler
install.packages("ggplot2")
install.packages("rsample")
install.packages("yardstick")
install.packages("dplyr")
install.packages("parsnip")

library(ggplot2)
library(rsample)
library(yardstick)
library(dplyr)
library(parsnip)

# diamonds veri setini y??kle
data("diamonds")

# Reproducible sonu??lar i??in
set.seed(123)

# 5 katl?? cross validation ayar??
cv_folds <- vfold_cv(diamonds, v = 5)

# Her kat i??in RMSE de??erlerini tutacak bir vekt??r
rmse_values <- c()

# Cross validation d??ng??s??
for (i in seq_along(cv_folds$splits)) {
  # E??itim ve test verilerini ay??rma
  train_data <- training(cv_folds$splits[[i]])
  test_data <- testing(cv_folds$splits[[i]])
  
  # Lineer regresyon modeli olu??turma
  model <- linear_reg() %>%
    set_engine("lm") %>%
    fit(price ~ carat + depth + table + x + y + z, data = train_data)
  
  # Test setinde tahmin yapma
  predictions <- predict(model, test_data)$.pred
  
  # Test setindeki ger??ek de??erleri ????karma
  actuals <- test_data$price
  
  # RMSE hesaplama
  rmse_value <- rmse_vec(actuals, predictions)
  
  # RMSE de??erini saklama
  rmse_values <- c(rmse_values, rmse_value)
}

# Ortalama RMSE hesaplama
average_rmse <- mean(rmse_values)

# Sonu??lar?? raporlama
cat("Her Kat??n RMSE De??erleri:\n")
print(rmse_values)
cat("\nOrtalama RMSE De??eri:", average_rmse, "\n")

