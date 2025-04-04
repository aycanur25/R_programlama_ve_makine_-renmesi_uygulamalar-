# Gerekli paketleri y??kle
library(tidyverse)
library(broom)
library(ggplot2)

# Veriyi oku
ames_train <- read.csv("D:/ames.csv")

# ??lk birka?? sat??r?? g??zlemle
head(ames_train)

model1 <- lm(SalePrice ~ Gr.Liv.Area, data = ames_train)

summary(model1)

# Veriye art??k ve tahmin kolonlar??n?? ekle
model1_aug <- augment(model1, data = ames_train)

# Regresyon do??rusu + scatter plot
ggplot(model1_aug, aes(x = Gr.Liv.Area, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  labs(title = "Regresyon Do??rusu: Sale Price ~ Gr Liv Area")

# Art??klar??n da????l??m??
ggplot(model1_aug, aes(x = Gr.Liv.Area, y = .resid)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Art??klar??n Da????l??m??", y = "Art??klar", x = "Gr Liv Area")
