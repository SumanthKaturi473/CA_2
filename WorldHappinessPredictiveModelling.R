library(rio)
library(RCurl)
library(bitops)
library(dplyr)
library(tidyverse)
library(DataExplorer)
library(tidymodels)
library(tidyverse)
library(rsample)
library(recipes)

# Loading Dataset(World-happiness.csv)
happiness<- read.csv("World-happiness.csv")
str(happiness)
head(happiness)

# Rename the country column name
names(happiness)[names(happiness) == "ï..Country.name"] <- "Country.name"

# Remove NA values
happiness_df <- happiness %>% drop_na(Country.name,year,Life.Ladder,
                                      Log.GDP.per.capita,Social.support,Healthy.life.expectancy.at.birth,
                                      Freedom.to.make.life.choices,Generosity,Perceptions.of.corruption,
                                      Positive.affect,Negative.affect)

# Exploratory data analysis
## Analysis of Positive affect 
summary(happiness_df$Positive.affect)
mean(happiness_df$Positive.affect)
var(happiness_df$Positive.affect)
median(happiness_df$Positive.affect)
sd(happiness_df$Positive.affect)
plot(happiness_df$Positive.affect, main = "Positive Affect by Frequency", xlab = "Frequency", ylab = "Positive Affect")

## Analysis of Negative affect
summary(happiness_df$Negative.affect)
mean(happiness_df$Negative.affect)
var(happiness_df$Negative.affect)
median(happiness_df$Negative.affect)
sd(happiness_df$Negative.affect)
plot(happiness_df$Negative.affect, main = "Positive Affect by Frequency", xlab = "Frequency", ylab = "Positive Affect")

## Boxplot of Positive and Negative affect by Year
boxplot(happiness_df$Positive.affect ~ happiness_df$year, xlab = "Year", ylab = "Positive Affect", main = "Boxplot of Positive Affect by Year")
boxplot(happiness_df$Negative.affect ~ happiness_df$year, xlab = "Year", ylab = "Positive Affect", main = "Boxplot of Negative Affect by Year")

## Correlation of Positive and Negative affect with Log GDP per capita 
cor(happiness_df$Positive.affect, happiness_df$Log.GDP.per.capita)
cor(happiness_df$Negative.affect, happiness_df$Log.GDP.per.capita)

## Correlation Plot
plot_correlation(happiness_df,'continuous')

# Predictive Modeling
set.seed(317)
## Split the data into training and testing sets
happiness_split <- initial_split(happiness_df, prop = 0.8)
train <- training(happiness_split)[, 1:2]
test <- testing(happiness_split)[, 1:2]

## Training set
happiness_split %>%
  training() %>%
  glimpse(width = 80)

## Testing set
happiness_split %>%
  testing() %>%
  glimpse(width = 80)

## Pre-process the data for modeling
happiness_recipe <- training(happiness_split) %>%
  select(-Country.name, -year) %>%
  recipe(Positive.affect ~.) %>%
  step_corr(all_predictors()) %>%
  step_nzv(all_predictors()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep()
happiness_recipe

## Pre-process the testing set
happiness_testing <- happiness_recipe %>%
  bake(testing(happiness_split)) 

glimpse(happiness_testing, width = 80)

## Averages on testing set
sapply(happiness_testing, mean)

## Averages on training set
sapply(juice(happiness_recipe), mean)

happiness_training <- juice(happiness_recipe)

## Applying Linear Regression model to predict Positive affect score
model <- lm(Positive.affect ~ ., data = happiness_training)
summary(model)

plot(happiness_training$Positive.affect ~ happiness_training$Life.Ladder, xlab = "Life Ladder", ylab = "Positive affect", main = "Scatterplot with a LSR Line for Life Ladder")
abline(model)

plot(happiness_training$Positive.affect ~ happiness_training$Log.GDP.per.capita, xlab = "Log GDP per capita", ylab = "Positive affect", main = "Scatterplot with a LSR Line for Log GDP per capita")
abline(model)

# Model Validation
pred_test_lm_metrics <- model %>% 
  metrics(truth = actual, estimate = prediction) %>%
  mutate(model = "LM") %>%
  select(model, everything())

pred_test_lm_metrics

# Model forecasting
pa.pred <- predict(model, happiness_testing, interval = "confidence")
preds_data <- cbind(happiness_testing, pa.pred)

library("ggplot2")
p <- ggplot(preds_data, aes(Positive.affect, Life.Ladder)) +
  geom_point() +
  stat_smooth(method = lm)
p 

p1 <- ggplot(preds_data, aes(Positive.affect, Log.GDP.per.capita)) +
  geom_point() +
  stat_smooth(method = lm)
p1 