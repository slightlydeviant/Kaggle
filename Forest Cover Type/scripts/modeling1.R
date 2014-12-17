# modeling1.R

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)

## Input data -----
forest_data <- read.csv("./data/train.csv", stringsAsFactors = FALSE)

# forest_data[, 12:55] <- lapply(forest_data[, 12:55], factor, levels = c(0, 1))
forest_data[, 56] <- factor(forest_data[, 56], levels = seq(1, 7), 
                            labels = c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine",
                                       "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz"))
forest_data$AspectCentered <- ifelse(forest_data$Aspect > 240, forest_data$Aspect - 360, forest_data$Aspect) - (240 - 360)

forest_data <- select(forest_data, -Aspect)

collapsed_forest_data <- forest_data %>%
  gather(key = Wilderness_Area, value = true, Wilderness_Area1:Wilderness_Area4) %>%
  filter(true == 1) %>%
  mutate(Wilderness_Area = factor(Wilderness_Area, levels = c("Wilderness_Area1", "Wilderness_Area2",
                                                              "Wilderness_Area3", "Wilderness_Area4"),
                                  labels = c("Rawah", "Neota", "Comanche Peak", "Cache la Poudre"))) %>%
  gather(key = Soil_Type, value = true2, Soil_Type1:Soil_Type40) %>%
  filter(true2 == 1) %>%
  mutate(Soil_Type = factor(str_extract(Soil_Type, "[0-9]+"), levels = seq(1:40))) %>%
  select(-true, -true2)

## Data partition -----
set.seed(12)
inTrain <- createDataPartition(forest_data$Cover_Type, p = .7, list = FALSE)

## KNN model -----
knnFit <- train(form = Cover_Type ~ . - Id,
                data = forest_data,  
                subset = inTrain, 
                method = "knn",
                preProcess = c("center", "scale"))

pred_knn <- predict(knnFit, forest_data[-inTrain, ])

confusionMatrix(knnFit)
plot(knnFit)

## LDA -----
ldaFit <- train(form = Cover_Type ~ . -Id - Soil_Type7 - Soil_Type15,
                data = forest_data,  
                subset = inTrain, 
                method = "lda",
                preProcess = c("center", "scale"))

pred_lda <- predict(ldaFit, newdata = forest_data[-inTrain, ])

## Penalized Multinomial Regression -----
multinomFit <- train(form = Cover_Type ~ . -Id - Soil_Type7 - Soil_Type15,
                data = forest_data,  
                subset = inTrain, 
                method = "multinom",
                preProcess = c("center", "scale"))

pred_mult <- predict(multinomFit, forest_data[-inTrain, ])

## Decision Tree -----
cart_tune <- expand.grid(cp = seq(.0001, .001, .0001))
cartFit <- train(form = Cover_Type ~ . - Id,
                 data = forest_data,  
                 subset = inTrain, 
                 method = "rpart", minsplit = 5,
                 preProcess = c("center", "scale"),
                 tuneGrid = cart_tune)

pred_cart <- predict(cartFit, forest_data[-inTrain, ])

## Ensemble by Voting -----
pred_mat <- cbind(pred_knn, pred_lda, pred_mult, pred_cart)
pred_vote <- apply(pred_mat, 1, function(x) { names(which.max(table(x))) })

pred_vote <- factor(pred_vote, levels = seq(1, 7), 
                    labels = c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine",
                               "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz"))

confusion_vote <- table(forest_data[-inTrain, "Cover_Type"], pred_vote)
confusion_vote

sum(diag(confusion_vote)) / sum(confusion_vote)



