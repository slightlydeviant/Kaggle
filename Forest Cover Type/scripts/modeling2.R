# modeling2.R

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(nnet)
library(rpart)
library(rpart.plot)

## Input data -----
forest_data <- read.csv("./data/train.csv", stringsAsFactors = FALSE)

# forest_data[, 12:55] <- lapply(forest_data[, 12:55], factor, levels = c(0, 1))
forest_data[, 56] <- factor(forest_data[, 56], levels = seq(1, 7), 
                            labels = c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine",
                                       "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz"))
forest_data$AspectCentered <- ifelse(forest_data$Aspect > 240, forest_data$Aspect - 360, forest_data$Aspect) - (240 - 360)
forest_data$Distance_To_Hydrology <- sqrt(forest_data$Vertical_Distance_To_Hydrology^2 + forest_data$Horizontal_Distance_To_Hydrology^2)
forest_data$Soil_Type35383940 <- with(forest_data, ifelse(Soil_Type35 == 1 | Soil_Type38 == 1 | Soil_Type39 == 1 | Soil_Type40 == 1, 1, 0))

forest_data <- select(forest_data, -Aspect, -Soil_Type1, -Soil_Type2, -(Soil_Type4:Soil_Type9), -(Soil_Type11:Soil_Type40))

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

## Models for wilderness area 1 -----
data_w1 <- filter(collapsed_forest_data, Wilderness_Area == "Rawah")

# linear model
data_w1_model <- data_w1[, c(2:3, 5, 6, 10, 11:13, 15)]
lm1 <- multinom(Cover_Type ~ ., data = data_w1_model, maxit = 500)

lm1tab <- table(as.character(data_w1_model$Cover_Type), as.character(predict(lm1, data_w1_model[,-6])))
sum(diag(lm1tab)) / sum(lm1tab)

## Models for wilderness area 2 -----
data_w2 <- filter(collapsed_forest_data, Wilderness_Area == "Neota")

# linear model
data_w2_model <- data_w2[, c(2:3, 5, 6, 10, 11:13, 15)]
lm2 <- multinom(Cover_Type ~ ., data = data_w2_model)

lm2tab <- table(as.character(data_w2_model$Cover_Type), as.character(predict(lm2, data_w2_model[,-6])))
sum(diag(lm2tab)) / sum(lm2tab)

## Models for wilderness area 3 -----
data_w3 <- filter(collapsed_forest_data, Wilderness_Area == "Comanche Peak")

# linear model
data_w3_model <- data_w3[, c(2:3, 5, 6, 10, 11:13, 15)]
lm3 <- multinom(Cover_Type ~ ., data = data_w3_model, maxit = 500)

lm3tab <- table(as.character(data_w3_model$Cover_Type), as.character(predict(lm3, data_w3_model[,-6])))
sum(diag(lm3tab)) / sum(lm3tab)

## Models for wilderness area 4 -----
data_w4 <- filter(collapsed_forest_data, Wilderness_Area == "Cache la Poudre")

# linear model
data_w4_model <- data_w4[, c(2:3, 5, 6, 10, 11:13, 15)]
lm4 <- multinom(Cover_Type ~ ., data = data_w4_model, maxit = 500)

lm4tab <- table(as.character(data_w4_model$Cover_Type), as.character(predict(lm4, data_w4_model[,-6])))
sum(diag(lm4tab)) / sum(lm4tab)

## Models for all areas -----
# multinomial logistic regression
data_all_model <- collapsed_forest_data[, c(2:3, 5, 6, 10, 11:15)]
lmall <- multinom(Cover_Type ~ ., data = data_all_model, maxit = 500)

lmalltab <- table(as.character(data_all_model$Cover_Type), as.character(predict(lmall, data_all_model[,-6])))
sum(diag(lmalltab)) / sum(lmalltab)

# single classification tree
loss_mat <- matrix(1, nrow = 7, ncol = 7)
diag(loss_mat) <- 0
loss_mat[1, 2] <- loss_mat[2, 1] <- .1  # Lodgepole & Spruce/Fir
loss_mat[5, 2] <- loss_mat[2, 5] <- .1  # Lodgepole & Aspen
loss_mat[6, 3] <- loss_mat[3, 6] <- .1  # Ponderosa & Douglas-Fir
trall <- train(form = Cover_Type ~ . - Id,
               data = forest_data,  
               method = "rpart2",
               tuneGrid = expand.grid(maxdepth = seq(16)),
               parms = list(split="information", loss = loss_mat))
print(trall)
prp(trall$finalModel, type = 0, extra = 4, under = TRUE, varlen = 0)
# Lodgepole & Aspen
# Ponderosa & Douglas-Fir
# Lodgepole & Spruce/Fir

# quadratic discriminant analysis
qdall <- train(form = Cover_Type ~ . - Id,
               data = forest_data,  
               method = "stepQDA", improvement = .01, fold = 3,
               tuneGrid = expand.grid(direction = "forward", maxvar = Inf))
print(qdall)

## Ensemble by Voting -----
true_cover <- collapsed_forest_data$Cover_Type
multinom_out <- predict(lmall, data_all_model[,-6])
rpart_out <- predict(trall, collapsed_forest_data[, -11])
qda_out <- predict(qdall, forest_data[, -55])

pred_mat <- cbind(multinom_out, rpart_out, qda_out)
pred_vote <- apply(pred_mat, 1, function(x) { names(which.max(table(x))) })

pred_vote <- factor(pred_vote, levels = seq(1, 7), 
                    labels = c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine",
                               "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz"))

confusion_vote <- table(true_cover, pred_vote)
confusion_vote

sum(diag(confusion_vote)) / sum(confusion_vote)
