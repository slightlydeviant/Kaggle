# 01_dataprep.R

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

forest_data <- read.csv("./data/train.csv", stringsAsFactors = FALSE)

## Data Structure -----
str(forest_data)

forest_data[, 12:55] <- lapply(forest_data[, 12:55], factor, levels = c(0, 1))
forest_data[, 56] <- factor(forest_data[, 56], levels = seq(1, 7), 
                            labels = c("Spruce/Fir", "Lodgepole Pine", "Ponderosa Pine",
                                       "Cottonwood/Willow", "Aspen", "Douglas-fir", "Krummholz"))

str(forest_data)


## Univariate Data Validation -----
# missing values
lapply(forest_data, function(x) {sum(is.na(x)) / length(x)}) %>%
  as.data.frame %>%
  t  # no missing values for all variables


# Elevation
hist(forest_data$Elevation)  # symmetric
summary(forest_data$Elevation)  # nothing unusual
table(forest_data$Cover_Type, cut(forest_data$Elevation, breaks = 7, labels = seq(-3, 3)))

ggplot(data = forest_data, aes(x = Elevation, color = Cover_Type, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Aspect
hist(forest_data$Aspect)  # bimodal
summary(forest_data$Aspect)  # all points in [0, 360]
# consider relabeling all 360 -> 0
# consider changing values > ~240 to a negative scale, and recentering (to normalize data and create symmetry)
forest_data$AspectCentered <- ifelse(forest_data$Aspect > 240, forest_data$Aspect - 360, forest_data$Aspect) - (240 - 360)

ggplot(data = forest_data, aes(x = AspectCentered, color = Cover_Type, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Slope
hist(forest_data$Slope)  # skew right
summary(forest_data$Slope)  # not much going on here

ggplot(data = forest_data, aes(x = Slope, color = Cover_Type)) + #, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Horizontal dist to hydrology
hist(forest_data$Horizontal_Distance_To_Hydrology)  # very skew right
hist(log(forest_data$Horizontal_Distance_To_Hydrology))  # almost uglier
summary(forest_data$Horizontal_Distance_To_Hydrology)

ggplot(data = forest_data, aes(x = Horizontal_Distance_To_Hydrology, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Vertical dist to hydrology
hist(forest_data$Vertical_Distance_To_Hydrology)
summary(forest_data$Vertical_Distance_To_Hydrology)
# relation to elevation? horizontal dist?
# Could we create a triangulated dist to hydrology?
ggplot(data = forest_data, aes(x = Vertical_Distance_To_Hydrology, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Dist to hydrology = sqrt(h^2 + v^2)
forest_data$Distance_To_Hydrology <- sqrt(forest_data$Vertical_Distance_To_Hydrology^2 + forest_data$Horizontal_Distance_To_Hydrology^2)

hist(forest_data$Distance_To_Hydrology)
summary(forest_data$Distance_To_Hydrology)

ggplot(data = forest_data, aes(x = Distance_To_Hydrology, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)

# log transform?
hist(log(forest_data$Distance_To_Hydrology + 1))
summary(log(forest_data$Distance_To_Hydrology + 1))

# Distance is zero or not
at_hydrology <- numeric(nrows(forest_data))
at_hydrology <- ifelse(forest_data$Distance_To_Hydrology == 0, 0, 1)
table(at_hydrology, forest_data$Cover_Type)
chisq.test(at_hydrology, forest_data$Cover_Type)  # significant difference between Cover_Types
forest_data$At_Hydrology <- at_hydrology


# Horizontal dist to Roadways
hist(forest_data$Horizontal_Distance_To_Roadways)  # skew right
hist(log(forest_data$Horizontal_Distance_To_Roadways))  # skew left now...
summary(forest_data$Horizontal_Distance_To_Roadways)

ggplot(data = forest_data, aes(x = Horizontal_Distance_To_Roadways, color = Cover_Type, fill = Cover_Type)) +
  geom_density(alpha = .5)
# Maybe a rural vs. wilderness distinction?

# Hillshade 9am
hist(forest_data$Hillshade_9am)  # Skew left
summary(forest_data$Hillshade_9am)  # All in [0, 255]

ggplot(data = forest_data, aes(x = Hillshade_9am, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Hillshade noon
hist(forest_data$Hillshade_Noon)  # slightly skew left
summary(forest_data$Hillshade_Noon)  # All in [0, 255]

ggplot(data = forest_data, aes(x = Hillshade_Noon, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)

# Hillshade 3pm
hist(forest_data$Hillshade_3pm)  # symmetric
summary(forest_data$Hillshade_3pm)

ggplot(data = forest_data, aes(x = Hillshade_3pm, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)
# hillshade is not very discriminating. Is there a way to combine these three metrics?

cor(select(forest_data, starts_with("Hill")))

ggplot(forest_data, aes(x = Hillshade_9am, y = Hillshade_Noon, color = Cover_Type)) + 
  geom_point() +
  scale_x_continuous(limits = c(0, 255)) +
  scale_y_continuous(limits = c(0, 255))

ggplot(forest_data, aes(x = Hillshade_9am, y = Hillshade_3pm, color = Cover_Type)) + 
  geom_point() +
  scale_x_continuous(limits = c(0, 255)) +
  scale_y_continuous(limits = c(0, 255))

ggplot(forest_data, aes(x = Hillshade_Noon, y = Hillshade_3pm, color = Cover_Type)) + 
  geom_point() +
  scale_x_continuous(limits = c(0, 255)) +
  scale_y_continuous(limits = c(0, 255))

# some zeros for Hillshade_3pm that don't seem to fit the pattern. Impute or drop or leave be?
# impute
hillreg <- select(forest_data, Id, starts_with("Hill"))
to_impute <- which(hillreg$Hillshade_3pm == 0)
hillreg_train <- hillreg[-to_impute, ]
hillreg_imp <- hillreg[to_impute, ]

hillmodel <- lm(Hillshade_3pm ~ Hillshade_9am + Hillshade_Noon, data = hillreg)  # lm probably not best option. How to improve? 
# lm seems to underestimate when looking at wilderness*soil clusters. Maybe wilderness*soil cluster mean imputation?
summary(hillmodel)  # very good model (though there is some functional relationship in residuals)

hillshade3_imp <- predict(hillmodel, hillreg_imp)
hillshade3_imp <- ifelse(hillshade3_imp < 0, 0, hillshade3_imp)
imputedhillshade3pm <- rbind(cbind("Hillshade_3pm" = hillshade3_imp, "Id" = as.numeric(to_impute)), 
                             cbind(hillreg_train$Hillshade_3pm, as.numeric(hillreg_train$Id))) %>%
  as.data.frame() %>%
  arrange(Id) %>%
  select(Hillshade_3pm)

forest_data$IMP_Hillshade_3pm <- imputedhillshade3pm[, 1]
cor(select(forest_data, starts_with("Hill"), IMP_Hillshade_3pm))  # not much difference - did not change relationships (a good thing)

# shade movement
ggplot(forest_data, aes())

# Horizontal dist to fire points
hist(forest_data$Horizontal_Distance_To_Fire_Points)  # Skew right
hist(log(forest_data$Horizontal_Distance_To_Fire_Points))  # Skew left now...
summary(forest_data$Horizontal_Distance_To_Fire_Points)

ggplot(data = forest_data, aes(x = Horizontal_Distance_To_Fire_Points, color = Cover_Type)) +  #, fill = Cover_Type)) +
  geom_density(alpha = .5)
# this looks very similar to distance to roadways and hydrology. Look at correlations and target based correlations.

# Wilderness area
# check if any levels overlap
lapply(forest_data[, 12:15], as.numeric) %>%
  as.data.frame %>%
  rowSums %>%
  table  # they do not

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

table(collapsed_forest_data$Wilderness_Area, collapsed_forest_data$Cover_Type)  # looks like a good discriminator

# Soil Type
table(collapsed_forest_data$Soil_Type, collapsed_forest_data$Cover_Type)  # looks like a good discriminator

# condense types using tree
library(rpart)
library(rpart.plot)

set.seed(12)
smalldata <- forest_data[sample(1:nrow(forest_data), 8000), ] %>%
  select(Cover_Type, contains("Soil"))
soil_tree <- rpart(Cover_Type ~ ., data = smalldata, cp = .005, parms = list(split="information"), maxdepth = 20)
prp(soil_tree, type = 0, extra = 4, under = TRUE, varlen = 0)

forest_data$Soil_Type35383940 <- with(forest_data, ifelse(Soil_Type35 == 1 | Soil_Type38 == 1 | Soil_Type39 == 1 | Soil_Type40 == 1, 1, 0))
