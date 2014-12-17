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


