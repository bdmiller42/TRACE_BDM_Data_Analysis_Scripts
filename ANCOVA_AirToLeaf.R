# Program Title: ANCOVA Analysis of leaf and air temperature
# Author: Benjamin D. Miller
# Date Started: Nov/21/2018
# Date Revised: May/30/2019
# Program Description:

#imports libraries
library(openair)
library(tidyverse)
library(HH)
library(ggplot2)
library(ggpubr)
library(lsmeans)
library(lubridate)

# Sets working directory and defines varibles from files
setwd("B:/Research/SURF/Data/CSV-Conversion_10.6.18/CSV_Files/master")
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")
data_master$halfhour <- as.POSIXct(data_master$halfhour)
# Selects daytime values and applies group desitnction to new dataframe
# for temperature ANCOVA
data_master <- with(data_master,
                    data_master[hour(halfhour) >= 7 & hour(halfhour) <= 17,])

data_master$date <- floor_date(data_master$halfhour, "1 day")

data_master$group <- as.factor(ifelse(data_master$sens_hgt == 20, "Upper",
                                      ifelse(data_master$sens_hgt == 2,
                                             "Understory",
                                             "Middle")))

temp_df <- aggregate(cbind(leaf_temp_c, air_temp_c) ~ date + group,
                     data = data_master,
                     FUN = mean,
                     na.rm = TRUE,
                     na.action = NULL)

# changes the factor of the groups so that the legend will be in the proper order in the
# legend of plot temp_temp
temp_df$group <- factor(temp_df$group, levels = c("Upper", "Middle", "Understory"))

# performs ANCOVA for leaf and air temperature values
mod_temp <- ancova(leaf_temp_c ~ air_temp_c * group, data.in = temp_df, groups = group)
pred_temp <- predict(mod_temp)

# Creates plot for model
temp_temp <- ggplot(data = (temp_df),
                    aes(x = air_temp_c, y = leaf_temp_c,
                        pch = group, linetype = group)) +
  theme_classic() +
  theme(text = element_text(size = 30))+
  labs(x = "Air Temperature (C)", y = "Leaf Temperature (C)",
       pch = "Sensor Group") +
  scale_shape_manual(values = c(3, 2, 1)) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = FALSE,
              color = "black", size = 1.5) +
  geom_point(na.rm = TRUE, size = 3, stroke = 2)+
  geom_abline(slope = 1, intercept = 0, linetype = 2, size = 2,
              color = 'red')

# Prints temp graph and associated models.
temp_temp

summary(mod_temp)

# Performs Post-Hoc analysis on the differnt graphs
temp.lst <- lstrends(mod_temp, ~group, var = "air_temp_c")

pairs(temp.lst)