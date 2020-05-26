# Program Title: ANCOVA Analysis
# Author: Benjamin D. Miller
# Date Started: 20181121
# Date Revised: 20190518
# Program Description: Performs ANCOVA Analysis for PPFD values grouped by
# Top and Middle PAR measurements. Also performs ANCOVA analysis for canopy
# leaf and air temperature grouped by Top-canopy, Understory and Mid-canopy.
# All analysis will be done using halfhourly averaged data to increase model
# resolution and resiliancy, only 6:00 to 18:00 will be used. 
# Additionally established a Delta T vector and created ancova analysis of 
# those variables

#imports libraries
library(openair)
library(tidyverse)
library(lubridate)
library(HH)
library(ggplot2)
library(ggpubr)

#Turns off current graphics
graphics.off()

# Sets working directory and defines varibles from files
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")
data_master$halfhour <- as.POSIXct(data_master$halfhour)
# Selects daytime values and applies group desitnction to new dataframe
# for temperature ANCOVA
data_master <- with(data_master,
                    data_master[hour(halfhour) >= 7 & hour(halfhour) <= 17,])
data_master$delta_t <- data_master$leaf_temp_c - data_master$air_temp_c

# Define groups of variables for ANCOVA of PPFD and height
#   Sets up mid canopy values to join to upper canopy
mid_one <- data_master[data_master$sens_hgt == "10",]
mid_two <- data_master[data_master$sens_hgt == "9",]
mid_join <- merge(x = mid_one[,c("halfhour","ppfd_mes")],
                  y = mid_two[,c("halfhour","delta_t","leaf_temp_c")],
                  by = c("halfhour"), all.x = TRUE)
mid_join$group <- "Middle"

#   Sets up the upper canopy data subset to join to the mid canopy
upper <- data_master[data_master$sens_hgt == "20", c("halfhour","ppfd_mes",
                                                     "delta_t","leaf_temp_c")]
upper$group <- "Upper"

# Binds rows of each dataset to themselves to make ppfd dataframe as 
# the ppfd values do not match in terms of height comprably.

ppfd_df <- rbind(upper, mid_join)
ppfd_df$group <- as.factor(ppfd_df$group)


# perfomes ANCOVA for the leaf temperature
mod_leaf <- ancova(leaf_temp_c ~ ppfd_mes * group, data.in = ppfd_df, groups = group)
pred_leaf <- predict(mod_leaf)

#summary(mod_leaf)
mod_leaf

# perfoms ANCOVA for Delta T values
mod_delta <- ancova(delta_t ~ ppfd_mes * group, data.in = ppfd_df, groups = group)
pred_delta <- predict(mod_delta)

#summary(mod_delta)
mod_delta

# Creates Graphs of PPFD with Leaf Temperature and ??T with seperate
# regressions. 
ppfd_delta <- ggplot(data = (ppfd_df),
                   aes(x = ppfd_mes, y = delta_t,
                       linetype = group,
                       color = group)) +
  theme_classic() +
  theme(text = element_text(size = 28))+
  labs(x = bquote("PPFD" ~ umol/s/m^2), y = expression(Delta*"T (?C)"),
       color = "Sensor Group") +
  scale_color_discrete() +
  geom_point(na.rm = TRUE, size = 3, stroke = 1.5)+
  geom_smooth(method = "lm", formula = y ~ x, show.legend = FALSE,
              color = "black", size = 2) +
  geom_hline(yintercept = 0, linetype = 3, size = 1.5)

ppfd_leaf <- ggplot(data = (ppfd_df),
                     aes(x = ppfd_mes, y = leaf_temp_c,
                         linetype = group,
                         color = group)) +
  theme_classic() +
  theme(text = element_text(size = 26))+
  labs(x = bquote("PPFD" ~ umol/s/m^2), y = "Leaf Temperature (?C)",
       color = "Sensor Group") +
  scale_color_discrete() +
  geom_point(na.rm = TRUE, size = 3, stroke = 1.5) +
  geom_smooth(method = "lm", formula = y ~ x, show.legend = FALSE,
              color = "black", size = 2)

# Arranges graphs into a two by two grid
tiff(filename = "Figure_5_PPFD_Temp_Regression.tiff",
     width = 1080, height = 720, units = "px",
     compression = "none")
ggarrange(ppfd_leaf, ppfd_delta, labels = c("(a)", "(b)"),
          legend = "bottom", common.legend = TRUE,
          font.label = list(size = 28, face = "bold", color = "black",
                            family = NULL),
          hjust = -2.5, vjust = 1,
          widths = c(1,1), heights = c(1,1))
dev.off()

#End of Script
 