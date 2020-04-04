#----
# Title: Graph outputs of SURF data
# Author: Ben Miller
# Date Started: 20181105
# Date updated: 20190518
# Description: Creates graphs of level by level analyses
#----

library(ggplot2)
library(tidyverse)
library(plyr)
library(Hmisc)
library(Rmisc)
library(ggpubr)

# Sets working directory and defines varibles from files
setwd("B:/Research/SURF/Data/CSV-Conversion_10.6.18/CSV_Files/master")
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")
data_master$halfhour <- as.POSIXlt(data_master$halfhour)
data_master$sens_hgt <- as.factor(data_master$sens_hgt)

# Creates easily averaged "time" column for data
data_master$time <- gsub(x = data_master$halfhour,
                         pattern = "\\d\\d\\d\\d-\\d\\d-\\d\\d ",
                         replacement = "")
data_master$time <- as.numeric(revalue(data_master$time,
                             c("00:00:00" = 0, "00:30:00" = .5,
                               "01:00:00" = 1, "01:30:00" = 1.5,
                               "02:00:00" = 2, "02:30:00" = 2.5,
                               "03:00:00" = 3, "03:30:00" = 3.5,
                               "04:00:00" = 4, "04:30:00" = 4.5,
                               "05:00:00" = 5, "05:30:00" = 5.5,
                               "06:00:00" = 6, "06:30:00" = 6.5,
                               "07:00:00" = 7, "07:30:00" = 7.5,
                               "08:00:00" = 8, "08:30:00" = 8.5,
                               "09:00:00" = 9, "09:30:00" = 9.5,
                               "10:00:00" = 10, "10:30:00" = 10.5,
                               "11:00:00" = 11, "11:30:00" = 11.5,
                               "12:00:00" = 12, "12:30:00" = 12.5,
                               "13:00:00" = 13, "13:30:00" = 13.5,
                               "14:00:00" = 14, "14:30:00" = 14.5,
                               "15:00:00" = 15, "15:30:00" = 15.5,
                               "16:00:00" = 16, "16:30:00" = 16.5,
                               "17:00:00" = 17, "17:30:00" = 17.5,
                               "18:00:00" = 18, "18:30:00" = 18.5,
                               "19:00:00" = 19, "19:30:00" = 19.5,
                               "20:00:00" = 20, "20:30:00" = 20.5,
                               "21:00:00" = 21, "21:30:00" = 21.5,
                               "22:00:00" = 22, "22:30:00" = 22.5,
                               "23:00:00" = 23, "23:30:00" = 23.5)))
str(data_master)

# Compute averages of each variable
leaf_temp_avg <- summarySE(data = data_master,
                             measurevar = "leaf_temp_c",
                             groupvars = c("time",
                                           "sens_hgt"),
                           na.rm = TRUE,
                           conf.interval = .95)

air_temp_avg <- summarySE(data = data_master,
                           measurevar = "air_temp_c",
                           groupvars = c("time",
                                         "sens_hgt"),
                           na.rm = TRUE,
                           conf.interval = .95)

ppfd_avg <- summarySE(data = data_master,
                           measurevar = "ppfd_mes",
                           groupvars = c("time",
                                         "sens_hgt"),
                           na.rm = TRUE,
                           conf.interval = .95)

vpd_avg <- summarySE(data = data_master,
                      measurevar = "vpd",
                      groupvars = c("time",
                                    "sens_hgt"),
                      na.rm = TRUE,
                      conf.interval = .95)

# Graphs leaf temperature
leaf_temp <- ggplot(data = leaf_temp_avg, aes(x = time, y = leaf_temp_c,
                                              group = sens_hgt)) +
  theme_classic() +
  theme(text = element_text(size = 20))+
  geom_ribbon(aes(ymin = leaf_temp_c - se, ymax = leaf_temp_c + se,
                  group = sens_hgt), color = rgb(red = 192,
                                                 green = 192,
                                                 blue = 192,
                                                 maxColorValue = 255),
              fill = rgb(red = 192, blue = 192, green =  192,
                         maxColorValue = 255)) +
  geom_line(aes(color = sens_hgt), na.rm = T, size = 1.5) +
  labs(x = "", y = "Leaf Temp (?C)",
       color = "Sensor \nHeight (m)") +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20),
                   labels = c("0:00", "4:00", "8:00", "12:00",
                              "16:00", "20:00")) +
  ylim(22, 31) +
  geom_hline(yintercept = 30, linetype = 2, size = 2)

# Graphs Air Temperature
air_temp <- ggplot(data = air_temp_avg, aes(x = time, y = air_temp_c,
                                            group = sens_hgt)) +
  theme_classic() +
  theme(text = element_text(size = 20))+
  geom_ribbon(aes(ymin = air_temp_c - se, ymax = air_temp_c + se,
                  group = sens_hgt), color = rgb(red = 192,
                                                 green = 192,
                                                 blue = 192,
                                                 maxColorValue = 255),
              fill = rgb(red = 192, blue = 192, green =  192,
                         maxColorValue = 255)) +
  geom_line(aes(color = sens_hgt), na.rm = T, size = 1.5) +
  labs(x = "", y = "Air Temp (?C)", color = "Sensor \nHeight (m)") +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20),
                   labels = c("0:00", "4:00", "8:00", "12:00",
                              "16:00", "20:00")) +
  ylim(22, 31)+
  geom_hline(yintercept = 30, linetype = 2, size = 2)

# Graphs PPFD

ppfd <- ggplot(data = ppfd_avg, aes(x = time, y = ppfd_mes,
                                    group = sens_hgt)) +
  theme_classic() +
  theme(text = element_text(size = 20))+
  geom_ribbon(aes(ymin = ppfd_mes - se, ymax = ppfd_mes + se,
                  group = sens_hgt), color = rgb(red = 192,
                                                 green = 192,
                                                 blue = 192,
                                                 maxColorValue = 255),
              fill = rgb(red = 192, blue = 192, green =  192,
                         maxColorValue = 255)) +
  geom_line(aes(color = sens_hgt), na.rm = T, size = 1.5) +
  labs(x = "Time", y = bquote("PPFD" ~ umol/s/m^2),
       color = "Sensor \nHeight (m)") +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20),
                   labels = c("0:00", "4:00", "8:00", "12:00", "16:00", "20:00"))

# Creates Graph For VPD

vpd <- ggplot(data = vpd_avg, aes(x = time, y = vpd,
                                  group = sens_hgt)) +
  theme_classic() +
  theme(text = element_text(size = 20))+
  geom_ribbon(aes(ymin = vpd - se, ymax = vpd + se,
                  group = sens_hgt), color = rgb(red = 192,
                                                 green = 192,
                                                 blue = 192,
                                                 maxColorValue = 255),
              fill = rgb(red = 192, blue = 192, green =  192,
                         maxColorValue = 255)) +
  geom_line(aes(color = sens_hgt), na.rm = T, size = 1.5) +
  labs(x = "Time", y = "Vapor Pressure Deficit (kPa)",
       color = "Sensor \nHeight (m)") +
  scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20),
                   labels = c("0:00", "4:00", "8:00", "12:00", "16:00", "20:00"))

# Creates grid of all graphs
ggarrange(leaf_temp, air_temp, vpd, ppfd,
          labels = c("(a)", "(b)", "(c)", "(d)"),
          legend = "right", common.legend = TRUE,
          font.label = list(size = 20, face = "bold", color = "black", family = NULL),
          hjust = -3.25, vjust = 1.5,
          widths = c(1,1), heights = c(1,1))
