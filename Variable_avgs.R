# Program Title: ANOVA Analysis for daily averages.
# Author: Benjamin D. Miller
# Date Started: 20181121
# Date Revised: 20190530
# Program Description: Creates barplots with standard error of the groups
# and perfoms anova analysis on each variable. Also does TukeyHSD analysis
# and adds characters to the graph based on the results. The tukey results
# do not auto update, they are added manually to the figure.

# Imports libraries
library(openair)
library(tidyverse)
library(HH)
library(lubridate)
library(Rmisc)
library(ggplot2)
library(ggpubr)

#Turns off current graphics
graphics.off()

# Sets working directory and defines varibles from files
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")
data_master$halfhour <- as.POSIXct(data_master$halfhour)

# Selects daytime values, establishes a "date" class, calculates T 
# and avereages values over the course of the day

data_master <- with(data_master,
                    data_master[hour(halfhour) >= 7 & hour(halfhour) <= 17,])
data_master$delta_t <- data_master$leaf_temp_c - data_master$air_temp_c

data_master$sens_hgt <- as.factor(data_master$sens_hgt)

data_master$date <- floor_date(data_master$halfhour, "1 day")

# Creates a daily Maximum dataframe based on sensor height
daily_max <- aggregate(cbind(leaf_temp_c, air_temp_c,
                             vpd, ppfd_mes) ~ date + sens_hgt,
                       data = data_master,
                       FUN = max,
                       na.action = NULL)

daily_max$group <- as.factor(ifelse(daily_max$sens_hgt == 20, "Upper",
                                    ifelse(daily_max$sens_hgt == 2,
                                           "Understory",
                                           "Middle")))

# Sets a group based on sensor height
data_master$group <- as.factor(ifelse(data_master$sens_hgt == 20, "Upper",
                                     ifelse(data_master$sens_hgt == 2,
                                            "Understory",
                                            "Middle")))

daily_mean <- aggregate(cbind(leaf_temp_c, air_temp_c,
                              vpd, ppfd_mes) ~ date + group,
                        data = data_master,
                        FUN = mean,
                        na.rm = TRUE,
                        na.action = NULL)

# Calculates daily means for each variable for graphing purposes
leaf_day_avg <- summarySE(data = daily_mean,
                           measurevar = "leaf_temp_c",
                           groupvars = c("group"),
                           na.rm = TRUE,
                           conf.interval = .95)

air_day_avg <- summarySE(data = daily_mean,
                          measurevar = "air_temp_c",
                          groupvars = c("group"),
                          na.rm = TRUE,
                          conf.interval = .95)

ppfd_day_avg <- summarySE(data = daily_mean,
                      measurevar = "ppfd_mes",
                      groupvars = c("group"),
                      na.rm = TRUE,
                      conf.interval = .95)

vpd_day_avg <- summarySE(data = daily_mean,
                     measurevar = "vpd",
                     groupvars = c("group"),
                     na.rm = TRUE,
                     conf.interval = .95)

# Calculates means of daily maximum values
# for each variable for graphing purposes
leaf_day_max <- summarySE(data = daily_max,
                          measurevar = "leaf_temp_c",
                          groupvars = c("group"),
                          na.rm = TRUE,
                          conf.interval = .95)

air_day_max <- summarySE(data = daily_max,
                         measurevar = "air_temp_c",
                         groupvars = c("group"),
                         na.rm = TRUE,
                         conf.interval = .95)

ppfd_day_max <- summarySE(data = daily_max,
                          measurevar = "ppfd_mes",
                          groupvars = c("group"),
                          na.rm = TRUE,
                          conf.interval = .95)

vpd_day_max <- summarySE(data = daily_max,
                         measurevar = "vpd",
                         groupvars = c("group"),
                         na.rm = TRUE,
                         conf.interval = .95)

# Stores all data into a list and applies a factor to each group for
# display purposes
df.list <- list(leaf_day_avg, air_day_avg, ppfd_day_avg, vpd_day_avg,
                leaf_day_max, air_day_max, ppfd_day_max, vpd_day_max)

names(df.list) <- c("leaf_day_avg", "air_day_avg", "ppfd_day_avg","vpd_day_avg",
                    "leaf_day_max", "air_day_max", "ppfd_day_max","vpd_day_max")

sortfun <- function(x) {
  x[,"sort"] <- ifelse(x[,"group"] == "Understory",3,
            ifelse(x[,"group"] == "Middle", 2, 1))
  return(x)
}

df.list <- sapply(df.list[1:8], "sortfun",
           simplify = FALSE,
           USE.NAMES = TRUE)

# Anova analysis of variable groups
leafmean.aov <- aov(formula = leaf_temp_c ~ group, data = daily_mean)
airmean.aov <- aov(air_temp_c ~ group, data = daily_mean)
ppfdmean.aov <- aov(ppfd_mes ~ group, data = daily_mean)
vpdmean.aov <- aov(vpd ~ group, data = daily_mean)
# Maximums
leafmax.aov <- aov(formula = leaf_temp_c ~ group, data = daily_max)
airmax.aov <- aov(air_temp_c ~ group, data = daily_max)
ppfdmax.aov <- aov(ppfd_mes ~ group, data = daily_max)
vpdmax.aov <- aov(vpd ~ group, data = daily_max)

# TukeyHSD analysis of ANOVA results
leafmean.HSD <- TukeyHSD(leafmean.aov, "group", conf.level = 0.95)
airmean.HSD <- TukeyHSD(airmean.aov, "group", conf.level = 0.95)
ppfdmean.HSD <- TukeyHSD(ppfdmean.aov, "group", conf.level = 0.95)
vpdmean.HSD <- TukeyHSD(vpdmean.aov, "group", conf.level = 0.95)

# max values
leafmax.HSD <- TukeyHSD(leafmax.aov, "group", conf.level = 0.95)
airmax.HSD <- TukeyHSD(airmax.aov, "group", conf.level = 0.95)
ppfdmax.HSD <- TukeyHSD(ppfdmax.aov, "group", conf.level = 0.95)
vpdmax.HSD <- TukeyHSD(vpdmax.aov, "group", conf.level = 0.95)

# Creates Bar charts for the different catagories of daily means
leaf_day_plot <- ggplot(df.list$leaf_day_avg, aes(x = reorder(group, -sort),
                                             y = leaf_temp_c,
                                             fill = group)) +
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(data = df.list$leaf_day_avg,
                aes(ymin = leaf_temp_c - se, ymax = leaf_temp_c + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(26.75, 27.1, 28.75),
                 pch = c(65, 65, 66),
                 stroke = 8)) +
  coord_flip(ylim = c(25,29)) +
  labs(y = "Daily Mean Leaf Temp (°C)", x = "") +
  theme_classic() +
  theme(text = element_text(size = 20))+
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

air_day_plot <- ggplot(df.list$air_day_avg, aes(x = reorder(group, -sort),
                                           y = air_temp_c,
                                           fill = group)) +
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(data = df.list$air_day_avg,
                aes(ymin = air_temp_c - se, ymax = air_temp_c + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(27, 27.25, 28.15),
                 pch = c(65, 65, 66),
                 stroke = 8)) +
  coord_flip(ylim = c(25,29)) +
  labs(y = " Daily Mean Air Temp (°C)", x = "") +
  theme_classic() +
  theme(text = element_text(size = 20))+
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

ppfd_day_plot <- ggplot(df.list$ppfd_day_avg, aes(x = reorder(group, -sort),
                                             y = ppfd_mes,
                                             fill = group),
                   na.rm = TRUE) +
  geom_bar(color = "black",stat = "identity") +
  geom_errorbar(data = df.list$ppfd_day_avg,
                aes(ymin = ppfd_mes - se, ymax = ppfd_mes + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(0, 180, 1000),
                 pch = c(26, 65, 66),
                 stroke = 8)) +
  coord_flip() +
  labs(y = bquote("Daily Mean PPFD" ~ umol/s/m^2), x = "") +
  theme_classic() +
  theme(text = element_text(size = 20))+
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

vpd_day_plot <- ggplot(df.list$vpd_day_avg, aes(x = reorder(group, -sort),
                                           y = vpd,
                                           fill = group)) +
  geom_bar(color = "black",stat = "identity") +
  geom_errorbar(data = df.list$vpd_day_avg,
                aes(ymin = vpd - se, ymax = vpd + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(.450, .625, .750),
                 pch = c(65, 66, 67),
                 stroke = 8)) +
  coord_flip() +
  labs(y = "Daily Mean \nVapor Pressure Deficit (kPa)", x = "") +
  theme_classic() +
  theme(text = element_text(size = 20))+
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

# Creates Bar charts for the different catagories of maximum values
leaf_max_plot <- ggplot(df.list$leaf_day_max, aes(x = reorder(group, -sort),
                                                 y = leaf_temp_c,
                                                 fill = group)) +
  geom_bar(color = "black",stat = "identity") +
  geom_errorbar(data = df.list$leaf_day_max,
                aes(ymin = leaf_temp_c - se, ymax = leaf_temp_c + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(28.6, 28.6, 32.5),
                 pch = c(65, 65, 66),
                 stroke = 8)) +
  geom_hline(yintercept = 30, linetype = 2, size = 2) +
  coord_flip(ylim = c(25,33)) +
  labs(y = "Daily Max Leaf Temp (°C)", x = "") +
  theme_classic() +
  theme(text = element_text(size = 20),axis.text.y = element_text(color = "white",
                                                                  size = 10)) +
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

air_max_plot <- ggplot(df.list$air_day_max, aes(x = reorder(group, -sort),
                                               y = air_temp_c,
                                               fill = group)) +
  geom_bar(color = "black",stat = "identity") +
  geom_errorbar(data = df.list$air_day_max,
                aes(ymin = air_temp_c - se, ymax = air_temp_c + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(28.4, 28.75, 29.6),
                 pch = c(65, 66, 67),
                 stroke = 8)) +
  geom_hline(yintercept = 30, linetype = 2, size = 2) +
  coord_flip(ylim = c(25,33)) +
  labs(y = "Daily Max Air Temp (°C)", x = "") +
  theme_classic() +
  theme(text = element_text(size = 20),axis.text.y = element_text(color = "white",
                                                                  size = 10)) +
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

ppfd_max_plot <- ggplot(df.list$ppfd_day_max, aes(x = reorder(group, -sort),
                                                 y = ppfd_mes,
                                                 fill = group),
                       na.rm = TRUE) +
  geom_bar(color = "black",stat = "identity") +
  geom_errorbar(data = df.list$ppfd_day_max,
                aes(ymin = ppfd_mes - se, ymax = ppfd_mes + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(0, 785, 2050),
                 pch = c(26, 65, 66),
                 stroke = 8)) +
  coord_flip() +
  labs(y = bquote("Daily Max PPFD" ~ umol/s/m^2), x = "") +
  theme_classic() +
  theme(text = element_text(size = 20),axis.text.y = element_text(color = "white",
                                                                  size = 10)) +
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

vpd_max_plot <- ggplot(df.list$vpd_day_max, aes(x = reorder(group, -sort),
                                               y = vpd,
                                               fill = group)) +
  geom_bar(color = "black ",stat = "identity") +
  geom_errorbar(data = df.list$vpd_day_max,
                aes(ymin = vpd - se, ymax = vpd + se),
                width = .5,
                size = .25) +
  scale_shape_identity() +
  geom_point(aes(x = c("Understory", "Middle", "Upper"),
                 y = c(.775, .950, 1.125),
                 pch = c(65, 66, 67),
                 stroke = 8)) +
  coord_flip() +
  labs(y = "Daily Max \nVapor Pressure Deficit (kPa)", x = "") +
  theme_classic() +
  theme(text = element_text(size = 20),axis.text.y = element_text(color = "white",
                                                                  size = 10)) +
  scale_fill_manual(values = c("#808080","#404040", "#C0C0C0"))

# Displays bar charts in a grid
tiff(filename = "Figure_3_averages_and_maxima.tiff",
     width = 1080, height = 720, units = "px",
     compression = "none")
ggarrange(leaf_day_plot, leaf_max_plot,
          air_day_plot, air_max_plot,
          vpd_day_plot, vpd_max_plot,
          ppfd_day_plot, ppfd_max_plot,
          ncol = 2,
          nrow = 4,
          labels = c("(a)", "(b)", "(c)", "(d)",
                     "(e)", "(f)", "(g)", "(h)"),
          legend = "none",
          font.label = list(size = 20, face = "bold", color = "black",
                            family = NULL),
          hjust = -.5, vjust = 1,
          widths = c(1,1), heights = c(1,1))
dev.off()
# Printed summary of anova results.
summary.aov(leafmean.aov)
leafmean.HSD
summary.aov(airmean.aov)
airmean.HSD
summary.aov(ppfdmean.aov)
ppfdmean.HSD
summary.aov(vpdmean.aov)
vpdmean.HSD

summary.aov(leafmax.aov)
leafmax.HSD
summary.aov(airmax.aov)
airmax.HSD
summary.aov(ppfdmax.aov)
ppfdmax.HSD
summary.aov(vpdmax.aov)
vpdmax.HSD