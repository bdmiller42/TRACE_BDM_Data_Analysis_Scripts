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
library(lubridate)
library(dplyr)

#Turns off current graphics
graphics.off()

# Sets working directory and defines varibles from files
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")

# Sets Z-value used for CI calculation
Z <- 1.960

# Sets average Topt Value with confidence intervals for graphic display and time above estimation
T_opt <- c(29,30,31)

#makes a vector of the variables desired for output

plot_labels <- c(expression(Delta*"T"*" (°C)"), expression("T"["leaf"]*" (°C)"),
                 expression(T["air"]*" (°C)"),
                 bquote("PPFD" ~ umol/s/m^2), "Vapor Pressure Deficit (kPa)")

# list of variables to make plots of. ***VARIBLES MUST MATCH THE ORDER IN THE ABOVE VECTOR****
plot_variables <- c("delta_temp", "leaf_temp_c", "air_temp_c", "ppfd_mes", "vpd")

##########------------------- END OF USER DEFINED INPUTS ------------------------------##########
# Sets data types, creates easily averaged "time" column for data, and calculates delta T
data_master$halfhour <- as.POSIXlt(data_master$halfhour)
data_master$sens_hgt <- as.factor(data_master$sens_hgt)

data_master$hour <- as.numeric(hour(data_master$halfhour))
data_master$minutes <- as.numeric(minute(data_master$halfhour))
data_master$time <- data_master$hour + ifelse(data_master$minutes == 30, .5, 0)

data_master$delta_temp <- data_master$leaf_temp_c - data_master$air_temp_c


#Uses dplyr to remove some columns
data_master <- data_master %>%
  select(-c(halfhour,X, hour, minutes))


# Function to make average calculations and create a plot of the output of a diurnal pattern
diurnal_plot_function <- function(input_df, var_vector, lab_vector, z_value) {
  #initializes empty plot list
  plots <- c()
  for (i in 1:length(var_vector)) {
    print(paste0("making ",var_vector[i], " diurnal graph"))
    input_df$var <- input_df[, var_vector[i]]
    includedvars <- c("var", "sens_hgt", "time")
    out_df <- input_df %>%
      dplyr::group_by(sens_hgt, time) %>%
      dplyr::summarise(mean = mean(var, na.rm = TRUE), sd = sd(var),
                       n = n(), ci = z_value*(sd/n))
    
    #Diurnal Plots are created one by one as the loop repeats
    plots[[var_vector[i]]] <- ggplot(data = out_df,
                                     aes(x = time, y = mean,
                                         group = sens_hgt)) +
      theme_classic() +
      theme(text = element_text(size = 20))+
      geom_ribbon(aes(ymin = mean - ci, ymax = mean + ci,
                      group = sens_hgt), color = rgb(red = 192,
                                                     green = 192,
                                                     blue = 192,
                                                     maxColorValue = 255),
                  fill = rgb(red = 192, blue = 192, green =  192,
                             maxColorValue = 255)) +
      geom_line(aes(color = sens_hgt), na.rm = T, size = 1.5) +
      labs(x = "", y = lab_vector[i],
           color = "Sensor \nHeight (m)") +
      scale_x_continuous(breaks = c(0, 4, 8, 12, 16, 20),
                         labels = c("0:00", "4:00", "8:00", "12:00",
                                    "16:00", "20:00"))
    if (grepl("temp_c", var_vector[i])){
      plots[[var_vector[i]]] <- plots[[var_vector[i]]] +
        scale_y_continuous(breaks = c(24, 26, 28, 30)) +
        # Adds error of Topt measurements as well as Topt to the air and leaf temp graphs
        geom_hline(yintercept = median(T_opt),
                   linetype = 2, size = 2) + 
        geom_hline(yintercept = min(T_opt),
                   linetype = 6, size = 1) + 
        geom_hline(yintercept = max(T_opt),
                   linetype = 6, size = 1)
    }
  }
  assign(x = "Diurnal_Plots", plots, envir = .GlobalEnv)
}
 
diurnal_plot_function(data_master, plot_variables, plot_labels, Z)

# Creates grid of all graphs
tiff(filename = "Figure_2_Diunal_Pattern.tiff",
     width = 720, height = 1080, units = "px",
     compression = "none")
ggarrange(Diurnal_Plots$delta_temp,
          nrow = 3, legend = "right",
          hjust = -4.25, vjust = 3,
          labels = "(a)",
          ggarrange(Diurnal_Plots$leaf_temp_c,
                    Diurnal_Plots$air_temp_c,
                    ncol = 2, legend = "none",
                    hjust = -4.25, vjust = 6,
                    labels = c("(b)", "(c)")),
          ggarrange(Diurnal_Plots$vpd, Diurnal_Plots$ppfd_mes,
                    ncol = 2, legend = "none",
                    hjust = -5.25, vjust =  4,
                    labels = c("(d)", "(e)")))

dev.off()

# #End of script