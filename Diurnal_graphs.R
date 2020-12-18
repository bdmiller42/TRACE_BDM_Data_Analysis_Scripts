#----
# Title: Diurnal Graph output from 2017 SURF Data
# Author: Ben Miller
# Date Started: 20181105
# Date updated: 20200617
# Description: Creates Diurnal pattern graphs of various variables that are then arranged in
#              a grid. The calculations are done within a function that can be seen below. 
#              Topt can be applied to the graphs at the end of the user defined variables.
#              It also calculates time above T_opt for each level of the canopy
#----

#Turns off current graphics and clears working environment
rm(list = ls())

# Sets libraries
library(tidyverse)
library(ggpubr)
library(lubridate)


# Sets working directory and defines varibles from files
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")

# Sets stat-value (in this isntance a Z value) used for CI calculation
test_stat <- 1.960 #95 percent

# Sets average Topt Value with confidence intervals for graphic display and time above estimation
T_opt <- c(29.07,30.32,31.32) #estimates for now, will change with updated values from Kelsey

# A ector of the axis labels desired for output

plot_labels <- c(expression(Delta*"T"*" (°C)"), expression("T"["leaf"]*" (°C)"),
                 expression(T["air"]*" (°C)"),
                 bquote("PPFD" ~ umol/s/m^2), "Vapor Pressure Deficit (kPa)")

# A list of variables to make plots of. 
# ***VARIBLES MUST MATCH THE ORDER WITH THE ABOVE VECTOR****

plot_variables <- c("delta_temp", "leaf_temp_c", "air_temp_c", "ppfd_mes", "vpd")

##########------------------- END OF USER DEFINED INPUTS ------------------------------##########
# Sets data types, creates easily averaged "time" column for data
data_master$halfhour <- as.POSIXlt(data_master$halfhour)
data_master$sens_hgt <- as.factor(data_master$sens_hgt)
data_master$unique_date <- format(data_master$halfhour, "%Y-%m-%d")

data_master$hour <- as.numeric(hour(data_master$halfhour))
data_master$minutes <- as.numeric(minute(data_master$halfhour))
data_master$time <- data_master$hour + ifelse(data_master$minutes == 30, .5, 0)
data_master <- data_master %>%
  dplyr::select(-c(halfhour,X, hour, minutes))

# Delta T Calculation
data_master$delta_temp <- data_master$leaf_temp_c - data_master$air_temp_c

# Function to perform average calculations and create plots 0of the output of the diurnal patterns
diurnal_plot_function <- function(input_df, var_vector, lab_vector, z_value) {
  #initializes empty plot list
  plots <- c()
  # loops through the variable vector, and for each variable in the vector does measures
  # of central tendancy and makes a plot of its diurnal pattern.
  for (i in 1:length(var_vector)) {
    print(paste0("Making ",var_vector[i], " diurnal graph"))
    input_df$x <- input_df[, var_vector[i]]
    out_df <- input_df %>%
      dplyr::group_by(sens_hgt, time) %>%
      dplyr::summarise(mean = mean(x, na.rm = TRUE), 
                       sd = sd(x, na.rm = TRUE),
                       n = n(),
                       ci = test_stat*(sd/sqrt(n)))
    
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
                                    "16:00", "20:00")) +
      scale_color_brewer(palette = "Blues")
    # Finds the air temp graphs based on thier identifying string and
    # adds the Topt information to the plots. Also matches the scales
    # of the two plots for ease of comparison.
    if (grepl("temp_c", var_vector[i])){
      plots[[var_vector[i]]] <- plots[[var_vector[i]]] +
        scale_y_continuous(breaks = c(24, 26, 28, 30)) +
        geom_hline(yintercept = median(T_opt),
                   linetype = 1, size = 1) + # Mean Line
        geom_hline(yintercept = min(T_opt),
                   linetype = 6, size = 1) + # Lower CI Line
        geom_hline(yintercept = max(T_opt),
                   linetype = 6, size = 1) # Upper CI Line
    }
  }
  assign(x = "Diurnal_Plots", plots, envir = .GlobalEnv)
}
 
diurnal_plot_function(data_master, plot_variables, plot_labels, Z)

# Creates grid of all graphs
print("Saving output of all diurnal graphs to file")

# Turns on a graphics device and saves the graphs all in a grid to a .tiff file.
# Filename must have .tiff at the end
tiff(filename = "Figure_2_Diunal_Pattern.tiff",
     width = 720, height = 1080, units = "px")

# Places all the plots into a grid, mess with the outputs to get different bits where
# they look nice. Would be difficult to automate that process as computers have no taste.
fin_plot <- ggarrange(Diurnal_Plots$delta_temp,
                      nrow = 3, legend = "right",
                      hjust = -4.25, vjust = 3,
                      labels = "(a)",
                      ggarrange(Diurnal_Plots$leaf_temp_c,
                                Diurnal_Plots$air_temp_c,
                                ncol = 2, legend = "none",
                                hjust = -4.25, vjust = 12,
                                labels = c("(b)", "(c)")),
                      ggarrange(Diurnal_Plots$ppfd_mes, Diurnal_Plots$vpd, 
                                ncol = 2, legend = "none",
                                hjust = -5.25, vjust =  4,
                                labels = c("(d)", "(e)")))
print(fin_plot)

dev.off()

print("Finished making graph")

### Calculation of average time above T opt if the temperature

above_t_opt_if_above <- data_master %>%
  dplyr::group_by(sens_hgt)%>% 
  dplyr::filter(leaf_temp_c >= T_opt[2]) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(unique_date, sens_hgt) %>% 
  dplyr::summarise(time_above = max(time) - min(time), .groups = 'keep') %>%
  dplyr::filter(time_above > 0) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(average_time_above = mean(time_above),
                   n = n(),
                   sd = sd(time_above),
                   up_ci = average_time_above + test_stat * (sd / sqrt(n)),
                   low_ci = average_time_above - test_stat * (sd / sqrt(n)))

above_t_opt_overall_average <- data_master %>% 
  dplyr::group_by(sens_hgt, time) %>%
  dplyr::summarise(average_temp_day_time = mean(leaf_temp_c, na.rm = TRUE)) %>%  
  dplyr::filter(average_temp_day_time >= T_opt[2])

total_days <- data_master %>%
  dplyr::group_by(unique_date) %>% 
  dplyr::summarise(unique_days = n())


# End of script
