# Program Title: ANOVA Analysis for daily averages.
# Author: Benjamin D. Miller
# Date Started: 20181121
# Date Revised: 20200617
# Program Description: Creates barplots with standard error of the groups
# and perfoms anova analysis on each variable. Also does TukeyHSD analysis
# and adds characters to the graph based on the results. The tukey results
# do not auto update, they are added manually to the figure.

# Clears environment
rm(list = ls())

# Imports libraries
library(HH)
library(lubridate)
library(ggpubr)
library(multcomp)
library(dplyr)
library(multcomp)
library(broom)

#Input file
data_master <- read.csv("SURF_data_master.csv", header = TRUE, sep = ",")

# Sets Z-value used for CI calculation
test_val <- 1.960 #95 percent

# Plot list name
plot_name <- "Daily_Mean_Max"
#Plot Labels
plot_labels <- c(expression(Delta*"T"*" (°C)"), expression("T"["leaf"]*" (°C)"),
                 expression("T"["air"]*" (°C)"),
                expression("PPFD "["umol/s/m"^"2"]), expression("Vapor Pressure Deficit (kPa)"))

# Sets variables to be averaged and maximized by day
# ***VARIBLES MUST MATCH THE ORDER WITH THE ABOVE VECTOR****
plot_variables <- c("delta_temp", "leaf_temp_c", "air_temp_c", "ppfd_mes", "vpd")

#######---------------------------- END USER DEFINED VARIABLES --------------------------######
#Turns off current graphics
graphics.off()

#### Data Wrangling ####
# Defines the variables in proper form, Also creates a date class for
# use during average and maxima calculations.

data_master$sens_hgt <- as.factor(data_master$sens_hgt)

data_master$date <- floor_date(as.POSIXct(data_master$halfhour), "1 day")

# Calculates Delta T
data_master$delta_temp <- data_master$leaf_temp_c - data_master$air_temp_c

# Selects only daytime Values
data_master <- with(data_master,
                    data_master[hour(halfhour) >= 7 & hour(halfhour) <= 17,])

# Assigns sensor heights to associated groups (1= Upper, 2 = Middle, 3= Understory)
data_master$group <-ifelse(data_master$sens_hgt == 20, 1,
                           ifelse(data_master$sens_hgt == 2,
                                  3,
                                  2))

daily_plot_function <- function(input_df, var_vector, lab_vector, test_stat) {
  #initializes empty lists for plots, Tucky and ancova results
  plots <- c()
  anova_summary <- c()
  tukey_results <- c()
  
  # Vector for "average" and Maximum
  form <- c("Average", "Maximum")
  
  # Makes the nested loop do it twice, once for each 
  for(j in 1:length(form)) {
    
    # loops through the variable vector, and for each variable in the vector does a mean
    # a mean and makes a bar plot of its result
    for (i in 1:length(var_vector)) {
      print(paste0("Making ",var_vector[i], " ", form[j], " graph"))
      input_df$x <- input_df[, var_vector[i]]
      out_df <- input_df
      
      if (form[j] != "Average"){
        # Pulls the daily maximums for each day at each level
        out_df <- out_df %>%
          dplyr::group_by(group, date) %>%
          dplyr::slice(which.max(x)) %>%
          dplyr::ungroup()
      } else {
        # Calculates daily mean for each day at each level
        out_df <- out_df %>%
          dplyr::group_by(group, date) %>%
          dplyr::summarise(x = mean(x, na.rm = TRUE)) %>%
          dplyr::ungroup()
      }
      
      # Sets "group" to a factor
      out_df$group <- as.factor(out_df$group)
      
      # Performs an ANOVA
      anova_results <- aov(formula = x ~ group, data = out_df)
      
      anova_summary[[paste0(var_vector[i], "_", form[j])]] <-
        summary.aov(anova_results)
      
      # Performs a Tukey's Honest Significant Difference Test and writes desired
      # outputs to a compatible dataframe with out_df
      
      tukey_results <- 
        broom::tidy(cld(glht(anova_results, linfct = mcp(group="Tukey"))))
      
      # Summarizes data for graphing
      out_df <- out_df %>% 
        dplyr::group_by(group) %>% 
        dplyr::summarise(plot_value = mean(x, na.rm = TRUE),
                         sd = sd(x, na.rm = TRUE),
                         ci = test_stat*(sd/sqrt(n()))) %>%
        dplyr::ungroup()
      
      out_df<- dplyr::left_join(out_df, tukey_results,
                                by = c('group' = "lhs"))
      
      # Checks for NA values in the df for na's and removes if they are present
      # before plotting
      out_df <-na.omit(out_df)
      
      
      #Diurnal Plots are created one by one as the loop repeats
      plots[[paste0(var_vector[i], "_", form[j])]] <- 
        ggplot(out_df,
               aes(x = as.numeric(group), 
                   y = plot_value,
                   fill = as.factor(group))) +
        geom_bar(color = "black", stat = "identity", show.legend = FALSE) +
        geom_errorbar(aes(ymin = plot_value - ci,
                          ymax = plot_value + ci),
                      width = .5, size = .25) +
        geom_text(aes(y = ifelse(plot_value <0,
                                 plot_value - sd,
                                 plot_value + sd),
                       label = letters)) +
        scale_shape_identity() +
        scale_x_reverse(breaks = c(1,2,3),
        labels = c("Upper", "Middle", "Understory")) +
        labs(y = lab_vector[i], x = "") +
        theme_classic() +
        theme(text = element_text(size = 20))+
        scale_fill_manual(values = c("#808080","#404040", "#C0C0C0")) +
        if (grepl("temp_c", var_vector[i])) {
          coord_flip(ylim = c(25,34))
        } else {
          coord_flip()
        }
      }
    assign(x = "Daily_Plots", plots, envir = .GlobalEnv)
  }
}

daily_plot_function(data_master, plot_variables, plot_labels, test_val) 

# Writes the figure to an output file

tiff(filename = "Figure_3_Averages_and_Maxima.tiff",
     width = 840, height = 1080, units = "px")

day_plot <- ggarrange(Daily_Plots$delta_temp_Average, Daily_Plots$delta_temp_Maximum,
                      Daily_Plots$leaf_temp_c_Average, Daily_Plots$leaf_temp_c_Maximum,
                      Daily_Plots$air_temp_c_Average, Daily_Plots$air_temp_c_Maximum,
                      Daily_Plots$ppfd_mes_Average, Daily_Plots$ppfd_mes_Maximum,
                      Daily_Plots$vpd_Average, Daily_Plots$vpd_Maximum,
                      ncol = 2,
                      nrow = 5,
                      labels = c("(a)", "(b)", "(c)", "(d)",
                                 "(e)", "(f)", "(g)", "(h)", "(i)", "(j)"),
                      legend = "none",
                      font.label = list(size = 20, face = "bold", color = "black",
                                        family = NULL),
                      hjust = 0, vjust = 1,
                      align = "hv",
                      widths = c(1,1), heights = c(1,1))

day_plot <- annotate_figure(day_plot,
                left = text_grob("Daily Average Values", face = "italic", size = 30, rot = 90),
                right = text_grob("Daily Maximum Values", face = "italic", size = 30, rot = 270))

print(day_plot)

dev.off()
