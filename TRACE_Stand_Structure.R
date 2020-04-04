####---------------- TRACE Tree Census Summary Data Calculation ---------------------------------####
# Author: Benjamin D. Miller
# Date Created: 20200402
# Date Revised: 20200402
# Program Description:
# This program calculates stand structure of the TRACE study site in Luquillo Puerto Rico
# using the census data. *** Operating on the assumption of census accuracy ***

# Note: I did change the file headers beginning with "2016" as R does not play 
# nicely with numbers as column names.
#--------------------------------------------------------------------------------------------------#

####----------------------- Close all devices and delete all variables. ------------------------####
rm(list = ls(all = TRUE))   # clear workspace
graphics.off()              # close any open graphics
closeAllConnections()       # close any open connections to files
#--------------------------------------------------------------------------------------------------#

####--------------------------------- Load required libraries ----------------------------------####
# Info: Loads required R libraries and warns if package is not availible.
ok <- require(ggplot2); if (!ok)
  install.packages("ggplot2")
#--------------------------------------------------------------------------------------------------#

####-----------------------------Set Files and Directory----------------------------------------####
## Read in datasets and sets output directory

tree.data <- read.csv("/Users/Ben/Documents/MTU/Research/TRACE_data/TRACE_tree census_July2016.csv",
                      stringsAsFactors = FALSE)
## Set output directory here
out.dir <- paste0("/Users/Ben/Documents/MTU/Research/TRACE_data/20200402_Trace_Stand_Calculations")
if (!dir.exists(out.dir)) dir.create(out.dir, recursive = TRUE)

## Set summary file name here
sum.file <- paste0("2016_Trace_TreeData_Summary")


############################# END USER DEFINED INPUTS ##############################################

####------------------ Calculate Basic Stand Structure -----------------------------------------####
# Calculates the Tree Basal Area and 'tree count to its representativ 'per hectare" value
tree.data$Rep.Basal.Area.Hectare <- 0.00007854 * tree.data$DIAM.2016 ^ 2 * 4
tree.data$Rep.Tree.Hectare <- ifelse(tree.data$A.D.NF.2016 == "A", 4, 0)

# Initializes an output data frame
out.summary <- data.frame(BA.per.hectare = numeric(c(1)),
                          LiveTrees.per.hectare = numeric(c(1)))

# Creates per hectare averages
out.summary$BA.per.hectare <- sum(tree.data$Rep.Basal.Area.Hectare, na.rm = TRUE)
out.summary$LiveTrees.per.hectare <- sum(tree.data$Rep.Tree.Hectare)

# Writes a .txt output file of the calculation to the directory
fwrite(out.summary, paste0(out.dir,"/",sum.file), sep = ";",col.names = TRUE, row.names = FALSE)
rm(out.summary)
#--------------------------------------------------------------------------------------------------#

####------------------ Makes summary Plots by species ------------------------------------------####
# Makes bins for easy plotting by 5 cm
tree.data$bins <- cut(tree.data$DIAM.2016,
                      breaks = c(seq(from = 0, to = 100, by = 5)))

# Makes plot of Basal Area per hectare distrubution
tiff(paste0(out.dir,"/BA_per_hectare_plot.tiff",sep = ""),
     height = 720 ,width = 1080, units = "px")
ggplot2::ggplot(tree.data, aes(x = bins,
                               y = Rep.Basal.Area.Hectare,
                               fill = SPECIES.2016)) +
  geom_col(na.rm = TRUE) + 
  theme_classic() +
  labs(x = "Diameter (cm)", y = "Basal area per hectare (cm^2/ha)") + 
  theme(legend.position = "bottom")
dev.off()

# Makes plot of Tree per hectare distrubution
tiff(paste0(out.dir,"/Tree_per_hectare_plot.tiff",sep = ""),
     height = 720 ,width = 1080, units = "px")
ggplot2::ggplot(tree.data, aes(x = bins,
                               y = Rep.Tree.Hectare,
                               fill = SPECIES.2016)) +
  geom_col(na.rm = TRUE) + 
  theme_classic() +
  labs(x = "Diameter (cm)", y = "Trees per Hectare (tree/ha)") + 
  theme(legend.position = "bottom")
dev.off()

########################################### END OF SCRIPT ##########################################