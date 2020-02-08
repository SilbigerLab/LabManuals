rm(list=ls())
library(tidyverse)

########################
# File Names
########################
foldername<-'20200207' # folder of the day
filename_cat<-'20200207_Cat.csv' # concatenated data from miniPAR Logger
filename_plot<-'20200207_Plot.csv' # plot data from miniPAR Logger
Date<-20200207
#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

# Tidy Concatenation Data
Concat_Data <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',filename_cat),
                       col_names = FALSE,
                       skip=7) #skips first 7 rows containing instrument information
# Split merged cell into column headings
Concat_Data <- separate(Concat_Data,"X1", into=c("Unix_Timestamp_second","UTC_Date_Time","PSTime","Battery_volts","Temp_degC","PAR_umols-1m-2","Ax_g","Ay_g","Az_g"), sep=",", remove=TRUE)
View(Concat_Data)



#################################################################################
# Tidy Plot Data (Redundant to Concatenation Data)

Plot_Data <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',filename_plot),
                        col_names = FALSE,
                        skip=3) #skips first 3 rows containing instrument information
# Split merged cell into column headings
Plot_Data <- separate(Plot_Data,"X1", into=c("Unix_Timestamp_second","Battery_volts","Temp_degC","PAR_umols-1m-2","Ax_g","Ay_g","Az_g"), sep=",", remove=TRUE)
View(Plot_Data)

# Create simple csv file
write_csv(Concat_Data,paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',Date,'_PARcat.csv'))

# Redundant data as Concat_Data
### write_csv(Plot_Data,paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',Date,'_PARplot.csv'))
