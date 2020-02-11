rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

########################
# File Names
########################

foldername<-'20200210' # folder of the day
filename_cat<-'20200210_Cat.csv' # concatenated data from miniPAR Logger
filename_plot<-'20200210_Plot.csv' # plot data from miniPAR Logger
filename_HOBOpendant <- 'LICOR_Cal_HOBO_Test_20200210.csv' # If calibrating HOBO pendant against LI-COR
Date<-20200210


#################################################################################
# DO NOT CHANGE ANYTHING BELOW HERE ----------------------------------
#################################################################################

# Tidy Concatenation Data
Concat_Data <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',filename_cat),
                       col_names = FALSE,
                       skip=7) #skips first 7 rows containing instrument information
# Split merged cell into column headings
# note that all vectors are character type
Concat_Data <- separate(Concat_Data,"X1", into=c("Unix_Timestamp_second","UTC_Date_Time","PST",
                                                 "Battery_volts","Temp_degC","PAR","Ax_g",
                                                 "Ay_g","Az_g"), sep=",", remove=TRUE)
# Convert PST to date and time vector type
Concat_Data$PST <- Concat_Data$PST %>%
  parse_datetime(format = "%Y-%m-%d %H:%M:%S", na = character(),
             locale = default_locale(), trim_ws = TRUE)

# Convert character strings to double number type
Concat_Data$Battery_volts <- Concat_Data$Battery_volts %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$Temp_degC <- Concat_Data$Temp_degC %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$PAR <- Concat_Data$PAR %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$Ax_g <- Concat_Data$Ax_g %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$Ay_g <- Concat_Data$Ay_g %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)
Concat_Data$Az_g <- Concat_Data$Az_g %>%
  parse_double(na=c("","NA"),locale = default_locale(), trim_ws = TRUE)

View(Concat_Data)
# Create simple csv file
write_csv(Concat_Data,paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',Date,'_PARcat.csv'))


#################################################################################
# Tidy Plot Data (Redundant to Concatenation Data)

Plot_Data <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',filename_plot),
                        col_names = FALSE,
                        skip=3) #skips first 3 rows containing instrument information
# Split merged cell into column headings
Plot_Data <- separate(Plot_Data,"X1", into=c("Unix_Timestamp_second","Battery_volts","Temp_degC",
                                             "PAR","Ax_g","Ay_g","Az_g"), sep=",", remove=TRUE)
# Write file to csv - redundant to Concat_Data
### write_csv(Plot_Data,paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',Date,'_PARplot.csv'))



#################################################################################
# Calibrating HOBO Pendant
# First bring in HOBO data
HOBO <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/HOBOpendant/',filename_HOBOpendant),
                 skip=1,
                 col_names=TRUE,
                 col_types=list("Button Down (LGR S/N: 20555870)"=col_skip(), # skips columns containing logger launch, connection, and stop information
                                "Button Up (LGR S/N: 20555870)"=col_skip(),
                                "Host Connected (LGR S/N: 20555870)"=col_skip(),
                                "Stopped (LGR S/N: 20555870)"=col_skip(),
                                "End Of File (LGR S/N: 20555870)"=col_skip())) 
# Drops NA values at the bottom of the data file by using the presence of Temp data as a proxy for logged values to keep
HOBO <- HOBO %>%
  filter(!is.na(`Temp, °C (LGR S/N: 20555870, SEN S/N: 20555870)`))
# Relabel column headings
HOBO <- HOBO %>%
  rename(PST=contains("Date"), Temp_degC=contains("Temp"), Intensity_Lux_HOBO=contains("Intensity"))
# Separates date and time into separate columns before converting to date and time string types
HOBO$PST <- HOBO$PST %>%
  parse_datetime(format = "%m/%d/%y %H:%M", na = character(),
             locale = default_locale(), trim_ws = TRUE)
View(HOBO)

# merge the LI-COR and HOBO dataframes
LICOR_HOBO <- full_join(Concat_Data,HOBO, by="PST")
# remove the unecessary columns
LICOR_HOBO <- LICOR_HOBO %>%
  select(-c('#',Battery_volts))
# rename temp columns
LICOR_HOBO <- LICOR_HOBO %>%
  rename(Temp_degC_LICOR="Temp_degC.x",Temp_degC_HOBO="Temp_degC.y")
# Drops NA values where LICOR and HOBO aren't overlapping readings
LICOR_HOBO <- LICOR_HOBO %>%
  filter(!is.na(Intensity_Lux_HOBO),!is.na(PAR))
View(LICOR_HOBO)

# Calibrating HOBO to LICOR
# PAR_LICOR = y0 + A1*e^(-HOBO / t1)
# PAR_LICOR: PAR data from the LICOR (μmol photons m–2 s–1)
# HOBO: raw output data (lumens m–2)
# A1, t1, and y0 are fitting constants

linear.model <-lm(Intensity_Lux_HOBO ~ PAR, LICOR_HOBO)
log.model <-lm(log(Intensity_Lux_HOBO) ~ PAR, LICOR_HOBO)
exp.model <-lm(Intensity_Lux_HOBO ~ exp(PAR), LICOR_HOBO)

# log.model.df <- tibble(x = LICOR_HOBO$PAR,
#                           y = exp(fitted(log.model)))

ExpRegPlot <- ggplot(LICOR_HOBO, aes(x=PAR, y=Intensity_Lux_HOBO)) + 
  geom_point() +
  geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ exp(x)), se=FALSE, linetype=1) +
 # geom_line(data=log.model.df, aes(x,y,color="Log Model"), size=1, linetype=1) +
  guides(color = guide_legend("Model Type")) +
  stat_poly_eq(eq.with.lhs = TRUE,formula=(y ~ exp(x)),output.type = "expression",parse=TRUE)
  
# View plot
ExpRegPlot
summary(ExpRegPlot)
