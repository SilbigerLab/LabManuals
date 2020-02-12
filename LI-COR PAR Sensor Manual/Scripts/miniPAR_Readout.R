rm(list=ls())
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(ggpmisc)

########################
# File Names
########################

foldername<-'20200211' # folder of the day
filename_cat<-'20200210-11_Cat.csv' # concatenated data from miniPAR Logger
filename_plot<-'20200211_Plot.csv' # plot data from miniPAR Logger
filename_HOBOpendant <- '20200210-11_HOBO.csv' # If calibrating HOBO pendant against LI-COR
Date <- 20200211

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
#View(Concat_Data)
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
# First bring in HOBO data - assumes file was stored in HOBOmobile app on a phone and shared as a csv file to the computer
HOBO <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/HOBOpendant/',filename_HOBOpendant),
                 skip=2,
                 col_names=TRUE,
                 col_types=list("Button Down"=col_skip(), # skips columns containing logger launch, connection, and stop information
                                "Button Up"=col_skip(),
                                "Host Connect"=col_skip(),
                                "Stopped"=col_skip(),
                                "EOF"=col_skip())) 
# Relabel column headings
HOBO <- HOBO %>%
  rename(PST=contains("Date"), Temp_degC=contains("Temp"), HOBO_Lux=contains("Intensity"))
# Drops NA values at the bottom of the data file by using the presence of Temp data as a proxy for logged values to keep
HOBO <- HOBO %>%
  filter(!is.na(Temp_degC))
# Converts to date and time string types
HOBO$PST <- HOBO$PST %>%
 parse_datetime(format = "%m/%d/%y %H:%M", na = character(),
           locale = default_locale(), trim_ws = TRUE)

# Convert to SI units 
HOBO <- HOBO %>%
  mutate(
    Temp_degC = (Temp_degC-32)*5/9,
    # converting lumens to lux to PAR
    HOBO_Lux = HOBO_Lux*10.7639104*0.0185
  )
# Lux to PAR converstion source: https://www.apogeeinstruments.com/conversion-ppfd-to-lux/
#View(HOBO)
write_csv(HOBO,paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/',Date,'_HOBOpar.csv'))

# If bringing in HOBO data from HOBOware on the computer
# When exporting table data from HOBOware, only click the relevant data (i.e. temp and intensitiy)
 #HOBO <- read_csv(paste0('LI-COR PAR Sensor Manual/Data/',foldername,'/HOBOpendant/',filename_HOBOpendant),
  #                skip=1,
   #               col_names=TRUE)
 
# Converts to date and time string types
 #HOBO$PST <- HOBO$PST %>%
 # parse_datetime(format = "%m/%d/%y %H:%M", na = character(),
 #           locale = default_locale(), trim_ws = TRUE)
 
 # Relabel column headings
 #HOBO <- HOBO %>%
  # rename(PST=contains("Date"), Temp_degC=contains("Temp"), HOBO_Lux=contains("Intensity"))
 # Drops NA values at the bottom of the data file by using the presence of Temp data as a proxy for logged values to keep
 #HOBO <- HOBO %>%
  # filter(!is.na(Temp_degC))
 
 
# merge the LI-COR and HOBO dataframes
LICOR_HOBO <- full_join(Concat_Data,HOBO, by="PST")
# remove the unecessary columns
LICOR_HOBO <- LICOR_HOBO %>%
  select(-c(Battery_volts))
# rename temp columns
LICOR_HOBO <- LICOR_HOBO %>%
  rename(Temp_degC_LICOR="Temp_degC.x",Temp_degC_HOBO="Temp_degC.y")
# Drops NA values where LICOR and HOBO aren't overlapping readings
LICOR_HOBO <- LICOR_HOBO %>%
  filter(!is.na(HOBO_Lux),!is.na(PAR))
#View(LICOR_HOBO)

# Calibrating HOBO to LICOR
# PAR_LICOR = y0 + A1*exp(-HOBO / t1)
# PAR_LICOR: PAR data from the LICOR (μmol photons m–2 s–1)
# HOBO: raw output data (lumens m–2)
# A1, t1, and y0 are fitting constants

x <- LICOR_HOBO$HOBO_Lux
y <- LICOR_HOBO$PAR
linear.model <-lm(y ~ x)
#log.model <-lm(log(PAR) ~ HOBO_Lux, LICOR_HOBO)
exp.model <-lm(y ~ exp(x))

#log.model.df <- tibble(x = LICOR_HOBO$PAR,
 #                          y = exp(fitted(log.model)))

formula <- y ~ exp(x)
ExpRegPlot <- ggplot(LICOR_HOBO, aes(x=HOBO_Lux, y=PAR)) + 
  geom_point() +
  geom_smooth(method="lm", aes(color="Exp Model"), formula= formula,
              se=FALSE, linetype=1) +
  # geom_smooth(method="lm", aes(x,y,color="Log Model"), formula=y~x, size=1, linetype=1) +
  # geom_line(data=log.model.df, aes(x,y,color="Log Model"), size=1, linetype=1) +
  guides(color = guide_legend("Model Type")) +
  stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label..,
                  sep = "~~italic(\",\")~~")),formula = formula, parse = TRUE)

# View plot
ExpRegPlot
exp.model
#linear.model
#log.model
#View(ExpRegPlot)
#summary(ExpRegPlot)
