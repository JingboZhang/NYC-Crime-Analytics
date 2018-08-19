library(data.table)
library(dplyr)
library(lubridate)
library(ggmap)
library(ggplot2)

crime <- read.csv('NYPD_Complaint_Data_Historic.csv', header = TRUE)
crime <- as.data.table(crime)
crime <- crime[!is.na(Latitude)]
crime <- crime[!is.na(CMPLNT_FR_TM)]
crime$Class <- NA
violent <- c('OFF. AGNST PUB ORD SENSBLTY &', 'OFFENSES AGAINST PUBLIC ADMINI', 'OFFENSES AGAINST PUBLIC SAFETY',
             'ARSON', 'DANGEROUS DRUGS', 'DANGEROUS WEAPONS', 'INTOXICATED & IMPAIRED DRIVING', 
             'INTOXICATED/IMPAIRED DRIVING', 'OFFENSES AGAINST THE PERSON')
sex <- c('SEX CRIMES', 'RAPE')
larceny <- c('GRAND LARCENY', 'PETIT LARCENY', 'OTHER OFFENSES RELATED TO THEF')
vehicle <- c('GRAND LARCENY OF MOTOR VEHICLE', 'PETIT LARCENY OF MOTOR VEHICLE')
assault <- c('ASSAULT 3 & RELATED OFFENSES', 'FELONY ASSAULT', 'MURDER & NON-NEGL. MANSLAUGHTER')

crime$Class <- ifelse(crime$OFNS_DESC %in% sex, 'SEX CRIME', crime$Class)
crime$Class <- ifelse(crime$OFNS_DESC == 'BURGLARY', 'BURGLARY', crime$Class)
crime$Class <- ifelse(crime$OFNS_DESC == 'ROBBERY', 'ROBBERY', crime$Class)
crime$Class <- ifelse(crime$OFNS_DESC %in% larceny, 'LARCENY', crime$Class)
crime$Class <- ifelse(crime$OFNS_DESC %in% vehicle, 'VEHICLE THEFT', crime$Class)
crime$Class <- ifelse(crime$OFNS_DESC %in% assault, 'ASSAULT', crime$Class)
crime$Class <- ifelse(crime$OFNS_DESC %in% violent, 'VIOLENT CRIME', crime$Class)
table(crime$Class)

crime$CMPLNT_FR_DT <- mdy(crime$CMPLNT_FR_DT)
crime$CMPLNT_FR_TM <- hms(crime$CMPLNT_FR_TM)
crime <- subset(crime, year(crime$CMPLNT_FR_DT)>2005)
crime <- crime %>% mutate(Year  = factor(year(CMPLNT_FR_DT), levels=2006:2016),
                          Month = factor(month(CMPLNT_FR_DT), levels=1:12),
                          DayOfWeek = factor(weekdays(CMPLNT_FR_DT)))
crime$Hour <- hour(crime$CMPLNT_FR_TM)
crime$DayOfWeek <- factor(crime$DayOfWeek, levels=c("Monday",
                                                    "Tuesday",
                                                    "Wednesday",
                                                    "Thursday",
                                                    "Friday",
                                                    "Saturday",
                                                    "Sunday"))
crime$Period <- ifelse(lubridate::hour(crime$CMPLNT_FR_TM)>=7 & lubridate::hour(crime$CMPLNT_FR_TM)<19, 
                       'DAY', 'NIGHT')
crime <- subset(crime, Class == 'VIOLENT CRIME')

map <- get_map(location=c(lon=mean(crime[!is.na(Longitude)]$Longitude),lat=mean(crime[!is.na(Latitude)]$Latitude)), maptype='roadmap',zoom=10)
ggmap(map) +
  stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..),
                 bins = 7, geom = "polygon", size = 0.1,
                 data = crime) +
  scale_alpha_continuous(range = c(0.25, 0.4), guide = 'none') +
  scale_fill_gradient("Crime\nDensity") +
  scale_fill_gradient(low = "black", high = "red") +
  ggtitle('Total Crime Density in NYC')