crime <- read.csv('NYPD_Complaint_Data_Historic.csv', header = TRUE)

library(sqldf)
library(data.table)
library(dplyr)
library(sp)
library(lubridate)
library(RJSONIO)
library(RCurl)

# drop rows that do not have location information
sapply(crime, function(x) sum(is.na(x)))
crime <- as.data.table(crime)
crime <- crime[!is.na(Latitude)]

# classification of detailed description
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

# convert to time format
crime$CMPLNT_FR_DT <- mdy(crime$CMPLNT_FR_DT)
crime$CMPLNT_FR_TM <- hms(crime$CMPLNT_FR_TM)
# only select crime event happened after 2005
crime <- subset(crime, year(crime$CMPLNT_FR_DT)>2005)

# grid transformation
min_lon <- min(crime$Longitude)
min_lat <- min(crime$Latitude)
# for each 0.002 change of longitude or latitude, the distance will change approximately 200m
crime$Round_lon <- floor((crime$Longitude-min_lon)/0.002)+1
crime$Round_lat <- floor((crime$Latitude-min_lat)/0.002)+1

# density calculation
# suppose the central element is the grid focused, calculate the weight of adjacent grids
distance_matrix <- matrix(1,5,5)
theta = 0.6
for (i in 1:5){
  for (j in 1:5)
    distance_matrix[i,j] <- exp(-((i-3)^2+(j-3)^2)/(2*theta^2))/(2*pi*theta^2)
}
# number of total crime in each grid
total_density <- sqldf('SELECT Round_lon, Round_lat, COUNT(*) AS Total_crime
                       FROM crime
                       GROUP BY Round_lon, Round_lat')
# define a subservience matrix
total_matrix <- matrix(0, max(crime$Round_lat)+4, max(crime$Round_lon)+4)
for (i in 1:nrow(total_density)){
  r = total_density$Round_lat[i]
  c = total_density$Round_lon[i]
  num = total_density$Total_crime[i]
  total_matrix[r+2,c+2] = num
}
# total crime density
density <- matrix(0,max(crime$Round_lat), max(crime$Round_lon))
for (i in 3:210){
  for (j in 3:280){
    sub_matrix <- total_matrix[(i-2):(i+2),(j-2):(j+2)]
    density_matrix <- sub_matrix*distance_matrix
    density[i-2,j-2] = sum(density_matrix)
  }
}
# calculate total crime density for each grid
total_density$Density = 0
for (i in 1:nrow(total_density)){
  r = total_density$Round_lat[i]
  c = total_density$Round_lon[i]
  total_density$Density[i] = density[r,c]
}
write.csv(total_density, 'total_density.csv', row.names = FALSE)
rm(density)
rm(density_matrix)
rm(sub_matrix)
rm(total_matrix)

# decide the district that each grid belongs to
# depends on the district that most crime events belong to within each grid
district <- sqldf('SELECT Round_lon, Round_lat, BORO_NM, COUNT(*) AS Num
                    FROM crime
                    GROUP BY Round_lon, Round_lat, BORO_NM')
district <- na.omit(district)
district <- subset(district, BORO_NM != "")
district <- sqldf('SELECT a.Round_lon, a.Round_lat, b.BORO_NM as District
                 FROM (SELECT Round_lon, Round_lat, MAX(Num) AS Max
                        FROM district
                        GROUP BY Round_lon, Round_lat) AS a
                  JOIN district AS b
                  ON a.Round_lon = b.Round_lon
                  AND a.Round_lat = b.Round_lat
                  AND a.Max = b.Num')

# get the longitude and latitude of central point for each grid
temp <- sqldf('SELECT Round_lon, Round_lat, AVG(Longitude) AS Central_lon
              FROM crime
              GROUP BY Round_lon, Round_lat')
district$Central_lon <- left_join(district, temp)$Central_lon
temp <- sqldf('SELECT Round_lon, Round_lat, AVG(Latitude) AS Central_lat
              FROM crime
              GROUP BY Round_lon, Round_lat')
district$Central_lat <- left_join(district, temp)$Central_lat
# geo encoding
latlon2zip <- function(lat, lon) {
  url <- sprintf("http://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f&zoom=18&addressdetails=1", lat, lon)
  res <- fromJSON(url)
  return(res[["address"]][["postcode"]])
}

for (i in 1:1000){
  district$address[i] = latlon2zip(district$Central_lat[i], district$Central_lon[i])
}  
district[district=="07206"] <- "10303"
district[district=="10048"] <- "10007"
district[district=="10055"] <- "10022"
library(stringr)
district$address <- as.character(district$address)
for (i in 1:nrow(district)){
  if (str_length(district$address[i]) > 5){
    temp = revgeocode(c(district$Central_lon[i], district$Central_lat[i]))
    district$address[i] = substr(temp, str_length(temp)-9, str_length(temp)-5)
  }
}
district[1641,"address"] = "10306"
district[13773,"address"] = "11365"
district[13776,"address"] = "11355"

# calculate the number of district in each zip code
district_summary <- sqldf('SELECT address, COUNT(*) AS num_district
                          FROM district
                          GROUP BY address
                          ORDER BY address')
write.csv(district_summary, 'district_summary.csv', row.names = FALSE)
write.csv(district, 'district.csv', row.names = FALSE)

# import demographic data
district <- read.csv('district.csv')
demograph <- read.csv('demographic_calculate.csv')
# impute missing value (mean)
for(i in 3:ncol(demograph)){
  demograph[is.na(demograph[,i]), i] <- mean(demograph[,i], na.rm = TRUE)
}
district$address = as.integer(district$address)
demograph <- left_join(district, demograph, by = "address")
demograph <- select(demograph, -Number.of.district)

# calculate the coordinate of adjacent cells for each grid
district <- add_rownames(district, var = "rowname")
district$rowname_1_1 <- sqldf('SELECT b.rowname AS rowname_1_1
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon
                              AND a.Round_lat = b.Round_lat-1')$rowname_1_1
district$rowname_1_2 <- sqldf('SELECT b.rowname AS rowname_1_2
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon
                              AND a.Round_lat = b.Round_lat+1')$rowname_1_2
district$rowname_1_3 <- sqldf('SELECT b.rowname AS rowname_1_3
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon-1
                              AND a.Round_lat = b.Round_lat')$rowname_1_3
district$rowname_1_4 <- sqldf('SELECT b.rowname AS rowname_1_4
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon+1
                              AND a.Round_lat = b.Round_lat')$rowname_1_4
district$rowname_2_1 <- sqldf('SELECT b.rowname AS rowname_2_1
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon-1
                              AND a.Round_lat = b.Round_lat-1')$rowname_2_1
district$rowname_2_2 <- sqldf('SELECT b.rowname AS rowname_2_2
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon-1
                              AND a.Round_lat = b.Round_lat+1')$rowname_2_2
district$rowname_2_3 <- sqldf('SELECT b.rowname AS rowname_2_3
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon+1
                              AND a.Round_lat = b.Round_lat-1')$rowname_2_3
district$rowname_2_4 <- sqldf('SELECT b.rowname AS rowname_2_4
                              FROM district AS a
                              LEFT OUTER JOIN district AS b
                              ON a.Round_lon = b.Round_lon+1
                              AND a.Round_lat = b.Round_lat+1')$rowname_2_4

# all posible occured date
occured_date <- matrix(0,132,2)
i = 1
for (y in 2006:2016){
  for (m in 1:12){
    occured_date[i,1] = y
    occured_date[i,2] = m
    i = i + 1
  }
}
occured_date <- as.data.frame(occured_date)
names(occured_date) <- c('Year', 'Month')
# all posibble occured area & date
full <- sqldf('SELECT * FROM district JOIN occured_date')

# data preparation
# please see preparation_function.R for detailed functions 
full_step('VIOLENT CRIME')

