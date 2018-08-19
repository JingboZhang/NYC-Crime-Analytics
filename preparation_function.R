library(dplyr)
library(sqldf)
library(lubridate)

# subset target crime type
crime_subset <- function(crime_type){
  data <- subset(crime, Class == crime_type)
  data <- subset(data, !is.na(CMPLNT_FR_TM) & !is.na(Round_lat))
  data$District <- left_join(data, district, by = c('Round_lon', 'Round_lat'))$District
  data$Year <- year(data$CMPLNT_FR_DT)
  data$Month <- month(data$CMPLNT_FR_DT)
  data$Period <- ifelse(lubridate::hour(data$CMPLNT_FR_TM)>=7 & lubridate::hour(data$CMPLNT_FR_TM)<19, 
                        'DAY', 'NIGHT')
  data <- select(data, Round_lon, Round_lat, District, Period, Year, Month, CMPLNT_NUM, CMPLNT_FR_DT, CMPLNT_FR_TM)
  return(data)
}

# preparation function
preparation <- function(data){
  # calculate crime density for single type of crime
  temp <- subset(data, Year != 2015 & Year != 2016)
  data_density <- sqldf('SELECT Round_lon, Round_lat, COUNT(*) AS Total_crime
                          FROM temp
                         GROUP BY Round_lon, Round_lat')
  total_matrix <- matrix(0, max(crime$Round_lat)+4, max(crime$Round_lon)+4)
  for (i in 1:nrow(data_density)){
    r = data_density$Round_lat[i]
    c = data_density$Round_lon[i]
    num = data_density$Total_crime[i]
    total_matrix[r+2,c+2] = num
  }
  density <- matrix(0,max(crime$Round_lat), max(crime$Round_lon))
  for (i in 3:210){
    for (j in 3:280){
      sub_matrix <- total_matrix[(i-2):(i+2),(j-2):(j+2)]
      density_matrix <- sub_matrix*distance_matrix
      density[i-2,j-2] = sum(density_matrix)
    }
  }
  data_density$Density = 0
  for (i in 1:nrow(data_density)){
    r = data_density$Round_lat[i]
    c = data_density$Round_lon[i]
    data_density$Density[i] = density[r,c]
  }
  rm(density)
  rm(density_matrix)
  rm(sub_matrix)
  rm(total_matrix)

  # get the full data for all possible occured date and location
  data <- sqldf('SELECT Round_lon, Round_lat, District, Year, Month, COUNT(*) As Num_crime
                 FROM data
                 GROUP BY Round_lon, Round_lat, District, Year, Month
                 ORDER BY Round_lon, Round_lat, Year, Month')
  data <- left_join(full, data)
  data <- select(data, -address, -Central_lon, -Central_lat)
  data[is.na(data)] <- 0

  # calculate the number of crime events happened in each grid last month
  data$Last_month <- 0
  Num_crime <- data$Num_crime
  Last_month <- data$Last_month
  for (area in 0:16514){
    temp = NA
    for (m in 1:132){
      Last_month[area*132+m] = temp
      temp = Num_crime[area*132+m]
    }
  }
  data$Last_month <- Last_month
  rm(Last_month)

  # calculate the weighted number of crime events happened in each grid within past six months
  data$Previous_occured <- 0
  # exponentially decay
  Previous_occured <- data$Previous_occured
  weight = c(exp(-6), exp(-5), exp(-4), exp(-3), exp(-2), exp(-1))
  for (area in 0:16514){
    time = Num_crime[(area*132+1):(area*132+6)]
    Previous_occured[area*132+7] = sum(time*weight)/sum(weight)
    for (m in 8:132){
      time[1:5] = time[2:6]
      time[6] = Num_crime[area*132+(m-1)]
      Previous_occured[area*132+m] = sum(time*weight)/sum(weight)
    }
  }
  data$Previous_occured = Previous_occured
  data[is.na(data)] <- 0

  # calculate the weighted number of crime events happened in adjacent grids of each grid within past six months
  data$Adjacent_previous_occured <- 0
  Adjacent_previous_occured <- data$Adjacent_previous_occured
  rowname_1_1 <- as.numeric(data$rowname_1_1)
  rowname_1_2 <- as.numeric(data$rowname_1_2)
  rowname_1_3 <- as.numeric(data$rowname_1_3)
  rowname_1_4 <- as.numeric(data$rowname_1_4)
  rowname_2_1 <- as.numeric(data$rowname_2_1)
  rowname_2_2 <- as.numeric(data$rowname_2_2)
  rowname_2_3 <- as.numeric(data$rowname_2_3)
  rowname_2_4 <- as.numeric(data$rowname_2_4)
  for (area in 0:16514){
    for (m in 1:132){
      if (rowname_1_1[area*132+m]>0){
        adjacent_area = rowname_1_1[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.1102378794383*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_1_2[area*132+m]>0){
        adjacent_area = rowname_1_2[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.1102378794383*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_1_3[area*132+m]>0){
        adjacent_area = rowname_1_3[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.1102378794383*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_1_4[area*132+m]>0){
        adjacent_area = rowname_1_4[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.1102378794383*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_2_1[area*132+m]>0){
        adjacent_area = rowname_2_1[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.0274880587289*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_2_2[area*132+m]>0){
        adjacent_area = rowname_2_2[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.0274880587289*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_2_3[area*132+m]>0){
        adjacent_area = rowname_2_3[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.0274880587289*Previous_occured[adjacent_area*132+m]
      }
      if (rowname_2_4[area*132+m]>0){
        adjacent_area = rowname_2_4[area*132+m]
        Adjacent_previous_occured[area*132+m] = Adjacent_previous_occured[area*132+m] + 
          0.0274880587289*Previous_occured[adjacent_area*132+m]
      }
    }
  }
  data$Adjacent_previous_occured <- Adjacent_previous_occured
  rm(Previous_occured)
  rm(Adjacent_previous_occured)

  # calculate number of months since last crime event happened in each cell 
  data$Last_occured <- 0
  Last_occured <- data$Last_occured
  for (area in 0:16514){
    lo = 1 
    for (m in 1:132){
      num = Num_crime[area*132+m]
      Last_occured[area*132+m] = lo
      if(num > 0)
        lo = 1
      else
        lo = lo + 1
    }
  }
  data$Last_occured = Last_occured
  rm(Num_crime)
  rm(Last_occured)

  # join the total crime density
  data$Crime_density <- left_join(data, data_density)$Density
  data$Total_density <- left_join(data, total_density)$Density

  data[is.na(data)] <- 0

  # join demographic data
  data <- left_join(data, demograph, by = c("Round_lon", "Round_lat", "District"))

  # drop columns
  data <- select(data, -rowname, -rowname_1_1, -rowname_1_2, -rowname_1_3, -rowname_1_4,
                -rowname_2_1, -rowname_2_2, -rowname_2_3, -rowname_2_4)

  # drop rows (2006.1 - 2006.6)
  data <- subset(data, Year >= 2007 | Month >= 7)

  month <- data.frame(Month = 1:12, 
                      Cha = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'))
  data$Month <- left_join(data, month, by = "Month")$Cha

  # create target variable
  data$Response = as.integer(data$Num_crime > 0)

  data <- select(data, -Num_crime)

  return(data)
  }

# full data preparation step
full_step <- function(crime_type){
  data <- crime_subset(crime_type)
  day <- subset(data, Period == 'DAY')
  day <- preparation(day)
  write.csv(day, paste(crime_type, '_day.csv', sep = ""), row.names = FALSE)
  night <- subset(data, Period == 'NIGHT')
  night <- preparation(night)
  write.csv(night, paste(crime_type, '_night.csv', sep = ""), row.names = FALSE)
}



