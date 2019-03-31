library(tidyr)

#Read data file with no.of days in years from 1975 to 2100 ----
nepali_date <- read.csv(file = "calendar_bs.csv", header = TRUE, sep = ",")

str(nepali_date)
Nnepali_date <- (gather(nepali_date, "Month", "NoofDays", 2:13 ))

#Convert Month Column as Factor ----
Nnepali_date$Month <- as.factor(Nnepali_date$Month)

#Rename levels of Factor from string to numbers ----- 
levels(Nnepali_date$Month)[1] <- 3
levels(Nnepali_date$Month)[2] <- 6
levels(Nnepali_date$Month)[3] <- 1
levels(Nnepali_date$Month)[4] <- 5
levels(Nnepali_date$Month)[5] <- 12
levels(Nnepali_date$Month)[6] <- 11
levels(Nnepali_date$Month)[7] <- 2
levels(Nnepali_date$Month)[8] <- 7
levels(Nnepali_date$Month)[9] <- 10
levels(Nnepali_date$Month)[10] <- 8
levels(Nnepali_date$Month)[11] <- 9
levels(Nnepali_date$Month)[12] <- 4

#Assigning base Value ------
base_year_AD <- as.Date("1918/04/13", "%Y/%m/%d")
base_year_BS <- as.Date("1975/01/01", "%Y/%m/%d")
base_day_Constant_BS <- 720876
base_day_Constant_AD <- 700114

#Function to Convert BS date to AD ------
BS_to_AD <- function(Date_to_be_Converted) {
  
  # Convert input date to DATE data type 
  Date_to_be_Converted <- as.Date(Date_to_be_Converted, "%Y/%m/%d" )
  
  #Extract year, month and day from the date to be converted ------
  year <- as.numeric(format(Date_to_be_Converted, "%Y"))
  month <- as.numeric(format(Date_to_be_Converted, "%m"))
  day <- as.numeric(format(Date_to_be_Converted, "%d"))
  
  #Extracting no of days from the year ----
  new_df <- Nnepali_date[Nnepali_date$Year < year,  ]
  no_of_days_from_year <- sum(new_df$NoofDays)
  
  #Extracting no of days from month ------- 
  if (month > 1) {
    new_df1 <- Nnepali_date[Nnepali_date$Year == year & as.numeric(as.character(Nnepali_date$Month)) < month ,  ]
    no_of_days_from_month <- sum(new_df1$NoofDays)
  } else {
    no_of_days_from_month <- 0
  }
  
  #calculating total number of days -------
  total_sum_of_days <- day + no_of_days_from_month + no_of_days_from_year
  
  #Now calculating date ------ 
  Converted_date <- base_year_AD + (total_sum_of_days - 1)
  
  return(Converted_date)
}

# Function to Convert AD to BS-----------

AD_to_BS <- function(Date_to_be_Converted) {
  # prepare dataframe Xnepali_data to work through 
  Xnepali_date <- Nnepali_date
  
  #Order Xnepali data frame in ascending order according to the year 
  Xnepali_date <-  Xnepali_date[order(Xnepali_date$Year),]
  Xnepali_date$CF <- cumsum(Xnepali_date$NoofDays)
  
  
  # Convert input date to DATE data type 
  ADdate <- as.Date(Date_to_be_Converted, "%Y/%m/%d" )
  
  #Calculate no. of days from the base year
  day_from_AD <- as.Date(ADdate) - base_year_AD
  
  closest_Cf <- min(Xnepali_date[Xnepali_date$CF > day_from_AD, "CF"])
  closest_date <- Xnepali_date[Xnepali_date$CF == closest_Cf, "NoofDays"]
  
  BS_Month <- Xnepali_date[Xnepali_date$CF == closest_Cf, "Month"]
  BS_Year <- Xnepali_date[Xnepali_date$CF == closest_Cf, "Year"]
  
  day_to_substracted <- closest_Cf - day_from_AD
  exact_day <- (closest_date - day_to_substracted) + 1
  
  
  converted_BS <- paste(BS_Year, BS_Month, exact_day, sep = "-")
  
  converted_BS <- as.Date(converted_BS)
  
  return(converted_BS)
}







