##----------------------------------------------------------------------------------------------
#    Header
##----------------------------------------------------------------------------------------------
#
# In this rscript file,
# we will process and clean raw data initially saved in 'raw_data' folder, 
# As a result, we will create two cleaned data frames 'storms' and 'tracks', and
# then save the cleaned data frames as csv files to 'data' folder
#
# For further details,
#
# storms.csv will contain the following columns
#     id: Storm number since year 1851
#     date: MM/DD/Year  (as Date)
#     days: Number of days in which positions are available
#     name: Name of the storm
#
#
# tracks.csv will contain the following columns
#     id: Storm number since year 1851
#     date: MM/DD/Year  (as Date)
#     period: 00h, 06h, 12h, 18h
#     stage: type of stage
#     lat: latitude
#     long: longitude (negative degrees for west)
#     wind: wind speed
#     press: surface pressure
#



##----------------------------------------------------------------------------------------------
#     Required packages
##----------------------------------------------------------------------------------------------

library(readr)
library(stringr)



##----------------------------------------------------------------------------------------------
#    Import data
##----------------------------------------------------------------------------------------------
# import the raw data from 'rawdata' folder
# and save the imported data as a character vector 'storm_data'

storm.data <- read_lines('./rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat')
 
 

##----------------------------------------------------------------------------------------------
#    Data processing and cleaning
##----------------------------------------------------------------------------------------------
# From uncleaned storm_data
# we will first devide the storm_data into two parts: one containg HEADER and the other containing DAILY DATA.
# 
# 
# Then, we will extract 
#     id: Storm number since year 1851
#     date: MM/DD/Year  (as Date)
#     days: Number of days in which positions are available
#     name: Name of the storm
# from HEADER and then
# form a data frame named 'storms' with these extracted values.
# 
# 
# Then, we will extract 
#     id: Storm number since year 1851
#     date: MM/DD/Year  (as Date)
#     period: 00h, 06h, 12h, 18h
#     stage: type of stage
#     lat: latitude
#     long: longitude (negative degrees for west)
#     wind: wind speed
#     press: surface pressure 
# from DAILY DATA and then form a data frame named 'tracks'
# with these extracted values
#
# 
# ================= Devide into HEADER and DAILY DATA ============================== 

# extract only HEADER and only DAILY DATA
# Note: HEADER lines always contains string 'SNBR', but DAILY DATA and TRAILER do not.
# Note: TRAILER lines always contains string 'SRC', but DAILY DATA and HEADER do not.

header <- grep(storm.data, pattern = 'SNBR', value = TRUE)
daily.data <- storm.data[-grep(storm.data, pattern= "SNBR|SRC")]




# ================= Process and clean HEADER ====================================== 
#
#
#
# Because within HEADER numbers and strings are distinguised by either ' ' or '=',
# we will split extracted header lines by either '=' or 'blank' into substrings to separate all numbers and strings
 
str.headers <- strsplit(header, split =  '= *| +', fixed = FALSE)
 


Extract<-function(i, list.str){
# Extracts the i-th string from each character vector in list 'list.str' 
#
# Args: 
#   i  : a positive integer index where the string is to be extracted
#   list.str: a list of character vectors whose i-th elements is to be extracted
#
# Returns:
#   The vector of i-th string in the vector in the list 'list.str'  
  sapply(list.str,function(x){x[i]})
}


 
 
# By using function 'Extract', extracts data,name, days of each storms from header 
# Note: 2nd string in each string vector is date,
#     : 4th string in each string vector is days(duration),
#     : 8th string in each string vector is name.
 
date <- as.Date(Extract(2, str.headers), format ='%m/%d/%Y') # save as Date
days <- as.numeric(Extract(4, str.headers)) 
name <- Extract(8, str.headers)



# create a data frame named 'storms'
# containing id,date,duration(days), and name of storms
# one storm per row
 
n <- length(date) # number of storms
storms <- data.frame(id= 1:n, date= format(date, format = '%m/%d/%Y'), days = days , name= name)



# ================== Process and clean DAILY DATA ===============================
 
# To acquired information about in which stage each storm was,
# we need to extract a letter that represent its stage.
# The result will be one of followings:
# '*' (tropical cyclone stage),
# 'S' (Subtropical stage)
# 'E' (extratropical stage)
# 'W' (wave stage - rarely used)
# 'L' (remanent Low stage - rarely used)
#
#


# Extract Stage letter 
#
StageLetter <- function(str){
# Extracts a letter that represent its stage. 
#
# Args: 
#   str: a string that contains only one of {'*','S','E','W','L'}
#
#
# Returns:
#   a character in {'*','S','E','W','L'} that is in str   
  str_extract_all(str, '\\*|S|E|W|L')
}

stage.letter <- StageLetter(daily.data)

RemoveLastStar <- function(x){
  x[-length(x)]
}

stage.letter <- lapply(stage.letter, RemoveLastStar)
stage.letter <- unlist(stage.letter)

# Transform the extracted stage letter, which is abbreviated, to its full stage name.
# '*' : cyclone,
# 'S' : subtropical
# 'E' : extratropical
# 'W' : wave
# 'L' : remanent low 

Stage <- function (str) { switch (
  str,
  '*' = 'cyclone',
  S = 'subtropical',
  E = 'extratropical',
  W = 'wave',
  'remant low'
  )
}


# stage name of each observations
stage <- sapply(stage.letter, Stage) 


# ============= Extract lat, long, wind, press ========================================

# In DAILY DATA, values can be distingusighed by either stage character ('*', 'S', 'E', 'W', 'L') or ' '.
# we will split DAILY DATA lines by either stage character ('*', 'S', 'E', 'W', 'L') or ' ' into substrings 
# to facilitate extracting procedure

str.daily <- strsplit(daily.data, '\\*|S|E|W|L')

# Extract the part that contains information about latitude, longitude, and pressure

str.daily.info <- lapply(str.daily, function(x) {
  x[2:5]
  })
str.daily.info <- unlist(str.daily.info) 

# extract latitude and convert it as numeric vector
# need to multiply by 0.1 according to notation rule of HURDAT format

lat <- as.numeric(substr(str.daily.info, 1, 3)) * 0.1 


# extract longitude and convert it as numeric vector
# need to multiply by 0.1 according to notation rule of HURDAT format
# Note: this longitude is within the range [0, 360]
#     : therefore, it needs to be converted into a new range [-180,180]

long <- as.numeric(substr(str.daily.info, 4, 7)) * 0.1
temp <- long[long > 180]
long[long > 180] <- temp - 360 # change longitude range from [0, 360] to [-180, 180]

# extract wind speed in knots

wind <- as.numeric(substr(str.daily.info, 8, 11))

# extract pressure in mb

press <- as.numeric(substr(str.daily.info, 12, 16))



# ============ Extract id, date, period =============================================


# date and id for daily data
# Note : each storms lasted for corresponding 'days' value in storms data frame.
#      : on each day, there are 4 observations.

years <- format(rep(date, times = (days * 4)), format = '%Y') # extract year from HEADER
temp <- Extract(i = 1, str.daily) # part containing month/day
date.temp <- str_extract(temp, '[0-9]{2}/[0-9]{2}') # extract month/day from DAILY DATA

date.tracks <- paste(rep(date.temp, each = 4), '/', years, sep='') # concatenate 'month/day' to 'year'
id.tracks <- rep(1:n, times = (days * 4))
period <- rep(c('00h','06h','12h','18h'), length(daily.data))


# =============== Combine all variables by creating a data frame ================================
# Create data frame named 'tracks' by combining the follwing set of varaibles for storms:
# id
# date
# lat: latitude
# long: longitude (negative degrees for west)
# wind: wind speed
# press: surface pressure

tracks <- data.frame(id = id.tracks, date = date.tracks, stage = stage, lat = lat, long = long, wind = wind, press = press)

# Remove those observations where lat, long, wind and press are all zeros

tracks <- subset(tracks, subset = !(lat == 0 & long == 0 & wind == 0 & press == 0))
?subset

tracks %>% filter(lat != 0 | long != 0 | wind != 0| press !=0) 
##----------------------------------------------------------------------------------------------
#   Save 
##----------------------------------------------------------------------------------------------
# save the cleaned data frames as csv files
# the files will be saved in 'data' folder with name 'storms.scv'
#
 
write_csv(storms, './data/storms.csv')
write_csv(tracks, './data/tracks.csv')
 
 
 
 
 