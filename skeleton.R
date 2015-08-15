

##----------------------------------------------------------------------------------------------
# Header
##----------------------------------------------------------------------------------------------

# The file skeleton.R is an R script with the commands 
# to create the directories and the README.md file.
# This file also contains the commands 
# to download the raw data files to the corresponding directory.

# Any resource files downloaded in resources folder, 
# skeleton.R  will also have the commands to download such information.


##----------------------------------------------------------------------------------------------
# Create the directories within package
##----------------------------------------------------------------------------------------------

dir.create('./code')
dir.create('./rawdata')
dir.create('./data')
dir.create('./resources')
dir.create('./report')
dir.create('./images')
file.create('./README.md')



##----------------------------------------------------------------------------------------------
# Download rawdata
##----------------------------------------------------------------------------------------------
# we downlaods hdat file from 
# 'ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat'
# and save it as a local file in 'rawdata' folder

# This rawdata contains contains storm record for the North Atlantic basin
# and will be used for data analysis part

download.file('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/hurdat_format/basin/Basin.NA.ibtracs_hurdat.v03r06.hdat
              ', './rawdata/Basin.NA.ibtracs_hurdat.v03r06.hdat' )


# These two rawdata contains storm record for the North Atlantic basin and East Pacific, respectively,
# and will be used for visualization.

download.file('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv',
              './rawdata/Basin.EP.ibtracs_wmo.v03r06.csv')
download.file('ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv',
              './rawdata/Basin.NA.ibtracs_wmo.v03r06.csv')




