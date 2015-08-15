##----------------------------------------------------------------------------------------------
#   Header
##----------------------------------------------------------------------------------------------
# Author : Hye Soo Choi
# Date: August 13, 2015
# Description: The third part of the project consists of visualizing the trajectory of the storms 
# in both the East Pacific (EP) and the North Atlantic (NA) basins, for the period 1980-2010.
#
# For these visualizations, following data were used:
#  ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.EP.ibtracs_wmo.v03r06.csv
#  ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/csv/basin/Basin.NA.ibtracs_wmo.v03r06.csv
# 
#
##----------------------------------------------------------------------------------------------
#   Packages
##----------------------------------------------------------------------------------------------
library(readr)
library(ggplot2)
library(dplyr)
library(maps)

##----------------------------------------------------------------------------------------------
#   Data import
##----------------------------------------------------------------------------------------------

# storm data in the East Pacific (EP) basin
ep <- read_csv('./rawdata/Basin.EP.ibtracs_wmo.v03r06.csv', skip = 1)
# remove unnecessary first line
ep <- ep[-1, ] 


# storm data in the North Atlantic (NA) basin
na <- read_csv('./rawdata/Basin.NA.ibtracs_wmo.v03r06.csv', skip = 1)
# remove unnecessary first line
na <- na[-1, ] 

##----------------------------------------------------------------------------------------------
#   Data process
##----------------------------------------------------------------------------------------------

# merge two data ep and na into single data frame naemd 'epna'
epna <- rbind(ep, na)

# Extract month and add it as new column named 'month'
epna$month <- format(strptime(epna$ISO_time, '%Y-%m-%d %H:%M:%S'), '%m')

# For future use, rename column names: year, serial.num, wind, press
epna <- epna %>% rename(year = Season)
epna <- epna %>% rename (serial.num = Serial_Num)
colnames(epna)[c(11,12)] <- c('wind','press')

# Select only variables that we are intersted in :
#     year, Basin, month, Latitude, Longitude, wind, press, serial.num
epna <- epna %>% select(year, Basin, month, Latitude, Longitude, wind, press, serial.num)
colnames(epna) <- tolower(colnames(epna))

# coerce to numeric, if necessary
epna$latitude <- as.numeric(epna$latitude)
epna$longitude <- as.numeric(epna$longitude)
epna$wind <- as.numeric(epna$wind)
epna$press <- as.numeric(epna$press)

# Filter data by 60 >= latitude >= 0 and longitude >= 0 
epna <- epna %>% filter(longitude <= 0, latitude >= 0, latitude <=  60) 

# Filter data by year between 1980-2010
epna <- epna %>% filter(year <= 2010 & year >= 1980)


##----------------------------------------------------------------------------------------------
#   Visualization
##----------------------------------------------------------------------------------------------


# retrieve map that spans over North Atlantic and East Pacific from world map
world <- map_data('world')
world.epna <- world %>% filter(long <= 0, lat >=  0, lat <= 60)


# Draw all storm tracks between 1980-2010 on the map of NA and EP
gg.map <- ggplot() + geom_polygon (data = world.epna, 
             aes(x = long, y = lat, group = group), colour="#dd855c40", fill="#dfc68a")

epna.map <- gg.map + geom_path(data = epna, aes(x = longitude, y = latitude, 
               col = cut(wind, c(0,50,100,150,200), include.lowest = TRUE), 
               group = serial.num), size = 0.3) +
               xlab('Longitude') + 
               ylab('Latitude') + 
               theme(panel.background = element_rect(fill = "#d7e5e5"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
               scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
               scale_color_manual(values=c('#5ab7fd', '#2ea5ff', '#0091ff','#4494d1'),
                                  'Wind (knots)')

# Save the plot in PDF and in PNG
pdf('./images/EPNAmap.pdf', width = 12, height = 4)
epna.map + ggtitle('Storm trajectories from 1980 to 2010')
dev.off()

png('./images/EPNAmap.png', width = 720, height = 240)
epna.map  + ggtitle('Storm trajectories from 1980 to 2010')
dev.off()

# ======================= Storm tracks per month =================================================
# Draw storm tracks per month between 1980 - 2010. 
# and save it in PDF and in PNG
pdf('./images/EPNAmap_month.pdf', width = 18, height = 9)
epna.map + facet_wrap( ~ month) + ggtitle('Storm trajectories by month from 1980 to 2010')
dev.off()

png('./images/EPNAmap_month.png', width = 720, height = 360)
epna.map + facet_wrap( ~ month) + ggtitle('Storm trajectories by month from 1980 to 2010')
dev.off()

# ===================== Strom tracks per year =================================================

# storm tracks in 1980s
epna.1980s <- epna %>% filter (year %in% 1980:1989) 

# map that shows storm tracks in 1980s
map.1980s <- gg.map + geom_path(data = epna.1980s, aes(x = longitude, y = latitude, 
             col = cut(wind, breaks = c(0, 50, 100, 150, 200), include.lowest = TRUE),
             group = serial.num), size = 0.3) +
             xlab('Longitude') + 
             ylab('Latitude') +
             theme( panel.background = element_rect(fill = "#d7e5e5"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank()) +
             scale_x_continuous(expand = c(0,0)) + 
             scale_y_continuous(expand = c(0,0)) +
             scale_color_manual(values = c('#5ab7fd', '#2ea5ff', '#0091ff','#4494d1'),
                                'Wind (knots)') 

# Save the map of storm tracks per year in 1980s
pdf('./images/EPNAmap_1980s.pdf', width = 18, height = 9)
map.1980s + facet_wrap( ~ year) + ggtitle("Storm trajectories by year in 1980s")
dev.off()

png('./images/EPNAmap_1980s.png', width = 720, height = 360)
map.1980s + facet_wrap( ~ year) + ggtitle("Storm trajectories by year in 1980s")
dev.off()

# storm tracks in 1990s
epna.1990s <- epna %>% filter (year %in% 1990:1999) 

# map that shows storm tracks in 1990s
map.1990s <- gg.map + geom_path(data = epna.1990s, aes(x = longitude, y = latitude, 
             col = cut(wind, breaks = c(0, 50, 100, 150, 200), include.lowest = TRUE), group = serial.num), 
             size = 0.3) + 
             xlab('Longitude') + 
             ylab('Latitude') +
             theme( panel.background = element_rect(fill = "#d7e5e5"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank()) +
             scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
             scale_color_manual(values = c('#5ab7fd', '#2ea5ff', '#0091ff','#4494d1'), 
                                'Wind (knots)') 

# Save the map of storm trajectories by year in 1990s
pdf('./images/EPNAmap_1990s.pdf', width = 18, height = 9)
map.1990s + facet_wrap( ~ year) + ggtitle("Storm trajectories by year 1990s")
dev.off()

png('./images/EPNAmap_1990s.png', width = 720, height = 360)
map.1990s + facet_wrap( ~ year) + ggtitle("Storm trajectories by year 1990s")
dev.off()

# storm tracks in 2000s
epna.2000s <- epna %>% filter (year %in% 2000:2010) 

# map that shows storm trajectories in 2000s
map.2000s <- gg.map + geom_path(data = epna.2000s, aes(x = longitude, y = latitude, 
        col = cut(wind, breaks = c(0, 50, 100, 150, 200), include.lowest = TRUE), 
        group = serial.num), size = 0.3) + 
        xlab('Longitude') + 
        ylab('Latitude') +
        theme( panel.background = element_rect(fill = "#d7e5e5"),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank()) +
        scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
        scale_color_manual(values = c('#5ab7fd', '#2ea5ff', '#0091ff','#4494d1'), 
                           'Wind (knots)') 

# Save the map of storm trajectories by year in 2000s
pdf('./images/EPNAmap_2000s.pdf', width = 18, height = 9)
map.2000s + facet_wrap( ~ year) + ggtitle("Storm trajectories by year in 2000s")
dev.off()

png('./images/EPNAmap_2000s.png', width = 720, height = 360)
map.2000s + facet_wrap( ~ year) + ggtitle("Storm trajectories by year in 2000s")
dev.off()


##----------------------------------------------------------------------------------------------
#   Save
##----------------------------------------------------------------------------------------------

# Save cleaned data frame as csv file for possible future use
write_csv(epna, './data/EPNA1980_2010.csv')

