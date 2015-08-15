Final Project for Stat 133
================

 the Description of the Project
------------------------------
In this project, we analyzed the data about storms that hit North Atlantic(NA) and East Pacific(EP) from 1980 to 2010, in terms of location, wind speed, surface pressure, date of occurence, and duration. The data is obtained from __International Best Track Archive for Climate Stewardship (IBTrACS)__  [(ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/cxml/year)](ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/cxml/year). 


 the Author Informantion
------------------------------
 - Name: Hye Soo Choi
 - Email: hyeso0oa@gmail.com
 
 the Organization of Directories and Files
-------------------------------------------


 *File skeleton.R* : an R script with the commands to create the directories and the README.md file. This file also contains the commands to download the raw data files to the corresponding directory.

 *Directory code* : contains all R script codes:
 - Data_cleaning.R : process and clean raw data and produce cleaned data.
 - Data_analysis.R : With cleaned data, analyze storms by frequency table of number of storms per month and year, its descriptive statistics and barplot, and regression on wind speed and surface pressure.
 - Vilsualization.R : draws word map with storm trajectories on East Pacific and North Atalantic.

 *Directory data* : contains cleaned data.
 
 *Direcory images* : all plots drawn in R scipts in code folder are saved both in PDF and in PNG.
 
 *Directory report* : contains final report in Rmd and in pdf.
 
 *Directory resources*: contains resources related to this project.
 
 *Directory rawdata*: contains rawdata that is obtained from __International Best Track Archive for Climate Stewardship (IBTrACS)__  [(ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/cxml/year)](ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r06/wmo/cxml/year). 
 
Specific instructions or comments
------------------------------

To reproduce, first run skeleton.R to create all directory that is necessary for saving cleaned data and plots in case they does not exist yet.
Second, run the Rscript in code folder in the following order:

 - 1)  Data_cleaning.R
 - 2)  Data_analysis.R
 - 3)  Visualization.R
 
 Third, to reproduce the final report in PDF, knitr report.Rmd.
 