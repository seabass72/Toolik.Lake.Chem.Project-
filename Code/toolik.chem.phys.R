# get working directory

getwd()

# load the necessary packages 

library(tidyverse)
library(ggplot2)
library(lubridate)
library(nlme)
library(lsmeans)
library(multcompView)
library(naniar)


# import the raw dataset 

Toolik_raw <- read.csv("./Data/RAW/ARC_Lakes_Physchem_2010-2014.csv")

# view the dataset

View(Toolik_raw)


########################### Clean up the Dataset #############################

# 1 remove unnessary columns 

Toolik_selected_columns <- Toolik_raw %>%
  select("Site", "Date", "Hydrolab.Depth", "Time_hr_dst","Temp_C","Cond_uS", "pH","DO_mg.l","Chl_ug.l" , "PAR_uEm2s", "Secchi_m" ) 
  
# 2 make periods (missing values into NAs)
  
  Toolik_selected_columns[ Toolik_selected_columns == "." ] <- NA
  Toolik_selected_columns[ Toolik_selected_columns == "" ] <- NA
  
# 3 rename the columns with simpler names 
  
colnames(Toolik_selected_columns) <- c("Site", "Date", "Depth_m", "Time_hr","Temp_C", "Cond_uS", "pH","Dissolved_Oxygen","Chla_ug", "PAR", "Secchi_Depth"  )
  
  
# 4 remove rows that have nas
  
Toolik_no_nas <- drop_na(Toolik_selected_columns)
 
  
  
  
