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
library(chron)


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

# 5 change columns that are numbers that are miss cateloged as factors with as.numeric and as.character

Toolik_no_nas$Depth_m <- as.numeric(as.character(Toolik_no_nas$Depth_m))

Toolik_no_nas$Temp_C <- as.numeric(as.character(Toolik_no_nas$Temp_C))

Toolik_no_nas$Cond_uS <- as.numeric(as.character(Toolik_no_nas$Cond_uS))

Toolik_no_nas$pH <- as.numeric(as.character(Toolik_no_nas$pH))

Toolik_no_nas$Dissolved_Oxygen <- as.numeric(as.character(Toolik_no_nas$Dissolved_Oxygen))

Toolik_no_nas$Chla_ug <- as.numeric(as.character(Toolik_no_nas$Chla_ug))

Toolik_no_nas$PAR <- as.numeric(as.character(Toolik_no_nas$PAR))

Toolik_no_nas$Secchi_Depth <- as.character(Toolik_no_nas$Secchi_Depth)

Toolik_no_nas$Time_hr <- as.character(Toolik_no_nas$Time_hr)

# 5 make sure that date column is in the date format 

Toolik_no_nas$Date <- as.Date(Toolik_no_nas$Date, "%d-%B-%y")


# 6 make secchi depths where hit bottom a number instead of a character (-1)

Toolik_no_nas$Secchi_Depth[Toolik_no_nas$Secchi_Depth == ">Xm"] <- -1

Toolik_no_nas$Secchi_Depth <- as.numeric(Toolik_no_nas$Secchi_Depth)


# 8 Save the processed dataset

write.csv(Toolik_no_nas,file = "./Data/Processed/Toolik_Chem_Phys_Processed.csv")


######################## Part 2 data Exploration ######################################

Toolik_Processed<- read.csv("./Data/Processed/Toolik_Chem_Phys_Processed.csv")
  
  
