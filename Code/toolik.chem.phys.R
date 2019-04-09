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
library(reshape2)


# theme for project 

theme_Final_Project <- theme_classic(base_size = 12) +
  theme(axis.text = element_text(color = "black"), 
        legend.position = "right", 
        axis.ticks = element_line(colour = "black"),
        panel.border = element_rect(fill= NA,color="black", size=0.5, 
                                    linetype="solid"),
        panel.grid.major.y =element_line(color = "grey"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank(),
        axis.text.x = element_text(angle = 0,  hjust = 1),
        panel.background = element_rect(fill = "#bdbdbd", 
                                          size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white") )


# import the raw dataset 

Toolik_raw <- read.csv("./Data/RAW/ARC_Lakes_Physchem_2010-2014.csv")

# view the dataset

View(Toolik_raw)


########################### Clean up the Dataset #############################

# 1 remove unnessary columns 

Toolik_selected_columns <- Toolik_raw %>%
  select("Site", "Date", "Rounded.Depth..m.", "Time_hr_dst","Temp_C","Cond_uS", "pH","DO_mg.l","Chl_ug.l" , "PAR_uEm2s", "Secchi_m" ) 
  
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

write.csv(Toolik_no_nas,file = "./Data/Processed/Toolik_Chem_Phys_Processed.csv",row.names = FALSE)


######################## Part 2 data Exploration ######################################


######## summary code section 

# working with the cleaned dataset

Toolik_Processed<- Toolik_no_nas

#convert back to Date


# 1 look at the summary for each of the columns present in the dataset

summary(Toolik_Processed)
  
# 2 the first 5 rows of the dataset 

head(Toolik_Processed)

# 3 structure of dataframe

str(Toolik_Processed)

# 4 dimensions of the dataset

dim(Toolik_Processed)

# 5 column names in the proceesed Toolik dataset

colnames(Toolik_Processed)


############### Data Exploration Graphs 



# 1 Plot to see Chlorophyll A concentration vs. Depth 

#filter for lakes with the most observations (top 5)

Toolik_5_MAJOR_Lakes <- Toolik_Processed %>%
  filter(Site == "Toolik"| Site == "E5" | Site == "Fog 2"|  Site == "I7"|  Site == "I5")



CHLA_DEPTH_PLOT <-  ggplot(Toolik_5_MAJOR_Lakes, aes(x= Chla_ug, y= Depth_m, color = Site)) +
  geom_point()+
  theme_Final_Project+
  scale_y_reverse( lim=c(20,0))+
  scale_colour_manual(values = c('#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))+
  ylab("Depth (m)")+
  xlab(expression("Chloropyll a Concentration"~"("*mu*g/L*")"))+
  ggtitle(" Chlorophyll a Concentrations vs. Depth for Lakes in the Toolik Region, Alaska")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color='Lake Name')

 CHLA_DEPTH_PLOT
 
 
 # 2 plot to see how chlorophyll a changes over Time (2010-2014)
 
 CHLA_Time_PLOT <-  ggplot(Toolik_5_MAJOR_Lakes, aes(x= Date, y= Chla_ug, color = Site)) +
   geom_point()+
   theme_Final_Project+
   scale_colour_manual(values = c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60'))+
   xlab("Date")+
   ylab(expression("Chloropyll a Concentration"~"("*mu*g/L*")"))+
   ggtitle(" Chlorophyll a Concentrations vs. Time for Lakes in the Toolik Region, Alaska")+
   theme(plot.title = element_text(hjust = 0.5))+
   labs(color='Lake Name')+
   scale_x_date(date_breaks = "2 month", date_labels = "%m/%Y")+
   theme(axis.text.x = element_text(angle = 45,  hjust = 1))
 
 CHLA_Time_PLOT
 
 
 # 3 comparing water quality parameters for the 5 lakes with facet_wrap plot
 
 
 
 # Water quality Measurements accross the five lakes 
 
 WATER_Q_PLOT <- ggplot(Toolik_5_MAJOR_Lakes, aes(x= Site, y = Temp_C ))+
   geom_boxplot()
   
   
  WATER_Q_PLOT
 
 



