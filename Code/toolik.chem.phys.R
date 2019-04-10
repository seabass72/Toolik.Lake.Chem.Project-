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
library(ggpubr)
library(FSA)
library(corrplot)
library(trend)


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

ARC.PhysChem.2010_14.Raw <- read.csv("./Data/RAW/ARC_Lakes_Physchem_2010-2014.csv")

# view the dataset

View(ARC.PhysChem.2010_14.Raw)


########################### Clean up the Dataset #############################

# 1 remove unnessary columns 

ARC.PhysChem.selectedcolumns.Raw <- ARC.PhysChem.2010_14.Raw %>%
  select("Site", "Date", "Rounded.Depth..m.", "Time_hr_dst","Temp_C","Cond_uS", "pH","DO_mg.l","Chl_ug.l" , "PAR_uEm2s", "Secchi_m" ) 
  
# 2 make periods (missing values into NAs)
  
  ARC.PhysChem.selectedcolumns.Raw[ ARC.PhysChem.selectedcolumns.Raw == "." ] <- NA
  ARC.PhysChem.selectedcolumns.Raw[ ARC.PhysChem.selectedcolumns.Raw == "" ] <- NA
  
# 3 rename the columns with simpler names 
  
colnames(ARC.PhysChem.selectedcolumns.Raw) <- c("Site", "Date", "Depth_m", "Time_hr","Temp_C", "Cond_uS", "pH","Dissolved_Oxygen","Chla_ug", "PAR", "Secchi_Depth"  )
  
  
# 4 remove rows that have nas
  
ARC.PhysChem.2010_2014.cleaned <- drop_na(ARC.PhysChem.selectedcolumns.Raw)

# 5 change columns that are numbers that are miss cateloged as factors with as.numeric and as.character

ARC.PhysChem.2010_2014.cleaned$Depth_m <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$Depth_m))

ARC.PhysChem.2010_2014.cleaned$Temp_C <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$Temp_C))

ARC.PhysChem.2010_2014.cleaned$Cond_uS <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$Cond_uS))

ARC.PhysChem.2010_2014.cleaned$pH <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$pH))

ARC.PhysChem.2010_2014.cleaned$Dissolved_Oxygen <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$Dissolved_Oxygen))

ARC.PhysChem.2010_2014.cleaned$Chla_ug <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$Chla_ug))

ARC.PhysChem.2010_2014.cleaned$PAR <- as.numeric(as.character(ARC.PhysChem.2010_2014.cleaned$PAR))

ARC.PhysChem.2010_2014.cleaned$Secchi_Depth <- as.character(ARC.PhysChem.2010_2014.cleaned$Secchi_Depth)

ARC.PhysChem.2010_2014.cleaned$Time_hr <- as.character(ARC.PhysChem.2010_2014.cleaned$Time_hr)

# 5 make sure that date column is in the date format 

ARC.PhysChem.2010_2014.cleaned$Date <- as.Date(ARC.PhysChem.2010_2014.cleaned$Date, "%d-%B-%y")


# 6 make secchi depths where hit bottom a number instead of a character (-1)

ARC.PhysChem.2010_2014.cleaned$Secchi_Depth[ARC.PhysChem.2010_2014.cleaned$Secchi_Depth == ">Xm"] <- -1

ARC.PhysChem.2010_2014.cleaned$Secchi_Depth <- as.numeric(ARC.PhysChem.2010_2014.cleaned$Secchi_Depth)


# 8 Save the processed dataset

write.csv(ARC.PhysChem.2010_2014.cleaned,file = "./Data/Processed/ARC.PhysChem.2010_2014.Processed.csv",row.names = FALSE)


######################## Part 2 data Exploration ######################################


######## summary code section 

# working with the cleaned dataset

ARC.PhysChem.2010_2014.processed<- ARC.PhysChem.2010_2014.cleaned



# 1 look at the summary for each of the columns present in the dataset

summary(ARC.PhysChem.2010_2014.processed)
  
# 2 the first 5 rows of the dataset 

head(ARC.PhysChem.2010_2014.processed)

# 3 structure of dataframe

str(ARC.PhysChem.2010_2014.processed)

# 4 dimensions of the dataset

dim(ARC.PhysChem.2010_2014.processed)

# 5 column names in the proceesed Toolik dataset

colnames(ARC.PhysChem.2010_2014.processed)


############### Data Exploration Graphs 



# 1 Plot to see Chlorophyll A concentration vs. Depth 

#filter for lakes with the most observations (top 5)

ARC.PhysChem.5_major_Lakes.processed <- ARC.PhysChem.2010_2014.processed %>%
  filter(Site == "Toolik"| Site == "E5" | Site == "Fog 2"|  Site == "I7"|  Site == "I5")



CHLA_DEPTH_PLOT <-  ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Chla_ug, y= Secchi_Depth, color = Site)) +
  geom_point()+
  theme_Final_Project+
  scale_y_reverse( lim=c(20,0))+
  scale_colour_manual(values = c('#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'))+
  ylab("Depth (m)")+
  xlab(expression("Chlorophyll a Concentration"~"("*mu*g/L*")"))+
  ggtitle(" Chlorophyll a Concentrations vs. Depth for Lakes in the Toolik Region, Alaska")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(color='Lake Name')

 CHLA_DEPTH_PLOT
 
 
 # 2 plot to see how chlorophyll a changes over Time (2010-2014)
 
 CHLA_Time_PLOT <-  ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Date, y= Chla_ug, color = Site)) +
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
 
 
 
 # Water quality Measurements accross the five lakes (use gg arrange to make a graph with 5 plots)
 
 
 #  temperature plot
 
 LAKE.v.TEMP.PLOT <- ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Site, y = Temp_C, fill =Site ))+
   geom_boxplot()+
   theme_Final_Project+
   xlab("Lake Name")+
   ylab(expression("Temperature"*~degree*C))+
   theme(legend.position = "none")+
   scale_fill_manual(values = c('#b2182b','#ef8a62','#fddbc7','#e0e0e0','#999999'))+
   theme(panel.background = element_rect(fill = "white", 
                                   size = 2, linetype = "solid"))+
   theme(panel.grid.major.y =element_blank())+
   theme(text = element_text(size=12))+
   ggtitle(" Temperature ")+
   theme(plot.title = element_text(hjust = 0.5))
   
 LAKE.v.TEMP.PLOT
 
 # conductivity plot 
 
 
 LAKE.v.COND.PLOT <- ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Site, y = Cond_uS, fill =Site ))+
   geom_boxplot()+
   theme_Final_Project+
   xlab("Lake Name")+
   ylab("Conductivity (uS/cm)")+
   theme(legend.position = "none")+
   scale_fill_manual(values = c('#b2182b','#ef8a62','#fddbc7','#e0e0e0','#999999'))+
   theme(panel.background = element_rect(fill = "white", 
                                         size = 2, linetype = "solid"))+
   theme(panel.grid.major.y =element_blank())+
   theme(text = element_text(size=12))+
   ggtitle(" Conductivity ")+
   theme(plot.title = element_text(hjust = 0.5))
 
 LAKE.v.COND.PLOT
 
 #pH plot
 LAKE.v.pH.PLOT <- ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Site, y = pH, fill =Site ))+
   geom_boxplot()+
   theme_Final_Project+
   xlab("Lake Name")+
   ylab("pH")+
   theme(legend.position = "none")+
   scale_fill_manual(values = c('#b2182b','#ef8a62','#fddbc7','#e0e0e0','#999999'))+
   theme(panel.background = element_rect(fill = "white", 
                                         size = 2, linetype = "solid"))+
   theme(panel.grid.major.y =element_blank())+
   theme(text = element_text(size=12))+
   ggtitle(" pH  ")+
   theme(plot.title = element_text(hjust = 0.5))
 
 LAKE.v.pH.PLOT
 
 # dissolved oxygen plot
 
 LAKE.v.DO.PLOT <- ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Site, y = Dissolved_Oxygen, fill =Site ))+
   geom_boxplot()+
   theme_Final_Project+
   xlab("Lake Name")+
   ylab("DO (mg/L)")+
   theme(legend.position = "none")+
   scale_fill_manual(values = c('#b2182b','#ef8a62','#fddbc7','#e0e0e0','#999999'))+
   theme(panel.background = element_rect(fill = "white", 
                                         size = 2, linetype = "solid"))+
   theme(panel.grid.major.y =element_blank())+
   theme(text = element_text(size=12))+
   ggtitle(" Dissolved Oxygen ")+
   theme(plot.title = element_text(hjust = 0.5))
 
 LAKE.v.DO.PLOT
 
 # chla plot
 
 LAKE.v.chla.PLOT <- ggplot(ARC.PhysChem.5_major_Lakes.processed, aes(x= Site, y = Chla_ug, fill =Site ))+
   geom_boxplot()+
   theme_Final_Project+
   xlab("Lake Name")+
   ylab(expression("Chla Concentration"~"("*mu*g/L*")"))+
   theme(legend.position = "none")+
   scale_fill_manual(values = c('#b2182b','#ef8a62','#fddbc7','#e0e0e0','#999999'))+
   theme(panel.background = element_rect(fill = "white", 
                                         size = 2, linetype = "solid"))+
   theme(panel.grid.major.y =element_blank())+
   theme(text = element_text(size=12))+
   ggtitle(" Chlorophyll a ")+
   theme(plot.title = element_text(hjust = 0.5))
 
 LAKE.v.chla.PLOT

 # using gg arrange to put all the plots together 
 
 WATER_Q_Total_Plot <- ggarrange(LAKE.v.TEMP.PLOT, LAKE.v.COND.PLOT, LAKE.v.DO.PLOT, LAKE.v.pH.PLOT, LAKE.v.chla.PLOT, ncol = 2, nrow = 3)
 WATER_Q_Total_Plot_complete<-annotate_figure(WATER_Q_Total_Plot,
                 top = text_grob("Distribution of Physical and Chemical Water Quality parameters for Lakes in the Toolik Region", color = "black", face = "bold", size = 14))

 WATER_Q_Total_Plot_complete
 
 
 ################## Data Analysis section
 
 # 1 separate water quality parameters for each of the five lakes 
 
  #A Toolik Lake
  ARC.PhysChem.Toolik.processed <- ARC.PhysChem.5_major_Lakes.processed %>%
    filter(Site =="Toolik")
  
  #B LAke E5
  
  ARC.PhysChem.E5.processed <- ARC.PhysChem.5_major_Lakes.processed %>%
    filter(Site =="E5")
  
  #C Lake FOG 2
  
  ARC.PhysChem.FOG_2.processed <- ARC.PhysChem.5_major_Lakes.processed %>%
    filter(Site =="Fog 2")
  
  #D Lake I7
  
  ARC.PhysChem.I7.processed <- ARC.PhysChem.5_major_Lakes.processed %>%
    filter(Site =="I7")
  
  #E Lake I5
  
  ARC.PhysChem.I5.processed <- ARC.PhysChem.5_major_Lakes.processed %>%
    filter(Site =="I5")
 
 
 #2 Check for Normality in the physical and Chemical parameters(temp, cond, pH, DO, and chla)
 
    #A Toolik Lake Normality shapiro wilks test
  
          shapiro.test(ARC.PhysChem.Toolik.processed$Temp_C)
          shapiro.test(ARC.PhysChem.Toolik.processed$Cond_uS)
          shapiro.test(ARC.PhysChem.Toolik.processed$pH)
          shapiro.test(ARC.PhysChem.Toolik.processed$Chla_ug)
          shapiro.test(ARC.PhysChem.Toolik.processed$Dissolved_Oxygen)
          shapiro.test(ARC.PhysChem.Toolik.processed$PAR)
          shapiro.test(ARC.PhysChem.Toolik.processed$Secchi_Depth)
          
          
   #B  Lake E5 Normality shapiro wilks test
          
          shapiro.test(ARC.PhysChem.E5.processed$Temp_C)
          shapiro.test(ARC.PhysChem.E5.processed$Cond_uS)
          shapiro.test(ARC.PhysChem.E5.processed$pH)
          shapiro.test(ARC.PhysChem.E5.processed$Chla_ug)
          shapiro.test(ARC.PhysChem.E5.processed$Dissolved_Oxygen)
          shapiro.test(ARC.PhysChem.E5.processed$PAR)
          shapiro.test(ARC.PhysChem.E5.processed$Secchi_Depth)
  
  #C Lake Fog 2 Normality shapiro wilks test
          
          shapiro.test(ARC.PhysChem.FOG_2.processed$Temp_C)
          shapiro.test(ARC.PhysChem.FOG_2.processed$Cond_uS)
          shapiro.test(ARC.PhysChem.FOG_2.processed$pH)
          shapiro.test(ARC.PhysChem.FOG_2.processed$Chla_ug)
          shapiro.test(ARC.PhysChem.FOG_2.processed$Dissolved_Oxygen)
          shapiro.test(ARC.PhysChem.FOG_2.processed$PAR)
          shapiro.test(ARC.PhysChem.FOG_2.processed$Secchi_Depth)
          
    #D Lake I7 Normality shapiro wilks test
          
          shapiro.test(ARC.PhysChem.I7.processed$Temp_C)
          shapiro.test(ARC.PhysChem.I7.processed$Cond_uS)
          shapiro.test(ARC.PhysChem.I7.processed$pH)
          shapiro.test(ARC.PhysChem.I7.processed$Chla_ug)
          shapiro.test(ARC.PhysChem.I7.processed$Dissolved_Oxygen)
          shapiro.test(ARC.PhysChem.I7.processed$PAR)
          shapiro.test(ARC.PhysChem.I7.processed$Secchi_Depth)
  
  
    #E Lake I5 Normality shapiro wilks test
  
         shapiro.test(ARC.PhysChem.I5.processed$Temp_C)
         shapiro.test(ARC.PhysChem.I5.processed$Cond_uS)
         shapiro.test(ARC.PhysChem.I5.processed$pH)
         shapiro.test(ARC.PhysChem.I5.processed$Chla_ug)
         shapiro.test(ARC.PhysChem.I5.processed$Dissolved_Oxygen)
         shapiro.test(ARC.PhysChem.I5.processed$PAR)
         shapiro.test(ARC.PhysChem.I5.processed$Secchi_Depth)
         
 # 2 check for the homogeneity of variances between the 5 major lakes for water quality parameters
 
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$Temp_C ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$Cond_uS ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$pH ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$Chla_ug ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$Dissolved_Oxygen ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$PAR ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         bartlett.test(ARC.PhysChem.5_major_Lakes.processed$Secchi_Depth ~ ARC.PhysChem.5_major_Lakes.processed$Site)
         
################ 1 Statistical test significant difference betweem 5 lakes for (temp, cond, pH, DO, and chla)
         
         
         # A. significant difference between lakes for (temp, cond, pH, DO, and chla) using kruskal wallis test
         
        Kruskal_Temp <-kruskal.test(ARC.PhysChem.5_major_Lakes.processed$Temp_C ~ ARC.PhysChem.5_major_Lakes.processed$Site)
        Kruskal_Cond <- kruskal.test(ARC.PhysChem.5_major_Lakes.processed$Cond_uS ~ ARC.PhysChem.5_major_Lakes.processed$Site)
        Kruskal_pH <-  kruskal.test(ARC.PhysChem.5_major_Lakes.processed$pH ~ ARC.PhysChem.5_major_Lakes.processed$Site)
        Kruskal_Chla <-  kruskal.test(ARC.PhysChem.5_major_Lakes.processed$Chla_ug ~ ARC.PhysChem.5_major_Lakes.processed$Site)
        Kruskal_DO <-  kruskal.test(ARC.PhysChem.5_major_Lakes.processed$Dissolved_Oxygen ~ ARC.PhysChem.5_major_Lakes.processed$Site)
       
        # B. see which lake are significant different for (temp, cond, pH, DO, and chla) using the dunn test
        
        dunnTest(ARC.PhysChem.5_major_Lakes.processed$Temp_C , ARC.PhysChem.5_major_Lakes.processed$Site)
        dunnTest(ARC.PhysChem.5_major_Lakes.processed$Cond_uS , ARC.PhysChem.5_major_Lakes.processed$Site)
        dunnTest(ARC.PhysChem.5_major_Lakes.processed$pH , ARC.PhysChem.5_major_Lakes.processed$Site)
        dunnTest(ARC.PhysChem.5_major_Lakes.processed$Chla_ug , ARC.PhysChem.5_major_Lakes.processed$Site)
        dunnTest(ARC.PhysChem.5_major_Lakes.processed$Dissolved_Oxygen , ARC.PhysChem.5_major_Lakes.processed$Site)

################ 2 Statistical test multiple linear regression to determine what factors are important in Chla concentrations

# 1 remove chla concentrations that are zero for log transformation of dependent variable
        ARC.PhysChem.all_Lakes.nozeros<- ARC.PhysChem.2010_2014.processed %>%
        filter(Chla_ug > 0 )
        
        
# check normality of the of dependent variable (chlorophyll a) with log transformation
        
        # use the shapiro wilks test to test normality
        
        shapiro.test(log(ARC.PhysChem.all_Lakes.nozeros$Chla_ug)) # much improved (p value = 10^-16 -> 0.00214)
        
        # look at the improvement in qqplot 
        
        qqnorm(log(ARC.PhysChem.all_Lakes.nozeros$Chla_ug))
        qqline(log(ARC.PhysChem.all_Lakes.nozeros$Chla_ug))
        
        
# multiple linear regression (full model)
        
CHLA_MODEL <- lm(data = ARC.PhysChem.all_Lakes.nozeros, log(Chla_ug)~Temp_C + Cond_uS + pH + Dissolved_Oxygen+PAR+Secchi_Depth)

summary(CHLA_MODEL)


# check Diagnostic Plots for Linear Regression Analysis

 plot(CHLA_MODEL)
 
 # A Residuals vs. fitted plot (linear relationship). horizontal line that follows zero that indicates that the their is a linear relationship

# B qq plot (normality). relatively straight line and little deviation at bottom and top, but good enough to satisfy laws of normality

 # c scale to location plot (assumption of equal variance), the points look relatively randomly distributed and the line is almost horizontal
 
 # d residuals vs. leverage plot (check for outliers), none of the outliers are outside of the cook distance so no need to remove outliers

 
 # use Step-AIC to find the model of best fit
 
 step(CHLA_MODEL)
 
 # model of best fit
 
 CHLA_MODEL_Best <- lm(data = ARC.PhysChem.all_Lakes.nozeros, log(Chla_ug)~Temp_C + pH + PAR+Secchi_Depth+Cond_uS)
 
 summary(CHLA_MODEL_Best)
 
 step(CHLA_MODEL_Best)
 
 ########### statistical test 3 mann-kendall test for Toolik Lake looking for change points (temp, cond, pH, DO, par, and chla)
 
 
 #Toolik lake mann-kendall test intial for chla 
 
             # filter out samples for single depth (depth == 0)
             
             ARC.PhysChem.Toolik_zero.processed<- ARC.PhysChem.Toolik.processed %>%
               filter(Depth_m == 0)
             
             # Initial Mann Kendall Test
             mk.test(ARC.PhysChem.Toolik_zero.processed$Chla_ug)
             
             # use the pettitt test determine the change points in the seasons 
             
             pettitt.test(ARC.PhysChem.Toolik_zero.processed$Chla_ug) # show a likely change point at point 33
             
             # confirm change point with mann kendall test for [1:32]
             
             mk.test(ARC.PhysChem.Toolik_zero.processed$Chla_ug[1:32])
             
             # use pettitt test on latter half of data to find next change point
             
             pettitt.test(ARC.PhysChem.Toolik_zero.processed$Chla_ug[33:63]) # not significant 
             
             
# mann kendall test for temp in toolik lake
             
             # Initial Mann Kendall Test
             mk.test(ARC.PhysChem.Toolik_zero.processed$Temp_C)
             
             # use the pettitt test determine the change points in the seasons 
             
             pettitt.test(ARC.PhysChem.Toolik_zero.processed$Temp_C) # show a likely change point at point 31
             
             # confirm change point with mann kendall test for [1:30]
             
             mk.test(ARC.PhysChem.Toolik_zero.processed$Temp_C[1:30])
             
             # use pettitt test on latter half of data to find next change point
             
             pettitt.test(ARC.PhysChem.Toolik_zero.processed$Chla_ug[31:63]) # not significant 
             
             
# mann kendall test for pH in toolik lake
             
             # Initial Mann Kendall Test
             mk.test(ARC.PhysChem.Toolik_zero.processed$pH) # no significant change point for pH
             
# mann kendall test for conductivity in toolik lake
             
             # Initial Mann Kendall Test
             mk.test(ARC.PhysChem.Toolik_zero.processed$Cond_uS) # not significant 
             
            
# mann kendall test for dissolved O in toolik lake
             
             # Initial Mann Kendall Test
             mk.test(ARC.PhysChem.Toolik_zero.processed$Dissolved_Oxygen)
             
             # use the pettitt test determine the change points in the seasons 
             
             pettitt.test(ARC.PhysChem.Toolik_zero.processed$Dissolved_Oxygen) # show a likely change point at point 32
             
             # confirm change point with mann kendall test for [1:31]
             
             mk.test(ARC.PhysChem.Toolik_zero.processed$Dissolved_Oxygen[1:31])
             
             # use pettitt test on latter half of data to find next change point
             
             pettitt.test(ARC.PhysChem.Toolik_zero.processed$Chla_ug[31:63]) # not significant
             
             
 # mann kendall test for PAR in toolik lake
             
             # Initial Mann Kendall Test
             mk.test(ARC.PhysChem.Toolik_zero.processed$PAR) # not significant, no further analysis with pettitt test
             
            
             
             
             
             
            
             
            