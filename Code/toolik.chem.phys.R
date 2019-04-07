# get working directory

getwd()

# load the necessary packages 

library(tidyverse)
library(ggplot2)
library(lubridate)
library(nlme)
library(lsmeans)
library(multcompView)


# import the raw dataset 

Toolik_raw <- read.csv("./Data/RAW/ARC_Lakes_Physchem_2010-2014.csv")
