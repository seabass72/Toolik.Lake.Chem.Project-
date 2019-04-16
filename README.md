# ARC Lakes Physical and Chemical dataset for 2010-2014

## Summary

This dataset was prepared for Environmental Data Analytics (ENV 872L) for our final project at Duke University, Spring 2019. 

The dataset includes physical and chemical water quality measurements for various lakes that are located near the Toolik Lake Research Station in Alaska for the years: 2010-2014. The Data was collected as part of the Long Term Ecological Research (LTER) for the National Science Foundation (NSF).

## Database Information

The data was collected from the LTER Network Data Portal wetsite. The URL for the dataset ARC Lakes Physical and Chemical dataset for 2010-2014 can be found here: https://portal.lternet.edu/nis/mapbrowse?packageid=knb-lter-arc.10582.3

The CSV was simply downloaded and imported into R and saved as `ARC_Lakes_Physchem_2010-2014.csv`

## Database Content Information

From NTL-LTER site under metadata section:

### Site descriptors 

Site discriptors include lake name, date, time, and depth. The number of lakes that were sampled were equal to 74. However to have a better understanding of chemical and physical parameter trends, the five lakes with the most observations were used: Toolik Lake, Lake E5, Lake Fog 2, Lake I5, and Lake I7. Time was also recorded in Alaska Daylight Savings Time. 


### Physical and Chemical variables 

From 2002 to present, A hydrolab Surveyor 4 data logger and Datsonde DS5 multiprobe were used to measure: depth (m), pH, specific conductivity (uS/cm), dissolved oxygen (mg/L), in-vivo fluorescence, and photosynthetic active radiation (PAR) at half meter to meter intervals depending on the depth of the lake. To determine the secchi depth, A secchi disk (22 cm black and white disk) was used. There was only one depth for each site that was taken on a particular day for secchi depth.

column names without descriptors are self explanatory (* columns used for analysis):

SortChem_.: number used to identify water samples
*Site: lake name 
*Date: year-month-day
*Rounded.Depth..m.: rounded value of depth (m)
hydrolab.depth: depth measured by the hydrolab(m)
*Time_hr_dst: time that the measurement was taken in Alaskan Daylight Saving time
IBVSvr4a_V: internal voltage of the battery of the data logger
*Temp_C: water Temperature at depth of sample (m)
*Cond_uS: electrical conductivity at depth (uS/cm)
*pH: opposite of the log of the hydrogen ion concentration
DO_Sat: the percen saturation of dissolved oxygen at depth
*DO_mg.l: The concentration of dissolved oxygen at depth (mg/L)
DO_umoles.L: The concentration of dissolved oxygen at depth (umoles/L)
Chl_V: In Vivo measurement of chlorophyll-a concentration (volts)
*Chl_ug.l: In Vivo measurement of chlorophyll-a concentration (ug/L)
*PAR_uEm2s: Photosynthetically active radiation measured below the water surface (uE/m2/sec)
refPAR_uEm2s: Photosynthetically active radition measured with deck unit to correct for changes in ambient light (uE/m2/sec)
Hydrolab: indicates what measurements were used to for measurements
*Secchi_m: Measure of water transparency by depth (m)


## Naming conventions and file formats
Files are named according to the following naming convention: `databasename.datatype.details.stage.format`, where: 

**databasename** refers to the database from where the data originated

**datatype** is a description of data 

**details** are additional descriptive details, particularly important for processed data 

**stage**refers to the stage in data management pipelines (e.g., raw, cleaned, or processed)

**format** is a non-proprietary file format (e.g., .csv, .txt)

## Additional Information and Support
For more information, please contact the data assembler, **Sebastian bognar** (sb500@duke.edu)






