# Carvins_Carbon

## Summary
Analysis of changes in dissolved organic matter (DOM) across the lotic-lentic transition in Carvins Cove Reservoir

## Recreating all analysis steps 

Run Install_Packages.R in './Scripts/', then scripts 2a-2c, 3, 4 in the same folder in order. These scripts will download all need packages and data to rerun the entire analysis. 

Next the following scripts located in './Scripts/' can be run in any order to recreate figures and analyses. The scripts include

- 3_Hydro_TimeSeries_Figure.R is used to make hydrology timeseries figures and create offset between HPB gauge and nearby USGS site
- 4_Compile_Chemistry_Sensor_Hydro.R Make joined data frame of chemistry variables to associated sensor and hydrology data for use in breakpoints script and data exploration
- 5_Iso_Rain_DOM_Timeseries.Rmd make timeseries of d-excess and DOM compared to daily rain 
- 6_Sensor_Figures.Rmd plots of Water Temp, Specific Conductance and Dissolved Oxygen Data. And Stream water fraction calculations
- 7_KruskalWallis.Rmd script to run KW tests across site types for DOM and water quality variables
- 8_Breakpoints.Rmd run breakpoint analysis to identify stream-reservoir boundary and where DOM and water quality change along this gradient 


## Recreating figures 

To just rerun the data analysis scripts and generate figures, the data generated in scripts 2-4 are provided in the Data Folder. Scripts 5-8 can be run using this data.


## Data folder 
This folders holds data that is generated from data compilation scripts. This is also where csvs for sampling distance and site max depth are located.

