# Maloney et al. 2024
Used to make all figures and data analysis for: "Blue appendages and temperature acclimation increase survival during acute heat stress in the upside-down jellyfish, Cassiopea xamachana"

## Content

In this repository, you will find a folder of raw data and a folder of data which has been wrangled to be used for statistical analysis / figure making. 
Within the raw data folder, you will find an R file which encompasses how data within the folder was modified in order to create the the files in data. 
Within the data folder, you will find files that can be used directly with the R file StatisticalAnalysis and Figures for the statistical anaysis figures associated with Maloney et al., 2024. 

For additional information about the files listed, please contact the cooresponding author: 
Megan Maloney
mem0294@auburn.edu
or
mem2318@gmail.com

## File Breakdown
This set of analysis contains the following data files which are ready to be used for statistics and figures in RStudio. 
For raw data, see RAW 
1. fieldtempStats 
     - temperature loggers were deployed from March 2022 - September 2022
     - Atlantic and Bay are the two sites where jellyfish were collected from (both in 2021, Atlantic only in 2022). 
     - used to create figure 1A and B and associated statistics
2. hs2021Diff / hs2022Diff
     -  In order to effectively compare individuals across temperatures, mean rate of bell pulsation before the first temperature change occurred (the morning of the experiment after they had been in their experimental tank overnight for ambient individuals; or 30 minutes after relocation, but before temperature change for elevated individuals), hereafter, the baseline bell pulsation rate, was subtracted from from the mean pulsation rate at each temperature.
     - used to create figures 1A, 2A, 3A and 3B.
3. TODall
     - file contains: sampleName	sex	temp	color	shock	lengthBase	lengthPA	lengthPreStress	lengthPostStress	tempDeath	DateStress	Location

     



