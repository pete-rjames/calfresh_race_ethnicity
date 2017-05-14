# calfresh_race_ethnicity

California Department of Social Services (CDSS) publishes county-level statistics on the race and ethnicity of households participating in CalFresh, the food stamps/SNAP program for California. 

Annual snapshots for the month of July can be downloaded from the [CDSS Data Portal](http://www.cdss.ca.gov/inforesources/Research-and-Data/CalFresh-Data-Tables). Separate reports are produced for participants recieving federal and state funding. 

This repo contains an R script that:

* Downloads federal and state reports from 2007 - 2016 
* Imports and aggregates data into a single, county-level time series with the following features:
  * combined totals for participating households (federal and state funding)
  * customized aggregation of race & ethnicity characteristics (see [variable names lookup](https://github.com/pete-rjames/calfresh_race_ethnicity/blob/master/variable_names_358.csv) for mapping)
  * flag variable; if TRUE, CDSS added note to source data, so values should be checked at source
* Exports three summary csv files for five most recent years, 2012-2016:
  * Time series data in long format, with counts and proportions for each combination of year-county
  * Time series in wide format - counts
  * Time series in wide format - proportions
  
The plot below highlights five-year trends for some of the most common race and ethnic groups:

![My image](https://github.com/pete-rjames/calfresh_race_ethnicity/blob/master/calfresh_race_ethnicity_2012_2016.PNG)

And this map shows the proportion of households that are hispanic/latino in each California county, based on the most recent data from 2016.

![My image](https://github.com/pete-rjames/calfresh_race_ethnicity/blob/master/calfresh_hispanic_percent_county_2016.PNG)
