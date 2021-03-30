Date: 2020-06-24

# structure

The Data directory contains input data. The Output folder has output files and figures from the R script **Metal_monitoring.R**.

# INPUT

The **Data** folder contains several data sets derived from ICES, SGU, IVL and SLU. There are three lines of input data files that contains Cu data that has water-concentration mesurments. This is the data that is being further analyzed.

Data servers searched (all data accessed 2020-06-23):

* [ICES](https://ecosystemdata.ices.dk) **ContaminantsInSeawater_ICES.csv**, lots of data on several parameters including metals in water

* [SMHI](https://sharkweb.smhi.se) no data available for metals
* [SGU](https://miljodata.slu.se/MVM/Query?mediums=12,5&products=6&parameters=Cu&startdate=2016-01-01&enddate=2020-02-27): **20200623_SGUmiljogifter_lan08.csv** Kalmars region data contains only biotic measurements, no water samples

* [SLU]((https://miljodata.slu.se/MVM/Query?mediums=12,5&products=6&parameters=Cu&startdate=2016-01-01&enddate=2020-02-27): Used a map based cutoff approach to retrieve data from East coast of Sweden, south of Stockholm only. **20200623_slu_CUaar.csv** with metadata descrition in **20200623_slu_CUaar.xlsx**

* [EMODnet](https://www.emodnet-chemistry.eu/products/catalogue#/search?from=1&to=30): Only non-essential heavy metals (Hg, Cd, Pb) included. No data downloaded

* [IVL](https://dvsb.ivl.se/dvss/DataSelect.aspx): **2020623_IVLallSweden.csv** contains all copper observations in Swedish water (including urban and wastewater facilities)

* [VISS](https://viss.lansstyrelsen.se//MonitoringPrograms.aspx?monitoringProgramID=163&tab=Stations&managementCycleName=Senaste_bedoemning#tabStations): Only metal data in Mussels, macroalgae, and sediments. No data downloaded.

So in the end we found three data sources to proceed with

1. **ContaminantsInSeawater_CU.csv** contains ICES data downloaded 2018-06-xx. The entire database (**ContaminantsInSeawater_ICES.csv**) was re-downloaded in 2020-06-23 and the Cu observations are the same (1966 of them). Contains coastal water data from Baltic countries. Both Filtered and unfiltered samples included.

2. **20200623_slu_CUaar.csv** contains SLU Cu data from river and river mouth data close to the Baltic coast of Sweden.

3. **2020623_IVLallSweden.csv** contains mixture of environmental water sample Cu data across all of sweden. 

# SCRIPT

Module *Read in the data and basic QC* reads in the data. *Transform units* standardizes unit to µM. In *Trimming of irrelevant sites and observations* unwanted sites are removed from datasets (i.e. not coastal related) and the max value for sites with multiple observations is chosen. *Output for GIS* creats data files for GIS visualization (only max values per site). *Stats and dataanalysis* filters out sites with observations that are within toxic levels in DRC of S. marinoi in media without EDTA. *Figures* boxplots of site data in various formats.

# Output
**Boxplots.pdf**: boxplot of all Cu sites
**BoxplotsAllstations.pdf**: boxplot of all Cu sites after triming
**BoxplotsHighStations.pdf**: boxplot of sites with concentrations above toxic levels in DRC of S. marinoi without EDTA.
**IVL.csv**: trimmed IVL data 
**ICES.csv** trimmed ICES data
**SLU.csv** trimmed SLU data



# 
