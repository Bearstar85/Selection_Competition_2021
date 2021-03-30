Date: 2020-10-11

# INPUT
The raw data was downloaded from https://sharkweb.smhi.se (2019-06-05) and major taxa was merged
using [Plankton Toolbox
v3!](https://www.smhi.se/en/research/research-news/plankton-toolbox-a-new-tool-for-working-with
-plankton-data-1.80679). The source file is **B1shark.txt** and the resulting .txt files are:  only
*Skeletonema sp* (**SkeletonemaB1.txt**), *only T.baltica* (**TbalticaB1.txt**), and *The rest of
the species groups according to main plankton group, diatoms, cyanos, dinoflagelates, others*
(**Rest_PlanctonGroupB1.txt**). These are in the directory **Data** and is used by the R script
**ASKO_monitoring.R**

# SCRIPT
# 
The directory **Analysis** contains the R script **ASKO_monitoring.R** which is used to merge and
analyze the 3 input files.

* *Read in the data and basic QC and formatting* Import data, changes indexes, names of species,
checks missing data-points, formats dates for R, basic correlations etc

* *calculate averages per month* Pretty much that

* *calculate averages per year* Pretty much that

* *Plotting monthly avarages* Exploratory plotting and correlation analysis of monthly trends

* *Plotting yearly avarages* Exploratory plotting and correlation analysis of yearly trends

* *Summary graphs result* First there's some formatting and sub-selectio of species/taxa in  for
final plots. I clone December and January in an attempt to improve the smoothing function around the
end/begining of the year but this does not help much. Cloning more months on both ends really threw of the
fitting. To enable log-plotting of densities i set 0 observations to 0.1 x the detection limit. It is worth noting that there are certain differences between smothing data on log and linear axises which can be seen in these final figures.


# Output
The only output file is the final figure.
