Date: 2020-08-18

# Structure

The Data directory contains input data. Figures contains output figures. StatResult is output of key statistical results.  Table is output of data for tables. The R script for computations is **Plots.R**. 

# INPUT

The **Data** folder contains results from the Competition experiment and selection models (see Module 04_SelectionModels). From the competition experiment there are the PAM (**PAMComp.csv**), RFU (**RFUComp.csv**) measurements, and Growth rates for the two species *S. marinoi* (SM) and *T. baltica* (TB) which are based on microscopy observations of cell surface areas in cultures (**GrowthRateComp.csv**). From the selection models there are predictions about how the growth rate is expected to evolve in the Competition experiment under Control, Ag, Cd, and Cu stress, based on the DRC predicted inhibitions (**RateMdrc.csv**) and the Parallel observed strain inhibitions (**RateMO.csv**).


# SCRIPT

The first Modules *PAM* through *Growth rate relartive SM* only compute avarages and makes plots. *Stats* checks assumptions and computes ANOVAs and Post hoc test on data. *Stats Examples* contains example code from [Datanovia using the rstatix package](#https://www.datanovia.com/en/classes/comparing-means-in-r/) #https://www.datanovia.com/en/lessons/repeated-measures-anova-in-r/ and the rstatix package. And finally *Old analysis that did not work for various reasons* is what it say.

# Output

Figures directory contains all figures generated and the Table directory two summarizing tables.
