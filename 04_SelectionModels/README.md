Date: 2020-06-17

# structure

The Ag, C, Cd and Cd directories models the result of selection under respective stress treatments.

the /ModelResults/XX directories takes the output from the respective models, and visualizes the results together with experimental observations

# INPUT
There are two lines of input data in the models.

1. The predicted strain specific inhibition level from the dose-response
curve analysis merged output files from Directory **02_DRC/XX/PR**. .csv
files needs file name example **PRvaluesAg.csv**, column names are added
in script in the order of Strain, predicted inhibition, low 95% conf.,
high 95% conf.)

2. The data file Strain_observations.csv which contains experimental
observations of strain specific inhibition (one column per metal, mean
of N=2) in the specific concentration of metals predicted from the DRC. This file also contains the observed control growth rates for each species, which is needed to compute all models

# SCRIPT

The two data files are imported and indexed and constrained based on
detection limit of flourometers density measurements and DRC predictions. Two selection models are creates based on each of the two input data set (on with
intraspecific selection, and one using a mean for each species)

Section *Model using avarage parameter per species* computes the
averages models. Inhibition observations are combined with growth rate
observations to make predictive model of competition/selection, which
finaly ends up in the **Treatments** dataframe. In the for-i-loop the exponential growth is predicted for each species with input from **Treatments**  

1. *t*: vector containing all timepoints predicted in days (can be manipulated)
2. *u*: predicted growth rate of species (from computations above)
3. *N0*: the start density in whatever arbitrary unit, here its approximately the total surface area of cells in um2 mL-1 media(can be manipulated)

Next section *Model with strain selection* repeats the process above but rather than computing species averages, each strain is modeled independently

The two selection models can be run on DRCprediction data, or observed inhibition data by changing *u* from $PRuP to $PRuO respectively 

The final section *Stats and tables/figures* makes output and runs t-tests contrasting inhibition observations between the two species, and also between the two types of observations (DRC predicted and observed inhibition)


# Output


1. Files
Storage directory for the species average model parameters in the for-i-loop

2. Files2
Storage directory for the strain specific model parameters in the for-i-loop

3. Model
Result files describing the predicted change in density over time for the respective species specific avarage models

4. Model2
Result file describing the predicted change in density over time for the respective strains

The .csv files of the four models needs to combined and moved to /ModelResults/XX. The files in Model needs to be merged to *PRoutcomeXX.csv* and Model2 to either *StrainMdrc_XX.csv* if the for-i-loop uses *$PRuP* or *StrainMO_XX.csv* for *$PRuO*

# Visualizing models

In /ModelResults/XX execute the script *Fig3_Comp_XX.R* 

2020-12-16
I have made several changes to the visualization script. These can be largely reversed. First i no longer display the avarage models, or the model that uses the observed data. This can be changed in the lines above Fig.3. Secondly i have computed strain-by-strain predictions of competitive outcome using the drc data. This was done in a factorial manner comparing the fitness of the 4 *S. marnioi* strains against each of the 4 *T. baltica* strain, generating 16 theoretical outcome. This arguably provides a clearer illustration of the importance of evolution in generating a robust prediction of the outcome. 
 
# The end
# 
