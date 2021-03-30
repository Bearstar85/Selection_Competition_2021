Date: 2020-10-11

# INPUT
The input data here is dose-response data from Andersson et al 2020
(file name *Doseresponse_all.csv*) for eight strains from Gropviken (4 TB
and 4 SM) and 7 metals. These strains are the same used in the as is
mixed in the downstream Competition experiment. Note *SM_GP2-4_16* and
*SM_GP2-4_6* are aliases for the same strain (my crappy handwriting caused
this)

# R-SCRIPT
Each script in subfolders sub-select the metal in question, correct
nominal metal concentrations to to absolutions values based on linear
regression with coefficients derived from Andersson et al 2020.

During the curve-fitting based on drc-package, note that different
formulas are used for different metal (eg. LL.2, W1.2, or W2.2) again
based on the best fit established in Andersson et al. 2020.

The script uses for-i-loops to run the 8 strain analyses independently,
and any number of strains can therefor be used in modifications of the
input file and the for-i-loop. This should work automatically depending on how many indexes there are in the source datafile. 

# Output
The output files are in the respective sub directories (need to be
created separately before running R-script):

1. EC

Contains .csv files with each strains predicted EC0.05, EC0.50, and
EC0.95. This data, from all folders, needs to be combined which I am
currently doing by moving them to an empty folder and use UNIX command
in terminal using. 

Merge EC values (I use UNIX for this) 'cat *.csv > ECvalues.csv'

This data then serves as input file for directory **03_EC** (upstream, name
**ECvaluesAll_MS.csv**). The output data needs the Headers: Strain, EC
level, Conc, SE, X95L, and X95H.

During merger I manually add metadata: Metal (Ag, Cd, Cu and Zn), Local (Gropviken), Species (SM or TB), and
compute Respons_Range (EC95/EC05: not shown in MS)

2. **Files**

The For-i-loop in the script breaks down dos-respons data into .csv file
per strain. They are stored here.

3. **Plots**

Here are output plots for the DRC curves, one per strain. I manually
change color between TB and SM and re-run script and move them to
downstream directory.

4. **PR**

Strain specific output files that predicts the inhibition at a given
concentration, in this case the average EC50 value between all strains
which is what is used in the competition experiment.

5. **Summary**

I store the combined files of the results in EC and PR directories (i.e. data
that is moved to next analysis step). Uses UNIX command for this: copy PR_* PR_all.csv to combine to one csv file or: cd /Users/xanbjg/Documents/R/EC followed by cat *.csv > ECvalues.csv. ECvalues.csv goes to directory 03_EC (input
file **ECvaluesAll_MStall.csv**). **PRvaluesXX.csv** (XX: either Ag, Cd, or Cu) serves as input for the
model predictions (**04_SelectionModels**) where Ag goes to directory Ag etc.

# The end

