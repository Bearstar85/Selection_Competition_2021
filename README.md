2020-06-17
Author: Bjorn Andersson

# Role of diversity, acclimatization, and chronic toxic effects in competition between diatoms under toxic stress.

## Project outline

This project contains data, statistics, and graphical illustrationsin in R. The experiments tests how diffrent 
toxic concentrations of heavy metals modulates competition between an artifically assembled community of Baltic 
Sea diatoms (four clonal strains each of the two diatom species *Skeletonema marinoi* (SM) and *Thalassiosira 
baltica* (TB). The experimental desig is illsutrated as seen below

![Experimetal Design](https://github.com/Bearstar85/R/blob/master/Competition_MS/ExperimentalDesign.jpeg)

Brifely we use toxic Dose Response curves (DRC) observations made in *Andersson et al. 2020* (accepted to 
Aquatic 2020-06-17) to model how inter and intra specific selection between strains is expected to be 
modulated under Cadmium, Copper and Silver stress. The stress level was set at the avarage 50% inhibition 
concentration (EC50) between all eigth strains. Strains were mixed at even biomass based on Relative Chl 
a Flourescence Units (RFU) and cultivated semicontinously to maintain exponential growth. The growth of 
individual species was measured through microscopy observations and the photosynthetic capicty was 
monitored using Pulse Amplitude Modulation (PAM) fluorometry.   
   
## Structure

The Directories breaks down the analysis into smaller parts organised around a Manuscript in prep. Each 
directory contains scripts and data amd README file, enabeling reproduction of computations, models, 
statistics and figures
 
1. **01_SMHI**:
 Contains monitoring data from SMHI at Asko station (B1) from 1981 to 2018 [SMHI](https://sharkweb.smhi.se) and visualized in R.

2. **02_DRC**:
Contains dose response data for all eight strains against seven heavy metals (data from *Andersson et al. 2020*). Scripts compute predicted inhibition responses under experimental conditions, EC05, EC50 and EC95, and plots individual DRC for all strains.

3. **03_EC**:
Visualizes EC05, EC50 and EC95 (**02_DRC** results) for all strains (not included in latest Manuscript version)

4. **04_SelectionModels**:
Computations for deterministic selection model based on variability in predicted inhibition between strains. In the end, models are contrasted agains experimental observations.

5. **05_CompetitionExp**:
Experimental observational data with plots and statistics. Some Model outputs results from **04_SelectionModels** is also included.

6. **06_MetalMonitoring**:
Maps generated in GIS and monitoring data for copper pollution in the baltic sea.

7. **07_InhibitionValues**:
Observed and DRC predicted inhibition values as shown in Fig. 2 of manuscript

8. **08_DRCdiffMedia**:
Dose-respons curves for S. marinoi strain RO5AC towards Cu in diffrent media conditions.

## For more details 
see README files in individual directories

20201101: MAde some changes in illustrations of the results for MS preperations.-Björn
