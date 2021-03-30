#Lets clear old objects
rm(list=ls())
getwd()
#and set the work directory in case we have moved around or opened another project
setwd("~/Documents/R/Andersson_etal_2021/04_SelectionModels/Cd")
dir()

#Load packages and data (not adapted)
library(reshape2)
library(devtools)
library(scales)
library(ggplot2)
library(devtools)
library(tidyverse)
library(plyr)

sessionInfo()
#Import data (a bit messy)
myData <- read.csv("PRvaluesCd.csv", header=FALSE)
head(myData)
myData2 <- read.csv2("Strain_observations.csv", header=TRUE)
Species <- c("SM", "SM", "SM", "SM", "TB", "TB", "TB", "TB")
myData3 <- cbind(myData, Species, myData2$Growth_rate, myData2$Cd_inhibition)
colnames(myData3)<- c("Strain", "PR", "Low", "High", "Species", "Growth", "Observed_Inhibition")

#Define numeric and factors
myData3$Growth <- as.numeric(as.character(myData3$Growth))
myData3$Observed_Inhibition <- as.numeric(as.character(myData3$Observed_Inhibition))
myData3$Species <- as.factor(as.character(myData3$Species))

#Confine inhibitions of observed drc data around 1 and 0, and Observed inhibition at 1.5 (detection limit of flourometer)
myData3$Low <- (ifelse(myData3$Low < 0, 0, myData3$Low))
myData3$High <- (ifelse(myData3$High > 1, 1, myData3$High))
myData3$Observed_Inhibition <- (ifelse(myData3$Observed_Inhibition > 1.5, 1.5, myData3$Observed_Inhibition))
myData3$Observed_Inhibition <- (ifelse(myData3$Observed_Inhibition < 0, 0, myData3$Observed_Inhibition))

#Graphical view of data
df.melted <- melt(myData3, id = "Strain")
ggplot(data = df.melted, aes(x = Strain, y = value, color = variable)) +
  geom_point()

#For Cd it looks like overall a slightly higher observed than predicted inhibition

#Model using average parameter per species####

#Note this approach was explored but not used in manuscript

#First move around data and indexing
myData4 <- myData3
myData4$Strain <- NULL
melted <- melt(myData4, id = "Species")
sapply(melted, class)

#Then calculate means for two species
Avarages <- ddply(melted, c("Species", "variable"), summarise,
                  mean = mean(value), sd = sd(value),
                  sem = sd(value)/sqrt(length(value)))

#untangle the averages and SD
Avarages2 <- dcast(data = Avarages,formula = Species~variable, fun.aggregate = sum,value.var = c("mean"))
Sd <- dcast(data = Avarages,formula = Species~variable, fun.aggregate = sum,value.var = c("sd"))
Avarages3 <- Avarages2

#Now generate parameters for the predictive models
Avarages3
Avarages3$PRuP <- ((1-Avarages2$PR)*(Avarages3$Growth))
Avarages3$PRuO <- ((1-Avarages2$Observed_Inhibition)*(Avarages2$Growth))
Avarages3$PR <- NULL
Avarages3$Low <- NULL
Avarages3$High <- NULL
Avarages3$Observed_Inhibition  <- NULL

Treatments <- melt(Avarages3, id = "Species")

#So Treatments is all the 6 parameters we want to run for this particular case
Treatments$ID <- paste(Treatments$Species,Treatments$variable, sep = "_")
Treatments
sapply(Treatments, class)
Treatments$ID <- as.factor(as.character(Treatments$ID))


#Create individual parameter files
for (name in levels(Treatments$ID)){
  #Subset data based on ID
  tmp=subset(Treatments,ID==name)
  #Create a new filename for each strain - the folder 'Files' should already exist
  fn=paste("Files/Parameter_",gsub(' ','',name), ".csv", sep='')
  #Save the CSV file containing separate expenses data for each strain
  write.table(tmp,fn,row.names=FALSE, sep=",")
}


#Generate vector needed for looped prediction
f <- as.vector(Treatments$ID)
f

for (i in f) {
  # define name of Input and Output 
  input=paste("Files/Parameter_",i,".csv", sep="")
  output=paste("Model/Model_",i,".csv", sep="")
  
  
  myData5= read.csv(input, header=TRUE)
  myData5
  #First look at the imported data
  
  #create the model and input parameters, ie. a vector of (time=t), and set start density N0 (same as Exp ca 80000 um-2, mL-1 in surface area), and set Metal.
  t <- seq(0, 10, by=0.1)
  n <- length(t)
  u <-  myData5$value
  N0 <- 80000
  P_density <- N0*exp(t*u)
  P_density
  S <- rep(i, each = n)
  Metal <- rep("Cd", each = n)
  P_density2 <- data.frame(S, Metal, t, P_density)
  #write.table(Response2, file = output, sep=",",  col.names=FALSE, row.names = i)
  write.table(P_density2, file = output, sep=",",  col.names=FALSE, row.names =FALSE)
}

#done use UNIX command (cat or copy) to combine output files

#Model with strain selection####

#The observed and predicted inhibition has a strong correlation
plot(myData3$PR~myData3$Observed_Inhibition, col=myData3$Species)
Regress <- lm(myData3$PR~myData3$Observed_Inhibition)
abline(Regress)

Strain <- myData3
Strain$PRuP <- ((1-Strain$PR)*(Strain$Growth))
Strain$PRuO <- ((1-Strain$Observed_Inhibition)*(Strain$Growth))

g <- as.vector(Strain$Strain)
g
#Create individual parameter files
for (name in g){
  #Subset data based on ID
  tmp=subset(Strain, Strain==name)
  #Create a new filename for each strain - the folder 'Files' should already exist
  fn=paste("Files2/Parameter_",gsub(' ','',name), ".csv", sep='')
  #Save the CSV file containing separate expenses data for each strain
  write.table(tmp,fn,row.names=FALSE, sep=",")
}

for (i in g) {
  #loop goes through script putting 1-30 for i to run over every strain file
  # define name of Input and Output 
  input=paste("Files2/Parameter_",i,".csv", sep="")
  output=paste("Model2/Model_",i,".csv", sep="")
  
  
  myData6= read.csv(input, header=TRUE)
  myData6
  #First look at the imported data
  
  #create the model and input parameters, ie. a vector of (time=t), and set start density N0 (same as Exp ca 80000 um-2, mL-1 in surface area), and set Metal.
  t <- seq(0, 10, by=0.1)
  n <- length(t)
  u <-  myData6$PRuP
  N0 <- 10000
  P_density <- N0*exp(t*u)
  P_density
  S <- rep(i, each = n)
  Metal <- rep("Cd", each = n)
  P_density2 <- data.frame(S, Metal, t, P_density)
  #write.table(Response2, file = output, sep=",",  col.names=FALSE, row.names = i)
  write.table(P_density2, file = output, sep=",",  col.names=FALSE, row.names =FALSE)
  
  #lets try to save strains as vectors and simply add them up by species without output
  Density_i <- P_density
  
  
}

#Stats and tables/figures####

#Make a Table out of parameters

Avarages$mean <- as.numeric(as.character(Avarages$mean))
Avarages$sd <- as.numeric(as.character(Avarages$sd))
Avarages$sem <- as.numeric(as.character(Avarages$sem))

CdTable <-  format(myData3, trim = FALSE, digits = 2)
Means <- format(Avarages, trim = FALSE, digits = 2)
CdTable

#F and t-test between the two species
GrowthVar <- var.test(formula = sqrt(Growth) ~ Species, data = myData3)
GrowthVar
Growth <- t.test(formula = sqrt(Growth) ~ Species, data = myData3, var.equal = FALSE)
Growth

PRVar <- var.test(formula = sqrt(PR) ~ Species, data = myData3)
PRVar
PR <- t.test(formula = sqrt(PR) ~ Species, data = myData3, var.equal = FALSE)
PR

Observed_InhibitionVar <- var.test(formula = sqrt(Observed_Inhibition) ~ Species, data = myData3)
Observed_InhibitionVar
Observed_Inhibition <- t.test(formula = sqrt(Observed_Inhibition) ~ Species, data = myData3, var.equal = FALSE)
Observed_Inhibition

#Paired t-test between the predicted and observed inhibition
CompInhibition <- t.test(x = sqrt(myData3$PR), y = sqrt(myData3$Observed_Inhibition), paired = T, var.equal = FALSE)
CompInhibition