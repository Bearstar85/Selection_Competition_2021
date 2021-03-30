#Import DATA to Plankton Toolbox (done) ####

#Data downloaded 2019-06-05, 

#1. Export Sharkweb monitoring data
#2. Import into Plankton toolbox (B1shark.txt)
#3. Modify data files in Plankton toolbox (easier than in R)
#4. Output files: 818 sample times per group: only Skeletonema sp (SkeletonemaB1.txt), only T.baltica (TbalticaB1.txt). The rest of the species are groups according to main plankton group, diatoms, cyanos, others: (Rest_PlanctonGroupB1.txt). 

#R analysis####

#Lets clear the objects
rm(list=ls())
getwd()
#and set the work directory to source file
setwd("~/Documents/R/Andersson_etal_2021/01_SMHI/Analysis")
dir()

#Load packages (based on recommendation)
#install.packages("magrittr")
library(magrittr)
library(ggplot2)
library(tidyverse)
library(lubridate) # useful for working with dates
library(cowplot) # useul for combining multiple plots
library(scales)
library(ggthemes)
library(dplyr)
library(broom)
library(ggpubr)
library(gridExtra)
library(car)
#library(ggpmisc)

#Read in the data and basic QC and formating####

SM <- read.delim("SkeletonemaB1.txt")
head(SM)
plot(SM$sample_date, SM$value)
dim(SM)

TB <- read.delim("TbalticaB1.txt")
head(TB)
plot(TB$sample_date, TB$value)
dim(TB)

ALL <- read.delim("Rest_PlanctonGroupB1.txt")
head(ALL)
plot(ALL$sample_date, ALL$value, type = "b", col= ALL$plankton_group, legend = ALL$plankton_group,
     ylab = "Bio-volume [mm3 L-1]", xlab = "date")

plot(SM$visit_month, SM$value, type = "p", ylab = "Bio-volume [mm3 L-1]", xlab = "Month")

#Merge all dataframes
B1old <- rbind(SM,TB,ALL)
#B1 <- rbind(SM,TB,ALL)
dim(B1old)
head(B1old)

#Lets also make dataframe with only the two species
SMTB <- rbind(SM,TB)

#There are some months missing, notably in 1991, and January or February some years in the 1980s, and winter 91.
plot(SM$visit_year, SM$visit_month)
plot(B1$visit_year, B1$visit_month)

#we have 818 monthly observations
dim(SM)

#354 without any observed SM
sum(SM$value == 0)

#and 521 without any TB
sum(TB$value == 0)

#When are both present and do they correlate? 
#They correlate (r2=0.34) but hard to correlate with many 0 values.
plot(TB$value~SM$value)
Slope <- lm(sqrt(TB$value)~sqrt(SM$value))
summary(Slope)

#When they co-occur its easier to show in one graph using log transformation.
plot((TB$value)~(SM$value), log = "xy")

Slope2 <- lm(log10(TB$value+0.00001)~log10(SM$value+0.00001))
plot(log10(TB$value+0.00001)~log10(SM$value+0.00001))
summary(Slope2)
abline(Slope2)

#Formatting

#Create dates that R can use
#Capital Y is needed when there are four digits in the year
sharkdate <- as.Date(B1old$sample_date, "%Y-%m-%d")
B1old$ryear <- year(sharkdate)
B1old$rmonth <- month(sharkdate)
B1old$rday <- mday(sharkdate)

#create dates that r can use
B1old = B1old %>%
  mutate(rdate = as.POSIXct(sample_date, "%Y-%m-%d", tz = 'GMT'))

#this creates a column with the days of the year starting with jan 1st (Julian days)
B1old$r365Day <- (B1old$rmonth-1)*30+B1old$rday
plot(B1old$r365Day)

#Filter out those "Other protozoa" which are not phytoplankton
Taxa <- unique(B1old$scientific_name)
Taxa

Keep <- c("Skeletonema", 
   "Thalassiosira",
   "Cyanobacteria",
   "Diatoms",
   "Dinoflagellates",
   "Other microalgae")
Keep

B1old2 <- filter(B1old, scientific_name %in% Keep)

#Removed 801 observations
dim(B1old)-dim(B1old2)

#Change the names of groups
B1old2$scientific_name <- recode_factor(B1old2$scientific_name, "Skeletonema" = "S. marinoi",
                                 "Thalassiosira" = "T. baltica",
                                 "Diatoms" = "Other diatoms")

#Rename
B1 <- B1old2
dim(B1)
levels(B1$scientific_name)

#calculate averages per month
B1month <- B1 %>%
  select(visit_month, value, scientific_name) %>%
  group_by(visit_month, scientific_name) %>%
  summarise(
    yearly_mean = mean(value),
    yearly_sd = sd(value),
    yearly_min = min(value),
    yearly_max = max(value),
    number_of_samples = n(),
    yearly_se = yearly_sd/sqrt(number_of_samples))

dim(B1month)
summary(B1month)
head(B1month)

ggplot(data = B1month) +
  geom_area(mapping = aes(x = visit_month, y = yearly_mean, color = scientific_name, fill = scientific_name))
#facet_wrap(~ scientific_name, nrow = 3)

#calculate averages per year
B1year <- B1 %>%
  select(visit_year, value, scientific_name) %>%
  group_by(visit_year, scientific_name) %>%
  summarise(
    yearly_mean = mean(value),
    yearly_sd = sd(value),
    yearly_min = min(value),
    yearly_max = max(value),
    number_of_samples = n(),
    yearly_se = yearly_sd/sqrt(number_of_samples))

dim(B1year)
summary(B1year)
head(B1year)

ggplot(data = B1year) +
geom_area(mapping = aes(x = visit_year, y = yearly_mean, color = scientific_name, fill = scientific_name))
          #facet_wrap(~ scientific_name, nrow = 3)

#Data exploration####

#Grab only SMTB
B1SMTByear <- filter(B1year, scientific_name %in% c("S. marinoi", "T. baltica"))

#Grab only diatoms
B1diatomsyear <- filter(B1year, scientific_name %in% c("S. marinoi", "T. baltica", "Other diatoms"))

#Correlate changes in SM and TB over time
ggplot(data = B1SMTByear) +
  geom_point(mapping = aes(x = visit_year, y = yearly_mean, color = scientific_name, fill = scientific_name))+
  geom_smooth(mapping = aes(x = visit_year, y = yearly_mean, color = scientific_name), method = "lm")

#Plot the different groups on log scale with panels
ggplot(data = B1) + 
  geom_point(mapping = aes(x = visit_month, y = log10(value))) + 
  geom_smooth(mapping = aes(x = visit_month, y = log10(value))) +
  facet_wrap(~ scientific_name, nrow = 6)

#and linear scales
ggplot(data = B1) + 
  geom_point(mapping = aes(x = visit_month, y = value)) + 
  geom_smooth(mapping = aes(x = visit_month, y = value)) +
  facet_wrap(~ scientific_name, nrow = 6)

#Smoothed averages across months on log scales
#Note that this ignores n/a (0 biovolume) observations in function which overestimate trend values!
ggplot(data = B1) + 
  geom_point(mapping = aes(x = visit_month, y = log10(value), color = scientific_name)) + 
  geom_smooth(mapping = aes(x = visit_month, y = log10(value), color = scientific_name))

ggplot(data = SM) + 
  geom_point(mapping = aes(x = visit_month, y = log10(value), color = scientific_name)) + 
  geom_smooth(mapping = aes(x = visit_month, y = log10(value), color = scientific_name))


#Smoothed averages across months linear scales (incorporates 0 values)
ggplot(data = B1) + 
  geom_point(mapping = aes(x = visit_month, y = (value), color = scientific_name)) + 
  geom_smooth(mapping = aes(x = visit_month, y = (value), color = scientific_name))

#Simplified geometric mean + CI (linear)
ggplot(data = B1) + 
  geom_smooth(mapping = aes(x = visit_month, y = (value), color = scientific_name))

#Plotting yearly averages

#plot the whole time series
ggplot(data = B1) +
  geom_point(mapping = aes(x = rdate, y = (value), color = scientific_name)) + 
  facet_wrap(~ scientific_name, ncol = 6)

#The expected trend of decreasing diatoms and increasing dinoflagelates is evident
ggplot(data = B1) + 
  geom_smooth(mapping = aes(x = rdate, y = (value), color = scientific_name)) + 
  facet_wrap(~ scientific_name, ncol = 6)

#In same graph with data points (linear include zeros)
ggplot(data = B1) + 
  geom_smooth(mapping = aes(x = visit_year, y = (value), color = scientific_name))

#only SM and TB (linear include zeros)
ggplot(data = SMTB) + 
  geom_smooth(mapping = aes(x = visit_year, y = (value), color = scientific_name))

#Summary graphs result####

#Generating some tools for plots

#Zoom in in time
start1984 <- ISOdatetime(1984,1,1,0,0,0, tz = "GMT")
end1990 <- ISOdatetime(2000,12,31,0,0,0, tz = "GMT")

start_end <- c(start1984,end1990)

#Test the time deliminator
ggplot(data = B1) + 
  geom_line(mapping = aes(x = rdate, y = (value), color = scientific_name)) + 
  facet_wrap(~ scientific_name, ncol = 6) + 
  xlim(start_end)

#Only SMTB
class(B1$scientific_name)

B1SMTB <- filter(B1, scientific_name %in% c("S. marinoi", "T. baltica"))
B1SMTB
ggplot(data = B1SMTB) + 
  geom_area(mapping = aes(x = rdate, y = (value), color = scientific_name, fill = scientific_name))+
  xlim(start_end)


#Lets clone data 1 month behind and ahead for improved smoothing
B1_behind <- subset.data.frame(B1, grepl("12", B1$rmonth))
B1_ahead <- subset.data.frame(B1, grepl("-01-", B1$rdate))
B1_behind$r365Day <- B1_behind$r365Day-365
B1_ahead$r365Day <- B1_ahead$r365Day+365
B1_14months <- rbind(B1, B1_behind, B1_ahead)

ggplot(data = B1_14months) +
  geom_point(mapping = aes(x = r365Day, y = (value), color = scientific_name)) + 
  facet_wrap(~ scientific_name, ncol = 6)

#Better but smoothing still messes up the fitting between January and December a bit
ggplot(data = B1_14months) + 
  geom_smooth(mapping = aes(x = r365Day, y = (value), color = scientific_name), method = "loess") +
  coord_cartesian(ylim=c(0,0.25), xlim=c(-30,+400))
  #coord_cartesian(xlim=c(-3,15))


#and change axis to log scale to include zeros?
start2008 <- ISOdatetime(2008,1,1,0,0,0, tz = "GMT")
end2018 <- ISOdatetime(2017,12,31,0,0,0, tz = "GMT")

#Wheres the detection limit for TB (0.000122656) and SM (3.25e-05)? lower in TB due to large size
min(TB[,16][which(TB[,16]>0)])
min(SM[,16][which(SM[,16]>0)])

#Lets also change 0 to small number so we can view log-scale
dlimit <- min(B1SMTB[,16][which(B1SMTB[,16]>0)])
dlimit
B1SMTB$value <- (ifelse(B1SMTB$value < dlimit, (dlimit*0.1), B1SMTB$value))

#FigX.A
FigX.a <- ggplot(B1SMTB, aes(x = rdate, y = (value), color = scientific_name)) + #fill = scientific_name
  #geom_line(mapping =aes(linetype = scientific_name), size = 1) + 
  geom_ribbon(mapping =aes(ymin = min(value), ymax = value, fill = scientific_name, color = scientific_name), alpha=0.4, size = 0.3) + #ymin = min(value), ymax = max(value)
  scale_y_continuous(trans = "log10", labels = trans_format("log10", math_format(10^.x))) + #geom_ribbon or area does not work with log scales because of negative values, known bug
  coord_cartesian(xlim=c(start2008,end2018), ylim=c(dlimit,2)) + #(xlim=c(start1990,end2010)
  annotation_logticks(sides = "l") + # adds linier tickmarks
  scale_color_manual(values=c("#008000", "#2846FF")) +
  scale_fill_manual(values=c("#008000", "#2846FF")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Year", y=expression(Biovolume~(mm^{3}~L^{-1}))) +
  theme(panel.spacing = unit(0.2, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=10)) +
  theme(text=(element_text(size=12))) +
  theme(axis.text=(element_text(size=12))) +
  theme(legend.text = element_text(face = "italic")) +
  theme(panel.background = element_blank()) +
  theme(aspect.ratio=0.33) +
  theme(legend.position = c(0.99, 0.99),
        legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.box.background = element_rect(fill='white'),
        legend.background = element_blank(),
        legend.spacing.x=unit(0, "cm"),
        legend.key.size = unit(0.4, "cm"))

print(FigX.a)

sapply(B1, class)

#FigX.Bmonth
FigX.Bmonth <- ggplot(data = B1_14months) + 
  geom_smooth(mapping = aes(x = r365Day, y = (value), color = scientific_name)) +
  coord_cartesian(xlim=c(0,366), ylim=c(0,0.4)) +
  scale_x_discrete(limits=c(0,90,180,270,360)) +
  scale_color_manual(values=c("#008000", "#2846FF", "#000000", "#99FFCC", "#CC6600", "#A0A0A0")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Julian day", y=expression(Biovolume~(mm^{3}~L^{-1}))) +
  theme(panel.spacing = unit(0.2, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=8)) +
  theme(text=(element_text(size=12))) +
  theme(axis.text=(element_text(size=12))) +
  theme(panel.background = element_blank()) +
  #theme(aspect.ratio=0.66) +
  theme(legend.text = element_text(face = "italic", margin = margin(t = 0))) +
  theme(legend.position = c(0.995, 0.995),
        legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.box.background = element_rect(fill='white'),
        legend.background = element_blank(),
        legend.spacing.x=unit(0, "cm"),
        legend.spacing.y=unit(0, "cm"),
        legend.key.size = unit(0.5, "cm"))

print(FigX.Bmonth)

#FigX.C
FigX.C <- ggplot(data = B1diatomsyear) +
  geom_col(mapping = aes(x = visit_year, y = (yearly_max), color = scientific_name, fill = scientific_name))+
  #geom_smooth(mapping = aes(x = visit_year, y = (yearly_mean), color = scientific_name), method = "lm") +
  coord_cartesian(ylim=c(-0.01,4), expand = F) +
  scale_color_manual(values=c("#000030", "#000030", "#000030")) +
  scale_fill_manual(values=c("#008000", "#2846FF", "#000000")) +
  background_grid(major = "none", minor = "none") + # add thin horizontal lines 
  panel_border(colour = "black", size = 1) + # and a border around each panel
  labs (x="Year", y=expression(Max~biovolume~(mm^{3}~L^{-1}))) +
  theme(panel.spacing = unit(0.2, "lines")) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=10)) +
  theme(text=(element_text(size=12))) +
  theme(axis.text=(element_text(size=12))) +
  theme(panel.background = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  #theme(aspect.ratio=1.33) +
  #stat_cor(aes(color = scientific_name), label.x = 3) +
  theme(legend.position = c(0.995, 0.995),
        legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2),
        legend.box.background = element_rect(fill='white'),
        legend.background = element_blank(),
        legend.spacing.x=unit(0, "cm"),
        legend.spacing.y=unit(0, "cm"))

print(FigX.C)

#Assembly and export of the final figure
dev.off()

ggarrange(FigX.a,
          ggarrange(FigX.Bmonth, FigX.C, ncol = 2,
                    labels = c("B", "C"), widths=c(0.5,0.5)),
          nrow = 2, labels = "A"
)

dev.copy(pdf, "FigX.pdf")
dev.off()