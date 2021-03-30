
#Running this script will output your current session information
rm(list=ls())
getwd()

#Note that not all these packages are utilized in this analysis.

#Main
library(drc)
library(ggplot2)
library(rstatix)
library(tidyverse)

#(Some are) assisting in data manipulation
library(tibble)
library(reshape2)
library(lattice)
library(devtools)
library(plyr)
library(scales)
library(devtools)
library(grid)
library(tidyverse)
library(lubridate) # useful for working with dates
library(cowplot) # useul for combining multiple plots
library(ggthemes)
library(broom)
library(ggpubr)
library(gridExtra)
library(magrittr)
library(dplyr)

#I ran this script when I finished the code and it was all working (SessionInfo_Author.txt)

#Export the version information as text file
writeLines(capture.output(sessionInfo()), "SessionInfo.txt")

citation(package = "ggplot2")
citation(package = "drc")
citation(package = "rstatix")
