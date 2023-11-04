library(tictoc)
library(doParallel)
library(tidyverse)
library(tweedie) 
library(ggplot2)
library(magrittr)
library(tidyverse)

######Script 1######
tic()

source("Scripts/script1.r")

toc() 

######Script 2######
tic()

source("Scripts/script2.r")

toc()

######Script 3######
tic()

source("Scripts/script3.r")

toc() 

#Script 1 uses 35.8 seconds
#Script 2 uses 27 seconds
#Scrip 3 uses 16.2 seconds
#Conclusion: rewriting the simTweedieTest function result in a shorter run time
#than rewriting the for-loop to use multiple cores. 
