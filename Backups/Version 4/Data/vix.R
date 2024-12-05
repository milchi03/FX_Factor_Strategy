# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Load the data
vix <- read_xlsx("./vix.xlsx")

names(vix) <- c("date","vix_level")

#Save
save(vix, file = "./vix.RData")
