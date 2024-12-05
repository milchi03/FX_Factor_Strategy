# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)

# Load the data
tbill <- read_xlsx("./tbill.xlsx")

#Raname
names(tbill) <- c("date", "3m_tbill_yield")

#Save
save(vix, file = "./vix.RData")
