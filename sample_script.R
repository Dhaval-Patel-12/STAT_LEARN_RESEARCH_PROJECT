#This is an R script.

#Those are the libraries.
library(magrittr)
library(dplyr)
library(ROCR)

#Extracting data from file
medicaldatarawfile = "File.csv"
medicaldataraw = read.csv(medicaldatarawfile)

#Basic info about the data
medicaldataraw %>% nrow()
str(medicaldataraw)
head(medicaldataraw)
missing_ratio(medicaldataraw)