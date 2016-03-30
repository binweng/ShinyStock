# Code Authors: Fadel M. Megahed - Email: fmegahed@auburn.edu | Web:www.fadelmegahed.com

#_____________________________Initilization________________________________________________________
rm(list = ls()) # clear environment
cat("\014") # clear console
setwd('D:/@Auburn/2016Spring/INSY7970_DataVisualization/projects/shinyStock')

#_______________________________Required Libraries_______________________________________________________________________________________
# Install any needed package with the following command: install.packages("Name", dependencies = c("Depends", "Suggests"))
library(caret);
library(fscaret);
library(neuralnet);
library(kernlab);
library(gmodels);
library(C50);
library(nnet);


#__________________________Read the dataset____________________________________________
fulldata <- read.csv("./Dataset_R_CSV.csv", header = T, sep = ",")
str(fulldata)  # report summaries of each of the data attributes
head(fulldata) # returns the first few rows of the data so we can see how it looks like
names(fulldata) # returns the names of each variable



#________________________Prepare the data___________________________________________________
int_cols <- sapply(fulldata, is.integer)
int_cols[["volume"]] <- FALSE ; int_cols[["Wiki.Traffic"]] <- FALSE; 
int_cols[["Wiki_Momentum_2"]] <- FALSE;  int_cols[["Google_Momentum_2"]] <- FALSE; 
int_data <- as.data.frame(sapply(fulldata[,int_cols], as.factor))
int_col_names <- names(int_data)
non_int_data <- fulldata[, -which(names(fulldata) %in% int_col_names)]
fulldata <- cbind(non_int_data,int_data)
str(fulldata)
dim(fulldata)

