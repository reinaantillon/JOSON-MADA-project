###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed-data","processeddata.rds")

#load data. 
clean_data <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

############################


#### First model fit
model_variety <- aov(peanut_yield ~ variety, data = clean_data)
summary(model_variety)

# place results from fit into a data frame with the tidy function
model1 <- broom::tidy(model_variety)

#look at fit results
print(model1)

# save fit results table  
table_file1 = here("results", "tables", "resulttable1.rds")
saveRDS(model1, file = table_file1)

############################
#### Second model fit

model_location <- aov(peanut_yield ~ location, data = df) 
summary(model_location)

# place results from fit into a data frame with the tidy function
model2 <- broom::tidy(model_location)

#look at fit results
print(model2)

# save fit results table  
table_file2 = here("results", "tables", "resulttable2.rds")
saveRDS(model2, file = table_file2)

############################
#### Third model fit
model3 <- t.test(peanut_yield ~ watered, data = df)
model3

# place results from fit into a data frame with the tidy function
model3 <- broom::tidy(model3)

#look at fit results
print(model3)

# save fit results table  
table_file3 = here("results", "tables", "resulttable3.rds")
saveRDS(model3, file = table_file3)
  