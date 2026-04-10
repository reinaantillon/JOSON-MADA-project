
#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(factoextra)
library(tidymodels)
library(tidyverse)

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
model_1 <- broom::tidy(model_variety)

#look at fit results
print(model_1)

# save fit results table  
table_file_1 = here("results", "tables", "resulttable_1.rds")
saveRDS(model_1, file = table_file_1)

############################
#### Second model fit

model_location <- aov(peanut_yield ~ location, data = clean_data) 
summary(model_location)

# place results from fit into a data frame with the tidy function
model_2 <- broom::tidy(model_location)

#look at fit results
print(model_2)

# save fit results table  
table_file_2 = here("results", "tables", "resulttable_2.rds")
saveRDS(model_2, file = table_file_2)

############################
#### Third model fit
model_3 <- t.test(peanut_yield ~ watered, data = clean_data)

# place results from fit into a data frame with the tidy function
model_3 <- broom::tidy(model_3)

#look at fit results
print(model_3)

# save fit results table  
table_file_3 = here("results", "tables", "resulttable_3.rds")
saveRDS(model_3, file = table_file_3)

##############################
#PCA
##############################
variety_summary <- clean_data %>%
  group_by(variety) %>%
  summarise(
    avg_yield = mean(peanut_yield, na.rm = TRUE),
    sd_yield = sd(peanut_yield, na.rm = TRUE),
    
    irrigated_yield = mean(peanut_yield[watered == TRUE], na.rm = TRUE),
    non_irrigated_yield = mean(peanut_yield[watered == FALSE], na.rm = TRUE),
    
    yield_diff = irrigated_yield - non_irrigated_yield,  # responsiveness
    
    n_obs = n()
  ) %>%
  filter(n_obs >= 50)   # optional 


variety_scaled <- variety_summary %>%
  select(-variety, -n_obs) %>%
  scale()

elbow_plot <- fviz_nbclust(variety_scaled, kmeans, method = "wss")
elbow = here("results", "figures","elbow.png")
ggsave(filename = elbow, plot=elbow_plot) 

set.seed(123)

kmeans_fit <- kmeans(variety_scaled, centers = 4)

variety_summary$cluster <- as.factor(kmeans_fit$cluster)

pca_plot <- fviz_cluster(kmeans_fit, data = variety_scaled,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_minimal())
pca = here("results", "figures","pca.png")
plot(pca_plot)
ggsave(filename = pca, plot=pca_plot) 

variety_summary %>%
  group_by(cluster) %>%
  summarise(across(c(avg_yield, sd_yield, yield_diff), mean))
