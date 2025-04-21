library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)

### Table 1
## remove extra column 
## species per order represented by salvaged and actively-collected specimens
species_per_order <- species_per_order_all_NAM[ , !(names(species_per_order_all_NAM) %in% c("count_proportional"))]
species_per_order <- species_per_order[ , -1]
species_per_order <- species_per_order %>%
  pivot_wider(names_from = coll_method, values_from = count)

colnames(species_per_order)[c(2, 3)] <- c("salvaged_species", "actively-collected_species")


## specimens per order represented by salvaged and actively-collected specimens
specimens_per_order <- specimens_per_order_all_NAM[ , !(names(specimens_per_order_all_NAM) %in% c("count_proportional"))]
specimens_per_order <- specimens_per_order[ , -1]
specimens_per_order <- specimens_per_order %>%
  pivot_wider(names_from = coll_method, values_from = count)

colnames(specimens_per_order)[c(2, 3)] <- c("salvaged_specimens", "actively-collected_specimens")
## merge tables together for Table 1
Table_1 <- merge(species_per_order, specimens_per_order, by = "order")
Table_1[is.na(Table_1)] <- 0

write.csv(Table_1, "./Figures/Table_1.csv")

### Now log-transform for analyses
## log transform data (very skewed)
count_proportional_byorder$log_active_species_prop <- log(count_proportional_byorder$species_count_active_proportional+1)
count_proportional_byorder$log_salvage_species_prop <- log(count_proportional_byorder$species_count_salvage_proportional+1)

count_proportional_byorder$log_active_specimen_prop <- log(count_proportional_byorder$specimen_count_active_proportional+1)
count_proportional_byorder$log_salvage_specimen_prop <- log(count_proportional_byorder$specimen_count_salvage_proportional+1)

write.csv(count_proportional_byorder, "./Data/proportion_dataset_for_analyses_and_plotting.csv")

count_proportional_byorder <-read.csv("./Data/proportion_dataset_for_analyses_and_plotting.csv")

### Correlation tests ###
cor.test(count_proportional_byorder$log_salvage_species_prop,count_proportional_byorder$log_active_species_prop)
cor.test(count_proportional_byorder$log_salvage_specimen_prop,count_proportional_byorder$log_active_specimen_prop)

## make rownames into separate column
#count_proportional_byorder <- tibble::rownames_to_column(count_proportional_byorder, "order") 

## now plot
ggplot(data = count_proportional_byorder, aes(x = log_salvage_species_prop, y = log_active_species_prop, color = X)) +
  # Scatterplot with point size
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_text(
  label=count_proportional_byorder$X, 
  nudge_x = 0.1, nudge_y = 0.1, 
  check_overlap = T, size = 2
   )+
  #annotate("text", x = min(order_species_wide$log_salvage), y = max(order_species_wide$log_active), label = eq_text, hjust = 0, size = 5, color = "darkred")+
  labs( 
    x = "log-transformed count of salvaged species", 
    y = "log-transformed count of actively collected species",
    color = "Order") +  # Legend title
  theme_minimal() 

ggplot(data = count_proportional_byorder, aes(x = specimen_count_salvage_proportional, y = specimen_count_active_proportional, color = X)) +
  geom_point() +  # Scatterplot with point size
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_text(
   label=count_proportional_byorder$X, 
   nudge_x = 0.1, nudge_y = 0.1, 
   check_overlap = T, size = 2
   )+
  labs( 
    x = "log-transformed count of salvaged specimens", 
    y = "log-transformed count of actively collected specimens",
    color = "Order") +  # Legend title
  theme_minimal() 

