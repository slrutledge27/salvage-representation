library(tidyverse)
library(dplyr)
library(ggplot2)

### Single data frame for plotting and stats ###
#count_proportional_byorder
#write.csv(count_proportional_byorder, "./Data/proportion_dataset_for_analyses_and_plotting.csv")

count_proportional_byorder <-read.csv("./Data/proportion_dataset_for_analyses_and_plotting.csv")

### Scatterplots ###
plot(log(count_proportional_byorder$species_count_salvage_proportional+1),log(count_proportional_byorder$species_count_active_proportional+1))
cor.test(log(count_proportional_byorder$species_count_salvage_proportional+1),log(count_proportional_byorder$species_count_active_proportional+1))

plot(log(count_proportional_byorder$specimen_count_salvage_proportional+1),log(count_proportional_byorder$specimen_count_active_proportional+1))
cor.test(log(count_proportional_byorder$specimen_count_salvage_proportional+1),log(count_proportional_byorder$specimen_count_active_proportional+1))

### NOTE: need to remove weird first column if re-importing csv files
#specimen_count[1] <- NULL
#species_count_per_order[1] <- NULL

### build a scatter plot; active vs salvage number of species per order

## log transform data (very skewed)
count_proportional_byorder$log_active_species_prop <- log(count_proportional_byorder$species_count_active_proportional+1)
count_proportional_byorder$log_salvage_species_prop <- log(count_proportional_byorder$species_count_salvage_proportional+1)

count_proportional_byorder$log_active_specimen_prop <- log(count_proportional_byorder$specimen_count_active_proportional+1)
count_proportional_byorder$log_salvage_specimen_prop <- log(count_proportional_byorder$specimen_count_salvage_proportional+1)

write.csv(count_proportional_byorder, "./Data/proportion_dataset_for_analyses_and_plotting.csv")

## count_proportional_byorder <- proportion_dataset_for_analyses_and_plotting

## test for correlations
cor.test(order_species_wide$log_active,order_species_wide$log_salvage)
## report effect size, p-value, se
cor.test(order_specimens_wide$log_active,order_specimens_wide$log_salvage)

## make rownames into separate column
count_proportional_byorder <- tibble::rownames_to_column(count_proportional_byorder, "order") 

## now plot
ggplot(data = count_proportional_byorder, aes(x = log_salvage_species_prop, y = log_active_species_prop, color = order)) +
  # Scatterplot with point size
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  # Show dots
  #geom_text(
  #label=order_species_wide$order, 
  #nudge_x = 0.1, nudge_y = 0.1, 
  #check_overlap = T, size = 2
  # )+
  #annotate("text", x = min(order_species_wide$log_salvage), y = max(order_species_wide$log_active), label = eq_text, hjust = 0, size = 5, color = "darkred")+
  labs( 
    x = "log-transformed count of salvaged species", 
    y = "log-transformed count of actively collected species",
    color = "Order") +  # Legend title
  theme_minimal() 

ggplot(data = count_proportional_byorder, aes(x = log_salvage_specimen_prop, y = log_active_specimen_prop, color = order)) +
  geom_point() +  # Scatterplot with point size
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_text(
   label=count_proportional_byorder$order, 
   nudge_x = 0.1, nudge_y = 0.1, 
   check_overlap = T, size = 2
   )+
  labs( 
    x = "log-transformed count of salvaged specimens", 
    y = "log-transformed count of actively collected specimens",
    color = "Order") +  # Legend title
  theme_minimal() 

