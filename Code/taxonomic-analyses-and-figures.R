library(tidyverse)
library(dplyr)
library(ggplot2)

### read in cleaned csv file 
Arctos_all<-read.csv("./Data/Arctos_all.csv")
keeps<-c("coll_method","genus_species")
Arctos_all<-Arctos_all[keeps]

### import NACC species list
NACC <-read.csv("./Data/NACC_list_species.csv")

### select columns to keep in NACC for specimen tally (order, family, species)
keeps<-c("order","family","species")
species_list<-NACC[keeps]

##rename column
colnames(species_list)[3] = "genus_species"

### merge to Arctos_all by genus_species
Arctos_orders <- merge(Arctos_all, species_list, by="genus_species")

## investigate unmatched entries
unmatched <- anti_join(Arctos_all, species_list, by="genus_species")
### get specimen count per order
specimen_count<-Arctos_orders%>% group_by(order, coll_method) %>% 
  summarize(count=n())

##get counts for species by order in NACC
Order_species_count <- NACC %>% 
  filter(!is.na(species)) %>%
  group_by(species) %>% 
  summarise(order = first(order), n = n())

## rename columns
colnames(Order_species_count)[1]<- "genus_species"

## merge NACC Order_species_count with Arctos_all by genus_species
Arctos_order_species <- left_join(Arctos_all, Order_species_count, by = "genus_species")

## select columns to keep
keeps<-c("coll_method", "order","genus_species")
Arctos_order_species<-Arctos_order_species[keeps]

## remove NA's
Arctos_order_species<- Arctos_order_species %>% filter(!is.na(order))

## now count species per order
species_count<- Arctos_order_species %>% 
  filter(!is.na(genus_species)) %>%
  group_by(order, genus_species,coll_method) %>% 
  summarize(count=n())

species_count_per_order<- species_count %>% 
  group_by(order,coll_method) %>% 
  summarize(count=n())
## write to csv
write.csv(specimen_count, "./Data/specimens_per_order_all.csv")
write.csv(species_count_per_order, "./Data/species_per_order_all.csv")


### build a scatter plot; active vs salvage number of species per order
## pivot table
order_species_wide <- Order_species_count %>%
  pivot_wider(names_from = coll_method,  # Create new columns from 'Category'
              values_from = count,    # Fill them with 'Value'
              values_fill = 0)


## now plot
ggplot(data = order_species_wide, aes(x = log(salvage), y = log(active), color = order)) +
  geom_point(size = 3) +  # Scatterplot with point size
  coord_cartesian(ylim = c(-50, max(log(order_species_wide$active) + 2)))+
  labs( 
       x = "Count of salvaged species", 
       y = "Count of actively collected species",
       color = "Order") +  # Legend title
  theme_minimal() 
