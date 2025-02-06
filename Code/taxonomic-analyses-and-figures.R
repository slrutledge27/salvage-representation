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

### NOTE: need to remove weird first column if re-importing csv files
#specimen_count[1] <- NULL
#species_count_per_order[1] <- NULL

### build a scatter plot; active vs salvage number of species per order
## pivot table
order_species_wide <- species_count_per_order %>%
  pivot_wider(names_from = coll_method,  # Create new columns from 'Category'
              values_from = count,    # Fill them with 'Value'
              values_fill = 0)

order_specimens_wide <- specimen_count %>%
  pivot_wider(names_from = coll_method,  # Create new columns from 'Category'
              values_from = count,    # Fill them with 'Value'
              values_fill = 0)

## log transform data (very skewed)
order_species_wide$log_active <- log(order_species_wide$active+1)
order_species_wide$log_salvage <- log(order_species_wide$salvage+1)

order_specimens_wide$log_active <- log(order_specimens_wide$active+1)
order_specimens_wide$log_salvage <- log(order_specimens_wide$salvage+1)

## test for correlations
cor.test(order_species_wide$log_active,order_species_wide$log_salvage)#Correlation is negative and significant. There is an inverse correlation between salvage and active specmien counts.
## report effect size, p-value, se
cor.test(order_specimens_wide$log_active,order_specimens_wide$log_salvage)#Correlation is negative and significant. There is an inverse correlation between salvage and active specmien counts.


## set up equations
# Fit linear model
model <- lm(log_active ~ log_salvage, data = order_species_wide)
coeff <- coef(model)  # Extract slope and intercept
r_squared <- summary(model)$r.squared  # R² value

# Create equation text
eq_text <- paste0("y = ", round(coeff[2], 2), "x + ", round(coeff[1], 2), 
                  "\nR² = ", round(r_squared, 3))
## now plot
ggplot(data = order_species_wide, aes(x = log_salvage, y = log_active, color = order)) +
    # Scatterplot with point size
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  # Show dots
  #geom_text(
    #label=order_species_wide$order, 
    #nudge_x = 0.1, nudge_y = 0.1, 
    #check_overlap = T, size = 2
 # )+
  annotate("text", x = min(order_species_wide$log_salvage), y = max(order_species_wide$log_active), label = eq_text, hjust = 0, size = 5, color = "darkred")+
  labs( 
       x = "log-transformed count of salvaged species", 
       y = "log-transformed count of actively collected species",
       color = "Order") +  # Legend title
  theme_minimal() 

ggplot(data = order_specimens_wide, aes(x = log_salvage, y = log_active, color = order)) +
  geom_point() +  # Scatterplot with point size
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  #geom_text(
   # label=order_specimens_wide$order, 
    #nudge_x = 0.1, nudge_y = 0.1, 
    #check_overlap = T, size = 2
  #)+
  labs( 
    x = "log-transformed count of salvaged specimens", 
    y = "log-transformed count of actively collected specimens",
    color = "Order") +  # Legend title
  theme_minimal() 



#oord_cartesian(ylim = c(-50, max(log(order_species_wide$active) + 2)))