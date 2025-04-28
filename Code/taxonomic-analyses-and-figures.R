library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr) #For cleaning eBird data to get order species counts for the state of California



### import NACC species list
NACC <-read.csv("./Data/NACC_list_species.csv")
rownames(NACC)<-NACC$species

### read in cleaned csv file 
Arctos_all<-read.csv("./Data/Arctos_all.csv")

head(Arctos_all)
keeps<-c("guid","coll_method","genus_species")
Arctos_all<-Arctos_all[keeps]

### Add in sp. for specimens not ID'd to species level ###
Arctos_all$genus_species[Arctos_all$genus_species=="Calypte "]<-"Calypte sp."
Arctos_all$genus_species[Arctos_all$genus_species=="Empidonax "]<-"Empidonax sp."
Arctos_all$genus_species[Arctos_all$genus_species=="Trochilidae "]<-"Trochilidae sp."
Arctos_all$genus_species[Arctos_all$genus_species=="Selasphorus "]<-"Selasphorus sp."
Arctos_all$genus_species[Arctos_all$genus_species=="Strix "]<-"Strix sp."

### Match the Arctos taxonomy to NACC taxonomy ###
Arctos_all$genus_species[Arctos_all$genus_species=="Phalacrocorax penicillatus"]<-"Urile penicillatus"
Arctos_all$genus_species[Arctos_all$genus_species=="Phalacrocorax pelagicus"]<-"Urile pelagicus"
Arctos_all$genus_species[Arctos_all$genus_species=="Phalacrocorax auritus"]<-"Nannopterum auritum"
Arctos_all$genus_species[Arctos_all$genus_species=="Regulus calendula"]<-"Corthylio calendula"
Arctos_all$genus_species[Arctos_all$genus_species=="Carduelis psaltria"]<-"Spinus psaltria"
Arctos_all$genus_species[Arctos_all$genus_species=="Oreothlypis celata"]<-"Leiothlypis celata"
Arctos_all$genus_species[Arctos_all$genus_species=="Vermivora celata"]<-"Leiothlypis celata"
Arctos_all$genus_species[Arctos_all$genus_species=="Carpodacus mexicanus"]<-"Haemorhous mexicanus"
Arctos_all$genus_species[Arctos_all$genus_species=="Carpodacus purpureus"]<-"Haemorhous purpureus"

Arctos_all<-Arctos_all[c(which(Arctos_all$genus_species %in% rownames(NACC)),grep("sp.",Arctos_all$genus_species)),]


### Save the cleaned MVZ Arctos data set to a new csv ###
Arctos_cleaned<-Arctos_all


### Read in and analyze eBird data to figure out how many species of each order could be present in the specimen data set ###
eBird_CA<-read.csv("./Data/ebird_US-CA__1950_2025_1_12_barchart.csv",row.names=1)
eBird_CA_samplesizes<-eBird_CA[1,]
eBird_CA<-eBird_CA[(-1),] #Remove first row after assigning it to a separate variable that has the number observation for each month quarter

colnames(eBird_CA)<-paste(rep(month.name,each=4),"_",rep(1:4,3),sep="")#Set column names to month and quarter of each month
rownames(eBird_CA)<-gsub("[><]","",str_match(rownames(eBird_CA), ">\\s*(.*?)\\s*<"))[,1]#Set row names to scientific names

eBird_CA_yearlyabundance<-apply(eBird_CA,1,sum)
eBird_CA_filter<-eBird_CA_yearlyabundance>0.1 #Can adjust this level to decide what species are included in the possible pool for each order

species_pool<-rownames(eBird_CA)[which(eBird_CA_filter)]

species_pool<-species_pool[-grep("sp[.]",species_pool)]
species_pool<-species_pool[-grep("/",species_pool)]
species_pool<-species_pool[-grep(" x ",species_pool)]

### Match species pool to our NACC-based taxonomy to get comprehensive species pool and Arctos datat set###
species_pool_df<-data.frame(species=species_pool,order=NACC[species_pool,]$order)

### Assigning the NAs to orders in the pool ###
species_pool_df$order[is.na(species_pool_df$order)]<-c("Charadriiformes","Charadriiformes","Pelecaniformes","Pelecaniformes","Accipitriformes","Strigiformes","Passeriformes")

Arctos_missing_from_pool<-unique(Arctos_cleaned$genus_species[!Arctos_cleaned$genus_species %in% species_pool_df$species])

Arctos_missing_from_pool_df<-data.frame(species=Arctos_missing_from_pool,order=rep(NA,length(Arctos_missing_from_pool)))
Arctos_missing_from_pool_df <- Arctos_missing_from_pool_df[-c(13,37),]
Arctos_missing_from_pool_df$order<-c("Accipitriformes","Strigiformes","Strigiformes","Strigiformes","Procellariiformes","Passeriformes","Procellariiformes","Piciformes","Passeriformes","Passeriformes","Charadriiformes","Passeriformes","Gruiformes","Accipitriformes","Charadriiformes","Apodiformes","Galliformes","Piciformes","Suliformes","Passeriformes","Charadriiformes","Charadriiformes","Procellariiformes","Strigiformes","Falconiformes","Passeriformes","Strigiformes","Gaviiformes","Passeriformes","Passeriformes","Passeriformes","Passeriformes","Strigiformes","Charadriiformes","Passeriformes")

#Arctos_missing_from_pool_df$order<-c("Apodiformes","Galliformes","Piciformes","Passeriformes","Strigiformes","Passeriformes","Passeriformes","Passeriformes","Passeriformes","Passeriformes","Galliformes","Gruiformes","Accipitriformes","Charadriiformes","Accipitriformes","Strigiformes","Strigiformes","Charadriiformes","Falconiformes","Passeriformes","Strigiformes","Strigiformes","Gaviiformes","Passeriformes","Strigiformes","Charadriiformes","Passeriformes","Apodiformes","Apodiformes","Strigiformes","Apodiformes")
species_pool_df<-rbind(species_pool_df,Arctos_missing_from_pool_df)

species_pool_df<-species_pool_df[-grep("sp.",species_pool_df$species),]

### Final pool of potential species that could occur in the Arctos data set that we will use to examine species and specimen counts in proportion to these numbers ###
species_pool_counts<-table(species_pool_df$order)

### select columns to keep in NACC for specimen tally (order, family, species)
keeps<-c("order","family","species")
species_list<-NACC[keeps]

##rename column
colnames(species_list)[3] = "genus_species"

### merge to Arctos_all by genus_species
Arctos_orders <- merge(Arctos_all, species_list, by="genus_species")

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

unique(Arctos_order_species[Arctos_order_species$order=="Strigiformes","genus_species"])
unique(Arctos_order_species$genus_species)
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

## Divide counts by the number of possible species for each order to get a proportion ###
i<-1
specimen_count_proportional<-vector()
for(i in 1:nrow(specimen_count)){
  specimen_count_proportional[i]<-specimen_count[i,]$count/species_pool_counts[as.character(specimen_count[i,1])]
}
specimen_count<-cbind(specimen_count,count_proportional=specimen_count_proportional)

species_count_proportional<-vector()
for(i in 1:nrow(species_count_per_order)){
  species_count_proportional[i]<-species_count_per_order[i,]$count/species_pool_counts[as.character(specimen_count[i,1])]
}
species_count_per_order<-cbind(species_count_per_order,count_proportional=species_count_proportional)

## write to csv
write.csv(specimen_count, "./Data/specimens_per_order.csv")
write.csv(species_count_per_order, "./Data/species_per_order.csv")
#species_per_order <- read.csv("./Data/species_per_order.csv")
#specimens_per_order <- read.csv("./Data/specimens_per_order.csv")
specimens_per_order <- specimen_count
species_per_order <- species_count_per_order


### Now log-transform for analyses
## log transform data (very skewed)
#count_proportional_byorder$log_active_species_prop <- log(count_proportional_byorder$species_count_active_proportional+1)
#count_proportional_byorder$log_salvage_species_prop <- log(count_proportional_byorder$species_count_salvage_proportional+1)
species_per_order$log_species_prop <- log(species_per_order$count_proportional+1)

#count_proportional_byorder$log_active_specimen_prop <- log(count_proportional_byorder$specimen_count_active_proportional+1)
#count_proportional_byorder$log_salvage_specimen_prop <- log(count_proportional_byorder$specimen_count_salvage_proportional+1)
specimens_per_order$log_specimens_prop <- log(specimens_per_order$count_proportional+1)

### pivot tables
species_per_order <- species_per_order %>%
  pivot_wider(
    names_from = coll_method,
    values_from = c(count, count_proportional, log_species_prop)
  )


specimens_per_order <- specimens_per_order %>%
  pivot_wider(
    names_from = coll_method,
    values_from = c(count, count_proportional, log_specimens_prop)
  )

## replace NAs with 0
species_per_order[is.na(species_per_order)] <- 0

specimens_per_order[is.na(specimens_per_order)] <- 0


## rename columns prior to merging
species_per_order <- species_per_order %>%
  rename(
    species_count_salvage = count_salvage,
    species_count_active = count_active,
    species_count_prop_salvage = count_proportional_salvage,
    species_count_prop_active = count_proportional_active
  )

specimens_per_order <- specimens_per_order %>%
  rename(
    specimens_count_salvage = count_salvage,
    specimens_count_active = count_active,
    specimens_count_prop_salvage = count_proportional_salvage,
    specimens_count_prop_active = count_proportional_active
  )


## now merge

taxa_dataset <- merge(species_per_order, specimens_per_order, by = "order")

write.csv(taxa_dataset, "./Data/taxa_dataset_for_analyses_and_plotting.csv")

#taxa_dataset <-read.csv("./Data/taxa_dataset_for_analyses_and_plotting.csv")

### Correlation tests ###
cor.test(taxa_dataset$log_species_prop_salvage,taxa_dataset$log_species_prop_active)
cor.test(taxa_dataset$log_specimens_prop_salvage,taxa_dataset$log_specimens_prop_active)

## make rownames into separate column
#taxa_dataset <- tibble::rownames_to_column(taxa_dataset, "order") 

## now plot
ggplot(data = taxa_dataset, aes(x = log_species_prop_salvage, y = log_species_prop_active, color = X)) +
  # Scatterplot with point size
  geom_point() +
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_text(
  label=taxa_dataset$X, 
  nudge_x = 0.1, nudge_y = 0.1, 
  check_overlap = T, size = 2
   )+
  #annotate("text", x = min(order_species_wide$log_salvage), y = max(order_species_wide$log_active), label = eq_text, hjust = 0, size = 5, color = "darkred")+
  labs( 
    x = "log-transformed count of salvaged species", 
    y = "log-transformed count of actively collected species",
    color = "Order") +  # Legend title
  theme_minimal() 

ggplot(data = taxa_dataset, aes(x = specimen_count_prop_salvage, y = specimen_count_prop_active, color = X)) +
  geom_point() +  # Scatterplot with point size
  geom_smooth(method = "lm", color = "red", se = FALSE)+
  geom_text(
   label=taxa_dataset$X, 
   nudge_x = 0.1, nudge_y = 0.1, 
   check_overlap = T, size = 2
   )+
  labs( 
    x = "log-transformed count of salvaged specimens", 
    y = "log-transformed count of actively collected specimens",
    color = "Order") +  # Legend title
  theme_minimal() 



### Table 1
## remove extra column 
## species per order represented by salvaged and actively-collected specimens
species_per_order <- species_per_order[ , !(names(species_per_order) %in% c("count_proportional"))]
species_per_order <- species_per_order[ , -1]
species_per_order <- species_per_order %>%
  pivot_wider(names_from = coll_method, values_from = count)

colnames(species_per_order)[c(2, 3)] <- c("salvaged_species", "actively-collected_species")


## specimens per order represented by salvaged and actively-collected specimens
specimens_per_order <- specimens_per_order[ , !(names(specimens_per_order) %in% c("count_proportional"))]
specimens_per_order <- specimens_per_order[ , -1]
specimens_per_order <- specimens_per_order %>%
  pivot_wider(names_from = coll_method, values_from = count)

colnames(specimens_per_order)[c(2, 3)] <- c("salvaged_specimens", "actively-collected_specimens")
## merge tables together for Table 1
Table_1 <- merge(species_per_order, specimens_per_order, by = "order")
Table_1[is.na(Table_1)] <- 0

write.csv(Table_1, "./Figures/Table_1.csv")
