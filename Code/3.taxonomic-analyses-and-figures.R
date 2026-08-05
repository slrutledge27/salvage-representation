library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr) #For cleaning eBird data to get order species counts for the state of California
library(patchwork)
library(VennDiagram)
library(xlsx)

### import NACC species list
NACC <-read.csv("./Data/NACC_list_species.csv")
rownames(NACC)<-NACC$species

NACC_orders_phylogenetic_order<-unique(NACC$order)
length(unique(NACC$order)) #31

### read in cleaned csv file 
Arctos_all<-read.csv("./Data/Arctos_all_w_dates_preps.csv")
# 4703

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
Arctos_all <- Arctos_all %>% distinct()

Arctos_all<-Arctos_all[-grep("NA",Arctos_all$genus_species),] #Remove Selasphorus NA, other Selasphorus are included

### Save the cleaned MVZ Arctos data set to a new csv ###
Arctos_cleaned<-Arctos_all
Arctos_salvaged<-Arctos_cleaned[Arctos_cleaned$coll_method=="salvage",]
Arctos_active<-Arctos_cleaned[Arctos_cleaned$coll_method=="active",]

### Specimen-level stats ###
nrow(Arctos_cleaned) #4978 total specimens ## 4645?
nrow(Arctos_salvaged) #2420 salvaged specimens ## 2231?
nrow(Arctos_salvaged) / nrow(Arctos_cleaned) # 48.61% salvaged ## 48.03 %?
nrow(Arctos_active) #2558 active specimens ## 2414?
nrow(Arctos_active) / nrow(Arctos_cleaned) # 51.37% active ## 51.970%?

### Species-level stats ###
length(unique(Arctos_cleaned$genus_species)) #264 total species in the data set ## 259?

venn_both<-intersect(unique(Arctos_salvaged$genus_species),unique(Arctos_active$genus_species))
length(venn_both) #108 are in both active and salvage # 108
venn_salvaged<-setdiff(unique(Arctos_salvaged$genus_species),unique(Arctos_active$genus_species))
length(venn_salvaged) #104 are only salvaged ## 104
venn_active<-setdiff(unique(Arctos_active$genus_species),unique(Arctos_salvaged$genus_species))
length(venn_active) #47 are only active ## 47

## Venn Diagram Figure ##
png(file="./Figures/activesalvage_venndiagram_v2.png",res=500,width=3.25,height=3.25,units="in")
draw.pairwise.venn(length(venn_active)+length(venn_both),length(venn_salvaged)+length(venn_both),length(venn_both),fill=c("#FF000070","#0000FF70"))
dev.off()

### Read in and analyze eBird data to figure out how many species of each order could be present in the specimen data set ###
eBird_CA<-read.csv("./Data/ebird_US-CA__1950_2025_1_12_barchart.csv",row.names=1)
eBird_CA_samplesizes<-eBird_CA[1,]
eBird_CA<-eBird_CA[(-1),] #Remove first row after assigning it to a separate variable that has the number observation for each month quarter

colnames(eBird_CA)<-paste(rep(month.name,each=4),"_",rep(1:4,3),sep="")#Set column names to month and quarter of each month
rownames(eBird_CA)<-gsub("[><]","",str_match(rownames(eBird_CA), ">\\s*(.*?)\\s*<"))[,1]#Set row names to scientific names

eBird_CA_yearlyabundance<-apply(eBird_CA,1,sum)
eBird_CA_filter<-eBird_CA_yearlyabundance>0.1 #Can adjust this level to decide what species are included in the possible pool for each order

species_pool<-rownames(eBird_CA)[which(eBird_CA_filter)]
sort(species_pool)

species_pool<-species_pool[-grep("sp[.]",species_pool)]
species_pool<-species_pool[-grep("/",species_pool)]
species_pool<-species_pool[-grep(" x ",species_pool)]

sort(species_pool)
length(species_pool)#316 possible species based on eBird

### Match species pool to our NACC-based taxonomy to get comprehensive species pool and Arctos data set###
species_pool_df<-data.frame(species=species_pool,order=NACC[species_pool,]$order)

### Assigning the NAs to orders in the pool ###
species_pool_df$order[is.na(species_pool_df$order)]<-c("Charadriiformes","Charadriiformes","Pelecaniformes","Pelecaniformes","Accipitriformes","Strigiformes","Passeriformes")

### Matching up Arctos taxonomy and NACC taxonomy ###
Arctos_missing_from_pool<-unique(Arctos_cleaned$genus_species[!Arctos_cleaned$genus_species %in% species_pool_df$species])
Arctos_missing_from_pool_df<-data.frame(species=Arctos_missing_from_pool,order=rep(NA,length(Arctos_missing_from_pool)))
Arctos_missing_from_pool_df <- Arctos_missing_from_pool_df[-c(13,37),]
Arctos_missing_from_pool_df$order<-c("Accipitriformes","Strigiformes","Strigiformes","Strigiformes","Procellariiformes","Passeriformes","Procellariiformes","Piciformes","Passeriformes","Passeriformes","Charadriiformes","Passeriformes","Gruiformes","Accipitriformes","Charadriiformes","Apodiformes","Galliformes","Piciformes","Suliformes","Passeriformes","Charadriiformes","Charadriiformes","Procellariiformes","Strigiformes","Falconiformes","Passeriformes","Strigiformes","Gaviiformes","Passeriformes","Passeriformes","Passeriformes","Passeriformes","Strigiformes","Charadriiformes","Passeriformes")

#Arctos_missing_from_pool_df$order<-c("Apodiformes","Galliformes","Piciformes","Passeriformes","Strigiformes","Passeriformes","Passeriformes","Passeriformes","Passeriformes","Passeriformes","Galliformes","Gruiformes","Accipitriformes","Charadriiformes","Accipitriformes","Strigiformes","Strigiformes","Charadriiformes","Falconiformes","Passeriformes","Strigiformes","Strigiformes","Gaviiformes","Passeriformes","Strigiformes","Charadriiformes","Passeriformes","Apodiformes","Apodiformes","Strigiformes","Apodiformes")
species_pool_df<-rbind(species_pool_df,Arctos_missing_from_pool_df)
species_pool_df<-species_pool_df[order(species_pool_df$order),]

### Final pool of potential species that could occur in the Arctos data set that we will use to examine species and specimen counts in proportion to these numbers ###
species_pool_counts<-table(species_pool_df$order)
sum(species_pool_counts) #351 possible species that could reasonably be salvaged or collected within California
                        
### Get the order for each species in our cleaned data set ###
Arctos_cleaned$order<-rep(NA,nrow(Arctos_cleaned))

for(i in 1:nrow(Arctos_cleaned)){
  Arctos_cleaned$order[i]<-NACC[Arctos_cleaned$genus_species[i],]$order
}

NACC_orders_phylogenetic_order_represented<-NACC_orders_phylogenetic_order[NACC_orders_phylogenetic_order %in% unique(Arctos_cleaned$order)]
length(NACC_orders_phylogenetic_order_represented) #number of orders in our dataset ## 20?

species_pool_counts<-species_pool_counts[NACC_orders_phylogenetic_order_represented]

### Summary Specimen Counts by Order ###
salvage_specimen_counts<-table(Arctos_cleaned[Arctos_cleaned$coll_method=="salvage",]$order)
salvage_specimen_counts<-salvage_specimen_counts[NACC_orders_phylogenetic_order_represented]

active_specimen_counts<-table(Arctos_cleaned[Arctos_cleaned$coll_method=="active",]$order)
active_specimen_counts<-active_specimen_counts[NACC_orders_phylogenetic_order_represented]
names(active_specimen_counts)<-NACC_orders_phylogenetic_order_represented
active_specimen_counts[is.na(active_specimen_counts)]<-0

### Summary Species Counts by Order ###
salvage_species_counts<-Arctos_cleaned[Arctos_cleaned$coll_method=="salvage",]$order[!duplicated(Arctos_cleaned[Arctos_cleaned$coll_method=="salvage",]$genus_species)]
salvage_species_counts<-table(salvage_species_counts)
salvage_species_counts<-salvage_species_counts[NACC_orders_phylogenetic_order_represented]
length(salvage_species_counts) ## 20
sort(salvage_species_counts)

active_species_counts<-Arctos_cleaned[Arctos_cleaned$coll_method=="active",]$order[!duplicated(Arctos_cleaned[Arctos_cleaned$coll_method=="active",]$genus_species)]
active_species_counts<-table(active_species_counts)
active_species_counts<-active_species_counts[NACC_orders_phylogenetic_order_represented]
names(active_species_counts)<-NACC_orders_phylogenetic_order_represented
active_species_counts[is.na(active_species_counts)]<-0
length(which(active_species_counts>0)) ## 14
sort(active_species_counts)

### Combine to make Table 1 for paper ###
rutledge_etal_table1<-cbind(salvage_species_counts,active_species_counts,salvage_specimen_counts,active_specimen_counts)
write.xlsx(rutledge_etal_table1,file="./Tables/Rutledge_etal_salvagevsactive_table1_v1.xlsx")

### Look for correlations in taxonomic data set ###
taxonomy_table_corr<-rutledge_etal_table1
for(i in 1:ncol(taxonomy_table_corr)){
  taxonomy_table_corr[,i]<-taxonomy_table_corr[,i]/species_pool_counts
}

plot(taxonomy_table_corr[,1],taxonomy_table_corr[,2])
cor.test(taxonomy_table_corr[,1],taxonomy_table_corr[,2]) #Report this in results
#Pearson's product-moment correlation

#data:  taxonomy_table_corr[, 1] and taxonomy_table_corr[, 2]
#t = -0.79978, df = 18, p-value = 0.4343
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
 #-0.5802055  0.2802467
#sample estimates:
#       cor 
#-0.1852476 


plot(taxonomy_table_corr[,3],taxonomy_table_corr[,4])
cor.test(taxonomy_table_corr[,3],taxonomy_table_corr[,4]) #Report this in results
#Pearson's product-moment correlation

#data:  taxonomy_table_corr[, 3] and taxonomy_table_corr[, 4]
#t = 1.6467, df = 18, p-value = 0.117
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.09607764  0.69333376
#sample estimates:
#      cor 
#0.3618267 



##### Now get specimen counts per species #####
NACC_species_phylogenetic_order<-unique(NACC$species)
length(unique(NACC$species)) #2186

NACC_species_phylogenetic_order_represented<-NACC_species_phylogenetic_order[NACC_species_phylogenetic_order %in% unique(Arctos_cleaned$genus_species)]
length(NACC_species_phylogenetic_order_represented) #number of species in dataset #259


### Summary Specimen Counts by species ###
salvage_specimen_counts<-table(Arctos_cleaned[Arctos_cleaned$coll_method=="salvage",]$genus_species)
salvage_specimen_counts<-salvage_specimen_counts[NACC_species_phylogenetic_order_represented]
salvage_specimen_counts <- as.data.frame(salvage_specimen_counts)
salvage_specimen_counts <- na.omit(salvage_specimen_counts)
rownames(salvage_specimen_counts) <- salvage_specimen_counts[, 1]
salvage_specimen_counts <- arrange(salvage_specimen_counts, desc(Freq))

active_specimen_counts<-table(Arctos_cleaned[Arctos_cleaned$coll_method=="active",]$genus_species)
active_specimen_counts<-active_specimen_counts[NACC_species_phylogenetic_order_represented]
active_specimen_counts <- as.data.frame(active_specimen_counts)
active_specimen_counts <- na.omit(active_specimen_counts)
rownames(active_specimen_counts) <- active_specimen_counts[, 1]
active_specimen_counts <- arrange(active_specimen_counts, desc(Freq))

#### Supplementary Figures for Salvaged and Actively collected specimens by species
salvage <- ggplot(salvage_specimen_counts, aes(x = reorder(species, -Freq), y = Freq)) +
  geom_col(fill = "seagreen", color = "black", width = 1) + # Match 'width' to your interval size
  labs(title = "Salvaged Specimens by Species", x = "Species", y = "Specimen Count") +
  theme_minimal() + theme(
    axis.text.x = element_blank(),  # Remove x-axis tick labels
    axis.ticks.x = element_blank()   # Keep ticks (optional)
  )

salvage

active <- ggplot(active_specimen_counts, aes(x = reorder(species, -Freq), y = Freq)) +
  geom_col(fill = "magenta", color = "black", width = 1) + # Match 'width' to your interval size
  labs(title = "Actively Collected Specimens by Species", x = "Species", y = "Specimen Count") +
  theme_minimal() + theme(
    axis.text.x = element_blank(),  # Remove x-axis tick labels
    axis.ticks.x = element_blank()   # Keep ticks (optional)
  )

active



##### get number of species that were in species pool but not represented in either active or salvaged specimens
## Supplementary tables 1 and 2
active_specimen_counts <- active_specimen_counts %>% rename(species = Var1)
salvage_specimen_counts <- salvage_specimen_counts %>% rename(species = Var1)

dropped_from_species_pool_active <- anti_join(species_pool_df, active_specimen_counts, by= "species")
dropped_from_species_pool_salvage <- anti_join(species_pool_df, salvage_specimen_counts, by= "species")

missing_from_both <- merge(dropped_from_species_pool_active, dropped_from_species_pool_salvage, by = "species")
missing_from_both$order.y <- NULL
missing_from_both <- missing_from_both %>% rename(order = order.x)

### now get count of species missing per order, for Supplementary Figure 1
missing_from_both_by_order <- missing_from_both %>%
  group_by(order) %>%
  summarize(count = n())

missing_from_both_by_order <- missing_from_both_by_order %>% arrange(desc(count))

by_order <- ggplot(missing_from_both_by_order, aes(x = reorder(order, -count), y = count)) +
  geom_col(fill = "cyan", color = "black", width = 1) +  coord_flip() +# Match 'width' to your interval size
  labs(title = "Missing Species by Order", x = "Order", y = "Species Count") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

by_order

