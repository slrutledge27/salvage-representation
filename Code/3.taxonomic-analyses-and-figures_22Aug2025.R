library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr) #For cleaning eBird data to get order species counts for the state of California
library(patchwork)
library(openxlsx)
require(VennDiagram)

### import NACC species list
NACC <-read.csv("./Data/NACC_list_species.csv")
rownames(NACC)<-NACC$species

NACC_orders_phylogenetic_order<-unique(NACC$order)
length(unique(NACC$order))

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
Arctos_all<-Arctos_all[-grep("NA",Arctos_all$genus_species),] #Remove Selasphorus NA, other Selasphorus are included

### Save the cleaned MVZ Arctos data set to a new csv ###
Arctos_cleaned<-Arctos_all


### Species-level stats ###
length(unique(Arctos_cleaned$genus_species)) #264 total species in the data set

Arctos_salvaged<-Arctos_cleaned[Arctos_cleaned$coll_method=="salvage",]
Arctos_active<-Arctos_cleaned[Arctos_cleaned$coll_method=="active",]

### Specimen-level stats ###
nrow(Arctos_cleaned) #4978 total specimens
nrow(Arctos_salvaged) #2420 salvaged specimens
nrow(Arctos_salvaged) / nrow(Arctos_cleaned) # 48.61% salvaged
nrow(Arctos_active) #2558 active specimens
nrow(Arctos_active) / nrow(Arctos_cleaned) # 51.37% active

venn_both<-intersect(unique(Arctos_salvaged$genus_species),unique(Arctos_active$genus_species))
length(venn_both) #109 are in both active and salvage
venn_salvaged<-setdiff(unique(Arctos_salvaged$genus_species),unique(Arctos_active$genus_species))
length(venn_salvaged) #106 are only salvaged
venn_active<-setdiff(unique(Arctos_active$genus_species),unique(Arctos_salvaged$genus_species))
length(venn_active) #49 are only active

## Venn Diagram Figure ##
png(file="./Figures/activesalvage_venndiagram_v1.png",res=500,width=3.25,height=3.25,units="in")
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

NACC_orders_phylogenetic_order_represented<-NACC_orders_phylogenetic_order[NACC_orders_phylogenetic_order %in% unique(Arctos_cleaned$order)]

species_pool_counts<-species_pool_counts[NACC_orders_phylogenetic_order_represented]

### Get the order for each species in our cleaned data set ###
Arctos_cleaned$order<-rep(NA,nrow(Arctos_cleaned))

for(i in 1:nrow(Arctos_cleaned)){
  Arctos_cleaned$order[i]<-NACC[Arctos_cleaned$genus_species[i],]$order
}


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
length(salvage_species_counts)

active_species_counts<-Arctos_cleaned[Arctos_cleaned$coll_method=="active",]$order[!duplicated(Arctos_cleaned[Arctos_cleaned$coll_method=="active",]$genus_species)]
active_species_counts<-table(active_species_counts)
active_species_counts<-active_species_counts[NACC_orders_phylogenetic_order_represented]
names(active_species_counts)<-NACC_orders_phylogenetic_order_represented
active_species_counts[is.na(active_species_counts)]<-0
length(which(active_species_counts>0))

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

plot(taxonomy_table_corr[,3],taxonomy_table_corr[,4])
cor.test(taxonomy_table_corr[,3],taxonomy_table_corr[,4]) #Report this in results
