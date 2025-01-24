library(tidyverse)
library(dplyr)
library(ggplot2)

### read in cleaned csv file 
Arctos_all<-read.csv("./Data/Arctos_all.csv")

### import NACC species list
NACC <-read.csv("./NACC_list_species.csv")

##get counts for species by order in NACC
Order_species_count <- NACC %>% 
  filter(!is.na(species)) %>%
  group_by(species) %>% 
  summarise(order = first(order), n = n())

## rename columns
colnames(Order_species_count)[1]<- "genus_species"

## merge NACC Order_species_count with Arctos_all by genus_species
Order_species_all <- left_join(Arctos_all, Order_species_count, by = "genus_species")

## select columns to keep
keeps<-c("coll_method", "order","genus_species")
Order_species_all<-Order_species_all[keeps]

## split by collecting method
df_salvage <- Order_species_all[which(Order_species_all$coll_method == "salvage"),]
df_active <- Order_species_all[which(Order_species_all$coll_method == "active"),]

Order_species_all <- rbind(Order_species_active, Order_species_salvage)
## write to csv
write.csv(Order_species_all, "./species_per_order_all.csv")


### build a scatter plot; active vs salvage number of species per order
p1<-ggplot(Order_species_all, aes(x=FLO, y=NSPECIMENS, colour=FLOCKING))+geom_jitter(position=position_jitter(0.2), size=3, alpha=0.4)+ geom_boxplot(lwd=1, colour="BLACK",outlier.shape = NA,alpha=0.5)+ theme(aspect.ratio = 1/1,panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black",size =2))+theme(axis.text=element_text(size=10,face="bold",family="sans"),axis.title=element_text(face="bold"))+theme(legend.position="none")+scale_colour_manual(values=c("magenta","darkblue"))+xlab("Flocking")+ylab("Number of Specimens")
flockplot2


