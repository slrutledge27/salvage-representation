### get proportions
Arctos_all<-read.csv("./Data/Arctos_all_w_dates_preps.csv")
## group by coll method and prep type
Arctos_prep_grouped <- Arctos_all %>% group_by(coll_method, PREP) %>% summarize(count = n())

## get proportions
df_salvage <- Arctos_all%>% filter(coll_method == "salvage")
df_active <- Arctos_all%>% filter(coll_method == "active")

prep_counts_salvage <- table(df_salvage$PREP)
prop_preps_salvage <- as.data.frame(prop.table(prep_counts_salvage))

prep_counts_active <- table(df_active$PREP)
prop_preps_active <- as.data.frame(prop.table(prep_counts_active))

### build figures
## raw counts
ggplot(Arctos_prep_grouped, aes(fill=PREP, y=count, x=coll_method)) + 
  geom_bar(position="stack", stat="identity") + scale_fill_viridis_d(option = "viridis") +  
  xlab("Collecting Method") +
  ylab("Specimen Count") + theme_minimal()

## proportion as donut plot  09 Feb 2026
Arctos_donut<-ggplot(Arctos_prep_grouped, aes(fill=PREP, y=count, x=coll_method)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_viridis_d(option = "viridis") +
  xlab("") +
  ylab("Proportion of Specimens") +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(fill = "Specimen Type") +
  theme(
    axis.text.y  = element_blank()
  ) +  
  annotate("text",
           x = 2,                 # radial position
           y = 0.25,        # 25% around circle
           label = "Salvage",
           size = 4) +
  annotate("text",
           x = 1,                 # radial position
           y = 0.25,        # 25% around circle
           label = "Active",
           size = 4)

png(file="./Figures/PrepTypeDonut_v1.png",width=6.5,height=6.5,res=500,units="in")
Arctos_donut
dev.off()

ggplot(Arctos_prep_grouped, aes(fill=PREP, y=count, x=coll_method)) + 
  geom_bar(position="fill", stat="identity") + 
  scale_fill_viridis_d(option = "viridis") +
  xlab("Collecting Method") +
  ylab("Proportion of Specimens") +
  coord_polar(theta = "y") +
  theme_minimal()

## get counts
df_skin <- Arctos_dates_preps%>% filter(PREP == "skin")
df_skel <- Arctos_dates_preps %>% filter(PREP == "skel")
df_fluid <- Arctos_dates_preps %>% filter(PREP == "fluid")
df_partial <-Arctos_dates_preps %>% filter(PREP == "partial")

nrow(df_skin) #3444
## per active
sum(df_skin$coll_method == "active") #2210
## per salvage
sum(df_skin$coll_method == "salvage") #1234

nrow(df_skel) #1003
## per active
sum(df_skel$coll_method == "active") #143
## per salvage
sum(df_skel$coll_method == "salvage") #860

nrow(df_fluid) #56
## per active
sum(df_fluid$coll_method == "active") #2
## per salvage
sum(df_fluid$coll_method == "salvage") #54

nrow(df_partial) #200
## per active
sum(df_partial$coll_method == "active") #64
## per salvage
sum(df_partial$coll_method == "salvage") #136

nrow(Arctos_dates_preps) #4703
