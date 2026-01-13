df_skin <- Arctos_dates_preps%>% filter(PREP == "skin")
df_skel <- Arctos_dates_preps %>% filter(PREP == "skel")
df_fluid <- Arctos_dates_preps %>% filter(PREP == "fluid")
df_partial <-Arctos_dates_preps %>% filter(PREP == "partial")

## get counts
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
