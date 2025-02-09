library(tidyverse)
library(dplyr)

######## Importing and cleaning data #####################################
#####Import raw data
Arctos_birds_Calif_2000_2020 <- read.csv("./Data/Arctos_birds_Calif_2000_2020.csv")
##### filter
## Remove entries w/ 'parts' that are just eggs, nests, or blood ###
df <- Arctos_birds_Calif_2000_2020 %>% filter(!(parts=="egg" | parts  =="egg; nest" | parts == "nest; egg" | parts == "media; egg"))
df2 <- df %>% filter(!(parts=="blood" | parts  =="blood; blood" | parts == "blood; blood; blood" | parts == "blood; blood; blood; blood"))

## remove entries with 'parts' that are 'unknown' or 'nest', as well as 'age' embryo or chick
df3 <- df2 %>% filter(!(parts=="nest" | parts  =="unknown"))
df4 <- df3 %>% filter(!(age == "embryo" | age == "chick" | age == "downy chick" | age == "downy chick, completely covered in fuzz, eyes still closed" | age == "naked chick, eyes closed" | age == "downy chick, fuzz on body, eyes not open" | age == "downy chick, pre-fledge" | age == "downy fledgling" | age == "not yet fully fledged, still downy" | age == "skull unossified, downy (only slightly fuzzy on head & backside of the body" | age == "skull unossified, naked & downy (some fuzz on head and backside, eyes still closed)" | age == "skull unossified, transitioning from naked to downy, eyes closed" | age == "skull unossified, downy, eyes closed" | age == "skull unossified, between naked and downy (some fuzz on head and backside, eyes still closed)")) 

Arctos_all <- df4
##### split into known and unknown collecting methods
Arctos_all$coll_method_2 <- ifelse(Arctos_all$collecting_method == "" | Arctos_all$collecting_method == "unknown", "unknown","known")

Arctos_known <- Arctos_all[which(Arctos_all$coll_method_2 == "known"),]
Arctos_unknown <- Arctos_all[which(Arctos_all$coll_method_2 == "unknown"),]

##### for known dataset, assign based on recorded method as active and salvage
z <- Arctos_known
z <- z %>% 
  mutate(coll_method_3 = ifelse(collecting_method == "dead on road" | collecting_method == "died on barbed wire fence" | collecting_method == "euthanized" | collecting_method == "euthanized while at Wetlands and Wildlife Care Center" | collecting_method == "euthanized while at WWCC" | collecting_method == "found injured and transferred to WWCC where euthanized" | collecting_method == "found injured, transferred to WWCC and euthanized" | collecting_method == "found injured, transferred to WWCC where euthanized" | collecting_method == "found injured, transferred to WWCC, 'wobbly, lies down often',  and euthanized on 2019-08-11" | collecting_method == "gift" | collecting_method == "salavage" | collecting_method == "salbage" | collecting_method == "salvage (caught in Tomahawk trap)" | collecting_method == "salvage"|collecting_method == "salvage " | collecting_method == "salvage (mammal trap)" | collecting_method == "salvaged" | collecting_method == "salvage-hit by vehicle", "salvage", ifelse(collecting_method == "feeder trap" | collecting_method == "feeder trap and observational" | collecting_method == "funnel trap" | collecting_method == "mistnet" | collecting_method == "mistnet and observational" | collecting_method == "netted" | collecting_method == "rat trap" | collecting_method == "shot" | collecting_method == "Shot" |collecting_method == "shotgun" | collecting_method == "trap" | collecting_method == "trapped with bow net", "active","unknown")))

### filter out "recordings" and "observations"
z <- z %>% filter(!(collecting_method=="recording" | collecting_method  =="observational" | collecting_method == "observation"))

### NEED TO DEAL WITH ONE MEASUREMENT #### has quotes inside, causing issues with recognizing it as salvage
z <- z %>% mutate(coll_method = ifelse(coll_method_3 == "active","active", "salvage"))

### subset to keep only needed columns
known_coll_method <- z[c('scientific_name', 'dec_lat', 'dec_long','coll_method')]

##### for unknown dataset...
### assign as salvage based on locality, parts
## university campuses, outside public buildings, military bases, airports, zoos & aviaries, with broad, general, non-specific localities

w <- Arctos_unknown
w <- w %>% 
  mutate(coll_method_3 = ifelse(spec_locality == "Aviary in Oakland" | spec_locality == "aviary of Monica J. Albe, Oakland" | spec_locality == "San Diego Zoo" | spec_locality == "CAPTIVE, San Diego Zoo [probably]" | spec_locality == "CAPTIVE, San Diego Zoo" | spec_locality == "San Francisco Zoo, San Francisco" | spec_locality == "Zoological Society of San Diego" | spec_locality == "Zoological Society of San Diego, CAPTIVE" | spec_locality == "CAPTIVE Zoological Societ of San Diego" | spec_locality == "CAPTIVE Zoological Society of San Diego" | spec_locality == " San Diego: Sea World CAPTIVE" | spec_locality == "Amador High School, 1155 Santa Rita Rd., Pleasanton" | spec_locality == "Pittsburg High School, Pittsburg" | spec_locality == "no specific locality recorded" | spec_locality == "unknown" | spec_locality == "Life Sciences Building courtyard, University of California campus, Berkeley" | spec_locality == "Natural Resources Building, Mineral" | spec_locality == "Outside Earth and Marine Sciences Building, University of California, Santa Cruz" | spec_locality == "near south door of Valley Life Sciences Building, University of California campus, Berkeley" | spec_locality == "near Protein Design Labs (34801 Campus Drive), near Dumbarton Bridge, Fremont" | spec_locality == "W side of Dwinelle Hall, Univ. Calif. campus, Berkeley" | spec_locality == "in vicinity of breezeway between Birge and LeConte Halls, University of California campus, Berkeley" | spec_locality == "Stadium Rim Way between Centennial Rd. and Galey Rd., University of California campus, Berkeley" | spec_locality == "University of California campus, Berkeley" | spec_locality == "University of California Riverside campus, Riverside" | spec_locality == "north side of Hildebrand Hall, University of California campus, Berkeley" | spec_locality == "east side of Lewis Hall, University of California Berkeley campus" |spec_locality == "Unit 2 Dormitory, between Channing and Haste Streets, Berkeley" | spec_locality == "Strawberry Creek Lodge (1320 Addison Street), Berkeley" | spec_locality == "Davis" | spec_locality == "University of California Botanical Garden, Strawberry Canyon, Berkeley" | spec_locality == "Berkeley" | spec_locality == "Life Sciences Addition, University of California, Berkeley" | spec_locality == "Donner Lab, University of California, Berkeley" | spec_locality == "California Hall, University of California, Berkeley" | spec_locality == "Women's Faculty Club, University of California, Berkeley" | spec_locality == "Linsdale Library, Hastings Natural History Reservation" | spec_locality == "San Rafael, 24 Madrona Street" | spec_locality == "San Diego: Sea World CAPTIVE" | spec_locality == "Edwards AFB" | spec_locality == "classroom at Hastings Natural History Reservation" | spec_locality == "labs at Hastings Natural History Reservation, Carmel Valley" | spec_locality == "Lawrence Livermore National Lab" | spec_locality == "outside the Davis Lab, Hastings Natural History Reservation, Carmel Valley" | spec_locality == "Sacramento" | spec_locality == "El Cerrito" | spec_locality == "Ojai" | spec_locality == "Livermore" | spec_locality == "Inverness" | spec_locality == "Bolinas" | spec_locality == "Oakland" | spec_locality == "Pittsburg" | spec_locality == "Pleasant Valley" | spec_locality == "Bethel Island" | spec_locality == "Woodacre" | spec_locality == "Jamesburg" | spec_locality == "Nicasio" | spec_locality == "La Casa de Maria, 801 Ladera Lane, Santa Barbara" | spec_locality == "Anaheim" | spec_locality == "Pt. Mugu Naval Base, Air National Guard hanger" | spec_locality == "Carmel Valley" | spec_locality == "Hopland Research and Extension Center bunkhouse" | spec_locality == "Wildlife Gallery, Bolinas" | spec_locality == "Alameda Naval Station, Alameda" | spec_locality == "Mills College, Oakland", "salvage", "unknown"))

## along roads/highways
w <- w %>% mutate(coll_method_4 = ifelse(coll_method_3 == "unknown" & spec_locality %in% c(grep("highway|hwy", spec_locality, ignore.case = T, value = T)),"salvage","unknown"))
w <- w %>% mutate(coll_method_5 = ifelse(coll_method_3 == "salvage" | coll_method_4 =="salvage", "salvage", "unknown"))

## outside residences (inputted as addresses)
w <- w %>% mutate(coll_method_6 = ifelse(coll_method_5 == "unknown" & grepl("^[0-9]+", spec_locality, ignore.case = T) & !grepl(" mi | km | block | m | yds ", spec_locality, ignore.case = T), "salvage", "unknown"))
w <- w %>% 
  mutate(coll_method_3 = ifelse(spec_locality == "dead on road" | spec_locality == "died on barbed wire fence" | collecting_method == "euthanized" | collecting_method == "euthanized while at Wetlands and Wildlife Care Center" | collecting_method == "euthanized while at WWCC" | collecting_method == "found injured and transferred to WWCC where euthanized" | collecting_method == "found injured, transferred to WWCC and euthanized" | collecting_method == "found injured, transferred to WWCC where euthanized" | collecting_method == "found injured, transferred to WWCC, 'wobbly, lies down often',  and euthanized on 2019-08-11" | collecting_method == "gift" | collecting_method == "salavage" | collecting_method == "salbage" | collecting_method == "salvage (caught in Tomahawk trap)" | collecting_method == "salvage"|collecting_method == "salvage " | collecting_method == "salvage (mammal trap)" | collecting_method == "salvaged" | collecting_method == "salvage-hit by vehicle", "salvage", ifelse(collecting_method == "feeder trap" | collecting_method == "feeder trap and observational" | collecting_method == "funnel trap" | collecting_method == "mistnet" | collecting_method == "mistnet and observational" | collecting_method == "netted" | collecting_method == "rat trap" | collecting_method == "shot" | collecting_method == "Shot" |collecting_method == "shotgun" | collecting_method == "trap" | collecting_method == "trapped with bow net", "active","unknown")))


w <- w %>% mutate(coll_method_7 = ifelse(coll_method_5 == "salvage" | coll_method_6 =="salvage", "salvage", "unknown"))

## only represented by skulls, wings, and/or tissues
w <- w %>% mutate(coll_method_8 = ifelse(w$parts=="tissue" | w$parts == "tissue; tissue sample" | w$parts == "wing; tissue", "salvage", "unknown"))
w <- w %>% mutate(coll_method = ifelse(coll_method_7 == "salvage" | coll_method_8 =="salvage", "salvage", "unknown"))


unknown_localities<- w[which(w$coll_method == "unknown"),] %>% group_by(verbatim_date, spec_locality) %>% 
  summarize(count=n())
### if no known locality, remove from dataset

### assign all other entries as actively-collected


##### bind unknown and known datasets
Arctos_all <- rbind(Arctos_active, Arctos_salvage)

##### split datasets into actively-collected and salvaged specimens dataset
df_salvage <- df7[which(df7$coll_method == "salvage"),]
df_active <- df7[which(df7$coll_method == "active"),]

