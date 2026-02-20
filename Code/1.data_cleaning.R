library(tidyverse)
library(dplyr)
library(pacman)

######## Importing and cleaning data #####################################
setwd("~/salvage-representation")

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

z <- z %>%
  separate(guid, into = c("collection", "taxa", "catalog"), sep = ":", remove = FALSE)

## see collection entries
unique(z$collection)
#  "MVZ"    "MVZObs" "UWYMV"  "MLZ"    "UCM"    "UTEP"   "UMZM"   "UCSC"   "DMNS"

## keep only "MLZ" and "MVZ" records
z <- z %>% filter(collection == "MVZ" | collection == "MLZ")

unique(z$collection)
### subset to keep only needed columns
known_coll_method <- z[c('guid','accn_number','spec_locality','scientific_name', 'dec_lat', 'dec_long','coll_method')]

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

### go through remaining localities
unknown_localities<- w[which(w$coll_method == "unknown"),] %>% group_by(guid, accn_number,verbatim_date, spec_locality) %>% 
  summarize(count=n())
### write to csv to send to Carla and Raurie
#write.csv(unknown_localities, "./Data/Arctos_unknown_localities.csv")
## upon obtaining response from Carla...
# import csv with new collecting method column
Arctos_unknown_methods_added <- read.csv("./Data/Table_S1-unknown_localities_method_added.csv")
m <- Arctos_unknown_methods_added

## string split
m <- m %>%
  separate(guid, into = c("collection", "taxa", "catalog"), sep = ":", remove = FALSE)

## see collection entries
unique(m$collection)
#  "MLZ"    "MVZ"    "MVZObs" "NMU"    "UAM"    "UCSC"   "UMNH"   "UTEP"   "UWYMV"

## keep only "MLZ" and "MVZ" records
m <- m %>% filter(collection == "MVZ" | collection == "MLZ")

## now look at new methods added...
unique(m$method)
## ""        "shotgun" "salvage" "trap"    "mistnet"

## assign as "salvage" or "active" in new column
m <- m %>% mutate(added_method = ifelse(method == "shotgun" | method =="trap" | method =="mistnet", "active", "salvage"))


## string split
w <- w %>%
  separate(guid, into = c("collection", "taxa", "catalog"), sep = ":", remove = FALSE)

## see collection entries
unique(w$collection)
#  "MVZ"    "MLZ"    "UAM"    "NMU"    "UMNH"   "UTEP"   "UWYMV"  "MVZObs" "UCSC"

## keep only "MLZ" and "MVZ" records
w <- w %>% filter(collection == "MVZ" | collection == "MLZ")

##keep only necessary columns in w for merging
k <- w[c('guid','accn_number','spec_locality','verbatim_date','scientific_name', 'dec_lat', 'dec_long','coll_method')]

## merge 
b <-merge(k, m, by=c('guid', 'accn_number', 'spec_locality'), all=TRUE)
#c <-merge(m, k, by=c('guid', 'accn_number', 'spec_locality'), all=TRUE)

# getting 1671 obs. when it should be 1664...figure out duplicate entries
#guids<- b%>% group_by(guid) %>% 
  #summarize(count=n())

## 2 each of: MVZ:Bird:180675, MVZ:Bird:181745, MVZ:Bird:181769, MVZ:Bird:181770, MVZ:Bird:181771, MVZ:Bird:183090, MVZ:Bird:183163

## look at each one separately...issues with different date formats...solution: do not merge by verbatim date...problem solved

## make new collecting method column
b <- b %>% mutate(coll_method_ = ifelse(coll_method == "salvage" | added_method =="salvage", "salvage", "active"))

unknown_coll_method <- b[c('guid','accn_number','spec_locality','scientific_name', 'dec_lat', 'dec_long','coll_method_')]
unknown_coll_method <- unknown_coll_method %>% 
  rename(
    coll_method = coll_method_)
##### bind unknown and known datasets
Arctos_all <- rbind(unknown_coll_method, known_coll_method)

### if no dec_lat & dec_long, remove from dataset
Arctos_all<-Arctos_all[!is.na(Arctos_all$dec_lat),]

## check that every entry has an entry for coll_method
unique(Arctos_all$coll_method)
Arctos_all <- Arctos_all %>%
  separate(scientific_name, into = c("genus", "species", "other"), sep = " ", remove = FALSE)

Arctos_all$genus_species <- paste(Arctos_all$genus,Arctos_all$species)

write.csv(Arctos_all, "./Data/Arctos_all.csv")

## get raw counts of specimens, percentages
nrow(Arctos_all[Arctos_all$coll_method == "salvage",]) # 2373

nrow(Arctos_all[Arctos_all$coll_method == "active",]) # 2423



## split by collecting method
df_salvage <- Arctos_all[which(Arctos_all$coll_method == "salvage"),]
df_active <- Arctos_all[which(Arctos_all$coll_method == "active"),]

## get count
species_count_active <- df_active %>% group_by(genus_species)%>% summarize(count=n())
species_count_salvage <- df_salvage %>% group_by(genus_species)%>% summarize(count=n())

print(intersect(species_count_active$genus_species,species_count_salvage$genus_species)) # 109 species represented by both active and salvage

## 109/159 (actively collected species count) = x/100 ; 109/231 (salvaged species count) = x/100

#### add in collecting dates and parts for seasonality and prep type analyses #####################################
## subset raw data to include just guid, accn_number, verbatim_number, and parts
Arctos_all_season_prep <- Arctos_birds_Calif_2000_2020[c('guid','accn_number','verbatim_date','parts')]

## merge with Arctos_all
Arctos_all_season_prep <- merge(Arctos_all_season_prep, Arctos_all, by = c("guid","accn_number"))

##########################################################################################
#################################### DATES ###############################################
##########################################################################################
## clean dates - make sure all are Gregorian calendar
#unique(Arctos_all_season_prep$verbatim_date)
library(lubridate)
pacman::p_load(
  lubridate,  # general package for handling and converting dates  
  parsedate,  # has function to "guess" messy dates
  aweek,      # another option for converting dates to weeks, and weeks to dates
  zoo,        # additional date/time functions
  here,       # file management
  rio,        # data import/export
  tidyverse)  # data management and visualization 


### remove dates that do not have day, month, and year
## format to keep = dd-Mon-yy
regex_pattern <- "^\\d{1,2}\\-"
Arctos_all_season_prep_filtered <- Arctos_all_season_prep[grepl(regex_pattern, Arctos_all_season_prep$verbatim_date), ]

## convert two digit year to four digit year
# split year into separate column
library(stringr)
Arctos_all_season_prep_filtered[c('day', 'month','year')] <- str_split_fixed(Arctos_all_season_prep_filtered$verbatim_date, '-', 3)
# add '20' in front of last two digits of year
prefix_num <- 20
Arctos_all_season_prep_filtered$year <- paste0(prefix_num, Arctos_all_season_prep_filtered$year)
Arctos_all_season_prep_filtered <- Arctos_all_season_prep_filtered %>%
  unite(date, day, month, year, sep = "-")

## convert messy dates to preferred format
Arctos_all_season_prep_filtered <- Arctos_all_season_prep_filtered %>%      
  mutate(date = parse_date(date))

## convert date column into correct class
Arctos_all_season_prep_filtered <- Arctos_all_season_prep_filtered %>%
  mutate(date = lubridate::ymd(date))
## verify column is now "Date" class
class(Arctos_all_season_prep_filtered$date)

write.csv(Arctos_all_season_prep_filtered, "./Data/Arctos_all_w_dates.csv")


#################################################################################
################################PREPS############################################
#################################################################################
## see what prep types are listed
unique(Arctos_all_season_prep_filtered$parts)

## split into SKIN, SKELETON, FLUID, OTHER

Arctos_all_season_prep_filtered <- Arctos_all_season_prep_filtered %>%
  mutate(PREP.skin = ifelse(str_detect(parts, regex("skin", ignore_case = TRUE)), "SKIN", "TBD"))

Arctos_all_season_prep_filtered <- Arctos_all_season_prep_filtered %>%
  mutate(PREP.skel = ifelse(str_detect(parts, regex("skeleton", ignore_case = TRUE)), "SKEL", "TBD"))

Arctos_all_season_prep_filtered <- Arctos_all_season_prep_filtered %>%
  mutate(PREP.fluid = ifelse(str_detect(parts, regex("whole organism", ignore_case = TRUE)), "FLUID", "TBD"))

## break into separate datasets
df_skin <- Arctos_all_season_prep_filtered[which(Arctos_all_season_prep_filtered$PREP.skin == "SKIN"),]
df_skel <- Arctos_all_season_prep_filtered[which(Arctos_all_season_prep_filtered$PREP.skel == "SKEL" & Arctos_all_season_prep_filtered$PREP.skin == "TBD"),]
df_fluid <- Arctos_all_season_prep_filtered[which(Arctos_all_season_prep_filtered$PREP.fluid == "FLUID"),]
df_TBD <-Arctos_all_season_prep_filtered %>% filter(PREP.skin == "TBD" & PREP.skel == "TBD" & PREP.fluid == "TBD")

## make new column
df_skin <- df_skin %>% mutate(PREP = "skin")
df_skel <- df_skel %>% mutate(PREP = "skel")
df_fluid <- df_fluid %>% mutate(PREP = "fluid")
df_TBD <- df_TBD %>% mutate(PREP = "partial")

Arctos_dates_preps <- rbind(df_skin, df_skel, df_fluid, df_TBD)
Arctos_dates_preps$PREP.fluid <- NULL
Arctos_dates_preps$PREP.skel <- NULL
Arctos_dates_preps$PREP.skin <- NULL
write.csv(Arctos_dates_preps, "./Data/Arctos_all_w_dates_preps.csv")
