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

### go through remaining localities
unknown_localities<- w[which(w$coll_method == "unknown"),] %>% group_by(verbatim_date, spec_locality) %>% 
  summarize(count=n())

## assign as active if three or more entries with same localities have same collecting dates, or continous collecting dates (unless clearly a salvage location, e.g. Outside health center); or multiple records for single day across several localities
unknown_localities<- w[which(w$coll_method == "unknown"),] %>% group_by(verbatim_date, spec_locality) %>% 
  summarize(count=n())

## same locality, same collection date
w <- w %>% mutate(coll_method_9 = ifelse(coll_method == "unknown" & spec_locality =="Carmel Valley Rd. at mile marker 26" | spec_locality=="Los Banos" | spec_locality=="Upper Shake Campground"
                                         | spec_locality=="3 mi S and 4 mi E Three Points" | spec_locality=="Moss Landing State Beach" | spec_locality=="Zmudowski State Beach" | spec_locality=="north of Moss Landing" | spec_locality==" Obsidian Dome"
                                         | spec_locality== "1 mi S and 3 mi E town of June Lake" | spec_locality=="1 mi W Hi Mountain Campground" | spec_locality=="3 mi S and 3 mi W Pozo"
                                         | spec_locality=="junction of Rd. 500 and Rd. 520" | spec_locality=="Jackson Demonstration State Forest" | spec_locality=="Paul L. Wattis Sanctuary"
                                         | spec_locality== "ca. 8.5 mi NNE town of Colusa" | spec_locality=="Garcia Mountain" | spec_locality == "2 mi S and 2 mi W Pozo" | spec_locality=="Rd. 500, Jackson Demonstration State Forest"
                                         | spec_locality=="Balch Park Rd." | spec_locality=="Sequoia National Forest" | spec_locality == "USFS Rd. 19S10 (Rancheria Fire Rd.) off Balch Park Rd."
                                         | spec_locality=="entrance to Elkhorn Slough" | spec_locality=="Moss Landing" | spec_locality=="Chalfant Valley" | spec_locality=="ca. 1 mi S Chalfant"
                                         | spec_locality=="Deadman Creek" | spec_locality=="4 mi S and 3.5 mi E town of June Lake" | spec_locality=="Little River State Beach, south of Moonstone"
                                         | spec_locality=="Moss Landing State Beach" | spec_locality=="Mark Stromberg's picnic table, Hastings Natural History Reservation, Carmel Valley"
                                         | spec_locality=="4.5 mi W Shinn Peaks" | spec_locality=="2 mi N and 0.5 mi W Markleeville" | spec_locality=="1 mi E Benton" | spec_locality=="south end of Point Reyes Beach, Point Reyes"
                                         | spec_locality=="Chalfant Valley, ca. 1 mi S Chalfant" | spec_locality=="Upper Shake Campground, 3 mi S and 4 mi E Three Points"| spec_locality=="Martin Rd., Hastings Natural History Reservation"
                                         | spec_locality=="1 mi E Benton" | spec_locality=="Valley View Drive, Stanislaus National Forest" | spec_locality== "S of Dinkey Creek Rd., 1 mi S and 3 mi E town of Shaver Lake"
                                         | spec_locality=="N of Dinkey Creek Rd., 2 mi S and 8 mi E town of Shaver Lake" | spec_locality=="University of California Sierra Foothill Range Field Station, 4.5 mi N Smartville"
                                         | spec_locality=="Eable Lake" | spec_locality=="10 mi S Davis Creek" | spec_locality=="Goose Lake at southern causeway" | spec_locality=="Finch Creek, near entrance to Hastings Natural History Reservation"
                                         | spec_locality=="0.5 mi W Hi Mountain Campground, 3 mi S and 2.5 mi W Pozo"
                                         , "active", "salvage"))


## same date, different localities (3 or more of same date) ### QUESTION TO NICK: DO THESE MAKE SENSE BEFORE I ASSIGN AS ACTIVELY-COLLECTED???
# entrance to Elkhorn Slough, Moss Landing, Elkhorn Slough, Moss Landing, barn, Blue Oak Ranch Reserve, Clam Beach County Park, Clam Beach,
# near Clam Beach; Elkhorn Slough, Moss Landing; 
# 24-Jan-07: Aptos: Fern Flat Rd.; Big Lake, Blue Oak Ranch Reserve; Corralitos: Hames Road; Hastings Entry Lane at Big Creek crossing, Hastings Natural History Reservation;
# 24-Jan-07: School House, Hastings Natural History Reservation; 
# 26-Nov-05: Carmel Valley Rd. at mile marker 19.5; Carmel Valley Rd. at mile marker 21; Santa Cruz: Stephen st. & Emeline ave.
# 26-Nov-12: Matilija Creek drainage, mouth of Murietta Canyon; Santa Cruz: FAC House; Santa Cruz:307 Laguna St.
# 30-Nov-07: Empire Grade: Creek Bed; Moss Landing State Beach; UCSC Arboretum; Zmudowski State Beach
# 31-May-06: Oakhurst: Episcopal Conference Center; Prather: Morgan Canyon: 30705 Pennyroyal Lane; Santa Cruz: 115 Effey St.; Windmill Meadow (50 m S of windmill), Blue Oak Ranch Reserve
# 5-May-06: Fresno Wastewater Treatment Plant; Fresno Wastewater Treatment Plant: Jensen & Cornelia Aves.; Fresno wastewater treatment plant, Jensen & Cornelia aves.
# 7/25/2005 & 7/24/2005 & 7/23/2005: Eable Lake, Eagle Lake

w <- w %>% mutate(coll_method_ = ifelse(coll_method == "salvage" | coll_method_9 =="salvage", "salvage", "active"))


unknown_coll_method <- w[c('scientific_name', 'dec_lat', 'dec_long','coll_method_')]
unknown_coll_method <- unknown_coll_method %>% 
  rename(
    coll_method = coll_method_)
##### bind unknown and known datasets
Arctos_all <- rbind(unknown_coll_method, known_coll_method)

### if no dec_lat & dec_long, remove from dataset
Arctos_all<-Arctos_all[!is.na(Arctos_all$dec_lat),]

