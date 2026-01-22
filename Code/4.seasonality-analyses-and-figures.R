library(tidyverse)
library(ggplot2)
library(viridis)
library(lubridate)
## split Arctos_dates_preps date column into year + month/day
#Arctos_dates_preps[c('year','month','day')] <- str_split_fixed(Arctos_dates_preps$date, '-', 3)
#Arctos_dates_preps <- Arctos_dates_preps %>% unite(day_month, day, month, sep = "-")

## convert between date formats
Arctos_dates_preps$mdy_format <- format(Arctos_dates_preps$date, "%m-%d-%Y")
## extract month
Arctos_dates_preps$month <- month(ymd(Arctos_dates_preps$date))

Arctos_month_grouped <- Arctos_dates_preps %>% group_by(coll_method, month) %>% summarize(count = n())

### split into active and salvage
df_salvage <- Arctos_dates_preps%>% filter(coll_method == "salvage")
df_active <- Arctos_dates_preps%>% filter(coll_method == "active")

## calculate proportions
month_counts_salvage <- table(df_salvage$month)
prop_month_salvage <- as.data.frame(prop.table(month_counts_salvage))

month_counts_active <- table(df_active$month)
prop_month_active <- as.data.frame(prop.table(month_counts_active))

## bin dates by month
#month_binned_Arctos <- Arctos_dates_preps %>% 
  #group_by(coll_method, month) %>% summarize(count = n())

## get frequencies of date
#date_counts_salvage<- as.data.frame(table(df_salvage$day_month))
#date_counts_active<- as.data.frame(table(df_active$day_month))

## rename columns
#names(date_counts_salvage) <- c("day_month", "count")
#names(date_counts_active) <- c("day_month", "count")

## re-add collecting method
#date_counts_salvage <- date_counts_salvage %>% mutate(coll_method = "salvage")
#date_counts_active <- date_counts_active %>% mutate(coll_method = "active")

#date_counts <- rbind(date_counts_salvage, date_counts_active)

## add month names for graphing
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 1, "1-Jan")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 2, "2-Feb")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 3, "3-Mar")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 4, "4-Apr")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 5, "5-May")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 6, "6-Jun")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 7, "7-Jul")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 8, "8-AUg")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 9, "9-Sep")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 10, "10-Oct")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 11, "11-Nov")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 12, "12-Dec")

## create polar bar chart
Arctos_date_polar_chart_dm <- ggplot(Arctos_month_grouped, aes(x = month, y = count, fill = coll_method)) +
  geom_bar(position="fill", stat = "identity") +
  coord_polar(start = 0) +  scale_fill_viridis_d(option = "viridis") +
  xlab("Collecting Method") +
  ylab("Proportion of Specimens") + theme_minimal()
 
Arctos_date_polar_chart_dm


Arctos_date_polar_chart <- ggplot(Arctos_month_grouped, aes(x = month, y = count, fill = coll_method)) +
  geom_bar(position="stack", stat = "identity") +
  coord_polar(start = 0) +  scale_fill_viridis_d(option = "viridis") +
  xlab("Collecting Method") +
  ylab("Specimen Count") + theme_minimal()

Arctos_date_polar_chart

