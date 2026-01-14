library(tidyverse)
library(ggplot2)
library(viridis)
## split Arctos_dates_preps date column into year + month/day
Arctos_dates_preps[c('year','month','day')] <- str_split_fixed(Arctos_dates_preps$date, '-', 3)

## replace month numbers with letters
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "01", "Jan")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "02", "Feb")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "03", "Mar")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "04", "Apr")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "05", "May")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "06", "Jun")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "07", "Jul")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "08", "Aug")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "09", "Sep")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "10", "Oct")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "11", "Nov")
Arctos_dates_preps$month <- replace(Arctos_dates_preps$month, Arctos_dates_preps$month == "12", "Dec")

Arctos_dates_preps <- Arctos_dates_preps %>% unite(day_month, day, month, sep = "-")

### split into active and salvage
df_salvage <- Arctos_dates_preps%>% filter(coll_method == "salvage")
df_active <- Arctos_dates_preps%>% filter(coll_method == "active")

## get frequencies of date
date_counts_salvage<- as.data.frame(table(df_salvage$day_month))
date_counts_active<- as.data.frame(table(df_active$day_month))

## rename columns
names(date_counts_salvage) <- c("day_month", "count")
names(date_counts_active) <- c("day_month", "count")

## re-add collecting method
date_counts_salvage <- date_counts_salvage %>% mutate(coll_method = "salvage")
date_counts_active <- date_counts_active %>% mutate(coll_method = "active")

date_counts <- rbind(date_counts_salvage, date_counts_active)

## create polar bar chart
Arctos_date_polar_chart_dm <- ggplot(date_counts, aes(x = day_month, y = count, fill = coll_method)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal()
Arctos_date_polar_chart_dm
