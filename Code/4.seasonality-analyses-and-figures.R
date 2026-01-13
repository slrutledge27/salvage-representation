library(tidyverse)
library(ggplot2)
library(viridis)
## split Arctos_dates_preps date column into year + month/day
Arctos_dates_preps[c('year','month','day')] <- str_split_fixed(Arctos_dates_preps$date, '-', 3)


### split into active and salvage
df_salvage <- Arctos_dates_preps%>% filter(coll_method == "salvage")
df_active <- Arctos_dates_preps%>% filter(coll_method == "active")

## get frequencies of date
date_counts_salvage<- as.data.frame(table(df_salvage$month))
date_counts_active<- as.data.frame(table(df_active$month))

## rename columns
names(date_counts_salvage) <- c("month", "count")
names(date_counts_active) <- c("month", "count")

## re-add collecting method
date_counts_salvage <- date_counts_salvage %>% mutate(coll_method = "salvage")
date_counts_active <- date_counts_active %>% mutate(coll_method = "active")

date_counts <- rbind(date_counts_salvage, date_counts_active)

## create polar bar chart
Arctos_date_polar_chart <- ggplot(date_counts, aes(x = month, y = count, fill = coll_method)) +
  geom_bar(stat = "identity") +
  coord_polar(start = 0) +
  theme_minimal()
Arctos_date_polar_chart
