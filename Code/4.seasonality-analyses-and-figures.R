library(tidyverse)
library(ggplot2)
library(viridis)
library(lubridate)

Arctos_all<-read.csv("./Data/Arctos_all_w_dates_preps.csv")

Arctos_all <- Arctos_all%>%
  mutate(date = lubridate::ymd(date))

is.Date(Arctos_all$date)

## split Arctos_all date column into year + month/day
Arctos_all[c('year','month','day')] <- str_split_fixed(Arctos_all$date, '-', 3)

## extract month
Arctos_all$month <- month(ymd(Arctos_all$date))

Arctos_month_grouped <- Arctos_all %>% group_by(coll_method, month) %>% summarize(count = n())

### split into active and salvage
df_salvage <- Arctos_all%>% filter(coll_method == "salvage")
df_active <- Arctos_all%>% filter(coll_method == "active")

## calculate proportions
month_counts_salvage <- table(df_salvage$month)
prop_month_salvage <- as.data.frame(prop.table(month_counts_salvage))

month_counts_active <- table(df_active$month)
prop_month_active <- as.data.frame(prop.table(month_counts_active))

## add month names for graphing
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 1, "01-Jan")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 2, "02-Feb")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 3, "03-Mar")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 4, "04-Apr")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 5, "05-May")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 6, "06-Jun")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 7, "07-Jul")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 8, "08-Aug")
Arctos_month_grouped$month <- replace(Arctos_month_grouped$month, Arctos_month_grouped$month == 9, "09-Sep")
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
  geom_col(
    position = "dodge",
    width = 0.8
  ) +
  geom_vline(
    data = data.frame(xintercept = seq_len(12) + 0.5),
    aes(xintercept = xintercept),
    linetype = "dotted",
    linewidth = 0.4,
    color = "grey60"
  ) +
  coord_polar(start = 0) +  
  scale_fill_viridis_d(option = "viridis",labels = c("Active", "Salvage")) +
  xlab("Collecting Method") +
  ylab("Specimen Count") + 
  theme_minimal(base_size=10) +
  theme(
    panel.grid.major.x = element_blank(),          # remove default angular lines
  )+
  labs(fill = "Specimen Type") 

Arctos_date_polar_chart

## Polar bar chart with collecting methods side by side by month
png(file="./Figures/SeasonalityFig_v2.png",width=6.5,height=6.5,units="in",res=500)
Arctos_date_polar_chart
dev.off()

getwd()
