library(sf) #for managing vectors
library(terra) #for managing rasters
library(exactextractr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggnewscale)
library(patchwork)
library(magick)
library(cowplot)
library(grid)

### Read in GeoTiff of population density data ###
pop_density<-rast("./Shapefiles/gpw_v4_population_density_rev11_2020_15_min.tif")
pop_density_utm<-terra:::project(pop_density,"+proj=utm +zone=10")

### Read in raw occurence data for active and salvage specimens ###
all_data<-read.csv("./Data/Arctos_all_w_dates_preps.csv")
salvage<-all_data[all_data$coll_method=="salvage",]
active<-all_data[all_data$coll_method=="active",]

nrow(all_data) # 4703
nrow(salvage) # 2284
nrow(active) # 2419

### Change data format for each set of coordinates so they can be analyzed by the grid below ###
active_sf <- st_as_sf(active, coords = c("dec_long", "dec_lat"), crs = 4326)  
salvage_sf <- st_as_sf(salvage, coords = c("dec_long", "dec_lat"), crs = 4326)  

### Extract the values for each coordinate of the data sets for the pop density raster ###
active_pop_dens<-terra:::extract(pop_density,active_sf)
salvage_pop_dens<-terra:::extract(pop_density,salvage_sf)

### Project points to UTM zone 10, which is what we will use for the Bay Area so we can use meters when setting the grid###
active_sf_UTM<-st_transform(active_sf,crs="+proj=utm +zone=10")
salvage_sf_UTM<-st_transform(salvage_sf,crs="+proj=utm +zone=10")

### Read in USA shape file and trim to California ###
USA <- read_sf(dsn = "./Shapefiles/tl_2023_us_state.shp", layer = "tl_2023_us_state")
CA<-USA[USA$NAME=="California",]

### Change projection to UTM Zone 10
CA_UTM<-st_transform(CA,crs="+proj=utm +zone=10")

### Set up grid to create specimen count heatmap ###
CA_grid<-st_make_grid(CA_UTM,cellsize=20000) #NAM: I tried various values for cellsize, 20000 seems to be pretty good balance

### Here is a plot of what the grids look like ###
plot(CA_grid)
plot(st_geometry(CA_UTM),add=T)
plot(CA_grid[CA_UTM], col = '#ff000088', add = TRUE)

### Isolate the California Grid ###
CA_grid_mask<-CA_grid[CA_UTM]
plot(CA_grid_mask)

### Get population density data for the same grid ###
popdensity_values<-exact_extract(pop_density_utm,CA_grid_mask,'mean')

### Count the number of points within each cell for both data sets ###
active_intersect<-st_intersects(CA_grid_mask,active_sf_UTM)
salvage_intersect<-st_intersects(CA_grid_mask,salvage_sf_UTM)

### Use the lengths function to get the number of hits for each cell intersect ###
active_counts<-lengths(active_intersect)
salvage_counts<-lengths(salvage_intersect)

### Create a vector index for those grid cells that have either a salvage or an active specimen ###
populated_cell_index<-which(!(active_counts==0 & salvage_counts==0))

### Combine these into a single data frame for plotting with ggplot2
count_data<-data.frame(log_active_counts=log(active_counts[populated_cell_index]+1),log_salvage_counts=log(salvage_counts[populated_cell_index]+1),human_pop_density=log(popdensity_values[populated_cell_index]+1))
  
### Create spatial feature object that combines the grid polygon with our active, salvage, and popdensity data log transformed for plotting ###
count_polygons<-st_as_sf(CA_grid[CA_UTM],id=1:length(active_counts),active_plot=log(active_counts+1),salvage_plot=log(salvage_counts+1),pop_den_plot=log(popdensity_values+1))

active_count_poly <- count_polygons %>% select(value = active_plot)
salvage_count_poly <- count_polygons %>% select(value = salvage_plot)
pop_den_poly <- count_polygons %>% select(value = pop_den_plot)

shared_theme_poly <- theme(
  plot.margin = unit(c(2, 2, 2, 2), "pt"),  # tight margins
  axis.text = element_text(size = 8),      # same size text
  legend.position.inside = c(0.8, 0.8),
  legend.key.height = unit(0.3, "cm"),  
  legend.key.width = unit(0.4, "cm"), 
  axis.text.x = element_text(size = 6), 
  axis.text.y = element_text(size = 6), 
  legend.text = element_text(size = 6),
  legend.title = element_text(size = 6),
  plot.title = element_text(size = 8, face = "bold", hjust = 0.5)
  )

shared_theme_scatter <- theme(
  plot.margin = margin(2,2,2,2),  # tight margins
  axis.text.x = element_text(size = 6), 
  axis.text.y = element_text(size = 6),
  axis.title.x = element_text(size = 6),
  axis.title.y = element_text(size = 6),
  coord_fixed()  # Match aspect ratio
)

shared_theme <- theme_void() +
  theme(
    plot.margin = unit(c(2, 2, 2, 2), "pt"),  # tight margins
    axis.text = element_text(size = 8),      # same size text
    legend.position = c(0.8, 0.8),
    legend.key.height = unit(0.3, "cm"),  
    legend.key.width = unit(0.4, "cm"), 
    axis.text.x = element_text(size = 6), 
    axis.text.y = element_text(size = 6), 
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 6),
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

p1 <- ggplot() +
  geom_sf(data = active_count_poly, aes(fill = value)) +
  labs(title="log(Active Specimen Counts)") +
  scale_fill_viridis_c(name = "",option="plasma") +
  shared_theme_poly +
  coord_sf(expand=F)

p2 <- ggplot() +
  geom_sf(data = salvage_count_poly, aes(fill = value)) +
  labs(title="log(Salvage Specimen Counts)") +
  scale_fill_viridis_c(name = "",option="plasma") +
  shared_theme_poly +
  coord_sf(expand=F)

p3 <- ggplot() +
  geom_sf(data = pop_den_poly, aes(fill = value)) +
  labs(title="log(Human Population Density)") +
  scale_fill_viridis_c(name = "",option="plasma") +
  shared_theme_poly +
  coord_sf(expand=F)

p4 <- ggplot(data=count_data,aes(x = log_active_counts, y = log_salvage_counts)) +
  geom_point(size=1) +
  theme_bw() +                         # clean white background with axes and grid
  geom_smooth(method = "lm", se = TRUE) +  # Linear trend line with confidence interval
  shared_theme_scatter +
  labs(x = "log(Active Specimen Counts)", y = "log(Salvage Specimen Counts)") 

p5 <- ggplot(data=count_data,aes(x = human_pop_density, y = log_active_counts)) +
  geom_point(size=1) +
  theme_bw() +                         # clean white background with axes and grid
  geom_smooth(method = "lm", se = TRUE) +  # Linear trend line with confidence interval
  shared_theme_scatter +
  labs(x = "log(Human Population Density)", y = "log(Active Specimen Counts)") 

p6 <- ggplot(data=count_data,aes(x = human_pop_density, y = log_salvage_counts)) +
  geom_point(size=1) +
  theme_bw() +                         # clean white background with axes and grid
  geom_smooth(method = "lm", se = TRUE) +  # Linear trend line with confidence interval
  shared_theme_scatter +
  labs(x = "log(Human Population Density)", y = "log(Salvage Specimen Counts)") 

png(file="./Figures/p1.png",width=6.5/3,height=6.5/3,units="in",res=500)
print(p1)
dev.off()

png(file="./Figures/p2.png",width=6.5/3,height=6.5/3,units="in",res=500)
print(p2)
dev.off()

png(file="./Figures/p3.png",width=6.5/3,height=6.5/3,units="in",res=500)
print(p3)
dev.off()

png(file="./Figures/p4.png",width=6.5/3,height=6.5/3,units="in",res=500)
print(p4)
dev.off()

png(file="./Figures/p5.png",width=6.5/3,height=6.5/3,units="in",res=500)
print(p5)
dev.off()

png(file="./Figures/p6.png",width=6.5/3,height=6.5/3,units="in",res=500)
print(p6)
dev.off()

### Output for results section ###

## Panel D
cor.test(log(active_counts[populated_cell_index]+1),log(salvage_counts[populated_cell_index]+1))#Correlation is negative and significant. There is an inverse correlation between salvage and active specmien counts.

## Panel E
cor.test(log(popdensity_values[populated_cell_index]+1),log(active_counts[populated_cell_index]+1))

## Panel F
cor.test(log(popdensity_values[populated_cell_index]+1),log(salvage_counts[populated_cell_index]+1))
