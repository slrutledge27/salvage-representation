library(sf) #for managing vectors
library(terra) #for managing rasters
library(gridExtra)

### Read in GeoTiff of population density data ###
pop_density<-rast("./Shapefiles/gpw_v4_population_density_rev11_2020_15_min.tif")
pop_density_utm<-terra:::project(pop_density,"+proj=utm +zone=10")

### Read in raw occurence data for active and salvage specimens ###
active<-read.csv("./Data/Arctos_active.csv")
salvage<-read.csv("./Data/Arctos_salvage.csv")

### Remove NA values from each data set ###
active<-active[!is.na(active$dec_lat),]
salvage<-salvage[!is.na(salvage$dec_lat),]

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
centroid_utmgrid<-vect(as(st_centroid(CA_grid_mask),"Spatial"))
centroid_popdensity_values<-terra::extract(pop_density_utm,centroid_utmgrid)

### Because the rasters are slightly off, we'll replace the NA values (which are all near the coast) with 0 for plotting purposes ###
centroid_popdensity_values[,2][is.na(centroid_popdensity_values[,2])]<-0

### Count the number of points within each cell for both data sets ###
active_intersect<-st_intersects(CA_grid_mask,active_sf_UTM)
salvage_intersect<-st_intersects(CA_grid_mask,salvage_sf_UTM)

### Use the lengths function to get the number of hits for each cell intersect ###
active_counts<-lengths(active_intersect)
salvage_counts<-lengths(salvage_intersect)

### Create a vector index for those grid cells that have either a salvage or an active specimen ###
populated_cell_index<-which(!(active_counts==0 & salvage_counts==0))

### Create spatial feature object that combines the grid polygon with our active, salvage, and popdensity data log transformed for plotting ###
plot_data<-st_as_sf(CA_grid[CA_UTM],active_plot=log(active_counts+1),salvage_plot=log(salvage_counts+1),pop_den_plot=log(centroid_popdensity_values[,2]+1))

### Draft figure 2 column width, 6 panels. 2 rows x 3 columns. Top row is rasters and grid values, bottom row is scatterplots correlation tests ###
### Panel A: Actively Collected Specimens ###
par(mar = c(0, 0, 0, 0))
plot(plot_data["active_plot"],ann=FALSE)

### Panel B: Salvaged Specimens ###
par(mar = c(0, 0, 0, 0))
plot(plot_data["salvage_plot"])

### Panel C: Population Density ###
par(mar = c(0, 0, 0, 0))
plot(plot_data["pop_den_plot"])

### Panel D: Active vs Salvage ###
par(mar = c(3, 3, 1, 1))
plot(log(active_counts[populated_cell_index]+1),log(salvage_counts[populated_cell_index]+1),cex.axis = 1.00, ann = FALSE)

cor.test(log(active_counts+1),log(salvage_counts+1))#Correlation is negative and significant. There is an inverse correlation between salvage and active specmien counts.
abline(lm(log(salvage_counts+1)~log(active_counts+1)))


### Panel E: Active vs Pop Density ###
par(mar = c(3, 3, 1, 1))
plot(log(centroid_popdensity_values[populated_cell_index,2]+1),log(active_counts[populated_cell_index]+1), cex.axis = 1.00, ann = FALSE)

abline(lm(log(active_counts[populated_cell_index]+1)~log(centroid_popdensity_values[populated_cell_index,2]+1)))
cor.test(log(centroid_popdensity_values[populated_cell_index,2]+1),log(active_counts[populated_cell_index]+1))


### Panel F: Salvage vs Pop Density ###
par(mar = c(3, 3, 1, 1))
plot(log(centroid_popdensity_values[populated_cell_index,2]+1),log(salvage_counts[populated_cell_index]+1),  cex.axis = 1.00, ann = FALSE)

abline(lm(log(salvage_counts[populated_cell_index]+1)~log(centroid_popdensity_values[populated_cell_index,2]+1)))
cor.test(log(centroid_popdensity_values[populated_cell_index,2]+1),log(salvage_counts[populated_cell_index]+1))


### export plots to pdfs; read in pdfs to form 6-panel figure - THIS WORKS BETTER THAN EXPORTING AS PNGs!!!
library(pdftools)
library(magick)

## need to convert pdfs into images
pdf_files <- c("./Figures/p1.pdf", "./Figures/p2.pdf", "./Figures/p3.pdf", "./Figures/p4.pdf", "./Figures/p5.pdf", "./Figures/p6.pdf")

# Convert each PDF to an image
images <- lapply(pdf_files, function(pdf) {
  image_read(pdf_render_page(pdf, page = 1, dpi = 300))
})

final_image <- image_append(c(
  image_append(c(images[[1]], images[[2]], images[[3]]), stack = FALSE),
  image_append(c(images[[4]], images[[5]], images[[6]]), stack = FALSE)
), stack = TRUE)

print(final_image)

# Save output
image_write(final_image, "Salvage-figure.png")

