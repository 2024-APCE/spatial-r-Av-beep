# Spatial analysis in R
# Han Olff nov 2021

rm(list = ls())
# set the working directory where your GIS data are located
setwd("C:/Biology/_projects/APCE2024GIS/apce2024gis")

# restore the libraries of the project 
renv::restore()


# load the different libraries
library(terra)       # for working with raster data
library(tidyterra)   # for adding terra objects to ggplot
library(ggspatial)  # for scale bars
library(sf)          # for vector data objects
library(tidyverse)   # ggplot, dplyr etc
library(scales)      # for oob (out of bounds) scale
library(ggnewscale) # for using multiple color fill scales in ggplot
library(patchwork)  # for combining multiple ggplots in one panel plot

# explore color palettes
# also see https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# Base R palettes
barplot(rep(1,10), col = grey.colors(10))
mycolors<-c("red", "white", "blue")
mycolors
barplot(rep(1,10), col = rev(topo.colors(10))) # rev turns the scale arround
barplot(rep(1,10), col = rev(terrain.colors(10)))
library(RColorBrewer) 
RColorBrewer::display.brewer.all()
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))
barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "BrBG"))
library(viridis)
barplot(rep(1,10), col = viridis::viridis(10))
barplot(rep(1,10), col = viridis::plasma(10))
barplot(rep(1,10), col = viridis::heat(10))
viridis::plasma(10)
library(wesanderson)
barplot(rep(1,10), col = rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous")))
pal_zissou1<-rev(wesanderson::wes_palette("Zissou1", 10, type = "continuous"))
pal_zissou2<-wesanderson::wes_palette("Zissou1", 10, type = "continuous")
pal_zissou1

# load the vector data for the whole ecosystem
sf::st_layers("./2022_protected_areas/protected_areas.gpkg")
protected_areas<-terra::vect("./2022_protected_areas/protected_areas.gpkg",
            layer="protected_areas_2022") # read protected area boundaries)
protected_areas
plot(protected_areas)
sf::st_layers("./2022_rivers/rivers_hydrosheds.gpkg")
rivers<-terra::vect("./2022_rivers/rivers_hydrosheds.gpkg",
                    layer="rivers_hydrosheds")
sf::st_layers("./lakes/lakes.gpkg")
lakes<-terra::vect("./lakes/lakes.gpkg",
                   layer="lakes")  
sf::st_layers("./studyarea/studyarea.gpkg")
studyarea<-terra::vect("./studyarea/studyarea.gpkg",
                              layer="my_study_area")


# load the raster data for the whole ecosystem
woodybiom<-terra::rast("./2016_WoodyVegetation/TBA_gam_utm36S.tif")
hillshade<-terra::rast("./2023_elevation/hillshade_z5.tif")
rainfall<-terra::rast("./rainfall/CHIRPS_MeanAnnualRainfall.tif")
elevation<-terra::rast("./2023_elevation/elevation_90m.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
xlimits<-c(550000,900000)
ylimits<-c(9600000,9950000)

# plot the woody biomass map that you want to predict
woody_map <- ggplot () +
  tidyterra::geom_spatraster(data=woodybiom) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), 
                       limits=c(0.77,6.55),
                       oob=squish,
                       name="TBA/ha") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Woody biomass in the study area",
       subtitle="Tropical Biomass Assessment 2016",
       caption="Source: APCE2024") +
  coord_sf(xlim=xlimits, ylim=ylimits, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
woody_map




# plot the rainfall map
rain_map <- ggplot () +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colours=pal_zissou1, 
                       limits=c(264,2500),
                       oob=squish,
                       name="mm") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Average annual rainfall in the study area",
       caption="Source: APCE2024") +
  coord_sf(xlim=xlimits, ylim=ylimits, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2) 
rain_map

# plot the elevation map
elev_map <- ggplot () +
  tidyterra::geom_spatraster(data=elevation) +
  scale_fill_gradientn(colours=terrain.colors(10), 
                       limits=c(500,2100),
                       oob=squish,
                       name="meters") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Elevation in the study area",
       caption="Source: APCE2024") +
  coord_sf(xlim=xlimits, ylim=ylimits, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
elev_map

woody_map + elev_map + rain_map

#plot woody_map + elev_map + rain_map in two columns
woody_map + elev_map + rain_map + plot_layout(ncol=2)

# combine the different maps  into one composite map using the patchwork library
# and save it to a high resolution png


############################
### explore your study area
# set the limits of your study area
xlimits<-sf::st_bbox(studyarea)[c(1,3)]
ylimits<-sf::st_bbox(studyarea)[c(2,4)]
saExt<-terra::ext(studyarea)

# crop the woody biomass to the extent of the studyarea


# plot the woody biomass


# make maps also for the other layers that you found

# create 500 random points in our study area


# and add them to the previous map

# make distance to river map



### put all maps together



# extract your the values of the different raster layers to the points


# make long format

# plot how woody cover is predicted by different variables


