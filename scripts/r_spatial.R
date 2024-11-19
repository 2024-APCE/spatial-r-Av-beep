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
5barplot(rep(1,10), col = RColorBrewer::brewer.pal(10, "Spectral"))
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
distriver<-terra::rast("./2022_rivers/DistanceToRiver.tif")
treecover<-terra::rast("./2019_copernicus_treecover/copernicus_tree_cover_new.tif")
soil<-terra::rast("./soil/CEC_5_15cm.tif")
burning<-terra::rast("./burning/BurnFreq.tif")
lastburn <- terra::rast("./burning/YearLastBurned.tif")
landform <- terra::rast("./landforms/landforms.tif")

# inspect the data 
class(protected_areas)
class(elevation)
plot(protected_areas)
plot(elevation)
plot(protected_areas, add=T)

# set the limits of the map to show (xmin, xmax, ymin, ymax in utm36 coordinates)
#limits for my studty area
xlimits<-c(740000,790000)
ylimits<-c(9740000,9770000)

#limits for the whole area
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


#plot hillshade - help
hillshade_map <- ggplot () +
  tidyterra::geom_spatraster(data=hillshade) +
  scale_fill_gradientn(colours=rev(terrain.colors(6)), 
                       limits=c(150,250),
                       oob=squish,
                       name="hill") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Hillshade") +
  coord_sf(xlim=xlimits, ylim=ylimits, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
hillshade_map

# plot the rainfall map
rain_map <- ggplot () +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colours=pal_zissou1, 
                       limits=c(300,1500),
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
                       limits=c(1500,2100),
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
  labs(title="Elevation") +
  coord_sf(xlim=xlimits, ylim=ylimits, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
elev_map

ggsave("elev_map.png", elev_map, width = 10, height = 10, dpi = 300)

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
saExt

# crop the woody biomass to the extent of the studyarea
woodybiom_star<-terra::crop(woodybiom, saExt)

#crop rainfall to the extent of the studyarea
rainfall_star <- terra::crop(rainfall, saExt)

#crop elevation to the extent of the studyarea
elevation_star <- terra::crop(elevation, saExt)

#crop distance to river
dist2river_sa <- terra::crop(distriver, saExt)

#crop burn frequency
burnfreq_sa <- terra::crop(burning, saExt)

#crop soil fertility
cec_sa <- terra::crop(soil, saExt)

#crop last year burned
lastburn_sa <- terra::crop(lastburn, saExt)

#crop treecover
treecover_sa <- terra::crop(treecover, saExt)

#crop landform
landform_sa <- terra::crop(landform, saExt)


## plot the woody biomass
woody_map_star <- ggplot () +
  tidyterra::geom_spatraster(data=woodybiom_star) +
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
  labs(title="Woody biomass")+
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
woody_map_star

# make maps also for the other layers that you found
rain_map_star <- ggplot () +
  tidyterra::geom_spatraster(data=rainfall) +
  scale_fill_gradientn(colours=pal_zissou1, 
                       limits=c(600,1600),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Average annual rainfall")+
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2) 
rain_map_star

### new rainfall
rainfall_30m <- rast(terra::ext(rainfall), resolution = 30, crs = crs(rainfall))
# Resample the raster to 30m resolution
rainfall_30m <- terra::resample(rainfall, rainfall_30m, method = "bilinear")  
rainfall_sa<-terra::crop(rainfall_30m,saExt) # crop to study area
rainfall_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=rainfall_sa) +
  scale_fill_gradientn(colours=pal_zissou1,
                       limits=c(600,1300),
                       oob=squish,
                       name="mm/yr") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Rainfall") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rainfall_map_sa

#elevation map
elev_map_star <- ggplot () +
  tidyterra::geom_spatraster(data=elevation_star) +
  scale_fill_gradientn(colours=terrain.colors(10), 
                       limits=c(1500,2100),
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
  labs(title="Elevation")+
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
elev_map_star

woody_map_star + elev_map_star + rain_map_star + plot_layout(ncol=2)


#distance to river
distriver_map <- ggplot () +
  tidyterra::geom_spatraster(data=distriver) +
  scale_fill_gradientn(colours=topo.colors(6), 
                       limits=c(0,8000),
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
  labs(title="Distance to river")+
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
distriver_map

#treecover
tree_map <-ggplot () +
  tidyterra::geom_spatraster(data=treecover) +
  scale_fill_gradientn(colours=(RColorBrewer::brewer.pal(n=9, name="Greens")), 
                       limits=c(0,60),
                       oob=squish,
                       name=("percentage")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Treecover") +
coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
tree_map

#soil map
soil_map <- ggplot ()+
  tidyterra::geom_spatraster(data=soil) +
  scale_fill_gradientn(colours=(RColorBrewer::brewer.pal(n=9, name="RdPu")), 
                       limits=c(134,274),
                       oob=squish,
                       name="mmol/km") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Soil fertility") +
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
soil_map

#burnmap
burning_map <- ggplot ()+
  tidyterra::geom_spatraster(data=burning) +
  scale_fill_gradientn(colours=(RColorBrewer::brewer.pal(n=9, name="YlOrRd")), 
                       limits=c(0,14),
                       oob=squish,
                       name="years burned") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Burn frequency") +
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
burning_map

#last year burned map
lastburn_map <- ggplot ()+
  tidyterra::geom_spatraster(data=lastburn) +
  scale_fill_gradientn(colours=rev(RColorBrewer::brewer.pal(n=9, name="Greys")), 
                       limits=c(0,1),
                       oob=squish,
                       name="value") +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             colour="deepskyblue2", linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, colour="red", linewidth=1)+
  tidyterra::geom_spatvector(data=lakes,
                             fill="royalblue3", linewidth=0.5)+
  labs(title="Last burned") +
  coord_sf(xlim=xlimits, ylim=ylimits, expand=F, datum= sf::st_crs(32736))+
  theme(axis.text=element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="bl", width_hint=0.2)
lastburn_map

landform <- terra::as.factor(landform)
landform_map <- ggplot() + 
  tidyterra::geom_spatraster(data=landform) +
  scale_fill_manual(
    values = c(
      "11" = "#141414", "12" = "#383838", "13" = "#808080", 
      "14" = "#ebeb8f", "15" = "#f7d311", "21" = "#aa0000", 
      "22" = "#d89382", "23" = "#ddc9c9", "24" = "#dccdce", 
      "31" = "#1c6330", "32" = "#68aa63", "33" = "#b5c98e", 
      "34" = "#e1f0e5", "41" = "#a975ba", "42" = "#6f198c"
    ),
    breaks = c("11", "12", "13", "14", "15", "21", "22", "23", "24", "31", "32", "33", "34", "41", "42"),  # Define the range explicitly
    na.value = "grey",             # Set a color for NA values
    labels = c(
      "Peak/ridge (warm)", "Peak/ridge", "Peak/ridge (cool)", "Mountain/divide", "Cliff", "Upper slope (warm)", "Upper slope", "Upper slope (cool)", "Upper slope (flat)", "Lower slope (warm)", "Lower slope", "Lower slope (cool)", "Lower slope (flat)", "Valley", "Valley (narrow)"),
    name = "landform types"
  ) +
  tidyterra::geom_spatvector(data=protected_areas,color="#4D4D4D",
                             fill=NA, linewidth=0.5) +
  tidyterra::geom_spatvector(data=lakes,
                             fill="#458EC8") +
  tidyterra::geom_spatvector(data=rivers,
                             color="#3773A4") +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA, color="#F11B00", linewidth=0.7) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl", width_hint = 0.5) 
landform_map

# core_protected_areas  map 
r<-terra::rast("./2022_protected_areas/CoreProtectedAreas.tif") 
CoreProtectedAreas_sa <- r |> #  replace NA by 0
  is.na() |>
  terra::ifel(0,r) 

CoreProtectedAreas_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(CoreProtectedAreas_sa)) +
  scale_fill_manual(values=c("grey","lightgreen"),
                    labels=c("no","yes")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Core protected areas") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
CoreProtectedAreas_map_sa

#hills map
landform_sa<-terra::rast("./landforms/hills.tif")
hill_map_sa<-ggplot() +
  tidyterra::geom_spatraster(data=as.factor(landform_sa)) +
  scale_fill_manual(values=c("black","orange"),
                    labels=c("valleys\nand\nplains","hills")) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.7) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="green") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="Landform") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
hill_map_sa


#maps together
woody_map_star + elev_map_star + rainfall_map_sa+ burning_map + distriver_map + CoreProtectedAreas_map_sa + soil_map + lastburn_map + landform_map + tree_map + rpoints_map_sa + hill_map_sa + plot_layout(ncol=3)

ggsave("studyarea.png", width=12, height=8, dpi=300)

###   from here the points

# create 250 random points in your study area
set.seed(124)
rpoints <- terra::spatSample(studyarea, size = 275, 
                             method = "random")
# plot the points
rpoints_map_sa<-ggplot() +
  tidyterra::geom_spatvector(data=rpoints, size=0.5) +
  tidyterra::geom_spatvector(data=protected_areas,
                             fill=NA,linewidth=0.5) +
  tidyterra::geom_spatvector(data=studyarea,
                             fill=NA,linewidth=0.5,col="red") +
  tidyterra::geom_spatvector(data=lakes,
                             fill="lightblue",linewidth=0.5) +
  tidyterra::geom_spatvector(data=rivers,
                             col="blue",linewidth=0.5) +
  labs(title="250 random points") +
  coord_sf(xlimits,ylimits,expand=F,
           datum = sf::st_crs(32736)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  ggspatial::annotation_scale(location="bl",width_hint=0.2)
rpoints_map_sa

#all maps together
woody_map_star + elev_map_star + rainfall_map_sa+ CoreProtectedAreas_map_sa + distriver_map + burning_map + soil_map + tree_map + lastburn_map + landform_map + rpoints_map_sa +plot_layout(ncol=3)

#ggsave("./figures/studyarea.png", width=12, height=8, dpi=300)

# extract your the values of the different raster layers to the points
# Extract raster values at the points
woody_points <- terra::extract(woodybiom_star, rpoints) |> 
  as_tibble() |>
  dplyr::rename(woody=TBA_gam_utm36s)
woody_points
dist2river_points <- terra::extract(dist2river_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(dist2river=distance)
dist2river_points
elevation_points <- terra::extract(elevation_star, rpoints) |> 
  as_tibble() 
elevation_points
CorProtAr_points <- terra::extract(CoreProtectedAreas_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(CorProtAr=CoreProtectedAreas)
CorProtAr_points
rainfall_points <- terra::extract(rainfall_star, rpoints) |> 
  as_tibble() |> 
  dplyr::rename(rainfall=CHIRPS_MeanAnnualRainfall)
rainfall_points
cec_points <- terra::extract(cec_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(cec='cec_5-15cm_mean')
cec_points
burnfreq_points <- terra::extract(burnfreq_sa, rpoints) |> 
  as_tibble() |>
  dplyr::rename(burnfreq=burned_sum)
burnfreq_points
landform_points <- terra::extract(landform_sa, rpoints) |> 
  as_tibble()|>
  dplyr::rename(hills=remapped)
landform_points

pointdata<-cbind(dist2river_points[,2],elevation_points[,2],
                 CorProtAr_points[,2],rainfall_points[,2], 
                 cec_points[,2],burnfreq_points[,2],
                 landform_points[,2],woody_points[,2]) |>
  as_tibble()
pointdata <- data.frame(pointdata)
pointdata

pointdata2<- pointdata[complete.cases(pointdata),]
pointdata2

getwd()
readr::write_csv(pointdata2, "pointdata.csv")

#next stap
# plot how woody cover is predicted by different variables
# Create a correlation panel plot
library(psych)
psych::pairs.panels(
  pointdata ,
  method = "pearson",     # Correlation method (use "spearman" for rank correlation)
  hist.col = "lightblue",  # Color for histograms
  density = TRUE,          # Add density plots
  ellipses = F,         # Add correlation ellipses
  lm = TRUE,                # Add linear regression lines
  stars=T
)

# make long format
names(pointdata2)
pointdata_long<-pivot_longer(data=pointdata2,
                             cols = dist2river:hills, # all except woody
                             names_to ="pred_var",
                             values_to = "pred_val")
pointdata_long

# panel plot
ggplot(data=pointdata_long, mapping=aes(x=pred_val,y=woody,group=pred_var)) +
  geom_point() +
  geom_smooth() +
  ylim(0,40) +
  facet_wrap(~pred_var,scales="free") 

# do a pca
# Load the vegan package
library(vegan)
# Perform PCA using the rda() function
pca_result <- vegan::rda(pointdata2,
                         scale = TRUE)
# Display a summary of the PCA
summary(pca_result)

# Plot the PCA
plot(pca_result, scaling = 2, type="n", xlab="",ylab="")  # Use scaling = 1 for distance preservation, scaling = 2 for correlations
# Add points for samples
points(pca_result, display = "sites", pch=pointdata$CorProtAr+1, col = pointdata$hills+1, bg = "blue", cex = 1)
# Add arrows for variables
arrows(0, 0, scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
       length = 0.1, col = "red")
# Label the variables with arrows
text(scores(pca_result, display = "species")[, 1], scores(pca_result, display = "species")[, 2], 
     labels = colnames(pointdata), col = "red", cex = 0.8, pos = 4)
# Add axis labels and a title
title(main = "PCA Biplot")
xlabel <- paste("PC1 (", round(pca_result$CA$eig[1] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
ylabel <- paste("PC2 (", round(pca_result$CA$eig[2] / sum(pca_result$CA$eig) * 100, 1), "%)", sep = "")
title(xlab=xlabel)
title(ylab=ylabel)
# add contours for woody cover
vegan::ordisurf(pca_result, pointdata2$woody, add = TRUE, col = "green4")




