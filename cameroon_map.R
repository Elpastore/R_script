library(ggplot2)
library(sf)
library(dplyr)
library(ggrepel)
library(ggspatial)
library(prettymapr)
library(patchwork)

#load spacial data
cameroon <- st_read("shape_files/cmr_admbnda_adm3_inc_20180104.shp")

# load sample data

data <- read.table("coordinates_csv.csv", h=T, sep=";", dec=".")
summary(data)
#convert data
data$Zone <- as.factor(data$Zone)
data$region <- as.factor(data$region)
data$longitude <- as.numeric(data$longitude)
data$latitude <- as.numeric(data$latitude)

# convert data into sf object

sf_object <- st_as_sf(data, coords=c("longitude", "latitude"), crs=st_crs(cameroon))


ggplot()
# plotting the map
map <- ggplot() +
  #annotation_map_tile(type="osm", zoom=8) +
  geom_sf(data=cameroon, fill="lightblue", color="black") +
  geom_sf(data=sf_object, color="red") +
  theme_void() +
  labs(title="sample map", x="longitude", y="latitude")

map



map_1 <- ggplot() +
  geom_sf(data=cameroon, fill="lightblue", color="black") +
  #geom_sf(data=sf_object, aes(size=1), color="red") +
  theme_void() +
  labs(title="sample map", x="longitude", y="latitude")

map_1

# let's zooming Buea

buea_zone <- cameroon[cameroon$ADM3_FR=="Buea",]
bbox <- st_bbox(buea_zone)
xmin <- bbox["xmin"]
xmax <- bbox["xmax"]
ymin <- bbox["ymin"]
ymax <- bbox["ymax"]

# let's zooming Edea
edea_zone <- cameroon[cameroon$ADM3_FR=="Edea 1er",]
bbox_2 <- st_bbox(edea_zone)
xmin_2 <- bbox_2["xmin"]
xmax_2 <- bbox_2["xmax"]
ymin_2 <- bbox_2["ymin"]
ymax_2 <- bbox_2["ymax"]

map_1 <- ggplot() +

  geom_sf(data=cameroon, fill="lightblue", color="black", linewidth = 0.5) +
  #geom_sf(data=sf_object, aes(size=1), color="red") +
  theme_void() +
  annotate("rect", xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, color="red", fill=NA, size=0.5) +
  annotate("rect", xmin=xmin_2, xmax=xmax_2, ymin=ymin_2, ymax=ymax_2, color="red", fill=NA, size=0.5) +
  labs(title="Cameroon map", x="longitude", y="latitude")

map_1

# draww the zooming zone map
buea_map <- ggplot() +
  geom_sf(data = cameroon, fill = "lightblue", color = "#023020", linewidth = 0.5) +
  
  # Plot points with a fixed size and color
  geom_sf(data = sf_object, aes(geometry = geometry), color = 'red', size = 3) +
  
  # adding reper
  geom_label_repel( data = data,
                    aes(label =  paste(region, sep = "") ,x = longitude, y = latitude, fontface = "bold"  ), 
                    color = 'black',
                    size = 2,
                    box.padding = unit(1.1, "lines"),
                    segment.color = '#132B43',
                    angle = 90,
                    max.overlaps = 20
  ) +
  
  # Zoom in on the Upper River Zone
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  
  # Title and theme adjustments
  ggtitle("Buea zone") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.6, vjust = 4, size = 13)) +
  
  # Remove legend
  theme(legend.position = "none")

buea_map

# draww the zooming zone map
edea_map <- ggplot() +
  geom_sf(data = cameroon, fill = "lightblue", color = "#023020", linewidth = 0.5) +
  
  # Plot points with a fixed size and color
  geom_sf(data = sf_object, aes(geometry = geometry), color = 'red', size = 3) +
  
  # adding reper
  geom_label_repel( data = data,
                    aes(label =  paste(region, sep = "") ,x = longitude, y = latitude, fontface = "bold"  ), 
                    color = 'black',
                    size = 2,
                    box.padding = unit(1.1, "lines"),
                    segment.color = '#132B43',
                    angle = 90,
                    max.overlaps = 20
  ) +
  # Zoom in on the Upper River Zone
  coord_sf(xlim = c(xmin_2, xmax_2), ylim = c(ymin_2, ymax_2), expand = FALSE) +
  
  # Title and theme adjustments
  ggtitle("Edea zone") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.6, vjust = 4, size = 13)) +
  
  # Remove legend
  theme(legend.position = "none")

edea_map

final_plot <- map_1 + buea_map + edea_map + plot_layout(ncol=3)
final_plot

final_plot_2 <- map_1+ buea_map / edea_map
final_plot_2
