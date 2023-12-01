
## This script creates a map for the TOM FORD Plastic Innovation Prize (2022)


## set up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#libraries for mapping
library(marmap)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(ggspatial)
library(ggsn)
library(cowplot)
library(ggmap)

###############################
#Figure 1
#Site map
###############################

#small insert of whole state

#get world data
# world <- ne_countries(scale='medium',returnclass = 'sf')
#subset US data
# usa <- subset(world, admin == "United States of America")
wa <- read_sf(here("data", "wa_state", "WA_State_Boundary.shp"))
# coast <- read_sf(here("data", "wa_shoreline", "WSDOT_-_Major_Shorelines.shp"))
coast <- read_sf(here("data", "wa_coastline", "WAECY_-_Shoreline_Management_Act_Jurisdiction.shp"))

#add a small bounding box
#use bboxfinder.com
cust_box <- st_as_sfc(st_bbox(c(xmin = -123.216065, xmax = -122.040528,
                                ymin = 47.000353, ymax = 48.097763),
                              crs = st_crs(4326)))


#small map insert of Washington state
small.map <- ggplot() +
  geom_sf(data = wa, color = "grey50", fill = "grey50", size = 0.15) +
  geom_sf(data = cust_box, color = "red", fill = "transparent", size = 0.5) +
  geom_sf(data = coast) +
  # coord_sf(crs = st_crs(4135), xlim = c(-161, -154), ylim = c(18,
  #                                                             23),
  #          expand = FALSE, datum = NA) +
  # geom_sf(data = wa) +
  theme_void() +
  theme(plot.background = element_rect(color = "black", fill = "black", size = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "white"))


#create big map of puget sound
big.map <- ggplot(data = coast) +
  geom_sf(color = "grey30", fill = "grey30", size = 0.15) +
  coord_sf(crs = st_crs(4135), xlim = c(-123.5, -122), ylim = c(47, 48.3)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "lightcyan2"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank()) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotation_scale(location = "br")


#enter seattle aquarium location data
#this was too far left
# long <- -122.2330
long <- -122.32
lat <- 47.6074
site <- as.data.frame(cbind(long,lat))

#choose a color for points
color <- "#D55E00"

# put map and points together
final.sa.large.map <- big.map +
  geom_point(data = site,
             aes(x= long, y = lat), 
             colour = color, 
             size = 3,
             stroke = 1.5) +
  annotation_north_arrow(location = "tl", which_north = "true",
                         pad_x = unit(0.05, "in"), pad_y = unit(0.05, "in"),
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  # geom_text_repel(data = site.coords.df, aes(x = Lon, y = Lat, label = Name),
  #                 segment.color = "black",
  #                 nudge_x = -0.22,
  #                 nudge_y = -0.03,
  #                 direction = "y",
  #                 size = 3, 
  #                 color = "black") +
  theme(legend.key = element_blank())


print(final.sa.large.map)

#put the big map and small map together
final.sa.map <- ggdraw() + draw_plot(final.sa.large.map) +
  draw_plot(small.map, x = 0.11, y = 0.17, width = .2, height = .2)

print(final.sa.map)

#turn off graphics panel in R studio
graphics.off()

#open new window, change size if needed
windows(8,6, record=T)

print(final.sa.map)

ggsave(here("output", "Map Figure.png"))

#ggsave didn't keep the symbol for Puako, so exported using cairo PDF
# ggsave(filename = "Figure 1", plot = final.hawaii.map, device = "eps", path = "Final Figures", dpi = 300, encoding = "MacRoman")
# ggsave(filename = "Fig1.png", plot = final.hawaii.map, device = "png", dpi = 300)
# ggsave(filename = "Fig1.jpg", plot = final.hawaii.map, device = "jpg", dpi = 300)


library(maps)
library(mapdata)

state <- map_data("state")
washington <- subset(state, region == "washington")

#better small map of washington
map <- ggplot(data = washington, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray") +
  theme_void() +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        plot.background = element_rect(color = "black", fill = "black", size = 1),
        panel.background = element_rect(fill = "white"))

#create bounding box
long <- c(-123, -122)
lat <- c(47, 48)
box <- as.data.frame(cbind(long,lat))

#trying to figure out how to add a bounding box to the small map

print(map)

ggsave(here("output", "Small map.png"))


final.sa.map <- ggdraw() + draw_plot(final.sa.large.map) +
  draw_plot(map, x = 0.11, y = 0.17, width = .2, height = .2)




