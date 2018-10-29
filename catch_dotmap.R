library(tidyverse)
library(sf)
library(rmapshaper)
library(geojsonio)
library(marmap)
library(raster)


# Import landings data and summarise
# Data available from Marine Management Organisation https://www.gov.uk/government/statistics/uk-sea-fisheries-annual-statistics-report-2017
landings <- read_csv("Landings_data_by_Exclusive_Economic_Zone_for_all_UK_registered_vessels_2017.csv")

landings_summary <- landings %>%
  group_by(Rectangle, `Vessel Nationality`) %>%
  summarise(total_weight = sum(`Live Weight (tonnes)`))

# Load ICES rectangles shapefile and crop around coastline
# ICES rectangles available from, http://ices.dk/marine-data/maps/Pages/default.aspx
# Land shapefile from https://www.naturalearthdata.com/downloads/10m-physical-vectors/
rectangles <- read_sf("ices_rectangles/ICES_Statistical_Rectangles_Eco.shp")
rectangles <- rectangles %>%
  filter((`NORTH` >= 45) & (`NORTH` <= 65)) %>%
  filter((`WEST` >= -16) & (`WEST` <= 8))

rectangles_json <- geojson_json(rectangles)
land <- read_sf("ne_10m_land/ne_10m_land.shp")
land_json <- geojson_json(land)
rectangles_clipped <- ms_erase(rectangles_json,land_json)
rectangles_clipped <- geojson_sf(rectangles_clipped)

# Join landings data to rectangles
landings_rectangles <- inner_join(rectangles_clipped, landings_summary, by = c("ICESNAME" = "Rectangle"))

# Split landings data by vessel nationality and generate points for each rectangle (1 point = 25 tonnes)
uk_nations <- c("UK - England", "UK - Northern Ireland", "UK - Scotland", "UK - Wales")

landings_split <- landings_rectangles %>%
  filter(`Vessel Nationality` %in% uk_nations) %>%
  split(.$`Vessel Nationality`)

generate_samples <- function(data)
  suppressMessages(st_sample(data, size = round(data$total_weight / 10)))

points <- map(landings_split, generate_samples)
points <- imap(points,
               ~st_sf(data_frame(`Vessel Nationality` = rep(.y, length(.x))),
                      geometry = .x))
points <- do.call(rbind, points)
points <- points[sample(1:nrow(points), nrow(points)), ]


#Download bathymetry for background using marmap package
seabed <- getNOAA.bathy(lon1 = -16, lon2 = 8, lat1 = 45, lat2 = 66, resolution = 1)
seabed <- fortify.bathy(seabed)


# Build plot
catch_dotmap <- ggplot() +
  geom_raster(data = seabed, aes(x = x, y = y, fill = z)) +
  scale_fill_gradient(low = "#00060C", high = "#002447", guide = FALSE) +
  geom_sf(data = land, fill = "black", colour = NA) + 
  geom_sf(data = points, aes(colour = Vessel.Nationality), size = .001) +
  scale_colour_manual(values = c("#D55E00","#F0E442", "#0072B2", "#A9A9A9"), labels = c("England", "Northern Ireland", "Scotland", "Wales")) +
  annotate("text", x = -15, y = 64.5, label = "UK fishing vessel catch, 2017", colour = "white", size = 15, hjust = 0) +
  annotate("text", x = 7, y = 47, label = "Each dot represents 10 tonnes of fish caught in each ICES rectangles\n\nDr Christopher Huggins, @chris_huggins\nFishing data: Marine Management Organisation\nICES rectangles: International Council for the Exploration of the Sea\nBathymetry: NOAA\nCoastline: Natural Earth", colour = "white", size = 4, hjust = 1) +
  coord_sf(xlim = c(-15, 7), ylim = c(64.5, 47)) +
  theme_void(base_size = 20) +
  guides(col = guide_legend(override.aes = list(shape = 20, size = 5, fill = c("#D55E00","#F0E442", "#0072B2", "#A9A9A9")))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 5), legend.title = element_blank(), legend.text = element_text(colour="white"), legend.justification = c(0.95,0.1), legend.position = c(0.95,0.1), panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"))
ggsave(catch_dotmap, file = "catch_dotmap2.pdf", height = 59.4, width = 42, units = "cm")

ggsave(catch_dotmap, file = "catch_dotmap2.png", height = 59.4, width = 42, units = "cm", dpi = 400)
