library(tidyverse) # dev ggplot version required: devtools::install_github("hadley/ggplot2")
library(sf)
library(readxl)
library(httr)
library(ggmap)
library(gganimate) # devtools::install_github("dgrtwo/gganimate")
library(hrbrthemes) # devtools::install_github("hrbrmstr/hrbrthemes")

# download the natural earth shapefile we need into your working directory
URL <- "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_map_units.zip"
temp <- tempfile()
download.file(URL, temp)
unzip(temp)
unlink(temp)

# read in shapefile as an sf object and set the projection
# this will be our base world map for plot sans Antarctica
world <- st_read("ne_110m_admin_0_map_units.shp") %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  filter(!name %in% c("Fr. S. Antarctic Lands", "Antarctica"))

# download dataset into your working directory
url <- "https://www.blog.cultureofinsight.com/data/wc.xlsx"
GET(url, write_disk("wc.xlsx", overwrite=TRUE))

# read in our the massive 20 rows of data and get the winner/runner-up variable in 1 column #tidyafdata
# setting factor for winner to show first in the legend
winners <- read_excel("wc.xlsx") %>% 
  gather(w_l, country, winner:runner_up) %>% 
  mutate(w_l = factor(w_l, levels = c("winner", "runner_up")))

# merge our world shape file with our main dataset
# this will add the polygon for the appropriate country to each row of our winners dataset
# and remove any countries that haven't won or come 2nd in the WC
wc_geo <- merge(world, winners, by.x = "name", by.y = "country")

# get the lon/lat coordinates of the world cup final locations via ggmap::geocode
# add year and final placenames from winners data then transorm into an sf object with same proj as basemap
# finally mutate 2 columns for lon/lat (x + y) to use for plotting text
locations_sf <- geocode(winners[1:20,]$Location) %>% 
  cbind(winners[1:20, 1:2]) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84", agr = "constant") %>% 
  mutate(x = sapply(geometry, "[[", 1), y = sapply(geometry, "[[", 2))

# plot base map + filtered map with fill on winner/runner-up variable and frame as year for animation
# then a point at each final location along with the place name text
# set the projection and all the theme commands to give it a dark and mysterious aesthetic
wc_map <- ggplot() +
  geom_sf(data = world, colour = "#ffffff20", fill = "#2d2d2d60", size = .5) +
  geom_sf(data = wc_geo, aes(fill = w_l, frame = Year)) +
  geom_sf(data = locations_sf, aes(frame = Year), size = .2, colour = "#ffffff90") +
  geom_text(data = locations_sf, aes(x, y, label = Location, frame = Year), 
            colour = "white", fill = "#00000040", nudge_y = -5) +
  coord_sf(crs = st_crs(world), datum = NA) +
  labs(title = "FIFA World Cup Winners, Runners Up & Final Locations", x=NULL, y=NULL,
       caption = "Culture of Insight / @paulcampbell91 / Source: Wikipedia") +
  theme_modern_rc(axis = FALSE, base_size = 16, caption_size = 18) +
  scale_fill_manual(values = c("#D9A441", "#A8A8A8"), name = NULL, labels = c("Winner", "Runner-Up")) +
  theme(legend.position = c(0.9, 1.01), legend.direction = "horizontal", axis.text = element_blank(), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank())

# set animation interval as 2 seconds, create the gif, and Robert's your Father's brother
animation::ani.options(interval = 2)
gganimate(wc_map, ani.width =  1250, ani.height = 585, "wc.gif", title_frame = TRUE)