# load library
library(ggplot2)
library(raster)
library(tidyverse)
library(ggrepel)
library(rgdal)
library(maps)
library(mapproj)

#create East asia plot as background
# Load the shapefile
shapefile <- readOGR(dsn = "ne_10m_admin_0_sovereignty", layer = "ne_10m_admin_0_sovereignty")

# Create a subset of the shapefile for East Asia, South Asia, and Southeast Asia
east_asia <- subset(shapefile, ADMIN %in% c("China", "Japan", "North Korea", "South Korea", "Taiwan"))
southeast_asia <- subset(shapefile, ADMIN %in% c("Brunei", "Cambodia", "Indonesia", "Laos", "Malaysia", "Myanmar", "Philippines", "Singapore", "Thailand", "Timor-Leste", "Vietnam"))

# Combine the subsets into one data frame
asia <- rbind(east_asia, southeast_asia)

# Plot the Asia subset of the shapefile
ggplot() +
  geom_polygon(data = asia, aes(x = long, y = lat, group = group), fill = "white", color = "grey20") +
  theme_void()

vietnam <- getData("GADM", country="Vietnam", level=1)
vn <- fortify(vietnam, regions="NAME_2")

china <- getData("GADM", country="China", level=1)
chn <- fortify(china, regions="NAME_2")

#sub-region

# Scrap list of provinces by region and sub-region level: 

library(rvest)

# Collect region/sub-region data from Wiki: 

provinces <- "https://en.wikipedia.org/wiki/Provinces_of_Vietnam"

provinces %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>% 
  html_table() %>% 
  .[[1]] -> provinces_vn

# Function extracts data in table: 

extract_table <- function(i) {
  
  provinces_vn %>% 
    slice(i) %>% 
    pull(3) %>% 
    str_split("\n", simplify = TRUE) %>% 
    str_replace_all("†", "") %>% 
    str_squish() %>% 
    as.vector() -> province_names
  
  provinces_vn %>% 
    slice(i) %>% 
    pull(1) %>% 
    str_split("\\(", simplify = TRUE) %>% 
    str_split("\\,", simplify = TRUE) %>% 
    str_replace_all("Vietnam", "") %>% 
    as.vector() %>% 
    str_squish() -> region
  
  provinces_vn %>% 
    slice(i) %>% 
    pull(2) %>% 
    str_split("\\(", simplify = TRUE) %>% 
    str_replace_all("\\)", "") %>% 
    str_replace_all("Vietnam", "") %>% 
    as.vector() %>% 
    str_squish() -> sub_region 
  
  tibble(province = province_names, region_vn = region[2], region_en = region[1], 
         sub_region_vn = sub_region[2], sub_region_en = sub_region[1]) -> df_final
  
  return(df_final)
  
}

# Use the function: 

lapply(1:nrow(provinces_vn), extract_table) -> province_region

do.call("bind_rows", province_region) -> province_region_vietnam

# Rename for some provinnces: 

library(stringi)

province_region_vietnam %>% 
  mutate(province_latin = stri_trans_general(province, "Latin-ASCII")) %>% 
  mutate(province_latin = case_when(province_latin == "Thua Thien-Hue" ~ "Thua Thien - Hue", 
                                    province_latin == "Ba Ria-Vung Tau" ~ "Ba Ria - Vung Tau", 
                                    province_latin == "Ho Chi Minh City" ~ "TP. Ho Chi Minh", 
                                    TRUE ~ province_latin)) -> province_region_vietnam

#subset vietnam map:
northern_province <- subset(province_region_vietnam, region_en == "Northern")$province
north_map <- vietnam[vietnam$NAME_1 %in% northern_province, ]

chosen_province <- subset(province_region_vietnam, province_latin %in% c("Tuyen Quang", "Bac Kan", "Vinh Phuc"))$province
chosen_map <- vietnam[vietnam$NAME_1 %in% chosen_province, ]


#chosen locations:
chosen.df <- read.csv(file = "chosen.csv")

(big_map <- ggplot() +
    geom_polygon(data = asia, aes(x = long, y = lat, group = group), linewidth = 0.1, col = "white",  fill = "#e5e5e5") +
    geom_polygon(data= vietnam, aes(x=long, y=lat, group=group), linewidth = 0,   fill = "grey60") +
    geom_polygon(data = china, aes(x=long, y=lat, group=group), linewidth = 0.1,   col = "#e5e5e5", fill = "grey99") +
    geom_polygon(data = chosen_map, aes(x=long, y=lat, group=group), linewidth = 0.1, col = "#e5e5e5", fill = "#457b9d") +
    theme_void() +
    coord_sf()
)

(sub_map <- ggplot() +
    geom_polygon(data = chosen_map, aes(x=long, y=lat, group=group), fill = "#457b9d", col = "#e5e5e5", show.legend=F) +
    geom_point(data = chosen.df, aes(x=long, y=lat), col = "#e63946") +
    geom_text_repel(data = chosen.df, aes(x=long, y=lat, label = location), box.padding = 1, nudge_x = 0.5) +
    theme_void() +
    theme(panel.border = element_rect(linetype = "solid", fill = NA),
          panel.background = element_rect(fill = "#e5e5e5")) + 
    coord_sf()
)

ggplot2::ggsave(filename = "bigmap.pdf", 
                plot = big_map,
                device = "pdf", 
                path = "images", 
                width = 10, 
                height = 9, 
                units = "in", 
                dpi = 320, 
                limitsize = TRUE,
                bg = "white")

ggplot2::ggsave(filename = "submap.pdf", 
                plot = sub_map,
                device = "pdf", 
                path = "images", 
                width = 7, 
                height = 4, 
                units = "in", 
                dpi = 320, 
                limitsize = TRUE,
                bg = "white")
