library(tidyverse)
library(readxl)
library(ggmap)
library(mapview)
library(sp)

school_grade <- read_excel("school-grade.xlsx", skip=6) %>%
  arrange(SCHOOL_NAME)
public_school <- read.csv("school_3.csv")

school <- public_school %>%
  left_join(school_grade, by=c("SCH_NAME"="SCHOOL_NAME"))

## static map, probably abandon this since we have a more fancy map
lat <- c(-71.17434, -71.00412)
long <- c(42.23390, 42.39162)
bbox <- make_bbox(long, lat, f=0.05)
ggmap(get_map(location=c(lon = -71.1, lat = 42.32), zoom=12, 
              maptype="toner-lite", source="stamen")) +
  geom_jitter(data=public_school, aes(X, Y, color=SCH_TYPE), size=5) +
  labs(x = " ", y = " ",
       title="School Locations", color = "School Type")

pop <- public_school %>%
  mutate('School Size' = School_Total,
         'School Name' = school_name_2,
         Address = ADDRESS) %>%
  select('School Name', Address, 'School Size')
  


coordinates(public_school) <- ~ X + Y
proj4string(public_school) <- "+init=epsg:4326"
mapview(public_school, zcol="SCH_TYPE", legend=TRUE, cex=8,
        popup=popupTable(pop)) 
