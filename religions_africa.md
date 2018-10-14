``` r
library(sf)
library(ggplot2)
library(tidyverse)
library(xlsx)
library(viridis)
library(scales)
library(ggrepel)
```

I start be loading and storing the Geographical Data Files (GDF) for the whole world.

``` r
gdf <- st_read("C:/Users/Yabra/Desktop/GDFs/gadm36_levels_shp/gadm36_0.shp")
```

    ## Reading layer `gadm36_0' from data source `C:\Users\Yabra\Desktop\GDFs\gadm36_levels_shp\gadm36_0.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 256 features and 2 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: -180 ymin: -90 xmax: 180 ymax: 83.65833
    ## epsg (SRID):    4326
    ## proj4string:    +proj=longlat +datum=WGS84 +no_defs

``` r
glimpse(gdf)
```

    ## Observations: 256
    ## Variables: 3
    ## $ GID_0    <fct> ABW, AFG, AGO, AIA, ALA, ALB, AND, ARE, ARG, ARM, ASM...
    ## $ NAME_0   <fct> Aruba, Afghanistan, Angola, Anguilla, Åland, Albania,...
    ## $ geometry <MULTIPOLYGON [Â°]> MULTIPOLYGON (((-69.9782 12..., MULTIPO...

Lets plot the world

``` r
plot(gdf["NAME_0"])
```

![](religions_africa_files/figure-markdown_github/Whole%20world-1.png)

Now I need to load interesting data to match with the map of the world. I am going to load a simple data set with information on religious practices in each African country.

``` r
rel_africa <- read.xlsx("C:/Users/Yabra/Desktop/Data Science/Github/Introduction-to-Maps/religions.xlsx", sheetIndex = 1,
                        encoding = "UTF-8") #Encoding option used to handle special characters

glimpse(rel_africa)
```

    ## Observations: 56
    ## Variables: 5
    ## $ NAME_0                <fct> Angola, Cameroon, Central African Republ...
    ## $ Region                <fct> Central Africa, Central Africa, Central ...
    ## $ Christianity          <dbl> 95.0, 69.2, 80.3, 44.1, 95.8, 85.9, 93.0...
    ## $ Islam                 <dbl> 0.5, 20.9, 10.1, 52.1, 1.5, 1.2, 1.0, 10...
    ## $ Traditional.religions <dbl> 4.5, 9.9, 9.6, 3.8, 2.7, 12.9, 6.0, 17.0...

Time to join geographical data with the information on religious practices that I just loaded.

``` r
#Make sure names are stored as characters and not factors
gdf$NAME_0 <- as.character(gdf$NAME_0)
rel_africa$NAME_0 <- as.character(rel_africa$NAME_0)

#Join both datasets
rel_africa_map <- gdf %>% left_join(rel_africa, by = "NAME_0")

#Get rid of data outside of Africa 
rel_africa_map <- rel_africa_map %>% filter(!is.na(Islam))

#Check if all data matched correctly
dim(rel_africa) # 56 rows
```

    ## [1] 56  5

``` r
dim(rel_africa_map) # 53 rows
```

    ## [1] 53  7

``` r
#Some countries in the religion database were not matched with their geo data
missing <- anti_join(rel_africa, rel_africa_map)
```

    ## Joining, by = c("NAME_0", "Region", "Christianity", "Islam", "Traditional.religions")

``` r
missing
```

    ##                  NAME_0         Region Christianity Islam
    ## 1 Republic of the Congo Central Africa         85.9   1.2
    ## 2               Réunion    East Africa         84.9   2.1
    ## 3            The Gambia    West Africa          9.0  90.0
    ##   Traditional.religions
    ## 1                  12.9
    ## 2                  13.0
    ## 3                   1.0

Lets see what are the names of those countries that didnt match

``` r
gdf$NAME_0
```

Rename countries with different names

``` r
rel_africa$NAME_0[rel_africa$NAME_0 == "Republic of the Congo"] <- "Republic of Congo"
rel_africa$NAME_0[rel_africa$NAME_0 == "Réunion"] <- "Reunion"
rel_africa$NAME_0[rel_africa$NAME_0 == "The Gambia"] <- "Gambia"

#Join the data again
rel_africa_map <- gdf %>% left_join(rel_africa, by = "NAME_0")
rel_africa_map <- rel_africa_map %>% filter(!is.na(Islam))
```

``` r
plot(rel_africa_map["Islam"])
```

![](religions_africa_files/figure-markdown_github/Plot%20religion-1.png)

Making beautiful maps
---------------------

I will start be defining a basic Instiglio template for maps

``` r
theme_instiglio_maps <- theme_void() + 
  theme(panel.grid = element_line(color = "transparent"), 
        plot.title = element_text(color = "#404040",face = "bold", hjust=0), 
        plot.subtitle = element_text(color = "#404040", face = "italic", hjust=0),
        plot.caption = element_text(color = "#404040", size = 8))
```

Now I will create a new plot

``` r
religions_africa <- ggplot() +
  geom_sf(data = rel_africa_map, aes(color = Islam ,fill = Islam)) +
  theme_void() +
  labs(
    title = "Religions in Africa",
    subtitle = "",
    caption = "Wikipedia",
    color = "Islam",
    fill = "Islam") + 
  theme_instiglio_maps +
    scale_fill_gradient2(high = "#c0504d", low = "#4f81bd" , midpoint = 50) + 
    scale_color_gradient2(high = "#c0504d", low = "#4f81bd", midpoint = 50)

religions_africa
```

![](religions_africa_files/figure-markdown_github/Beatuiful%20map-1.png)

However, I am assuming that the population that is Muslim, is Christian

A different approach to the map
-------------------------------

I am going to create a new variable that contains the religion practiced by the majority of the population in each country

``` r
rel_africa_map <- rel_africa_map %>% 
  mutate(religion = if_else(Christianity > 50, "Christianity", ""))

rel_africa_map <- rel_africa_map %>% 
  mutate(religion = if_else(Islam > 50, "Islam", religion))


rel_africa_map$Traditional.religions <- as.numeric(rel_africa_map$Traditional.religions)

rel_africa_map <- rel_africa_map %>% 
  mutate(religion = if_else(Traditional.religions > 50, "Traditional", religion))

rel_africa_map <- rel_africa_map %>% 
  mutate(religion = if_else(religion == "", "No majority", religion))
```

``` r
colors <- c("#4f81bd","#c0504d", "#1f6862", "#c4bd97")

religions_africa2 <- ggplot() +
  geom_sf(data = rel_africa_map, aes(color = religion, fill = religion)) +
  theme_void() + theme_instiglio_maps +
  labs(
    title = "Majority Religions in African Countries",
    subtitle = "",
    caption = "Wikipedia, 2018",
    color = "",
    fill = "") + 
    scale_color_manual(values = alpha(colors, 0.7), aesthetics = c("colour", "fill"))

religions_africa2
```

![](religions_africa_files/figure-markdown_github/Final%20map-1.png)
