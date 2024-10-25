library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(plotly)
library(sf)
library(stringr)
library(tmap)
library(tmaptools)
library(janitor)
library(here)
library(fs)
library(ggplot2)
library(terra)

#load csv data and select gender data we want
rawdata <- read.csv(here("hw4_data","HDR23-24_Composite_indices_complete_time_series.csv"),
                    na= "NULL")
giidata <- rawdata %>%
  dplyr::select(contains("iso3"),
                contains("country"),
                contains("hdicode"),
                contains("region"),
                contains("gii_"))%>%
  clean_names()

#load geojson data
map <- st_read(here("hw4_data", "World_Countries_(Generalized)_9029012925078512962.geojson"))

#instal countrycode pkg
install.packages("countrycode")
library(countrycode)

#将giidata中的 ISO3 列转换为 ISO2格式
giidata2 <- giidata%>%
  mutate(iso2=iso3)%>%
  select(iso2, everything())%>% #将iso2放到第一列
  slice(1:195)
giidata2$iso2 <- countrycode(giidata2$iso3, origin = 'iso3c', destination = 'iso2c')
  
# 查看转换后的数据
head(giidata2)

#merge csv and shp
mapmerge <- map%>%
  merge(.,
        giidata2,
        by.x="ISO", 
        by.y="iso2")

mapmerge%>%
  head(., n=10)

#mutate a new column to show the difference between 2010 and 2019
mapmerge <- mapmerge%>%
  mutate(dif = gii_2019-gii_2010)

#push to github
library(usethis)

#type "use_github()" in console area

#lable the differences
mapmerge_above_below <- mapmerge %>%
  #na.omit() %>%
  mutate(above_below = case_when(dif<0 ~ "below",
                                 dif>0 ~ "above",
                                 dif==0 ~ "equal"
  ))

#check if there are some null in 'above_below' column
sum(is.na(mapmerge_above_below$above_below))

#make a map using tmap
tm_shape(mapmerge_above_below) + 
  tm_polygons("above_below", 
              # style="pretty",
              palette="Greens",
              midpoint=NA,
              #title="Number of years",
              alpha = 0.5) + 
  tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "GII difference between 2010 and 2019", 
            legend.position = c("right", "bottom"),
            legend.outside = TRUE)

#make a map using ggplot2
ggplot(data = mapmerge_above_below) +
  geom_sf(aes(fill = above_below)) +
  scale_fill_manual(
    values = c("above" = "red", "below" = "blue", "NA" = "grey"),
    na.value = "grey",
    name = "GII Difference between 2010 and 2019",
    labels = c("Above" = "Above", "Below" = "Below", "NA" = "No Data")
  ) +
  labs(title = "GII in 2019 Above/Below 2010") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "bottom",  # 图例在底部
    legend.justification = "center",  # 图例居中
    legend.box.margin = margin(10, 10, 10, 10)  # 增加图例与图框的距离
  )




