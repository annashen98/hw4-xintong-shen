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








































