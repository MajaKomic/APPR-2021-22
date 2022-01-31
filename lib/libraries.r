library(knitr)
library(rvest)
library(gsubfn)
library(tidyr)
library(tmap)
library(shiny)
library(readr)
library(dplyr)
library(tidyverse)
library(stringi)

library(ggplot2)
library(dplyr)
library(plotly)

library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(cluster)
library(ggalt)

#v to mapo pišemo vse knjižnice, v ostale pa ni treba nobene
options(gsubfn.engine="R")

# Uvozimo funkcije za pobiranje in uvoz zemljevida.
source("lib/uvozi.zemljevid.r", encoding="UTF-8")
