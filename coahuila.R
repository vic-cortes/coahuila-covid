library(dplyr) 
library(readr) 
library(janitor) 
library(rgdal) # shp files
library(tmap) # Mapas

source("const.R")


data <- read_csv(url(DATA_URL)) %>%
            clean_names()


# Limpieza de datos
data %>%
    mutate(fecha_identificacion = as.Date(fecha_identificacion,"%d/%m/%Y")) %>%
    group_by(cve_mun, fecha_identificacion) %>%
    tally(name = "n_casos") %>%
    mutate(acumulado = cumsum(n_casos))
