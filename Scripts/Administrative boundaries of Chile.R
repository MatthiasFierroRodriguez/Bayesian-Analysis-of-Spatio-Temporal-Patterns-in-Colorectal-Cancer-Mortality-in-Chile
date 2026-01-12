rm(list = ls())

###################
#### Libraries ####
###################

library(tidyverse)
library(chilemapas)
library(sf)
library(spdep)
library(INLA)

############################################
#### Administrative Boundaries of Chile ####
############################################

regiones = chilemapas::generar_regiones()

## Some regions were created after 2007, so we merged them with the original regions to which they belonged:

reg1 = regiones |> filter(codigo_region %in% c("01", "15")) |> st_union() |> st_sf() |> mutate(codigo_region = "01") 
reg8 = regiones |> filter(codigo_region %in% c("16", "08")) |> st_union() |> st_sf() |> mutate(codigo_region = "08")
reg10 = regiones |> filter(codigo_region %in% c("10", "14")) |> st_union() |> st_sf() |> mutate(codigo_region = "10")

st_geometry(reg1) = "geometry"
st_geometry(reg8) = "geometry"
st_geometry(reg10) = "geometry"

regiones_c = regiones |> filter(!(codigo_region %in% c("01", "08", "10", "14", "15", "16")))

reg_ant = rbind(reg1,reg8,reg10, regiones_c) |> arrange(codigo_region)

aux = st_crop(reg_ant, xmin = -80, ymin = -56.52511,
              xmax = -66.41617, ymax = -17.49778)

# save(aux, file = "regantgeo.Rdata")

########################
#### Creating Graph ####
########################

temp = poly2nb(aux)

# nb2INLA(file = "reg_ant.graph", temp)
