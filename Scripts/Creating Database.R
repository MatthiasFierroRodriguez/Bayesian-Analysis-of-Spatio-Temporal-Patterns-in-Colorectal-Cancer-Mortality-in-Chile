rm(list = ls())

###################
#### Libraries ####
###################

# install.packages("tidyverse")
# install.packages("readxl")
library(tidyverse)
library(readxl)

############################
#### Mortality Database ####
############################

data = read.csv("DEFUNCIONES_FUENTE_DEIS_1990_2022_12012023.csv",
                header = F, sep = ";", fileEncoding = "latin1") |>
  filter(V10 == "C00-D48") |> 
  select(-c(V2, V6, V7, V9, V10, V11, V12, V13, V15, V16, V17, V18,
            V19, V20, V21, V22, V23, V24, V25, V26, V27)) |> 
  filter(V14 %in% c("C18", "C19", "C20", "C21")) |> 
  select(-V14) |>
  mutate(Edad = NA)

data[data$V4 != 1, ] = data |> filter(V4 != 1) |> mutate(V5 = 0.1) # If the age is in months, it is changed to 0.1 years to avoid problems

data = data |>
  filter(V5 >= 50) |> # We considered only individuals older than 50 years.
  mutate(Edad = cut(V5, breaks = c(seq(50, 80, by = 5), Inf), # We define five-year age groups up to 80 years of age, based on the census database
                    ordered_result = T,
                    include.lowest = T)) |>
  select(-c(V4, V5))

Colon = data |>
  group_by(V1, V3, V8, Edad) |>
  summarise(n = n())

colnames(Colon) = c("Año", "Sexo", "Region", "Edad", "Casos")

# save(data, file = "CancerColonNacional.Rdata")
# save(Colon, file = "CasosCancerColon.Rdata")

################################################
#### Population Estimations and Proyections ####
################################################

rm(list = ls())

Pob = read.table("ine_estimaciones-y-proyecciones-2002-2035_base-2017_region_base.csv",
                 header = T, sep = ",") # For 2006 and 2015

Pob2000 = read_excel("Poblacion2000.xlsx") # For 2000

Pob$Region = Pob$Region |> 
  replace(Pob$Region == 1, "De Tarapacá") |> 
  replace(Pob$Region == 2, "De Antofagasta") |> 
  replace(Pob$Region == 3, "De Atacama") |> 
  replace(Pob$Region == 4, "De Coquimbo") |> 
  replace(Pob$Region == 5, "De Valparaíso") |> 
  replace(Pob$Region == 6, "Del Libertador B. O'Higgins") |> 
  replace(Pob$Region == 7, "Del Maule") |> 
  replace(Pob$Region == 8, "Del Bíobío") |> 
  replace(Pob$Region == 9, "De La Araucanía") |> 
  replace(Pob$Region == 10, "De Los Lagos") |> 
  replace(Pob$Region == 11, "De Aisén del Gral. C. Ibáñez del Campo") |> 
  replace(Pob$Region == 12, "De Magallanes y de La Antártica Chilena") |> 
  replace(Pob$Region == 13, "Metropolitana de Santiago") |> 
  replace(Pob$Region == 14, "De Los Ríos") |> 
  replace(Pob$Region == 15, "De Arica y Parinacota") |> 
  replace(Pob$Region == 16, "De Ñuble")

Pob$Sexo = ifelse(Pob$Sexo == 1, "Hombre", "Mujer")

Pob = Pob |> filter(Edad >= 50) # We considered only individuals older than 50 years.
Pob2000 = Pob2000 |> filter(Edad >= 50) # We considered only individuals older than 50 years.

breaks = c(seq(50, 80, by = 5), Inf) # We define five-year age groups up to 80 years of age, based on the census database

Pob = Pob |> mutate(Edad = cut(Edad, breaks = breaks,
                               ordered_result = T,
                               include.lowest = T))

Pob2000 = Pob2000 |> mutate(Edad = cut(Edad, breaks = breaks,
                                       ordered_result = T,
                                       include.lowest = T))

Pob_L = pivot_longer(Pob, cols = a2002:a2035,
                     names_to = "Año")

Pob_L$Año = Pob_L$Año |> str_remove("a") |> as.numeric()  

Poblacion = Pob_L |>
  group_by(Region, Sexo, Edad, Año) |>
  summarise(Poblacion = sum(value)) |> 
  ungroup()

Pob2000 = Pob2000[, colnames(Poblacion)]

Poblacion = rbind(Pob2000, Poblacion)

# save(Poblacion, file = "PoblacionSexoEdad.Rdata")

#############################################################
#### Standardized Mortality Ratios for Colorectal Cancer ####
#############################################################

rm(list = ls())

load("CasosCancerColon.Rdata")
load("PoblacionSexoEdad.Rdata")

Colon = Colon |> filter(Region != "Ignorada")

Plantilla = expand.grid(
  Año = Colon$Año |> unique(),
  Region = Colon$Region |> unique(),
  Sexo = Colon$Sexo |> unique(),
  Edad = Poblacion$Edad |> unique()
) 

aux = left_join(Plantilla, Colon, by = colnames(Plantilla))

aux$Casos = aux$Casos |>
  replace(is.na(aux$Casos), 0)

Colon_c = left_join(aux, Poblacion, by = c("Año", "Sexo", "Region", "Edad"))

## Some regions were created after 2007, so we merged them with the original regions to which they belonged:

Colon_Atacama = Colon_c |>
  filter(Region %in% c("De Arica y Parinacota", "De Atacama")) |>
  group_by(Año, Sexo, Edad) |> 
  summarise(Casos = sum(Casos), Poblacion = sum(Poblacion)) |> 
  mutate(Region = "De Atacama") |> 
  ungroup()

Colon_Lagos = Colon_c |>
  filter(Region %in% c("De Los Lagos", "De Los Ríos")) |>
  group_by(Año, Sexo, Edad) |> 
  summarise(Casos = sum(Casos), Poblacion = sum(Poblacion)) |> 
  mutate(Region = "De Los Lagos") |> 
  ungroup()

Colon_Biobio = Colon_c |>
  filter(Region %in% c("Del Bíobío", "De Ñuble")) |>
  group_by(Año, Sexo, Edad) |> 
  summarise(Casos = sum(Casos), Poblacion = sum(Poblacion, na.rm = T)) |> 
  mutate(Region = "Del Bíobío") |> 
  ungroup()

Colon_cr = Colon_c |> filter(!(Region %in% c("De Arica y Parinacota",
                                             "De Atacama",
                                             "De Los Lagos",
                                             "De Los Ríos",
                                             "Del Bíobío",
                                             "De Ñuble")))

Colon_c = rbind(Colon_cr, Colon_Atacama, Colon_Biobio, Colon_Lagos)

Colon_cr2000 = Colon_c |> filter(Año == 2000)

Colon_cr2000

rj2000 = Colon_cr2000 |>
  group_by(Edad, Sexo) |> 
  summarise(Suma = sum(Casos), Pop = sum(Poblacion)) |>
  ungroup() |>
  mutate(rj = Suma/Pop) |> select(c(Edad,Sexo,rj))

Ei2000 = Colon_cr2000 |>
  select(Año, Region, Edad, Sexo, Poblacion) |> 
  left_join(rj2000, by = c("Edad","Sexo")) |> 
  group_by(Año, Region) |> summarise(Ei = sum(Poblacion*rj))

Mortalidad2000 = Colon_cr2000 |>
  group_by(Año, Region) |>
  summarise(Casos = sum(Casos)) |> ungroup()

Final2000 = left_join(Mortalidad2000, Ei2000, by = c("Region", "Año")) |>
  mutate(SMR = Casos/Ei)

Colon_cr2006 = Colon_c |> filter(Año == 2006)

rj2006 = Colon_cr2006 |>
  group_by(Edad, Sexo) |> 
  summarise(Suma = sum(Casos), Pop = sum(Poblacion)) |>
  ungroup() |>
  mutate(rj = Suma/Pop) |> select(c(Edad,Sexo,rj))

Ei2006 = Colon_cr2006 |>
  select(Año, Region, Edad, Sexo, Poblacion) |> 
  left_join(rj2006, by = c("Edad","Sexo")) |> 
  group_by(Año, Region) |> summarise(Ei = sum(Poblacion*rj))

Mortalidad2006 = Colon_cr2006 |>
  group_by(Año, Region) |>
  summarise(Casos = sum(Casos)) |>
  ungroup()

Final2006 = left_join(Mortalidad2006, Ei2006, by = c("Region", "Año")) |>
  mutate(SMR = Casos/Ei)

Colon_cr2015 = Colon_c |> filter(Año == 2015)

rj2015 = Colon_cr2015 |> group_by(Edad, Sexo) |> 
  summarise(Suma = sum(Casos), Pop = sum(Poblacion)) |>
  ungroup() |>
  mutate(rj = Suma/Pop) |> select(c(Edad,Sexo,rj))

Ei2015 = Colon_cr2015 |>
  select(Año, Region, Edad, Sexo, Poblacion) |> 
  left_join(rj2015, by = c("Edad","Sexo")) |> 
  group_by(Año, Region) |> summarise(Ei = sum(Poblacion*rj))

Mortalidad2015 = Colon_cr2015 |>
  group_by(Año, Region) |>
  summarise(Casos = sum(Casos)) |> 
  ungroup()

Final2015 = left_join(Mortalidad2015, Ei2015, by = c("Region", "Año")) |>
  mutate(SMR = Casos/Ei)

Colon_cr2016 = Colon_c |> filter(Año == 2016)

rj2016 = Colon_cr2016 |> group_by(Edad, Sexo) |> 
  summarise(Suma = sum(Casos), Pop = sum(Poblacion)) |>
  ungroup() |>
  mutate(rj = Suma/Pop) |>
  select(c(Edad,Sexo,rj))

Ei2016 = Colon_cr2016 |>
  select(Año, Region, Edad, Sexo, Poblacion) |> 
  left_join(rj2016, by = c("Edad","Sexo")) |> 
  group_by(Año, Region) |> summarise(Ei = sum(Poblacion * rj))

Mortalidad2016 = Colon_cr2016 |>
  group_by(Año, Region) |>
  summarise(Casos = sum(Casos)) |>
  ungroup()

Final2016 = left_join(Mortalidad2016, Ei2016, by = c("Region", "Año")) |>
  mutate(SMR = Casos/Ei)

BaseFinal = rbind(
  Final2000,
  Final2006,
  Final2015,
  Final2016
)

# save(BaseFinal, file = "SMR_Nacional.Rdata")

#####################################################
#### Merging all databases into a single dataset ####
#####################################################

rm(list = ls())

load("SMR_Nacional.Rdata")

BaseFinal = BaseFinal |> filter(!(Año == 2016))

Encavi = read_excel("Original Database.xlsx", sheet = "Hoja1") |> select(-Casos)

BaseEncavi = left_join(BaseFinal, Encavi, by = c("Region", "Año"))

auxID = data.frame(Region = BaseEncavi$Region |> unique(),
                   ID = c(11, 2, 3, 4, 9, 10, 12, 1, 5, 8, 6, 7, 13)) |> arrange(ID)

auxTIME = data.frame(Año = BaseEncavi$Año |> unique(), Time = 1:3)

BaseEncavi = left_join(BaseEncavi, auxID, by = "Region")
BaseEncavi = left_join(BaseEncavi, auxTIME, by = "Año")

BaseEncavi = BaseEncavi |> arrange(ID, Time)

# save(file = "BaseFinalActualizada.Rdata", BaseEncavi)

#################################
#### Last Update of Database ####
#################################

rm(list = ls())

load("BaseFinalActualizada.Rdata")
load("PoblacionSexoEdad.Rdata")

Diccionario = Poblacion |> 
  group_by(Region, Año) |> 
  summarise(Poblacion = sum(Poblacion))

BaseEncavi = left_join(BaseEncavi, Diccionario, by = c("Region", "Año"))

# save(file = "OriginalDatabaseUpdated.Rdata", BaseEncavi)
