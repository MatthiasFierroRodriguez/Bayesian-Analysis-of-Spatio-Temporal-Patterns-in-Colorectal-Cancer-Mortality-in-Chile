rm(list = ls())

###################
#### Libraries ####
###################

# install.packages(tidyverse)
library(tidyverse)

##################
#### Database ####
##################

load("OriginalDatabaseUpdated.Rdata")

############################
#### Recoding Variables ####
############################

BaseEncavi = BaseEncavi |>
  mutate(Sexo = Sexo_H/Sexo_M) |>
  mutate(Zona = Zona_U/Zona_R) |> 
  mutate(Desayuno = Desayuno_Todos/(Desayuno_Aveces + Desayuno_Nunca)) |> 
  mutate(Peso = Peso_Normal/(Peso_Bajopeso + Peso_Sobrepeso + Peso_Obeso)) |> 
  mutate(FumaCasa = FumaCasa_SI/FumaCasa_NO) |>
  mutate(FumaMes = FumaMes_SI/FumaMes_NO) |> 
  mutate(HumoTrabajo = HumoTrabajo_SI/HumoTrabajo_NO) |>
  mutate(VerdurasFrutas = (Verd_Frutas_Todos)/(Verd_Frutas_2o3_sem + Verd_Frutas_1_sem + Verd_Frutas_1o2_mes)) |> 
  mutate(CarnesBlancas = (Carnes_Blancas_Todos + Carnes_Blancas_2o3_sem)/(Carnes_Blancas_1_sem + Carnes_Blancas_1o2_mes)) |> 
  mutate(Legumbres = (Legumbres_2o3_sem)/(Legumbres_Todos + Legumbres_1_sem + Legumbres_1o2_mes)) |> 
  mutate(Lacteos = (Lacteos_2o3_sem + Lacteos_Todos)/(Lacteos_1_sem + Lacteos_1o2_mes)) |> 
  mutate(Toma = Toma_SI/Toma_NO) |> 
  mutate(Vivienda = (Vivienda_Casa)/(Vivienda_Depto + Vivienda_Pieza + Vivienda_Mediagua + Vivienda_Choza)) |> 
  mutate(Educacion = (Educa_Universidad + Educa_TP + Educa_Humanidades)/(Educa_Normal + Educa_IP + Educa_Basica + Educa_Media)) |> 
  mutate(EstadoCivil = (Civil_Casado + Civil_Conviviente)/(Civil_Soltero + Civil_Viudo + Civil_Separado + Civil_Divorciado)) |> 
  mutate(Privacidad = Privacidad_Bien/(Privacidad_Reg + Privacidad_Mal)) |> 
  mutate(Dinero = Dinero_Bien/(Dinero_Reg + Dinero_Mal)) |>
  mutate(CondicionFisica = CondicionFisica_Bien/(CondicionFisica_Mal + CondicionFisica_Regular)) |> 
  mutate(SaludMental = BienMental_Bien/(BienMental_Mal + BienMental_Regular)) |> 
  mutate(Diversion = Diversion_Bien/(Diversion_Regular + Diversion_Mal)) |> 
  mutate(VidaFamiliar = Vidafam_Bien/(Vidafam_Regular + Vidafam_Mal)) |> 
  mutate(Trabajo = Trabajo_Bien/(Trabajo_Regular + Trabajo_Mal)) |> 
  mutate(VidaGeneral = Vidagral_Bien/(Vidagral_Regular + Vidagral_Mal)) |>
  mutate(Salud = Salud_Bien/(Salud_Regular + Salud_Mal)) |> 
  mutate(InterfiereDolor = (InterfiereDolor_Nada + InterfiereDolor_Poco)/(InterfiereDolor_Moderado + InterfiereDolor_Mucho)) |> 
  mutate(Accidente = AccidenteNO/Accidente_SI) |> 
  mutate(Dedicacion_Trabajar = Dedicacion_Trabajar) |> 
  mutate(Dedicacion_Casa = Dedicacion_Casa) |> 
  mutate(Jubilado = Jubilado_NO/Jubilado_SI) |>
  mutate(Auto = Auto_SI/Auto_NO) |> 
  mutate(Decil = (Decil_10 + Decil_9 + Decil_8 + Decil_7 + Decil_6 + Decil_5)/(Decil_4 + Decil_3 + Decil_2 + Decil_1)) |> 
  select(Año, Region, Casos, Ei, SMR, ID, Time, Poblacion,
         Sexo, Zona, Desayuno, Peso, FumaCasa, FumaMes, HumoTrabajo, VerdurasFrutas,
         CarnesBlancas, Legumbres, Lacteos, Toma, Vivienda, Educacion, EstadoCivil, Privacidad,
         Dinero, CondicionFisica, SaludMental, Diversion, VidaFamiliar, Trabajo, VidaGeneral,
         Salud, InterfiereDolor, Accidente, Dedicacion_Trabajar, Jubilado, Decil)

#######################
#### Save Database ####
#######################

# save(BaseEncavi, file = "databasemodel.Rdata")
