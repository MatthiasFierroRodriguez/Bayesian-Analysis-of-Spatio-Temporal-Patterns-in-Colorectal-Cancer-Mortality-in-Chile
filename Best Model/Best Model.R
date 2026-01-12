rm(list = ls())

###################
#### Libraries ####
###################

# install.packages("tidyverse")
# install.packages("INLA", repos = c(getOption("repos"), 
#                                    INLA = "https://inla.r-inla-download.org/R/stable"),
#                  dep = TRUE)
library(tidyverse)
library(INLA)

##################
#### Database ####
##################

load("databasemodel.Rdata")

#######################
#### Spatial Graph ####
#######################

H = inla.read.graph("reg_ant.graph")

##############################################
#### Spatio-Temporal Interaction Matrices ####
##############################################

load("A1.Rdata")
load("C1.Rdata")
load("A2.Rdata")
load("C2.Rdata")
load("A3.Rdata")
load("C3.Rdata")
load("A4.Rdata")
load("C4.Rdata")

A1 = t(A1)
A2 = t(A2)
A3 = t(A3)
A4 = t(A4)

#################
#### Indices ####
#################

BaseEncavi$IDTIME = 1:nrow(BaseEncavi)
BaseEncavi$Time2 = BaseEncavi$Time

################################################################
#### Model Without Spatio-Temporal Interaction (Best Model) ####
################################################################

formula.noint =  Casos ~ 1 + 
  
  # Explanatory Variables:
  
  # log(Sexo) + #
  log(Desayuno) + ############ 275.1759
  # log(Peso) + #
  # log(FumaCasa) + #
  # log(FumaMes) + #
  # log(HumoTrabajo) + #
  # log(VerdurasFrutas) + #
  log(CarnesBlancas) + ################ 274.8853
  log(Legumbres) + ############## 274.8304
  # log(Toma + 10^(-6))+ #
  # log(Lacteos) + #
  log(Educacion) + ################## 279.6328
  log(EstadoCivil) + ################## 275.9254
  # log(Privacidad) + #
  # log(Dinero) + #
  # log(CondicionFisica) + #
  # log(SaludMental) + #
  # log(Diversion) + #
  log(VidaFamiliar) + #################### 277.2812
  # log(Trabajo) + #
  # log(VidaGeneral) + #
  # log(Salud) + #
  # log(InterfiereDolor) + #
  # log(Accidente) + #
  # log(Dedicacion_Trabajar) + #
  log(Jubilado) + ################# 274.6128
  # log(Decil) + #
  
  # Spatial Random Effects:
  
  f(ID, model = "bym2", # Model
    graph = H, # Graph
    scale.model = T, # Scaling
    constr = T, # Constraint
    hyper = list(
      phi = list(prior = "pc", param = c(0.5, 2/3)), # Prior Dist. Phi
      prec = list(prior = "pc.prec", param = c(0.75, 0.05))) # Prior Dist. Theta
  ) +
  
  # Temporal Random Effects:
  
  f(Time, model = "rw1", # Model
    scale.model = T, # Scaling
    constr = T, # Constraint
    hyper = list(theta = list(prior = "pc.prec", param = c(1,0.01))) # Prior Dist. Theta
  ) +
  
  f(Time2, model = "iid")  # Model

# Model fitting:

mod.noint = inla(formula.noint, # Formula
                 family = "poisson", # Likelihood
                 data = BaseEncavi, # Database
                 E = Ei, # Offset
                 control.predictor = list(compute = T), # Adjusted Values
                 control.compute = list(dic = T, # DIC
                                        cpo = T, # CPO
                                        return.marginals.predictor = T, # Marginals
                                        waic = T, # WAIC
                                        config = T), # INLA Format
                 control.inla = list(strategy = "laplace"), # Approximation Strategy
                 control.fixed = list(mean.intercept = 0, # Mean Intercept
                                      prec.intercept = 0.001, # Precision Intercept
                                      mean = 0, # Mean Coefficients
                                      prec = 0.001)) # Precision Coefficients

#######################
#### Model Summary ####
#######################

mod.noint |> summary()

mod.noint$summary.hyperpar
mod.noint$summary.random
mod.noint$summary.fixed

#########################
#### Goodness of fit ####
#########################

mod.noint$dic$dic
mod.noint$dic$p.eff
mod.noint$dic$mean.deviance
mod.noint$cpo$cpo |> log() |> sum()

######################
#### Saving Model ####
######################

# save(mod.noint, file = "BestModel.Rdata")
