rm(list = ls())

###################
#### Libraries ####
###################

library(tidyverse)
library(INLA)

######################################
#### Creating Covariance Matrices ####
######################################

load("databasemodel.Rdata")

sp = length(unique(BaseEncavi$Region))
tm = length(unique(BaseEncavi$Time))

H = inla.read.graph("reg_ant.graph")

NSp = diag(sp) # Unstructured Spatial

NTm = diag(tm) # Unstructured Temporal


SpM = inla.graph2matrix(H) # Structured Spatial

SpM = Diagonal(nrow(SpM), colSums(SpM)) - SpM

SpM = SpM |> as.matrix()

Dt = diff(diag(tm), differences = 1)

Qt = t(Dt) %*% Dt

Qt # Structured Spatial

R1 = kronecker(NSp,NTm) # Interaction Type I
R2 = kronecker(NSp,Qt) # Interaction Type II
R3 = kronecker(SpM,NTm) # Interaction Type III
R4 = kronecker(SpM,Qt) # Interaction Type IV

eigens1 = eigen(R1, symmetric = T)
eigens2 = eigen(R2, symmetric = T)
eigens3 = eigen(R3, symmetric = T)
eigens4 = eigen(R4, symmetric = T)

index1 = ((eigens1$values |> round(10)) == 0)
index2 = ((eigens2$values |> round(10)) == 0)
index3 = ((eigens3$values |> round(10)) == 0)
index4 = ((eigens4$values |> round(10)) == 0)

sum(index1)
sum(index2)
sum(index3)
sum(index4)

A1 = eigens1$vectors[,index1]
A2 = eigens2$vectors[,index2]
A3 = eigens3$vectors[,index3]
A4 = eigens4$vectors[,index4]

indexaux1 = which(R1 != 0, arr.ind = T)
C1 = data.frame(indexaux1, Value = R1[indexaux1]) |> arrange(row, col)

indexaux2 = which(R2 != 0, arr.ind = T)
C2 = data.frame(indexaux2, Value = R2[indexaux2]) |> arrange(row, col)

indexaux3 = which(R3 != 0, arr.ind = T)
C3 = data.frame(indexaux3, Value = R3[indexaux3]) |> arrange(row, col)

indexaux4 = which(R4 != 0, arr.ind = T)
C4 = data.frame(indexaux4, Value = R4[indexaux4]) |> arrange(row, col)

C1 = sparseMatrix(i = C1$row, j = C1$col, x = C1$Value)
C2 = sparseMatrix(i = C2$row, j = C2$col, x = C2$Value)
C3 = sparseMatrix(i = C3$row, j = C3$col, x = C3$Value)
C4 = sparseMatrix(i = C4$row, j = C4$col, x = C4$Value)

# save(A1, file = "A1.Rdata")
# save(A2, file = "A2.Rdata")
# save(A3, file = "A3.Rdata")
# save(A4, file = "A4.Rdata")
# save(C1, file = "C1.Rdata")
# save(C2, file = "C2.Rdata")
# save(C3, file = "C3.Rdata")
# save(C4, file = "C4.Rdata")
# save(R1, file = "R1.Rdata")
# save(R2, file = "R2.Rdata")
# save(R3, file = "R3.Rdata")
# save(R4, file = "R4.Rdata")
