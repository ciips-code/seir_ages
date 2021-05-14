library(tidyr)
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(zoo)
library(stats)

# carga RData
rm(list = ls())
load("DatosIniciales_ARG.RData")
rm(list=setdiff(ls(), c("duracionMediaInf", 
                        "diasHospCasosCriticos",
                        "diasHospCasosGraves", 
                        "periodoPreinfPromedio",
                        "dataEcdc",
                        "porcentajeCasosGraves",
                        "porcentajeCasosCriticos")))
# lee funciones
source("functions/seirAges.R", encoding = "UTF-8")
ifr = c(.003,.005,0.01)

# crea matrices de contacto y efectividad
contact_matrix = matrix(c(5,1,1,
                          2,4,4,
                          .5,1,5),3,byrow = T)
colnames(contact_matrix) = rownames(contact_matrix) = c("0-19","20-65","65+")
transmission_probability = matrix(.05,3,3)
colnames(transmission_probability) = rownames(transmission_probability) = c("0-19","20-65","65+")

# datos de poblacion ejemplo Argentina
N1 <- 14554190
N2 <- 25594851
N3 <-  5227722

N = c(N1,N2,N3)

# prepara datos reportados

t <- seq(1:nrow(dataEcdc))

def0019 <- dataEcdc$nd0004+dataEcdc$nd0509+dataEcdc$nd1014+dataEcdc$nd1519
def2064 <- dataEcdc$nd2024+dataEcdc$nd2529+dataEcdc$nd3034+dataEcdc$nd3539+dataEcdc$nd4044+dataEcdc$nd4549+dataEcdc$nd5054+dataEcdc$nd5559+dataEcdc$nd6064
def6599 <- dataEcdc$nd6569+dataEcdc$nd7074+dataEcdc$nd7579+dataEcdc$nd8084+dataEcdc$nd8589+dataEcdc$nd9099

# muertes
def_p <- cbind(def0019,def2064,def6599)

# casos nuevos diarios
inf_p <- sweep(def_p, MARGIN = 2, ifr, "/")

inf_p=inf_p[-c(1:17),]
colnames(inf_p) <- c("inf0019","inf2064","inf6599")
# expuestos
E_p = inf_p[-round(periodoPreinfPromedio,digits=0),]

# infectados
I_p = matrix(nrow=nrow(inf_p),ncol=3)

dmI_redondeada = round(duracionMediaInf,digits=0)

for (t in dmI_redondeada:nrow(inf_p)) {
  I_p[t,]=colMeans(inf_p[(t-dmI_redondeada+1):t,])*dmI_redondeada
}

# susceptibles

S_p = matrix(nrow=nrow(inf_p),ncol=3)

lastS = N

for (t in 1:nrow(E_p)) {
  lastS = lastS - E_p[t,]
  S_p[t,] = lastS
}

# recuperados

R_p = matrix(nrow=nrow(inf_p),ncol=3)
lastR = c(0,0,0)

for (t in (dmI_redondeada+1):nrow(inf_p)) {
  lastR = lastR + inf_p[t-dmI_redondeada,]
  R_p[t,]= lastR
}

# arranque para seir
zero_S=colMeans(S_p[(nrow(S_p)-1):(nrow(S_p)-15),])
zero_E=colMeans(E_p[(nrow(E_p)):((nrow(E_p)-14)),])
zero_I=colMeans(I_p[(nrow(I_p)):((nrow(I_p)-14)),])
zero_R=colMeans(R_p[(nrow(R_p)):((nrow(R_p)-14)),])
zero_d=colMeans(def_p[(nrow(def_p)):((nrow(def_p)-14)),])
zero_D=colSums(def_p)
zero_N = zero_S
 

proy <- seir_ages(dias=300,
                  duracionE = periodoPreinfPromedio,
                  duracionIi = duracionMediaInf,
                  porc_gr = porcentajeCasosGraves,
                  porc_cr = porcentajeCasosCriticos,
                  duracionIg = diasHospCasosGraves,
                  duracionIc = diasHospCasosCriticos,
                  ifr = ifr,
                  vacunados = c(0,0,0),
                  contact_matrix = contact_matrix,
                  transmission_probability = transmission_probability,
                  N = zero_N,
                  zero_sus = zero_S,
                  zero_exp = zero_E,
                  zero_cases = zero_I,
                  zero_rec = zero_R,
                  zero_D = zero_D,
                  zero_d = zero_d
)

proy <- data.frame(proy[proy$group=="g1",8],
                   proy[proy$group=="g2",8],
                   proy[proy$group=="g3",8])
colnames(proy) <- c("inf0019","inf2064","inf6599")

full <- union_all(data.frame(inf_p),proy)

plot(full[,3])


smooth <- 
cbind(
GRUPO_00_19=predict(loess(inf0019 ~ seq(1,nrow(full),1), data=full, span=0.10)),
GRUPO_20_64=predict(loess(inf2064 ~ seq(1,nrow(full),1), data=full, span=0.10)),
GRUPO_65_MAS=predict(loess(inf6599 ~ seq(1,nrow(full),1), data=full, span=0.10))
)


library(plotly)
fig <- plot_ly(data.frame(smooth), x = ~seq(1,nrow(full),1), y = ~GRUPO_00_19, name = 'Menor de 20', type = 'scatter', mode = 'lines') 
fig <- fig %>% add_trace(y = ~GRUPO_20_64, name = '20 a 64', mode = 'lines') 
fig <- fig %>% add_trace(y = ~GRUPO_65_MAS, name = '65 y más', mode = 'lines')

fig
