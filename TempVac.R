segundasDosisHoyV3 = segundasDosisHoyS3 = rep(0,length(ageGroups))
# Asume 30 el intervalo entre primera y segunda dosis
if (t > (tVacunasCero + latencia + 30) && t < (tVacunasCero + diasVacunacion + 30)) {
  #  - La segunda dosis se aplica a la linea 3 en S y en V, y la 1 en S
  #  - Dosis para V3 =  CantDia * porcV
  #  - Dosis para S3 =  CantDia * porcProt
  #  - Dosis para S1 =  CantDia * 1-porcV-porcProt
  #  - Dosis para V3: Se pasan de 3 a 4
  #  - Dosis para S3: Se pasan de 3 a 4
  #  - Dosis para S1: Se quedan en S1
  #  - Con esto no estamos prolongando los tiempos
  #  - Se mantienen los efectos de la primera dosis, el que paso a V se queda, el que tuvo proteccion se queda, etc
  # # segundasDosisHoyS1 no se calcula
  dosis1aReforzar =  vA[[t-30]][3,] #S[[t-1]][3,]/30 #vA[[t-30]][3,]
  
  segundasDosisHoyV3 = dosis1aReforzar * porcV
  Vout[3,] =  Vout[3,] + segundasDosisHoyV3
  Vin[4,] = Vin[4,] + segundasDosisHoyV3
  
  
  segundasDosisHoyS3 = dosis1aReforzar * porcProt
  for (iAge in c(1:length(ageGroups))) {
    if (segundasDosisHoyS3[iAge] < S[[t]][3,iAge]) {
      S[[t]][3,iAge] = S[[t]][3,iAge] - segundasDosisHoyS3[iAge]
      S[[t]][4,iAge] = S[[t]][4,iAge] + segundasDosisHoyS3[iAge]
    } else {
      S[[t]][4,iAge] = S[[t]][4,iAge] + S[[t]][3,iAge]
      S[[t]][3,iAge] = S[[t]][3,iAge] * 0
    }
  }
  
  vA[[t]][4,] = segundasDosisHoyV3 + segundasDosisHoyS3
}

#####--------------------------------



refuerzosHoyS4 = 0
###############################
# browser( expr = { t >= 695})
# refuerzosHoyV4 = refuerzosHoyS4 = refuerzosHoyS4Efectivos = rep(0,length(ageGroups))
# if (t>2) {
#   dosis2aReforzar = S[[t-2]][4,]/30
#   
#   refuerzosHoyV4 = dosis2aReforzar * porcV
#   Vout[4,] =  Vout[4,] + refuerzosHoyV4
#   Vin[4,] = Vin[4,] + refuerzosHoyV4
#   
#   refuerzosHoyS4 = dosis2aReforzar * porcProt
#   vA[[t]][4,] = vA[[t]][4,] + refuerzosHoyV4 + refuerzosHoyS4
# }
###############################

#####------------------------------

# # Salida de V
loopTiempoV = tiempoV * modificadorVariantes[[5]][1,1]
totSalidaVHoy = V[[t-1]] / loopTiempoV
Vout[3,] <- totSalidaVHoy[3,]
Vout[4,] <- totSalidaVHoy[4,]
V[[t]][3,] = V[[t-1]][3,] + Vin[3,] - Vout[3,]
V[[t]][4,] = V[[t-1]][4,] + Vin[4,] - Vout[4,]
# if (t>loopTiempoV+1) {
#   # browser(expr = { t == 1000 })
#   totSalidaVHoy = v[[t-loopTiempoV]][vacuna,]
#   # reasignar entre 3 y 4 de acuerdo a proporciones
#   porc3 = V[[t-1]][3,] / (V[[t-1]][3,] + V[[t-1]][4,])
#   porc3[is.na(porc3)] <- 0
#   totSalidaVHoy3 = totSalidaVHoy * porc3
#   porc4 = V[[t-1]][4,] / (V[[t-1]][3,] + V[[t-1]][4,])
#   porc4[is.na(porc4)] <- 0
#   totSalidaVHoy4 = totSalidaVHoy * porc4
#   Vout[3,] <- totSalidaVHoy3
#   Vout[4,] <- totSalidaVHoy4
# }
# V[[t]][3,] = V[[t-1]][3,] + Vin[3,] - Vout[3,]
# V[[t]][4,] = V[[t-1]][4,] - Vout[4,]
# for (iAge in c(1:length(ageGroups))) {
#   if (segundasDosisHoyV3[iAge] < V[[t]][3,iAge]) {
#     V[[t]][3,iAge] = V[[t]][3,iAge] - segundasDosisHoyV3[iAge]
#     V[[t]][4,iAge] = V[[t]][4,iAge] + segundasDosisHoyV3[iAge]
#   }
# }
v[[t]] = Vin
# Empiezo a armar el S a partir de las transiciones
S[[t]] = S[[t-1]] - e[[t-1]]  + Vout
S[[t]][1,] = S[[t]][1,] - colSums(Vin)
S[[t]][2,] = S[[t]][2,] + colSums(losQueHoyPierdenImunidad)
# # Reasigno de renglon los que quedaron hoy en S luego de vacunarse
S[[t]][1,]=S[[t]][1,] - colSums(VquedaEnS)
S[[t]] =  S[[t]] + VquedaEnS

# Sacar de S3 a los que se les vence la vacunacion
# print(paste(t,1))
# S[[t]][3,]= S[[t-1]][3,] - S[[t-1]][3,] / loopTiempoV
# S[[t]][4,]= S[[t-1]][4,] + S[[t-1]][3,] / loopTiempoV
# # browser(expr = { t==500 })
# print(paste(2, S[[t]][3,], S[[t]][1,]))

# for (iAge in c(1:length(ageGroups))) {
#   #browser(expr = {t==338})
#   if (segundasDosisHoyS3[iAge] < S[[t]][3,iAge]) {
#     S[[t]][3,iAge] = S[[t]][3,iAge] - segundasDosisHoyS3[iAge]
#     S[[t]][4,iAge] = S[[t]][4,iAge] + segundasDosisHoyS3[iAge]
#   }
# }
# S[[t]][1,] = colSums(E[[t]]) - colSums(I[[t]]) - colSums(R[[t]]) - colSums(V[[t]])
# los recuperados de hoy son los recuperados de ayer menos los que se expusieron hoy
# Pasar de renglon a "No inmunes" a los vacunados que vencen (tiempoP, index 5 en paramvac)


#------------------------------



# Vuelve a No inmunes los que terminan su tiempo de proteccion
for (vacuna in c(4:nrow(paramVac))) {
  if (is.null(modificador_tiempoP)==F) {
    tiempoP = modificador_tiempoP  
  } else {
    tiempoP = as.numeric(paramVac[vacuna,5])  
  }
  loopTiempoP = tiempoP * modificadorVariantes[[5]][1,1]
  # print(paste(fechas_master[t], S[[t]][vacuna,], (S[[t]][vacuna,]/loopTiempoP), refuerzosHoyS4))
  S[[t]][vacuna,] =  S[[t]][vacuna,] - ( (S[[t]][vacuna,]/loopTiempoP) - refuerzosHoyS4)
  S[[t]][1,]=S[[t]][1,] + (S[[t]][vacuna,]/loopTiempoP)
  
  # tVacunadosQueVencenHoy = t - tiempoP
  # if (tVacunadosQueVencenHoy>1) {
  #   porcentajeSinRefuerzo = .2
  #   S[[t]][vacuna,] =  S[[t]][vacuna,] - S[[tVacunadosQueVencenHoy]][vacuna,] * porcentajeSinRefuerzo
  #   S[[t]][1,]=S[[t]][1,] + S[[tVacunadosQueVencenHoy]][vacuna,] * porcentajeSinRefuerzo
  # }
}