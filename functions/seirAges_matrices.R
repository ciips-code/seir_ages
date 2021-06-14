seir_ages <- function(dias = 500,
                      duracionE, 
                      duracionIi, 
                      porc_gr, 
                      duracionIg, 
                      porc_cr, 
                      duracionIc,
                      ifr = c(.03,.03,.03),
                      vacunados = c(0,0,0),
                      contact_matrix,
                      transmission_probability,
                      N, 
                      modif_beta = matrix(c(1,1,1,
                                            .70,.70,.70,
                                            .60,.60,.60),3,3,byrow=T),
                      modif_porc_gr = matrix(c(1,1,1,
                                               .70,.70,.70,
                                               .60,.60,.60),3,3,byrow=T),
                      modif_porc_cr = matrix(c(1,1,1,
                                               .70,.70,.70,
                                               .60,.60,.60),3,3,byrow=T),
                      modif_ifr = matrix(c(1,1,1,
                                               .70,.70,.70,
                                               .60,.60,.60),3,3,byrow=T),
                      zero_sus,
                      zero_exp,
                      zero_cases = c(0,1/45e6*N,0),
                      zero_rec,
                      zero_D,
                      zero_d,
                      duracion_inmunidad=190, # cuanto se quedan en U
                      Rt=c(1.1,1.1,1.1),
                      defunciones_reales,
                      expuestos_reales
){
  immunityStates = c("no inmunes", "recuperados", "Vacunado")
  ageGroups = c("0 a 19", "20 a 64", "65 y mas")
  names = list(immunityStates,
               ageGroups)
  #browser()
  # cada columna es un grupo
  e = E = S = i = Ss = I = Ii = Ig = Ic = r = R = D = d = U = u = Av = V = v = beta = lapply(1:dias, matrix, data= 0, nrow=length(immunityStates), ncol=length(ageGroups), dimnames = names)
  
  namesVac = list(immunityStates,
                  c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP"))

  paramVac <- matrix(data=c(0,0,0,0,0,
                            0,0,0,0,0,
                            20,.2,180,.3,180), nrow=length(immunityStates), ncol=5, byrow=T, dimnames = namesVac)

  # zero cases
  #S[1,] = zero_sus
  #Ss[1,] = zero_sus
  #E[1,] = zero_exp
  
  
  S[[1]] = matrix(c(N[1],N[2],N[3],0,0,0,0,0,0,0,0,0),length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = names)
  
  N = matrix(c(N[1],N[2],N[3],0,0,0,0,0,0,0,0,0),length(immunityStates),length(ageGroups), byrow = T,
               dimnames = names)
  
  I[[1]] = matrix(0,length(immunityStates),length(ageGroups), byrow = T,
                    dimnames = names)
  I[[1]][1,2] = 1 # La semilla del primer infectado
  
  #Av = Historia de vacunación + Plan de vacunación futuro
  # Av = lapply(1:dias, matrix, data=c(0,0,0, # en cero por compatibilidad con la estructura de la matriz
  #                                    0,0,0, # en cero por compatibilidad con la estructura de la matriz
  #                                    0,50,100), nrow=length(immunityStates), ncol=length(ageGroups), dimnames = names)
  # 
  Av = creaAv(min(modeloSimulado$fecha))
  # print(length(Av))
  #R[1,] = zero_rec
  #d[1,] = zero_d
  #D[1,] = zero_D
  
  # efecto vacunas
  # porc_gr = porc_gr * (1-vacunados)
  # porc_cr = porc_cr * (1-vacunados)
  
  # seir
  tHoy = nrow(defunciones_reales)-18-round(periodoPreinfPromedio,0)-20
  factorModificadorBeta = NULL
  for(t in 2:dias){
    print(t)
    # contagiados según matriz de contacto
    beta       = contact_matrix * transmission_probability
    I_edad = colSums(I[[t-1]])
    N_edad = colSums(N)
    
    if (t<tHoy){
    
      #e[[t-1]][1,] = expuestos_reales[t,]
      e[[t-1]][1,] = defunciones_reales[t+17+round(periodoPreinfPromedio,0),] / ifr
      
    } else {
      if (is.null(factorModificadorBeta)) {
        factorModificadorBeta = calcularFactorModificadorBeta()
      }
      e[[t-1]] = S[[t-1]] * matrix((1.12 * beta) %*% I_edad/N_edad, nrow=length(immunityStates), length(ageGroups),byrow = T) * modif_beta
      e[[t-1]]    =  pmin(e[[t-1]], S[[t-1]]) # no negativo
    }
    
    # resto seir
    E[[t]]      = E[[t-1]] + e[[t-1]] - E[[t-1]]/duracionE
    
    i[[t]]      = E[[t-1]]/duracionE
    Ii[[t]]     = Ii[[t-1]] + i[[t]] - Ii[[t-1]]/duracionIi
    
    Ig[[t]]     = Ig[[t-1]] - Ig[[t-1]]/duracionIg + Ii[[t-1]]/duracionIi*porc_gr*modif_porc_gr
    
    Ic[[t]]     = Ic[[t-1]] - Ic[[t-1]]/duracionIc + Ii[[t-1]]/duracionIi*porc_cr*modif_porc_cr
    I[[t]]      = Ii[[t]] + Ig[[t]] + Ic[[t]]
    #browser(expr = {t=200})
    if (t<tHoy){
      d[[t]][1,] = defunciones_reales[t,]
      
    } else {
      d[[t]]      = Ic[[t-1]]/duracionIc * (ifr *1.2) * modif_ifr/porc_cr*modif_porc_cr # siendo ifr = d[t]/i[t-duracionIi-duracionIc]
      
    }
    D[[t]]      = D[[t-1]] + d[[t]]
    u[[t]]      = Ii[[t-1]]/duracionIi*(1-porc_gr*modif_porc_gr-porc_cr*modif_porc_cr) + Ig[[t-1]]/duracionIg + Ic[[t-1]]/duracionIc * (1-ifr/porc_cr*modif_porc_cr)
    U[[t]]      = U[[t-1]] + u[[t]]
    R[[t]]      = U[[t]] + D[[t]]
    # Ajustes de pasajes S-V-S y U-S
    Vin = VquedaEnS =  matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = names)
    
    Vout <- matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = names)
    
    vacunasDelDia <- matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                          dimnames = names)
    
    # Check si hay suficientes S para vacunar hoy
    for (vacuna in c(3:nrow(paramVac))) {
      latencia = paramVac[vacuna,1]
      if (t>latencia) {
        vacunasDelDia[vacuna,]=Av[[t-latencia]][vacuna,]
      }
    }
    
    haySparaVacunar = S[[t-1]][1,] > colSums(vacunasDelDia)
    
    for (vacuna in c(3:nrow(paramVac))) {
      latencia = paramVac[vacuna,1]
      #vacunadosTotales = colSums(Av[[t-latencia]])
      porcV = paramVac[vacuna,2]
      tiempoV = paramVac[vacuna,3]
      porcProt = paramVac[vacuna,4]
      vacunadosVacunaDia = 0
      # print(haySparaVacunar)
      if (t > latencia & all(haySparaVacunar)) {
        # Los que pasan a V
        vacunadosVacunaDia = Av[[t-latencia]][vacuna,]
        Vin[vacuna,] =  vacunadosVacunaDia * porcV
        
        # Los que se quedan en S pero protegidos (deben cambiar de renglon)
        vacunadosVacunaDia = Av[[t-latencia]][vacuna,]
        VquedaEnS[vacuna,] =  vacunadosVacunaDia * porcProt
        
        # Los que no les hace ningun efecto y quedan en S como no inmunes, no hay que hacer nada
        
      }
      if (t>tiempoV+1) {
        Vout[vacuna,] <- v[[t-tiempoV]][vacuna,]
      }
      V[[t]][vacuna,] = V[[t-1]][vacuna,] + Vin[vacuna,] - Vout[vacuna,]
    }
    
    v[[t]] = Vin
      
    Vsum <- matrix(data=0, nrow = length(immunityStates), ncol = length(ageGroups), byrow = T)
    Vsum[1,] <- colSums(V[[t]])
    S[[t]]      = N - E[[t]] - I[[t]] - R[[t]] - Vsum
    # Transicion U -> S
    if (t>duracion_inmunidad+1) {
      S[[t]][2,]  = S[[t]][2,] + colSums(u[[t-duracion_inmunidad]])
      U[[t]] = U[[t]] - u[[t-duracion_inmunidad]]
      R[[t]] = U[[t]] + D[[t]]
    }
    # S[[t]][1,] = S[[t]][1,] - colSums(Vout)
    S[[t]]=S[[t]]+Vout
    # Reasigno de renglon los que quedaron en S luego de vacunarse
    S[[t]][1,]=S[[t]][1,] - colSums(VquedaEnS)
    S[[t]] =  S[[t]] + VquedaEnS
    # Pendiente: Pasar de renglon a "No inmunes" a los vacunados que vencen (tiempoP, index 5 en paramvac)
    # 
  }
  
  # org rr
  salida <- list(S,V,E,e,I,i,Ig,Ic,U,u,D,d,R, defunciones_reales)
  names(salida) <- c("S","V","E","e","I","i","Ig","Ic","U","u","D","d","R", "defunciones_reales")
  out <- salida
  # out <- bind_rows(
  #   tibble(Compart = "S", do.call(rbind, lapply(S,colSums)) %>% as_tibble()),
  #   tibble(Compart = "V", do.call(rbind, lapply(V,colSums)) %>% as_tibble()),
  #   tibble(Compart = "E", do.call(rbind, lapply(E,colSums)) %>% as_tibble()),
  #   tibble(Compart = "e", do.call(rbind, lapply(e,colSums)) %>% as_tibble()),
  #   tibble(Compart = "I", do.call(rbind, lapply(I,colSums)) %>% as_tibble()),
  #   tibble(Compart = "Ic", do.call(rbind, lapply(Ic,colSums)) %>% as_tibble()),
  #   tibble(Compart = "i", do.call(rbind, lapply(i,colSums)) %>% as_tibble()),
  #   tibble(Compart = "D", do.call(rbind, lapply(D,colSums)) %>% as_tibble()),
  #   tibble(Compart = "d", do.call(rbind, lapply(d,colSums)) %>% as_tibble()),
  #   tibble(Compart = "R", do.call(rbind, lapply(R,colSums)) %>% as_tibble())) %>%
  #   dplyr::mutate(fecha = rep(1:dias,10)) %>% 
  #   dplyr::rename("0-19"=2,"20-64"=3,"65+"=4)
  out  
}

calcularFactorModificadorBeta = function() {
  # Calcular Rcori
  Rt = 1.12
  # Calcular Modificador con NGM
  # mod = get_factor_given_rt(contact_matrix, transmission_probability, duracionI, porc_S, Rt)
  factorModificadorBeta = 1.12
  return(factorModificadorBeta)
}

get_R0_given_cm = function(contact_matrix, transmission_probability,duracionI){
  # matrices de duración y transmisión (no depende de la edad del infectado, solo del susceptible)
  transmission_probability <- diag(transmission_probability, 3)
  duracionI <- diag(duracionI, 3)
  # next generation matrix
  NGM <- transmission_probability %*% contact_matrix %*% duracionI
  # autovalor dominante es R0
  R0  <- abs(eigen(NGM)$values[1])
  return(R0)
}

get_factor_given_rt = function(contact_matrix, transmission_probability, duracionI, porc_S, Rt){
  # R0 teórico inicial
  R0 <- get_R0_given_cm(contact_matrix, transmission_probability,duracionI)
  # R0 implícito en lo observado (era asi????)
  R0_equivalent <- Rt/porc_S
  # aca esto lo simple
  factor <- R0_equivalent/R0
  # nueva mc
  contact_matrix_equivalent <- contact_matrix * factor
  return(list(contact_matrix_equivalent = contact_matrix_equivalent,
              factor = factor))
}

# graficar
# do.call(rbind, lapply(S,colSums))[,2]


# seir_ages <- function(dias,
#                       duracionE, 
#                       duracionIi, porc_gr, duracionIg, porc_cr, duracionIc,
#                       ifr = c(.03,.03,.03),
#                       vacunados = c(0,0,0),
#                       contact_matrix,
#                       transmission_probability,
#                       N = c(1/3,1/3,1/3), 
#                       zero_cases = c(0,1/45e6*N,0)){
#   
#   # cada columna es un grupo
#   e = E = S =  i = I = Ii = Ig = Ic = r = R = D = d = U = beta = matrix(0,dias,3)
#   # zero cases
#   I[1,] = zero_cases
#   S[1,] = N - zero_cases
#   # efecto vacunas
#   porc_gr = porc_gr * (1-vacunados)
#   porc_cr = porc_cr * (1-vacunados)
#   
#   # seir
#   for(t in 2:dias){
#     
#     # contagiados según matriz de contacto
#     beta       = contact_matrix * transmission_probability
#     e[[t-1]]    = S[[t-1]] * (beta %*% I[[t-1]]/N)
#     e[[t-1]]    =  pmin(e[[t-1]], S[[t-1]]) # no negativo
#     
#     # resto seir
#     E[[t]]      = E[[t-1]] + e[[t-1]] - E[[t-1]]/duracionE
#     i[[t]]      = E[[t-1]]/duracionE
#     Ii[[t]]     = Ii[[t-1]] + i[[t]] - Ii[[t-1]]/duracionIi
#     Ig[[t]]     = Ig[[t-1]] - Ig[[t-1]]/duracionIg + Ii[[t-1]]/duracionIi*porc_gr
#     Ic[[t]]     = Ic[[t-1]] - Ic[[t-1]]/duracionIc + Ii[[t-1]]/duracionIi*porc_cr
#     I[[t]]      = Ii[[t]] + Ig[[t]] + Ic[[t]]
#     d[[t]]      = Ic[[t-1]]/duracionIc * ifr/porc_cr # siendo ifr = d[t]/i[t-duracionIi-duracionIc]
#     D[[t]]      = D[[t-1]] + d[[t]]
#     U[[t]]      = U[[t-1]] + Ii[[t-1]]/duracionIi*(1-porc_gr-porc_cr) + Ig[[t-1]]/duracionIg + Ic[[t-1]]/duracionIc * (1-ifr/porc_cr)
#     R[[t]]      = U[[t]] + D[[t]]
#     S[[t]]      = N - E[[t]] - I[[t]] - R[[t]]
#   }
#   results = list(g1 = data.frame(fecha = 1:dias, S=S[,1], E=E[,1], I=I[,1], D=D[,1],R=R[,1]),
#                  g2 = data.frame(fecha = 1:dias, S=S[,2], E=E[,2], I=I[,2], D=D[,2],R=R[,2]),
#                  g3 = data.frame(fecha = 1:dias, S=S[,3], E=E[,3], I=I[,3], D=D[,3],R=R[,3]))
#   return(bind_rows(
#     tibble(group = "g1", results[["g1"]]),
#     tibble(group = "g2", results[["g2"]]),
#     tibble(group = "g3", results[["g3"]]))
#   )
# }
# 
