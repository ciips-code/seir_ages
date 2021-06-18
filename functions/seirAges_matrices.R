seir_ages <- function(dias,
                      duracionE, 
                      duracionIi, 
                      porc_gr, 
                      duracionIg, 
                      porc_cr, 
                      duracionIc,
                      ifr,
                      vacunados,
                      contact_matrix,
                      transmission_probability,
                      N, 
                      modif_beta,
                      modif_porc_gr,
                      modif_porc_cr,
                      modif_ifr,
                      duracion_inmunidad,
                      defunciones_reales,
                      Av,
                      immunityStates,
                      ageGroups,
                      paramVac
){
  ifrm = matrix(rep(ifr,length(immunityStates)),length(immunityStates),3,byrow = T)
  names = list(immunityStates,
               ageGroups)
  # cada columna es un grupo
  e = E = S = i = Ss = I = Ii = Ig = Ic = r = R = D = d = U = u = V = v = beta = lapply(1:dias, matrix, data= 0, nrow=length(immunityStates), ncol=length(ageGroups), dimnames = names)
  
  S[[1]] = matrix(c(N[1],N[2],N[3],0,0,0,0,0,0,0,0,0),length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = names)
  
  N = matrix(c(N[1],N[2],N[3],0,0,0,0,0,0,0,0,0),length(immunityStates),length(ageGroups), byrow = T,
               dimnames = names)
  
  I[[1]] = matrix(0,length(immunityStates),length(ageGroups), byrow = T,
                    dimnames = names)
  I[[1]][1,2] = 1 # La semilla del primer infectado
  
  # seir
  # tHoy = cantidad de dias con muertes reales - 17 dias de inf - 5 dias de preinf - 7 dias de ajuste por retrasos en la notificacion
  tHoy <<- nrow(defunciones_reales)-17-round(periodoPreinfPromedio,0)
  factorModificadorBeta = NULL
  for(t in 2:dias){
    # print(t)
    # contagiados según matriz de contacto
    beta       = contact_matrix * transmission_probability
    I_edad = colSums(I[[t-1]])
    N_edad = colSums(N)
    if (t<tHoy){
      expuestosTotalesHoy = defunciones_reales[t+17+round(periodoPreinfPromedio,0),] / ifr
      # distribuir segun filas en S[[t-1]]
      a = matrix(rbind(rep(expuestosTotalesHoy,3)),length(immunityStates),length(ageGroups), byrow = T,
                 dimnames = names)
      b = S[[t-1]]
      b = pmax(b,0)
      c = matrix(rbind(rep(colSums(b),3)),length(immunityStates),length(ageGroups), byrow = T,
                 dimnames = names)
      e[[t-1]] = a*b/c
      e[[t-1]] <- ifelse(is.na(e[[t-1]]),0,e[[t-1]])
      # e[[t-1]][1,] = expuestosTotalesHoy
      e[[t-1]] <- ifelse(e[[t-1]]<0.1,0,e[[t-1]])
    } else {
      if (is.null(factorModificadorBeta)) {
        ten_days_incidents = sapply((t-10):(t-1), function(s) {sum(i[[s]])})
        porc_S = sum(S[[t-1]])/sum(N)
        factorModificadorBeta = calcularFactorModificadorBeta(ten_days_incidents,
                                                              contact_matrix,
                                                              diag(transmission_probability),
                                                              porc_S,
                                                              duracionIi)
      }
      e[[t-1]] = S[[t-1]] * matrix((factorModificadorBeta$factor * beta) %*% I_edad/N_edad, 
                                   nrow=length(immunityStates), length(ageGroups),byrow = T) * modif_beta
      e[[t-1]] <- ifelse(e[[t-1]]<0.1,0,e[[t-1]])
    }
    
    # resto seir
    E[[t]]      = E[[t-1]] + e[[t-1]] - E[[t-1]]/duracionE
    
    i[[t]]      = E[[t-1]]/duracionE
    Ii[[t]]     = Ii[[t-1]] + i[[t]] - Ii[[t-1]]/duracionIi
    
    Ig[[t]]     = Ig[[t-1]] - Ig[[t-1]]/duracionIg + Ii[[t-1]]/duracionIi*porc_gr*modif_porc_gr
    
    Ic[[t]]     = Ic[[t-1]] - Ic[[t-1]]/duracionIc + Ii[[t-1]]/duracionIi*porc_cr*modif_porc_cr
    I[[t]]      = Ii[[t]] + Ig[[t]] + Ic[[t]]
    if (t<tHoy){
      d[[t]][1,] = defunciones_reales[t,]
    } else {
      d[[t]]      = Ic[[t-1]]/duracionIc * (ifrm) * modif_ifr/porc_cr*modif_porc_cr # siendo ifr = d[t]/i[t-duracionIi-duracionIc]
    }
    D[[t]]      = D[[t-1]] + d[[t]]
    u[[t]]      = Ii[[t-1]]/duracionIi * (1-porc_gr*modif_porc_gr-porc_cr*modif_porc_cr) +
                  Ig[[t-1]]/duracionIg +
                  Ic[[t-1]]/duracionIc * (1-ifr/porc_cr*modif_porc_cr)
    U[[t]]      = U[[t-1]] + u[[t]]
    R[[t]]      = U[[t]] + D[[t]]
    
    
    # Pasajes S-V-S y U-S
    Vin = VquedaEnS =  Vout = vacunasDelDia = Vsum = matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = names)
    
    # Check si hay suficientes S para vacunar hoy
    for (vacuna in c(3:nrow(paramVac))) {
      latencia = paramVac[vacuna,1]
      if (t>latencia) {
        vacunasDelDia[vacuna,]=Av[[t-latencia]][vacuna,]
      }
    }
    # TODO: No esta vacunando recuperados
    haySparaVacunar = S[[t-1]][1,] > colSums(vacunasDelDia)
    
    for (vacuna in c(3:nrow(paramVac))) {
      latencia = paramVac[vacuna,1]
      porcV = paramVac[vacuna,2]
      tiempoV = paramVac[vacuna,3]
      porcProt = paramVac[vacuna,4]
      # print(haySparaVacunar)
      if (t > latencia & all(haySparaVacunar)) {
        vacunadosVacunaDia = Av[[t-latencia]][vacuna,]
        # Los que pasan a V
        Vin[vacuna,] =  vacunadosVacunaDia * porcV
        # Los que se quedan en S pero protegidos (deben cambiar de renglon)
        VquedaEnS[vacuna,] =  vacunadosVacunaDia * porcProt
        # Los que no les hace ningun efecto y quedan en S como no inmunes, no hay que hacer nada
      }
      if (t>tiempoV+1) {
        Vout[vacuna,] <- v[[t-tiempoV]][vacuna,]
      }
      V[[t]][vacuna,] = V[[t-1]][vacuna,] + Vin[vacuna,] - Vout[vacuna,]
    }
    v[[t]] = Vin
    # Vsum tiene todos los V agrupados en la primera linea para hacer la resta de todos los compartimentos
    Vsum[1,] <- colSums(V[[t]])
    # Empiezo a armar el S con el resto de todos los que estan en otros compartimentos
    S[[t]][1,] = colSums(N) - colSums(E[[t]]) - colSums(I[[t]]) - colSums(R[[t]]) - colSums(V[[t]])
    # los recuperados de hoy son los recuperados de ayer menos los que se expusieron hoy
    S[[t]][2,] = S[[t-1]][2,] - e[[t]][2,]
    # Transicion U -> S, sumamos los recuperados que pierden inmunidad hoy (U-S)
    if (t>duracion_inmunidad+1) {
      S[[t]][2,]  = S[[t]][2,] + colSums(u[[t-duracion_inmunidad]])
      U[[t]] = U[[t]] - u[[t-duracion_inmunidad]]
      R[[t]] = U[[t]] + D[[t]]
    }
    # restamos todos los recuperados de la S[1,]
    S[[t]][1,] = S[[t]][1,] - S[[t]][2,]
    # sacamos los que salienron de V y tienen que pasar a los S[3:n,]    
    S[[t]][1,] = S[[t]][1,] - colSums(Vout)
    S[[t]]=S[[t]]+Vout
    # Reasigno de renglon los que quedaron hoy en S luego de vacunarse
    S[[t]][1,]=S[[t]][1,] - colSums(VquedaEnS)
    S[[t]] =  S[[t]] + VquedaEnS
    # busco los que ayer estaban protegidos (en s[3:n,]) y los reasigno de s[1,]
    protegidosAyer = S[[t-1]]
    protegidosAyer[1,] = 0 # borro no inmunes
    protegidosAyer[2,] = 0 # borro recuperados
    S[[t]][1,]=S[[t]][1,] - colSums(protegidosAyer)
    S[[t]] =  S[[t]] + protegidosAyer
    # Pendiente: Pasar de renglon a "No inmunes" a los vacunados que vencen (tiempoP, index 5 en paramvac)
    # Borrador
    if (t>181) {
      S[[t]][1,] = S[[t]][1,] + colSums(v[[t-180]])
      S[[t]] = S[[t]] - v[[t-180]]
    }
    # Saca negativos
    # S[[t]] <- ifelse(S[[t]]<0.1,0,S[[t]])
  }
  salida <- list(S,V,E,e,I,Ii,i,Ig,Ic,U,u,D,d,R, defunciones_reales)
  names(salida) <- c("S","V","E","e","I","Ii","i","Ig","Ic","U","u","D","d","R", "defunciones_reales")
  return(salida) 
}

calcularFactorModificadorBeta = function(ten_days_incidents, contact_matrix, transmission_probability, porc_S,duracionI) {
  # Calcular Rcori
  Rt_estimate <- estimate_R(ten_days_incidents, method = "parametric_si",
                            config = make_config(list(mean_si = 3, std_si = 4)))$R # parámetros de biblio
  Rt <- Rt_estimate$`Median(R)`[nrow(Rt_estimate)]
  # Calcular Modificador con NGM
  factorModificadorBeta = get_factor_given_rt(contact_matrix, transmission_probability, duracionI, porc_S, Rt)
  # factorModificadorBeta = 1.12
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
  R0 <- get_R0_given_cm(contact_matrix, transmission_probability, duracionI)
  # R0 implícito en lo observado
  R0_equivalent <- Rt/porc_S
  # aca esto lo simple
  factor <- R0_equivalent/R0
  # nueva mc
  contact_matrix_equivalent <- contact_matrix * factor
  return(list(contact_matrix_equivalent = contact_matrix_equivalent,
              factor = factor))
}