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
                      planVacunacionFinal,
                      immunityStates,
                      ageGroups,
                      paramVac,
                      tVacunasCero,
                      relaxNpi,
                      relaxGoal,
                      relaxFactor
){
  #browser()
  ifrm = matrix(rep(ifr,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)
  names = list(immunityStates,
               ageGroups)
  # cada columna es un grupo
  e = E = S = i = Ss = I = Ii = Ig = Ic = r = R = D = d = U = u = V = v = vA = beta = tot = lapply(1:dias, 
                                                                                        matrix, 
                                                                                        data= 0, 
                                                                                        nrow=length(immunityStates), 
                                                                                        ncol=length(ageGroups), 
                                                                                        dimnames = names)
        
  S[[1]][1,] = N
  
  N = S[[1]]
  
  I[[1]] = matrix(0,length(immunityStates),length(ageGroups), byrow = T,
                    dimnames = names)
  I[[1]][1,2] = 1 # La semilla del primer infectado
  
  # seir
  # tHoy = cantidad de dias con muertes reales - 17 dias de inf - 5 dias de preinf - 7 dias de ajuste por retrasos en la notificacion
  #tHoy <<- nrow(defunciones_reales)-17-round(periodoPreinfPromedio,0)
  tHoy <<- tVacunasCero
  factorModificadorBeta = NULL
  relaxValue = rep(1,dias)
  if (relaxNpi) {
    vectorRelajado = rep(1,relaxGoal-tHoy)
    dailyRelax = relaxFactor / (relaxGoal-tHoy)
    relaxSum = 0
    for(ri in c(1:length(vectorRelajado))) {
      relaxSum = relaxSum + dailyRelax
      vectorRelajado[ri] = vectorRelajado[ri]+relaxSum
    }
    relaxValue[tHoy:(relaxGoal-1)] = vectorRelajado
    relaxValue[relaxGoal:length(relaxValue)] = 1 + relaxFactor
  }
  for(t in 2:dias){
    # if(t==448){browser()}
    # print(t)
    # contagiados según matriz de contacto
    beta       = contact_matrix * transmission_probability
    I_edad = colSums(I[[t-1]])
    N_edad = colSums(N)
    if (t<tHoy){
      expuestosTotalesHoy = as.numeric(defunciones_reales[t+17+round(periodoPreinfPromedio,0),]) / ifr
      # distribuir segun filas en S[[t-1]]
      apor = matrix(rbind(rep(expuestosTotalesHoy,3)),length(immunityStates),length(ageGroups), byrow = T,
                 dimnames = names)
      bpor <- S[[t-1]]
      bpor <- pmax(bpor,0)
      cpor <- matrix(rbind(rep(colSums(bpor),3)),length(immunityStates),length(ageGroups), byrow = T,
                 dimnames = names)
      e[[t-1]] = apor*bpor/cpor
      e[[t-1]] <- ifelse(is.na(e[[t-1]]),0,e[[t-1]])
      # e[[t-1]][1,] = expuestosTotalesHoy
      e[[t-1]] <- ifelse(e[[t-1]]<0.1,0,e[[t-1]])
    } else {
      # if (is.null(factorModificadorBeta)) {
      #   ten_days_incidents = sapply((t-10):(t-1), function(s) {sum(i[[s]])})
      #   porc_S = sum(S[[t-1]])/sum(N)
      #   factorModificadorBeta = calcularFactorModificadorBeta(ten_days_incidents,
      #                                                         contact_matrix,
      #                                                         diag(transmission_probability),
      #                                                         porc_S,
      #                                                         duracionIi)
      # }
      # e[[t-1]] = S[[t-1]] * matrix((factorModificadorBeta$factor * beta) %*% I_edad/N_edad, 
      #                              nrow=length(immunityStates), length(ageGroups),byrow = T) * modif_beta
      # e[[t-1]] <- ifelse(e[[t-1]]<0.1,0,e[[t-1]])
      beta       = contact_matrix * transmission_probability * relaxValue[t]
      e[[t-1]] = S[[t-1]] * matrix((1.12 * beta) %*% I_edad/N_edad, nrow=length(immunityStates), length(ageGroups),byrow = T) * modif_beta
    }
    
    # resto seir
    E[[t]]      = E[[t-1]] + e[[t-1]] - E[[t-1]]/duracionE
    i[[t]]      = E[[t-1]]/duracionE
    Ii[[t]]     = Ii[[t-1]] + i[[t-1]] - Ii[[t-1]]/duracionIi
    
    Ig[[t]]     = Ig[[t-1]] - Ig[[t-1]]/duracionIg + Ii[[t-1]]/duracionIi*porc_gr*modif_porc_gr
    
    Ic[[t]]     = Ic[[t-1]] - Ic[[t-1]]/duracionIc + Ii[[t-1]]/duracionIi*porc_cr*modif_porc_cr
    I[[t]]      = Ii[[t]] + Ig[[t]] + Ic[[t]]
    if (t<tHoy){
      d[[t]][1,] = as.numeric(defunciones_reales[t,])
    } else {
      d[[t]]      = Ic[[t-1]]/duracionIc * (ifrm) * modif_ifr/porc_cr*modif_porc_cr # siendo ifr = d[t]/i[t-duracionIi-duracionIc]
    }
    D[[t]]      = D[[t-1]] + d[[t-1]]
    u[[t]]      = Ii[[t-1]]/duracionIi * (1-porc_gr*modif_porc_gr-porc_cr*modif_porc_cr) +
                  Ig[[t-1]]/duracionIg +
                  Ic[[t-1]]/duracionIc * (1-ifr/porc_cr*modif_porc_cr)
    U[[t]]      = U[[t-1]] + u[[t-1]]
    # Transicion U -> S, sumamos los recuperados que pierden inmunidad hoy (U-S)
    losQueHoyPierdenImunidad = matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                                      dimnames = names)
    if (t>duracion_inmunidad+1) {
      losQueHoyPierdenImunidad = u[[t-duracion_inmunidad]]
      U[[t]] = U[[t]] - losQueHoyPierdenImunidad
    }
    R[[t]]      = U[[t]] + D[[t]]
    
    # Pasajes S-V-S y U-S
    Vin = VquedaEnS =  Vout = vacunasDelDia = vacunasDelDia2 = vacunadosVacunaDia = vacunadosVacunaDia2 = matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = names)
    
    # Arma la lista de vacunas
    for (vacuna in c(3:nrow(paramVac))) {
      latencia = as.numeric(paramVac[vacuna,1])
      if (t>latencia) {
        vacunasDelDia[vacuna,]=planVacunacionFinal[[t-latencia]][vacuna,]
      }
    }

    # Primera dosis
    # TODO: No esta vacunando recuperados / Hard Coded para solo 3,4
    haySparaVacunar = S[[t-1]][1,] > vacunasDelDia[3,]
    haySparaVacunar[is.na(haySparaVacunar)] <- FALSE
    # for (vacuna in c(3:nrow(paramVac))) {
    # Aplica primera dosis
    vacuna = 3
    latencia = as.numeric(paramVac[vacuna,1])
    porcV = as.numeric(paramVac[vacuna,2])
    tiempoV = as.numeric(paramVac[vacuna,3])
    porcProt = as.numeric(paramVac[vacuna,4])
    if (t > latencia) {
      for (iAge in c(1:length(ageGroups))) {
        if (vacunasDelDia[vacuna,iAge] < S[[t-1]][1,iAge]) {
          vacunadosVacunaDia[vacuna,iAge] = vacunasDelDia[vacuna,iAge]
        } else {
          vacunadosVacunaDia[vacuna,iAge] = 0
        }
      }
      vA[[t]][vacuna,] = vacunadosVacunaDia[vacuna,]
      # Los que pasan a V
      Vin[vacuna,] =  vacunadosVacunaDia[vacuna,] * porcV
      # Los que se quedan en S pero protegidos (deben cambiar de renglon)
      VquedaEnS[vacuna,] =  vacunadosVacunaDia[vacuna,] * porcProt
      # Los que no les hace ningun efecto y quedan en S como no inmunes, no hay que hacer nada
    }
    # TODO: Segunda dosis
    #  - La segunda dosis se aplica a la linea 3 en S y en V, y la 1 en V
    #  - Dosis para V3 =  CantDia * porcV
    #  - Dosis para S3 =  CantDia * porcProt
    #  - Dosis para S1 =  CantDia * 1-porcV-porcProt
    #  - Dosis para V3: Se pasan de 3 a 4
    #  - Dosis para S3: Se pasan de 3 a 4
    #  - Dosis para S1: Se quedan en S1
    #  - Con esto no estamos prolongando los tiempos
    #  - Se mantienen los efectos de la primera dosis, el que paso a V se queda, el que tuvo proteccion se queda, etc
    segundasDosisHoyV3 = vacunasDelDia[4,] * porcV
    segundasDosisHoyS3 = vacunasDelDia[4,] * porcProt
    # segundasDosisHoyS1 no se calcula
    # Salida de V
    if (t>tiempoV+1) {
      totSalidaVHoy = v[[t-tiempoV]][vacuna,]
      # reasignar entre 3 y 4 de acuerdo a proporciones
      porc3 = V[[t-1]][3,] / (V[[t-1]][3,] + V[[t-1]][4,])
      porc3[is.na(porc3)] <- 0
      totSalidaVHoy3 = totSalidaVHoy * porc3
      porc4 = V[[t-1]][4,] / (V[[t-1]][3,] + V[[t-1]][4,])
      porc4[is.na(porc4)] <- 0
      totSalidaVHoy4 = totSalidaVHoy * porc4
      Vout[3,] <- totSalidaVHoy3
      Vout[4,] <- totSalidaVHoy4
    }
    V[[t]][3,] = V[[t-1]][3,] + Vin[3,] - Vout[3,]
    V[[t]][4,] = V[[t-1]][4,] - Vout[4,]
    # browser(expr = {t > 322})
    for (iAge in c(1:length(ageGroups))) {
      if (segundasDosisHoyV3[iAge] < V[[t]][3,iAge]) {
        V[[t]][3,iAge] = V[[t]][3,iAge] - segundasDosisHoyV3[iAge]
        V[[t]][4,iAge] = V[[t]][4,iAge] + segundasDosisHoyV3[iAge]
      }
    }
    # browser(expr = {t > 322})
    v[[t]] = Vin
    # Empiezo a armar el S a partir de las transiciones
    S[[t]] = S[[t-1]] - e[[t-1]]  + Vout
    S[[t]][1,] = S[[t]][1,] - colSums(Vin)
    S[[t]][2,] = S[[t]][2,] + colSums(losQueHoyPierdenImunidad)
    # 
    # # Reasigno de renglon los que quedaron hoy en S luego de vacunarse
    S[[t]][1,]=S[[t]][1,] - colSums(VquedaEnS)
    S[[t]] =  S[[t]] + VquedaEnS
    for (iAge in c(1:length(ageGroups))) {
      if (segundasDosisHoyS3[iAge] < S[[t]][3,iAge]) {
        S[[t]][3,iAge] = S[[t]][3,iAge] - segundasDosisHoyS3[iAge]
        S[[t]][4,iAge] = S[[t]][4,iAge] + segundasDosisHoyS3[iAge]
      }
    }
    
    # S[[t]][1,] = colSums(E[[t]]) - colSums(I[[t]]) - colSums(R[[t]]) - colSums(V[[t]])
    # los recuperados de hoy son los recuperados de ayer menos los que se expusieron hoy
   
    # Aplica Segunda dosis
    # - Calcula las cantidades de segundas dosis que se hacen efectivas hoy
    # - La matriz de segundas dosis tiene que venir en la fila correcta (ej. 4)
    # - Suma la matriz
    # - Resta las segundas de las primeras, segun una tabla de equivalencias, y comprobando si no da negativo
    #
    # for (vacuna in c(3:nrow(paramVac))) {
    #   latencia = paramVac[vacuna,1]
    #   porcV = paramVac[vacuna,2]
    #   porcProt = paramVac[vacuna,4]
    #   if (t > latencia) {
    #     for (iAge in c(1:length(ageGroups))) {
    #       if (vacunasDelDia2[vacuna,iAge] < S[[t-1]][1,iAge]) {
    #         vacunadosVacunaDia2[vacuna,iAge] = vacunasDelDia2[vacuna,iAge]
    #       } else {
    #         vacunadosVacunaDia2[vacuna,iAge] = 0
    #       }
    #     }
    #   }
    # }
    
    # Pasar de renglon a "No inmunes" a los vacunados que vencen (tiempoP, index 5 en paramvac)
    # Vuelve a No inmunes los que terminan su tiempo de proteccion
    for (vacuna in c(3:nrow(paramVac))) {
      tiempoP = as.numeric(paramVac[vacuna,5])
      S[[t]][vacuna,] =  S[[t]][vacuna,] - S[[t]][vacuna,]/tiempoP
      S[[t]][1,]=S[[t]][1,] + S[[t]][vacuna,]/tiempoP
    }
    
    
    
    # Corrigiendo los negativos generados por la reasignación de renglones
    # if(any(S[[t]]<0)){browser()}
    # for (ixx in seq_along(ageGroups)) {
    #   if (S[[t]][1,ixx] < 0) {
    #     S[[t]][2,ixx] = S[[t]][2,ixx] + S[[t]][1,ixx]
    #     S[[t]][1,ixx] = 0
    #   }
    # }
    
    tot[[t]] = S[[t]] + V[[t]] + E[[t]] + I[[t]] + R[[t]]
    # browser()
    # Diferencias: print(paste0(t, ", ", sum(tot[[t]])-sum(N)))
  }
  salida <- list("S: Susceptible"=S,
                 "V: Vaccinated"=V,
                 "vA: Daily vaccinations"=vA,
                 "E: Exposed"=E,
                 "e: Daily exposed"=e,
                 "I: Infectious"=I,
                 "i: Daily infectious"=i,
                 "Ii: Infectious (mild)"=Ii,
                 "Ig: Infectious (moderate)"=Ig,
                 "Ic: Infectious (severe)"=Ic,
                 "R: Recovered (survivors + deaths)"=R,
                 "U: Survivors"=U,
                 "u: Daily survivors"=u,
                 "D: Deaths"=D,
                 "d: Daily deaths"=d)
  #names(salida) <- c("S","V","E","e","I","Ii","i","Ig","Ic","U","u","D","d","R","v","vA")
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
  transmission_probability <- diag(transmission_probability)
  duracionI <- diag(duracionI, ncol(transmission_probability))
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

# get contact matrix from covoid study: library(covoid) 
get_empirical_cm <- function(country, ages, type = "general"){
  # get matrix from covoid package
  mc_arg  <- as.data.frame(t(import_contact_matrix(country = country,setting = type)))
  pop_arg <- data.frame(pop = as.numeric(import_age_distribution(country)), group = rownames(mc_arg))
  # some inconsistencies in covoid package for Argentina
  # plot(mc_arg[,"5"]*pop_arg[pop_arg$group=="5","pop"],mc_arg[,"80"]*pop_arg[pop_arg$group=="80","pop"])

  # rebuild given ages
  age_il <- ages
  contact_matrix <-
    pivot_longer(mc_arg %>%
                   mutate(to_group = rownames(mc_arg)),
                 cols=1:ncol(mc_arg),names_to = "group") %>%
    left_join(pop_arg) %>%
    mutate(group = as.integer(group), to_group = as.integer(to_group),
           new_group = cut(group, breaks = c(age_il-1,100)),
           new_to_group = cut(to_group, breaks = c(age_il-1,100))) %>%
    group_by(new_group, new_to_group) %>%
    summarise(new_contacts = sum(pop*value)/sum(pop)) %>%
    pivot_wider(names_from = "new_group", values_from = "new_contacts") %>% ungroup() %>%
    select(-1) %>% as.matrix()
  rownames(contact_matrix) <-  colnames(contact_matrix) <- age_il
  contact_matrix
}

# weight contact matrix by place of contact, given Davis matrix scenario. https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(20)30133-X/fulltext
# Shortcut here: infectiouness modifications (65%) are applied directly to contact matrix because will multiply after the transmission parameter to do beta
get_npi_cm_scenario <- function(scenario = "Baseline", 
                             matrix_list = NULL,
                             ages= c(0, 18, 30, 40, 50, 60, 70, 80)){
  
  list2env(matrix_list, .GlobalEnv)
  # columns of olders 
  cols_70_older <- which((ages)>=70)
  # scenarios
  if(scenario == "Baseline"){
    out <- contact_matrix
  }
  if(scenario == "School closures"){
    out <- 1 * contact_matrix_home + 1 * contact_matrix_work + 0 * contact_matrix_school + 1 * contact_matrix_other
  }
  if(scenario == "Physical distancing"){
    out <- 1 * contact_matrix_home + .5 * contact_matrix_work + 1 * contact_matrix_school + .5 * contact_matrix_other
  }
  if(scenario == "Shielding of older people"){
    out <- 1 * contact_matrix_home   + .25 * cbind(contact_matrix_work [,cols_70_older] + contact_matrix_work [,-cols_70_older]) +
           1 * contact_matrix_school + .25 * cbind(contact_matrix_other[,cols_70_older] + contact_matrix_other[,-cols_70_older])
  }
  if(scenario == "Self-isolation"){
    out <- contact_matrix * .65
  }
  if(scenario == "Combined"){
    out <- 1 * contact_matrix_home   + .25 * cbind(contact_matrix_work [,cols_70_older] + .5 * contact_matrix_work [,-cols_70_older]) +
           0 * contact_matrix_school + .25 * cbind(contact_matrix_other[,cols_70_older] + .5 * contact_matrix_other[,-cols_70_older])
    out <- out * .65 
  }
  if(scenario == "Intensive interventions with schools closed"){
    out <- 1 * contact_matrix_home   + .25 * cbind(contact_matrix_work [,cols_70_older] + .65 * contact_matrix_work [,-cols_70_older]) +
           0 * contact_matrix_school + .16 * cbind(contact_matrix_other[,cols_70_older] + .59 * contact_matrix_other[,-cols_70_older])
    out <- out * .65 
  }
  if(scenario == "Intensive interventions with schools open"){
    out <- 1 * contact_matrix_home   + .25 * cbind(contact_matrix_work [,cols_70_older] + .65 * contact_matrix_work [,-cols_70_older]) +
           1 * contact_matrix_school + .16 * cbind(contact_matrix_other[,cols_70_older] + .59 * contact_matrix_other[,-cols_70_older])
    out <- out * .65 
  }
  if(scenario == "Lockdown"){
    out <- 1 * contact_matrix_home + .1 * contact_matrix_work + 0 * contact_matrix_school + .1 * contact_matrix_other
    out <- out * .65 
  }
  return(out)
}
