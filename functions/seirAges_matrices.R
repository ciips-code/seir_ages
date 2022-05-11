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
                      relaxationThreshold,
                      contact_matrix_relaxed,
                      transmission_probability,
                      N, 
                      modif_beta,
                      modif_porc_gr,
                      modif_porc_cr,
                      modif_ifr,
                      duracion_inmunidad,
                      defunciones_reales,
                      planVacunacionFinal,
                      selectedPriority,
                      selectedUptake,
                      ritmoVacunacion,
                      diasVacunacion,
                      immunityStates,
                      ageGroups,
                      paramVac,
                      tVacunasCero,
                      relaxNpi,
                      relaxGoal,
                      relaxFactor,
                      country,
                      modificador_tiempoP = NULL,
                      usarVariantes = T
                      # tablaDeAnosDeVidaPerdidos
){
  # diasVacunacion = diasVacunacion
  ifrm = matrix(rep(ifr,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)
  matrixNames = list(immunityStates,
               ageGroups)
            
  e = E = S = i = Ss = I = Ii = Ig = Ic = r = R = D = d = U = u = V = v = vA = tot = yl = ylq = yld = ylqd = lapply(1:dias, 
                                                                                                                      matrix, 
                                                                                                                      data= 0, 
                                                                                                                      nrow=length(immunityStates), 
                                                                                                                      ncol=length(ageGroups), 
                                                                                                                      dimnames = matrixNames)
  
  S[[1]][1,] = N
  N = S[[1]]
  
  I[[1]] = matrix(0,length(immunityStates),length(ageGroups), byrow = T,
                    dimnames = matrixNames)
  I[[1]][1,2] = 1 # La semilla del primer infectado
  
  # seir
  tHoy <<- tVacunasCero + 4
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
  vacGroupActive = 1
  for(t in 2:dias){
    if (usarVariantes) {
      modificadorVariantes = obtenerModificadorDeVariante(t, iso_country)
    } else {
      modificadorVariantes = getMatrizModificadoresVariantesSingle(1)
    }
    duracionIcLoop = duracionIc * modificadorVariantes$duracionDiasInternacion
    duracionIgLoop = duracionIg * modificadorVariantes$duracionDiasInternacion
    # Calculo de cobertura para escenario de cambio de NPIs
    cantidadVacunas = Reduce('+',vA)
    cantidadVacunasMas60 = cantidadVacunas[3,6] + cantidadVacunas[3,7] + cantidadVacunas[3,8]
    poblacionMayores60 = N[1,6] + N[1,7] + N[1,8]
    coberturaMas60 = cantidadVacunasMas60 / poblacionMayores60
    # browser()
    
    # TODO: Llamar a la funcion ECO, para modificar la variable contact_matrix
    
    if (customMatrix & t>tHoy+3 & is.na(customBeta[1,1]) == F) {
      rn <- as.numeric(rownames(customBeta[as.numeric(customBeta$start)<=t & as.numeric(customBeta$end)>=t,]))
      beta = eval(parse(text=paste0('`',customBeta$beta[as.numeric(rn)],'`')))
      
      beta <- get_custom_matrix(scenario = customBeta$beta[as.numeric(rn)],
                                dia_loop = t,
                                matrix_list = list(
                                  contact_matrix = contact_matrix,
                                  contact_matrix_work = contact_matrix_work,
                                  contact_matrix_home = contact_matrix_home,
                                  contact_matrix_school = contact_matrix_school,
                                  contact_matrix_other = contact_matrix_other),
                                ages= as.numeric(ageGroupsV)) * trans_prob_param
    } else {
      beta = contact_matrix * transmission_probability
      # print(beta)
    }
    
    I_edad = colSums(I[[t-1]])
    N_edad = colSums(N)
    if (t<tHoy){
      expuestosTotalesHoy = as.numeric(defunciones_reales[t+17+round(periodoPreinfPromedio,0),]) / ifr
      # distribuir segun filas en S[[t-1]]
      apor = matrix(rbind(rep(expuestosTotalesHoy,3)),length(immunityStates),length(ageGroups), byrow = T,
                 dimnames = matrixNames)
      bpor <- S[[t-1]]
      bpor <- pmax(bpor,0)
      cpor <- matrix(rbind(rep(colSums(bpor),3)),length(immunityStates),length(ageGroups), byrow = T,
                 dimnames = matrixNames)
      e[[t-1]] = apor*bpor/cpor
      e[[t-1]] <- ifelse(is.na(e[[t-1]]),0,e[[t-1]])
      e[[t-1]] <- ifelse(e[[t-1]]<0.1,0,e[[t-1]])
      e[[t-1]] <- e[[t-1]] * modificadorVariantes[[1]]
    } else {
      beta       = beta * relaxValue[t]
      e[[t-1]] = S[[t-1]] * matrix((beta) %*% I_edad/N_edad, nrow=length(immunityStates), length(ageGroups),byrow = T) * modif_beta * modificadorVariantes$modVacTransmision * modificadorVariantes$transmision
    }
    
    # resto seir
    E[[t]]      = E[[t-1]] + e[[t-1]] - E[[t-1]]/duracionE
    i[[t]]      = E[[t-1]]/duracionE
    
    Ii[[t]]     = Ii[[t-1]] + i[[t-1]] - Ii[[t-1]]/duracionIi
    
    Ig[[t]]     = Ig[[t-1]] - Ig[[t-1]]/duracionIgLoop + Ii[[t-1]]/duracionIi*porc_gr*modif_porc_gr*modificadorVariantes$hospitalizacion*modificadorVariantes$modVacGrave

    Ic[[t]]     = Ic[[t-1]] - Ic[[t-1]]/duracionIcLoop + Ii[[t-1]]/duracionIi*porc_cr*modif_porc_cr*modificadorVariantes$critico*modificadorVariantes$modVacCritico
    
    I[[t]]      = Ii[[t]] + Ig[[t]] + Ic[[t]]
    
    
    if (t<tHoy){
      d[[t]][1,] = as.numeric(defunciones_reales[t,])
    } else {
      # browser(expr = { t==500 })
      d[[t]]      = Ic[[t-1]]/duracionIcLoop * (ifrm) * modificadorVariantes$muerte * modif_ifr / porc_cr * modif_porc_cr * modificadorVariantes$modVacMuerte # siendo ifr = d[t]/i[t-duracionIi-duracionIcLoop]
      # if (country == "Argentina") {
      #   d[[t]] = d[[t]] * 0.89
      # } else if (country == "Peru") {
      #   d[[t]] = d[[t]] * 1.2
      # } else if (country == "Colombia") {
      #   d[[t]] = d[[t]] * 1.10
      # } else if (country == "Chile") {
      #   d[[t]] = d[[t]] * 1.10
      # } else if (country == "Mexico") {
      #   d[[t]] = d[[t]] * 1.25
      # } else if (country == "Brazil") {
      #   d[[t]] = d[[t]] * 1
      # } else if (country == "Uruguay") {
      #   d[[t]] = d[[t]] * 1.70
      # } else if (country == "Costa Rica") {
      #   d[[t]] = d[[t]] * 1.40
      # }
    }
    # Acá calculamos los años de vida perdidos
    # Multiplicamos el total de muertos por grupo de edad
    # Multiplicado por una matriz que tenga los años de vida perdidos para cada grupo para este pais y los mismos valores para todos los estados inmunitarios
    # Esto tiene que venir de parametros (ver si el vector o todo)
    tablaDeAnosDeVidaPerdidos = yearsLost_new[yearsLost_new$country==iso_country,"yearsLost"]
    matrizDeAnosDeVidaPerdidos = matrix(rep(tablaDeAnosDeVidaPerdidos,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)

    tablaDeAnosDeVidaPerdidosQualy = yearsLost_new[yearsLost_new$country==iso_country,"yearsLostQualy"]
    matrizDeAnosDeVidaPerdidosQualy = matrix(rep(tablaDeAnosDeVidaPerdidosQualy,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)

    tablaDeAnosDeVidaPerdidosDesc = yearsLost_new[yearsLost_new$country==iso_country,"yearsLostDisc"]
    matrizDeAnosDeVidaPerdidosDesc = matrix(rep(tablaDeAnosDeVidaPerdidosDesc,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)

    tablaDeAnosDeVidaPerdidosQualyDesc = yearsLost_new[yearsLost_new$country==iso_country,"yearsLostQualyDisc"]
    matrizDeAnosDeVidaPerdidosQualyDesc = matrix(rep(tablaDeAnosDeVidaPerdidosQualyDesc,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)


    yl[[t]] = d[[t]] * matrizDeAnosDeVidaPerdidos
    ylq[[t]] = d[[t]] * matrizDeAnosDeVidaPerdidosQualy
    yld[[t]] = d[[t]] * matrizDeAnosDeVidaPerdidosDesc
    ylqd[[t]] = d[[t]] * matrizDeAnosDeVidaPerdidosQualyDesc

    
    
    
    
    D[[t]]      = D[[t-1]] + d[[t-1]]
    u[[t]]      = Ii[[t-1]]/duracionIi * (1-porc_gr*modif_porc_gr-porc_cr*modif_porc_cr) +
                  Ig[[t-1]]/duracionIgLoop +
                  Ic[[t-1]]/duracionIcLoop * (1-ifr/porc_cr*modif_porc_cr)
    U[[t]]      = U[[t-1]] + u[[t-1]]
    # Transicion U -> S, sumamos los recuperados que pierden inmunidad hoy (U-S)
    losQueHoyPierdenImunidad = matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                                      dimnames = matrixNames)
    duracionInmunidad_loop = duracion_inmunidad * modificadorVariantes$duracionInmumidad[1,1]
    if (t>duracionInmunidad_loop+1) {
      losQueHoyPierdenImunidad = u[[t-duracionInmunidad_loop]]
      U[[t]] = U[[t]] - losQueHoyPierdenImunidad
    }
    R[[t]]      = U[[t]] + D[[t]]
    
    losQueHoyPierdenImunidad[2,] = losQueHoyPierdenImunidad[2,] + losQueHoyPierdenImunidad[1,]
    losQueHoyPierdenImunidad[1,] = losQueHoyPierdenImunidad[1,] * 0
    S[[t]] = S[[t-1]] - e[[t-1]] + losQueHoyPierdenImunidad
    # S[[t]][2,] = S[[t]][2,] - S[[t-1]][2,] / duracionInmunidad_loop
    # S[[t]][1,] = S[[t]][1,] + S[[t-1]][2,] / duracionInmunidad_loop
    
    # Pasajes S-V-S
    Vin = VquedaEnS =  Vout = vacunasDelDia = vacunasDelDia2 = vacunadosVacunaDia = vacunadosVacunaDia2 = matrix(data=0,length(immunityStates),length(ageGroups), byrow = T,
                   dimnames = matrixNames)

    vacuna = 3
    latencia = as.numeric(paramVac[vacuna,1])
    porcV = as.numeric(paramVac[vacuna,2])
    tiempoV = as.numeric(paramVac[vacuna,3])
    porcProt = as.numeric(paramVac[vacuna,4])
    # agregar if escenarios hi y low


    intervalo = as.numeric(paramVac[vacuna,6])
    if (t > (tVacunasCero + latencia) && t < (tVacunasCero + diasVacunacion)) {
      # browser(expr = {S[[t-1]][1,6] < 101000 })
      prioridadesHoy <- selectedPriority
      prioridadesHoy[prioridadesHoy != vacGroupActive] = 0
      prioridadesHoy[prioridadesHoy == vacGroupActive] = 1
      numeroDeGrupos = length(prioridadesHoy[prioridadesHoy == 1])
      if (numeroDeGrupos > 0) {
        ritmoPorGrupo = ritmoVacunacion/numeroDeGrupos
      } else {
        ritmoPorGrupo = 0
      }
      
      vectorVacunacion = prioridadesHoy * ritmoPorGrupo
      haySparaVacunar = vectorVacunacion - S[[t-1]][1,]
      haySparaVacunar[haySparaVacunar<1] = 0
      haySparaVacunar[haySparaVacunar>0] = 1
      # limitesPorGrupoDeEdad = N[1,] * selectedUptake
      # haySparaVacunar = (Reduce("+",vA)[vacuna,] < limitesPorGrupoDeEdad)
      # haySparaVacunar[haySparaVacunar == TRUE] = 1
      # haySparaVacunar[is.na(haySparaVacunar)] = 0
      # haySparaVacunar = haySparaVacunar * prioridadesHoy
      # print(t)
      if (sum(haySparaVacunar) > 0) {
        vectorVacunacion = vectorVacunacion * haySparaVacunar
        vA[[t]][vacuna,] = vectorVacunacion
        # Los que pasan a V
        Vin[vacuna,] =  vectorVacunacion * porcV
        # Los que se quedan en S pero protegidos (deben cambiar de renglon)
        VquedaEnS[vacuna,] =  vectorVacunacion * porcProt
        # Los que no les hace ningun efecto y quedan en S como no inmunes, no hay que hacer nada
      } else {
        vacGroupActive = vacGroupActive + 1
      }
    }

    # Salida de V
    loopTiempoV = tiempoV * modificadorVariantes$duracionInmumidad[1,1]
    totSalidaVHoy = V[[t-1]] / loopTiempoV
    Vout[3,] <- totSalidaVHoy[3,]
    Vout[4,] <- totSalidaVHoy[4,]
    V[[t]][3,] = V[[t-1]][3,] + Vin[3,] - Vout[3,]
    V[[t]][4,] = V[[t-1]][4,] + Vin[4,] - Vout[4,]

    v[[t]] = Vin
    # Empiezo a armar el S a partir de las transiciones
    S[[t]] = S[[t]] + Vout
    S[[t]][1,] = S[[t]][1,] - colSums(Vin)
    # # Reasigno de renglon los que quedaron hoy en S luego de vacunarse
    S[[t]][1,]=S[[t]][1,] - colSums(VquedaEnS)
    S[[t]] =  S[[t]] + VquedaEnS
    
    if (t > tVacunasCero + latencia + 30) {
      losQuePodemosVacunarConSegundaEnTres = S[[t-1]][3,] / 30
      S[[t]][3,]=S[[t]][3,] - losQuePodemosVacunarConSegundaEnTres
      S[[t]][4,]=S[[t]][4,] + losQuePodemosVacunarConSegundaEnTres
      vA[[t]][4,]=losQuePodemosVacunarConSegundaEnTres
    }
    
    
    # Saco de S3 los que cumplen 30 dias y los pasa a 4 (Aplicacion de 2da dosis)
    # if (t > tVacunasCero + latencia + 30) {
    # 
    #   losQueHoyTienenQueRecibirLaSegunda = vA[[t-30]][3,]
    #   losQuePodemosVacunarConSegundaEnTres = S[[t-1]][3,] / 30
    #   losQueHayQueVacunarConSegundaEnDosRecu = losQueHoyTienenQueRecibirLaSegunda - losQuePodemosVacunarConSegundaEnTres
    #   losQueHayQueVacunarConSegundaEnDosRecu[losQueHayQueVacunarConSegundaEnDosRecu<0] = 0
    # 
    #   S[[t]][3,]=S[[t]][3,] - losQuePodemosVacunarConSegundaEnTres
    #   S[[t]][2,]=S[[t]][2,] - losQueHayQueVacunarConSegundaEnDosRecu
    #   S[[t]][2,][S[[t]][2,]<0]=0
    #   S[[t]][4,]=S[[t]][4,] + losQuePodemosVacunarConSegundaEnTres + losQueHayQueVacunarConSegundaEnDosRecu
    #   vA[[t]][4,]=losQuePodemosVacunarConSegundaEnTres + losQueHayQueVacunarConSegundaEnDosRecu
    #   # Saca las segunda cuando se vence el período de cobertura
    #   if (t > tVacunasCero + latencia + 30 + 150) {
    #     S[[t]][4,]=S[[t]][4,] - S[[t-1]][4,] / 150
    #     S[[t]][1,]=S[[t]][1,] + S[[t-1]][4,] / 150
    #   }
    #   fecha_fin_refuerzos = which(fechas_master == "2022-12-31")
    #   if (t > (tVacunasCero + diasVacunacion) && t < fecha_fin_refuerzos) {
    #     losQueHoyTienenQueRecibirElRefuerzo = vA[[t-150]][4,]
    #     losQuePodemosVacunarConRefuerzoEnCuatro = S[[t-1]][4,] / 150
    #     losQueHayQueVacunarConRefuerzoEnDosRecu = losQueHoyTienenQueRecibirElRefuerzo - losQuePodemosVacunarConRefuerzoEnCuatro
    #     losQueHayQueVacunarConRefuerzoEnDosRecu[losQueHayQueVacunarConRefuerzoEnDosRecu<0] = 0
    #     S[[t]][1,]=S[[t]][1,] - losQuePodemosVacunarConRefuerzoEnCuatro
    #     S[[t]][2,]=S[[t]][2,] - losQueHayQueVacunarConRefuerzoEnDosRecu
    #     losQueNoPudeDar = S[[t]][2,]*-1
    #     losQueNoPudeDar[losQueNoPudeDar<0]=0
    #     S[[t]][2,][S[[t]][2,]<0]=0
    #     S[[t]][4,]=S[[t]][4,] + losQuePodemosVacunarConRefuerzoEnCuatro + losQueHayQueVacunarConRefuerzoEnDosRecu
    #     vA[[t]][4,]=losQuePodemosVacunarConRefuerzoEnCuatro + losQueHayQueVacunarConRefuerzoEnDosRecu - losQueNoPudeDar
    #   }
    # }
    
    
    tot[[t]] = S[[t]] + V[[t]] + E[[t]] + I[[t]] + R[[t]]
    
    

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
                 "d: Daily deaths"=d,
                 "yl: Years lost"=yl,
                 "ylq: Years lost Qualy"=ylq,
                 "yld: Years lost Disc"=yld,
                 "ylqd: Years lost Qualy Disc"=ylqd)
  
  Rt <- estimate_R(sapply(salida$`i: Daily infectious`,sum), 
                   method = "parametric_si",
                   config = make_config(list(mean_si = 3, std_si = 4)))$R
  salida$'Rt: Effective reproduction number' <- c(rep(0,7),Rt)
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
  if (country=="Barbados") {country <- "Argentina"}
  # get matrix from covoid package
  mc_arg  <- as.data.frame(t(import_contact_matrix(country = country,setting = type)))
  pop_arg <- data.frame(pop = as.numeric(import_age_distribution(country)), group = rownames(mc_arg))
  # some inconsistencies in covoid package for Argentina
  # plot(mc_arg[,"5"]*pop_arg[pop_arg$group=="5","pop"],mc_arg[,"80"]*pop_arg[pop_arg$group=="80","pop"])

  # rebuild given ages
  age_il <- ages
  contact_matrix <-
    pivot_longer(mc_arg %>%
                   dplyr::mutate(to_group = rownames(mc_arg)),
                 cols=1:ncol(mc_arg),names_to = "group") %>%
    left_join(pop_arg, by = "group") %>%
    dplyr::mutate(group = as.integer(group), to_group = as.integer(to_group),
           new_group = cut(group, breaks = c(age_il-1,100)),
           new_to_group = cut(to_group, breaks = c(age_il-1,100))) %>%
    group_by(new_group, new_to_group) %>%
    dplyr::summarise(new_contacts = sum(pop*value)/sum(pop)) %>%
    pivot_wider(names_from = "new_group", values_from = "new_contacts") %>% ungroup() %>%
    dplyr::select(-1) %>% as.matrix()
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
    out <- 1 * contact_matrix_home   + cbind(contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           1 * contact_matrix_school + cbind(contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
  }
  if(scenario == "Self-isolation"){
    out <- contact_matrix * .65
  }
  if(scenario == "Combined with schools open"){
    out <- 1 * contact_matrix_home   + cbind(.5 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
      1 * contact_matrix_school + cbind(.5 * contact_matrix_other[,-cols_70_older], .25 * contact_matrix_other[,cols_70_older])
    out <- out * .65 
  }
  if(scenario == "Combined with schools closed"){
    out <- 1 * contact_matrix_home   + cbind(.5 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           0 * contact_matrix_school + cbind(.5 * contact_matrix_other[,-cols_70_older], .25 * contact_matrix_other[,cols_70_older])
    out <- out * .65 
  }
  if(scenario == "Intensive interventions with schools closed"){
    out <- 1 * contact_matrix_home   + cbind(.65 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           0 * contact_matrix_school + cbind(.59 * contact_matrix_other[,-cols_70_older], .16 * contact_matrix_other[,cols_70_older])
    out <- out * .65 
  }
  if(scenario == "Intensive interventions with schools open"){
    out <- 1 * contact_matrix_home   + cbind(.65 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           1 * contact_matrix_school + cbind(.59 * contact_matrix_other[,-cols_70_older], .16 * contact_matrix_other[,cols_70_older])
    out <- out * .65
  }
  if(scenario == "Lockdown"){
    out <- 1 * contact_matrix_home + .1 * contact_matrix_work + 0 * contact_matrix_school + .1 * contact_matrix_other
    out <- out * .65 
  }
  return(out)
}
