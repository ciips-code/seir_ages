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
                      country
                      # tablaDeAnosDeVidaPerdidos
){
  ifrm = matrix(rep(ifr,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)
  names = list(immunityStates,
               ageGroups)
  # cada columna es un grupo
  contact_matrix <- lapply(1:dias, 
                           matrix, 
                           data= contact_matrix, 
                           nrow=length(ageGroupsV), 
                           ncol=length(ageGroupsV), 
                           dimnames = list(ageGroupsV,
                                           ageGroupsV))

  transmission_probability <- lapply(1:dias, 
                                     matrix, 
                                     data= transmission_probability, 
                                     nrow=length(ageGroupsV), 
                                     ncol=length(ageGroupsV), 
                                     dimnames = list(ageGroupsV,
                                                     ageGroupsV))
  
  beta <- lapply(1:dias, 
                 matrix, 
                 data= transmission_probability[[1]] * contact_matrix[[1]], 
                 nrow=length(ageGroupsV), 
                 ncol=length(ageGroupsV), 
                 dimnames = list(ageGroupsV,
                                 ageGroupsV))
  
            
  e = E = S = i = Ss = I = Ii = Ig = Ic = r = R = D = d = U = u = V = v = vA = beta = tot = yl = lapply(1:dias, 
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
  vacGroupActive = 1
  for(t in 2:dias){
    
    # Calculo de cobertura para escenario de cambio de NPIs
    cantidadVacunas = Reduce('+',vA)
    cantidadVacunasMas60 = cantidadVacunas[3,6] + cantidadVacunas[3,7] + cantidadVacunas[3,8]
    poblacionMayores60 = N[1,6] + N[1,7] + N[1,8]
    coberturaMas60 = cantidadVacunasMas60 / poblacionMayores60
    if (coberturaMas60 > relaxationThreshold) {
      beta[[t]] = contact_matrix_relaxed * transmission_probability[[t]]
    } else {
      beta[[t]] = contact_matrix[[t]] * transmission_probability[[t]]
    }
    
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
      e[[t-1]] <- ifelse(e[[t-1]]<0.1,0,e[[t-1]])
    } else {
      beta[[t]]       = beta[[t]] * relaxValue[t]
      e[[t-1]] = S[[t-1]] * matrix((beta[[t]]) %*% I_edad/N_edad, nrow=length(immunityStates), length(ageGroups),byrow = T) * modif_beta
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
      if (country == "Argentina") {
        d[[t]] = d[[t]] * 0.89
      } else if (country == "Peru") {
        d[[t]] = d[[t]] * 1.2
      } else if (country == "Colombia") {
        d[[t]] = d[[t]] * 1.10
      } else if (country == "Chile") {
        d[[t]] = d[[t]] * 1.10
      } else if (country == "Mexico") {
        d[[t]] = d[[t]] * 1.25
      } else if (country == "Brazil") {
        d[[t]] = d[[t]] * 1
      }
    }
    # Acá calculamos los años de vida perdidos
    # Multiplicamos el total de muertos por grupo de edad
    # Multiplicado por una matriz que tenga los años de vida perdidos para cada grupo para este pais y los mismos valores para todos los estados inmunitarios
    # Esto tiene que venir de parametros (ver si el vector o todo)
    tablaDeAnosDeVidaPerdidos = c(80,70,60,50,40,30,20,10)
    matrizDeAnosDeVidaPerdidos = matrix(rep(tablaDeAnosDeVidaPerdidos,length(immunityStates)),length(immunityStates),length(ageGroups),byrow = T)
    yl[[t]] = d[[t]] * matrizDeAnosDeVidaPerdidos
    
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
    
    vacuna = 3
    latencia = as.numeric(paramVac[vacuna,1])
    porcV = as.numeric(paramVac[vacuna,2])
    tiempoV = as.numeric(paramVac[vacuna,3])
    porcProt = as.numeric(paramVac[vacuna,4])
    tiempoP = as.numeric(paramVac[vacuna,5])
    intervalo = as.numeric(paramVac[vacuna,6])
    if (t > (tVacunasCero + latencia) && t < (tVacunasCero + diasVacunacion)) {
      prioridadesHoy <- selectedPriority
      prioridadesHoy[prioridadesHoy != vacGroupActive] = 0
      prioridadesHoy[prioridadesHoy == vacGroupActive] = 1
      numeroDeGrupos = length(prioridadesHoy[prioridadesHoy == 1])
      ritmoPorGrupo = ritmoVacunacion/numeroDeGrupos
      vectorVacunacion = prioridadesHoy * ritmoPorGrupo
      limitesPorGrupoDeEdad = N[1,] * selectedUptake
      haySparaVacunar = (Reduce("+",vA)[vacuna,] < limitesPorGrupoDeEdad)
      haySparaVacunar[haySparaVacunar == TRUE] = 1
      haySparaVacunar[is.na(haySparaVacunar)] = 0
      haySparaVacunar = haySparaVacunar * prioridadesHoy
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
      dosis1aReforzar = vA[[t-30]][3,]
      segundasDosisHoyV3 = dosis1aReforzar * porcV
      segundasDosisHoyS3 = dosis1aReforzar * porcProt
      vA[[t]][4,] = segundasDosisHoyV3 + segundasDosisHoyS3
    }
    
    # # Salida de V
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
    for (iAge in c(1:length(ageGroups))) {
      if (segundasDosisHoyV3[iAge] < V[[t]][3,iAge]) {
        V[[t]][3,iAge] = V[[t]][3,iAge] - segundasDosisHoyV3[iAge]
        V[[t]][4,iAge] = V[[t]][4,iAge] + segundasDosisHoyV3[iAge]
      }
    }
    v[[t]] = Vin
    # Empiezo a armar el S a partir de las transiciones
    S[[t]] = S[[t-1]] - e[[t-1]]  + Vout
    S[[t]][1,] = S[[t]][1,] - colSums(Vin)
    S[[t]][2,] = S[[t]][2,] + colSums(losQueHoyPierdenImunidad)
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
    # Pasar de renglon a "No inmunes" a los vacunados que vencen (tiempoP, index 5 en paramvac)
    # Vuelve a No inmunes los que terminan su tiempo de proteccion

    for (vacuna in c(3:nrow(paramVac))) {
      tiempoP = as.numeric(paramVac[vacuna,5])
      S[[t]][vacuna,] =  S[[t]][vacuna,] - S[[t]][vacuna,]/tiempoP
      S[[t]][1,]=S[[t]][1,] + S[[t]][vacuna,]/tiempoP
    }

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
                 "yl: Years lost"=yl)
  
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
