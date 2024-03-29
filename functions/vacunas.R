namesVac <<- list(immunityStates,
                  c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP", "intervaloInterDosis", "idVacuna", "dosis"))

paramVac <<- matrix(data=c(0,0,0,0,0,0,0,0,
                           0,0,0,0,0,0,0,0,
                           20,0,30,.8,280,30,"SchemeIncomplete",1
                           ,20,0,30,.9,280,30,"SchemeComplete",2
), nrow=length(immunityStates), ncol=8, byrow=T, dimnames = namesVac)

generaEscenarioSage <- function(uptake, goal, priorities, AvArg, N, tVacunasCero, diaCeroVac) {
  intervaloInterDosis = 30
  # TODO: Obtener intervalo inter dosis de paramvac
  coverage = getUptakeCoverage(uptake)
  if (!is.null(coverage)) {
    if (sum(coverage) == 0) {
      AvArgParam <- lapply(AvArg, function(dia) {
        return(dia * 0)  
      })
      return(AvArgParam)
    } else {
      dias = as.numeric(as.Date(goal) - diaCeroVac)
      AvArgParam <- generaPlanVacunacion(coverage, N, dias, tVacunasCero, AvArg, intervaloInterDosis)
      AvArgParam <- applyPriority(priorities, AvArgParam)
      return(AvArgParam)
    }
  } else {
    if (priorities == "Current priorities") {
      return(AvArg)
    } else {
      AvArgParam <- applyPriority(priorities, AvArg)
      return(AvArgParam)
    }
  }
}

generaPlanVacunacion <- function(metas, N, dias, tVacunasCero, AvArg, intervaloInterDosis) {
  cantidadVacunasMeta = metas * N
  ritmo = cantidadVacunasMeta / dias
  # TODO: hardcodeado para 4 
  diaVac1Dosis = matrix(c(rep(0,(length(ritmo)*2)),ritmo,rep(0,(length(ritmo)))),4,length(ritmo),byrow = T)
  diaVac2Dosis = matrix(c(rep(0,(length(ritmo)*2)),ritmo,ritmo),4,length(ritmo),byrow = T)
  planVac = lapply(seq_len(dias+1), function(i) { 
    return(diaVac2Dosis)
  })
  planVac[1:intervaloInterDosis] = lapply(seq_len(intervaloInterDosis), function(i) { 
    return(diaVac1Dosis)
  })
  AvArg[(tVacunasCero):(tVacunasCero+dias)] = planVac
  planContinuacion = lapply(seq_len(length(AvArg) - (tVacunasCero+dias)+1), function(i) { 
    return(diaVac2Dosis)
  })
  AvArg[(tVacunasCero+dias):length(AvArg)] = planContinuacion
  return(AvArg)
}

applyPriority <- function(priorities, AvArg) {
  if (priorities == "Current priorities") {
    
    return(AvArg)
  } else {
    priorityValues <- getPriorityDetails(priorities)
    AvArgParam <- lapply(AvArg, function(dia) {
      
      totalDia <- sum(dia)
      
      for (i in 1:ncol(dia)) {
        for (j in 3:length(immunityStates)) {
          dia[j,i] <- totalDia * priorityValues[i]/sum(priorityValues) 
          
        }
      }
      # browser(expr={iso_country=="CRI"})
      return(dia)
    })
    
    return(AvArgParam)
  }
}

getPriorityDetails <- function(priorities) {
  if (priorities == "Priority: older -> adults -> young") {
    return(c(.05,.05,.05,.05,.10,.10,.30,.30))
  } else if (priorities == "Priority: older + adults -> young") {
    return(c(.05,.05,.05,.05,.15,.15,.25,.25))
  } else if (priorities == "Priority: adults -> older -> young") {
    return(c(.05,.05,.05,.05,.35,.15,.20,1))
  } else if (priorities == "Priority: school age -> others") {
    return(c(.26,.10,.10,.10,.11,.11,.11,.11))
  } else if (priorities == "No priorities") {
    return(c(.12,.12,.12,.12,.13,.13,.13,.13))
  } 
}

getUptakeCoverage <- function(uptake) {
  coverage = NULL
  if (uptake == "High uptake: 95%") {
    coverage = c(.95,.95,.95,.95,.95,.95,.95,.95)
  } else if (uptake == "High uptake: 80%") {
    coverage = c(.80,.80,.80,.80,.80,.80,.80,.80)
  }  else if (uptake == "Mid-range uptake: 50%") {
    coverage = c(.50,.50,.50,.50,.50,.50,.50,.50)
  }  else if (uptake == "Low uptake: 20%") {
    coverage = c(.20,.20,.20,.20,.20,.20,.20,.20)
  }  else if (uptake == "No vaccination") {
    coverage = c(0,0,0,0,0,0,0,0)
  }  else if (uptake == "Current uptake") {
    coverage = NULL
  }
  return(coverage)
}

defaultEfficacy = list()
defaultEfficacy$modif_beta =    matrix(rep(c(1,.3,.3,.3),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
defaultEfficacy$modif_porc_gr = matrix(rep(c(1,.3,.3,.2),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
defaultEfficacy$modif_porc_cr = matrix(rep(c(1,.1,.2,.2),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
defaultEfficacy$modif_ifr =     matrix(rep(c(1,0,.1,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)

applyVaccineEfficacy <- function(selectedEfficacy) {
  matrixNames <- list(immunityStates,
                ageGroups)
  efficacy = list()
  if (selectedEfficacy == "A. 100% all") {
    efficacy$modif_beta = matrix(rep(c(1,.1,.2,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_gr = matrix(rep(c(1,.1,.3,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,.1,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_ifr = matrix(rep(c(1,.05,0,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  } else if (selectedEfficacy == "B1. 100%, 80%, 80%") {
    efficacy$modif_beta =    defaultEfficacy$modif_beta
    efficacy$modif_porc_gr = defaultEfficacy$modif_porc_gr
    efficacy$modif_porc_cr = defaultEfficacy$modif_porc_cr
    efficacy$modif_ifr =     defaultEfficacy$modif_ifr
  } else if (selectedEfficacy == "B2. 100%, 80%, 50%") {
    efficacy$modif_beta =    matrix(rep(c(1,.1,.7,.5),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.3,.2),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,1,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_ifr =     matrix(rep(c(1,0,0,0),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  } else if (selectedEfficacy == "C1. 80%, 80%, 50%") {
    efficacy$modif_beta = matrix(rep(c(1,.1,.7,.5),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.3,.2),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,.1,.2),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_ifr = matrix(rep(c(1,.05,.05,.1),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  } else if (selectedEfficacy == "C2. 80%, 50%, 50%") {
    efficacy$modif_beta = matrix(rep(c(1,.1,.7,.5),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.7,.5),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,.4,.2),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
    efficacy$modif_ifr = matrix(rep(c(1,.05,.3,.1),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  }
  return(efficacy)
}

######### V2

getPrioritiesV2 <- function(priorities) {
  if (priorities == "Priority: older -> adults -> young") {
    return(c(8,7,6,5,4,3,2,1))
  } else if (priorities == "Priority: older + adults -> young") {
    return(c(8,7,6,4,2,5,3,1))
  } else if (priorities == "Priority: adults -> older -> young") {
    return(c(8,7,6,2,1,3,4,5))
  } else if (priorities == "Priority: school age -> others") {
    return(c(1,8,7,6,5,4,3,2))
  } else if (priorities == "No priorities") {
    return(c(1,1,1,1,1,1,1,1))
  } 
}

getUptake <- function(uptake) {
  coverage = NULL
  if (uptake == "High uptake: 95%") {
    coverage = .95
  } else if (uptake == "High uptake: 80%") {
    coverage = .80
  }  else if (uptake == "Mid-range uptake: 50%") {
    coverage = .50
  }  else if (uptake == "Low uptake: 20%") {
    coverage = .20
  }  else if (uptake == "No vaccination") {
    coverage = 0
  }  else if (uptake == "Current uptake") {
    coverage = 0
  }
  return(coverage)
}

# choices = c("A. 100% all disease",
#             "B1. 100% severe, 80% moderate, 80% mild",
#             "B2. 100% severe, 80% moderate, 50% mild",
#             "C1. 80% severe, 80% moderate, 50% mild",
#             "C2. 80% severe, 50% moderate, 50% mild")

# for (j in c(3:nrow(vacunasDelDia))) {
#   for (i in seq_len(length(ageGroups))) {
#     if (!haySparaVacunar[i]) {
#       vacunasDelDia[j,i] = 0
#     }
#   }
# }


# print(input$vacDateGoal)
# print(input$vacUptake)
# print(input$vacStrat)
# 
# if (input$vacStrat == "No vaccination") {
#   
#   AvArgParam <- lapply(AvArg, function(dia) {
#     return(dia * 0)  
#   })
# } else if (input$vacStrat=="No priority") {
#   AvArgParam <- lapply(AvArg, function(dia) {
#     totalDia <- sum(dia)
#     for (i in 1:ncol(dia)) {
#       for (j in 3:length(immunityStates)) {
#         dia[j,i] <- totalDia * N[i]/sum(N)    
#       }
#     }
#     return(dia)
#   })
# } else if (input$vacStrat=="Priority: older -> adults -> young") {
#   prioridad <- c(10,10,25,25,30)
#   AvArgParam <- lapply(AvArg, function(dia) {
#     totalDia <- sum(dia)
#     for (i in 1:ncol(dia)) {
#       for (j in 3:length(immunityStates)) {
#         dia[j,i] <- totalDia * prioridad[i]/sum(prioridad)    
#       }
#     }
#     return(dia)
#   })
# } else if (input$vacStrat=="Coverage goals 1") {
#   metas <- c(0,.50,.65,.75,.85)
#   AvArgParam <- generaPlanVacunacion(metas, N, diaCeroVac, as.Date("2022-01-01"), tVacunasCero, AvArg)
# } else {
#   AvArgParam <- AvArg
# }
# 
# AvArgParam <- lapply(AvArgParam, function (dia) {
#   
#   colnames(dia) <- ageGroups
#   return(dia)
# })