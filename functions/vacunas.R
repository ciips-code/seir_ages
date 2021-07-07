generaEscenarioSage <- function(uptake, goal, priorities, AvArg, N, tVacunasCero, diaCeroVac) {
  # browser()
  coverage = getUptakeCoverage(uptake)
  if (!is.null(coverage)) {
    if (sum(coverage) == 0) {
      AvArgParam <- lapply(AvArg, function(dia) {
        return(dia * 0)  
      })
      return(AvArgParam)
    } else {
      dias = as.numeric(as.Date(goal) - diaCeroVac)
      AvArgParam <- generaPlanVacunacion(coverage, N, dias, tVacunasCero, AvArg)
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

generaPlanVacunacion <- function(metas, N, dias, tVacunasCero, AvArg) {
  cantidadVacunasMeta = metas * N
  ritmo = cantidadVacunasMeta / dias
  diaVac = matrix(c(rep(0,(length(ritmo)*2)),ritmo),3,length(ritmo),byrow = T)
  planVac = lapply(seq_len(dias+1), function(i) { 
    return(diaVac)
  })
  AvArg[tVacunasCero:(tVacunasCero+dias)] = planVac
  planCero = lapply(seq_len(length(AvArg) - (tVacunasCero+dias)+1), function(i) { 
    return(diaVac)
  })
  AvArg[(tVacunasCero+dias):length(AvArg)] = planCero
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
      return(dia)
    })
    return(AvArgParam)
  }
}

getPriorityDetails <- function(priorities) {
  if (priorities == "Priority: older -> adults -> young") {
    return(c(.05,.05,.05,.05,.25,.25,.30))
  } else if (priorities == "Priority: older + adults -> young") {
    return(c(.05,.05,.05,.05,.26,.27,.27))
  } else if (priorities == "Priority: adults -> older -> young") {
    return(c(.05,.05,.05,.05,.40,.20,.20))
  } else if (priorities == "No priorities") {
    return(c(.14,.14,.14,.14,.14,.14,.16))
  }
}

getUptakeCoverage <- function(uptake) {
  coverage = NULL
  if (uptake == "High uptake: 95%") {
    coverage = c(.95,.95,.95,.95,.95,.95,.95)
  } else if (uptake == "High uptake: 80%") {
    coverage = c(.80,.80,.80,.80,.80,.80,.80)
  }  else if (uptake == "Mid-range uptake: 50%") {
    coverage = c(.50,.50,.50,.50,.50,.50,.50)
  }  else if (uptake == "Low uptake: 20%") {
    coverage = c(.20,.20,.20,.20,.20,.20,.20)
  }  else if (uptake == "No vaccination") {
    coverage = c(0,0,0,0,0,0,0)
  }  else if (uptake == "Current uptake") {
    coverage = NULL
  }
  return(coverage)
}

applyVaccineEfficacy <- function(selectedEfficacy) {
  names <- list(immunityStates,
                ageGroups)
  efficacy = list()
  if (selectedEfficacy == "A. 100% all") {
    efficacy$modif_beta = matrix(rep(c(1,1,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_ifr = matrix(rep(c(1,.05,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
  } else if (selectedEfficacy == "B1. 100%, 80%, 80%") {
    efficacy$modif_beta = matrix(rep(c(1,1,.2),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.2),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_ifr = matrix(rep(c(1,.05,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
  } else if (selectedEfficacy == "B2. 100%, 80%, 50%") {
    efficacy$modif_beta = matrix(rep(c(1,1,.5),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.2),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_ifr = matrix(rep(c(1,.05,0),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
  } else if (selectedEfficacy == "C1. 80%, 80%, 50%") {
    efficacy$modif_beta = matrix(rep(c(1,1,.5),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.2),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,.2),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_ifr = matrix(rep(c(1,.05,.1),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
  } else if (selectedEfficacy == "C2. 80%, 50%, 50%") {
    efficacy$modif_beta = matrix(rep(c(1,1,.5),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_gr = matrix(rep(c(1,.3,.5),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_porc_cr = matrix(rep(c(1,.1,.2),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
    efficacy$modif_ifr = matrix(rep(c(1,.05,.1),length(ageGroups)),3,length(ageGroups),byrow=F,dimnames = names)
  }
  return(efficacy)
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