generaPlanVacunacion <- function(metas, N, fechaInicio, fechaObjetivo, tVacunasCero, AvArg) {
  cantidadVacunasMeta = metas * N
  dias = as.numeric(fechaObjetivo - fechaInicio)
  ritmo = cantidadVacunasMeta / dias
  diaVac = matrix(c(rep(0,10),ritmo),3,5,byrow = T)
  planVac = lapply(seq_len(dias+1), function(i) { 
    return(diaVac)
  })
  AvArg[tVacunasCero:(tVacunasCero+dias)] = planVac
  planCero = lapply(seq_len(length(AvArg) - (tVacunasCero+dias)+1), function(i) { 
    return(matrix(0,3,5,byrow = T))
  })
  AvArg[(tVacunasCero+dias):length(AvArg)] = planCero
  return(AvArg)
}


# for (j in c(3:nrow(vacunasDelDia))) {
#   for (i in seq_len(length(ageGroups))) {
#     if (!haySparaVacunar[i]) {
#       vacunasDelDia[j,i] = 0
#     }
#   }
# }