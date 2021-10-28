setParameters <- function () {
  diasDeProyeccion <<- 1100
  
  primeraVez <<- porc_gr_primeraVez <<- porc_cr_primeraVez <<- paramVac_primeraVez <<- ifr_primeraVez <<- transprob_primeraVez <<- mbeta_primeraVez <<- mgraves_primeraVez <<- mcriticos_primeraVez <<- mifr_primeraVez <<- TRUE
  # crea matrices de contacto y efectividad - set TRUE si queremos observada
  use_empirical_mc <<- T
  immunityStates <<- c("No immunity", "Recovered", "1Dosis", "2Dosis")
  ageGroups <<- c("0-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
  ageGroupsV <<- c("00","18","30","40","50", "60", "70", "80")
  matrixNames <<- list(immunityStates,
                ageGroups)
  # crea matrices de contacto y efectividad
  contact_matrix <<- matrix(c(5,1,1,1,1,1,1,1,
                              2,4,4,4,4,4,4,4,
                              2,4,4,4,4,4,4,4,
                              2,4,4,4,4,4,4,4,
                              2,4,4,4,4,4,4,4,
                              2,4,4,4,4,4,4,4,
                              .5,1,1,1,1,1,5,5,
                              .5,1,1,1,1,1,5,5),8,byrow = T)
  colnames(contact_matrix) <<- rownames(contact_matrix) <<- ageGroups
  
  transmission_probability <<- matrix(c(0.2299, 0.2413, 0.2527, 0.266, 0.2831, 0.3097, 0.3211, 0.3211,
                                        0.47795, 0.50165, 0.52535, 0.553, 0.58855, 0.64385, 0.66755, 0.66755,
                                        0.5203, 0.5461, 0.5719, 0.602, 0.6407, 0.7009, 0.7267, 0.7267,
                                        0.484, 0.508, 0.532, 0.56, 0.596, 0.652, 0.676, 0.676,
                                        0.4961, 0.5207, 0.5453, 0.574, 0.6109, 0.6683, 0.6929, 0.6929,
                                        0.5324, 0.5588, 0.5852, 0.616, 0.6556, 0.7172, 0.7436, 0.7436,
                                        0.4477, 0.4699, 0.4921, 0.518, 0.5513, 0.6031, 0.6253, 0.6253,
                                        0.4477, 0.4699, 0.4921, 0.518, 0.5513, 0.6031, 0.6253, 0.6253),length(ageGroups),length(ageGroups),byrow= T)
  
  transmission_probability <<- transmission_probability * 0.68
  
  colnames(transmission_probability) <- rownames(transmission_probability) <<- ageGroups
  
  # datos de gravedad
  
  # Age specific IFR
  ifr <<- c(8.8e-05,0.000284,0.000745,0.001868,0.004608,0.011231,0.026809,0.079684)
  porcentajeCasosGravesRow <<- c(0.003634, 0.003644, 0.005372, 0.008520, 0.025740, 0.044253, 0.099200, 0.205628) * 0.7
  porcentajeCasosGraves <<- matrix(rep(porcentajeCasosGravesRow,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = matrixNames)
  porcentajeCasosCriticosRow <<- c(0.000966,0.000969,0.001428,0.00348,0.01326,0.024747,0.0608,0.094372) * 0.7
  porcentajeCasosCriticos <<- matrix(rep(porcentajeCasosCriticosRow,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = matrixNames)
  porcAsignadoCovid <<- .7
  
  
  
  modif_beta <<-  modif_beta_param <<- matrix(rep(c(1,0.15,.6,.5),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  modif_porc_gr <<-  modif_porc_gr_param <<- matrix(rep(c(1,.3,.1,.05),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  modif_porc_cr <<-  modif_porc_cr_param <<- matrix(rep(c(1,.1,.03,.02),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  modif_ifr <<-  modif_ifr_param <<- matrix(rep(c(1,.05,.01,.005),length(ageGroups)),4,length(ageGroups),byrow=F,dimnames = matrixNames)
  duracion_inmunidad <<- 180
  duracion_proteccion <<- 360 # TODO: Implementar, Cuanto?
  
  namesVac <<- list(immunityStates,
                  c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP", "intervaloInterDosis", "idVacuna", "dosis"))
  
  paramVac <<- matrix(data=c(0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             20,.2,30,.6,360,30,"SchemeIncomplete",1
                             ,20,.4,30,.5,360,30,"SchemeComplete",2
                             # ,20,.4,0,.5,360,30,"SINOPH",1
                             # ,20,.4,0,.5,360,30,"SINOPH",2
  ), nrow=length(immunityStates), ncol=8, byrow=T, dimnames = namesVac)
  
  
}
