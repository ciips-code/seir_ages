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
  # ifr_hi <<- 1.25*c(8.8e-05,0.000284,0.000745,0.001868,0.004608,0.011231,0.026809,0.079684)
  # ifr_low <<- .75*c(8.8e-05,0.000284,0.000745,0.001868,0.004608,0.011231,0.026809,0.079684)
  # 
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
  
  duracion_inmunidad_hi <<- 180 * .75
  duracion_proteccion_hi <<- 360 * .75
  
  duracion_inmunidad_low <<- 180 * 1.25
  duracion_proteccion_low <<- 360 * 1.25
  
  namesVac <<- list(immunityStates,
                  c("latencia", "porcV", "tiempoV", "porcProt", "tiempoP", "intervaloInterDosis", "idVacuna", "dosis"))
  
  paramVac <<- matrix(data=c(0,0,0,0,0,0,0,0,
                             0,0,0,0,0,0,0,0,
                             20,.2,30,.6,360,30,"SchemeIncomplete",1
                             ,20,.4,30,.5,360,30,"SchemeComplete",2
                             # ,20,.4,0,.5,360,30,"SINOPH",1
                             # ,20,.4,0,.5,360,30,"SINOPH",2
  ), nrow=length(immunityStates), ncol=8, byrow=T, dimnames = namesVac)
  
  sens_transmission_low <<- matrix(c(0.8496,	0.8502,	0.8500,	0.8504,	0.8493,	0.8495,	0.8489,	0.8489,
                                     0.8499,	0.8502,	0.8518,	0.8495,	0.8510,	0.8486,	0.8506,	0.8506,
                                     0.8500,	0.8487,	0.8497,	0.8505,	0.8502,	0.8502,	0.8501,	0.8501,
                                     0.8506,	0.8492,	0.8496,	0.8500,	0.8507,	0.8497,	0.8506,	0.8506,
                                     0.8485,	0.8491,	0.8514,	0.8502,	0.8494,	0.8503,	0.8499,	0.8499,
                                     0.8496,	0.8502,	0.8496,	0.8506,	0.8491,	0.8508,	0.8495,	0.8495,
                                     0.8513,	0.8498,	0.8496,	0.8494,	0.8512,	0.8507,	0.8512,	0.8512,
                                     0.8513,	0.8498,	0.8496,	0.8494,	0.8512,	0.8507,	0.8512,	0.8512),length(ageGroups),length(ageGroups),byrow= T)
  
  sens_transmission_hi <<- matrix(c(1.146,	1.146,	1.150,	1.150,	1.147,	1.150,	1.148,	1.148,
                                    1.148,	1.150,	1.150,	1.149,	1.151,	1.150,	1.151,	1.151,
                                    1.150,	1.149,	1.150,	1.150,	1.150,	1.150,	1.150,	1.150,
                                    1.149,	1.151,	1.150,	1.150,	1.149,	1.150,	1.149,	1.149,
                                    1.150,	1.149,	1.150,	1.150,	1.151,	1.151,	1.150,	1.150,
                                    1.150,	1.152,	1.150,	1.149,	1.149,	1.151,	1.149,	1.149,
                                    1.151,	1.150,	1.150,	1.151,	1.151,	1.151,	1.150,	1.150,
                                    1.151,	1.150,	1.150,	1.151,	1.151,	1.151,	1.150,	1.150),length(ageGroups),length(ageGroups),byrow= T)
  
  modif_porcentajeCasosGravesRow_low <<- c(0.652173913,0.652305159,0.808823529,0.8,0.769230769,0.768128714,0.75,0.333331712)
  modif_porcentajeCasosGraves_low <<- matrix(rep(modif_porcentajeCasosGravesRow_low,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = matrixNames)
  modif_porcentajeCasosCriticosRow_low <<- c(0.51552795,0.51496388,0.638655462,0.568103448,0.507692308,0.492625369,0.465,0.228478786)
  modif_porcentajeCasosCriticos_low <<- matrix(rep(modif_porcentajeCasosCriticosRow_low,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = matrixNames)
  
  modif_porcentajeCasosGravesRow_hi <<- c(1.565217391,1.565312843,1.235294118,1.166666667,1.282051282,1.304363546,1.375,3.299956232)
  modif_porcentajeCasosGraves_hi <<- matrix(rep(modif_porcentajeCasosGravesRow_hi,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = matrixNames)
  modif_porcentajeCasosCriticosRow_hi <<- c(1.565217391,1.564499484,1.235294118,1.166666667,1.282051282,1.304319716,1.375,3.299919468)
  modif_porcentajeCasosCriticos_hi <<- matrix(rep(modif_porcentajeCasosCriticosRow_hi,length(immunityStates)),4,length(ageGroups),byrow=T,dimnames = matrixNames)
  
  
}
