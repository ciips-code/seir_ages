calibradores <<- list(
  omicron = list(
    ARG = list(
      transmision=4.5, # Omicron
      hospitalizacion=.44,  # Omicron
      critico=.33,  # Omicron
      muerte=.3,  # Omicron
      duracionInmumidad=.8,  # Omicron
      modVacTransmision=1.45,  # Omicron
      modVacGrave=2.277,  # Omicron
      modVacCritico=2.652,  # Omicron
      modVacMuerte=3.174,  # Omicron
      duracionDiasInternacion=.31,  # Omicron
      fechaTransicion=as.Date("2021-11-01"),  # Omicron
      periodoTransicion=30,  # Omicron
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    BRA = list(
      transmision=1,
      hospitalizacion=.44,
      critico=.33,
      muerte=.3,
      duracionInmumidad=.8,
      modVacTransmision=1.45,
      modVacGrave=2.277,
      modVacCritico=2.652,
      modVacMuerte=3.174,
      duracionDiasInternacion=.31,
      fechaTransicion=as.Date("2021-09-01"),
      periodoTransicion=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    JAM = list(
      transmision=1,
      hospitalizacion=.44,
      critico=.33,
      muerte=.3,
      duracionInmumidad=.8,
      modVacTransmision=1.45,
      modVacGrave=2.277,
      modVacCritico=2.652,
      modVacMuerte=3.174,
      duracionDiasInternacion=.31,
      fechaTransicion=as.Date("2021-09-01"),
      periodoTransicion=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    MEX = list(
      transmision=1,
      hospitalizacion=.44,
      critico=.33,
      muerte=.3,
      duracionInmumidad=.8,
      modVacTransmision=1.45,
      modVacGrave=2.277,
      modVacCritico=2.652,
      modVacMuerte=3.174,
      duracionDiasInternacion=.31,
      fechaTransicion=as.Date("2021-09-01"),
      periodoTransicion=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    )
  ),
  delta = list(
    ARG = list(
      transmision=4, # delta
      hospitalizacion=.44,  # delta
      critico=.33,  # delta
      muerte=.3,  # delta
      duracionInmumidad=.8,  # delta
      modVacTransmision=1.45,  # delta
      modVacGrave=2.277,  # delta
      modVacCritico=2.652,  # delta
      modVacMuerte=3.174,  # delta
      duracionDiasInternacion=.31,  # delta
      fechaTransicion=as.Date("2021-04-20"),  # delta
      periodoTransicion=30,  # delta
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    BRA = list(
      transmision=1,
      hospitalizacion=.44,
      critico=.33,
      muerte=.3,
      duracionInmumidad=.8,
      modVacTransmision=1.45,
      modVacGrave=2.277,
      modVacCritico=2.652,
      modVacMuerte=3.174,
      duracionDiasInternacion=.31,
      fechaTransicion=as.Date("2021-09-01"),
      periodoTransicion=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    JAM = list(
      transmision=1,
      hospitalizacion=.44,
      critico=.33,
      muerte=.3,
      duracionInmumidad=.8,
      modVacTransmision=1.45,
      modVacGrave=2.277,
      modVacCritico=2.652,
      modVacMuerte=3.174,
      duracionDiasInternacion=.31,
      fechaTransicion=as.Date("2021-09-01"),
      periodoTransicion=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    MEX = list(
      transmision=1,
      hospitalizacion=.44,
      critico=.33,
      muerte=.3,
      duracionInmumidad=.8,
      modVacTransmision=1.45,
      modVacGrave=2.277,
      modVacCritico=2.652,
      modVacMuerte=3.174,
      duracionDiasInternacion=.31,
      fechaTransicion=as.Date("2021-09-01"),
      periodoTransicion=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    )
  )
)


calibradorDefault <- list (
  omicron = list(
    transmision=4,
    hospitalizacion=.44,
    critico=.33,
    muerte=.3,
    duracionInmumidad=.8,
    modVacTransmision=1.45,
    modVacGrave=2.277,
    modVacCritico=2.652,
    modVacMuerte=3.174,
    duracionDiasInternacion=.31,
    fechaTransicion=as.Date("2021-11-01"),
    periodoTransicion=30,
    porcentajeCasosCriticosCalibrador=1,
    porcentajeCasosGravesCalibrador=1,
    ifrCalibrador=1.3,
    transmission_probabilityCalibrador=0.52
  ),
  delta = list(
    transmision=4,
    hospitalizacion=.44,
    critico=.33,
    muerte=.3,
    duracionInmumidad=.8,
    modVacTransmision=1.45,
    modVacGrave=2.277,
    modVacCritico=2.652,
    modVacMuerte=3.174,
    duracionDiasInternacion=.31,
    fechaTransicion=as.Date("2021-03-20"),
    periodoTransicion=30,
    porcentajeCasosCriticosCalibrador=1,
    porcentajeCasosGravesCalibrador=1,
    ifrCalibrador=1.3,
    transmission_probabilityCalibrador=0.52
  )
  
)

getCalibracion <- function(country,variante) {
  if (is.null(calibradores[[variante]][[country]])) {
    return(calibradorDefault[[variante]])
  } else {
    return(calibradores[[variante]][[country]])
  }
}

