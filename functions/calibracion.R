getCalibracion <- function(country) {
  calibradores <- list(
    ARG = list(
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
      fechaTransicionOmicron=as.Date("2021-11-01"),
      periodoTransicionOmicron=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    ),
    COL = list(
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
      fechaTransicionOmicron=as.Date("2021-09-01"),
      periodoTransicionOmicron=30,
      porcentajeCasosCriticosCalibrador=1,
      porcentajeCasosGravesCalibrador=1,
      ifrCalibrador=1.3,
      transmission_probabilityCalibrador=0.52
    )
  )
  
  
  calibradorDefault <- list (
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
    fechaTransicionOmicron=as.Date("2021-11-01"),
    periodoTransicionOmicron=30,
    porcentajeCasosCriticosCalibrador=1,
    porcentajeCasosGravesCalibrador=1,
    ifrCalibrador=1.3,
    transmission_probabilityCalibrador=0.52
  )
  if (is.null(calibradores[[country]])) {
    return(calibradorDefault)
  } else {
    return(calibradores[[country]])
  }
}

