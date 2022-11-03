# Efecto variantes c(transmision, casos moderados, graves, muerte) con respecto a alpha + efectividad de inmunidad
# fechas_master <<- seq(as.Date("2020-01-01"),
#                       as.Date("2024-01-01"),by=1)

ageGroups <<- c("0-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
immunityStates <<- c("No immunity", "Recovered", "1Dosis", "2Dosis")
matrixNames <<- list(immunityStates,
                     ageGroups)

getMatrizModificadoresVariantesSingle <<- function(v) {
  return (matrix(data=c(
    rep(v,length(ageGroups)),
    rep(v,length(ageGroups)),
    rep(v,length(ageGroups)),
    rep(v,length(ageGroups)) 
  ), nrow=length(immunityStates), ncol=length(ageGroups), byrow=T, dimnames = matrixNames))
}

getMatrizModificadoresVariantesSingleVac <<- function(v) {
  return (matrix(data=c(
    rep(1,length(ageGroups)),
    rep(1,length(ageGroups)),
    rep(v,length(ageGroups)),
    rep(v,length(ageGroups)) 
  ), nrow=length(immunityStates), ncol=length(ageGroups), byrow=T, dimnames = matrixNames))
}

sinModificacion = getMatrizModificadoresVariantesSingle(1)
modificadorCero = getMatrizModificadoresVariantesSingle(0)

modificadores <<- c('transmision',
                  'hospitalizacion',
                  'critico',
                  'muerte',
                  'duracionInmumidad',
                  'modVacTransmision',
                  'modVacGrave',
                  'modVacCritico',
                  'modVacMuerte',
                  'duracionDiasInternacion')
variantes <<- list(
  'alpha' = setNames(lapply(seq_len(10), function(X) sinModificacion),modificadores),
  'beta' = setNames(lapply(seq_len(10), function(X) sinModificacion),modificadores),
  'gamma' = setNames(lapply(seq_len(10), function(X) sinModificacion),modificadores),
  # AGREGAR TRANSICION DELTA: 
  # - Crear las variables en getCalibración
  # - Agregar esas variables aca en 'delta'
  # - en transicionesEpidemiologicasCountry crear la progresion de la transición para delta
  'delta' = setNames(list(getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"delta")[['transmision']]),
                          getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"delta")[['hospitalizacion']]),
                          getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"delta")[['critico']]),
                          getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"delta")[['muerte']]),
                          getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"delta")[['duracionInmumidad']]),
                          getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"delta")[['modVacTransmision']]),
                          getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"delta")[['modVacGrave']]),
                          getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"delta")[['modVacCritico']]),
                          getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"delta")[['modVacMuerte']]),
                          getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"delta")[['duracionDiasInternacion']])
                          )
                          ,modificadores),
  'omicron' = setNames(list(getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"omicron")[['transmision']]),
                            getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"omicron")[['hospitalizacion']]),
                            getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"omicron")[['critico']]),
                            getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"omicron")[['muerte']]),
                            getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"omicron")[['duracionInmumidad']]),
                            getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"omicron")[['modVacTransmision']]),
                            getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"omicron")[['modVacGrave']]),
                            getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"omicron")[['modVacCritico']]),
                            getMatrizModificadoresVariantesSingleVac(getCalibracion(iso_country,"omicron")[['modVacMuerte']]),
                            getMatrizModificadoresVariantesSingle(getCalibracion(iso_country,"omicron")[['duracionDiasInternacion']])
                            ),
                       modificadores)
)

variantes_base <<- variantes
fechaTransicion <<- getCalibracion(iso_country,"omicron")[["fechaTransicion"]]
periodoTransicion <<- getCalibracion(iso_country,"omicron")[["periodoTransicion"]]


obtenerModificadorDeVariante <<- function(t,iso_country) {
  
  
  # AGREGAR TRANSICION DELTA: 
  # - Tomas las variables fechaTransicionOmicron y periodoTransicionOmicron del pais iso_country 
  #    - reemplazar getCalibracion(iso_country,"omicron")[["fechaTransicionOmicron"]] abajo para buscarlo aca en la funcion
  # - Generar la para esos valores transición diferente
  # browser()
  
  fechaTransicionDelta <<- getCalibracion(iso_country,"delta")[["fechaTransicion"]]
  periodoTransicionDelta <<- getCalibracion(iso_country,"delta")[["periodoTransicion"]]
  
  tTransicionDelta <- which(fechas_master==fechaTransicionDelta)
  
  fechas_curva_delta <<- seq(fechaTransicionDelta,
                             fechaTransicionDelta+periodoTransicionDelta,by=1)
  
  fechaTransicionOmicron <<- getCalibracion(iso_country,"omicron")[["fechaTransicion"]]
  periodoTransicionOmicron <<- getCalibracion(iso_country,"omicron")[["periodoTransicion"]]
  
  tTransicionOmicron <- which(fechas_master==fechaTransicionOmicron)
  
  fechas_curva_omicron <<- seq(fechaTransicionOmicron,
                       fechaTransicionOmicron+periodoTransicionOmicron,by=1)
  
  
  # Transiciones epidemiologicas de cada variante c(fecha predominante, % cada variante)
  
  transicionesEpidemiologicasCountry <- 
    if (iso_country=="ARG") {
      list(
        '2020-03-01' = c(1),
        '2020-06-10' = c(0,1),
        '2021-01-01' = c(0,0,1)
      )
    } else {
      list(
        '2020-03-01' = c(1),
        '2020-06-10' = c(0,1),
        '2021-01-01' = c(0,0,1)
      )
    }
  
  #browser()
  valoresCurvaDelta = seq(from = 0, to = 1, by = (1/periodoTransicionDelta))
  row = 0
  for (dia in as.character(fechas_curva_delta)) {
    row = row + 1
    transicionesEpidemiologicasCountry[[dia]] = c(0,0,1-valoresCurvaDelta[row],valoresCurvaDelta[row])
  }
  
  valoresCurvaOmicron = seq(from = 0, to = 1, by = (1/periodoTransicionOmicron))
  row = 0
  for (dia in as.character(fechas_curva_omicron)) {
    row = row + 1
    transicionesEpidemiologicasCountry[[dia]] = c(0,0,0,1-valoresCurvaOmicron[row],valoresCurvaOmicron[row])
  }
  
  transicionesEpidemiologicas <<- list(
    transicionesEpidemiologicasCountry)
  
  names(transicionesEpidemiologicas) <- iso_country
  
  modificador = setNames(lapply(seq_len(10), function(X) sinModificacion),modificadores)
  if (is.null(transicionesEpidemiologicas[[iso_country]]) == F) {
    for (fechaTransicionVariante in names(transicionesEpidemiologicas[[iso_country]])) {

      if (length(which(fechas_master == as.Date(fechaTransicionVariante)))==0) {
        tFecha <- fechas_master[1]
      } else {
        tFecha <- which(fechas_master == as.Date(fechaTransicionVariante))
      }
      
      if (t>=tFecha) {
        col = 0
        modificador = setNames(lapply(seq_len(10), function(X) modificadorCero),modificadores)
        for (proporcion in  transicionesEpidemiologicas[[iso_country]][[fechaTransicionVariante]]) {
          col = col + 1
          # print(paste(t,col))
          # browser( expr = { t == 303 })
          valorProp = lapply(variantes[[col]],"*",proporcion)
          modificador = Map("+", modificador, valorProp)
          # modificador = modificador + proporcion *  variantes[[col]]
        }
      }
    }
  }
  # browser(expr = {t == 400})
  return(modificador)
}

