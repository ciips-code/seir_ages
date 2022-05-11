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
  'delta' = setNames(lapply(seq_len(10), function(X) sinModificacion),modificadores),
  'omicron' = setNames(list(getMatrizModificadoresVariantesSingle(4),
                            getMatrizModificadoresVariantesSingle(0.44),
                            getMatrizModificadoresVariantesSingle(0.33),
                            getMatrizModificadoresVariantesSingle(0.3),
                            getMatrizModificadoresVariantesSingle(0.8),
                            getMatrizModificadoresVariantesSingleVac(1.45),
                            getMatrizModificadoresVariantesSingleVac(2.277),
                            getMatrizModificadoresVariantesSingleVac(2.652),
                            getMatrizModificadoresVariantesSingleVac(3.174),
                            getMatrizModificadoresVariantesSingle(0.31)
                            ),
                       modificadores)
)

fechaTransicionOmicron <<- as.Date("2021-11-01")
periodoTransicionOmicron <<- 30

obtenerModificadorDeVariante <<- function(t,iso_country) {
  tTransicionOmicron <- which(fechas_master==fechaTransicionOmicron)
  fechas_curva <<- seq(fechaTransicionOmicron,
                       fechaTransicionOmicron+periodoTransicionOmicron,by=1)
  
  
  # Transiciones epidemiologicas de cada variante c(fecha predominante, % cada variante)
  transicionesEpidemiologicasArg =  list(
    '2020-03-01' = c(1),
    '2020-06-10' = c(0,1),
    '2021-01-01' = c(0,0,1),
    '2021-06-01' = c(0,0,0,1)
  )
  
  valoresCurva = seq(from = 0, to = 1, by = (1/periodoTransicionOmicron))
  row = 0
  for (dia in as.character(fechas_curva)) {
    row = row + 1
    transicionesEpidemiologicasArg[[dia]] = c(0,0,0,1-valoresCurva[row],valoresCurva[row])
  }
  
  transicionesEpidemiologicas <<- list(
    'ARG' = transicionesEpidemiologicasArg,
    "BRA" = transicionesEpidemiologicasArg,
    "CHL" = transicionesEpidemiologicasArg,
    "COL" = transicionesEpidemiologicasArg,
    "PER" = transicionesEpidemiologicasArg,
    "MEX" = transicionesEpidemiologicasArg,
    "CRI" = transicionesEpidemiologicasArg
  )
  modificador = setNames(lapply(seq_len(10), function(X) sinModificacion),modificadores)
  
  if (is.null(transicionesEpidemiologicas[[iso_country]]) == F) {
    for (fechaTransicion in names(transicionesEpidemiologicas[[iso_country]])) {

      if (length(which(fechas_master == as.Date(fechaTransicion)))==0) {
        tFecha <- fechas_master[1]
      } else {
        tFecha <- which(fechas_master == as.Date(fechaTransicion))
      }
      
      if (t>=tFecha) {
        col = 0
        modificador = setNames(lapply(seq_len(10), function(X) modificadorCero),modificadores)
        for (proporcion in  transicionesEpidemiologicas[[iso_country]][[fechaTransicion]]) {
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
  return(modificador)
}

