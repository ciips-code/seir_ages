# Efecto variantes c(transmision, casos moderados, graves, muerte) con respecto a alpha + efectividad de inmunidad
# fechas_master <<- seq(as.Date("2020-01-01"),
#                       as.Date("2024-01-01"),by=1)


getMatrizModificadoresVariantes <<- function(t,h,c,m,i) {
  return (matrix(data=c(
    rep(t,length(ageGroups)), # Modificador de transmision
    rep(h,length(ageGroups)), # Modificador de hospitalizacion
    rep(c,length(ageGroups)), # Modificador de criticos
    rep(m,length(ageGroups)), # Modificador de muerte
    rep(i,length(ageGroups)) # Modificador de inmunidad (?)
  ), nrow=length(immunityStates), ncol=length(ageGroups), byrow=T, dimnames = matrixNames))
}

getMatrizModificadoresVariantesSingle <<- function(v) {
  return (matrix(data=c(
    rep(v,length(ageGroups)), # Modificador de transmision
    rep(v,length(ageGroups)), # Modificador de hospitalizacion
    rep(v,length(ageGroups)), # Modificador de criticos
    rep(v,length(ageGroups)), # Modificador de muerte
    rep(v,length(ageGroups)) # Modificador de inmunidad (?)
  ), nrow=length(immunityStates), ncol=length(ageGroups), byrow=T, dimnames = matrixNames))
}

sinModificacion = getMatrizModificadoresVariantesSingle(1)
modificadorCero = getMatrizModificadoresVariantesSingle(0)

modificadores = c('transmision','hospitalizacion','critico','muerte','innumidad')

variantes <<- list(
  'alpha' = setNames(list(sinModificacion,sinModificacion,sinModificacion,sinModificacion,sinModificacion),modificadores),
  'beta' = setNames(list(sinModificacion,sinModificacion,sinModificacion,sinModificacion,sinModificacion),modificadores),
  'gamma' = setNames(list(sinModificacion,sinModificacion,sinModificacion,sinModificacion,sinModificacion),modificadores),
  'delta' = setNames(list(sinModificacion,sinModificacion,sinModificacion,sinModificacion,sinModificacion),modificadores),
  'omicron' = setNames(list(getMatrizModificadoresVariantesSingle(1.365),getMatrizModificadoresVariantesSingle(0.44),
                            getMatrizModificadoresVariantesSingle(0.33),getMatrizModificadoresVariantesSingle(0.16),sinModificacion),modificadores)
)

obtenerModificadorDeVariante <<- function(t,iso_country) {
  fechaTransicionOmicron <- as.Date("2021-11-01")
  tTransicionOmicron <- which(fechas_master==fechaTransicionOmicron)
  periodoTransicionOmicron <- 45
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
    'ARG' = transicionesEpidemiologicasArg
  )
  modificador = setNames(list(sinModificacion,sinModificacion,sinModificacion,sinModificacion,sinModificacion),modificadores)
  if (is.null(transicionesEpidemiologicas[[iso_country]]) == F) {
    for (fechaTransicion in names(transicionesEpidemiologicas[[iso_country]])) {
      tFecha <- which(fechas_master == as.Date(fechaTransicion))
      if (t>=tFecha) {
        col = 0
        modificador = setNames(list(modificadorCero,modificadorCero,modificadorCero,modificadorCero,modificadorCero),modificadores)
        for (proporcion in  transicionesEpidemiologicas[[iso_country]][[fechaTransicion]]) {
          col = col + 1
          print(paste(t,col))
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

