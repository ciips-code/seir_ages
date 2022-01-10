# Efecto variantes c(transmision, casos moderados, graves, muerte) con respecto a alpha + efectividad de inmunidad
# fechas_master <<- seq(as.Date("2020-01-01"),
#                       as.Date("2024-01-01"),by=1)

variantes <<- list(
  'alpha' = c(1,1,1,1,1),
  'beta' = c(1,1,1,1,1),
  'gamma' = c(1,1,1,1,1),
  'delta' = c(1,1,1,1,1),
  'omicron' = c(1.3,0.5,0.3,0.1,1)
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
  modificador = c(1,1,1,1,1)
  if (is.null(transicionesEpidemiologicas[[iso_country]]) == F) {
    for (fechaTransicion in names(transicionesEpidemiologicas[[iso_country]])) {
      tFecha <- which(fechas_master == as.Date(fechaTransicion))
      if (t>=tFecha) {
        col = 0
        modificador = c(0,0,0,0,0)
        for (proporcion in  transicionesEpidemiologicas[[iso_country]][[fechaTransicion]]) {
          col = col + 1
          modificador = modificador + proporcion *  variantes[[col]]
        }
      }
    }
  }
  return(modificador)
}

