# Efecto variantes c(transmision, casos moderados, graves, muerte) con respecto a alpha
variantes <<- list(
  'alpha' = c(1,1,1,1),
  'beta' = c(1,1,1,1),
  'gamma' = c(1,1,1,1),
  'delta' = c(1.5,1.5,1.5,1.5),
  'omicron' = c(2,0.5,0.3,0.1)
)

# Transiciones epidemiologicas de cada variante c(fecha predominante, % cada variante)
transicionesEpidemiologicasArg =  list(
  '2020-03-01' = c(1),
  '2020-06-10' = c(0,1),
  '2021-01-01' = c(0,0,1),
  '2021-06-01' = c(0,0,0,1),
  '2021-10-15' = c(0,0,0,.8,.2),
  '2021-11-15' = c(0,0,0,.2,.8),
  '2021-12-31' = c(0,0,0,0,1)
)


transicionesEpidemiologicas <<- list(
  'ARG' = transicionesEpidemiologicasArg
)

# fechas_master <<- seq(as.Date("2020-01-01"),
#                       as.Date("2024-01-01"),by=1)

obtenerModificadorDeVariante <<- function(t,iso_country) {
  modificador = c(1,1,1,1)
  if (is.null(transicionesEpidemiologicas[[iso_country]]) == F) {
    for (fechaTransicion in names(transicionesEpidemiologicas[[iso_country]])) {
      tFecha <- which(fechas_master == as.Date(fechaTransicion))
      if (t>=tFecha) {
        col = 0
        modificador = c(0,0,0,0)
        for (proporcion in  transicionesEpidemiologicas[[iso_country]][[fechaTransicion]]) {
          col = col + 1
          modificador = modificador + proporcion *  variantes[[col]]
        }
      }
    }
  }
  return(modificador)
}