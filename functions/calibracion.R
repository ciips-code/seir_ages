getCalibracion <- function(country) {
  calibradores <- list(
    ARG = list(
      calibrador1 = 0.2,
      calibrador2 = 34
    ),
    CHL = list(
      calibrador1 = 0.2,
      calibrador2 = 34
    )
  )
  
  calibradorDefault <- list (
    calibrador1 = 1,
    calibrador2 = 1
  )
  
  if (is.na(calibradores[[country]])) {
    return(calibradorDefault)
  } else {
    return(calibradores[[country]])
  }
}