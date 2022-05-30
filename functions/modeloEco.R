setEcoParameters <- function () {
 workplace_closure <<- c(2,2,2,2,2,2,2,2,2,2,1,1,1)
 coef_mob <<- c(-42.17619,-0.0005177,5.89e-08,-3.673106,0.15,15.33772*2,-0.0004777*2)
 
 ## Pre-pandemic (equilibrium) level of working hours for each sector
 w_star <- c(0.065,0.002,0.007,0.133,0.006,0.088,0.206,0.035,0.075,0.015,0.069,0.086,0.053,0.060,0.050,0.051)
 w_star <- w_star * 32548570.3923792
 
 ## Working hours modifier
 # Measures the sensitivity of working hours to mobility
 # Depends on the capability to switch efforts to remote work
 hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5)
 
 # output_per_worker
 # measured in ARS mn
 output_pw <- c(0.49,1.17,3.75,0.76,1.99,0.29,0.48,0.38,0.50,1.33,0.90,0.56,0.63,0.52,0.35,0.08)
 
}

# getModificadorActividadLaboral <<- function(t, muertesTmenosUno, country) {
#   # acumuladoDeImpactoEco <<- acumuladoDeImpactoEco + 1
#   # fatiga = 0.5
#   # return(fatiga)
#   # browser(expr = { t == 672 })
#   workingHoursValue = NA
#   if (country == "ARG" && t > 0) {
#     workingHours$month <- str_pad(workingHours$month,2,"left",0)
#     workingHoursValue <- workingHours$workingHours[workingHours$year==substring(fechas_master[t],1,4) &
#                                                      workingHours$month==substring(fechas_master[t],6,7)]
#     if (length(workingHoursValue) == 0) {
#       workingHoursValue = NA
#     }
#   }
#   return(workingHoursValue)
#   
# }


# getModificadorActividadLaboral(which(fechas_master=="2021-01-01"))




################

# Initial parameters



# initial parameter is for Dec/20

# Regression result

# Google_mobility = workplace_closure + l.deaths + l.deaths^2 + time + time^2 + timexclosure

# workplace_closure:  -49.82972
# l.deaths:   -0.0013974
# l.deaths^2: 6.43e-08   
# time: -3.967755
# time_2:
# timexclosure: 18.5069
# death_closure:


## Effective working hours

work_mob <- function(t, w_closure,deaths) {
  año <- substring(fechas_master[t+1],1,4)
  if (is.na(año) | año == '2021') {
    return(1+workplace_mob(t,w_closure,deaths)/100)
  } else {
    return(NA)
  }
}

# This function estimates the workplace mobility

workplace_mob <- function(t, w_closure,deaths) {
  mes <- as.numeric(substring(fechas_master[t+1],6,7))
  time <- min(mes + 12,24)
  vars <- c(w_closure[mes],deaths,deaths^2,time,time^2,log(time,10)*w_closure[mes],deaths*w_closure[mes])  
  return(min(max(sum(vars*coef_mob),-80),0))
  
}

# The inputs for this function are:
# time: measured in months of the year (starting at Jan/21 = 0)
# w_closure \in {0,1,2,3}
# deaths: from last month

# Fatigue function
# depends on the first step regression between the mobility and workplace_closure
# 
# fatigue <- function(time, w_closure,deaths) {
#   (workplace_mob(time, w_closure,deaths) -19.1425*w_closure) + 100
# }


## Economic cost

# sector_shares <<- c(0.058944527,0.003700592,0.041402887,0.162506549,0.020351264,0.047133354,0.167688444,0.024221756,0.064220244,0.042451056,0.115679348,0.088448044,0.062985801,0.059689505,0.032556379,0.00802025)
# there are 16 sectors; all sector share add to 1

## Sector composition (#16)

# A	Agriculture and animal production
# B	Fishing and aquaculture
# C	Mining and quarrying
# D	Manufacture industry
# E	Electricity, gas, and water supply
# F	Construction
# G	Wholesale and retail trade
# H	Hotels, accomodations and restaurants
# I	Transportation, storing and communication
# J	Financial intermediation
# K	Real estate
# L	Public administration and defense
# M	Education
# N	Social security and health services
# O	Social work activities and others
# P	Household activities


# pre-pandemic workforce (in thousand hours per year)
# calibrated for 2019


# Example. Agro (sector 1) does not respond to a reduction on the workplace mobility


loss_t <- function(work_mob_t) {
  w_t <- w_star * ( 1 - hours_mod * (1 - work_mob_t)) 
  ## Output
  # y* is the pre-pandemic (calibrated for 2019)
  y_star <-  sum(w_star * output_pw)
  ## Effective output (direct impact)
  y_t <- sum(w_t * output_pw)
  ## Economic loss per period
  return(y_t/y_star -1)
}














