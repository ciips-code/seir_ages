setEcoParametersCountry <- function () {
  ecoParams <<- 
    list(
      ARG=list(
        coef_mob <<- c(-34.12556,-0.0018216,-7.11e-08,-4.741735,0.2079539,10.36272*2,-0.0003383*2),
        w_star <- c(0.065,0.002,0.007,0.133,0.006,0.088,0.206,0.035,0.075,0.015,0.069,0.086,0.053,0.060,0.050,0.051),
        w_star <- w_star * 32548570.3923792,
        hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5),
        output_pw <- c(0.49,1.17,3.75,0.76,1.99,0.29,0.48,0.38,0.50,1.33,0.90,0.56,0.63,0.52,0.35,0.08),
        gender_sector <<- c(0.866,1,0.916,0.698,0.87,0.97,0.596,0.515,0.725,0.498,0.633,0.563,0.26,0.333,0.551,0.041),
        labor_income <<- c(14725,46230,49716,17611,26998,13426,14414,11961,20463,31476,15427,26284,23116,25516,13150,6991),
        output_poverty <<- c(-0.48,27)
      ),
      BRA=list(
        coef_mob <- c(-15.86268,0.0033494,-1.57e-07,-4.327578,0.1852589,2.546838*2,0.000391*2),
        w_star <- c(0.09,0.004,0.004,0.144,0.004,0.067,0.18,0.037,0.048,0.013,0.061,0.05,0.05,0.037,0.041,0.074),
        w_star <- w_star*1608832340000,
        hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5),
        output_pw <- c(0.09,004,1.2,0.14,1.26,0.1,0.12,0.16,0.16,0.93,0.27,0.29,0.29,0.27,0.25,0.14),
        gender_sector <<- c(0.767,1,0.86,0.62,0.81,0.95,0.53,0.49,0.68,0.48,0.61,0.52,0.27,0.30,0.51,0.02),
        labor_income <<- c(4908,15409,16572,5870,8999,4475,4804,3986,6820,10492,5142,8761,7705,8505,4383,2330),
        output_poverty <<- c(-0.48,30)
      ),
      JAM=list(
        coef_mob <- c(-18.83097,-0.0040916,2.21e-07,0.3649423,-0.0045154,3.292909*2,0.0004495*2),
        w_star <- NA,
        w_star <- NA,
        hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5),
        output_pw <- NA,
        gender_sector <<- NA,
        labor_income <<- NA,
        output_poverty <<- NA
      ),
      MEX=list(
        coef_mob <- c(-18.94503,0.000216,-1.92e-08,-4.284018,0.2115661,4.599035*2,0.0005458*2),
        w_star <- c(0.03,0.00,0.05,0.17,0.02,0.08,0.17,0.1,0.03,0.07,0.16,0.01,0.01,0.09,0.00,0.01),
        w_star <- w_star* 23682200789000/19.8,
        hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5),
        output_pw <- c(0.09,0.09,3.14,0.35,0.31,0.31,0.33,0.11,0.62,1.11,1.11,0.32,0.32,0.16,0.16,0.08),
        gender_sector <<- NA,
        labor_income <<- NA,
        output_poverty <<- NA
      )
    )
  lapply(seq_along(ecoParams), function (i) {
    names(ecoParams[[i]]) <<- c("coef_mob","w_s","w_star","hours_mod","output_pw","gender_sector","labor_income","output_poverty")
  })
  return(print("Setting eco parameters: done"))
}

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
  return(min(max(sum(vars*ecoParams[[iso_country]]$coef_mob),-80),0))
  
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

# loss_t <- function(work_mob_t) {
#   w_t <- ecoParams[[iso_country]]$w_star * ( 1 - ecoParams[[iso_country]]$hours_mod * (1 - work_mob_t)) 
#   ## Output
#   # y* is the pre-pandemic (calibrated for 2019)
#   y_star <-  sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw)
#   ## Effective output (direct impact)
#   y_t <- sum(w_t * ecoParams[[iso_country]]$output_pw)
#   ## Economic loss per period
#   return(y_t/y_star -1)
# }

loss_t <- function(work_mob_t) {
  w_t <- ecoParams[[iso_country]]$w_star * ( 1 - ecoParams[[iso_country]]$hours_mod * (1 - work_mob_t)) 
  ## Output
  # y* is the pre-pandemic (calibrated for 2019)
  y_star <-  sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw)
  ## Effective output (direct impact)
  y_t <- sum(w_t * ecoParams[[iso_country]]$output_pw)
  ## Economic loss per period
  
  gender_star <- 1-sum(ecoParams[[iso_country]]$gender_sector * ecoParams[[iso_country]]$w_star )
  
  gender_t <- 1-sum(ecoParams[[iso_country]]$gender_sector * w_t )
  
  change_gender <- gender_t - gender_star
  GDP_change <- y_t/y_star -1
  poverty_change <- sum(ecoParams[[iso_country]]$output_poverty * c(1,GDP_change))
  
  
  ## change return
  # return(y_t/y_star -1)
  
  return(
    list(change_gender,GDP_change,poverty_change)
  )
}

matchDavies <- function(NPIs) {
  val <- NPIs
  for (i in 1:length(NPIs)) {
    val[i] <- if (NPIs[i]=="Physical distancing") {0} else
              if (NPIs[i]=="Physical distancing + Shielding of older people") {0} else
              if (NPIs[i]=="Physical distancing + Shielding of older people + Self isolation") {1} else
              if (NPIs[i]=="Physical distancing + Shielding of older people + Self isolation + School closures") {2} else
              if (NPIs[i]=="Physical distancing + Shielding of older people + Lockdown + School closures") {3}
  }
  lengthVal <- length(val)
  if (lengthVal<12) {
    val <- c(val,rep(tail(val,1),12-lengthVal))
  }
  as.numeric(val)
}

trade_off_scenarios <- function () {
  list(scenario_1 <- rep('Physical distancing',12),
       scenario_2 <- rep('Physical distancing + Shielding of older people',12),
       scenario_3 <- rep('Physical distancing + Shielding of older people + Self isolation',12),
       scenario_4 <- rep('Physical distancing + Shielding of older people + Self isolation + School closures',12),
       scenario_5 <- rep('Physical distancing + Shielding of older people + Lockdown + School closures',12)
  )
}

trade_off_scenarios()









