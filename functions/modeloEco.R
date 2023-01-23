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
        labor_income_women <<- c(11318,0,40035,11880,28011,19563,11533,10270,17855,30079,11353,24386,22593,23108,10424,7048),
        labor_income_men <<- c(15254,46230,50602,20085,26846,13236,16364,13553,21451,32887,17792,27755,24605,30345,15374,5665),
        labor_income_poor <<- c(751,1761,3352,4202,2816,2975,2771,3416,3756,1997,4931,3585,4184,4538,3843,4994),
        labor_income_rich <<- c(58336,39153,28977,33012,27870,33508,66815,30647,27870,28319,21809,25521,27468,21317,25502,16037),
        rich_share <<- c(0.45273,0.6785,0.8502,0.5745,0.7399,0.50334,0.4936,0.4423,0.00108,0.794,0.510204082,0.766477083,0.771248179,0.738245614,0.429699842,0.180845372),
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
        labor_income_women <<- c(4698,1,14743,4418,9417,6844,4816,3765,6586,10721,4214,9624,8931,9739,3852,2671),
        labor_income_men <<- c(6332,18477,18634,7470,9026,4630,6834,4968,7912,11722,6604,10953,9727,12789,5681,2147),
        labor_income_poor <<- c(312,704,1234,1563,947,1041,1157,1252,1385,712,1830,1415,1654,1913,1420,1892),
        labor_income_rich <<- c(24214,15648,10671,12278,9370,11722,27902,11235,10280,10094,8095,10072,10858,8984,9423,6077),
        rich_share <<- c(0.54,0.74,0.91,0.64,0.78,0.60,0.57,0.52,0.09,0.84,0.60,0.77,0.79,0.80,0.51,0.21),
        output_poverty <<- c(-0.48,30)    
      ),
      JAM=list(
        coef_mob <- c(-18.83097,-0.0040916,2.21e-07,0.3649423,-0.0045154,3.292909*2,0.0004495*2),
        w_star <- c(0.14,0.005,0.06,0.005,0.09,0.19,0.09,0.062,0.02,0.08,0.06,0.06,0.03,0.06,0.05,0.002),
        w_star <- w_star*781024,
        hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5),
        output_pw <- c(0.28,0.28,0.24,9.63,0.21,0.21,1.15,0.64,2.98,0.88,1.11,0.38,0.38,0.38,0.38,22.84),
        gender_sector <<- NA,
        labor_income <<- NA,        
        labor_income_women <<- NA,
        labor_income_men <<- NA,
        labor_income_poor <<- NA,
        labor_income_rich <<- NA,
        rich_share <<- NA,
        output_poverty <<- NA
      ),
      MEX=list(
        coef_mob <- c(-18.94503,0.000216,-1.92e-08,-4.284018,0.2115661,4.599035*2,0.0005458*2),
        w_star <- c(0.03,0.00,0.05,0.17,0.02,0.08,0.17,0.1,0.03,0.07,0.16,0.01,0.01,0.09,0.00,0.01),
        w_star <- w_star* 23682200789000/19.8,
        hours_mod <- c(0,0,0.5,0.75,0.25,0.75,1,1,0.75,0.5,0.5,1,1,0,0,0.5),
        output_pw <- c(0.09,0.09,3.14,0.35,0.31,0.31,0.33,0.11,0.62,1.11,1.11,0.32,0.32,0.16,0.16,0.08),
        gender_sector <<-c(0.9361,1.0000,0.9307,0.7680,0.9557,1.0000,0.6152,0.6041,0.7954,0.5604,0.7329,0.5692,0.2871,0.2991,0.3548,0.0278),
        labor_income <<- c(4689.90,14608.55,15709.12,5574.72,8534.96,4251.05,4566.93,3781.57,6515.17,10003.49,4862.27,8284.39,7273.01,8048.68,4164.43,2156.39),
        labor_income_women <<-c(3553.68,0,12636.53,3753.99,8865.75,6232.03,3696.16,3164.02,5667.43,9575.71,3667.00,7740.60,7209.50,7289.86,3255.19,2232.30),
        labor_income_men <<-c(4836.63,14586.68,16053.00,6300.60,8513.80,4115.51,5126.77,4292.04,6791.57,10398.94,5654.89,8716.93,7781.34,9579.23,4843.05,1745.14),
        labor_income_poor <<- c(237.25,532.81,1074.91,1371.45,942.99,1028.78,874.28,1061.51,1117.16,605.74,1580.39,1166.50,1360.42,1445.83,1213.93,1570.37),
        labor_income_rich <<- c(18501.19,12396.86,9228.28,10481.45,8805.59,10645.20,21097.48,9696.49,8882.60,9001.53,6891.66,8116.96,8681.29,6702.54,8087.55,5116.24),
        rich_share <<- c(0.435,0.669,0.868,0.573,0.723,0.488,0.508,0.439,0.001,0.803,0.511,0.739,0.772,0.776,0.396,0.184),
        output_poverty <<- c(-0.458,26.961)
      )
    )
  lapply(seq_along(ecoParams), function (i) {
    names(ecoParams[[i]]) <<- c("coef_mob",
                                "w_s",
                                "w_star",
                                "hours_mod",
                                "output_pw",
                                "gender_sector",
                                "labor_income",
                                "labor_income_women",
                                "labor_income_men",
                                "labor_income_poor",
                                "labor_income_rich",
                                "rich_share",
                                "output_poverty")
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
  labor_income_men_star <- sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw  * ecoParams[[iso_country]]$labor_income_men * ecoParams[[iso_country]]$gender_sector / sum(ecoParams[[iso_country]]$gender_sector))
  labor_income_men_t <- sum(w_t* ecoParams[[iso_country]]$output_pw * ecoParams[[iso_country]]$labor_income_men * ecoParams[[iso_country]]$gender_sector / sum(ecoParams[[iso_country]]$gender_sector))
  change_labor_men <-  labor_income_men_t/labor_income_men_star-1
  labor_income_women_star <- sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw  * ecoParams[[iso_country]]$labor_income_women * (1-ecoParams[[iso_country]]$gender_sector) / sum((1-ecoParams[[iso_country]]$gender_sector)))
  labor_income_women_t <- sum(w_t* ecoParams[[iso_country]]$output_pw * ecoParams[[iso_country]]$labor_income_women * (1-ecoParams[[iso_country]]$gender_sector) / sum((1-ecoParams[[iso_country]]$gender_sector)))
  
  # este es un fix que hice porque al haber un cero daba NA y se romp?a en la divisi?n
  clm <- log(labor_income_women_t)-log(labor_income_women_star)
  clm[is.na(clm)] <- 0
  
  change_labor_women <-  clm
  labor_income_rich_star <- sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw * ecoParams[[iso_country]]$labor_income_rich * ecoParams[[iso_country]]$rich_share)
  labor_income_rich_t <- sum(w_t* ecoParams[[iso_country]]$output_pw * ecoParams[[iso_country]]$labor_income_rich * ecoParams[[iso_country]]$rich_share)
  change_labor_rich <-  log(labor_income_rich_t)-log(labor_income_rich_star)
  labor_income_poor_star <- sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw * ecoParams[[iso_country]]$labor_income_poor * (1-ecoParams[[iso_country]]$rich_share))
  labor_income_poor_t <- sum(w_t* ecoParams[[iso_country]]$output_pw * ecoParams[[iso_country]]$labor_income_poor * (1-ecoParams[[iso_country]]$rich_share))
  change_labor_poor <-  log(labor_income_poor_t)-log(labor_income_poor_star)
  
  list(change_labor_men=change_labor_men, 
       change_labor_women=change_labor_women,
       change_labor_rich=change_labor_rich,
       change_labor_poor=change_labor_poor,
       change_gender=change_gender,
       GDP_change=GDP_change,
       poverty_change=poverty_change,
       ecoParams[[iso_country]]$w_star,
       w_t,
       y_t,
       y_star
  )
  
}

# loss_t <- function(work_mob_t) {
#   # work_mob_t <- 0.5086785
#   # iso_country = "ARG"
#   w_t <- ecoParams[[iso_country]]$w_star * ( 1 - ecoParams[[iso_country]]$hours_mod * (1 - work_mob_t)) 
#   ## Output
#   # y* is the pre-pandemic (calibrated for 2019)
#   y_star <-  sum(ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$output_pw)
#   ## Effective output (direct impact)
#   y_t <- sum(w_t * ecoParams[[iso_country]]$output_pw)
#   ## Economic loss per period
#   
#   gender_star <- 1-sum(ecoParams[[iso_country]]$gender_sector * ecoParams[[iso_country]]$w_star )
#   
#   gender_t <- 1-sum(ecoParams[[iso_country]]$gender_sector * w_t )
#   
#   change_gender <- gender_t - gender_star
#   GDP_change <- y_t/y_star -1
#   poverty_change <- sum(ecoParams[[iso_country]]$output_poverty * c(1,GDP_change))
#   
#   
#   labor_income_men_star <- ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$labor_income_men * ecoParams[[iso_country]]$gender_sector
#   labor_income_men_t <- w_t * ecoParams[[iso_country]]$labor_income_men * ecoParams[[iso_country]]$gender_sector
#   
#   change_labor_men <-  mean( labor_income_men_t/labor_income_men_star-1)
#   
#   labor_income_women_star <- ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$labor_income_women * (1-ecoParams[[iso_country]]$gender_sector)
#   labor_income_women_t <- w_t * ecoParams[[iso_country]]$labor_income_women * (1-ecoParams[[iso_country]]$gender_sector)
#   
#   clm <- labor_income_women_t/labor_income_women_star-1
#   clm[is.na(clm)] <- 0
#   change_labor_women <-  mean(clm)
#   
#   
#   labor_income_rich_star <- ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$labor_income_rich * ecoParams[[iso_country]]$rich_share
#   labor_income_rich_t <- w_t * ecoParams[[iso_country]]$labor_income_rich * ecoParams[[iso_country]]$rich_share
#   
#   change_labor_rich <-  mean(labor_income_rich_t/labor_income_rich_star-1)
#   
#   labor_income_poor_star <- ecoParams[[iso_country]]$w_star * ecoParams[[iso_country]]$labor_income_poor * (1-ecoParams[[iso_country]]$rich_share)
#   labor_income_poor_t <- w_t * ecoParams[[iso_country]]$labor_income_poor * (1-ecoParams[[iso_country]]$rich_share)
#   
#   change_labor_poor <-  mean(labor_income_poor_t/labor_income_poor_star-1)
#   
#   list(change_labor_men, 
#        change_labor_women,
#        change_labor_rich,
#        change_labor_poor,
#        change_gender,
#        GDP_change,
#        poverty_change
#   )
#   
# }


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









