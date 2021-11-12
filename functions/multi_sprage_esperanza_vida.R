## create Sprague multiplier data


#' Sprague index (multipliers)
#' 
#' Using the Sprague multipliers, the age counts are estimated for each year
#' having 5-years interval data as input.
#' 
#' The input is population counts of age classes 0-4, 5-9, 10-14, ... , 77-74,
#' 75-79, 80+.
#' 
#' @name sprague
#' @param x numeric vector of age counts in five-year intervals
#' @return Population counts for age 0, 1, 2, 3, 4, ..., 78, 79, 80+.
#' @author Matthias Templ
#' @seealso \code{\link{whipple}}
#' @references G. Calot and J.-P. Sardon.  Methodology for the calculation of
#' Eurostat's demographic indicators.  Detailed report by the European
#' Demographic Observatory
#' 
#' @keywords arith
#'
#'  @export
#' @examples
#'
#' 

#### Funcion multiplicadores sprague
sprague <- function(x){
  if(length(x) != 18) stop("input writen for original sprague with age groups 0-4,
  5-9, 10-14, ... ,77-74,75-79,80-84,85+")
  breaks=c(seq(0,85,5),150)
  if(x==FALSE){
    x <- cut(x, breaks, right=FALSE)
    Ns <- table(x) #arma tabla de doble entrada con flags 1 y 0 s/poblacion
  } else {
    Ns <- x
    if( length(Ns) != 19 ) {
      warning("input table with five-yers age groups expected but not provided.")
    }
  }
  
  plus85 <- Ns[length(Ns)]
  
  
  multipliers <- data.frame(G1 = c(0.3616, 0.264, 0.184, 0.12, 0.0704,
                                   0.0336, 0.008, -0.008, -0.016, -0.0176,#semiextremo
                                   -0.0128,  -0.0016, 0.0064, 0.0064, 0.0016,#central
                                   rep(0,10)),
                            G2 = c(-0.2768,  -0.096, 0.04, 0.136, 0.1968,
                                   0.2272, 0.232, 0.216, 0.184, 0.1408,#semiextremo
                                   0.0848, 0.0144, -0.0336, -0.0416, -0.024,#central
                                   -0.0144,  -0.008, 0, 0.008, 0.0144,
                                   0.0176, 0.016, 0.008, -0.008, -0.0336),
                            G3 = c(0.1488, 0.04, -0.032, -0.072, -0.0848,
                                   -0.0752, -0.048, -0.008, 0.04, 0.0912,
                                   0.1504, 0.2224, 0.2544, 0.2224, 0.1504,
                                   0.0912, 0.04, -0.008, -0.048,  -0.0752,
                                   -0.0848, -0.072,  -0.032, 0.04, 0.1488),
                            G4 = c(-0.0336, -0.008, 0.008,  0.016, 0.0176,
                                   0.0144,  0.008, 0, -0.008, -0.0144,
                                   -0.024,  -0.0416, -0.0336,  0.0144, 0.0848,
                                   0.1408,  0.184,  0.216, 0.232, 0.2272,
                                   0.1968, 0.136,  0.04, -0.096, -0.2768),
                            G5 = c(0,  0, 0,  0, 0,
                                   0,  0, 0, 0, 0,
                                   0.0016,  0.0064, 0.0064, -0.0016, -0.0128,
                                   -0.0176, -0.016, -0.008, 0.008, 0.0336,
                                   0.0704, 0.12,  0.184, 0.264, 0.3616)
  )
  multipliers <- cbind(groups=rep(c("lowest","low","normal","high", "highest"),
                                  each=5), multipliers)
  
  # 27 years:
  ## correspond to n3 since it is
  ## in group 25,26,27,28,29
  infoGroup <- function(n, mult=multipliers, mybreaks=breaks, popN=Ns){
    ## from a five years group, which one of the five years:
    groups <- (n) %% 5
    ## extreme group or normal:
    if(n < 5){
      tab <- subset(mult, subset=groups=="lowest")
    } else if(n >= 5 & n < 10){
      tab <- subset(mult, subset=groups=="low")
    } else if(n >= 75 & n < 80){
      tab <- subset(mult, subset=groups=="high")
    } else if(n >= 79){
      tab <- subset(mult, subset=groups=="highest")
    } else{
      tab <- subset(mult, subset=groups=="normal")
    }
    ## my groups:
    ng <- cut(n, mybreaks, right=FALSE)
    mygroup <- which(levels(ng) %in% ng)
    ## cohort
    rowsm <- tab[groups+1,2:ncol(tab)]
    if(mygroup==1){
      s <- seq(mygroup,mygroup+4,1)
    } else if(mygroup==2){
      s <- seq(mygroup-1, mygroup+3,1)
    } else if(mygroup==17){
      s <- seq(mygroup-4,mygroup,1)
    } else if(mygroup==16){
      s <- seq(mygroup-3,mygroup+1,1)
    } else{
      s <- seq(mygroup-2,mygroup+2, 1)
    }
    
    
    cohort <- sum(rowsm * popN[s])
    return(cohort)
  }
  
  cohorts <- sapply(0:84, infoGroup)
  ## group 80+
  cohorts <- c(cohorts, plus85)
  names(cohorts) <- c(0:84,"85+")
  return(cohorts)
}


  
####################### pOBLACIONES POR PAISES ############################
     
#' ## example from ARGENTINA
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

#argentina
pop=c(199174, #agrupe 0-4 de argentina 
      99072,  98993,98890,98566,
      98087,  97617,97083,96352,
      95400,  93926,91697,88160,
      82742,  75056,64396,51267, 34851
))


#' ## example from COLOMBIA
x <- data.frame(age=as.factor(c(
  "0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(198870, #agrupo de 0 a 4
      98681,      98573,
      98436,      98004,
      97399,      96815,
      96181,      95525,
      94747,      93749,
      92318,      90021,
      86622,      81790,
      74084,      61729,
      46050
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))


#' ## example from CHILE
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(199367,
      99268,      99206,
      99127,      98926,
      98636,      98290,
      97899,      97408,
      96681,     95637,
      93983,      91499,
      88124,     83040,
      74427,      63001,
      47798
      
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))


#' ## example from PERU
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(199046,
      98748,      98612,
      98468,      98208,
      97823,      97347,
      96796,      96128,
      95263,      94102,
      92460,      90109,
      86636,      81500,
      74034,      63019,
      48580
      
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))



#' ## example from BRASIL
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(198771,
      98621,      98525,
      98388,      97765,
      96947,      96202,
      95440,      94480,
      93264,      91568,
      89174,      85706,
      80721,      73831,
      64275,      51970,
      37161
      
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))


#' ## example from URUGUAY
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(199460,
      99332,      99275,
      99193,      98859,
      98270,      97709,
      97095,      96279,
      95213,      93760,
      91554,      88071,
      82655,      75205,
      64644,      51287,
      35503
      
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))


#' ## example from MEXICO
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(198772,
      98575,      98467,
      98325,      97977,
      97421,      96745,
      95981,      95007,
      93676,      91939,
      89387,      85735,
      80684,      73333,
      63852,      51225,
      34911
      
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))



#' ## example from COSTA RICA
x <- data.frame(age=as.factor(c(
  "0-4",
  "5-9","10-14","15-19", "20-24",
  "25-29","30-34","35-39","40-44","45-49",
  "50-54","55-59","60-64","65-69","77-74","75-79","80-84","85+")
),

pop=c(199240,
      99132,      99061,
      98964,      98680,
      98154,      97603,
      97055,      96372,
      95511,      94374,
      92900,      90725,
      86989,      82557,
      75854,      66158,
      51926
      
))


# SPRAGUE
s  <- sprague(x[,2])
y= as.data.frame(s)
y

#control
all.equal(sum(s), sum(x[,2]))
