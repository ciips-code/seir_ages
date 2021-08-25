library(dplyr)
library(data.table)
library(stringr)

# cases
url <- 'https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto16/CasosGeneroEtario.csv'
data <- read.csv(url, encoding = 'UTF-8')

casos <- data[,-2] %>% dplyr::group_by(Grupo.de.edad) %>%
                       dplyr::summarise(across(everything(), list(sum)))

casos$Grupo.de.edad <- str_remove_all(casos$Grupo.de.edad,'años')
casos$Grupo.de.edad <- str_remove_all(casos$Grupo.de.edad,' ')
casos <- transpose(casos, keep.names = "col", make.names = "Grupo.de.edad")
casos$`80-84` <- round(casos$`80ymás`/2,0) 
casos$`85-99` <- casos$`80-84`
casos$`80ymás` <- NULL
casos$col <- str_replace_all(casos$col,'_1','')
casos$col <- str_replace_all(casos$col,'X','')
casos$col <- str_replace_all(casos$col, '\\.','-')
colnames(casos)[1] <- "fecha"
casos$rep=T
casos$fecha <- as.Date(casos$fecha)

casos <- data.frame(fecha=seq(min(casos$fecha[is.na(casos$fecha)==F]),
                              max(casos$fecha[is.na(casos$fecha)==F]),
                              by=1)) %>% left_join(casos) 

casos$row <- NA
casos$row <- row_number(casos$rep==T)
casos$rep[is.na(casos$rep)] <- F

for (i in c(2:nrow(casos))) {
  
  if (is.na(casos$row[i]) & casos$rep[i-1]==T) {casos$row[i]=casos$row[i-1]+1} else
  if (is.na(casos$row[i]) & casos$rep[i-1]==F) {casos$row[i]=casos$row[i-1]}  
}

for (i in c(1:nrow(casos))) {
  if (nrow(casos[casos$row==i,])>1) {
    n_rows <- nrow(casos[casos$row==i,])  
    casos[casos$row==i,][n_rows,2:19] <- casos[casos$row==i,][n_rows,2:19] / n_rows 
    for (ii in (1):(n_rows-1)) {
      casos[casos$row==i,][ii,2:19] <- as.numeric(casos[casos$row==i,2:19][n_rows,])
    }  
  }
}

casos[2:19] <- sapply(casos[2:19],function(x) round(x, digits = 0))
casos$rep <- NULL
casos$row <- NULL

# deaths

url <- 'https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto10/FallecidosEtario_T.csv'
data <- read.csv(url, encoding = 'UTF-8')
data$`30-34`[1] <- data$X..39[1]/2
data$`35-39`[1] <- data$X..39[1]/2
data$`40-44`[1] <- data$X40.49[1]/2
data$`45-49`[1] <- data$X40.49[1]/2
data$`50-54`[1] <- data$X50.59[1]/2
data$`55-59`[1] <- data$X50.59[1]/2
data$`60-64`[1] <- data$X60.69[1]/2
data$`65-69`[1] <- data$X60.69[1]/2
data$`70-74`[1] <- data$X70.79[1]/2
data$`75-79`[1] <- data$X70.79[1]/2
data$`80-84`[1] <- data$X80.89[1]/2
data$`85-89`[1] <- data$X80.89[1]/2
data$`90-99`[1] <- data$X..90[1]

for (i in c(2:nrow(data))) {
  data$`30-34`[i] <- (data$X..39[i]-data$X..39[i-1])/2
  data$`35-39`[i] <- (data$X..39[i]-data$X..39[i-1])/2
  data$`40-44`[i] <- (data$X40.49[i]-data$X40.49[i-1])/2
  data$`45-49`[i] <- (data$X40.49[i]-data$X40.49[i-1])/2
  data$`50-54`[i] <- (data$X50.59[i]-data$X50.59[i-1])/2
  data$`55-59`[i] <- (data$X50.59[i]-data$X50.59[i-1])/2
  data$`60-64`[i] <- (data$X60.69[i]-data$X60.69[i-1])/2
  data$`65-69`[i] <- (data$X60.69[i]-data$X60.69[i-1])/2
  data$`70-74`[i] <- (data$X70.79[i]-data$X70.79[i-1])/2
  data$`75-79`[i] <- (data$X70.79[i]-data$X70.79[i-1])/2
  data$`80-84`[i] <- (data$X80.89[i]-data$X80.89[i-1])/2
  data$`85-89`[i] <- (data$X80.89[i]-data$X80.89[i-1])/2
  data$`90-99`[i] <- (data$X..90[i]-data$X..90[i-1])
  }

# aplica distribucion de ARG
load("data/dataARG.RData")
dist <- as.numeric(sapply(countryData$ARG$def[,2:9] %>% as.data.frame, function (x) sum(x))/sum(sapply(countryData$ARG$def[,2:9] %>% as.data.frame, function (x) sum(x))))
data$para_dist <- data$`30-34`+data$`35-39`
data$`00-04` <- dist[1]*data$para_dist
data$`05-09` <- dist[2]*data$para_dist
data$`10-14` <- dist[3]*data$para_dist
data$`15-19` <- dist[4]*data$para_dist
data$`20-24` <- dist[5]*data$para_dist
data$`25-29` <- dist[6]*data$para_dist
data$`30-34` <- dist[7]*data$para_dist
data$`35-39` <- dist[8]*data$para_dist
rm(countryData)

data <- data %>% select(fecha=Grupo.de.edad,`00-04`,`05-09`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-99`)
data$fecha <- as.Date(data$fecha)


def <- left_join(data.frame(fecha=seq(min(casos$fecha),
                                      max(casos$fecha),
                                      by=1)), data)

def[is.na(def)] <- 0

rm(data)

# vacunas



