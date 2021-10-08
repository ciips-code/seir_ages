library(stringr)
library(plyr)
library(dplyr)

# archivos que se actualizan

archivo_dosis1 <- "output/producto88/vacunacion_fabricantes_edad_1eraDosis.csv"
archivo_dosis2 <- "output/producto88/vacunacion_fabricantes_edad_2daDosis.csv"
archivo_dosis0 <- "output/producto88/vacunacion_fabricantes_edad_UnicaDosis.csv"

# crea funcion para actualizar
actualizaArchivo <- function (archivo,dosis) {
shell("git switch master")
shell("git pull")
shell(paste0("git log --  ", archivo, " > hist.txt"))

hist <- read.csv("hist.txt", encoding = "UTF-8", header=F)[,1] 
hist <- data.frame(commit=hist[grep("commit",hist)],
                   date=hist[grep("Date",hist)])

hist$date <- str_replace_all(hist$date,"Date:","")

for (i in 1:nrow(hist)) {
  hist$date[i] <-  paste(str_split(hist$date[i]," ")[[1]][8],
                         str_pad(match(str_split(hist$date[i]," ")[[1]][5],month.abb),2,"left","0"),
                         str_pad(str_split(hist$date[i]," ")[[1]][6],2,"left","0"),
                         sep='-')
}

hist$date <- as.Date(hist$date)
hist$commit <- str_remove(hist$commit,"commit ")

# crea lista con aplicaciones acumuladas
Vacunas <- list()

for (i in nrow(hist)) {
  commit <- hist[i,1]
  date <- hist[i,2]
  shell(paste("git checkout", commit))
  data <- data.frame(read.csv(archivo, encoding="UTF-8"))
  data$date <- date
  data$dosis <- dosis
  Vacunas$dosis0[[paste0('`',date,'`')]] <- data  
  
}

shell("git checkout HEAD")
#browser()
dataVac <- Reduce(plyr::rbind.fill,Vacunas$dosis0)
dataVac$Fabricante <- str_replace_all(dataVac$Fabricante,'Campaña SARS-CoV-2','')
dataVac$Fabricante <- str_replace_all(dataVac$Fabricante,'\\(','')
dataVac$Fabricante <- str_replace_all(dataVac$Fabricante,'\\)','')
dataVac$Fabricante <- str_replace_all(dataVac$Fabricante,' ','')
dataVac <- dataVac[dataVac$Fabricante!="",]
dataVac
}

# actualiza
dosis1 <- actualizaArchivo(archivo_dosis1,1)
dosis2 <- actualizaArchivo(archivo_dosis2,2)
dosis0 <- actualizaArchivo(archivo_dosis0,0)

# crea funcion que agrupa edades en columnas

gruposEdad <- function (archivo) {
  for (e in c("00-17",
              "18-29",
              "30-39",
              "40-49",
              "50-59",
              "60-69",
              "70-79",
              "80-89",
              "90-99")) {
  
  col <- colnames(archivo)[substring(colnames(archivo),1,1)=="X" &
                          as.numeric(substring(colnames(archivo),2,4)) >= as.numeric(substring(e,1,2)) &
                          as.numeric(substring(colnames(archivo),2,4)) <= as.numeric(substring(e,4,5))]
  
  col <- paste(paste0("archivo$",col),collapse="+")
  
  archivo[e] <- eval(parse(text=col))
  }
  
  archivo <- archivo[,c((ncol(archivo)-10),
                      1,
                      (ncol(archivo)-9),
                      (ncol(archivo)-8):ncol(archivo))]

  archivo %>% dplyr::arrange(date) 
  archivo <- cbind(archivo[,1:3],sapply(archivo[,4:12], simplify = T,  function(x) {coalesce(x,0)}))
  
  archivo
}

# agrupa edades
dosis1 <- gruposEdad(dosis1)
dosis2 <- gruposEdad(dosis2)
dosis0 <- gruposEdad(dosis0)

# crea funcion para valores diarios

diarios <- function (archivo) {
  archivo <- archivo %>% dplyr::arrange(date)
  for (f in unique(archivo$Fabricante)) {
    for (i in 2:nrow(archivo[archivo$Fabricante==f,])) {
      for (v in colnames(archivo[4:12])) {
        #browser(expr = {f=="Pfizer" & v=="50-59"})
        archivo[archivo$Fabricante==f,v][i] = archivo[archivo$Fabricante==f,v][i] - sum(archivo[archivo$Fabricante==f,v][1:(i-1)])
      }
    }
  }
  archivo
}

dosis1 <- diarios(dosis1)
dosis2 <- diarios(dosis2)
dosis0 <- diarios(dosis0)

# distribuye en dias

distDias <- function (archivo) {
  archivo <- archivo %>% 
    dplyr::filter(is.na(date)==F) %>%
    dplyr::group_by(fecha_aplicacion=date) %>%
    summarise_at(colnames(archivo)[4:ncol(archivo)],sum) %>% as.data.frame()
  
  archivo$rep=T
  archivo <- data.frame(fecha_aplicacion=seq(min(archivo$fecha_aplicacion),
                                              max(archivo$fecha_aplicacion),
                                              by=1)) %>% left_join(archivo) 
  
  archivo$row <- NA
  archivo$row <- row_number(archivo$rep==T)
  archivo$rep[is.na(archivo$rep)] <- F
  
  for (i in c(2:nrow(archivo))) {
    
    if (is.na(archivo$row[i]) & archivo$rep[i-1]==T) {archivo$row[i]=archivo$row[i-1]+1} else
      if (is.na(archivo$row[i]) & archivo$rep[i-1]==F) {archivo$row[i]=archivo$row[i-1]}  
  }
  
  for (i in c(1:nrow(archivo))) {
    if (nrow(archivo[archivo$row==i,])>1) {
      
      n_rows <- nrow(archivo[archivo$row==i,])  
      archivo[archivo$row==i,][n_rows,2:10] <- archivo[archivo$row==i,][n_rows,2:10] / n_rows 
      for (ii in (1):(n_rows-1)) {
        archivo[archivo$row==i,][ii,2:10] <- as.numeric(archivo[archivo$row==i,2:10][n_rows,])
      }  
    }
  }
  archivo[,1:10]
}

Vacunas1 <- distDias(dosis1)
Vacunas2 <- distDias(dosis2)
Vacunas0 <- distDias(dosis0)

save(Vacunas1,Vacunas2,Vacunas0, file="chile.Rdata")
