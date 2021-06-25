library(dplyr)
library(reshape)

##### funcion update #####
update <-  function(pais,diasDeProyeccion) {
  countryData <- list()
  if (pais=="ARG") {
    
    # url <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip'
    # download.file(url, "Covid19Casos.zip")
    # unzip("Covid19Casos.zip")
    #file.remove('Covid19Casos.zip')
    #file.remove('datos_nomivac_covid19.csv')

    data <- read.csv("Covid19Casos.csv", 
                     fileEncoding = "UTF-8")
    
    data <- data %>% dplyr::filter(clasificacion_resumen=="Confirmado" &
                                   fallecido=="SI") %>%
                     dplyr::select(edad,
                                   edad_años_meses,
                                   fecha_fallecimiento,
                                   fecha_diagnostico,
                                   fecha_apertura,
                                   fecha_inicio_sintomas,
                                   clasificacion_resumen) %>%
                     dplyr::mutate(fecha=dplyr::coalesce(na_if(fecha_fallecimiento,""),
                                                         na_if(fecha_diagnostico,""),
                                                         na_if(fecha_apertura,""),
                                                         na_if(fecha_inicio_sintomas,""))) %>%
                     dplyr::mutate(gredad=case_when(edad_años_meses=="Meses" | edad_años_meses=="Años" & edad>=1 & edad <=4 ~ "00-04",
                                                    edad_años_meses=="Años" & edad>=5 & edad <=9 ~ "05-09",
                                                    edad_años_meses=="Años" & edad>=10 & edad <=14 ~ "10-14",
                                                    edad_años_meses=="Años" & edad>=15 & edad <=17 ~ "15-17",
                                                    edad_años_meses=="Años" & edad>=18 & edad <=24 ~ "18-24",
                                                    edad_años_meses=="Años" & edad>=25 & edad <=29 ~ "25-29",
                                                    edad_años_meses=="Años" & edad>=30 & edad <=34 ~ "30-34",
                                                    edad_años_meses=="Años" & edad>=35 & edad <=39 ~ "35-39",
                                                    edad_años_meses=="Años" & edad>=40 & edad <=44 ~ "40-44",
                                                    edad_años_meses=="Años" & edad>=45 & edad <=49 ~ "45-49",
                                                    edad_años_meses=="Años" & edad>=50 & edad <=54 ~ "50-54",
                                                    edad_años_meses=="Años" & edad>=55 & edad <=59 ~ "55-59",
                                                    edad_años_meses=="Años" & edad>=60 & edad <=64 ~ "60-64",
                                                    edad_años_meses=="Años" & edad>=65 & edad <=69 ~ "65-69",
                                                    edad_años_meses=="Años" & edad>=70 & edad <=74 ~ "70-74",
                                                    edad_años_meses=="Años" & edad>=75 & edad <=79 ~ "75-79",
                                                    edad_años_meses=="Años" & edad>=80 & edad <=84 ~ "80-84",
                                                    edad_años_meses=="Años" & edad>=85 & edad <=89 ~ "85-89",
                                                    edad_años_meses=="Años" & edad>=90 & edad <=110 ~ "90-99",
                                                    TRUE ~ "S.I.")) %>%
                     dplyr::mutate(cuenta=1) %>% 
                     reshape::cast(fecha~gredad, sum) %>%
                     dplyr::select(-`S.I.`)
        
      #download.file('https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip', 'datos_nomivac_covid19.zip')
      #unzip('datos_nomivac_covid19.zip','datos_nomivac_covid19.csv')
      Vacunas = read.csv2('datos_nomivac_covid19.csv', sep=',', encoding = 'UTF-8')
      #file.remove('datos_nomivac_covid19.csv')
      #file.remove('datos_nomivac_covid19.zip')

      Vacunas = Vacunas %>% dplyr::filter(orden_dosis==1 &
                                          fecha_aplicacion!="S.I.") %>%
                            dplyr::mutate(edad = case_when(grupo_etario %in% c(">=100") ~ "90-99",
                                                           TRUE ~ grupo_etario)) %>%
                            dplyr::filter(edad!="S.I.") %>%
                            dplyr::group_by(fecha_aplicacion, edad) %>%
                            dplyr::summarise(n=n()) %>%
                            reshape::cast(fecha_aplicacion~edad, mean) %>%
                            dplyr::mutate(`00-17`= 0) %>%
                            dplyr::select(fecha_aplicacion,`00-17`,`18-29`,`30-39`,`40-49`,`50-59`,`60-69`,`70-79`,`80-89`,`90-99`) %>%
                            dplyr::mutate(fecha_aplicacion=as.Date(fecha_aplicacion))

      Vacunas[is.na(Vacunas)] <- 0
      
      eval(parse(text=paste0('countryData$',
                             pais,
                             ' <- list(def=data,vac=Vacunas)')))

      #load("vacunasArg.RData")
      
      # diaCeroVac <- min(VacunasArg$fecha_aplicacion)
      # 
      # tVacunasCero <-  as.Date(diaCeroVac)-as.Date(min(data$fecha))
      # 
      # vacPre = lapply(1:(as.numeric(tVacunasCero)-1), matrix, data=c(0,0,0,
      #                                                                0,0,0,
      #                                                                0,0,0), 
      #                                                         nrow=3, 
      #                                                         ncol=ncol(VacunasArg)-1)
      # 
      # vacArg = lapply(1:nrow(VacunasArg), matrix, data=c(0,0,0,
      #                                                    0,0,0,
      #                                                    0,0,0), 
      #                 nrow=3, 
      #                 ncol=ncol(VacunasArg)-1)
      # 
      # for (t in 1:length(vacArg)) {
      #   vacArg[[t]][3,]  = as.numeric(VacunasArg[t,2:4])
      # }
      # 
      # promedio = round(Reduce(`+`, vacArg[(length(vacArg)-8):(length(vacArg)-1)])/7,0)
      # vacPlan = lapply(1:(diasDeProyeccion-length(vacArg)-length(vacPre)), matrix, data=t(promedio), 
      #                  nrow=3, 
      #                  ncol=3)
      # 
      # Av = c(vacPre,vacArg,vacPlan)
      # return(result=list(Av=Av,
      #                    diaPlan=length(vacPre)+length(vacArg)))
      #           
      
  } # cierra if de ARG
  print(paste('Actualizado:',pais, Sys.Date()))
  save(countryData,file=paste0('data/data',pais,'.RData'))
}


##### funcion formatData #####

formatData <- function(pais, ageGroups) {

  load(paste0("data/data",pais,".RData"))
  eval(parse(text=paste0('countryData$FMTD <- countryData$',pais)))
  
  # muertes
  fechasDef <- countryData$ARG$def$fecha
  fechasVac <- countryData$ARG$vac$fecha
  ageGSel <- colnames(countryData$FMTD$def)[substring(colnames(countryData$FMTD$def),1,2) %in% ageGroups]
  ageSelCol <- which(colnames(countryData$FMTD$def) %in% ageGSel)
  group <- c()
  vector <- c()
  
  for (i in 1:length(ageSelCol)) {
    if (i!=length(ageSelCol)) {
      vector <- c(vector,rowSums(countryData$FMTD$def[,ageSelCol[i]:(ageSelCol[(i+1)]-1)]))
    } else
    {
      vector <- c(vector,rowSums(countryData$FMTD$def[,ageSelCol[i]:(ncol(countryData$FMTD$def))]))
    }
  }
 
  countryData$FMTD$def <- data.frame((matrix(vector, ncol=length(ageSelCol))))
  countryData$FMTD$def <- cbind(fechasDef,countryData$FMTD$def)
  substring(ageGSel[length(ageGSel)],4,5) <- "99"
  colnames(countryData$FMTD$def) <- c("fecha",ageGSel)
  countryData$FMTD$def$fecha <- as.Date(as.character(countryData$FMTD$def$fecha))
  # vacunas
  ageGSel <- colnames(countryData$FMTD$vac)[substring(colnames(countryData$FMTD$vac),1,2) %in% ageGroups]
  ageSelCol <- which(colnames(countryData$FMTD$vac) %in% ageGSel)
  
  label <- c()
  group <- c()
  vector <- c()
  
  for (i in 1:length(ageSelCol)) {
    if (i!=length(ageSelCol)) {
      vector <- c(vector,rowSums(data.frame(countryData$FMTD$vac[,ageSelCol[i]:(ageSelCol[(i+1)]-1)])))
    } else
    {
      vector <- c(vector,rowSums(countryData$FMTD$vac[,ageSelCol[i]:(ncol(countryData$FMTD$vac))]))
    }
  }
  
  countryData$FMTD$vac <-  data.frame(matrix(vector, ncol=length(ageSelCol)))
  countryData$FMTD$vac <-  cbind(fechasVac,countryData$FMTD$vac)
  substring(ageGSel[length(ageGSel)],4,5) <- "99"
  colnames(countryData$FMTD$vac) <- c("fecha",ageGSel)
  return(countryData)
}


# actualiza argentina y guarda RData
# update(pais = "ARG",
#        diasDeProyeccion = 900)

# agrupa edades
# datosArg <- formatData("ARG")



