library(dplyr)
library(reshape)

##### funcion update #####
update <-  function(pais,diasDeProyeccion) {
  countryData <- list()
  if (pais=="ARG") {
    
    # url <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.zip'
    # download.file(url, "Covid19Casos.zip")
    # unzip("Covid19Casos.zip")
    # file.remove('Covid19Casos.zip')
    # file.remove('datos_nomivac_covid19.csv')
    
    data <- read.csv("Covid19Casos.csv", 
                     fileEncoding = "UTF-8")
    
    
    
    casos <- data %>% dplyr::filter(clasificacion_resumen=="Confirmado") %>%
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
                    
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                              as.Date(max(casos$fecha)),
                                              by=1))) %>% left_join(casos) 
    
    casos[is.na(casos)] <- 0
    
    
    
    
    def <- data %>% dplyr::filter(clasificacion_resumen=="Confirmado" &
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
      
      def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                              as.Date(max(def$fecha)),
                              by=1))) %>% left_join(def) 
    
      def[is.na(def)] <- 0
      
      # download.file('https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip', 'datos_nomivac_covid19.zip')
      # unzip('datos_nomivac_covid19.zip','datos_nomivac_covid19.csv')
      dataVacunas = read.csv2('datos_nomivac_covid19.csv', sep=',', encoding = 'UTF-8')
      # file.remove('datos_nomivac_covid19.csv')
      # file.remove('datos_nomivac_covid19.zip')
      
      Vacunas = dataVacunas
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

      
      Vacunas <- data.frame(fecha_aplicacion=seq(as.Date(min(Vacunas$fecha)),
                                                 as.Date(max(Vacunas$fecha)),
                                                 by=1)) %>% left_join(Vacunas) 
      
      Vacunas[is.na(Vacunas)] <- 0
      
      Vacunas2 = dataVacunas
      Vacunas2 = Vacunas2 %>% dplyr::filter(orden_dosis==2 &
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
      
      
      Vacunas2 <- data.frame(fecha_aplicacion=seq(as.Date(min(Vacunas2$fecha)),
                                                  as.Date(max(Vacunas2$fecha)),
                                                  by=1)) %>% left_join(Vacunas2) 
      
      Vacunas2[is.na(Vacunas2)] <- 0
      
      
      
      
      eval(parse(text=paste0('countryData$',
                             pais,
                             ' <- list(def=def,vac=Vacunas, vac2=Vacunas2, casos=casos)')))

      
  } # cierra if de ARG
  
  print(paste('Actualizado:',pais, Sys.Date()))
  save(countryData,file=paste0('data/data',pais,'.RData'))
}


##### funcion formatData #####

formatData <- function(pais, ageGroups) {

  load(paste0("data/data",pais,".RData"))
  eval(parse(text=paste0('countryData$FMTD <- countryData$',pais)))
  
  
  fechasDef <- countryData$ARG$def$fecha
  fechasVac <- countryData$ARG$vac$fecha
  fechasVac2 <- countryData$ARG$vac2$fecha
  fechasCasos <- countryData$ARG$casos$fecha
  ageGSel <- colnames(countryData$FMTD$def)[substring(colnames(countryData$FMTD$def),1,2) %in% ageGroups]
  ageSelCol <- which(colnames(countryData$FMTD$def) %in% ageGSel)
  
  # casos
  group <- c()
  vector <- c()
  
  for (i in 1:length(ageSelCol)) {
    if (i!=length(ageSelCol)) {
      vector <- c(vector,rowSums(countryData$FMTD$casos[,ageSelCol[i]:(ageSelCol[(i+1)]-1)]))
    } else
    {
      vector <- c(vector,rowSums(data.frame(countryData$FMTD$casos[,ageSelCol[i]:(ncol(countryData$FMTD$casos))])))
    }
  }
  
  countryData$FMTD$casos <- data.frame((matrix(vector, ncol=length(ageSelCol))))
  countryData$FMTD$casos <- cbind(fechasCasos,countryData$FMTD$casos)
  substring(ageGSel[length(ageGSel)],4,5) <- "99"
  colnames(countryData$FMTD$casos) <- c("fecha",ageGSel)
  countryData$FMTD$casos$fecha <- as.Date(as.character(countryData$FMTD$casos$fecha))
  
  
  # muertes
  group <- c()
  vector <- c()
  
  for (i in 1:length(ageSelCol)) {
    if (i!=length(ageSelCol)) {
      vector <- c(vector,rowSums(countryData$FMTD$def[,ageSelCol[i]:(ageSelCol[(i+1)]-1)]))
    } else
    {
      vector <- c(vector,rowSums(data.frame(countryData$FMTD$def[,ageSelCol[i]:(ncol(countryData$FMTD$def))])))
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
      
      vector <- c(vector,rowSums(data.frame(countryData$FMTD$vac[,ageSelCol[i]:(ncol(countryData$FMTD$vac))])))
    }
  }
  
  countryData$FMTD$vac <-  data.frame(matrix(vector, ncol=length(ageSelCol)))
  countryData$FMTD$vac <-  cbind(fechasVac,countryData$FMTD$vac)
  substring(ageGSel[length(ageGSel)],4,5) <- "99"
  colnames(countryData$FMTD$vac) <- c("fecha",ageGSel)
  
  
  # vacunas2
  ageGSel <- colnames(countryData$FMTD$vac2)[substring(colnames(countryData$FMTD$vac2),1,2) %in% ageGroups]
  ageSelCol <- which(colnames(countryData$FMTD$vac2) %in% ageGSel)
  
  label <- c()
  group <- c()
  vector <- c()
  
  for (i in 1:length(ageSelCol)) {
    
    if (i!=length(ageSelCol)) {
      
      vector <- c(vector,rowSums(data.frame(countryData$FMTD$vac2[,ageSelCol[i]:(ageSelCol[(i+1)]-1)])))
    } else
    {
      
      vector <- c(vector,rowSums(data.frame(countryData$FMTD$vac2[,ageSelCol[i]:(ncol(countryData$FMTD$vac2))])))
    }
  }
  
  countryData$FMTD$vac2 <-  data.frame(matrix(vector, ncol=length(ageSelCol)))
  countryData$FMTD$vac2 <-  cbind(fechasVac2,countryData$FMTD$vac2)
  substring(ageGSel[length(ageGSel)],4,5) <- "99"
  colnames(countryData$FMTD$vac2) <- c("fecha",ageGSel)
  
  return(countryData)
}


# actualiza argentina y guarda RData
# update(pais = "ARG", diasDeProyeccion = 1100)

# agrupa edades
# datosArg <- formatData("ARG", ageGroups = ageGroupsV)



