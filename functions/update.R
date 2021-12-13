library(dplyr)
library(reshape)
#library(archive)
#library(RSocrata)
library(plyr)
library(zip)
library(stringr)
library(data.table)

##### test date format function #####
is_date = function(x, format) {
  formatted = try(as.Date(x, format), silent = TRUE)
  return(as.character(formatted) == x)
}

##### update function #####
update <-  function(pais,diasDeProyeccion) {
  countryData <- list()
  ##### ARGENTINA #####
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
                      dplyr::mutate(fecha=dplyr::coalesce(na_if(fecha_inicio_sintomas,""),
                                                          na_if(fecha_diagnostico,""),
                                                          na_if(fecha_apertura,""))) %>%
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
      
      
      
      
      

      
  } # cierra if de ARG
  
  ##### PERU #####
  
  if (pais=="PER") {
    
    url <- 'https://cloud.minsa.gob.pe/s/AC2adyLkHCKjmfm/download'
    download.file(url, "covidPeru.csv")
    casos <- read.csv2("covidPeru.csv", encoding = "UTF-8")
    
    casos <- casos %>% dplyr::mutate(gredad=case_when(EDAD>=1 & EDAD <=4 ~ "00-04",
                                                      EDAD>=5 & EDAD <=9 ~ "05-09",
                                                      EDAD>=10 & EDAD <=14 ~ "10-14",
                                                      EDAD>=15 & EDAD <=17 ~ "15-17",
                                                      EDAD>=18 & EDAD <=24 ~ "18-24",
                                                      EDAD>=25 & EDAD <=29 ~ "25-29",
                                                      EDAD>=30 & EDAD <=34 ~ "30-34",
                                                      EDAD>=35 & EDAD <=39 ~ "35-39",
                                                      EDAD>=40 & EDAD <=44 ~ "40-44",
                                                      EDAD>=45 & EDAD <=49 ~ "45-49",
                                                      EDAD>=50 & EDAD <=54 ~ "50-54",
                                                      EDAD>=55 & EDAD <=59 ~ "55-59",
                                                      EDAD>=60 & EDAD <=64 ~ "60-64",
                                                      EDAD>=65 & EDAD <=69 ~ "65-69",
                                                      EDAD>=70 & EDAD <=74 ~ "70-74",
                                                      EDAD>=75 & EDAD <=79 ~ "75-79",
                                                      EDAD>=80 & EDAD <=84 ~ "80-84",
                                                      EDAD>=85 & EDAD <=89 ~ "85-89",
                                                      EDAD>=90 & EDAD <=110 ~ "90-99",
                                                      TRUE ~ "S.I."), 
                                     fecha=as.Date(paste(substring(FECHA_RESULTADO,1,4),
                                                         substring(FECHA_RESULTADO,5,6),
                                                         substring(FECHA_RESULTADO,7,8), 
                                                         sep = '-'))) %>%
                                     dplyr::mutate(cuenta=1) %>% 
                                     reshape::cast(fecha~gredad, sum) %>%
                                     dplyr::select(-`S.I.`) %>%
                                     dplyr::filter(is.na(fecha)==F)
    
    casos <- data.frame(fecha=seq(min(casos$fecha),
                                  max(casos$fecha),
                                  by=1)) %>% left_join(casos)
    
    casos[is.na(casos)] <- 0
    
    
    url <- 'https://www.datosabiertos.gob.pe/node/6460/download'
    download.file(url, "covidPeru_DEF.csv")
    def <- read.csv2("covidPeru_DEF.csv", encoding = "UTF-8")
    
    
    def <- def %>% dplyr::mutate(grupedad=case_when(EDAD_DECLARADA>=1 & EDAD_DECLARADA <=4 ~ "00-04",
                                                    EDAD_DECLARADA>=5 & EDAD_DECLARADA <=9 ~ "05-09",
                                                    EDAD_DECLARADA>=10 & EDAD_DECLARADA <=14 ~ "10-14",
                                                    EDAD_DECLARADA>=15 & EDAD_DECLARADA <=17 ~ "15-17",
                                                    EDAD_DECLARADA>=18 & EDAD_DECLARADA <=24 ~ "18-24",
                                                    EDAD_DECLARADA>=25 & EDAD_DECLARADA <=29 ~ "25-29",
                                                    EDAD_DECLARADA>=30 & EDAD_DECLARADA <=34 ~ "30-34",
                                                    EDAD_DECLARADA>=35 & EDAD_DECLARADA <=39 ~ "35-39",
                                                    EDAD_DECLARADA>=40 & EDAD_DECLARADA <=44 ~ "40-44",
                                                    EDAD_DECLARADA>=45 & EDAD_DECLARADA <=49 ~ "45-49",
                                                    EDAD_DECLARADA>=50 & EDAD_DECLARADA <=54 ~ "50-54",
                                                    EDAD_DECLARADA>=55 & EDAD_DECLARADA <=59 ~ "55-59",
                                                    EDAD_DECLARADA>=60 & EDAD_DECLARADA <=64 ~ "60-64",
                                                    EDAD_DECLARADA>=65 & EDAD_DECLARADA <=69 ~ "65-69",
                                                    EDAD_DECLARADA>=70 & EDAD_DECLARADA <=74 ~ "70-74",
                                                    EDAD_DECLARADA>=75 & EDAD_DECLARADA <=79 ~ "75-79",
                                                    EDAD_DECLARADA>=80 & EDAD_DECLARADA <=84 ~ "80-84",
                                                    EDAD_DECLARADA>=85 & EDAD_DECLARADA <=89 ~ "85-89",
                                                    EDAD_DECLARADA>=90 & EDAD_DECLARADA <=110 ~ "90-99",
                                                    TRUE ~ "S.I."), 
                                 fecha=as.Date(paste(substring(FECHA_FALLECIMIENTO,1,4),
                                                     substring(FECHA_FALLECIMIENTO,5,6),
                                                     substring(FECHA_FALLECIMIENTO,7,8), 
                                                     sep = '-'))) %>%
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~grupedad, sum) %>%
      dplyr::select(-`S.I.`) %>%
      dplyr::filter(is.na(fecha)==F)
    
    def <- data.frame(fecha=seq(min(casos$fecha),
                                max(casos$fecha),
                                by=1)) %>% left_join(def)
    
    def[is.na(def)] <- 0
    
    tf <- tempfile()
    td <- tempdir()
    file.path <- 'https://cloud.minsa.gob.pe/s/To2QtqoNjKqobfw/download'
    download.file( file.path , tf , mode = "wb" )
    file <- archive(tf)
    dataVacunas <- read.csv(archive_read(file, "vacunas_covid.csv"), encoding="UTF-8")

    Vacunas = dataVacunas
    
    Vacunas <- Vacunas %>% dplyr::filter(DOSIS==1) %>%
                           dplyr::mutate(grupedad=case_when(EDAD>=1 & EDAD <=4 ~ "00-04",
                                                            EDAD>=5 & EDAD <=9 ~ "05-09",
                                                            EDAD>=10 & EDAD <=14 ~ "10-14",
                                                            EDAD>=15 & EDAD <=17 ~ "15-17",
                                                            EDAD>=18 & EDAD <=24 ~ "18-24",
                                                            EDAD>=25 & EDAD <=29 ~ "25-29",
                                                            EDAD>=30 & EDAD <=34 ~ "30-34",
                                                            EDAD>=35 & EDAD <=39 ~ "35-39",
                                                            EDAD>=40 & EDAD <=44 ~ "40-44",
                                                            EDAD>=45 & EDAD <=49 ~ "45-49",
                                                            EDAD>=50 & EDAD <=54 ~ "50-54",
                                                            EDAD>=55 & EDAD <=59 ~ "55-59",
                                                            EDAD>=60 & EDAD <=64 ~ "60-64",
                                                            EDAD>=65 & EDAD <=69 ~ "65-69",
                                                            EDAD>=70 & EDAD <=74 ~ "70-74",
                                                            EDAD>=75 & EDAD <=79 ~ "75-79",
                                                            EDAD>=80 & EDAD <=84 ~ "80-84",
                                                            EDAD>=85 & EDAD <=89 ~ "85-89",
                                                            EDAD>=90 & EDAD <=110 ~ "90-99",
                                                            TRUE ~ "S.I."), 
                                 fecha=as.Date(paste(substring(FECHA_VACUNACION,1,4),
                                                     substring(FECHA_VACUNACION,5,6),
                                                     substring(FECHA_VACUNACION,7,8), 
                                                     sep = '-'))) %>%
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~grupedad, sum) %>%
      dplyr::select(-`S.I.`) %>%
      dplyr::filter(is.na(fecha)==F)
    
    Vacunas <- data.frame(fecha=seq(min(Vacunas$fecha),
                                    max(Vacunas$fecha),
                                    by=1)) %>% left_join(Vacunas)
    
    ageG <- c("00-04",
              "05-09",
              "10-14",
              "15-17",
              "18-24",
              "25-29",
              "30-34",
              "35-39",
              "40-44",
              "45-49",
              "50-54",
              "55-59",
              "60-64",
              "65-69",
              "70-74",
              "75-79",
              "80-84",
              "85-89",
              "90-99")
              
    Vacunas_df <- data.frame(fecha='',t(matrix(rep(NA,length(ageG)), byrow = F)))
    colnames(Vacunas_df) <- c("fecha",ageG)
    Vacunas_df[,Vacunas_df$fecha!='']
    Vacunas <- plyr::rbind.fill(Vacunas_df,Vacunas)
    Vacunas[is.na(Vacunas)] <- 0
    Vacunas <- Vacunas[Vacunas$fecha!="",]
    
    Vacunas$fecha <- as.Date(Vacunas$fecha)
    
    Vacunas2 <- dataVacunas
    Vacunas2 <- Vacunas2 %>% dplyr::filter(DOSIS==1) %>%
      dplyr::mutate(grupedad=case_when(EDAD>=1 & EDAD <=4 ~ "00-04",
                                       EDAD>=5 & EDAD <=9 ~ "05-09",
                                       EDAD>=10 & EDAD <=14 ~ "10-14",
                                       EDAD>=15 & EDAD <=17 ~ "15-17",
                                       EDAD>=18 & EDAD <=24 ~ "18-24",
                                       EDAD>=25 & EDAD <=29 ~ "25-29",
                                       EDAD>=30 & EDAD <=34 ~ "30-34",
                                       EDAD>=35 & EDAD <=39 ~ "35-39",
                                       EDAD>=40 & EDAD <=44 ~ "40-44",
                                       EDAD>=45 & EDAD <=49 ~ "45-49",
                                       EDAD>=50 & EDAD <=54 ~ "50-54",
                                       EDAD>=55 & EDAD <=59 ~ "55-59",
                                       EDAD>=60 & EDAD <=64 ~ "60-64",
                                       EDAD>=65 & EDAD <=69 ~ "65-69",
                                       EDAD>=70 & EDAD <=74 ~ "70-74",
                                       EDAD>=75 & EDAD <=79 ~ "75-79",
                                       EDAD>=80 & EDAD <=84 ~ "80-84",
                                       EDAD>=85 & EDAD <=89 ~ "85-89",
                                       EDAD>=90 & EDAD <=110 ~ "90-99",
                                       TRUE ~ "S.I."), 
                    fecha=as.Date(paste(substring(FECHA_VACUNACION,1,4),
                                        substring(FECHA_VACUNACION,5,6),
                                        substring(FECHA_VACUNACION,7,8), 
                                        sep = '-'))) %>%
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~grupedad, sum) %>%
      dplyr::select(-`S.I.`) %>%
      dplyr::filter(is.na(fecha)==F)
    
    Vacunas2 <- data.frame(fecha=seq(min(Vacunas2$fecha),
                                     max(Vacunas2$fecha),
                                     by=1)) %>% left_join(Vacunas2)
    
    Vacunas2_df <- data.frame(fecha='',t(matrix(rep(NA,length(ageG)), byrow = F)))
    colnames(Vacunas2_df) <- c("fecha",ageG)
    Vacunas2_df[,Vacunas2_df$fecha!='']
    Vacunas2 <- plyr::rbind.fill(Vacunas2_df,Vacunas2)
    Vacunas2[is.na(Vacunas2)] <- 0
    Vacunas2 <- Vacunas2[Vacunas2$fecha!="",]
    
    Vacunas2$fecha <- as.Date(Vacunas2$fecha)
    
    
    
    }
  
  ##### COLOMBIA (FALTA VACUNAS) #####
    if (pais=="COL") {
    # download data
    data <-
      read.socrata(
        "https://www.datos.gov.co/resource/gt2j-8ykr.csv?$select=edad,unidad_medida,recuperado,fecha_inicio_sintomas,fecha_diagnostico,fecha_muerte",
        app_token = "aipnpw1291SWIUyehIACm2hUz",
        email     = "adrian.santoro@gmail.com",
        password  = "Es.LoQueHay2021"
      )
    
    # format age column
    data$edad[data$unidad_medida %in% c(2,3)] <- 0
    data$unidad_medida <- NULL
    
    # format date columns
    data$fecha_inicio_sintomas <- as.Date(data$fecha_inicio_sintomas, format='%d/%m/%Y')
    data$fecha_diagnostico <- as.Date(data$fecha_diagnostico, format='%d/%m/%Y')
    data$fecha_muerte <- as.Date(data$fecha_muerte, format='%d/%m/%Y')
    
    # cases
    casos <- data %>% dplyr::mutate(fecha=dplyr::coalesce(fecha_inicio_sintomas,
                                                          fecha_diagnostico,
                                                          fecha_muerte)) %>%
      dplyr::mutate(gredad=case_when(edad>=0 & edad <=4 ~ "00-04",
                                     edad>=5 & edad <=9 ~ "05-09",
                                     edad>=10 & edad <=14 ~ "10-14",
                                     edad>=15 & edad <=17 ~ "15-17",
                                     edad>=18 & edad <=24 ~ "18-24",
                                     edad>=25 & edad <=29 ~ "25-29",
                                     edad>=30 & edad <=34 ~ "30-34",
                                     edad>=35 & edad <=39 ~ "35-39",
                                     edad>=40 & edad <=44 ~ "40-44",
                                     edad>=45 & edad <=49 ~ "45-49",
                                     edad>=50 & edad <=54 ~ "50-54",
                                     edad>=55 & edad <=59 ~ "55-59",
                                     edad>=60 & edad <=64 ~ "60-64",
                                     edad>=65 & edad <=69 ~ "65-69",
                                     edad>=70 & edad <=74 ~ "70-74",
                                     edad>=75 & edad <=79 ~ "75-79",
                                     edad>=80 & edad <=84 ~ "80-84",
                                     edad>=85 & edad <=89 ~ "85-89",
                                     edad>=90 & edad <=110 ~ "90-99",
                                     TRUE ~ "S.I.")) %>%
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~gredad, sum) %>%
      dplyr::select(-`S.I.`)
    
    casos <- data.frame(fecha=seq(min(casos$fecha[is.na(casos$fecha)==F]),
                                  max(casos$fecha[is.na(casos$fecha)==F]),
                                  by=1)) %>% left_join(casos) 
    
    casos[is.na(casos)] <- 0
    
    
    # deaths
    def <- data %>% dplyr::filter(recuperado %in% c("Fallecido","fallecido")) %>%
      dplyr::mutate(fecha=dplyr::coalesce(fecha_muerte,
                                          fecha_inicio_sintomas,
                                          fecha_diagnostico)) %>%
      dplyr::mutate(gredad=case_when(edad>=0 & edad <=4 ~ "00-04",
                                     edad>=5 & edad <=9 ~ "05-09",
                                     edad>=10 & edad <=14 ~ "10-14",
                                     edad>=15 & edad <=17 ~ "15-17",
                                     edad>=18 & edad <=24 ~ "18-24",
                                     edad>=25 & edad <=29 ~ "25-29",
                                     edad>=30 & edad <=34 ~ "30-34",
                                     edad>=35 & edad <=39 ~ "35-39",
                                     edad>=40 & edad <=44 ~ "40-44",
                                     edad>=45 & edad <=49 ~ "45-49",
                                     edad>=50 & edad <=54 ~ "50-54",
                                     edad>=55 & edad <=59 ~ "55-59",
                                     edad>=60 & edad <=64 ~ "60-64",
                                     edad>=65 & edad <=69 ~ "65-69",
                                     edad>=70 & edad <=74 ~ "70-74",
                                     edad>=75 & edad <=79 ~ "75-79",
                                     edad>=80 & edad <=84 ~ "80-84",
                                     edad>=85 & edad <=89 ~ "85-89",
                                     edad>=90 & edad <=110 ~ "90-99",
                                     TRUE ~ "S.I.")) %>%
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~gredad, sum)
    
    def <- data.frame(fecha=seq(min(casos$fecha[is.na(casos$fecha)==F]),
                                max(casos$fecha[is.na(casos$fecha)==F]),
                                by=1)) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    
    
    
    }
  
  ##### MEXICO #####
  
  if (pais == "MEX") {
    
    url <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
    download.file(url, "Covid19Mex.zip")
    unzip("Covid19Mex.zip")
    fileName <- zip_list("Covid19Mex.zip")[1,1]
    file.remove('Covid19Mex.zip')
    data <- read.csv(fileName, 
                     fileEncoding = "UTF-8")
    
    #casos
    data[data$CLASIFICACION_FINAL %in% c(1,2,3),]
    
    #muertes
    data[data$FECHA_DEF != "9999-99-99" & data$CLASIFICACION_FINAL %in% c(1,2,3),]
    
    
    casos <- data %>% dplyr::filter(CLASIFICACION_FINAL %in% c(1,2,3)) %>%
      dplyr::select(EDAD,
                    FECHA_SINTOMAS,
                    FECHA_INGRESO) %>%
      dplyr::mutate(fecha=dplyr::coalesce(FECHA_SINTOMAS,
                                          FECHA_INGRESO)) %>%
      dplyr::mutate(gredad=case_when(EDAD>=0 & EDAD <=4 ~ "00-04",
                                     EDAD>=5 & EDAD <=9 ~ "05-09",
                                     EDAD>=10 & EDAD <=14 ~ "10-14",
                                     EDAD>=15 & EDAD <=17 ~ "15-17",
                                     EDAD>=18 & EDAD <=24 ~ "18-24",
                                     EDAD>=25 & EDAD <=29 ~ "25-29",
                                     EDAD>=30 & EDAD <=34 ~ "30-34",
                                     EDAD>=35 & EDAD <=39 ~ "35-39",
                                     EDAD>=40 & EDAD <=44 ~ "40-44",
                                     EDAD>=45 & EDAD <=49 ~ "45-49",
                                     EDAD>=50 & EDAD <=54 ~ "50-54",
                                     EDAD>=55 & EDAD <=59 ~ "55-59",
                                     EDAD>=60 & EDAD <=64 ~ "60-64",
                                     EDAD>=65 & EDAD <=69 ~ "65-69",
                                     EDAD>=70 & EDAD <=74 ~ "70-74",
                                     EDAD>=75 & EDAD <=79 ~ "75-79",
                                     EDAD>=80 & EDAD <=84 ~ "80-84",
                                     EDAD>=85 & EDAD <=89 ~ "85-89",
                                     EDAD>=90 & EDAD <=110 ~ "90-99",
                                     TRUE ~ "S.I.")) %>%
      dplyr::mutate(cuenta=1) %>%
      reshape::cast(fecha~gredad, sum) %>%
      dplyr::select(-`S.I.`)

    casos <- data.frame(fecha=as.character(seq(as.Date(min(casos$fecha)),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos)

    casos[is.na(casos)] <- 0

    
    def <- data %>% dplyr::filter(FECHA_DEF != "9999-99-99" & CLASIFICACION_FINAL %in% c(1,2,3)) %>%
                    dplyr::select(EDAD,
                                  FECHA_DEF,
                                  FECHA_SINTOMAS,
                                  FECHA_INGRESO) %>%
                    dplyr::mutate(fecha=dplyr::coalesce(FECHA_DEF,
                                                        FECHA_SINTOMAS,
                                                        FECHA_INGRESO)) %>%
                    dplyr::mutate(gredad=case_when(EDAD>=0 & EDAD <=4 ~ "00-04",
                                                   EDAD>=5 & EDAD <=9 ~ "05-09",
                                                   EDAD>=10 & EDAD <=14 ~ "10-14",
                                                   EDAD>=15 & EDAD <=17 ~ "15-17",
                                                   EDAD>=18 & EDAD <=24 ~ "18-24",
                                                   EDAD>=25 & EDAD <=29 ~ "25-29",
                                                   EDAD>=30 & EDAD <=34 ~ "30-34",
                                                   EDAD>=35 & EDAD <=39 ~ "35-39",
                                                   EDAD>=40 & EDAD <=44 ~ "40-44",
                                                   EDAD>=45 & EDAD <=49 ~ "45-49",
                                                   EDAD>=50 & EDAD <=54 ~ "50-54",
                                                   EDAD>=55 & EDAD <=59 ~ "55-59",
                                                   EDAD>=60 & EDAD <=64 ~ "60-64",
                                                   EDAD>=65 & EDAD <=69 ~ "65-69",
                                                   EDAD>=70 & EDAD <=74 ~ "70-74",
                                                   EDAD>=75 & EDAD <=79 ~ "75-79",
                                                   EDAD>=80 & EDAD <=84 ~ "80-84",
                                                   EDAD>=85 & EDAD <=89 ~ "85-89",
                                                   EDAD>=90 & EDAD <=110 ~ "90-99",
                                                   TRUE ~ "S.I.")) %>%
                    dplyr::mutate(cuenta=1) %>%
                    reshape::cast(fecha~gredad, sum) %>%
                    dplyr::select(-`S.I.`)
                  
    def <- data.frame(fecha=as.character(seq(as.Date(min(casos$fecha)),
                                             as.Date(max(casos$fecha)),
                                             by=1))) %>% left_join(def)
    
    def[is.na(def)] <- 0
    def$fecha <- as.Date(def$fecha)
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    
    
  }
  
  ##### CHILE #####
  
  if (pais == "CHL") {
  # browser()
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
    fix <- round(casos$`15-19`*.4,digits=0)
    casos$`15-19` <- casos$`15-19`-fix 
    casos$`20-24` <- casos$`20-24`+fix 
    
    colnames(casos)[5] <- "15-17"
    colnames(casos)[6] <- "18-24"
    
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
    casos$`85-89` <- casos$`85-99`
    casos$`85-99` <- NULL
    casos$`90-99` <- 0
    
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
    data$`15-17` <- dist[4]*data$para_dist
    data$`18-24` <- dist[5]*data$para_dist
    data$`25-29` <- dist[6]*data$para_dist
    data$`30-34` <- dist[7]*data$para_dist
    data$`35-39` <- dist[8]*data$para_dist
    countryData$ARG <- NULL
    
    data <- data %>% select(fecha=Grupo.de.edad,`00-04`,`05-09`,`10-14`,`15-17`,`18-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-99`)
    data$fecha <- as.Date(data$fecha)
    
    
    def <- left_join(data.frame(fecha=seq(min(casos$fecha),
                                          max(casos$fecha),
                                          by=1)), data)
    
    def[is.na(def)] <- 0
    
    rm(data)
    
    # vacunas
    
    # save project unit and directory
    projDir <- getwd()
    unit <- substring(projDir,1,2)
    
    # clone repository
    shell('cd/ & git clone https://github.com/MinCiencia/Datos-COVID19', intern = F, wait = T)
    
    # source update code
    setwd(paste0(unit,"/Datos-COVID19"))
    source(paste0(projDir,"/functions/updateChile.R"))
    
    # delete temp repository
    shell("echo S|cacls datos-COVID19 /P everyone:f & cd/ & rmdir /s /q Datos-COVID19")
    
    # set project directory as wd
    setwd(projDir)
  }


  ##### URUGUAY #####
  
  if (pais=="URY") {
    
    def = read.csv2("https://github.com/GUIAD-COVID/datos-y-visualizaciones-GUIAD/raw/master/datos/estadisticasUY_fallecimientos.csv", sep=",")
    def$fecha <- as.Date(def$fecha, "%d/%m/%Y")
    str_def <- data.frame(fecha=as.Date(NA),
                          `00-04`=NA,
                          `05-09`=NA,
                          `10-14`=NA,
                          `15-17`=NA,
                          `18-24`=NA,
                          `25-29`=NA,
                          `30-34`=NA,
                          `35-39`=NA,
                          `40-44`=NA,
                          `45-49`=NA,
                          `50-54`=NA,
                          `55-59`=NA,
                          `60-64`=NA,
                          `65-69`=NA,
                          `70-74`=NA,
                          `75-79`=NA,
                          `80-84`=NA,
                          `85-89`=NA,
                          `90-99`=NA
                          )
    colnames(str_def) <- c("fecha", "00-04", "05-09", "10-14", "15-17", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                           "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-99")
                          
    
    
    def <- def %>% dplyr::mutate(gredad=case_when(edad >= 0 & edad<= 4  ~ "00-04",
                                                  edad>=5 & edad <=9 ~ "05-09",
                                                  edad>=10 & edad <=14 ~ "10-14",
                                                  edad>=15 & edad <=17 ~ "15-17",
                                                  edad>=18 & edad <=24 ~ "18-24",
                                                  edad>=25 & edad <=29 ~ "25-29",
                                                  edad>=30 & edad <=34 ~ "30-34",
                                                  edad>=35 & edad <=39 ~ "35-39",
                                                  edad>=40 & edad <=44 ~ "40-44",
                                                  edad>=45 & edad <=49 ~ "45-49",
                                                  edad>=50 & edad <=54 ~ "50-54",
                                                  edad>=55 & edad <=59 ~ "55-59",
                                                  edad>=60 & edad <=64 ~ "60-64",
                                                  edad>=65 & edad <=69 ~ "65-69",
                                                  edad>=70 & edad <=74 ~ "70-74",
                                                  edad>=75 & edad <=79 ~ "75-79",
                                                  edad>=80 & edad <=84 ~ "80-84",
                                                  edad>=85 & edad <=89 ~ "85-89",
                                                  edad>=90 & edad <=110 ~ "90-99",
                                                  TRUE ~ "S.I.")) %>%
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~gredad, sum) %>%
      dplyr::select(-`S.I.`)
      
    
    def <- data.frame(fecha=seq(as.Date('2020-03-01'),
                                as.Date(max(def$fecha)),
                                by=1)) %>% left_join(def) 
    
    def <- rbind.fill(str_def,def)[-1,]
    def[is.na(def)] <- 0
    
    casos <- def
    casos[,-1] <- 0
    
    Vacunas <- data.frame(fecha_aplicacion=casos[,1])
    Vacunas$`00-17` <- 0
    Vacunas$`18-29` <- 0
    Vacunas$`30-39` <- 0
    Vacunas$`40-49` <- 0
    Vacunas$`50-59` <- 0
    Vacunas$`60-69` <- 0
    Vacunas$`70-79` <- 0
    Vacunas$`80-89` <- 0
    Vacunas$`90-99` <- 0
    Vacunas <- Vacunas[Vacunas$fecha_aplicacion>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas
    
  }


  ##### BRASIL #####

  # if (pais=="BRA") {
  #   url <- "https://opendatasus.saude.gov.br/dataset/casos-nacionais"
  #   html <- paste(readLines(url), collapse="\n")
  #   links <- str_match_all(html, "<a href=\"(.*?)\"")
  #   links <- str_subset(links[[1]],c("csv"))
  #   links <- links[substring(links,1,1)!="<"] 
  #   casesBrz <- data.frame()
  #   defBrz <- data.frame()
  #   
  #   for (l in links) {
  #     
  #     file=str_replace_all(l,"https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/","")
  #     
  #     print(paste("Leyendo:",l))
  #     df <- read.csv2(l) 
  #     df <- df %>% dplyr::filter(substring(classificacaoFinal,1,6)=="Confir") %>%
  #       dplyr::mutate(fechaRep=substring(dataNotificacao,1,10),
  #                     fechaSintomas=substring(dataInicioSintomas,1,10),
  #                     edad=as.numeric(idade)) %>%
  #       dplyr::select(fechaRep,fechaSintomas,edad,evolucaoCaso) %>%
  #       dplyr::mutate(gredad=case_when(edad>=1 & edad <=4 ~ "00-04",
  #                                      edad>=5 & edad <=9 ~ "05-09",
  #                                      edad>=10 & edad <=14 ~ "10-14",
  #                                      edad>=15 & edad <=17 ~ "15-17",
  #                                      edad>=18 & edad <=24 ~ "18-24",
  #                                      edad>=25 & edad <=29 ~ "25-29",
  #                                      edad>=30 & edad <=34 ~ "30-34",
  #                                      edad>=35 & edad <=39 ~ "35-39",
  #                                      edad>=40 & edad <=44 ~ "40-44",
  #                                      edad>=45 & edad <=49 ~ "45-49",
  #                                      edad>=50 & edad <=54 ~ "50-54",
  #                                      edad>=55 & edad <=59 ~ "55-59",
  #                                      edad>=60 & edad <=64 ~ "60-64",
  #                                      edad>=65 & edad <=69 ~ "65-69",
  #                                      edad>=70 & edad <=74 ~ "70-74",
  #                                      edad>=75 & edad <=79 ~ "75-79",
  #                                      edad>=80 & edad <=84 ~ "80-84",
  #                                      edad>=85 & edad <=89 ~ "85-89",
  #                                      edad>=90 & edad <=110 ~ "90-99",
  #                                      TRUE ~ "S.I."))
  #     df$fechaRep[df$fechaRep=="null"] <- NA
  #     df$fechaRep[df$fechaRep=="undefined"] <- NA
  #     df$fechaSintomas[df$fechaSintomas=="null"] <- NA
  #     df$fechaSintomas[df$fechaSintomas=="undefined"] <- NA
  #     df$fecha=coalesce(df$fechaSintomas,df$fechaRep)
  #     df$fecha=as.Date(df$fecha)
  #     df <- df %>% dplyr::select(fecha,gredad,evolucaoCaso)
  #     casos <- df %>% dplyr::mutate(cuenta=1) %>%
  #       reshape::cast(fecha~gredad, sum)
  #     casos <- data.frame(fecha=as.Date(as.character(seq(as.Date("2020-01-01"),
  #                                                        as.Date(Sys.Date()),
  #                                                        by=1)))) %>% left_join(casos) 
  #     casos[is.na(casos)] <- 0
  #     casos$type <- "cases"
  #     casos$file <- file
  #     def <- df %>% dplyr::filter(evolucaoCaso=="Óbito") %>%
  #       dplyr::mutate(cuenta=1) %>%
  #       reshape::cast(fecha~gredad, sum) 
  #     def <- data.frame(fecha=as.Date(as.character(seq(as.Date("2020-01-01"),
  #                                                      as.Date(Sys.Date()),
  #                                                      by=1)))) %>% left_join(def) 
  #     def[is.na(def)] <- 0
  #     def$type <- "def"
  #     def$file <- file
  #     casesBrz <- rbind.fill(casesBrz,casos)
  #     defBrz <- rbind.fill(defBrz,def)
  #     print(Sys.time())
  #     print(paste("Agregado:",file))
  #     print(paste("Archivos agregados:",grep(l,links)))
  #     
  #     casos <- casesBrz %>% dplyr::group_by(fecha) %>%
  #       dplyr::summarise_at(colnames(casesBrz)[substring(colnames(casesBrz),1,1)>="0" &
  #                                                substring(colnames(casesBrz),1,1)<="9"], sum)
  #     casos <- as.data.frame(casos)
  #     defBrz[is.na(defBrz)] <- 0
  #     def <- defBrz %>% dplyr::group_by(fecha) %>%
  #       dplyr::summarise_at(colnames(defBrz)[substring(colnames(defBrz),1,1)>="0" &
  #                                              substring(colnames(defBrz),1,1)<="9"], sum)
  #     
  #     def <- as.data.frame(def)
  #     
  #     
  # ##### Vacunas Brazil #####    
  #     
  #     #defino lista vacia
  #     datos_vacunas_brasil <- c() 
  #     
  #     # lista de estados de Brasil
  #     estados = c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ','RN','RO','RR','RS','SC','SE','SP','TO')
  #     #estados = c('AC', 'TO')
  #     
  #     for (i in estados) {
  #       
  #       # levanto info de la url
  #       print(paste0('Descargo el listado de vacunas del estado ', i, ' de Brasil'))
  #       
  #       url= paste0('https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/PNI/vacina/uf/2021-09-23/uf%3D', i, '/part-00000-a8e22644-5d59-4166-9357-5246241f143a.c000.csv')
  #       print(url)
  #       download.file(url, "vacunas_brasil_estado.csv") 
  #       
  #       # levanto datos en R
  #       print(paste0('Guardo datos de vacunas en R del estado ', i, ' de Brasil'))
  #       
  #       library('data.table')
  #       datos_vacunas = fread("vacunas_brasil_estado.csv",  sep=';')
  #       
  #       #Remuevo el csv
  #       file.remove('vacunas_brasil_estado.csv')
  #       
  #       
  #       print('Seleccion y Manipulacion de datos')
  #       
  #       # Selecciono datos que necesito
  #       datos_vacunas = datos_vacunas[, c("paciente_idade", "paciente_enumsexobiologico","vacina_dataaplicacao","vacina_descricao_dose")]
  #       
  #       # Cambio los Nombres 
  #       setnames(datos_vacunas, c("paciente_idade", "paciente_enumsexobiologico","vacina_dataaplicacao","vacina_descricao_dose"),
  #                c("edad", "genero","fecha_aplicacion","tipo_dosis"))
  #       
  #       # reemplazar caracteres raros
  #       datos_vacunas$tipo_dosis = gsub("ÂªÂ."," ",datos_vacunas$tipo_dosis)
  #       datos_vacunas$tipo_dosis = gsub("Â ","",datos_vacunas$tipo_dosis)
  #       
  #       # Cambio el texto de tipo de dosis
  #       datos_vacunas[tipo_dosis == 'Dose', num_dosis:= 0] #monodosis
  #       datos_vacunas[tipo_dosis == "1 Dose", num_dosis:= 1] #1° dosis
  #       datos_vacunas[tipo_dosis == '2 Dose', num_dosis:= 2] #2° dosis
  #       
  #       
  #       #uno las tablas que voy descargando
  #       print('Append de las vacunas de los distintos estados de Brasil')
  #       
  #       datos_vacunas_brasil <- rbind(datos_vacunas_brasil, datos_vacunas)
  #       remove(datos_vacunas)
  #     }
  #     
  #     rm(i)
  #     gc()
  #     
  #     
  #     
  #     ## Armo la estructura de vacunacion por grupo etario
  #     
  #     library(dplyr)
  #     library(reshape2)
  #     
  #     Vacunas = setDF(datos_vacunas_brasil)
  #     
  #     
  #     ########## 1° Dosis ###########
  #     
  #     Vacunas1 = Vacunas %>% dplyr::filter(num_dosis==1 ) %>%  
  #       
  #       # agrego variables agrupadas por edad  
  #       dplyr::mutate(gr_edad=case_when(edad >=1 & edad <=4 ~ "00-04",
  #                                       edad >=5 & edad <=9 ~ "05-09",
  #                                       edad >=10 & edad <=14 ~ "10-14",
  #                                       edad >=15 & edad <=17 ~ "15-17",
  #                                       edad >=18 & edad <=24 ~ "18-24",
  #                                       edad >=25 & edad <=29 ~ "25-29",
  #                                       edad >=30 & edad <=34 ~ "30-34",
  #                                       edad >=35 & edad <=39 ~ "35-39",
  #                                       edad >=40 & edad <=44 ~ "40-44",
  #                                       edad >=45 & edad <=49 ~ "45-49",
  #                                       edad >=50 & edad <=54 ~ "50-54",
  #                                       edad >=55 & edad <=59 ~ "55-59",
  #                                       edad >=60 & edad <=64 ~ "60-64",
  #                                       edad >=65 & edad <=69 ~ "65-69",
  #                                       edad >=70 & edad <=74 ~ "70-74",
  #                                       edad >=75 & edad <=79 ~ "75-79",
  #                                       edad >=80 & edad <=84 ~ "80-84",
  #                                       edad >=85 & edad <=89 ~ "85-89",
  #                                       edad >=90 ~ "90-99")) %>%
  #       
  #       # agrupo por fecha y edad
  #       dplyr::group_by(fecha_aplicacion, gr_edad) %>%
  #       
  #       # sumarizo 
  #       dplyr::summarise(n=n()) %>%
  #       
  #       reshape::cast(fecha_aplicacion~gr_edad, mean) %>%
  #       
  #       dplyr::select(fecha_aplicacion,"00-04","05-09","10-14","15-17","18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-99") %>%
  #       dplyr::mutate(fecha_aplicacion=as.Date(fecha_aplicacion))
  #     
  #     
  #     Vacunas1 <- data.frame(fecha_aplicacion=seq(as.Date(min(Vacunas1$fecha_aplicacion)),
  #                                                 as.Date(max(Vacunas1$fecha_aplicacion)),
  #                                                 by=1)) %>% left_join(Vacunas1) 
  #     
  #     #vacunas=setDT(Vacunas)
  #     #Vacunas[,.N, by=gr_edad]
  #     
  #     # reemplazo nulos por cero
  #     Vacunas[is.na(Vacunas)] <- 0
  #     
  #     
  #     ########## 2° Dosis ###########
  #     
  #     #Vacunas2 = setDF(dataVacunas)
  #     Vacunas2 = Vacunas %>% dplyr::filter(num_dosis==2 ) %>%   
  #       
  #       # agrego variables agrupadas por edad  
  #       dplyr::mutate(gr_edad=case_when(edad >=1 & edad <=4 ~ "00-04",
  #                                       edad >=5 & edad <=9 ~ "05-09",
  #                                       edad >=10 & edad <=14 ~ "10-14",
  #                                       edad >=15 & edad <=17 ~ "15-17",
  #                                       edad >=18 & edad <=24 ~ "18-24",
  #                                       edad >=25 & edad <=29 ~ "25-29",
  #                                       edad >=30 & edad <=34 ~ "30-34",
  #                                       edad >=35 & edad <=39 ~ "35-39",
  #                                       edad >=40 & edad <=44 ~ "40-44",
  #                                       edad >=45 & edad <=49 ~ "45-49",
  #                                       edad >=50 & edad <=54 ~ "50-54",
  #                                       edad >=55 & edad <=59 ~ "55-59",
  #                                       edad >=60 & edad <=64 ~ "60-64",
  #                                       edad >=65 & edad <=69 ~ "65-69",
  #                                       edad >=70 & edad <=74 ~ "70-74",
  #                                       edad >=75 & edad <=79 ~ "75-79",
  #                                       edad >=80 & edad <=84 ~ "80-84",
  #                                       edad >=85 & edad <=89 ~ "85-89",
  #                                       edad >=90 ~ "90-99")) %>%
  #       
  #       # agrupo por fecha y edad
  #       dplyr::group_by(fecha_aplicacion, gr_edad) %>%
  #       
  #       # sumarizo 
  #       dplyr::summarise(n=n()) %>%
  #       
  #       reshape::cast(fecha_aplicacion~gr_edad, mean) %>%
  #       
  #       dplyr::select(fecha_aplicacion,"00-04","05-09","10-14","15-17","18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-99") %>%
  #       dplyr::mutate(fecha_aplicacion=as.Date(fecha_aplicacion))
  #     
  #     
  #     Vacunas2 <- data.frame(fecha_aplicacion=seq(as.Date(min(Vacunas2$fecha_aplicacion)),
  #                                                 as.Date(max(Vacunas2$fecha_aplicacion)),
  #                                                 by=1)) %>% left_join(Vacunas2) 
  #     
  #     
  #     # reemplazo nulos por cero
  #     Vacunas2[is.na(Vacunas2)] <- 0
  #     
  #     
  #     
  #     ########## Monodosis ###########
  #     
  #     #Vacunas0 = setDF(dataVacunas)
  #     Vacunas0 = Vacunas %>% dplyr::filter(num_dosis==0 ) %>%   
  #       
  #       # agrego variables agrupadas por edad  
  #       dplyr::mutate(gr_edad=case_when(edad >=1 & edad <=4 ~ "00-04",
  #                                       edad >=5 & edad <=9 ~ "05-09",
  #                                       edad >=10 & edad <=14 ~ "10-14",
  #                                       edad >=15 & edad <=17 ~ "15-17",
  #                                       edad >=18 & edad <=24 ~ "18-24",
  #                                       edad >=25 & edad <=29 ~ "25-29",
  #                                       edad >=30 & edad <=34 ~ "30-34",
  #                                       edad >=35 & edad <=39 ~ "35-39",
  #                                       edad >=40 & edad <=44 ~ "40-44",
  #                                       edad >=45 & edad <=49 ~ "45-49",
  #                                       edad >=50 & edad <=54 ~ "50-54",
  #                                       edad >=55 & edad <=59 ~ "55-59",
  #                                       edad >=60 & edad <=64 ~ "60-64",
  #                                       edad >=65 & edad <=69 ~ "65-69",
  #                                       edad >=70 & edad <=74 ~ "70-74",
  #                                       edad >=75 & edad <=79 ~ "75-79",
  #                                       edad >=80 & edad <=84 ~ "80-84",
  #                                       edad >=85 & edad <=89 ~ "85-89",
  #                                       edad >=90 ~ "90-99")) %>%
  #       
  #       # agrupo por fecha y edad
  #       dplyr::group_by(fecha_aplicacion, gr_edad) %>%
  #       
  #       # sumarizo 
  #       dplyr::summarise(n=n()) %>%
  #       
  #       reshape::cast(fecha_aplicacion~gr_edad, mean) %>%
  #       
  #       dplyr::select(fecha_aplicacion,"00-04","05-09","10-14","15-17","18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-99") %>%
  #       dplyr::mutate(fecha_aplicacion=as.Date(fecha_aplicacion))
  #     
  #     
  #     Vacunas0 <- data.frame(fecha_aplicacion=seq(as.Date(min(Vacunas0$fecha_aplicacion)),
  #                                                 as.Date(max(Vacunas0$fecha_aplicacion)),
  #                                                 by=1)) %>% left_join(Vacunas0) 
  #     
  #     # reemplazo nulos por cero
  #     Vacunas0[is.na(Vacunas0)] <- 0
  #     
  #     
  #   } # cierra if de Brazil
  #   

        
  ######## COSTA RICA ##########
    
    if (pais=="CRI") {
      
      # Levanto los datos  
      
      library(readxl)
      library('data.table')
      
      #2020
      temp0 <- tempfile()
      download.file("https://geovision.uned.ac.cr/oges/archivos_covid/baseanonimizada/BASE%20ANONIMIZADA%20CASOS%20COVID%202020.xlsx",
                    temp0, 
                    mode = "wb")
      data20 = readxl::read_xlsx(temp0, sheet = "BASE")
      data20 = setDT(data20)
      
      # Selecciono datos que necesito
      datos20 = data20[, c("FECHA PUBLICACION", "EDAD","FALLECIDO")]
      
      # Cambio los Nombres 
      setnames(datos20, c("FECHA PUBLICACION","EDAD","FALLECIDO"), c("fecha", "edad", "fallecido"))
      
      datos20$edad = as.numeric(datos20$edad)
      datos20$fecha = as.character(datos20$fecha)
      
      #2021
      temp <- tempfile()
      download.file("http://geovision.uned.ac.cr/oges/archivos_covid/baseanonimizada/BASE%20ANONIMIZADA%20CASOS%20COVID%202021%20AL%2005%20SETIEMBRE.xlsx",
                    temp, 
                    mode = "wb")
      data21 = readxl::read_xlsx(temp, sheet = "BASE")
      
      data21 = setDT(data21)
      
      # Selecciono datos que necesito
      datos21 = data21[, c("FECHA PUBLICACION", "EDAD","FALLECIDO")]
      
      # Cambio los Nombres 
      setnames(datos21, c("FECHA PUBLICACION","EDAD","FALLECIDO"), c("fecha", "edad", "fallecido"))
      
      datos21$edad = as.numeric(datos21$edad)
      datos21$fecha = as.character(datos21$fecha)
      
      
      # uno ambos datasets 2020 + 2021
      datos = rbind(datos20, datos21)
      

      
  ######## Casos Costa Rica #############  
      
      library(dplyr)
      casos <- datos %>% 
        
        # filtro los casos que vienen sin edad cargada
        dplyr::filter(!is.na(edad) & !is.na(fecha)) %>%
        
        #formato de fecha 
        dplyr::mutate(fecha=as.character(fecha)) %>%
        
        # agrego variables agrupadas por edad  
        dplyr::mutate(gredad=case_when(edad >=0 & edad <=4 ~ "00-04",
                                       edad >=5 & edad <=9 ~ "05-09",
                                       edad >=10 & edad <=14 ~ "10-14",
                                       edad >=15 & edad <=17 ~ "15-17",
                                       edad >=18 & edad <=24 ~ "18-24",
                                       edad >=25 & edad <=29 ~ "25-29",
                                       edad >=30 & edad <=34 ~ "30-34",
                                       edad >=35 & edad <=39 ~ "35-39",
                                       edad >=40 & edad <=44 ~ "40-44",
                                       edad >=45 & edad <=49 ~ "45-49",
                                       edad >=50 & edad <=54 ~ "50-54",
                                       edad >=55 & edad <=59 ~ "55-59",
                                       edad >=60 & edad <=64 ~ "60-64",
                                       edad >=65 & edad <=69 ~ "65-69",
                                       edad >=70 & edad <=74 ~ "70-74",
                                       edad >=75 & edad <=79 ~ "75-79",
                                       edad >=80 & edad <=84 ~ "80-84",
                                       edad >=85 & edad <=89 ~ "85-89",
                                       edad >=90 ~ "90-99")) %>%
        
        
        dplyr::mutate(cuenta=1) %>% 
        reshape::cast(fecha~gredad, sum)
      
      
      casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                                 as.Date(max(casos$fecha)),
                                                 by=1))) %>% left_join(casos) 
      
      casos[is.na(casos)] <- 0
      
      
  ######## Defunciones Costa Rica #############
      
      
      def <- datos %>% 
        
        # filtro los casos que vienen sin edad cargada
        dplyr::filter(!is.na(edad) & !is.na(fecha) &
                        fallecido=="SI") %>%
        
        #formato de fecha 
        dplyr::mutate(fecha=as.character(fecha)) %>%
        
        # agrego variables agrupadas por edad  
        dplyr::mutate(gredad=case_when(edad >=0 & edad <=4 ~ "00-04",
                                       edad >=5 & edad <=9 ~ "05-09",
                                       edad >=10 & edad <=14 ~ "10-14",
                                       edad >=15 & edad <=17 ~ "15-17",
                                       edad >=18 & edad <=24 ~ "18-24",
                                       edad >=25 & edad <=29 ~ "25-29",
                                       edad >=30 & edad <=34 ~ "30-34",
                                       edad >=35 & edad <=39 ~ "35-39",
                                       edad >=40 & edad <=44 ~ "40-44",
                                       edad >=45 & edad <=49 ~ "45-49",
                                       edad >=50 & edad <=54 ~ "50-54",
                                       edad >=55 & edad <=59 ~ "55-59",
                                       edad >=60 & edad <=64 ~ "60-64",
                                       edad >=65 & edad <=69 ~ "65-69",
                                       edad >=70 & edad <=74 ~ "70-74",
                                       edad >=75 & edad <=79 ~ "75-79",
                                       edad >=80 & edad <=84 ~ "80-84",
                                       edad >=85 & edad <=89 ~ "85-89",
                                       edad >=90 ~ "90-99")) %>%
        
        
        dplyr::mutate(cuenta=1) %>% 
        reshape::cast(fecha~gredad, sum)
      
      
      def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(def$fecha)),
                                               by=1))) %>% left_join(def) 
      
      def[is.na(def)] <- 0
      
      
      ########### Vacunas Costa Rica (PENDIENTE) #############
      
      
      Vacunas = casos 
      Vacunas[,2:20] <- 0
      Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
      Vacunas2 <- Vacunas
      Vacunas0 <- Vacunas
      
    } # cierra if de CR
  
  
  
  ##### PARAGUAY #####
  
  if (pais=="PRY") {
    
    
    ############ Casos Paraguay ###############
    
    # Levanto los datos  
    
    library(readxl)
    library('data.table')
    
    
    url= ('https://public.tableau.com/vizql/w/COVID19PY-Registros/v/Descargardatos/vudcsv/sessions/E2ACCEE4FBB54BB9A92ED8D2E878B709-0:0/views/7713620505763405234_2641841674343653269?underlying_table_id=Migrated%20Data&underlying_table_caption=Datos%20completos.csv')
    print(url)
    download.file(url, "Casos_Confirmados.csv") 
    
    
    casos_confirmados = fread("Casos_Confirmados.csv",  sep=';') 
    
    
    # Selecciono datos que necesito
    datos = casos_confirmados[, c("Fecha Confirmacion", "Edad")]
    
    # Cambio los Nombres 
    setnames(datos, c("Fecha Confirmacion", "Edad"), c("fecha", "edad"))
    
    datos$edad = as.numeric(datos$edad)
    datos$fecha = as.Date(datos$fecha, format="%d/%m/%Y")
    datos$fecha = as.character(datos$fecha)
    
    #names(datos)[sapply(datos, class)=='character']
    #names(datos)[sapply(datos, class)=='numeric']
    
    dim(datos)  #462283      2
    
    ###data[,.N, by='EC PARTE 1.Fecha_Aplicacion_Primer_Dosis'] # todos nulos
    
    
    library(dplyr)
    casos <- datos %>% 
      
      # filtro los casos que vienen sin edad cargada
      dplyr::filter(!is.na(edad) & !is.na(fecha)) %>%
      
      #formato de fecha 
      dplyr::mutate(fecha=as.character(fecha)) %>%
      
      # agrego variables agrupadas por edad  
      dplyr::mutate(gredad=case_when(edad >=0 & edad <=4 ~ "00-04",
                                     edad >=5 & edad <=9 ~ "05-09",
                                     edad >=10 & edad <=14 ~ "10-14",
                                     edad >=15 & edad <=17 ~ "15-17",
                                     edad >=18 & edad <=24 ~ "18-24",
                                     edad >=25 & edad <=29 ~ "25-29",
                                     edad >=30 & edad <=34 ~ "30-34",
                                     edad >=35 & edad <=39 ~ "35-39",
                                     edad >=40 & edad <=44 ~ "40-44",
                                     edad >=45 & edad <=49 ~ "45-49",
                                     edad >=50 & edad <=54 ~ "50-54",
                                     edad >=55 & edad <=59 ~ "55-59",
                                     edad >=60 & edad <=64 ~ "60-64",
                                     edad >=65 & edad <=69 ~ "65-69",
                                     edad >=70 & edad <=74 ~ "70-74",
                                     edad >=75 & edad <=79 ~ "75-79",
                                     edad >=80 & edad <=84 ~ "80-84",
                                     edad >=85 & edad <=89 ~ "85-89",
                                     edad >=90 ~ "90-99")) %>%
      
      
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~gredad, sum)
    
    
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    
    casos[is.na(casos)] <- 0
    
    
    ######## Defunciones Paraguay #############
    
    # Levanto los datos de Fallecidos
    
    url= ('https://public.tableau.com/vizql/w/COVID19PY-Registros/v/FALLECIDOS/vudcsv/sessions/E2ACCEE4FBB54BB9A92ED8D2E878B709-0:0/views/7713620505763405234_5043410824490810379?underlying_table_id=Migrated%20Data&underlying_table_caption=Datos%20completos.csv')
    print(url)
    download.file(url, "datos_fallecidos.csv") 
    
    
    datos_fallecidos = fread("datos_fallecidos.csv",  sep=';') 
    
    # Selecciono datos que necesito
    datos = datos_fallecidos[, c("Fecha Obito", "Sexo","Edad")]
    
    # Cambio los Nombres 
    setnames(datos, c("Fecha Obito", "Sexo","Edad"), c("fecha", "sexo", "edad"))
    
    datos$edad = as.numeric(datos$edad)
    datos$fecha = as.Date(datos$fecha, format="%d/%m/%Y")
    datos$fecha = as.character(datos$fecha)
    
    
    
    def <- datos %>% 
      
      
      # filtro los casos que vienen sin edad cargada
      dplyr::filter(!is.na(edad) & !is.na(fecha)) %>%
      
      #formato de fecha 
      dplyr::mutate(fecha=as.character(fecha)) %>%
      
      # agrego variables agrupadas por edad  
      dplyr::mutate(gredad=case_when(edad >=0 & edad <=4 ~ "00-04",
                                     edad >=5 & edad <=9 ~ "05-09",
                                     edad >=10 & edad <=14 ~ "10-14",
                                     edad >=15 & edad <=17 ~ "15-17",
                                     edad >=18 & edad <=24 ~ "18-24",
                                     edad >=25 & edad <=29 ~ "25-29",
                                     edad >=30 & edad <=34 ~ "30-34",
                                     edad >=35 & edad <=39 ~ "35-39",
                                     edad >=40 & edad <=44 ~ "40-44",
                                     edad >=45 & edad <=49 ~ "45-49",
                                     edad >=50 & edad <=54 ~ "50-54",
                                     edad >=55 & edad <=59 ~ "55-59",
                                     edad >=60 & edad <=64 ~ "60-64",
                                     edad >=65 & edad <=69 ~ "65-69",
                                     edad >=70 & edad <=74 ~ "70-74",
                                     edad >=75 & edad <=79 ~ "75-79",
                                     edad >=80 & edad <=84 ~ "80-84",
                                     edad >=85 & edad <=89 ~ "85-89",
                                     edad >=90 ~ "90-99")) %>%
      
      
      dplyr::mutate(cuenta=1) %>% 
      reshape::cast(fecha~gredad, sum)
    
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    
    #Remuevo el csv
    file.remove('Casos_Confirmados.csv')
    file.remove('datos_fallecidos.csv')
    
    ########### Vacunas Paraguay (PENDIENTE) #############
    ########## LAs vacunas que figuran en la p?gina no tienen la edad
    ########## Solamente hay listado con documento y nombre de los que dieron consentimiento para publicar sus datos
    
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  } # cierra if de Paraguay
  
  
### Resto de los 26 países ###
  
#countries <<- c("BHs", BRB","BLZ","BOL","DOM","ECU","GTM","GUY","HND","HTI","JAM","NIC","PAN","SLV","SUR","TTO", "VEN")
  
# se estiman los casos y las muertes por grupos de edades de acuerdo a la distribución de los fallecidos de ARG #
  
  
  # Descargo casos y muertes totales 
  dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
  
  
  # proporción de fallecidos por grupos de edad para ARG
  distAge <- c(0.001887598,
               0.007639806,
               0.020992082,
               0.057571754,
               0.125386213,
               0.224485133,
               0.266509036,
               0.295528379)
  
  ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
  
  
  ## Barbados
  
  if (pais=="BRB") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="BRB", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
  
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
  
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Barbados 
  
  
  ## Belize
  
  if (pais=="BLZ") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="BLZ", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Belize
  

  ## Bahamas
  
  if (pais=="BHS") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="BHS", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Bahamas
  
  
  ## Republica Dominicana
  
  if (pais=="DOM") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="DOM", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Republica Dominicana
  
  
  ## Ecuador
  
  if (pais=="ECU") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="ECU", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Ecuador
  
  
  ## Guatemala
  
  if (pais=="GTM") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="GTM", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Guatemala
  
  
  ## Guyana
  
  if (pais=="GUY") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="GUY", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Guyana
  
  
  ## Honduras
  
  if (pais=="HND") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="HND", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Honduras
  
  
  ## Haiti
  
  if (pais=="HTI") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="HTI", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra HAiti
  
  
  
  ## Jamaica
  
  if (pais=="JAM") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="JAM", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Jamaica
  
  
  ## Nicaragua
  
  if (pais=="NIC") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="NIC", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Nicaragua
  
  
  ## Panama
  
  if (pais=="PAN") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="PAN", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Panama
  
  
  ## El Salvador
  
  if (pais=="SLV") {
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    
    data <- dataOWD[dataOWD$iso_code=="SLV", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra El Salvador
  
  
  ## Suriname
  
  if (pais=="SUR") {
    
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="SUR", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Suriname
  
  
  ## Trinidad & Tobago
  
  if (pais=="TTO") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="TTO", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Trinidad y Tobago
  
  
  
  ## Venezuela
  
  if (pais=="VEN") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="VEN", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Venezuela
  
  
  ## Bolivia
  
  if (pais=="BOL") {
    
    
    # Descargo casos y muertes totales 
    dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
    
    
    # proporción de fallecidos por grupos de edad para ARG
    distAge <- c(0.001887598,
                 0.007639806,
                 0.020992082,
                 0.057571754,
                 0.125386213,
                 0.224485133,
                 0.266509036,
                 0.295528379)
    
    ageGroups <<- c("00-17", "18-29", "30-39", "40-49","50-59", "60-69", "70-79", "80+")
    
    data <- dataOWD[dataOWD$iso_code=="BOL", c("iso_code","date","new_deaths","new_cases")]
    data$new_deaths <- as.numeric(data$new_deaths)
    data$new_cases <- as.numeric(data$new_cases)
    data[is.na(data)] <- 0
    
    for (i in 1:length(ageGroups)) {
      data[[paste0(ageGroups[i],"_deaths")]] <- data$new_deaths * distAge[i]
      data[[paste0(ageGroups[i],"_cases")]] <- data$new_cases * distAge[i]
    }
    
    
    #Casos 
    
    casos <- data[, c("date", "00-17_cases","18-29_cases","30-39_cases","40-49_cases","50-59_cases", "60-69_cases", "70-79_cases", "80+_cases")] 
    
    setnames(casos, "date", "fecha")
    setnames(casos, grep("_cases", names(casos), value = TRUE), gsub("_cases", "", grep("_cases", names(casos), value = TRUE)))
    
    
    library(dplyr)
    casos <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                               as.Date(max(casos$fecha)),
                                               by=1))) %>% left_join(casos) 
    casos[is.na(casos)] <- 0
    
    
    #defunciones
    
    def <- data[, c("date", "00-17_deaths","18-29_deaths","30-39_deaths","40-49_deaths","50-59_deaths", "60-69_deaths", "70-79_deaths", "80+_deaths")] 
    
    setnames(def, "date", "fecha")
    setnames(def, grep("_deaths", names(def), value = TRUE), gsub("_deaths", "", grep("_deaths", names(def), value = TRUE)))
    
    
    library(dplyr)
    
    def <- data.frame(fecha=as.character(seq(as.Date('2020-03-01'),
                                             as.Date(max(def$fecha)),
                                             by=1))) %>% left_join(def) 
    
    def[is.na(def)] <- 0
    
    #vacunas
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas
    Vacunas0 <- Vacunas #monodosis
    
    
  }  # cierra Bolivia
  
  
  
  
  eval(parse(text=paste0('countryData$',
                         pais,
                         ' <- list(def=def,vac=Vacunas, vac2=Vacunas2, vac0=Vacunas0, casos=casos)')))
  
  print(paste('Actualizado:',pais, Sys.Date()))
  save(countryData,file=paste0('data/data',pais,'.RData'))
  
}


##### formatData function #####

formatData <- function(pais, ageGroups) {

  load(paste0("data/data",pais,".RData"))
  eval(parse(text=paste0('countryData$FMTD <- countryData$',pais)))
  
  
  fechasDef <- countryData[[pais]]$def$fecha
  fechasVac <- countryData[[pais]]$vac$fecha
  fechasVac2 <- countryData[[pais]]$vac2$fecha
  fechasCasos <- countryData[[pais]]$casos$fecha
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


updateDataOWD <- function (countries) {
  dataOWD <- read.csv2("https://covid.ourworldindata.org/data/owid-covid-data.csv", sep = ",")
  dataOWD <- dataOWD %>% dplyr::filter(iso_code %in% countries)
  update <- as.Date(max(dataOWD$date))
  updateWeek <- update-6
  OWDSummaryData <- data.frame()
  
  for (p in countries) {
    filteredData <- dataOWD[dataOWD$iso_code == p,]
    population <- as.numeric(unique(filteredData$population))
    dailyCases <- round(mean(as.numeric(filteredData$new_cases[filteredData$date<=update &
                                                            filteredData$date>=updateWeek])), digits=0)
    dailyDeaths <- round(mean(as.numeric(filteredData$new_deaths[filteredData$date<=update &
                                                              filteredData$date>=updateWeek])), digits=0)
    populationOver65 <- round(as.numeric(unique(filteredData$aged_65_older)), digits = 1)
    totalCases <- as.numeric(last(filteredData$total_cases))
    totalDeaths <- as.numeric(last(filteredData$total_deaths))
    lifeExp <- round(as.numeric(unique(filteredData$life_expectancy)), digits= 1)
    totalTestPerMillon <- as.numeric(last(filteredData$total_tests_per_thousand[filteredData$total_tests_per_thousand!=""]))*1000
    dailyTests <- mean(tail(as.numeric(filteredData$new_tests[filteredData$new_tests!=""]), 7))
    
    filteredData <- data.frame(iso_code=p,
                               metric=c("population",
                                        "dailyCases",
                                        "dailyDeaths",
                                        "populationOver65",
                                        "totalCases",
                                        "totalDeaths",
                                        "lifeExp",
                                        "totalTestPerMillon",
                                        "dailyTests"),
                               value=c(population,
                                       dailyCases,
                                       dailyDeaths,
                                       populationOver65,
                                       totalCases,
                                       totalDeaths,
                                       lifeExp,
                                       totalTestPerMillon,
                                       dailyTests)
                    )
    OWDSummaryData <- rbind.fill(OWDSummaryData,filteredData)
    
  }
  save(OWDSummaryData, file="data/OWDSummaryData.RData")
}

# actualiza OWD
#updateDataOWD(c("ARG","BRA","CHL","COL","MEX","PER","URY","CRI", "PRY", "BHs", "BRB","BLZ","BOL","DOM","ECU","GTM","GUY","HND","HTI","JAM","NIC","PAN","SLV","SUR","TTO", "VEN"))


# actualiza argentina y guarda RData
 #update(pais = "BHS", diasDeProyeccion = 1100)
 #update(pais = "BRB", diasDeProyeccion = 1100)
 #update(pais = "BLZ", diasDeProyeccion = 1100)
 #update(pais = "BOL", diasDeProyeccion = 1100)
 #update(pais = "DOM", diasDeProyeccion = 1100)
 #update(pais = "ECU", diasDeProyeccion = 1100)
 #update(pais = "GTM", diasDeProyeccion = 1100)
 #update(pais = "GUY", diasDeProyeccion = 1100)
 #update(pais = "HND", diasDeProyeccion = 1100)
 #update(pais = "HTI", diasDeProyeccion = 1100)
 #update(pais = "JAM", diasDeProyeccion = 1100)
 #update(pais = "NIC", diasDeProyeccion = 1100)
 #update(pais = "PAN", diasDeProyeccion = 1100)
 #update(pais = "SLV", diasDeProyeccion = 1100)
 #update(pais = "SUR", diasDeProyeccion = 1100)
 #update(pais = "TTO", diasDeProyeccion = 1100)
 #update(pais = "VEN", diasDeProyeccion = 1100)
 
 
 
# agrupa edades
# datosArg <- formatData("URY", ageGroups = ageGroupsV)

