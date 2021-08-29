library(dplyr)
library(reshape)
library(archive)
library(RSocrata)
library(plyr)
library(zip)
library(stringr)
library(data.table)

##### funcion update #####
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
  browser()
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
    
    Vacunas = casos 
    Vacunas[,2:20] <- 0
    Vacunas <- Vacunas[Vacunas$fecha>="2021-01-01",]
    Vacunas2 <- Vacunas

  }
  
  
  eval(parse(text=paste0('countryData$',
                         pais,
                         ' <- list(def=def,vac=Vacunas, vac2=Vacunas2, casos=casos)')))
  
  print(paste('Actualizado:',pais, Sys.Date()))
  save(countryData,file=paste0('data/data',pais,'.RData'))
  
}


##### funcion formatData #####

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



# actualiza argentina y guarda RData
# update(pais = "MEX", diasDeProyeccion = 1100)

# agrupa edades
# datosArg <- formatData("MEX", ageGroups = ageGroupsV)

