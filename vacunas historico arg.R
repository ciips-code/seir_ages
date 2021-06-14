library(dplyr)
library(reshape)

creaAv <-  function(diaCeroModelo, diasDeProyeccion) {
  #browser()
  #download.file('https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip', 'datos_nomivac_covid19.zip')
  #unzip('datos_nomivac_covid19.zip','datos_nomivac_covid19.csv')
  #file.remove('datos_nomivac_covid19.csv')
  #file.remove('datos_nomivac_covid19.zip')
  #VacunasArg = read.csv2('nomivac_corto.csv', sep=',', encoding = 'UTF-8')
  # VacunasArg = read.csv2('datos_nomivac_covid19.csv', sep=',', encoding = 'UTF-8')
  # VacunasArg = VacunasArg %>% dplyr::filter(orden_dosis==1 &
  #                                             fecha_aplicacion!="S.I.") %>% 
  #   dplyr::mutate(edad = case_when(grupo_etario %in% c("18-29") ~ "00 a 29",
  #                                  grupo_etario %in% c("30-39","40-49","50-59") ~ "30 a 59",
  #                                  grupo_etario %in% c("60-69","70-79","80-89","90-99",">=100") ~ "60 y mas",
  #                                  TRUE ~ "Sin esp.")) %>%
  #   dplyr::filter(edad!="Sin esp.") %>%
  #   dplyr::group_by(fecha_aplicacion, edad) %>%
  #   dplyr::summarise(n=n()) %>%
  #   reshape::cast(fecha_aplicacion~edad, mean) %>%
  #   dplyr::mutate(fecha_aplicacion=as.Date(fecha_aplicacion))
  # 
  # VacunasArg[is.na(VacunasArg)] <- 0
  # 
  load("vacunasArg.RData")
  
  diaCeroVac <- min(VacunasArg$fecha_aplicacion)
  
  tVacunasCero <-  as.Date(diaCeroVac)-as.Date(diaCeroModelo)
  
  vacPre = lapply(1:(as.numeric(tVacunasCero)-1), matrix, data=c(0,0,0,
                                                                 0,0,0,
                                                                 0,0,0), 
                                                          nrow=3, 
                                                          ncol=ncol(VacunasArg)-1)
  
  vacArg = lapply(1:nrow(VacunasArg), matrix, data=c(0,0,0,
                                                     0,0,0,
                                                     0,0,0), 
                  nrow=3, 
                  ncol=ncol(VacunasArg)-1)
  
  for (t in 1:length(vacArg)) {
    vacArg[[t]][3,]  = as.numeric(VacunasArg[t,2:4])
  }
  
  promedio = round(Reduce(`+`, vacArg[(length(vacArg)-8):(length(vacArg)-1)])/7,0)
  vacPlan = lapply(1:(diasDeProyeccion-length(vacArg)-length(vacPre)), matrix, data=t(promedio), 
                   nrow=3, 
                   ncol=3)
  
  Av = c(vacPre,vacArg,vacPlan)
  return(result=list(Av=Av,
                     diaPlan=length(vacPre)+length(vacArg)))
                
}





