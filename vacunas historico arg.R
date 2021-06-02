library(dplyr)
library(reshape)

download.file('https://sisa.msal.gov.ar/datos/descargas/covid-19/files/datos_nomivac_covid19.zip', 'datos_nomivac_covid19.zip')
unzip('datos_nomivac_covid19.zip','datos_nomivac_covid19.csv')
VacunasArg = read.csv2('datos_nomivac_covid19.csv', sep=',', encoding = 'UTF-8')
# file.remove('datos_nomivac_covid19.csv')
#file.remove('datos_nomivac_covid19.zip')


VacunasArg = VacunasArg %>% dplyr::filter(orden_dosis==1 &
                                          fecha_aplicacion!="S.I.") %>% 
                            dplyr::mutate(edad = case_when(grupo_etario %in% c("18-29") ~ "00 a 29",
                                          grupo_etario %in% c("30-39","40-49","50-59") ~ "30 a 59",
                                          grupo_etario %in% c("60-69","70-79","80-89","90-99",">=100") ~ "60 y mas",
                                          TRUE ~ "Sin esp.")) %>%
                            dplyr::filter(edad!="Sin esp.") %>%
                            dplyr::group_by(fecha_aplicacion, edad) %>%
                            dplyr::summarise(n=n()) %>%
                            reshape::cast(fecha_aplicacion~edad, mean) %>%
                            dplyr::mutate(fecha_aplicacion=as.Date(fecha_aplicacion))


VacunasArg[is.na(VacunasArg)==T]=0

VacunasArg = left_join(data.frame(fecha_aplicacion=seq(as.Date(min(VacunasArg$fecha_aplicacion)),
                                                       as.Date(max(VacunasArg$fecha_aplicacion)),
                                                               by=1)),
                       VacunasArg)
VacunasArg$t = c(1:nrow(VacunasArg))


vacArg = lapply(1:nrow(VacunasArg), matrix, data=c(0,0,0,
                                                   0,0,0,
                                                   0,0,0), 
                                            nrow=3, 
                                            ncol=ncol(VacunasArg)-2)
for (t in 1:length(vacArg)) {
  vacArg[[t]][3,]  = as.numeric(VacunasArg[t,2:4])
}

promedio = round(Reduce(`+`, vacArg[(length(vacArg)-8):(length(vacArg)-1)])/7,0)
vacPlan = lapply(1:(500-length(vacArg)), matrix, data=t(promedio), 
                                                     nrow=3, 
                                                     ncol=ncol(VacunasArg)-2)

