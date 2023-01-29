library(readxl)
library(dplyr)
library(plotly)

paises <- readxl::excel_sheets("data/tradeOff.xlsx")

dataPlot <- list()
plot <- plotly::plot_ly()

for (iso_country in paises) {
  dataPlot[[iso_country]]$trade_off_summary <- readxl::read_xlsx("data/tradeOff.xlsx", sheet = iso_country)
  dataPlot[[iso_country]]$m1 <- lm(dataPlot[[iso_country]]$trade_off_summary$muertes_primer_semestre ~ dataPlot[[iso_country]]$trade_off_summary$costo_primer_semestre, data = dataPlot[[iso_country]]$trade_off_summary)
  dataPlot[[iso_country]]$m2 <- lm(dataPlot[[iso_country]]$trade_off_summary$muertes_segundo_semestre ~ dataPlot[[iso_country]]$trade_off_summary$costo_segundo_semestre, data = dataPlot[[iso_country]]$trade_off_summary)
  dataPlot[[iso_country]]$linea1 <- predict(dataPlot[[iso_country]]$m1)
  dataPlot[[iso_country]]$linea2 <- predict(dataPlot[[iso_country]]$m2)
   
}


plot <- plot %>%
  add_trace(
    
    x=~dataPlot[["ARG"]]$trade_off_summary$costo_segundo_semestre*-1, 
    y=~predict(dataPlot[["ARG"]]$m2),
    name = 'Argentina',
    type = "scatter",
    mode = "lines",
    showlegend = F,
    marker=list(symbol = 0,
                opacity = 0),
    line = list(shape = 'spline', 
                color = '#fc8d59', 
                width = 2)) %>% add_annotations(x=(max(dataPlot[["ARG"]]$trade_off_summary$costo_segundo_semestre*-1)+min(dataPlot[["BRA"]]$trade_off_summary$costo_segundo_semestre*-1))/2,
                                                y=(min(predict(dataPlot[["ARG"]]$m2))+max(predict(dataPlot[["ARG"]]$m2)))/2*1.15,
                                                text=paste0("Argentina (slope: ",format(round(dataPlot[["ARG"]]$m2[1]$coefficients[2]*-1,1)), nsmall=1,")"),
                                                showarrow = F) %>%
  add_trace(
    
    x=~dataPlot[["BRA"]]$trade_off_summary$costo_segundo_semestre*-1, 
    y=~predict(dataPlot[["BRA"]]$m2),
    name = 'Brasil',
    type = "scatter",
    mode = "lines",
    showlegend = F,
    marker=list(symbol = 0,
                opacity = 0),
    line = list(shape = 'spline', 
                color = '#d7191c', 
                width = 2)) %>% add_annotations(x=(max(dataPlot[["BRA"]]$trade_off_summary$costo_segundo_semestre*-1)+min(dataPlot[["BRA"]]$trade_off_summary$costo_segundo_semestre*-1))/2,
                                                y=(min(predict(dataPlot[["BRA"]]$m2))+max(predict(dataPlot[["BRA"]]$m2)))/2*1.15,
                                                text=paste0("Brasil (slope: ",format(round(dataPlot[["BRA"]]$m2[1]$coefficients[2]*-1,1)), nsmall=1,")"),
                                                showarrow = F) %>%
  add_trace(
    
    x=~dataPlot[["MEX"]]$trade_off_summary$costo_segundo_semestre*-1, 
    y=~predict(dataPlot[["MEX"]]$m2),
    name = 'México',
    type = "scatter",
    mode = "lines",
    showlegend = F,
    marker=list(symbol = 0,
                opacity = 0),
    line = list(shape = 'spline', 
                color = '#91bfdb', 
                width = 2)) %>% add_annotations(x=(max(dataPlot[["MEX"]]$trade_off_summary$costo_segundo_semestre*-1)+min(dataPlot[["MEX"]]$trade_off_summary$costo_segundo_semestre*-1))/2,
                                                y=(min(predict(dataPlot[["MEX"]]$m2))+max(predict(dataPlot[["MEX"]]$m2)))/2*1.15,
                                                text=paste0("Mexico (slope: ",format(round(dataPlot[["MEX"]]$m2[1]$coefficients[2]*-1,1)), nsmall=1,")"),
                                                showarrow = F) %>%
  add_trace(
    
    x=~dataPlot[["JAM"]]$trade_off_summary$costo_segundo_semestre*-1, 
    y=~predict(dataPlot[["JAM"]]$m2),
    name = 'Jamaica',
    type = "scatter",
    mode = "lines",
    showlegend = F,
    marker=list(symbol = 0,
                opacity = 0),
    line = list(shape = 'spline', 
                color = '#91bfdb', 
                width = 2)) %>% add_annotations(x=(max(dataPlot[["JAM"]]$trade_off_summary$costo_segundo_semestre*-1)+min(dataPlot[["JAM"]]$trade_off_summary$costo_segundo_semestre*-1))/2,
                                                y=(min(predict(dataPlot[["JAM"]]$m2))+max(predict(dataPlot[["JAM"]]$m2)))/2*2.5,
                                                text=paste0("Jamaica (slope: ",format(round(dataPlot[["JAM"]]$m2[1]$coefficients[2]*-1,1)), nsmall=1,")"),
                                                showarrow = F) %>% layout(title = '<br>Economic and epidemiological trade-off by country',
                                                                          yaxis = list(title = 'Daily deaths (average)'),
                                                                          xaxis = list(title = 'GDP loss (average)<br>'))


plot

