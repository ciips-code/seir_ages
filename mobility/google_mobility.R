library(covmobility)
library(dplyr)
library(zoo)
library(stats)
library(plotly)

data(google_mobility)
mobility <- google_mobility %>% filter(country_region=="Argentina" &
                                       is.na(sub_region_1) &
                                       type == "transit") %>%
                                mutate(pct_diff_smooth = predict(loess(pct_diff~seq(1,nrow(mobility)),span=.1)))

plotly_lineas <- function (df,x_axis,y_axis,y_names) {
  fig <- plot_ly(df, x = ~x_axis)
  
  eje_y <- c()
  
  for (var in y_axis)
  {eje_y <- c(eje_y,paste0("fig <- fig %>% add_trace(y = ~",var,", name = '",y_names[length(eje_y)+1],"',mode = 'lines')"))}
  
  eval(parse(text=paste(eje_y,collapse=';')))
  
  return(fig)
}

plotly_lineas(df=mobility,
              x_axis=mobility$date,
              y_axis=c("mobility$pct_diff","mobility$pct_diff_smooth"),
              y_names=c("crudo","suavizado"))

