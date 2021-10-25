addBox = function(npiIndex,text) {
  removeUI(selector = "#tail")
  insertUI(
    selector = "#npis-output",
    where = "beforeEnd",
    ui = div(align="center",
             tags$span(HTML(paste0(format(dates[dateIndex],"%m"),"/",format(dates[dateIndex],"%Y"),"<br><b>",text,"</b>"))),
             style=paste0("color: #222426; background-color: ",colores[npiIndex],"; border: 1px solid ; border-radius: 5px; margin: 5px; padding: 5px;")
    )
  )
  insertUI(
    selector = "#npis-output",
    where = "beforeEnd",
    ui =  tags$span(icon("arrow-right"), style=("margin-top: 15px;"))
  )
  insertUI(
    selector = "#npis-output",
    where = "beforeEnd",
    ui = div(id="tail", style=("margin-top: 15px;"),
             tags$span(HTML(paste0("No changes up to ",format(as.Date(date2, "%d-%m-%Y"),"%m"),"/",format(as.Date(date2, "%d-%m-%Y"),"%Y"))), style=("margin-top: 15px;")),
    )
  )
  dateIndex <<- dateIndex + 1
}


addBoxTable <- function (matrixName) {
  if (is.na(customBeta[1,1])) {
    customBeta$start[1]<<-tHoy+4
    customBeta$end[1]<<-diasDeProyeccion
    customBeta$beta[1]<<-matrixName
    customMatrix <<- T 
  } else {
    rows <- nrow(customBeta)
    customBeta[rows+1,]<<-c(as.numeric(customBeta$start[rows])+as.numeric(lubridate::days_in_month(rows)),diasDeProyeccion,matrixName)
    customBeta$end[rows]<<-as.numeric(customBeta$start[rows+1])-1
    customMatrix <<- T
  }
}
