addBox = function(npiIndex,text) {
  enable("go")
  removeUI(selector = "#tail")
  insertUI(
    selector = "#npis-output",
    where = "beforeEnd",
    ui = div(align="center",
             tags$span(HTML(paste0(format(dates[dateIndex],"%m"),"/",format(dates[dateIndex],"%Y"),"<br><b>",text,"</b>"))),
             style=paste0("color: #222426; background-color: ",colores[npiIndex],"; border: 1px solid ; border-radius: 5px; margin: 5px; padding: 5px; float: left;")
    )
  )
  insertUI(
    selector = "#npis-output",
    where = "beforeEnd",
    ui =  tags$span(icon("arrow-right"), style=("margin-top: 15px; float: left;"))
  )
  insertUI(
    selector = "#npis-output",
    where = "beforeEnd",
    ui = div(id="tail", style=("margin-top: 15px; float: left;"),
             tags$span(HTML(paste0("No changes up to ",format(as.Date(date2, "%d-%m-%Y"),"%m"),"/",format(as.Date(date2, "%d-%m-%Y"),"%Y"))), style=("margin-top: 15px;")),
    )
  )
  dateIndex <<- dateIndex + 1
}


addBoxTable <- function (matrixName,country) {
  #`Physical distancing` <<- contact_matrix_scenario * trans_prob_param 
  `Physical distancing` <<- get_custom_matrix(scenario = "Physical distancing",
                                              matrix_list = list(
                                                contact_matrix = contact_matrix,
                                                contact_matrix_work = contact_matrix_work,
                                                contact_matrix_home = contact_matrix_home,
                                                contact_matrix_school = contact_matrix_school,
                                                contact_matrix_other = contact_matrix_other),
                                              ages= as.numeric(ageGroupsV)) * trans_prob_param
  
  `Physical distancing + Shielding of older people` <<- get_custom_matrix(scenario = "Physical distancing + Shielding of older people",
                                                                          matrix_list = list(
                                                                            contact_matrix = contact_matrix,
                                                                            contact_matrix_work = contact_matrix_work,
                                                                            contact_matrix_home = contact_matrix_home,
                                                                            contact_matrix_school = contact_matrix_school,
                                                                            contact_matrix_other = contact_matrix_other),
                                                                          ages= as.numeric(ageGroupsV)) * trans_prob_param
  
  `Physical distancing + Shielding of older people + Self isolation` <<- get_custom_matrix(scenario = "Physical distancing + Shielding of older people + Self isolation",
                                                                          matrix_list = list(
                                                                            contact_matrix = contact_matrix,
                                                                            contact_matrix_work = contact_matrix_work,
                                                                            contact_matrix_home = contact_matrix_home,
                                                                            contact_matrix_school = contact_matrix_school,
                                                                            contact_matrix_other = contact_matrix_other),
                                                                          ages= as.numeric(ageGroupsV)) * trans_prob_param
  
  
  `Physical distancing + Shielding of older people + Self isolation` <<- get_custom_matrix(scenario = "Physical distancing + Shielding of older people + Self isolation",
                                                                                           matrix_list = list(
                                                                                             contact_matrix = contact_matrix,
                                                                                             contact_matrix_work = contact_matrix_work,
                                                                                             contact_matrix_home = contact_matrix_home,
                                                                                             contact_matrix_school = contact_matrix_school,
                                                                                             contact_matrix_other = contact_matrix_other),
                                                                                           ages= as.numeric(ageGroupsV)) * trans_prob_param
  
  `Physical distancing + Shielding of older people + Self isolation + School closures` <<- get_custom_matrix(scenario = "Physical distancing + Shielding of older people + Self isolation + School closures",
                                                                                           matrix_list = list(
                                                                                             contact_matrix = contact_matrix,
                                                                                             contact_matrix_work = contact_matrix_work,
                                                                                             contact_matrix_home = contact_matrix_home,
                                                                                             contact_matrix_school = contact_matrix_school,
                                                                                             contact_matrix_other = contact_matrix_other),
                                                                                           ages= as.numeric(ageGroupsV)) * trans_prob_param
  
  
  `Physical distancing + Shielding of older people + Lockdown + School closures` <<- get_custom_matrix(scenario = "Physical distancing + Shielding of older people + Lockdown + School closures",
                                                                                                             matrix_list = list(
                                                                                                               contact_matrix = contact_matrix,
                                                                                                               contact_matrix_work = contact_matrix_work,
                                                                                                               contact_matrix_home = contact_matrix_home,
                                                                                                               contact_matrix_school = contact_matrix_school,
                                                                                                               contact_matrix_other = contact_matrix_other),
                                                                                                             ages= as.numeric(ageGroupsV)) * trans_prob_param
  
  
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



get_custom_matrix <- function(scenario, 
                              matrix_list = NULL,
                              ages= c(0, 18, 30, 40, 50, 60, 70, 80)){
  
  list2env(matrix_list, .GlobalEnv)
  # columns of olders 
  cols_70_older <- which((ages)>=70)
  # scenarios
  if(scenario == "Physical distancing"){
    out <- 1 * contact_matrix_home + 1 * contact_matrix_work + 1 * contact_matrix_school + 0.5 * contact_matrix_other
  }
  
  if(scenario == "Physical distancing + Shielding of older people"){
    out <- 1 * contact_matrix_home + 
           cbind(.50 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           1 * contact_matrix_school +
           cbind(.50 * contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
           
      
  }
  
  if(scenario == "Physical distancing + Shielding of older people + Self isolation"){
    out <- 1 * contact_matrix_home + 
           cbind(.50 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           1 * contact_matrix_school +
           cbind(.50 * contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
    out <- out * .65
    
  }
  
  if(scenario == "Physical distancing + Shielding of older people + Self isolation + School closures"){
    out <- 1 * contact_matrix_home + 
           cbind(.50 * contact_matrix_work [,-cols_70_older], .25 * contact_matrix_work [,cols_70_older]) +
           0 * contact_matrix_school +
           cbind(.50 * contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
    out <- out * .65
    
  }
  
  if(scenario == "Physical distancing + Shielding of older people + Lockdown + School closures"){
    out <- 1 * contact_matrix_home + 
           .10 * contact_matrix_work +
           0 * contact_matrix_school +
           .10 * contact_matrix_other
    out <- out * .65
    
  }
  
  
  return(out)
}


defaultScenario <- function (country) {
  default_ARG <- c(
    rep("Physical distancing + Shielding of older people + Self isolation + School closures", 8),
    rep("Physical distancing + Shielding of older people + Self isolation", 2),
    rep("Physical distancing + Shielding of older people",1),
    rep("Physical distancing",1)
  )
  browser()
  if (country=="ARG" & 
      identical(customBeta,default_ARG) |
      country=="ARG" & 
      primeraVez==T){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country)
    addBoxTable("Physical distancing + Shielding of older people",input$country)
    addBoxTable("Physical distancing",input$country)
    dateIndex <<- 1
    browser()
    
  }
  
}


