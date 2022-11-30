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
                              dia_loop=0,
                              matrix_list = NULL,
                              ages= c(0, 18, 30, 40, 50, 60, 70, 80),
                              muertes=0){
  
  list2env(matrix_list, .GlobalEnv)
  # columns of olders 
  cols_70_older <- which((ages)>=70)
  # if (dia_loop==0) { <- 1} else {modificador_eco <- getModificadorActividadLaboral(dia_loop, 0, )}
  # modificador_eco <- getModificadorActividadLaboral(dia_loop, 0, iso_country) # agregar country y asegurarse que devuelva NA cuando t ==0 y cuando pais no modelo
  modificador_eco <- work_mob(dia_loop,matchDavies(customBeta$beta),muertes)
  usar_davies <- is.na(modificador_eco) 
  
  
  if (usar_davies) {
    # print(dia_loop)
    # print('davies')
    # scenarios
    if(scenario == "Physical distancing"){
      out <- 1 * contact_matrix_home + 
             0.5 * contact_matrix_work + 
             1 * contact_matrix_school + 
             0.5 * contact_matrix_other
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
      #print(modificador_eco)
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
  } else { # Usa modelo eco
    # scenarios
    # if (exists("costo_economico")==F) {
    #   costo_economico <<- data.frame(fecha=NA,
    #                                  costo=NA,
    #                                  muertes=NA,
    #                                  escenario=NA)[-1,]
    #   costo_economico[nrow(costo_economico)+1,1] <<- as.character(fechas_master[(dia_loop)])
    #   costo_economico[nrow(costo_economico),2] <<- loss_t(work_mob(dia_loop,matchDavies(customBeta$beta),muertes))
    #   costo_economico[nrow(costo_economico),3] <<- muertes
    #   if (ECORunning==F) {
    #     costo_economico[nrow(costo_economico),4] <<- "DEFAULT"  
    #   } else {
    #     costo_economico[nrow(costo_economico),4] <<- nombreEscenario  
    #   }
    #   
    #   
    #   
    # } else {
    #   costo_economico[nrow(costo_economico)+1,1] <<- as.character(fechas_master[dia_loop])
    #   costo_economico[nrow(costo_economico),2] <<- loss_t(work_mob(dia_loop,matchDavies(customBeta$beta),muertes))
    #   costo_economico[nrow(costo_economico),3] <<- muertes
    #   if (ECORunning==F) {
    #     costo_economico[nrow(costo_economico),4] <<- "DEFAULT"  
    #   } else {
    #     costo_economico[nrow(costo_economico),4] <<- nombreEscenario  
    #   }
    # }
    
    if (ECORunning==F) {
      print(paste0("costo principal", " - dia: ", length(costo_economico_principal), " - pais: ", iso_country))
      costo_economico_principal <<- c(costo_economico_principal,loss_t(work_mob(dia_loop,matchDavies(customBeta$beta),muertes)))
      costo_economico_principal_fecha <<- c(costo_economico_principal_fecha,as.character(fechas_master[dia_loop]))
    } else {
      print(paste0("costo alternativo", " - dia: ", length(costo_economico_alternativo), " - pais: ", iso_country))
      costo_economico_alternativo <<- c(costo_economico_alternativo,loss_t(work_mob(dia_loop,matchDavies(customBeta$beta),muertes)))
      costo_economico_alternativo_fecha <<- c(costo_economico_alternativo_fecha,as.character(fechas_master[dia_loop]))
      costo_economico_alternativo_muertes <<- c(costo_economico_alternativo_muertes,muertes)
    }
    
    # print(dia_loop)
    # print('eco')
    if(scenario == "Physical distancing"){
      out <- 1 * contact_matrix_home + modificador_eco * contact_matrix_work + 1 * contact_matrix_school + 0.5 * contact_matrix_other
    }
    
    if(scenario == "Physical distancing + Shielding of older people"){
      out <- 1 * contact_matrix_home + 
        modificador_eco * contact_matrix_work +
        1 * contact_matrix_school +
        cbind(.50 * contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
    }
    
    if(scenario == "Physical distancing + Shielding of older people + Self isolation"){
      out <- 1 * contact_matrix_home + 
        modificador_eco * contact_matrix_work +
        1 * contact_matrix_school +
        cbind(.50 * contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
      out <- out * .65
      #print(modificador_eco)
    }
    
    if(scenario == "Physical distancing + Shielding of older people + Self isolation + School closures"){
      out <- 1 * contact_matrix_home + 
        modificador_eco * contact_matrix_work +
        0 * contact_matrix_school +
        cbind(.50 * contact_matrix_other [,-cols_70_older], .25 * contact_matrix_other [,cols_70_older])
      out <- out * .65
    }
    
    if(scenario == "Physical distancing + Shielding of older people + Lockdown + School closures"){
      out <- 1 * contact_matrix_home + 
        modificador_eco * contact_matrix_work +
        0 * contact_matrix_school +
        .10 * contact_matrix_other
      out <- out * .65
    }
  }
  return(out)
}


defaultScenario <- function (country) {
  default_ARG <- c(
    rep("Physical distancing + Shielding of older people + Self isolation + School closures", 2),
    rep("Physical distancing + Shielding of older people + Self isolation", 2),
    rep("Physical distancing + Shielding of older people + Self isolation + School closures", 3),
    rep("Physical distancing + Shielding of older people + Self isolation", 1),
    rep("Physical distancing + Shielding of older people",4)
  )
  
  default_COL <- c(
    rep("Physical distancing",3),
    rep("Physical distancing + Shielding of older people + Lockdown + School closures",2),
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",1),
    rep("Physical distancing + Shielding of older people",3),
    rep("Physical distancing",3)
  )
  
  default_BRA <- c(
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",12)
    
  )
  
  default_CHL <- c(
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",2),
    rep("Physical distancing + Shielding of older people + Lockdown + School closures",3),
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",1),
    rep("Physical distancing + Shielding of older people + Lockdown + School closures",3),
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",3)
  )
  
  default_PER <- c(
    rep("Physical distancing + Shielding of older people + Lockdown + School closures",1),
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",2),
    rep("Physical distancing + Shielding of older people + Lockdown + School closures",1),
    rep("Physical distancing + Shielding of older people + Self isolation + School closures",8)
  )
  
  default_MEX <- c(
    rep("Physical distancing + Shielding of older people + Self isolation", 12)
  )
  
  
  
  if (country=="ARG" & 
      identical(customBeta$beta,default_ARG) |
      country=="ARG" & 
      primeraVez==T){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    dateIndex <<- 1
    
  }
  
  if (country=="COL" & 
      identical(customBeta$beta,default_COL)==F |
      country=="COL" & 
      primeraVez==T){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    
    removeUI(selector = "#npis-output")
    insertUI(selector = "#npis-col",
             where = "beforeEnd",
             fluidRow(id = "npis-output",
                      div(id = "tail", tags$span("Seleccionar medidas..."), style = 'float:left;')
             )
    )
    

    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    addBox(1,"<br>Distanciamiento social,<br>uso de mascarillas faciales<br><br>")
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing",input$country)
    addBoxTable("Physical distancing",input$country)
    addBoxTable("Physical distancing",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country) #5
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people",input$country)
    addBoxTable("Physical distancing + Shielding of older people",input$country)
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    addBoxTable("Physical distancing",input$country)
    addBoxTable("Physical distancing",input$country)
    addBoxTable("Physical distancing",input$country)
    
    dateIndex <<- 1
    
  }
  
  
  if (country=="BRA" & 
      identical(customBeta$beta,default_COL)==F |
      country=="BRA" & 
      primeraVez==T){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    
    removeUI(selector = "#npis-output")
    insertUI(selector = "#npis-col",
             where = "beforeEnd",
             fluidRow(id = "npis-output",
                      div(id = "tail", tags$span("Seleccionar medidas..."), style = 'float:left;')
             )
    )
    
    
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    
    
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country) #5
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    dateIndex <<- 1
    
  }
  
  if (country=="CHL" & 
      identical(customBeta$beta,default_COL)==F |
      country=="CHL" & 
      primeraVez==T){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    
    removeUI(selector = "#npis-output")
    insertUI(selector = "#npis-col",
             where = "beforeEnd",
             fluidRow(id = "npis-output",
                      div(id = "tail", tags$span("Seleccionar medidas..."), style = 'float:left;')
             )
    )
    
    
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    
    

    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    
    dateIndex <<- 1
    
  }
  
  if (country=="PER" & 
      identical(customBeta$beta,default_COL)==F |
      country=="PER" & 
      primeraVez==T){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    
    removeUI(selector = "#npis-output")
    insertUI(selector = "#npis-col",
             where = "beforeEnd",
             fluidRow(id = "npis-output",
                      div(id = "tail", tags$span("Seleccionar medidas..."), style = 'float:left;')
             )
    )
    
    
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(5,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas y confinamiento")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    addBox(4,"Distanciamiento social, mascarillas<br>faciales, aislamiento de ancianos<br>y aislamiento personal,<br>con cierre de escuelas")
    
    
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Lockdown + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country)
    
    
    dateIndex <<- 1
    
  }
  
  if (country=="MEX" & 
      identical(customBeta$beta,default_ARG) |
      country=="MEX" & 
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
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    
    dateIndex <<- 1
    
  }
  
  
  if (country=="JAM" & 
      identical(customBeta$beta,default_ARG) |
      country=="JAM" & 
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
    addBox(3,"Distanciamiento social,<br>mascarillas faciales,<br>aislamiento de ancianos y<br>aislamiento personal")
    addBox(2,"Distanciamiento social,<br>uso de mascarillas faciales<br>y aislamiento de ancianos<br><br>")
    
    tHoy <<- tVacunasCero+4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation + School closures",input$country) #4
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people + Self isolation",input$country) #3
    addBoxTable("Physical distancing + Shielding of older people",input$country) #2
    
    dateIndex <<- 1
    
  }
}


altScenario <- function (country,stringency) {
  scenarios <- c ('Physical distancing',
                  'Physical distancing + Shielding of older people',
                  'Physical distancing + Shielding of older people + Self isolation',
                  'Physical distancing + Shielding of older people + Self isolation + School closures',
                  'Physical distancing + Shielding of older people + Lockdown + School closures')
  
  label <- c('Distanciamiento social, uso de mascarillas faciales',
             'Agrega aislamiento de ancianos',
             'Agrega aislamiento personal',
             'Agrega cierre de escuelas',
             'Implementa confinamiento total')
  for (i in 1:length(st)) {
    st[i] <<- scenarios[which(label==st[i])]
  }
  alt_ARG <- st
  alt_BRA <- st
  alt_MEX <- st
  
  
  if (country=="MEX"){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    tHoy <<- tVacunasCero+4
    
    for (i in 1:length(alt_MEX)) {
      addBoxTable(alt_MEX[i],input$country)
    }
    
    dateIndex <<- 1
    
  }
  
  if (country=="BRA"){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    tHoy <<- tVacunasCero+4
    
    for (i in 1:length(alt_BRA)) {
      addBoxTable(alt_BRA[i],input$country)
    }
    
    dateIndex <<- 1
    
  }
  if (country=="ARG"){
    
    customBeta <<- data.frame(start=NA,
                              end=NA,
                              beta=NA)
    tHoy <<- tVacunasCero+4
    
    for (i in 1:length(alt_ARG)) {
      addBoxTable(alt_ARG[i],input$country)
    }
    
    dateIndex <<- 1
    
  }
  
}


# tradeOffScenario <- function (country, scenario) {
#   rows_costo_economico <<- nrow(costo_economico)
#   customBeta <<- data.frame(start=NA,
#                             end=NA,
#                             beta=NA)
#   tHoy <<- tVacunasCero+4
#   
#   for (i in 1:length(trade_off_scenarios()[[scenario]])) {
#     addBoxTable(trade_off_scenarios()[[scenario]],input$country)
#   }
# 
#   dateIndex <<- 1
#   nombreEscenario <<- paste("Trade-off:",
#                             scenario)
#   
# }
#   



