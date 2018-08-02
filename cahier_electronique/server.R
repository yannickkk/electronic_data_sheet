source("connect.R")

##################                  SERVER                 ################# 

## Dataframe pour les prélevements :

df_prelevement <- data.frame(dbGetQuery(con, "select sat_type from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sac_id"), 
                             dbGetQuery(con, "select sal_localisation from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sac_id"),
                             dbGetQuery(con, "select sac_conditionnement from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sac_id"),
                             dbGetQuery(con,"select sas_solvant from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal, lu_tables.tr_samples_solvant_sas where sat_id=sac_sat_id and sat_id=sal_sat_id and sas_sat_id=sat_id and sac_id=sas_sac_id order by sac_id"))

colnames(df_prelevement)<-c("prel_type","prel_local","prel_condi", "prel_solv")


## Dataframe pour les blessures :

df_blessure <- data.frame(dbGetQuery(con,"select bll_localisation from lu_tables.tr_blessure_localisation_bll, lu_tables.tr_blessure_gravite_blg where blg_bll_id=bll_id"),
                          dbGetQuery(con, "select blg_gravite from lu_tables.tr_blessure_localisation_bll, lu_tables.tr_blessure_gravite_blg where blg_bll_id=bll_id"))

colnames(df_blessure)<-c("ble_local","ble_gravite")


server <- function(input, output,session) {
  
  ##################              RUBRIQUE ANIMAL                       #################
  
  updateSelectizeInput(session, "idRFID", choices = choix[["idRFID"]], selected = NULL) 
  updateSelectizeInput(session, "idSite", choices = choix[["idSite"]], selected = NULL)
  updateSelectizeInput(session, "nAnimal2", choices =choix[["nAnimal2"]], selected = NULL)
  updateNumericInput(session, "cirCou", max = choix[["cirCou"]])
  updateNumericInput(session, "lPattArriere", max = choix[["lPattArriere"]])
  updateNumericInput(session, "lBoisGauche", max = choix[["lBoisGauche"]])
  updateNumericInput(session, "lBoisDroit", max = choix[["lBoisDroit"]])
  updateSelectizeInput(session, "etatBois", choices = choix[["etatBois"]], selected = NULL)
  #updateSelectizeInput(session, "idTagOrG2", choices = choix[["idTagOrG2"]], selected = NULL)
  #updateSelectizeInput(session, "idTagOrD2", choices = choix[["idTagOrD2"]], selected = NULL)
  #updateSelectizeInput(session, "idRFID2", choices = choix[["idRFID2"]], selected = NULL)
  updateSelectizeInput(session, "idSite2", choices = choix[["idSite2"]], selected = NULL)
  #updateSelectizeInput(session, "idRFID_new", choices = choix[["idRFID_new"]], selected = NULL) 
  #updateSelectizeInput(session, "age", choices = choix[["age"]], selected = NULL) 
  updateSelectizeInput(session, "numSabot", choices = choix[["numSabot"]], selected = NULL) 
  
  #########          Sélection site/RFID/tag à partir du n°animal                   #########
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_gauche from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagOrG2 <- resres[1,1]
      updateSelectizeInput(session, "idTagOrG2",  selected = (idTagOrG2))
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_droit from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq = '", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagOrD2 <- resres[1,1]
      updateSelectizeInput(session, "idTagOrD2", selected = idTagOrD2)
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id and ani_etiq='",input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagRfid <- resres[1, 1]
      if (!is.null(idTagRfid)){
        updateSelectizeInput(session, "idRFID2", selected = idTagRfid)
      }
      else (updateSelectizeInput(session, "idRFID2", options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and ani_etiq = '", input$nAnimal2, "' order by cap_date DESC))")
      resres = dbGetQuery(con,str)
      idSite2 <- resres[1, 1]
      updateSelectizeInput(session, "idSite2", selected = idSite2)
    }
  })
  
  observeEvent(input$nAnimal2, {
    if ((input$nAnimal2)!="") {
      str = paste0("select ani_sexe from public.t_animal_ani where ani_etiq ='", input$nAnimal2, "'")
      resres = dbGetQuery(con,str)
      sexe <- resres[1, 1]
      updateAwesomeRadio(session, "sexe", selected = sexe)
    }
  })
  
  testNouvelAnimal = observeEvent(input$estNouvelAnimal, {
    if (input$estNouvelAnimal=="non"){
      updateAwesomeRadio(session, "identifie", choices = c("oui","non"), selected = "oui")
    }
    if (input$estNouvelAnimal=="oui"){
      updateAwesomeRadio(session, "identifie", choices = c("oui","non"), selected = "non")
    }  
  })
  
  #########          Sélection nAnimal/RFID/site/tagG à partir du tagD              #########
  
  observeEvent(input$idTagOrD2,{
    if ((input$idTagOrD2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap where cap_ani_id = ani_id and cap_tag_droit ='", input$idTagOrD2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      nAnimalFound <- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
  #########          Sélection nAnimal/RFID/site/tagD à partir du tagG              #########
  
  observeEvent(input$idTagOrG2,{
    if ((input$idTagOrG2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap where cap_ani_id = ani_id and cap_tag_gauche ='", input$idTagOrG2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      nAnimalFound <- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
  #########          Sélection nAnimal/site/tagD/tagG à partir du RFID              #########
  
  observeEvent(input$idRFID2,{
    if ((input$idRFID2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap, public.t_rfid_rfi where cap_id = rfi_cap_id and cap_ani_id = ani_id and rfi_tag_code ='", input$idRFID2,"'")
      resres = dbGetQuery(con,str)
      nAnimalFound <- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
  #########          Vérification des tags  (metal ou non)                          ######### 
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_droit_metal from public.t_capture_cap, public.t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"'")
      resres = dbGetQuery(con,str)
      tag_droit_metal <- resres[1,1]
      updateCheckboxInput(session, "metal_tag_d2", value = tag_droit_metal)
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_gauche_metal from public.t_capture_cap, public.t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"'")
      resres = dbGetQuery(con,str)
      tag_gauche_metal <- resres[1,1]
      updateCheckboxInput(session, "metal_tag_g2", value = tag_gauche_metal)
    }
  })
  
  listTagD = dbGetQuery(con,"select distinct cap_tag_droit from public.t_capture_cap")
  names(listTagD)<-c("nom")
  listTagG = dbGetQuery(con,"select distinct cap_tag_gauche from public.t_capture_cap")
  names(listTagG)<-c("nom")
  listTag = rbind(listTagD,listTagG)
  
  reactive_tagG <- reactive({ 
    stock_tagG <- input$idTagOrG
  })
  
  slow_tagG <- debounce(reactive_tagG, 1500)
  
  reactive_tagD <- reactive({ 
    stock_tagD <- input$idTagOrD
  })
  
  slow_tagD <- debounce(reactive_tagD, 1500)
  
  output$tagDroitExiste <- renderUI({
    if (!is.null(slow_tagD())) {
      for (i in listTag) {
        if (slow_tagD() %in% i)
        {shinyalert("TAG DROIT DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrD", value = "")} 
    } 
}  })
  
  output$tagGaucheExiste <- renderUI({
    if (!is.null(slow_tagG())) {
      for (i in listTag) {
        if (slow_tagG() %in% i)
        {shinyalert("TAG GAUCHE DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrG", value = "")} }
      }
  })
  
  reactive_tagG2 <- reactive({ 
    stock_tagG2 <- input$idTagOrG3
  })
  
  slow_tagG2 <- debounce(reactive_tagG2, 1500)
  
  reactive_tagD2 <- reactive({ 
    stock_tagD2 <- input$idTagOrD3
  })
  
  slow_tagD2 <- debounce(reactive_tagD2, 1500)
  
  output$tagDroitExiste2 <- renderUI({
    if (!is.null(slow_tagD2())) {
      for (i in listTag) {
        if (slow_tagD2() %in% i)
        {shinyalert("TAG DROIT DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrD3", value = "")} 
      } 
    }  })
  
  output$tagGaucheExiste2 <- renderUI({
    if (!is.null(slow_tagG2())) {
      for (i in listTag) {
        if (slow_tagG2() %in% i)
        {shinyalert("TAG GAUCHE DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
          updateTextInput(session, "idTagOrG3", value = "")} }
    }
  })
  
  #########          Vérification existence numéro nouvel animal                    ##########   
  
  listAnimal = dbGetQuery(con,"select distinct ani_etiq from public.t_animal_ani")
  
  reactive_nAnimal <- reactive({ 
    stock_nAnimal <- input$nAnimal
  })
  
  slow_nAnimal <- debounce(reactive_nAnimal, 1500)
  
  output$animalExiste <- renderUI({
      if (!is.null(slow_nAnimal())){
        for (i in listAnimal) {
          if (toupper(slow_nAnimal() %in% i))
          {shinyalert("ANIMAL DEJA EXISTANT!", "Décocher '1ere capture' ou choisir un autre 'ani_etiq'", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)
            updateTextInput(session, "nAnimal", value = "")} }
      }
    })

  
  #########          Test données: poids, num sabot , tour de cou, lg patte, bois   ######### 
  
  ### Poids
  
  output$poids_ani = renderText({input$pSabotPlein-input$pSabotVide})
  
  output$alert_poids <- renderUI({
    if (!is.na(input$pSabotPlein) && !is.na(input$pSabotVide)) {
      if ((input$pSabotPlein-input$pSabotVide)>40) {
        shinyalert("STOP!", "Poids supérieur à 40kgs!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }} })
  
  ### Sabot
  
  liste_sabot = dbGetQuery(con,"select distinct sab_valeur from lu_tables.tr_sabots_sab")
  
  output$sabotExiste <- renderUI({
    if ((input$numSabot)!="") {
      for (i in liste_sabot) {
        if (!(input$numSabot %in% i))
        {shinyalert("STOP!", "Est-ce un nouveau numero de sabot ?", type = "warning",confirmButtonText="Oui", showCancelButton=T,cancelButtonText="Non",html=TRUE, callbackR = modalCallback_num_sabot)} 
      }}
  })
  
  modalCallback_num_sabot <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "numSabot" , value = 0)}
    else (dbSendQuery(con,sprintf("INSERT INTO lu_tables.tr_sabots_sab (sab_valeur) VALUES ('%s')", input$numSabot))) }
  
  output$out_sabot_plein <- renderUI({
    if (!is.na(input$pSabotPlein)) {
      if (input$pSabotPlein>65) {
        shinyalert("STOP!", " Poids Sabot plein elevé!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_sabot_plein )
      }} })  
  
  modalCallback_sabot_plein <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "pSabotPlein" , value = NA)}}
  
  output$out_sabot_vide <- renderUI({
    if (!is.na(input$pSabotVide)) {
      if (input$pSabotVide>50) {
        shinyalert("STOP!", " Poids Sabot vide elevé!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_sabot_vide )
      }} }) 
  
  modalCallback_sabot_vide <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "pSabotVide" , value = NA)}}
  
  
  ### Cou
  
  output$out_cirCou <- renderUI({
    if (!is.na(input$cirCou)) {
    if (input$cirCou > dbGetQuery(con,"select max(cap_circou) from public.t_capture_cap")) {
      shinyalert("STOP!", "Circonference élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_circou)
    }}})
  
  modalCallback_circou <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "cirCou" , value = NA)}}
  
  ### Patte
  
  output$out_lPattArriere <- renderUI({
    if(!is.na(input$lPattArriere)) {
    if (input$lPattArriere > dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")) {
      shinyalert("STOP!", "Longueur patte élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_lg_patte)
    }}})
  
  modalCallback_lg_patte <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "lPattArriere" , value = NA)}}
  
  ### Bois
  
  output$out_lBoisGauche <- renderUI({
    if (!is.na(input$lBoisGauche)) {
    if (input$lBoisGauche > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
      shinyalert("STOP!", "Longueur bois gauche elevee", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
    }}})
  
  output$out_lBoisDroit <- renderUI({
    if (!is.na(input$lBoisDroit)) {
    if (input$lBoisDroit > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
      shinyalert("STOP!", "Longueur bois droit elevee", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
    }}})
  
  liste_etatbois = dbGetQuery(con,"select distinct etb_description from lu_tables.tr_etat_bois_etb order by etb_description")
  
  observeEvent(input$etatBois, {
    for (i in liste_etatbois) {
      if (!(input$etatBois %in% i)) {
        if (input$etatBois != "")
        {shinyalert("WAIT!", "Est-ce un nouvel état de bois ?", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_new_etatbois)} 
      }}
  })
  
  modalCallback_new_etatbois <- function(value) {
    if (value == TRUE) {
      (dbSendQuery(con,sprintf("INSERT INTO lu_tables.tr_etat_bois_etb (etb_description) VALUES ('%s')", input$etatBois))) }}
  
  #########          Récupération de l'heure                                        #########    
  

  
  observeEvent(input$to_current_time_caract, {
    gettime_caract_posix <<- Sys.time()
    gettime_caract= as.character(Sys.time())
    gettime_caract=strsplit(gettime_caract, " ")[[1]]
    gettime_caract=gettime_caract[2]
    
    #output$time_caract <- renderText({gettime})
      
    updateTextInput(session, "time_caract", value = gettime_caract)
  })
  
  #########          Récupération du site                                           #########    
  
  liste_date <- dbGetQuery(con,"select cap_date from t_capture_cap")
  
  observeEvent(input$nAnimal2, {
    for (i in liste_date) {
      if (input$date_caract %in% i) {
        str = paste0("select distinct sit_nom_court from public.tr_site_capture_sit, public.t_capture_cap where sit_id=cap_sit_id and cap_date = '", input$date_caract,"'")
        resres = dbGetQuery(con,str)
        same_date <- resres[1,1]
        updateSelectizeInput(session, "idSite2", selected = same_date)
      }
    }
  })
  
  observeEvent(input$nAnimal, {
    for (i in liste_date) {
      if (input$date_caract %in% i) {
        str = paste0("select distinct sit_nom_court from public.tr_site_capture_sit, public.t_capture_cap where sit_id=cap_sit_id and cap_date = '", input$date_caract,"'")
        resres = dbGetQuery(con,str)
        same_date <- resres[1,1]
        updateSelectizeInput(session, "idSite", selected = same_date)
      }
    }
  })
  
  liste_site <- dbGetQuery(con, "select distinct sit_nom_court from public.tr_site_capture_sit")
  
  observeEvent(input$idSite, {
    for (i in liste_site) {
      if (!(input$idSite %in% i)) {
        if (input$idSite != "")
        {shinyalert("NOUVEAU SITE?", "Est-ce un nouveau site ?", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_new_site)} 
      }}
  })
  
  modalCallback_new_site <- function(value) {
    if (value == TRUE) {
      (dbSendQuery(con,sprintf("INSERT INTO public.tr_site_capture_sit (sit_nom_court) VALUES ('%s')", input$idSite))) }}
  
  observeEvent(input$idSite2, {
    for (i in liste_site) {
      if (!(input$idSite2 %in% i)) {
        if (input$idSite2 != "")
        {shinyalert("NOUVEAU SITE?", "Est-ce un nouveau site ?", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_new_site2)} 
      }}
  })
  
  modalCallback_new_site2 <- function(value) {
    if (value == TRUE) {
      (dbSendQuery(con,sprintf("INSERT INTO public.tr_site_capture_sit (sit_nom_court) VALUES ('%s')", input$idSite2))) }}
  
  #########          Alerte perte de poids                                          #########
  
  output$perte_poids <- renderUI({
    if ((input$nAnimal2) != "") {
      str = paste0("select cap_poids from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      verif_poids <- resres[1,1]
      if (!is.na(input$pSabotPlein) && !is.na(input$pSabotVide)) {
        #test_poids = verif_poids - (input$pSabotPlein - input$pSabotVide)
        if ((verif_poids - (input$pSabotPlein - input$pSabotVide)) > 1) 
        {shinyalert("PERDU PLUS D'UN KILO!", "L'animal a perdu du poids par rapport à la capture précédente", type = "warning", showCancelButton=F, showConfirmButton = T)}
      }}
  })

  #########          Panneau conditionnel                                           #########
  
  output$conditionalInput1 <- renderUI({
    if(input$newTagG){
      textInput("idTagOrG3", h4("New Tag Gauche"),value="")}
    else if (input$newTagG == F) {selectizeInput("idTagOrG2", h4("Tag Oreille Gauche"), choices = dbGetQuery(con,"select distinct cap_tag_gauche from public.t_capture_cap"),options=list(placeholder='Choisir une valeur :',create=TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
  })
  
  output$conditionalInput2 <- renderUI({
    if(input$newTagD){
      textInput("idTagOrD3", h4("New Tag Droit"),value="")}
    else if (input$newTagD == F) {selectizeInput("idTagOrD2", h4("Tag Oreille Droite"), choices = dbGetQuery(con,"select distinct cap_tag_droit from public.t_capture_cap"),options=list(placeholder='Choisir une valeur :',create=TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
  })
  
  output$conditionalInput3 <- renderUI({
    if(input$newRFIDbox){
      selectizeInput("idRFID_new", h4("RFID_new"), choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
    else if (input$newRFIDbox == F) {selectizeInput("idRFID2", h4("RFID"), choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id"), 
                         options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
    })
  
  output$conditionalInput4 <- renderUI({
    if(input$newTagG){
      checkboxInput("metal_tag_g3", "New Tag G. métal", value = FALSE ) }
    else if (input$newTagG == F) {checkboxInput("metal_tag_g2", "Tag G. métal", value = FALSE )}
  })
  
  output$conditionalInput5 <- renderUI({
    if(input$newTagD){
      checkboxInput("metal_tag_d3", "New Tag D. métal", value = FALSE )}
    else if (input$newTagD == F) {checkboxInput("metal_tag_d2", "Tag D. métal", value = FALSE )}
  })
  
  # output$conditionalInput6 <- renderUI({
  #   if(input$newRFIDbox == FALSE){
  # })
  

  #########          Lecture RFID                                                   #########
  
  observeEvent(input$rfid_read, {
    
    source(file = "read_RFID.R")
    # updateDateInput(session, "date_caract", value = date)
    # updateTextInput(session, "time_caract", value = time)
    updateSelectizeInput(session, "idRFID2", selected = rfid)
    updateTextInput(session, "idRFID", value = rfid)
  })
  
  #########          effacer mémoire RFID prend 40 secondes                         ##########
 
  observeEvent(input$rfid_clear, {
    source(file = "clear_RFID.R")
    updateTextInput(session, "rfid_erase", value = resultat)
  })
  
  ##################           RUBRIQUE BLESSURES                       #################
  
  blessure = data.frame()
  row.names(blessure) = NULL
  
  output$tableblessure = DT::renderDT(expr = blessure,server = F)
  
  sup_Ligne = observeEvent(input$sup_Bles, {
    if (!is.null(input$tableblessure_rows_selected)) {
      blessure <<- blessure[-as.numeric(input$tableblessure_rows_selected),]
      output$tableblessure = DT::renderDT(blessure,server = F)
    }
  })
  
  observeEvent(input$ajoutBle, {
    if ((length(input$traitement))>1)
    {
      list_ble = ""
      for (u in input$traitement) {
        list_ble = paste(u,list_ble,sep="_")
      }
      if (input$remarques_ble=="")
      {blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(list_ble), "Liste" = paste(c(input$locali),c(input$grave), c(list_ble), sep = "-")))}
      else { blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(list_ble), "Liste" = paste(c(input$locali),c(input$grave), c(list_ble),input$remarques_ble, sep = "-")))}
      updateSelectizeInput(session,"locali", options=list(selected=NULL))
      updateSelectizeInput(session,"traitement", options=list(selected=NULL))
    }
    
    if ((length(input$traitement))==1)
    {
      if (input$remarques_ble=="")
      {blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(input$traitement), "Liste" = paste(c(input$locali),c(input$grave),c(input$traitement), sep = "-")))}
      else { blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(input$traitement), "Liste" = paste(c(input$locali),c(input$grave), c(input$traitement), input$remarques_ble, sep = "-")))}
    }
    
    output$tableblessure = DT::renderDT(blessure,server = F)
    #print(blessure[1][1])
    #print(blessure$Liste)
    updateSelectizeInput(session,"grave", options=list(selected=NULL))
    updateSelectizeInput(session,"locali", options=list(selected=NULL))
    updateSelectizeInput(session,"traitement", options=list(selected=NULL))
    updateTextInput(session, "remarques_ble", value = "", placeholder = "Remarque")
  })
  
  observeEvent(input$ajoutBle, {
    i=1
    liste_blessures =""
    while (i <= nrow(blessure)) {
      liste_blessures <- paste0(liste_blessures, blessure[i,]$Liste, "~")
      i=i+1
      updateTextInput(session, "liste_blessures", value = liste_blessures)
      updateSelectizeInput(session,"solsol", options=list(selected=NULL))

      
    }
    
  })
  
  ### Mise en forme des blessures en cascade :
  
  output$casc_ble1 <- renderUI({
    selectizeInput("locali", h4("Localisation"), choices = df_blessure$ble_local,options=list(placeholder='Choisir une valeur :',create= TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)
  })
  
  output$casc_ble2 <- renderUI({
    x <- input$locali
    if (any(
      is.null(x)
    ))
      return("Select")
    choice2 <- df_blessure[df_blessure$ble_local == x,  "ble_gravite"]
    selectizeInput("grave", h4("gravité"), choices = choice2, options=list(create= TRUE))
  })
  
  updateSelectizeInput(session,"traitement", choices = dbGetQuery(con,"select blt_traitement from lu_tables.tr_blessure_traitement_blt "))
  
  # observeEvent(input$traitement, {
  #   #print(input$traitement[2])
  # if (!is.na(input$traitement[2])) {
  #   updateSelectizeInput(session,"traitement", choices = (dbGetQuery(con,"select blt_traitement from lu_tables.tr_blessure_traitement_blt")))
  #   }
  # })
  
  
  #########          Ajout d'un nouveau traitement                                  ########
  
  liste_traitement = dbGetQuery(con,"select blt_traitement from lu_tables.tr_blessure_traitement_blt")
  
  observeEvent(input$traitement, {
    for (i in liste_traitement) {
      if (!(input$traitement %in% i)) {
        if (input$traitement != "")
        {shinyalert("WAIT!", "Est-ce un nouveau type de traitement ?", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_traitement)} 
      }}
  })
  
  modalCallback_traitement <- function(value) {
    if (value == TRUE) {
      (dbSendQuery(con,sprintf("INSERT INTO lu_tables.tr_blessure_traitement_blt (blt_traitement) VALUES ('%s')", input$traitement))) }}
  
  ##################           RUBRIQUE PRELEVEMENTS                    #################
  
  prelevement = data.frame()
  row.names(prelevement) = NULL
  
  output$tableprelevement = DT::renderDT(expr = prelevement,server = F)
  
  sup_Ligne_prelev = observeEvent(input$sup_prelev, {
    if (!is.null(input$tableprelevement_rows_selected)) {
      prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
      output$tableprelevement = DT::renderDT(prelevement,server = F)
    }
  })
  
  observeEvent(input$ajout_prelev, {
    
    prelevement <<- rbind(prelevement, data.frame("Type" = c(input$typetype), "Localisation" =c(input$localoca), "Contenant" = c(input$condi),"Solvant" = c(input$solsol),"Nombre d'echantillons" = c(input$nbre_echant),  "Remarques" = c(input$remarques_prel)))
    output$tableprelevement = DT::renderDT(prelevement,server = F)
    updateSelectizeInput(session,"typetype", options=list(selected=NULL))
    updateSelectizeInput(session,"solsol", options=list(selected=NULL))
    updateTextInput(session, "remarques_prel", value = "", placeholder = "Remarque")
    updateSelectizeInput(session,"nbre_echant", choices =list( 1,2,3,4,5) ,options=list(create=T), selected = as.character(1))
    
 })
  
  ### Mise en forme des prélevements en cascade :
  
  output$table_prel <- renderTable({df_prelevement})
  
  output$control1 <- renderUI({
    selectizeInput("typetype", h4("Type"), choices = df_prelevement$prel_type, options=list(placeholder='Choisir une valeur :',create= TRUE, onInitialize = I('function() { this.setValue(""); }')))
  })
  
  output$control2 <- renderUI({
    x <- input$typetype
    if (any(
      is.null(x)
    ))
      return("Select")
    choice2 <- df_prelevement[df_prelevement$prel_type == x,  "prel_local"]
    selectizeInput("localoca", h4("Localisation"), choices = (choice2), options=list(create= TRUE), selected=1)
  })
  
  observeEvent(input$typetype, {
    if (input$typetype == "sang") {
      updateSelectizeInput(session,"localoca", selected="jugulaire")}
    if (input$typetype == "feces") {
      updateSelectizeInput(session,"localoca", selected="anus")}
    if (input$typetype == "poils") {
      updateSelectizeInput(session,"localoca", selected="coup")}
    if (input$typetype == "mucus") {
      updateSelectizeInput(session,"localoca", selected="vagin")}
    if (input$typetype == "peau") {
      updateSelectizeInput(session,"localoca", selected="oreille")}
    if (input$typetype == "tiques") {
      updateSelectizeInput(session,"localoca", selected="oreille")}
  })
  
  output$control3 <- renderUI({
    x <- input$typetype
    y <- input$localoca
    if (any(
      is.null(x),
      is.null(y)
    ))
      return("Select")
    
    choice3 <- df_prelevement[df_prelevement$prel_type == x & df_prelevement$prel_local == y, "prel_condi"]
    selectizeInput("condi", h4("Conditionnement"), choices = choice3, options=list(create= TRUE))
  })
  
  output$control4 <- renderUI({
    x <- input$typetype
    y <- input$localoca
    z <- input$condi
    if (any(
      is.null(x),
      is.null(y),
      is.null(z)
    ))
      return("Select")
    
    choice4 <- df_prelevement[df_prelevement$prel_type == x & df_prelevement$prel_local == y & df_prelevement$prel_condi == z, "prel_solv"]
    selectizeInput("solsol", h4("Solvant"), choices = choice4, list(create= TRUE))
  })
  
  observeEvent(input$ajout_prelev, {
    cat_prelevement1 <- paste0( c(input$typetype), "_" , c(input$localoca), "_", c(input$condi), "_", c(input$solsol))
    cat_prelevement <- paste0( c(input$typetype), "_" , c(input$localoca), "_", c(input$condi), "_", c(input$solsol), "_",c(input$nbre_echant),"_",c(input$remarques_prel))
    liste_prelevement[nrow(prelevement)] <<- cat_prelevement
    liste_prelevement2[nrow(prelevement)] <<- cat_prelevement1
    updateTextInput(session, "remarques_prel", value = NULL)
    
  })
  
  liste_prelevement=list()
  liste_prelevement2=list()
  liste_prel_db = dbGetQuery(con,"select sav_intitule from lu_tables.tr_samples_verification_sav")
  
  ##################           RUBRIQUE COLLIER                         #################

  query <- reactive({
    date_mod = input$date_caract
    date_mod = format(date_mod, "%d/%m/%Y")
    date_mod = as.character(date_mod)
    
    mois = strsplit(date_mod, "/")[[1]][2]
    annee = strsplit(date_mod, "/")[[1]][3]
    if (as.integer(mois)>=10) {annee_suivie <- as.integer(annee) + 1}
    if (as.integer(mois)<10) {annee_suivie <- annee}
  
liste_collier <- dbGetQuery(con,paste0("select eqc_annee_suivi, teq_nom_court, eqc_remarque, eqt_id_usuel,eqc_drop_off,sen_association, eqc_couleur_boitier, eqc_couleur_collier,eqt_frequence, eqc_memoire FROM public.t_equipement_eqt, public.t_equipement_conf_eqc, public.tr_type_equipement_teq,lu_tables.tr_sensors_sen  where eqc_eqt_id = eqt_id
                              and teq_id = eqt_teq_id and eqc_sen_id=sen_id and eqc_annee_suivi = '",annee_suivie,"' order by teq_nom_court")) 

return(liste_collier)})

  output$tablecollier = DT::renderDataTable(expr = query() , selection = 'single')
  
  affichage_choix_collier <- observeEvent(input$tablecollier_rows_selected, {
    if (!is.null(input$tablecollier_rows_selected)) {
      ligne_selection = input$tablecollier_rows_selected
      collier_tech = query()[ligne_selection,"teq_nom_court"]
      collier_col_c = query()[ligne_selection,"eqc_couleur_collier"]
      collier_col_b = query()[ligne_selection,"eqc_couleur_boitier"]
      cat_col = paste(toupper(collier_tech),": collier ", toupper(collier_col_c)," boitier ", toupper(collier_col_b) )
      output$collier_choisi = renderText(cat_col)
    }
  })
  
  observeEvent(input$sup_col, {
  proxy <- dataTableProxy("tablecollier",session, deferUntilFlush = FALSE)
  reloadData(proxy, resetPaging = TRUE, clearSelection = c("all"))
  ####remise a jour de la ligne texte de selection du collier
  output$collier_choisi = renderText("")
  })
  
  ##################           RUBRIQUE TABLE                           #################
  
  updateSelectizeInput(session, "Notation_euro_table", choices = choix[["Notation_euro_table"]])
  updateSelectizeInput(session, "position_temp1", choices = choix[["position_temp1"]], 
                       options=list(create= TRUE), selected = 'anus')
  updateSelectizeInput(session, "position_temp2", choices = choix[["position_temp2"]], 
                       options=list(create= TRUE), selected = 'exterieur')
  
  observeEvent(input$identifie, {
    if (input$identifie == "oui") {
      updateAwesomeRadio(session,"cribague", selected = "NA")}  
  })
  
  observeEvent(input$to_current_time_table, {
    
    gettime_table_posix <<- Sys.time()
    gettime_table = as.character(Sys.time())
    gettime_table = strsplit(gettime_table, " ")[[1]]
    gettime_table = gettime_table[2]
    updateTextInput(session, "time_table", value = gettime_table)
  })
  
  observeEvent(input$criautre, {
    if (!is.null(input$criautre) && !is.null(input$cribague)) {
      if (((input$cribague == "NA" || input$cribague == "0")) && (input$criautre == "0")) {
        cri_synthese = FALSE }
      else {cri_synthese = TRUE }
    } })
  
  observeEvent(input$cribague, {
    if (!is.null(input$criautre) && !is.null(input$cribague)) {
      if (((input$cribague == "NA" || input$cribague == "0")) && (input$criautre == "0")) {
        cri_synthese = FALSE }
      else {cri_synthese = TRUE }
    } })
  
  
  temperature = data.frame()
  row.names(temperature) = NULL
  output$tabletemperature = DT::renderDT(expr = temperature,server = F)

  rv <- reactiveValues(i = 0)
  maxIter <- 1800
  plot_temp <<- data.frame()
  
  output$plot <- renderPlot( {
    if(rv$i > 0) {
      if (input$suivi_temp == T) {
      tempr <- t(read.delim("/sys/devices/w1_bus_master1/28-0417503f2cff/w1_slave"))[,1]
      tempr <- as.numeric(substr(tempr,as.numeric(regexpr("t=",tempr)[1])+2,as.numeric(nchar(tempr))))/1000
      tempb <- t(read.delim("/sys/devices/w1_bus_master1/28-031724cb00ff/w1_slave"))[,1]
      tempb <- as.numeric(substr(tempb,as.numeric(regexpr("t=",tempb)[1])+2,as.numeric(nchar(tempb))))/1000
      table_temp <<- data.frame(rv$i, tempr, tempb)
      plot_temp <<- rbind(plot_temp, table_temp)
      par(mar = c(5,5,2,5))
      if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "anus")) {
        plot(x = plot_temp$rv.i, y = plot_temp$tempr,xlab = "Temps (sec)", ylab="Sonde rouge (°C)",  type = "b", xlim=c(rv$i-20,rv$i), ylim=c(20,45), col="red", pch = 2 ) 
        temperature <<- rbind(temperature,data.frame("Date" = c(Sys.time()), "Temperature_r" =c(tempr), "Temperature_b" =c(tempb)))}

      else if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "exterieur")) {
        plot(x = plot_temp$rv.i, y = plot_temp$tempr,xlab = "Temps (sec)", ylab="Sonde rouge (°C)",  type = "b", xlim=c(rv$i-20,rv$i), ylim=c(0,25), col="red", pch = 2 ) 
        temperature <<- rbind(temperature,data.frame("Date" = c(Sys.time()), "Temperature_r" =c(tempr), "Temperature_b" =c(tempb)))}
      
      par(new = T)
      if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "anus")) {
        with(plot_temp, plot(x = plot_temp$rv.i, y = plot_temp$tempb, col="blue", type = "b", pch = 1,xlim=c(rv$i-20,rv$i), ylim=c(20,45), axes = F, xlab=NA, ylab=NA )) }
      else if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "exterieur")) {
        with(plot_temp, plot(x = plot_temp$rv.i, y = plot_temp$tempb, col="blue", type = "b", pch = 1,xlim=c(rv$i-20,rv$i), ylim=c(0,25), axes = F, xlab=NA, ylab=NA )) }

      axis(side = 4)
      mtext(side = 4, line = 3, 'Sonde blanche (°C)')
      legend(x = "left", y = "left", legend = c("Sonde rouge", "Sonde Blanche"), col = c("red","blue"), pch = c(2,1), lty = c(1,1))
      } }
  })
  
  observeEvent(input$suivi_temp, {
    doublons <- which(duplicated(temperature)) 
    temperature <- temperature[-doublons,] 
    
    if (input$suivi_temp == T) {
      
      observe({
        isolate({
          rv$i = nrow(plot_temp) + 1
        })
   
        if (isolate(rv$i) < maxIter){
          print(rv$i)
          invalidateLater(1000, session)
        }
      })
    } 
})
  
  ##################           RUBRIQUE HISTORIQUE                      #################
  
  output$historique <- DT::renderDataTable({
  
    outp <- dbGetQuery(con,paste0("select t.ani_etiq as ani, t.ani_sexe as s, t.cap_date as date, t.cap_poids as poids, t.cap_lpa as lpa, t.cap_age_classe as age, t.sit_nom_court as site,
                                  t.teq_nom_court as teq, t.eqa_date_debut as debut, t.eqa_date_fin as fin, t.cap_annee_suivi as an, round(t.temps_suivi/30.43) as mois, count(t.cpos_id) as locs, t.eqt_id_usuel as equip, t.mar_libelle as marque, t.mod_libelle as modele, t.sen_association as                        capteurs
                                  from (SELECT cpos_id, ani_etiq, ani_sexe, cap_date, cap_poids, cap_lpa, cap_age_classe, sit_nom_court,
                                  teq_nom_court, cap_annee_suivi, eqa_date_debut, eqa_date_fin, eqa_date_fin - eqa_date_debut as temps_suivi, eqt_id_usuel, mar_libelle, mod_libelle, sen_association
                                  FROM historique.t_aniposi_gpsgsm) as t where ani_etiq = '",input$nAnimal2,"'
                                  group by ani_etiq, cap_annee_suivi, cap_date, ani_sexe, cap_age_classe,
                                  cap_poids, cap_lpa, sit_nom_court, teq_nom_court, eqt_id_usuel,
                                  sen_association, mar_libelle, mod_libelle, eqa_date_debut, t.temps_suivi,
                                  eqa_date_fin order by cap_annee_suivi"))
    
    
    ret <- DT::datatable(outp)
    return(ret)
  })
  
  ##################           RUBRIQUE CHECKLIST 1                     #################
  #########           Animal                                           #########
  
  checklist1 = data.frame()
  row.names(checklist1) = NULL
  output$tablechecklist1 = DT::renderDT(expr = checklist1,server = F)
  
  output$checklist_1 <- renderUI( { 
    
    checklist1 = data.frame()
    #output$tablechecklist1 = DT::renderDT(expr = NULL,server = F)
    
    if ((input$numSabot)=="") {
      checklist1 = data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de Sabot"))}
    
    if ((input$nAnimal =="") & input$estNouvelAnimal == 'oui') {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$nAnimal2=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non')  & (input$nAnimal=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$idSite2 =="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$estNouvelAnimal == 'non')  & (input$identifie == 'non') & (input$idSite =="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idSite=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$identifie == 'non') & (input$idRFID)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("RFID")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$idRFID2=="") & (input$newRFIDbox == F)) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("RFID")))}
    
    if ((input$estNouvelAnimal == 'non') && (input$identifie == 'oui') && (input$newRFIDbox == T) && (input$idRFID_new == "")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau RFID")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idTagOrG)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idTagOrD)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idTagOrD)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idTagOrG)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$newTagD == F) & (input$idTagOrD2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$newTagG == F) & (input$idTagOrG2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'non') && (input$identifie == 'oui') && (input$newTagD == T) && (input$idTagOrD3=="")) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') && (input$identifie == 'oui') && (input$newTagG == T) && (input$idTagOrG3)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau Tag Gauche")))}
    
    if (is.na(input$lPattArriere)) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Longueur patte")))}
    
    if (is.null(input$sexe)) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Sexe")))}
    
    if (is.na(input$lBoisGauche) & !is.null(input$sexe)) {
      if (input$sexe=='M') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Longueur bois G")))}}
    
    if (is.na(input$lBoisDroit) & !is.null(input$sexe)) {
      if (input$sexe=='M') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Longueur bois D")))}}
    
    if (((input$etatBois)=="") & !is.null(input$sexe)){
      if (input$sexe=='M') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Etat bois")))}}
    
    if (is.na(input$tglucose)) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Glucose")))}
    
    if (is.na(input$cirCou)) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Cou")))}
    
    if (input$diarrhee =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Diarrhee")))}
    
    if (input$tiques =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nombre de tiques")))}
    
    if ((input$lactation=="") & !is.null(input$sexe)){
      if (input$sexe=='F') {
        checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Lactation")))}}
    
    if (input$age =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Age")))}
    
    if (is.na(input$pSabotVide) && is.na(input$pSabotPlein) ) {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Poids de l'animal")))}
    
    if (input$time_caract =="") {
      checklist1 =  rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Heure debut")))}
    
    if (nrow(checklist1)==0) {
      checklist1 =  rbind(checklist1,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist1 = DT::renderDT(checklist1,server = F) 
    
    ### Table
    
    checklist_table = data.frame()
    
    if ((input$sonde_temp1)=="") {
      checklist_table = data.frame("VALEUR_MANQUANTE_TABLE"= c("Sonde temperature 1"))}
    
    if ((input$sonde_temp2)=="") {
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Sonde temperature 2")))}
    
    if ((input$position_temp1)==""){
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Position sonde 1")))}
    
    if ((input$position_temp2)==""){
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Position sonde 2")))}
    
    if (is.null(input$lutte)) {
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Lutte")))}
    
    if (is.null(input$halete)) {
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Halete")))}
    
    if (is.null(input$cribague)) {
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Cri bague")))}
    
    if (is.null(input$criautre)) {
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Cri autre")))}
    
    if ((input$Notation_euro_table)=="") {
      checklist_table = rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Eurodeer")))}
    
    if (input$time_table =="") {
      checklist_table =  rbind(checklist_table,data.frame("VALEUR_MANQUANTE_TABLE"= c("Heure fin")))}
    
    if (nrow(checklist_table)==0) {
      checklist_table =  rbind(checklist_table,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist_table = DT::renderDT(checklist_table,server = F) 
    
    
    ### Prelevement
    
    checklist_prel = data.frame()
    
    observeEvent(input$typetype, {

    for (i in (1:nrow(liste_prel_db))){
      temp = liste_prel_db[i,1]
      if (!(temp %in% liste_prelevement2)) {
        checklist_prel <<- rbind(checklist_prel,data.frame("PRELEVEMENT_MANQUANT"= c(temp)))}}

    if (nrow(checklist_prel)==0) {
      checklist_prel <<-  rbind(checklist_prel,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))} 

    output$tablechecklist_prel = DT::renderDT(checklist_prel) })
    
    ### Collier
    
    checklist_collier = data.frame()

    if (is.null(input$tablecollier_rows_selected)) {
      checklist_collier = data.frame("COLLIER_MANQUANT"= c("Pas de collier choisi"))}

    if (!is.null(input$tablecollier_rows_selected)) {
      checklist_collier = data.frame("PARFAIT"= c("Collier bien sélectionné"))}

    output$tablechecklist_collier = DT::renderDT(checklist_collier,server = F)
    
    
    ### Bilan
    
    observeEvent(input$valid_checklist1, ignoreInit = T, {
      
      if ( ((checklist_prel[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist_table[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist1[1,1])!="PAS DE DONNEES MANQUANTES") || ((checklist_collier[1,1])!="Collier bien sélectionné"))
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE,  callbackR = modalCallback_check1 )}
      else
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check1 )}

    })
  
    # ## provisoire quand pas de serveur :  
    # observeEvent(input$valid_checklist1, ignoreInit = T, {
    #   if ( ((checklist_table[1][1])!="PAS DE DONNEES MANQUANTES") || ((checklist1[1][1])!="PAS DE DONNEES MANQUANTES"))  
    #   {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE,  callbackR = modalCallback_check1 )}
    #   else      
    #   {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check1 )}
    #   
    # })
  })
  
  #########           Table                                            ########                
  
  checklist_table = data.frame()
  row.names(checklist_table) = NULL
  output$tablechecklist_table = DT::renderDT(expr = checklist_table,server = F)
  
  #########           Prelevement                                      ########
  
  checklist_prel = data.frame()
  row.names(checklist_prel) = NULL
  output$tablechecklist_prel = DT::renderDT(expr = checklist_prel,server = F)
  
  #########           Collier                                          ########
  
  checklist_collier = data.frame()
  row.names(checklist_collier) = NULL
  output$tablechecklist_collier = DT::renderDT(expr = checklist_collier,server = F)
  
  ##################           RUBRIQUE LACHER                          #################
  
  updateSelectizeInput(session, "habitat", choices = choix[["habitat"]])
  updateSelectizeInput(session, "habitat_perte", choices = choix[["habitat_perte"]])
  updateSelectizeInput(session, "Notation_euro", choices = choix[["Notation_euro"]])
  
  
  observeEvent(input$to_current_time, {
    gettime_time_posix <<- Sys.time()
    gettime_time <- as.character(Sys.time())
    gettime_time <- strsplit(gettime_time, " ")[[1]]
    gettime_time <- gettime_time[2]
    updateTextInput(session, "time", value = gettime_time)
  })
  
  
  observeEvent(input$to_current_time2, {
    gettime_time2_posix <<- Sys.time()
    gettime_time2= as.character(Sys.time())
    gettime_time2=strsplit(gettime_time2, " ")[[1]]
    gettime_time2<- gettime_time2[2]
    updateTextInput(session, "time2", value = gettime_time2)
  })
  
  observeEvent(input$save_checklist2, { 
    # cat(file=stderr(), "visi", input$titube, "\n")
    
    tmp_time= as.character(input$time)
    tmp_time=strsplit(tmp_time, " ")[[1]]
    tmp_time=tmp_time[2]
}
  )
  
  ##################           RUBRIQUE CHECKLIST 2                     #################
  checklist2 = data.frame()
  row.names(checklist2) = NULL
  output$tablechecklist2 = DT::renderDT(expr = checklist2,server = F)
  
  
  output$checklist_2 <- renderUI( {
    #cat(file=stderr(), "testttt2t", input$titube, "\n")
    
    checklist2 = data.frame()
    
    if (is.null(input$vitesse))  {
      checklist2 = data.frame("DONNNES_LACHER_MANQUANTES" = c("Vitesse"))}
    
    if (is.null(input$titube)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Titube")))}
    
    if (is.null(input$couche)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Couche")))}

    if (is.null(input$cabriole_saut)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Cabriole")))}

    if (is.null(input$cri)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Cri")))}

    if (is.null(input$allure)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Allure")))}

    if (is.null(input$gratte_collier)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Gratte-collier")))}

    if (is.null(input$tombe)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Tombe")))}

    if ((input$habitat)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Habitat")))}

    if ((input$Notation_euro)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Eurodeer")))}

    if ((input$habitat_perte)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Habitat perte")))}

    if (is.na(input$nbre_stops)) {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Nombre de stops")))}

    if ((input$visibilite)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Visibilité")))}

    if ((input$nbre_personnes)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Nombre de personnes")))}
    
    if ((input$time)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Heure de lacher")))}
    
    # if ((input$time2)=="") {
    #   checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Heure de 2nd lacher")))}
    
    if (nrow(checklist2)==0) {
      checklist2 =  data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES"))}

    
    output$tablechecklist2 = DT::renderDT(checklist2,server = F) 
 
    ### Bilan
    
    observeEvent(input$valid_checklist2,ignoreInit = T, {
      if  ((checklist2[1,1])!="PAS DE DONNEES MANQUANTES")
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE, callbackR  = modalCallback_check2)}
      else      
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check2)}
    })
  })
  
  ##################           RUBRIQUE CAPTURE                         #################
  
#updateSelectizeInput(session, "numSabot_capture", choices = choix[["numSabot_capture"]])
  
####affichage des dates disponibles
observe({
     if ((input$date_capture)=="") {
     fi<-grep(".csv",list.files(), value =TRUE)
     fi<-sub("captures_","",fi)
     fi<-sub(".csv","",fi)
     fi<<-gsub("_","-",fi)
     updateSelectizeInput(session, "date_capture", choices = fi)
     }
     })

####affichage des données de l'individu
observe({
             if ((input$date_capture)!="") { 
                  fichier_lu <- read.table(file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))
                  colnames(fichier_lu)<- noms_colonnes
                  updateSelectizeInput(session, "numSabot_capture", choices = unique(fichier_lu[,c("num_sabot")]))
               if(input$numSabot_capture!="") {
                 select_line <<- which(fichier_lu[,c("num_sabot")]==c(input$numSabot_capture),arr.ind=TRUE)[1]
                 ani <- fichier_lu[select_line, c("N°Animal")]
                 ani2<- ani
                 sexe<- fichier_lu[select_line, c("Sexe")]
                 print(sexe)
                 sabot<- fichier_lu[select_line, c("num_sabot")]
                 age<- fichier_lu[select_line, c("Age cahier")]
                 tagd<- fichier_lu[select_line, c("cap_tag_droit")]
                 tagg<- fichier_lu[select_line, c("cap_tag_gauche")]
                 poids<-fichier_lu[select_line, c("Poids")]
                 sante<-fichier_lu[select_line, c("etat_sante")]
                 site<-fichier_lu[select_line, c("Site Capture")]
                 anim<-dbGetQuery(con, "Select ani_etiq from t_animal_ani")
                 if (length(grep(ani2,as.character(anim[,1]))) != 0){
                   updateRadioButtons(session,"estNouvelAnimal", selected = "non")
                   updateSelectizeInput(session, "idSite2",  selected = site)
                   updateSelectizeInput(session, "nAnimal2",  selected = ani2)
                 }else {
                 updateRadioButtons(session,"estNouvelAnimal", selected = "oui")
                 updateSelectizeInput(session, "idSite",  selected = site)
                 updateTextInput(session, "nAnimal",  value = ani)}
                 updateSelectizeInput(session, "numSabot_capture", choices = unique(fichier_lu[,c("num_sabot")]),  selected = sabot)
                 updateSelectizeInput(session, "age",  selected = age)
                 updateTextInput(session, "idTagOrG", value =  tagd)
                 updateTextInput(session, "idTagOrD", value =  tagg)
                 output$poids_ani = renderText({poids})
                 updateTextInput(session,"remarque_ani", value = sante)
                 updateAwesomeRadio(session, "sexe", choices =choix[["sexe"]], selected = sexe)
                }}
               })

  ##################           RUBRIQUE SABOT                           #################
  
  updateSelectizeInput(session, "cpt_dose_acepromazine", choices = choix[["cpt_dose_acepromazine"]])

#####calcul de l'heure en sabot 
observe({
  if (!is.na(strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"))) { if (!is.na(input$cpt_temps_filet)) {
   heure_sab<- strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S") + input$cpt_temps_filet*60
   updateTimeInput(session, "cpt_heure_mise_sabot", value = heure_sab)
  }}
}) 
  
  ##################           RUBRIQUE CHECKLIST 3                     #################
  
  checklist_capture = data.frame()
  row.names(checklist_capture) = NULL
  output$tablechecklist_capture = DT::renderDT(expr = checklist_capture,server = F)
  
  checklist_sabot = data.frame()
  row.names(checklist_sabot) = NULL
  output$tablechecklist_sabot = DT::renderDT(expr = checklist_sabot,server = F)
  
  output$checklist_capture <- renderUI( {
    
    if (length(unlist(strsplit(as.character(input$cpt_heure_debut_filet), " "))) == 2){
    gettime=as.character(input$cpt_heure_debut_filet)
    gettime=strsplit(gettime, " ")[[1]]
    gettime<<-sub(":00$","", gettime[2])} else {gettime <- "00:00"}
    if (length(strsplit(as.character(input$cpt_heure_mise_sabot), " ")[[1]]) == 2){
    gettime3=as.character(input$cpt_heure_mise_sabot)
    gettime3=strsplit(gettime3, " ")[[1]]
    gettime3<<-sub(":00$","", gettime3[2])}else{gettime3 <- "00:00"}
    if (length(strsplit(as.character(input$cpt_heure_fin_surv), " ")[[1]]) == 2){
    gettime4=as.character(input$cpt_heure_fin_surv)
    gettime4=strsplit(gettime4, " ")[[1]]
    gettime4<<-sub(":00$","", gettime4[2])} else {gettime4 <- "00:00"}

    checklist_capture = data.frame()
    
    # if ((input$numSabot_capture)=="")  {
    #   checklist_capture = data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Numéro de sabot"))}
    
    if ((input$date_capture)=="")  {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Date")))}
    
    if ((gettime)== "00:00")  {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Heure début filet")))}
    
    if (is.na(input$cpt_temps_filet))  {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Temps passé au filet")))}
    
    if ((input$nom_capteur_txt)=="") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Capteurs")))}

    if ((input$Nbre_pers_experimentes)=="") {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Nombre de personnes expérimentées")))}

    if (is.null(input$cpt_filet_vitesse)) {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Vitesse filet")))}

    if (is.null(input$cpt_filet_allure)) {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Allure filet")))}

    if (is.null(input$cpt_filet_lutte)) {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Lutte filet")))}

    if (is.null(input$cpt_filet_halete)) {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Halete")))}

    if (is.null(input$cpt_filet_cri)) {
      checklist_capture = rbind(checklist_capture,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Cri")))}
    
    if (nrow(checklist_capture)==0) {
      checklist_capture =  rbind(checklist_capture,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist_capture = DT::renderDT(checklist_capture,server = F)
    
    checklist_sabot = data.frame()
    
    if ((gettime3)=="00:00") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Heure mise en sabot")))}

    if ((gettime4)=="00:00") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Heure fin de surveillance")))}

    if ((input$cpt_dose_acepromazine)=="") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Dose azepromazine")))}

    if (is.null(input$cpt_sabot_retournement)) {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Retournement ?")))}

    if (is.null(input$cpt_sabot_couche)) {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Couché ?")))}

    if (is.null(input$cpt_sabot_agitation)) {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Agité ?")))}

    if ((input$Observateur)=="") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Observateur")))}
    
    if (nrow(checklist_sabot)==0) {
      checklist_sabot =  rbind(checklist_sabot,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist_sabot = DT::renderDT(checklist_sabot,server = F)
 
    
    ### Bilan
    
    observeEvent(input$valid_checklist3, ignoreInit = T, {
      if  ((checklist_capture[1,1]!="PAS DE DONNEES MANQUANTES") || (checklist_sabot[1,1]!="PAS DE DONNEES MANQUANTES")) 
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE, callbackR = modalCallback_check3)}
      else      
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check3)}
      
    })
    
  })
  
  
  ##################           CSV CHECKLIST 1                          #####     
  
      modalCallback_check1 = function(value) {
        if (value == TRUE) {
          
          save1 = data.frame()
          
          if (startsWith(input$nAnimal, "F")){
            faon1 =  "oui" }
          else {faon1="non"}
          
          if (startsWith(input$nAnimal2, "F")){
            faon2 =  "oui" }
          else {faon2="non"}
          
          date_mod = input$date_caract
          date_mod = format(date_mod, "%d/%m/%Y")
          date_mod = as.character(date_mod)
          
          jour = strsplit(date_mod, "/")[[1]][1]
          mois = strsplit(date_mod, "/")[[1]][2]
          annee = strsplit(date_mod, "/")[[1]][3]
          
          if (input$age == '0.5' || input$age == '<1' ) {
            cat_age_all = "jeune" 
            cat_age = "j"}
          else if (input$age=='1.5' || input$age=='1') {
            cat_age_all = "yearling"
            cat_age = "y" }
          else if (input$age=='2' || input$age=='2.5' || input$age=='3' || input$age=='3.5' || input$age=='4.5-5.5' || input$age=='4-5' || input$age=='>=6' || input$age=='>6.5') {cat_age_all="adulte"
          cat_age=""}
          else {cat_age_all="" 
          cat_age=""}
          
          if (!is.null(input$criautre) && !is.null(input$cribague)) {
            if (input$criautre!='0' || (input$cribague=='1-2' || input$cribague=='>2'))
              {cri_total = 1}
              else {cri_total = 0}
          }
          else {cri_total=""}
          
          ligne_selection = input$tablecollier_rows_selected
          collier_tech = query()[ligne_selection,"teq_nom_court"]
          collier_col_c = query()[ligne_selection,"eqc_couleur_collier"]
          collier_col_b = query()[ligne_selection,"eqc_couleur_boitier"]
          cat_col = paste(toupper(collier_tech),": collier ", toupper(collier_col_b)," boitier ", toupper(collier_col_c))
          
          if (input$nAnimal2!="") {
          nbre_capt = dbGetQuery(con,paste0("SELECT count(cap_id) FROM public.t_capture_cap, public.t_animal_ani where ani_id = cap_ani_id and ani_etiq= '",input$nAnimal2,"' group by ani_etiq order by ani_etiq"))
          nbre_capt <- nbre_capt[1,1] + 1
          
          cap_pertinent = dbGetQuery(con,paste0("select cap_annee_suivi from public.t_capture_cap, public.t_animal_ani where cap_ani_id=ani_id and ani_etiq = '",input$nAnimal2,"' order by cap_annee_suivi DESC"))
          cap_pertinent <- cap_pertinent[1,1]
          if (annee == cap_pertinent) {cap_pertinent = FALSE} else {cap_pertinent = TRUE} }
          
        for (i in (1:(length(liste_prelevement)))) {
            test1 = strsplit(as.character(liste_prelevement[i][1]),"_")[[1]][1]

            if (test1=="peau") {
              if (exists("peau")) {
                peau = paste(peau, as.character(liste_prelevement[i][1]), sep = "~")}
              else {peau = as.character(liste_prelevement[i][1])}}
  
            if (test1=="poils") {
              if (exists("poils")) {
                poils = paste(poils, as.character(liste_prelevement[i][1]),  sep = "~")}
              else{poils = as.character(liste_prelevement[i][1])}}

            if (test1=="sang") {
              if (exists("sang")) {
               sang = paste(sang, as.character(liste_prelevement[i][1]),  sep = "~")}
              else{sang = as.character(liste_prelevement[i][1])}}

            if (test1=="feces") {
              if (exists("feces")) {
                feces = paste(feces, as.character(liste_prelevement[i][1]),  sep = "~")}
              else{feces = as.character(liste_prelevement[i][1])}}
    
            if (test1=="tiques") {
              if (exists("tiques")) {
               tiques = paste(tiques, as.character(liste_prelevement[i][1]),  sep = "~")}
              else{tiques = as.character(liste_prelevement[i][1])}}
     
            if (test1=="mucus") {
              if (exists("mucus")) {
               mucus = paste(mucus, as.character(liste_prelevement[i][1]),  sep = "~")}
              else{mucus = as.character(liste_prelevement[i][1])}}
          }
        
        for (i in (1:(length(liste_prelevement)))) {
          
          test2 = strsplit(as.character(liste_prelevement[i][1]),"_")[[1]][2]
          
          if (!is.na(test2)) {
          
          if (test2=="vagin") {
            if (exists("vagin")) {
              vagin = paste(vagin, as.character(liste_prelevement[i][1]), sep = "~")}
            else{vagin = as.character(liste_prelevement[i][1])}}
          
          if (test2=="nez") {
            if (exists("nez")) {
              nez = paste(nez, as.character(liste_prelevement[i][1]), sep = "~")}
            else{nez = as.character(liste_prelevement[i][1])}}
          }}
          
          if (exists("collier_tech")) {
            if (!is.null(ligne_selection)) {
            collier_tech_test = collier_tech }
            else{collier_tech_test = ""}}
          else{collier_tech_test = ""}
          
          sen_association <-  dbGetQuery(con,"select sen_association from lu_tables.tr_sensors_sen")
          sen_id_acc <- sen_association[grep("accelerometre",as.character(sen_association[,1])),1]
          sen_id_prox <- sen_association[grep("proximite",as.character(sen_association[,1])),1]
          sen_id_act <- sen_association[grep("activite",as.character(sen_association[,1])),1]
          
          if (!is.null(ligne_selection)) {
            collier_test = query()[ligne_selection,"sen_association"]
          collier_id_usuel = query()[ligne_selection,"eqt_id_usuel"] }
          else { collier_id_usuel = ""}

        if (!is.null(ligne_selection)) {
            if (collier_test != "rien") {
              if (collier_test %in% sen_id_acc) {
               collier_acc <- 1} else {collier_acc <- ""}} else {collier_acc <- ""}} else {collier_acc <- ""}
          
        if (!is.null(ligne_selection)) {
          if (collier_test != "rien") {
            if (collier_test %in% sen_id_prox) {
              collier_prox = 1 } else {collier_prox <- ""}} else {collier_prox <- ""}} else {collier_prox <- ""}
            
        if (!is.null(ligne_selection)) {
          if (collier_test != "rien") {                
            if (collier_test %in% sen_id_act) {
              collier_act = 1}else {collier_act <- ""}} else {collier_act <- ""}} else {collier_act <- ""}
          
          diarrhee = paste("diarrhee/",input$diarrhee, sep="")
          bledia = paste(input$liste_blessures, diarrhee)
          
          if (as.integer(mois)>=10) {
            annee_suivie <- as.integer(annee) + 1  }
          if (as.integer(mois)<10) {annee_suivie <- annee}
          
          if (faon1 != 'oui' ) {
            cap_bague = paste0(input$idTagOrD, "_", str_sub(annee_suivie, -2)) }
          
          if (faon1 == 'oui' ) {
            cap_bague = paste0("F", "_", input$idTagOrD, "_", str_sub(annee_suivie, -2)) }
          
          if (input$newTagD == T  && (faon2 != 'oui' )) {
            cap_bague2 = paste0(input$idTagOrD3, "_", str_sub(annee_suivie, -2)) }
          
          if (input$newTagD == T  && (faon2 == 'oui' )) {
            cap_bague2 = paste0("F", "_", input$idTagOrD3, "_", str_sub(annee_suivie, -2)) }
          
          if ((input$newTagG == T) && (faon2 != 'oui' )) {
            cap_bague2 = paste0(input$idTagOrG3, "_", str_sub(annee_suivie, -2)) }
          
          if ((input$newTagG == T) && (faon2 == 'oui' )) {
            cap_bague2 = paste0("F", "_", input$idTagOrG3, "_", str_sub(annee_suivie, -2)) }
          
          if (input$newTagD == F && input$newTagG == F) {
            cap_bague2 = paste0(input$nAnimal2, "_", str_sub(annee_suivie, -2)) }
          
          #loc_sonde =  as.integer(dbGetQuery(con, "select tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus'")[1,1])
          
          if (exists("gettime_table_posix")) {
            if (exists("gettime_caract_posix")) {
            duree_marquage <- times(as.numeric(difftime(gettime_table_posix, gettime_caract_posix)) / (24*3600))}
            else {duree_marquage=""}} else {duree_marquage=""}
 
          ######cas d''une capture         
 if(((input$estNouvelAnimal == 'oui') || (input$estNouvelAnimal == 'non' && input$identifie == 'non')) && input$nAnimal!="") {
            save1 = data.frame("N°Animal" = c(input$nAnimal))
            save1 = cbind(save1,data.frame("ani_nom" = c("")))
            save1 = cbind(save1,data.frame("N°Animal telemetrie" = c(paste0(tolower(input$sexe),cat_age,"_",input$nAnimal))))
            save1 = cbind(save1,data.frame("N° bague annee capture" = c(cap_bague)))
            save1 = cbind(save1,data.frame("Nombre capture" = c(1)))
            save1 = cbind(save1,data.frame("inconnue" = c("")))
            save1 = cbind(save1,data.frame("Site Capture" = c(input$idSite)))
            save1 = cbind(save1,data.frame("capture faon" = c(faon1)))
            save1 = cbind(save1,data.frame("Date" = c(input$date_caract)))
            save1 = cbind(save1,data.frame("jour" = c(jour)))
            save1 = cbind(save1,data.frame("mois" = c(mois)))
            save1 = cbind(save1,data.frame("annee" = c(annee)))
            save1 = cbind(save1,data.frame("annee  de suivi" = c(annee_suivie)))
            if (!is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = as.character(c(input$sexe)))) }
            if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = c(""))) }            
            save1 = cbind(save1,data.frame("Age cahier" = c(input$age)))
            save1 = cbind(save1,data.frame("Age corrige" = c(input$age)))
            save1 = cbind(save1,data.frame("categorie d'age" = c(cat_age_all)))
            save1 = cbind(save1,data.frame("etat_sante" = c(bledia)))
            save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD)))
            save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG)))
            save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d)))
            save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g)))
            save1 = cbind(save1,data.frame("cap_pertinent" = c(TRUE)))
            if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("cap_lactation" = c("indeterminé"))) }
            if (input$sexe == 'M') {save1 = cbind(save1,data.frame("cap_lactation" = c("")))}
            if (input$sexe == 'F') {save1 = cbind(save1,data.frame("cap_lactation" = c(input$lactation)))}
            save1 = cbind(save1,data.frame("RFID" = c(input$idRFID)))
            save1 = cbind(save1,data.frame("Poids" = c(input$pSabotPlein - input$pSabotVide)))
            save1 = cbind(save1,data.frame("Cir Cou" = c(input$cirCou)))
            save1 = cbind(save1,data.frame("Long patte Ar" = c(input$lPattArriere)))
            save1 = cbind(save1,data.frame("machoire" = c("")))
            save1 = cbind(save1,data.frame("long bois gauche" = c(input$lBoisGauche)))
            save1 = cbind(save1,data.frame("long bois droit" = c(input$lBoisDroit)))
            save1 = cbind(save1,data.frame("glucose" = c(input$tglucose)))
            save1 = cbind(save1,data.frame("T°C_ext" = c("")))
            save1 = cbind(save1,data.frame("TIQUES FIXES" = c(input$tiques)))
            save1 = cbind(save1,data.frame("autres parasites"= c(input$parasites)))
            if (exists("peau")) {save1 = cbind(save1,data.frame("Peau" = c(peau))) } else {save1 = cbind(save1,data.frame("Peau" = c("")))}
            if (exists("poils")) {save1 = cbind(save1,data.frame("poils" = c(poils))) } else {save1 = cbind(save1,data.frame("poils" = c("")))}
            if (exists("sang")) {save1 = cbind(save1,data.frame("sang" = c(sang))) } else {save1 = cbind(save1,data.frame("sang" = c("")))}
            if (exists("feces")) {save1 = cbind(save1,data.frame("feces" = c(feces))) } else {save1 = cbind(save1,data.frame("feces" = c("")))}
            if (exists("tiques")) {save1 = cbind(save1,data.frame("tiques" = c(tiques))) }else {save1 = cbind(save1,data.frame("tiques" = c("")))}
            if (exists("vagin")) {save1 = cbind(save1,data.frame("vaginal" = c(vagin))) } else {save1 = cbind(save1,data.frame("vaginal" = c("")))}
            if (exists("nez")) {save1 = cbind(save1,data.frame("Nasal" = c(nez))) } else {save1 = cbind(save1,data.frame("Nasal" = c("")))}
            if (!is.null(input$remarques_ani)) {save1 = cbind(save1,data.frame("remarque" = c(input$remarques_ani)))} else {save1 = cbind(save1,data.frame("remarque" = c("")))}
            save1 = cbind(save1,data.frame("Collier" = c(collier_tech_test)))
            save1 = cbind(save1,data.frame("accelero" = c(collier_acc)))
            save1 = cbind(save1,data.frame("proximite" = c(collier_prox)))
            save1 = cbind(save1,data.frame("id_collier" = c(collier_id_usuel)))
            save1 = cbind(save1,data.frame("dat_deb" = c(input$date_caract)))
            save1 = cbind(save1,data.frame("date_fin" = c("")))
            save1 = cbind(save1,data.frame("date_fin arrondie" = c("")))
            save1 = cbind(save1,data.frame("date_fin_capteur" = c("")))
            save1 = cbind(save1,data.frame("suivi_GPS oui si>60jours" = c("")))
            save1 = cbind(save1,data.frame("jrs_suivi" = c("")))
            save1 = cbind(save1,data.frame("capteur Activite" = c(collier_act)))
            if (!is.null(input$remarque_collier)) {save1 = cbind(save1,data.frame("probleme collier" = c(input$remarque_collier)))} else {save1 = cbind(save1,data.frame("probleme collier" = c("")))}
            save1 = cbind(save1,data.frame("site vie" = c("")))
            save1 = cbind(save1,data.frame("secteur" = c("")))
            save1 = cbind(save1,data.frame("Mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort arrondie" = c("")))
            save1 = cbind(save1,data.frame("Cause detaille" = c("")))
            save1 = cbind(save1,data.frame("cause categories" = c("")))
            save1 = cbind(save1,data.frame("Pds mort" = c("")))
            save1 = cbind(save1,data.frame("nom capteur" = c("")))
            save1 = cbind(save1,data.frame("nombre d'experimentes (n)" = c("")))
            save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("lutte" = (c(""))))
            save1 = cbind(save1,data.frame("haletement (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("cri (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("acepromazine (1=0,3cc)" = c("")))
            save1 = cbind(save1,data.frame("num_sabot" = c(input$numSabot)))
            save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("agitation (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("retournement (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("hre fin surv" = c("")))
            save1 = cbind(save1,data.frame("surveillance (mn)" = c("")))
            save1 = cbind(save1,data.frame("distance (KM)" = c("")))
            if (is.null(input$lutte)) { save1 = cbind(save1,data.frame("lutte (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte (1/0)" = (c(input$lutte))))}
            if (is.null(input$halete)) { save1 = cbind(save1,data.frame("halete (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("halete (1/0)" = (c(input$halete))))}
            save1 = cbind(save1,data.frame("cri" = c(cri_total)))
            save1 = cbind(save1,data.frame("T°C 1" = c("")))
            save1 = cbind(save1,data.frame("T°C 2" = c("")))
            save1 = cbind(save1,data.frame("Cœur 1" = c("")))
            save1 = cbind(save1,data.frame("Cœur 2" = c("")))
            save1 = cbind(save1,data.frame("remarque_table" = c(input$remarques_table)))
            save1 = cbind(save1,data.frame("localisation sonde temperature" = c(input$position_temp1))) 
            save1 = cbind(save1,data.frame("eurodeer" = c(input$Notation_euro_table)))
            save1 = cbind(save1,data.frame("titube (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("course (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("tombe (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("cabriole (1/0)" = (c("")))) 
            save1 = cbind(save1,data.frame("bolide (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c("")))) 
            save1 = cbind(save1,data.frame("filet" = c("")))
            save1 = cbind(save1,data.frame("sabot sur place" = c("")))
            save1 = cbind(save1,data.frame("transport+attente" = c("")))
            save1 = cbind(save1,data.frame("marquage" = c(duree_marquage) ))
            save1 = cbind(save1,data.frame("total" = c("")))
            save1 = cbind(save1,data.frame("capture" = c("")))
            save1 = cbind(save1,data.frame("sabot" = c("")))
            save1 = cbind(save1,data.frame("acepro" = c("")))
            save1 = cbind(save1,data.frame("transport" = c("")))
            save1 = cbind(save1,data.frame("table" = c(input$time_caract)))
            save1 = cbind(save1,data.frame("lache" = c("")))
            save1 = cbind(save1,data.frame("remarque_generale" = c("")))
            if (is.null(input$cribague)) { save1 = cbind(save1,data.frame("bague" = (c(""))))} else {save1 = cbind(save1,data.frame("bague" = c(input$cribague)) )}
            if (is.null(input$criautre)) { save1 = cbind(save1,data.frame("autre" = (c(""))))} else {save1 = cbind(save1,data.frame("autre" = c(input$criautre)))}
            save1 = cbind(save1,data.frame("stop" = c("")))
            save1 = cbind(save1,data.frame("habitat lacher" = c("")))
            save1 = cbind(save1,data.frame("habite perte vue" = c("")))
            save1 = cbind(save1,data.frame("visibilite" = c("")))
            save1 = cbind(save1,data.frame("nb_public" = c("")))
            save1 = cbind(save1,data.frame("eurodeer_lacher" = c("")))
            save1 = cbind(save1,data.frame("remise sabot" = c("")))
            save1 = cbind(save1,data.frame("heure_lacher_2" = c("")))
          }
          
          ######## cas d''une recapture 
          
 if(input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$nAnimal2!="") {
            save1 = data.frame("N°Animal" = c(input$nAnimal2))
            save1 = cbind(save1,data.frame("ani_nom" = c("")))
            save1 = cbind(save1,data.frame("N°Animal telemetrie" = c(paste0(tolower(input$sexe),cat_age,"_",input$nAnimal2))))#change
            save1 = cbind(save1,data.frame("N° bague annee capture" = c(cap_bague2)))
            save1 = cbind(save1,data.frame("Nombre capture" = c(nbre_capt)))
            save1 = cbind(save1,data.frame("inconnue" = c("")))
            save1 = cbind(save1,data.frame("Site Capture" = c(input$idSite2)))
            save1 = cbind(save1,data.frame("capture faon" = c(faon2)))
            save1 = cbind(save1,data.frame("Date" = c(input$date_caract)))
            save1 = cbind(save1,data.frame("jour" = c(jour)))
            save1 = cbind(save1,data.frame("mois" = c(mois)))
            save1 = cbind(save1,data.frame("annee" = c(annee)))
            save1 = cbind(save1,data.frame("annee  de suivi" = c(annee_suivie)))
            if (!is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = c(input$sexe))) }
            if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("Sexe" = c(""))) }
            save1 = cbind(save1,data.frame("Age cahier" = c(input$age)))
            save1 = cbind(save1,data.frame("Age corrige" = c(input$age)))
            save1 = cbind(save1,data.frame("categorie d'age" = c(cat_age_all)))
            save1 = cbind(save1,data.frame("etat_sante" = c(bledia)))
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == F) {save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD2))) }
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == T) {save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD3))) }
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == F) {save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG2))) }
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == T) {save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG3))) }
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == F) {save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d2)))}
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagD == T) {save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d3))) }
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == F) {save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g2))) }
            if (input$estNouvelAnimal == 'non' && input$identifie == 'oui' && input$newTagG == T) {save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g3))) }
            save1 = cbind(save1,data.frame("cap_pertinent" = c(cap_pertinent)))
            if (is.null(input$sexe)) {save1 = cbind(save1,data.frame("cap_lactation" = c("indeterminé"))) }
            if (input$sexe == 'M') {save1 = cbind(save1,data.frame("cap_lactation" = c("non")))}
            if (input$sexe == 'F') {save1 = cbind(save1,data.frame("cap_lactation" = c(input$lactation)))}
            if (input$newRFIDbox == F && input$idRFID2!=""){save1 = cbind(save1,data.frame("RFID" = c(input$idRFID2)))}
            if (input$newRFIDbox == F && input$idRFID2==""){save1 = cbind(save1,data.frame("RFID" = c("")))}
            if (input$newRFIDbox == T && input$idRFID_new!=""){save1 = cbind(save1,data.frame("RFID" = c(input$idRFID_new)))}
            if (input$newRFIDbox == T && input$idRFID_new==""){ save1 = cbind(save1,data.frame("RFID" = c("")))}
            save1 = cbind(save1,data.frame("Poids" = c(input$pSabotPlein - input$pSabotVide)))
            save1 = cbind(save1,data.frame("Cir Cou" = c(input$cirCou)))
            save1 = cbind(save1,data.frame("Long patte Ar" = c(input$lPattArriere)))
            save1 = cbind(save1,data.frame("machoire" = c("")))
            save1 = cbind(save1,data.frame("long bois gauche" = c(input$lBoisGauche)))
            save1 = cbind(save1,data.frame("long bois droit" = c(input$lBoisDroit)))
            save1 = cbind(save1,data.frame("glucose" = c(input$tglucose)))
            save1 = cbind(save1,data.frame("T°C_ext" = c("")))
            save1 = cbind(save1,data.frame("TIQUES FIXES" = c(input$tiques)))
            save1 = cbind(save1,data.frame("autres parasites"= c(input$parasites)))
            if (exists("peau")) {save1 = cbind(save1,data.frame("Peau" = c(peau))) } else {save1 = cbind(save1,data.frame("Peau" = c("")))}
            if (exists("poils")) {save1 = cbind(save1,data.frame("poils" = c(poils))) } else {save1 = cbind(save1,data.frame("poils" = c("")))}
            if (exists("sang")) {save1 = cbind(save1,data.frame("sang" = c(sang))) } else {save1 = cbind(save1,data.frame("sang" = c("")))}
            if (exists("feces")) {save1 = cbind(save1,data.frame("feces" = c(feces))) } else {save1 = cbind(save1,data.frame("feces" = c("")))}
            if (exists("tiques")) {save1 = cbind(save1,data.frame("tiques" = c(tiques))) } else {save1 = cbind(save1,data.frame("tiques" = c("")))}
            if (exists("vagin")) {save1 = cbind(save1,data.frame("vaginal" = c(vagin))) } else {save1 = cbind(save1,data.frame("vaginal" = c("")))}
            if (exists("nez")) {save1 = cbind(save1,data.frame("Nasal" = c(nez))) } else {save1 = cbind(save1,data.frame("Nasal" = c("")))}
            if (!is.null(input$remarques_ani)) {save1 = cbind(save1,data.frame("remarque" = c(input$remarques_ani)))} else {save1 = cbind(save1,data.frame("remarque" = c("")))}
            save1 = cbind(save1,data.frame("Collier" = c(collier_tech_test)))
            save1 = cbind(save1,data.frame("accelero" = c(collier_acc)))
            save1 = cbind(save1,data.frame("proximite" = c(collier_prox)))
            save1 = cbind(save1,data.frame("id_collier" = c(collier_id_usuel)))
            save1 = cbind(save1,data.frame("dat_deb" = c(input$date_caract)))
            save1 = cbind(save1,data.frame("date_fin" = c("")))
            save1 = cbind(save1,data.frame("date_fin arrondie" = c("")))
            save1 = cbind(save1,data.frame("date_fin_capteur" = c("")))
            save1 = cbind(save1,data.frame("suivi_GPS oui si>60jours" = c("")))
            save1 = cbind(save1,data.frame("jrs_suivi" = c("")))
            save1 = cbind(save1,data.frame("capteur Activite" = c(collier_act)))
            if (!is.null(input$remarque_collier)) {save1 = cbind(save1,data.frame("probleme collier" = c(input$remarque_collier)))} else {save1 = cbind(save1,data.frame("probleme collier" = c("")))}
            save1 = cbind(save1,data.frame("site vie" = c("")))
            save1 = cbind(save1,data.frame("secteur" = c("")))
            save1 = cbind(save1,data.frame("Mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort arrondie" = c("")))
            save1 = cbind(save1,data.frame("Cause detaille" = c("")))
            save1 = cbind(save1,data.frame("cause categories" = c("")))
            save1 = cbind(save1,data.frame("Pds mort" = c("")))
            save1 = cbind(save1,data.frame("nom capteur" = c("")))
            save1 = cbind(save1,data.frame("nombre d'experimentes (n)" = c("")))
            save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("lutte" = (c(""))))
            save1 = cbind(save1,data.frame("haletement (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("cri (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("acepromazine (1=0,3cc)" = c("")))
            save1 = cbind(save1,data.frame("num_sabot" = c(input$numSabot)))
            save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("agitation (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("retournement (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("hre fin surv" = c("")))
            save1 = cbind(save1,data.frame("surveillance (mn)" = c("")))
            save1 = cbind(save1,data.frame("distance (KM)" = c("")))
            if (is.null(input$lutte)) { save1 = cbind(save1,data.frame("lutte (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte (1/0)" = (c(input$lutte))))}
            if (is.null(input$halete)) { save1 = cbind(save1,data.frame("halete (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("halete (1/0)" = (c(input$halete))))}
            save1 = cbind(save1,data.frame("cri" = c(cri_total)))
            save1 = cbind(save1,data.frame("T°C 1" = c("")))
            save1 = cbind(save1,data.frame("T°C 2" = c("")))
            save1 = cbind(save1,data.frame("Cœur 1" = c("")))
            save1 = cbind(save1,data.frame("Cœur 2" = c("")))
            save1 = cbind(save1,data.frame("remarque_table" = c(input$remarques_table)))
            save1 = cbind(save1,data.frame("localisation sonde temperature" = c(input$position_temp1))) 
            save1 = cbind(save1,data.frame("eurodeer" = c(input$Notation_euro_table)))
            save1 = cbind(save1,data.frame("titube (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("course (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("tombe (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("cabriole (1/0)" = (c("")))) 
            save1 = cbind(save1,data.frame("bolide (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c(""))))
            save1 = cbind(save1,data.frame("filet" = c("")))
            save1 = cbind(save1,data.frame("sabot sur place" = c("")))
            save1 = cbind(save1,data.frame("transport+attente" = c("")))
            save1 = cbind(save1,data.frame("marquage" = c(duree_marquage)))
            save1 = cbind(save1,data.frame("total" = c("")))
            save1 = cbind(save1,data.frame("capture" = c("")))
            save1 = cbind(save1,data.frame("sabot" = c("")))
            save1 = cbind(save1,data.frame("acepro" = c("")))
            save1 = cbind(save1,data.frame("transport" = c("")))
            save1 = cbind(save1,data.frame("table" = c(input$time_caract)))
            save1 = cbind(save1,data.frame("lache" = c("")))
            save1 = cbind(save1,data.frame("remarque_generale" = c("")))
            if (is.null(input$cribague)) { save1 = cbind(save1,data.frame("bague" = (c(""))))} else {save1 = cbind(save1,data.frame("bague" = c(input$cribague)) )}
            if (is.null(input$criautre)) { save1 = cbind(save1,data.frame("autre" = (c(""))))} else {save1 = cbind(save1,data.frame("autre" = c(input$criautre)))}
            save1 = cbind(save1,data.frame("stop" = c("")))
            save1 = cbind(save1,data.frame("habitat lacher" = c("")))
            save1 = cbind(save1,data.frame("habite perte vue" = c("")))
            save1 = cbind(save1,data.frame("visibilite" = c("")))
            save1 = cbind(save1,data.frame("nb_public" = c("")))
            save1 = cbind(save1,data.frame("eurodeer_lacher" = c("")))
            save1 = cbind(save1,data.frame("remise sabot" = c("")))
            save1 = cbind(save1,data.frame("heure_lacher_2" = c("")))
            
          }
         
          write.table(save1 , file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append = TRUE, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), na="", row.names = F, sep=";")
        
        }}
      
  ##################           CSV CHECKLIST 2                          #####     
      
      modalCallback_check2 = function  (value) {
        if (value == TRUE) {

         fichier_lu <- read.table(file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), sep = ";", na.strings = "", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))

         colnames(fichier_lu)<- noms_colonnes
         
         
         remarque_tot = paste0(input$remarques_capt, input$remarque_collier, input$remarques_table, input$remarques_lacher, sep = "~")
          
          if ((input$time2)!=""){
            remise_sabot = 1}
          else {remise_sabot = ""}
          
          if (input$nAnimal != "") {
            select_line = which(fichier_lu[1]==(input$nAnimal),arr.ind=TRUE)[1] }
          if (input$nAnimal2 != "") {
            select_line = which(fichier_lu[1]==(input$nAnimal2),arr.ind=TRUE)[1] }
          
          ####column to fill
          col_concerned<- c("titube (1/0)","couche (1/0)", "course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher")
          ####reactive values names
          reactive_values <- c("titube" ,"couche","vitesse","tombe","gratte_collier","cabriole_saut","allure","cri","nbre_stops","habitat","habitat_perte","visibilite","nbre_personnes","Notation_euro")
          ####loop to fill each column with corresponding reactive_value

          for (i in 1: length(col_concerned)){
           #print(paste0("",reactive_values[i]," est ", length(as.character(input[[reactive_values[i]]],""))))
            if (length(as.character(input[[reactive_values[i]]])) != 0) {
            fichier_lu[select_line,col_concerned[i]]<- input[[reactive_values[i]]]
            } else {
            fichier_lu[select_line, col_concerned[i]]<- c("")}}
        
          #####add fill the last column automatically 
    
          fichier_lu[select_line,"lache"]<- input$time
          fichier_lu[select_line,"hre_lacher_2"]<- input$time2
          fichier_lu[select_line,"remise sabot"]<- remise_sabot
          fichier_lu[select_line,"remarque_generale"]<- remarque_tot
          
          
          #print(fichier_lu)
          write.table(fichier_lu, file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), sep = ";", na = "", append = F, row.names = F)
          
          
          #######preparation des champs pour la saisie d''un nouvel individu
          text_input<-c("time_caract", "time_table","time","time2","idTagOrD", "idTagOrG","lBoisDroit","lBoisGauche", "liste_blessures","nAnimal", "nom_capteur_txt", "Observateur","parasites", "remarque_ani","remarque_collier","Remarques","remarques_ble","remarques_capt","remarques_lacher","remarques_prel","remarques_table", "rfid_erase")
          numeric_input<-c("tglucose","cirCou","lPattArriere", "pSabotPlein","pSabotVide", "nbre_stops")
          awe_radio_input<-c("sexe")
          radio_input<-c("lutte", "halete", "titube" ,"couche","tombe","gratte_collier","cabriole_saut","cri")
          check_input<-c("metal_tag_d","metal_tag_d2","metal_tag_g","metal_tag_g2","newRFIDbox","newTagD","newTagG", "suivi_temp")
          select_input<-c("visibilite", "habitat", "habitat_perte","tiques","diarrhee","age","idRFID", "idRFID2", "idTagOrD2","idTagOrG2","lactation","nAnimal2","Nbre_pers_experimentes","nbre_personnes","Notation_euro", "Notation_euro_table","numSabot","numSabot_capture")
    
          updateRadioButtons(session,"estNouvelAnimal", selected = "oui")
          updateRadioButtons(session,"identifie", selected = "non")
          updateSelectInput(session,"idSite2", selected = input$idSite2)
          updateSelectInput(session,"idSite", selected = input$idSite)
          updateRadioButtons(session,"cribague", choices = choix[["cribague"]], selected = FALSE)
          updateRadioButtons(session,"criautre", choices = choix[["criautre"]], selected = FALSE)
          updateRadioButtons(session,"vitesse", choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = FALSE)
          updateRadioButtons(session,"allure", choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = FALSE)

          
          for (i in 1:length(text_input)){
            updateTextInput(session, text_input[i], value = NA, placeholder = "Entrez un texte :")}
          for (i in 1:length(numeric_input)){
            updateNumericInput(session, numeric_input[i], value = NA)}
          for (i in 1:length(radio_input)){
            updateRadioButtons(session, radio_input[i], choiceNames = choix[["names_oui_non"]],choiceValues =choix[["values_oui_non"]], selected = FALSE)}
          for (i in 1:length(check_input)){
            updateCheckboxInput(session, check_input[i], value = FALSE)}
          for (i in 1:length(select_input)){
            updateSelectizeInput(session, select_input[i], choices = choix[[select_input[i]]], selected = NULL)}
          for (i in 1:length(awe_radio_input)){
            updateRadioButtons(session, awe_radio_input[i],  choices = choix[[awe_radio_input[i]]], selected = NA)}
          
          ###effacement du tableau de prelevements
          prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
          output$tableprelevement = DT::renderDT(prelevement,server = F)
          ###effacement des blessures
          blessure <- data.frame()
          output$tableblessure = DT::renderDT(blessure,server = F) 
          ####deselection du collier
          proxy <- dataTableProxy("tablecollier",session, deferUntilFlush = FALSE)
          reloadData(proxy, resetPaging = TRUE, clearSelection = c("all"))
          ####remise a jour de la ligne texte de selection du collier
          output$collier_choisi = renderText("")  
          
        }}
      
  ##################           CSV CHECKLIST 3                          #####     
      
      
      modalCallback_check3 = function(value) {
        if (value == TRUE) {
          

          fichier_lu2 <- read.table(file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, colClasses = c("character"))
  
          colnames(fichier_lu2)<- noms_colonnes

          if (input$nAnimal != "") {
            select_line = which(fichier_lu2[1]==(input$nAnimal),arr.ind=TRUE)[1] }
          if (input$nAnimal2 != "") {
            select_line = which(fichier_lu2[1]==(input$nAnimal2),arr.ind=TRUE)[1] }
          
          if (!is.null(input$time2)){
            #difftime(gettime_time2_posix
            duree_totale <- sub(" hours","",sub("Time difference of ","",difftime(strptime(input$time2, "%Y-%m-%d %H:%M:%S"), strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"))))}
          else {duree_totale <- sub(" hours","",sub("Time difference of ","",difftime(strptime(input$time, "%Y-%m-%d %H:%M:%S"), strptime(input$cpt_heure_debut_filet, "%Y-%m-%d %H:%M:%S"))))}
                
          duree_totale<- times(as.numeric(duree_totale) / (24 * 3600))

          surveillance<- sub(" hours","",sub("Time difference of ","",difftime(strptime(input$cpt_heure_fin_surv, "%Y-%m-%d %H:%M:%S"),strptime(input$cpt_heure_mise_sabot, "%Y-%m-%d %H:%M:%S"))))
          surveillance<- times(as.numeric(surveillance) / (24 * 3600))
          
          ####column to fill
          col_concerned<- c("nom capteur", "nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)"
                            ,"lutte","haletement (1/0)","cri (1/0)","filet","capture","acepromazine (1=0,3cc)"
                            ,"sabot","acepro","couche (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv")
          ####reactive values names
          reactive_values <- c("nom_capteur_txt","Nbre_pers_experimentes","cpt_filet_vitesse","cpt_filet_allure"
                            ,"cpt_filet_lutte","cpt_filet_halete","cpt_filet_cri","cpt_temps_filet","cpt_heure_debut_filet","cpt_dose_acepromazine"
                            ,"cpt_heure_mise_sabot","cpt_heure_mise_sabot","cpt_sabot_couche","cpt_sabot_agitation","cpt_sabot_retournement","cpt_heure_fin_surv")
          ####loop to fill each column with corresponding reactive_value
          
            for (i in 1: length(col_concerned)){
            print(paste0("",reactive_values[i]," est ", length(as.character(input[[reactive_values[i]]],""))))
            if (length(as.character(input[[reactive_values[i]]])) != 0) {
              fichier_lu2[select_line,col_concerned[i]]<- input[[reactive_values[i]]]
            } else {
              fichier_lu2[select_line, col_concerned[i]]<- c("")}}
          
          #####add fill the last column automatically  
          fichier_lu2[select_line,"total"]<- duree_totale
          fichier_lu2[select_line,"surveillance (mn)"]<- surveillance
          
          write.table(fichier_lu2, file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), sep = ";", na = "", append = F, row.names = F)
          
          #######preparation des champs pour la saisie d''un nouvel individu
          text_input<-c("remarques_capt", "nom_capteur_txt","Remarques","Observateur","time_caract", "time_table","time","time2","idTagOrD", "idTagOrG","lBoisDroit","lBoisGauche", "liste_blessures","nAnimal", "nom_capteur_txt", "Observateur","parasites", "remarque_ani","remarque_collier","Remarques","remarques_ble","remarques_capt","remarques_lacher","remarques_prel","remarques_table", "rfid_erase")
          numeric_input<-c("cpt_temps_filet","tglucose","cirCou","lPattArriere", "pSabotPlein","pSabotVide", "nbre_stops")
          awe_radio_input<-c("sexe")
          radio_input<-c("cpt_filet_cri", "cpt_filet_halete", "cpt_sabot_agitation","cpt_sabot_couche", "cpt_sabot_retournement", "lutte", "halete", "titube" ,"couche","tombe","gratte_collier","cabriole_saut","cri")
          check_input<-c("metal_tag_d","metal_tag_d2","metal_tag_g","metal_tag_g2","newRFIDbox","newTagD","newTagG", "suivi_temp")
          select_input<-c("Nbre_pers_experimentes", "cpt_dose_acepromazine","visibilite", "habitat", "habitat_perte","tiques","diarrhee","age","idRFID", "idRFID2", "idTagOrD2","idTagOrG2","lactation","nAnimal2","Nbre_pers_experimentes","nbre_personnes","Notation_euro", "Notation_euro_table","numSabot","numSabot_capture")
          
          updateRadioButtons(session,"estNouvelAnimal", selected = "oui")
          updateRadioButtons(session,"identifie", selected = "non")
          updateSelectInput(session,"idSite2", selected = input$idSite2)
          updateSelectInput(session,"idSite", selected = input$idSite)
          updateRadioButtons(session,"cribague", choices = choix[["cribague"]], selected = FALSE)
          updateRadioButtons(session,"criautre", choices = choix[["criautre"]], selected = FALSE)
          updateRadioButtons(session,"vitesse", choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = FALSE)
          updateRadioButtons(session,"allure", choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = FALSE)
          updateRadioButtons(session,"cpt_filet_vitesse", choiceNames = choix[["vitesse"]],choiceValues = choix[["values_vitesse"]], selected = FALSE)
          updateRadioButtons(session,"cpt_filet_allure", choiceNames = choix[["allure"]],choiceValues = choix[["values_allure"]], selected = FALSE)
          updateRadioButtons(session,"cpt_filet_lutte", choices =choix[["cpt_filet_lutte"]], selected = FALSE)
          updateTimeInput(session, "cpt_heure_fin_surv", value = strptime("00:00",format = "%H:%M"))
          updateTimeInput(session, "cpt_heure_debut_filet", value = strptime("00:00",format = "%H:%M"))
          updateTimeInput(session, "cpt_heure_mise_sabot", value = strptime("00:00",format = "%H:%M"))
          
          for (i in 1:length(text_input)){
            updateTextInput(session, text_input[i], value = NA, placeholder = "Entrez un texte :")}
          for (i in 1:length(numeric_input)){
            updateNumericInput(session, numeric_input[i], value = NA)}
          for (i in 1:length(radio_input)){
            updateRadioButtons(session, radio_input[i], choiceNames = choix[["names_oui_non"]],choiceValues =choix[["values_oui_non"]], selected = FALSE)}
          for (i in 1:length(check_input)){
            updateCheckboxInput(session, check_input[i], value = FALSE)}
          for (i in 1:length(select_input)){
            updateSelectizeInput(session, select_input[i], choices = choix[[select_input[i]]], selected = NULL)}
          for (i in 1:length(awe_radio_input)){
            updateRadioButtons(session, awe_radio_input[i],  choices = choix[[awe_radio_input[i]]], selected = NA)}
          
          ###effacement du tableau de prelevements
          prelevement <<- prelevement[-as.numeric(input$tableprelevement_rows_selected),]
          output$tableprelevement = DT::renderDT(prelevement,server = F)
          ###effacement des blessures
          blessure <- data.frame()
          output$tableblessure = DT::renderDT(blessure,server = F) 
          ####deselection du collier
          proxy <- dataTableProxy("tablecollier",session, deferUntilFlush = FALSE)
          reloadData(proxy, resetPaging = TRUE, clearSelection = c("all"))
          ####remise a jour de la ligne texte de selection du collier
          output$collier_choisi = renderText("")  
          
          
          # save3 = data.frame()
          # 
          # save3 = data.frame("num_sabot" = c(input$numSabot_capture))
          # save3 = cbind(save3,data.frame("nom capteur" = c(input$nom_capteur_txt)))
          # save3 = cbind(save3,data.frame("nombre d'experimentes (n)" = c(input$Nbre_pers_experimentes)))
          # if (is.null(input$cpt_filet_vitesse)) { save3 = cbind(save3,data.frame("arrivee filet course (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("arrivee filet course (1/0)" = (c(input$cpt_filet_vitesse))))}
          # if (is.null(input$cpt_filet_allure)) { save3 = cbind(save3,data.frame("arrivee filet panique (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("arrivee filet panique (1/0)" = (c(input$cpt_filet_allure))))}
          # if (is.null(input$cpt_filet_lutte)) { save3 = cbind(save3,data.frame("lutte" = (c(""))))} else {save3 = cbind(save3,data.frame("lutte" = (c(input$cpt_filet_lutte))))}
          # if (is.null(input$cpt_filet_halete)) { save3 = cbind(save3,data.frame("haletement (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("haletement (1/0)" = (c(input$cpt_filet_halete))))}
          # if (is.null(input$cpt_filet_cri)) { save3 = cbind(save3,data.frame("cri (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("cri (1/0)" = (c(input$cpt_filet_cri))))}
          # save3 = cbind(save3,data.frame("filet" = c(input$cpt_temps_filet,stringsAsFactors = FALSE)))
          # save3 = cbind(save3,data.frame("capture" = c(input$cpt_heure_debut_filet,stringsAsFactors = FALSE)))
          # 
          # save3 = cbind(save3,data.frame("acepromazine (1=0,3cc)" = c(input$cpt_dose_acepromazine),stringsAsFactors = FALSE))
          # save3 = cbind(save3,data.frame("total" = c(duree_totale)))
          # save3 = cbind(save3,data.frame("sabot" = c(input$cpt_heure_mise_sabot)))
          # save3 = cbind(save3,data.frame("acepro" = c(input$cpt_heure_mise_sabot)))
          # if (is.null(input$cpt_sabot_couche)) { save3 = cbind(save3,data.frame("couche (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("couche (1/0)" = (c(input$cpt_sabot_couche))))}
          # if (is.null(input$cpt_sabot_agitation)) { save3 = cbind(save3,data.frame("agitation (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("agitation (1/0)" = (c(input$cpt_sabot_agitation))))}
          # if (is.null(input$cpt_sabot_retournement)) { save3 = cbind(save3,data.frame("retournement (1/0)" = (c(""))))} else {save3 = cbind(save3,data.frame("retournement (1/0)" = (c(input$cpt_sabot_retournement))))}
          # save3 = cbind(save3,data.frame("surveillance (mn)" = c(as.integer(input$cpt_heure_fin_surv) - as.integer(input$cpt_heure_mise_sabot))))
          # save3 = cbind(save3,data.frame("hre fin surv" = c(input$cpt_heure_fin_surv,stringsAsFactors = FALSE)))
          # 
          # fichier_lu2$num_sabot[select_line] <- paste(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"num_sabot"])
          # fichier_lu2$nom.capteur[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"nom.capteur"])
          # fichier_lu2$nombre.d.experimentes..n.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"nombre.d.experimentes..n."])
          # fichier_lu2$arrivee.filet.course..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"arrivee.filet.course..1.0."])
          # fichier_lu2$arrivee.filet.panique..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"arrivee.filet.panique..1.0."])
          # fichier_lu2$lutte[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"lutte"])
          # fichier_lu2$haletement..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"haletement..1.0."])
          # fichier_lu2$cri..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"cri..1.0."])
          # fichier_lu2$filet[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"filet"])
          # fichier_lu2$capture[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"capture"])
          # 
          # fichier_lu2$acepromazine..1.0.3cc.[select_line] <- paste(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"acepromazine..1.0.3cc."])
          # fichier_lu2$total[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"total"])
          # fichier_lu2$sabot[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"sabot"])
          # fichier_lu2$acepro[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"acepro"])
          # fichier_lu2$couche..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"couche..1.0."])
          # fichier_lu2$agitation..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"agitation..1.0."])
          # fichier_lu2$retournement..1.0.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"retournement..1.0."])
          # fichier_lu2$surveillance..mn.[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"surveillance..mn."])
          # fichier_lu2$hre.fin.surv[select_line] <- paste0(save3[match(fichier_lu2$num_sabot[select_line],save3$num_sabot),"hre.fin.surv"])
          
          # write.table(fichier_lu2, file = paste0("captures_",gsub("-","_",input$date_capture), ".csv"), sep = ";", na = "", append = F, row.names = F)
          # 
         #  cap_id = dbGetQuery(con, paste0("select cap_id from public.t_capture_cap where cap_num_sabot = '",input$numSabot_capture,"' and cap_date = '",as.character(input$date_capture),"' " ))
         #  
         #  gettime= as.character(input$cpt_heure_debut_filet)
         #  gettime=strsplit(gettime, " ")[[1]]
         #  gettime=gettime[2]
         #  gettime2= as.character(input$cpt_temps_filet)
         #  gettime2=strsplit(gettime2, " ")[[1]]
         #  gettime2=gettime2[2]
         #  gettime3= as.character(input$cpt_heure_mise_sabot)
         #  gettime3=strsplit(gettime3, " ")[[1]]
         #  gettime3=gettime3[2]
         #  gettime4= as.character(input$cpt_heure_fin_surv)
         #  gettime4=strsplit(gettime4, " ")[[1]]
         #  gettime4=gettime4[2]
         # 
         # last_send = sprintf("UPDATE cmpt.t_capture_cpt SET cpt_nom_capteur = '%s', cpt_nbre_pers_experimentes = '%s',
         #                                cpt_heure_debut_filet = '%s',
         #                                cpt_temps_filet = '%s', cpt_arrivee_filet_course = '%s',
         #                                cpt_arrivee_filet_panique = '%s', cpt_filet_lutte = '%s',
         #                                cpt_filet_haletement = '%s', cpt_filet_cri = '%s', cpt_heure_mise_sabot = '%s' , cpt_dose_acepromazine = '%s',
         #                                cpt_sabot_retournement = '%s',cpt_sabot_couche = '%s', cpt_sabot_agitation = '%s',cpt_hre_fin_surv = '%s', cpt_remarque = '%s'
         #                                WHERE cpt_date = '%s' and cpt_cap_id = '%s'",input$nom_capteur_txt, input$Nbre_pers_experimentes, input$cpt_heure_debut_filet, input$cpt_temps_filet, input$cpt_filet_vitesse,
         #                                input$cpt_filet_allure, input$cpt_filet_lutte, input$cpt_filet_halete,input$cpt_filet_cri, input$cpt_heure_mise_sabot, input$cpt_dose_acepromazine ,input$cpt_sabot_retournement,input$cpt_sabot_couche,
         #                                input$cpt_sabot_agitation,input$cpt_heure_fin_surv, input$Remarques, as.character(input$date_capture), cap_id)
         #  
         # last_send = gsub("'NA'","NULL", last_send)
         # last_send = gsub("''","NULL", last_send)
         # # dbSendQuery(con, last_send)

         #shinyjs::js$refresh()
          
        }}
      
  
}
# ##################           BASE DE DONNEES                            ####
#         
#         date_mod = input$date_caract
#         date_mod = format(date_mod, "%d/%m/%Y")
#         date_mod = as.character(date_mod)
#         annee = strsplit(date_mod, "/")[[1]][3]
#         jour = strsplit(date_mod, "/")[[1]][1]
#         mois = strsplit(date_mod, "/")[[1]][2]
# 
#         diarrhee = paste("diarrhee/",input$diarrhee, sep="")
#         bledia = paste(input$liste_blessures, diarrhee, sep = "~")
# 
#         if (input$nAnimal != "") {
#           if (startsWith(input$nAnimal, "F")){
#             faon =  TRUE }
#           else {faon= FALSE} }
# 
#         if (input$nAnimal2 != "") {
#           if (startsWith(input$nAnimal2, "F")){
#             faon =  TRUE }
#           else {faon= FALSE} }
# 
#         if (input$age == '<1' || input$age == '0.5' ) {
#           cat_age_all = "jeune" }
#         else if (input$age=='1.5' || input$age=='2' || input$age=='1') {
#           cat_age_all = "yearling"}
#         else if (input$age=='2.5' || input$age=='3' || input$age=='3.5' || input$age=='4.5-5.5' || input$age=='4-5' || input$age=='>=6' || input$age=='>6.5') {cat_age_all="adulte"}
#         else {cat_age_all="" }
# 
#         if (input$nAnimal2!="") {
#           cap_pertinent = dbGetQuery(con,paste0("select cap_annee_suivi from public.t_capture_cap, public.t_animal_ani where cap_ani_id=ani_id and ani_etiq = '",input$nAnimal2,"' order by cap_annee_suivi DESC"))
#           cap_pertinent <- as.character(cap_pertinent[1,1])
#           if (annee == cap_pertinent) {cap_pertinent = FALSE} else {cap_pertinent = TRUE} }
# 
#         gettime= as.character(Sys.time())
#         gettime=strsplit(gettime, " ")[[1]]
#         gettime=gettime[2]
# 
#         if (as.integer(mois)>=10) {
#           annee_suivie = as.integer(annee) + 1  }
#         if (as.integer(mois)<10) {annee_suivie = annee}
# 
#         if (!is.null(input$criautre) && !is.null(input$cribague)) {
#           if (input$criautre!='0' || (input$cribague=='1-2' || input$cribague=='>2'))
#           {cri_total = 1}
#           else {cri_total = 0}
#         }
#         else {cri_total=""}
# 
#         remarque_tot = paste0(input$remarques_capt, input$remarque_collier, input$remarques_table, input$remarques_lacher, collapse = "~")
#         
#         id_lgg = dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'longueur bois gauche' ")
#         id_lgd = dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'longueur bois droit' ")
#         id_etat_bois =  dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'etat des bois' ")
#         id_lactation =  dbGetQuery(con, "select var_id from public.tr_variable_mesuree_var where var_nom_court = 'lactation' ")
# 
#         value_etatbois = dbGetQuery(con, paste0("SELECT etb_id from lu_tables.tr_etat_bois_etb where etb_description = '",input$etatBois,"' "))[1,1]
# 
#         cat_tempA = ""
#         cat_tempE = ""
# 
#         ligne_selection = input$tablecollier_rows_selected
# 
#         for (i in 1:nrow(temperature)) {
#           if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "anus")) {
#             cat_tempA = paste0(cat_tempA, temperature$Temperature_r[i], collapse = "~")
#           }
#           else if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "anus")) {
#             cat_tempA = paste0(cat_tempA, temperature$Temperature_b[i], collapse  = "~")
#           }
#           if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "exterieur")) {
#             cat_tempE = paste0(cat_tempE, temperature$Temperature_r[i], collapse = "~")
#           }
#           else if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "exterieur")) {
#             cat_tempE = paste0(cat_tempE, temperature$Temperature_b[i], collapse = "~")
#           }
#         }
# 
#         deb_zone_etude = substring(input$zone_etude, 1, 1)
#         var_id_glu =  dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='glucose_blood'"))[1,1]
#         var_id_dia =  dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='diarrhée'"))[1,1]
#         var_id_tique =  dbGetQuery(con, paste0("SELECT var_id FROM para_phy.tr_variable_measured_var where var_name_long ='ticks_count'"))[1,1]
# 
#         exp_id = dbGetQuery(con, paste0("SELECT exp_id FROM para_phy.tr_experimenter_exp where exp_name ='Verheyden, Helene/Thomas Roedl'"))[1,1]
#         cnt_id = dbGetQuery(con, paste0("SELECT cnt_id FROM para_phy.tr_analysis_counting_cnt where cnt_analysis_type ='counted around the head and between the rear leggs'"))[1,1]
#         pat_id = dbGetQuery(con, paste0("SELECT pat_id FROM para_phy.tr_pathogen_pat where pat_name ='Ixodida'"))[1,1]
# 
# 
# 
#         #### Nouvel animal ####
#         print(input$estNouvelAnimal)
#         if (input$estNouvelAnimal == 'oui') {
# 
#           if (faon == F) {
#             cap_bague = paste0(input$idtagOrD, "_", str_sub(annee_suivie, -2)) }
# 
#           if (faon == T) {
#             cap_bague = paste0("F", "_", input$idTagOrD, "_", str_sub(annee_suivie, -2)) }
# 
#           send_new1 =  sprintf("INSERT INTO public.t_animal_ani( ani_etiq, ani_sexe, ani_remarque ) values ('%s', '%s', '%s')", input$nAnimal, input$sexe, input$remarque_ani)
# 
#           send_new1 = gsub("'NA'","NULL", send_new1)
#           send_new1 = gsub("''","NULL", send_new1)
#           print(send_new1)
#           dbSendQuery(con,send_new1)
# 
#           find_ani_id = dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal,"'"))
#           find_ani_id <- find_ani_id[1,1]
#           find_site_id = dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite,"'"))
#           find_site_id <- find_site_id[1,1]
# 
#           send_new2 = sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe, cap_poids, cap_circou, cap_lpa, cap_etat_sante cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal) values ('%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
#                               find_ani_id, find_site_id, cap_bague, as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, (input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
#                               bledia ,gettime, TRUE, input$numSabot, input$idTagOrD, input$idTagOrG, input$metal_tag_d, input$metal_tag_g)
# 
#           send_new2 = gsub("'NA'","NULL", send_new2)
#           send_new2 = gsub("''","NULL", send_new2)
# 
#           dbSendQuery(con,send_new2)
# 
#           find_cap_id = dbGetQuery(con, "select cap_id from public.t_capture_cap order by cap_id desc limit 1")[1,1]
# 
#           dbSendQuery(con, sprintf("UPDATE public.t_rfid_rfi SET rfi_cap_id = '%s' where rfi_tag_code = '%s'", find_cap_id, input$idRFID))
# 
#           send_new3 = paste0("INSERT INTO cmpt.t_capture_cpt (cpt_ani_etiq, cpt_date, cpt_annee_suivi, cpt_tble_lutte, cpt_tble_halete, cpt_tble_cri_synthese, cpt_tble_cri_bague, cpt_tble_cri_autre, cpt_table_eurodeer, cpt_lache_titube, cpt_lache_couche, cpt_lache_course, cpt_lache_tombe, cpt_lache_gratte_collier, cpt_lache_cabriole, cpt_lache_bolide, cpt_lache_aboiement_cri, cpt_lache_nbre_stop, cpt_lache_habitat_lache, cpt_lache_habitat_pertevue, cpt_lache_visibilite, cpt_lache_public, cpt_lache_eurodeer,cpt_heure_debut_table, cpt_heure_lache, cpt_heure_second_lache, cpt_remarque, cpt_cap_id)
#                           values ('",input$nAnimal,"', '",as.character(input$date_caract),"', '",annee_suivie,"', '",input$lutte,"', '",input$halete,"', '",cri_total,"', '",input$cribague,"', '",input$criautre,"', '",input$Notation_euro_table,"', '",input$titube,"', '",input$couche,"', '",input$vitesse,"',
#                                   '",input$tombe,"', '",input$gratte_collier,"', '",input$cabriole_saut,"', '",input$allure,"', '",input$cri,"', '",input$nbre_stops,"', '",input$habitat,"', '",input$habitat_perte,"', '",input$visibilite,"',
#                                   '",input$nbre_personnes,"', '",input$Notation_euro,"', '",input$time_caract,"', '",input$time,"', '",input$time2,"', '",remarque_tot,"', '",find_cap_id,"')")
# 
#           send_new3 = gsub("'NA'","NULL", send_new3)
#           send_new3 = gsub("''","NULL", send_new3)
# 
#           dbSendQuery(con, send_new3)
# 
#           if (!is.na(input$lBoisGauche) && input$sexe == 'M') {
#             send_new4 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgg,"', '",find_cap_id,"','",input$lBoisGauche,"')")
#             dbSendQuery(con, send_new4)}
# 
#           if (!is.na(input$lBoisDroit) && input$sexe == 'M') {
#             send_new5 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgd,"', '",find_cap_id,"','",input$lBoisDroit,"')")
#             dbSendQuery(con, send_new5) }
# 
#           if (input$etatBois != "" && input$sexe == 'M') {
#             send_new6 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_etat_bois,"', '",find_cap_id,"','",value_etatbois,"')")
#             dbSendQuery(con, send_new6) }
# 
#           if (input$lactation != "" && input$sexe == 'F') {
#             send_new7 = paste0("INSERT INTO public.tj_mesureealpha_capture_aca (aca_var_id, aca_cap_id, aca_valeur) VALUES ('",id_lactation,"', '",find_cap_id,"','",input$lactation,"')")
#             dbSendQuery(con, send_new7) }
# 
#           for (i in 1:nrow(blessure)) {
#             if (nrow(blessure) !=0) {
#             liste_trait = blessure[i,3]
#             liste_trait =  strsplit(as.character(liste_trait), split = "_")
#             for (j in 1:length(liste_trait[[1]])) {
#               ble_loc = blessure[i,1]
#               ble_grav = blessure[i,2]
#               ble_trait = liste_trait[[1]][j]
#               id_ble_loc = dbGetQuery(con, paste0("SELECT bll_id from lu_tables.tr_blessure_localisation_bll where bll_localisation = '",ble_loc,"'"))
#               id_ble_grav = dbGetQuery(con, paste0("SELECT blg_id from lu_tables.tr_blessure_gravite_blg where blg_gravite = '",ble_grav,"' and blg_bll_id = '",id_ble_loc,"'"))
#               id_ble_trait = dbGetQuery(con, paste0("SELECT blt_id from lu_tables.tr_blessure_traitement_blt where blt_traitement = '",ble_trait,"'"))
#               send_new8 = paste0("INSERT INTO public.t_blessure_capture_blc (blc_cap_id, blc_bll_id, blc_blg_id, blc_blt_id, blc_remarque) values ('",find_cap_id,"', '",id_ble_loc,"', '",id_ble_grav,"', '",id_ble_trait,"', '",input$remarques_ble,"')")
#               dbSendQuery(con, send_new8)
#             }
#           } }
# 
#           for (i in 1:nrow(prelevement)) {
#             if (nrow(prelevement) !=0) {
#               prel_type = prelevement[i,1]
#               prel_loc = prelevement[i,2]
#               prel_cont = prelevement[i,3]
#               prel_solv = prelevement[i,4]
#               prel_nb = prelevement[i,5]
#               prel_remarque = prelevement[i,6]
#               id_prel_type = dbGetQuery(con, paste0("SELECT sat_id from lu_tables.tr_samples_types_sat where sat_type = '",prel_type,"'"))
#               id_prel_loc = dbGetQuery(con, paste0("SELECT sal_id from lu_tables.tr_samples_localisation_sal where sal_localisation = '",prel_loc,"' AND sal_sat_id = '",id_prel_type,"'"))
#               id_prel_cont = dbGetQuery(con, paste0("SELECT sac_id from lu_tables.tr_samples_contenant_sac where sac_conditionnement = '",prel_cont,"' AND sac_sat_id = '",id_prel_type,"'"))
#               id_prel_solv = dbGetQuery(con, paste0("SELECT sas_id from lu_tables.tr_samples_solvant_sas where sas_solvant = '",prel_solv,"' AND sas_sac_id = '",id_prel_cont,"' "))
#               send_new9 = paste0("INSERT INTO public.t_sample_capture_sca (sca_cap_id, sca_sat_id, sca_sal_id, sca_sac_id, sca_sas_id, sca_value, sca_remarque) values ('",find_cap_id,"', '",id_prel_type,"', '",id_prel_loc,"', '",id_prel_cont,"', '",id_prel_solv,"', '",prel_nb,"', '",prel_remarque,"')")
#               dbSendQuery(con, send_new9)
#             }}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "anus")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus' ")
#                 send_new10 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_r[i],"')")
#                 dbSendQuery(con, send_new10)
#               }}}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "exterieur")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'exterieur' ")
#                 send_new11 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_r[i],"')")
#                 dbSendQuery(con, send_new11)
#               }}}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "anus")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus' ")
#                 send_new12 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_b[i],"')")
#                 dbSendQuery(con, send_new12)
#               }}}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "exterieur")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'exterieur' ")
#                 send_new13 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_b[i],"')")
#                 dbSendQuery(con, send_new13)
#               }}}
# 
# 
#           if (!is.null(ligne_selection)) {
# find_eqt_id = dbGetQuery(con, paste0("select eqt_id from public.t_equipement_eqt where eqt_id_usuel = '",query()[ligne_selection,4],"'"))[1,1]
# find_pb_collier = dbGetQuery(con, paste0("select eqc_remarque from public.t_equipement_conf_eqc where eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
# 
#             send_new14 = paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id, eqa_eqt_id, eqa_date_debut, eqa_date_fin_arrondi, eqa_probleme, eqa_annee_suivi) VALUES
#                                      ('",find_ani_id,"', '",find_eqt_id,"', '",as.character(input$date_caract),"', FALSE ,'",find_pb_collier,"','",annee_suivie,"')")
#             send_new14 = gsub("'NA'","NULL", send_new14)
#             send_new14 = gsub("''","NULL", send_new14)
#             dbSendQuery(con, send_new14) }
# 
#           cat_labo = paste0(deb_zone_etude, "_", annee, mois, jour,"_", input$nAnimal)
#           cat_phyhuman_glu = paste0(input$nAnimal, "_", cat_labo, "_", as.character(input$date_caract), "_", 'glucose_blood')
#           cat_phyhuman_dia = paste0(input$nAnimal, "_", cat_labo, "_", as.character(input$date_caract), "_", 'diarrhée')
# 
#           if (!is.na(input$tglucose)) {
#             send_new15 = paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
#                                      ('",cat_phyhuman_glu,"', '",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_glu,"', '",exp_id,"', '",input$tglucose,"')")
#             send_new15 = gsub("'NA'","NULL", send_new15)
#             send_new15 = gsub("''","NULL", send_new15)
#             dbSendQuery(con, send_new15) }
# 
#           if (input$diarrhee != "") {
#             send_new16 = paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
#                                      ('",cat_phyhuman_dia,"', '",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_dia,"', '",exp_id,"', '",input$diarrhee,"')")
#             send_new16 = gsub("'NA'","NULL", send_new16)
#             send_new16 = gsub("''","NULL", send_new16)
#             dbSendQuery(con, send_new16) }
# 
#           if (input$tiques != "") {
#             send_new17 = paste0("INSERT INTO para_phy.t_parasitology_para ( para_ani_id, para_laboriginid, para_daysampling, para_exp_id, para_var_id, para_pat_id, para_res, para_cnt_id) VALUES
#                                      ('",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",exp_id,"', '",var_id_tique,"', '",pat_id,"', '",input$tiques,"', '",cnt_id,"')")
#             send_new17 = gsub("'NA'","NULL", send_new17)
#             send_new17 = gsub("''","NULL", send_new17)
#             dbSendQuery(con, send_new17) }
# 
#         }
# 
# 
# 
#         #### Ancien animal mais sans identifiant  ####
# 
#         else if (input$estNouvelAnimal == 'non' && input$identifie == 'non') {
# 
#           if (faon == F) {
#             cap_bague = paste0(input$idtagOrD, "_", str_sub(annee_suivie, -2)) }
# 
#           if (faon == T) {
#             cap_bague = paste0("F", "_", input$idTagOrD, "_", str_sub(annee_suivie, -2)) }
# 
#           send_old_lost1 = sprintf("INSERT INTO public.t_animal_ani( ani_etiq, ani_sexe, ani_remarque ) values ('%s', '%s', '%s')", input$nAnimal, input$sexe, input$remarque_ani)
# 
#           send_old_lost1 = gsub("'NA'","NULL", send_old_lost1)
#           send_old_lost1 = gsub("''","NULL", send_old_lost1)
# 
#           dbSendQuery(con,send_old_lost1)
# 
#           find_ani_id = dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal,"'"))
#           find_ani_id <- find_ani_id[1,1]
#           find_site_id = dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite,"'"))
#           find_site_id <- find_site_id[1,1]
# 
#           if (!is.null(ligne_selection)) { cap_pertinent2 = TRUE} else { cap_pertinent2 = FALSE}
# 
#           send_old_lost2 = sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe,cap_poids, cap_circou, cap_lpa, cap_etat_sante,cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
#                                    find_ani_id, find_site_id, cap_bague, as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, (input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
#                                    bledia,gettime, cap_pertinent2, input$numSabot, input$idTagOrD, input$idTagOrG, input$metal_tag_d, input$metal_tag_g)
# 
#           send_old_lost2 = gsub("'NA'","NULL", send_old_lost2)
#           send_old_lost2 = gsub("''","NULL", send_old_lost2)
# 
#           dbSendQuery(con,send_old_lost2)
# 
#           send_old_lost3 = sprintf("INSERT INTO public.t_correspondance_animal_cor(cor_ancien, cor_valide) values ('%s','%s')", input$nAnimal, input$nAnimal)
# 
#           send_old_lost3 = gsub("'NA'","NULL", send_old_lost3)
#           send_old_lost3 = gsub("''","NULL", send_old_lost3)
# 
#           dbSendQuery(con,send_old_lost3)
# 
#           find_cap_id = dbGetQuery(con, "select cap_id from public.t_capture_cap order by cap_id desc limit 1")[1,1]
# 
#           dbSendQuery(con, sprintf("UPDATE public.t_rfid_rfi SET rfi_cap_id = '%s' where rfi_tag_code = '%s'", find_cap_id, input$idRFID))
# 
#           send_old_lost4 = paste0("INSERT INTO cmpt.t_capture_cpt (cpt_ani_etiq, cpt_date, cpt_annee_suivi, cpt_tble_lutte, cpt_tble_halete, cpt_tble_cri_synthese, cpt_tble_cri_bague, cpt_tble_cri_autre, cpt_table_eurodeer, cpt_lache_titube, cpt_lache_couche, cpt_lache_course, cpt_lache_tombe, cpt_lache_gratte_collier, cpt_lache_cabriole, cpt_lache_bolide, cpt_lache_aboiement_cri, cpt_lache_nbre_stop, cpt_lache_habitat_lache, cpt_lache_habitat_pertevue, cpt_lache_visibilite, cpt_lache_public, cpt_lache_eurodeer,cpt_heure_debut_table, cpt_heure_lache, cpt_heure_second_lache, cpt_remarque, cpt_cap_id)
#                           values ('",input$nAnimal,"', '",as.character(input$date_caract),"', '",annee_suivie,"', '",input$lutte,"', '",input$halete,"', '",cri_total,"', '",input$cribague,"', '",input$criautre,"', '",input$Notation_euro_table,"', '",input$titube,"', '",input$couche,"', '",input$vitesse,"',
#                          '",input$tombe,"', '",input$gratte_collier,"', '",input$cabriole_saut,"', '",input$allure,"', '",input$cri,"', '",input$nbre_stops,"', '",input$habitat,"', '",input$habitat_perte,"', '",input$visibilite,"',
#                          '",input$nbre_personnes,"', '",input$Notation_euro,"', '",input$time_caract,"', '",input$time,"', '",input$time2,"', '",remarque_tot,"', '",find_cap_id,"')")
# 
#           send_old_lost4 = gsub("'NA'","NULL", send_old_lost4)
#           send_old_lost4 = gsub("''","NULL", send_old_lost4)
# 
#           dbSendQuery(con, send_old_lost4)
# 
#           if (!is.na(input$lBoisGauche) && input$sexe == 'M') {
#             send_old_lost5 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgg,"', '",find_cap_id,"','",input$lBoisGauche,"')")
#             dbSendQuery(con, send_old_lost5)}
# 
#           if (!is.na(input$lBoisDroit) && input$sexe == 'M') {
#             send_old_lost6 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgd,"', '",find_cap_id,"','",input$lBoisDroit,"')")
#             dbSendQuery(con, send_old_lost6) }
# 
#           if (input$etatBois != "" && input$sexe == 'M') {
#             send_old_lost7 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_etat_bois,"', '",find_cap_id,"','",value_etatbois,"')")
#             dbSendQuery(con, send_old_lost7) }
# 
#           if (input$lactation != "" && input$sexe == 'F') {
#             send_old_lost8 = paste0("INSERT INTO public.tj_mesureealpha_capture_aca (aca_var_id, aca_cap_id, aca_valeur) VALUES ('",id_lactation,"', '",find_cap_id,"','",input$lactation,"')")
#             dbSendQuery(con, send_old_lost8) }
# 
#           for (i in 1:nrow(blessure)) {
#             if (nrow(blessure) !=0) {
#             liste_trait = blessure[i,3]
#             liste_trait =  strsplit(as.character(liste_trait), split = "_")
#             for (j in 1:length(liste_trait[[1]])) {
#               ble_loc = blessure[i,1]
#               ble_grav = blessure[i,2]
#               ble_trait = liste_trait[[1]][j]
#               id_ble_loc = dbGetQuery(con, paste0("SELECT bll_id from lu_tables.tr_blessure_localisation_bll where bll_localisation = '",ble_loc,"'"))
#               id_ble_grav = dbGetQuery(con, paste0("SELECT blg_id from lu_tables.tr_blessure_gravite_blg where blg_gravite = '",ble_grav,"' and blg_bll_id = '",id_ble_loc,"'"))
#               id_ble_trait = dbGetQuery(con, paste0("SELECT blt_id from lu_tables.tr_blessure_traitement_blt where blt_traitement = '",ble_trait,"'"))
#               send_old_lost9 = paste0("INSERT INTO public.t_blessure_capture_blc (blc_cap_id, blc_bll_id, blc_blg_id, blc_blt_id, blc_remarque) values ('",find_cap_id,"', '",id_ble_loc,"', '",id_ble_grav,"', '",id_ble_trait,"', '",input$remarques_ble,"')")
#               dbSendQuery(con, send_old_lost9)
#             }
#           }}
# 
#           for (i in 1:nrow(prelevement)) {
#             if (nrow(prelevement) !=0) {
#               prel_type = prelevement[i,1]
#               prel_loc = prelevement[i,2]
#               prel_cont = prelevement[i,3]
#               prel_solv = prelevement[i,4]
#               prel_nb = prelevement[i,5]
#               prel_remarque = prelevement[i,6]
#               id_prel_type = dbGetQuery(con, paste0("SELECT sat_id from lu_tables.tr_samples_types_sat where sat_type = '",prel_type,"'"))
#               id_prel_loc = dbGetQuery(con, paste0("SELECT sal_id from lu_tables.tr_samples_localisation_sal where sal_localisation = '",prel_loc,"' AND sal_sat_id = '",id_prel_type,"'"))
#               id_prel_cont = dbGetQuery(con, paste0("SELECT sac_id from lu_tables.tr_samples_contenant_sac where sac_conditionnement = '",prel_cont,"' AND sac_sat_id = '",id_prel_type,"'"))
#               id_prel_solv = dbGetQuery(con, paste0("SELECT sas_id from lu_tables.tr_samples_solvant_sas where sas_solvant = '",prel_solv,"' AND sas_sac_id = '",id_prel_cont,"' "))
#               send_old_lost10 = paste0("INSERT INTO public.t_sample_capture_sca (sca_cap_id, sca_sat_id, sca_sal_id, sca_sac_id, sca_sas_id, sca_value, sca_remarque) values ('",find_cap_id,"', '",id_prel_type,"', '",id_prel_loc,"', '",id_prel_cont,"', '",id_prel_solv,"', '",prel_nb,"', '",prel_remarque,"')")
#               dbSendQuery(con, send_old_lost10)
#           }}
# 
#         for (i in 1:nrow(temperature)) {
#           if (nrow(temperature) !=0) {
#             if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "anus")) {
#               which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus' ")
#               send_old_lost11 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_r[i],"')")
#               dbSendQuery(con, send_old_lost11)
#             }}}
# 
#         for (i in 1:nrow(temperature)) {
#           if (nrow(temperature) !=0) {
#             if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "exterieur")) {
#               which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'exterieur' ")
#               send_old_lost12 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_r[i],"')")
#               dbSendQuery(con, send_old_lost12)
#             }}}
# 
#         for (i in 1:nrow(temperature)) {
#           if (nrow(temperature) !=0) {
#             if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "anus")) {
#               which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus' ")
#               send_old_lost13 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_b[i],"')")
#               dbSendQuery(con, send_old_lost13)
#             }}}
# 
#         for (i in 1:nrow(temperature)) {
#           if (nrow(temperature) !=0) {
#             if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "exterieur")) {
#               which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'exterieur' ")
#               send_old_lost14 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_b[i],"')")
#               dbSendQuery(con, send_old_lost14)
#             }}}
# 
# 
#         if (!is.null(ligne_selection)) {
# find_eqt_id = dbGetQuery(con, paste0("select eqt_id from public.t_equipement_eqt where eqt_id_usuel = '",query()[ligne_selection,4],"'"))[1,1]
# find_pb_collier = dbGetQuery(con, paste0("select eqc_remarque from public.t_equipement_conf_eqc where eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
# 
#           send_old_lost15 = paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id, eqa_eqt_id, eqa_date_debut, eqa_date_fin_arrondi, eqa_probleme, eqa_annee_suivi) VALUES
#                           ('",find_ani_id,"', '",find_eqt_id,"', '",as.character(input$date_caract),"', FALSE ,'",find_pb_collier,"','",annee_suivie,"')")
#           send_old_lost15 = gsub("'NA'","NULL", send_old_lost15)
#           send_old_lost15 = gsub("''","NULL", send_old_lost15)
#           dbSendQuery(con, send_old_lost15) }
# 
#         cat_labo = paste0(deb_zone_etude, "_", annee, mois, jour,"_", input$nAnimal)
#         cat_phyhuman_glu = paste0(input$nAnimal, "_", cat_labo, "_", as.character(input$date_caract), "_", 'glucose_blood')
#         cat_phyhuman_dia = paste0(input$nAnimal, "_", cat_labo, "_", as.character(input$date_caract), "_", 'diarrhée')
# 
#         if (!is.na(input$tglucose)) {
#           send_old_lost16 = paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
#                           ('",cat_phyhuman_glu,"', '",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_glu,"', '",exp_id,"', '",input$tglucose,"')")
#           send_old_lost16 = gsub("'NA'","NULL", send_old_lost16)
#           send_old_lost16 = gsub("''","NULL", send_old_lost16)
#           dbSendQuery(con, send_old_lost16) }
# 
#         if (input$diarrhee != "") {
#           send_old_lost17 = paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
#                           ('",cat_phyhuman_dia,"', '",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_dia,"', '",exp_id,"', '",input$diarrhee,"')")
#           send_old_lost17 = gsub("'NA'","NULL", send_old_lost17)
#           send_old_lost17 = gsub("''","NULL", send_old_lost17)
#           dbSendQuery(con, send_old_lost17) }
# 
#         if (input$tiques != "") {
#           send_old_lost18 = paste0("INSERT INTO para_phy.t_parasitology_para ( para_ani_id, para_laboriginid, para_daysampling, para_exp_id, para_var_id, para_pat_id, para_res, para_cnt_id) VALUES
#                           ('",find_ani_id,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",exp_id,"', '",var_id_tique,"', '",pat_id,"', '",input$tiques,"', '",cnt_id,"')")
#           send_old_lost18 = gsub("'NA'","NULL", send_old_lost18)
#           send_old_lost18 = gsub("''","NULL", send_old_lost18)
#           dbSendQuery(con, send_old_lost18) }
# 
#         }
# 
#         #### Ancien animal identifié ####
# 
#         else if (input$estNouvelAnimal == 'non' && input$identifie == 'oui') {
# 
#           if (input$newTagD == T  && (faon == F )) {
#             cap_bague2 = paste0(input$idTagOrD3, "_", str_sub(annee_suivie, -2)) }
# 
#           if (input$newTagD == T  && (faon == T )) {
#             cap_bague2 = paste0("F", "_", input$idTagOrD3, "_", str_sub(annee_suivie, -2)) }
# 
#           if ((input$newTagG == T) && (faon == F )) {
#             cap_bague2 = paste0(input$idTagOrG3, "_", str_sub(annee_suivie, -2)) }
# 
#           if ((input$newTagG == T) && (faon == T )) {
#             cap_bague2 = paste0("F", "_", input$idTagOrG3, "_", str_sub(annee_suivie, -2)) }
# 
#           if (input$newTagD == F && input$newTagG == F) {
#             cap_bague2 = paste0(input$nAnimal2, "_", str_sub(annee_suivie, -2)) }
# 
#           find_ani_id2 = dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal2,"'"))
#           find_ani_id2 <- find_ani_id2[1,1]
#           find_site_id2 = dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '", input$idSite2,"'"))
#           find_site_id2 <- find_site_id2[1,1]
# 
#           if (input$newTagG == F && input$newTagD == F) {
# 
#             send1 = sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe,  cap_poids, cap_circou, cap_lpa, cap_etat_sante, cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
#                             find_ani_id2, find_site_id2, cap_bague2, as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, as.integer(input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
#                             bledia,input$time, cap_pertinent, input$numSabot, input$idTagOrD2, input$idTagOrG2, input$metal_tag_d2, input$metal_tag_g2)
# 
#             send1 = gsub("'NA'","NULL", send1)
#             send1 = gsub("''","NULL", send1)
# 
#             dbSendQuery(con,send1) }
# 
# 
#           else if (input$newTagG == T && input$newTagD == F) {
# 
#             send2 = sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe, cap_poids, cap_circou, cap_lpa, cap_etat_sante, cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
#                             find_ani_id2, find_site_id2, cap_bague2, as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, as.integer(input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
#                             bledia,input$time, cap_pertinent, input$numSabot, input$idTagOrD2, input$idTagOrG3, input$metal_tag_d2, input$metal_tag_g3)
# 
#             send2 = gsub("'NA'","NULL", send2)
#             send2 = gsub("''","NULL", send2)
# 
#             dbSendQuery(con,send2) }
# 
# 
#           else if (input$newTagG == F && input$newTagD == T) {
# 
#             send3 = sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe, cap_poids, cap_circou, cap_lpa, cap_etat_sante, cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
#                             find_ani_id2, find_site_id2, cap_bague2, as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, as.integer(input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
#                             bledia,input$time, cap_pertinent, input$numSabot, input$idTagOrD3, input$idTagOrG2, input$metal_tag_d3, input$metal_tag_g2)
# 
#             send3 = gsub("'NA'","NULL", send3)
#             send3 = gsub("''","NULL", send3)
# 
#             dbSendQuery(con,send3) }
# 
# 
#           else if (input$newTagG == T && input$newTagD == T) {
# 
#             send4 = sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe, cap_poids, cap_circou, cap_lpa, cap_etat_sante, cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal, cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
#                             find_ani_id2, find_site_id2, cap_bague2, as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, as.integer(input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
#                             bledia , input$time, cap_pertinent, input$numSabot, input$idTagOrD3, input$idTagOrG3, input$metal_tag_d3, input$metal_tag_g3)
# 
#             send4 = gsub("'NA'","NULL", send4)
#             send4 = gsub("''","NULL", send4)
# 
#             dbSendQuery(con, send4)
#             }
# 
#           find_cap_id = dbGetQuery(con, "select cap_id from public.t_capture_cap order by cap_id desc limit 1")[1,1]
# 
#            if (input$newRFIDbox == T && input$idRFID_new != "") {
#             dbSendQuery(con, sprintf("UPDATE public.t_rfid_rfi SET rfi_cap_id = '%s' where rfi_tag_code = '%s'", find_cap_id, input$idRFID_new)) }
# 
#           send5 = paste0("INSERT INTO cmpt.t_capture_cpt (cpt_ani_etiq, cpt_date, cpt_annee_suivi, cpt_tble_lutte, cpt_tble_halete, cpt_tble_cri_synthese, cpt_tble_cri_bague, cpt_tble_cri_autre, cpt_tble_temp_animal, cpt_tble_temp_exterieur, cpt_table_eurodeer, cpt_lache_titube, cpt_lache_couche, cpt_lache_course, cpt_lache_tombe, cpt_lache_gratte_collier, cpt_lache_cabriole, cpt_lache_bolide, cpt_lache_aboiement_cri, cpt_lache_nbre_stop, cpt_lache_habitat_lache, cpt_lache_habitat_pertevue, cpt_lache_visibilite, cpt_lache_public, cpt_lache_eurodeer,cpt_heure_debut_table, cpt_heure_lache, cpt_heure_second_lache, cpt_remarque, cpt_cap_id)
#                           values ('",input$nAnimal2,"', '",as.character(input$date_caract),"', '",annee_suivie,"', '",input$lutte,"', '",input$halete,"', '",cri_total,"', '",input$cribague,"', '",input$criautre,"','",cat_tempA,"','",cat_tempE,"', '",input$Notation_euro_table,"', '",input$titube,"', '",input$couche,"', '",input$vitesse,"',
#                           '",input$tombe,"', '",input$gratte_collier,"', '",input$cabriole_saut,"', '",input$allure,"', '",input$cri,"', '",input$nbre_stops,"', '",input$habitat,"', '",input$habitat_perte,"', '",input$visibilite,"',
#                           '",input$nbre_personnes,"', '",input$Notation_euro,"', '",input$time_caract,"', '",input$time,"', '",input$time2,"', '",remarque_tot,"', '",find_cap_id,"')")
# 
#           print(cat_tempA)
#           print(cat_tempE)
# 
#           send5 = gsub("'NA'","NULL", send5)
#           send5 = gsub("''","NULL", send5)
# 
#           dbSendQuery(con, send5)
# 
#           if (!is.na(input$lBoisGauche) && input$sexe == 'M') {
#             send6 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgg,"', '",find_cap_id,"','",input$lBoisGauche,"')")
#             dbSendQuery(con, send6)
#             }
# 
#           if (!is.na(input$lBoisDroit) && input$sexe == 'M') {
#             send7 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_lgd,"', '",find_cap_id,"','",input$lBoisDroit,"')")
#             dbSendQuery(con, send7)
#             }
# 
#           if (input$etatBois != "" && input$sexe == 'M') {
#             send8 = paste0("INSERT INTO public.tj_mesureenum_capture_nca (nca_var_id, nca_cap_id, nca_valeur) VALUES ('",id_etat_bois,"', '",find_cap_id,"','",value_etatbois,"')")
#             dbSendQuery(con, send8)
#             }
# 
#           if (input$lactation != "" && input$sexe == 'F') {
#             send9 = paste0("INSERT INTO public.tj_mesureealpha_capture_aca (aca_var_id, aca_cap_id, aca_valeur) VALUES ('",id_lactation,"', '",find_cap_id,"','",input$lactation,"')")
#             dbSendQuery(con, send9)
#           }
# 
#          for (i in 1:nrow(blessure)) {
#            if (nrow(blessure) !=0) {
#              liste_trait = blessure[i,3]
#              liste_trait =  strsplit(as.character(liste_trait), split = "_")
#              for (j in 1:length(liste_trait[[1]])) {
#                ble_loc = blessure[i,1]
#                ble_grav = blessure[i,2]
#                ble_trait = liste_trait[[1]][j]
#                id_ble_loc = dbGetQuery(con, paste0("SELECT bll_id from lu_tables.tr_blessure_localisation_bll where bll_localisation = '",ble_loc,"'"))
#                id_ble_grav = dbGetQuery(con, paste0("SELECT blg_id from lu_tables.tr_blessure_gravite_blg where blg_gravite = '",ble_grav,"' and blg_bll_id = '",id_ble_loc,"'"))
#                id_ble_trait = dbGetQuery(con, paste0("SELECT blt_id from lu_tables.tr_blessure_traitement_blt where blt_traitement = '",ble_trait,"'"))
#                send10 = paste0("INSERT INTO public.t_blessure_capture_blc (blc_cap_id, blc_bll_id, blc_blg_id, blc_blt_id, blc_remarque) values ('",find_cap_id,"', '",id_ble_loc,"', '",id_ble_grav,"', '",id_ble_trait,"', '",input$remarques_ble,"')")
#                dbSendQuery(con, send10)
#              }
#            }
#          }
# 
#          for (i in 1:nrow(prelevement)) {
#            if (nrow(prelevement) !=0) {
#              prel_type = prelevement[i,1]
#              prel_loc = prelevement[i,2]
#              prel_cont = prelevement[i,3]
#              prel_solv = prelevement[i,4]
#              prel_nb = prelevement[i,5]
#              prel_remarque = prelevement[i,6]
#              id_prel_type = dbGetQuery(con, paste0("SELECT sat_id from lu_tables.tr_samples_types_sat where sat_type = '",prel_type,"'"))
#              id_prel_loc = dbGetQuery(con, paste0("SELECT sal_id from lu_tables.tr_samples_localisation_sal where sal_localisation = '",prel_loc,"' AND sal_sat_id = '",id_prel_type,"'"))
#              id_prel_cont = dbGetQuery(con, paste0("SELECT sac_id from lu_tables.tr_samples_contenant_sac where sac_conditionnement = '",prel_cont,"' AND sac_sat_id = '",id_prel_type,"'"))
#              id_prel_solv = dbGetQuery(con, paste0("SELECT sas_id from lu_tables.tr_samples_solvant_sas where sas_solvant = '",prel_solv,"' AND sas_sac_id = '",id_prel_cont,"' "))
#              send11 = paste0("INSERT INTO public.t_sample_capture_sca (sca_cap_id, sca_sat_id, sca_sal_id, sca_sac_id, sca_sas_id, sca_value, sca_remarque) values ('",find_cap_id,"', '",id_prel_type,"', '",id_prel_loc,"', '",id_prel_cont,"', '",id_prel_solv,"', '",prel_nb,"', '",prel_remarque,"')")
#              dbSendQuery(con, send11)
#            }}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "anus")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus' ")
#                 send12 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_r[i],"')")
#                 dbSendQuery(con, send12)
#              }}}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "rouge" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'rouge' && input$position_temp2 == "exterieur")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'exterieur' ")
#                 send13 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_r[i],"')")
#                 dbSendQuery(con, send13)
#               }}}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "anus") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "anus")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'anus' ")
#                 send14 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_b[i],"')")
#                 dbSendQuery(con, send14)
#               }}}
# 
#           for (i in 1:nrow(temperature)) {
#             if (nrow(temperature) !=0) {
#               if ((input$sonde_temp1 == "blanche" && input$position_temp1 == "exterieur") || (input$sonde_temp2 == 'blanche' && input$position_temp2 == "exterieur")) {
#                 which_sonde = dbGetQuery(con, "SELECT tel_id from lu_tables.tr_temperatures_localisation_tel where tel_localisation = 'exterieur' ")
#                 send15 = paste0("INSERT INTO public.t_temperatures_tem (tem_cap_id, tem_time_local_cest, tem_tel_id, tem_val) VALUES ('",find_cap_id,"', '",temperature$Date[i],"', '",which_sonde,"', '",temperature$Temperature_b[i],"')")
#                 dbSendQuery(con, send15)
#               }}}
# 
#           if (!is.null(ligne_selection)) {
# find_eqt_id = dbGetQuery(con, paste0("select eqt_id from public.t_equipement_eqt where eqt_id_usuel = '",query()[ligne_selection,4],"'"))[1,1]
# find_pb_collier = dbGetQuery(con, paste0("select eqc_remarque from public.t_equipement_conf_eqc where eqc_eqt_id = '",find_eqt_id,"'"))[1,1]
# 
#             send16 = paste0("INSERT INTO public.tj_equipement_animal_eqt_ani_eqa (eqa_ani_id, eqa_eqt_id, eqa_date_debut, eqa_date_fin_arrondi, eqa_probleme, eqa_annee_suivi) VALUES
#                       ('",find_ani_id2,"', '",find_eqt_id,"', '",as.character(input$date_caract),"', FALSE ,'",find_pb_collier,"','",annee_suivie,"')")
#             send16 = gsub("'NA'","NULL", send16)
#             send16 = gsub("''","NULL", send16)
#             dbSendQuery(con, send16)
#             }
# 
#           cat_labo = paste0(deb_zone_etude, "_", annee, mois, jour,"_", input$nAnimal2)
#           cat_phyhuman_glu = paste0(input$nAnimal2, "_", cat_labo, "_", as.character(input$date_caract), "_", 'glucose_blood')
#           cat_phyhuman_dia = paste0(input$nAnimal2, "_", cat_labo, "_", as.character(input$date_caract), "_", 'diarrhée')
# 
#           if (!is.na(input$tglucose)) {
#             send17 = paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
#                       ('",cat_phyhuman_glu,"', '",find_ani_id2,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_glu,"', '",exp_id,"', '",input$tglucose,"')")
#             send17 = gsub("'NA'","NULL", send17)
#             send17 = gsub("''","NULL", send17)
#             dbSendQuery(con, send17)
#             }
# 
#           if (input$diarrhee != "") {
#             send18 = paste0("INSERT INTO para_phy.t_physiology_phy (phy_human_id, phy_ani_id, phy_laboriginid, phy_daysampling, phy_var_id, phy_exp_id, phy_res) VALUES
#                             ('",cat_phyhuman_dia,"', '",find_ani_id2,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",var_id_dia,"', '",exp_id,"', '",input$diarrhee,"')")
#             send18 = gsub("'NA'","NULL", send18)
#             send18 = gsub("''","NULL", send18)
#             dbSendQuery(con, send18)
#             }
# 
#           if (input$tiques != "") {
#             send19 = paste0("INSERT INTO para_phy.t_parasitology_para ( para_ani_id, para_laboriginid, para_daysampling, para_exp_id, para_var_id, para_pat_id, para_res, para_cnt_id) VALUES
#                             ('",find_ani_id2,"', '", cat_labo,"', '",as.character(input$date_caract),"', '",exp_id,"', '",var_id_tique,"', '",pat_id,"', '",input$tiques,"', '",cnt_id,"')")
#             send19 = gsub("'NA'","NULL", send19)
#             send19 = gsub("''","NULL", send19)
#             dbSendQuery(con, send19)
#             }
# 
# 
#         }

#shinyjs::js$refresh("all")
#shinyjs::reset("all")


