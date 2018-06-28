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
  
  updateSelectizeInput(session, "idRFID", choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")) 
  updateSelectizeInput(session, "idSite", choices = dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit"))
  updateSelectizeInput(session, "nAnimal2", choices = dbGetQuery(con,"select ani_etiq from public.t_animal_ani order by ani_id DESC"))
  updateNumericInput(session, "cirCou", max = dbGetQuery(con,"select max(cap_circou) from t_capture_cap"))
  updateNumericInput(session, "lPattArriere", max = dbGetQuery(con,"select max(cap_lpa) from t_capture_cap"))
  updateNumericInput(session, "lBoisGauche", max = dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca"))
  updateNumericInput(session, "lBoisDroit", max = dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca"))
  updateSelectizeInput(session, "etatBois", choices = dbGetQuery(con,"select distinct etb_description from lu_tables.tr_etat_bois_etb order by etb_description"))
  #updateSelectizeInput(session, "idTagOrG2", choices = dbGetQuery(con, "select distinct cap_tag_gauche from public.t_capture_cap"))
  #updateSelectizeInput(session, "idTagOrD2", choices = dbGetQuery(con, "select distinct cap_tag_droit from public.t_capture_cap"))
  #updateSelectizeInput(session, "idRFID2", choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id"))
  updateSelectizeInput(session, "idSite2", choices = dbGetQuery(con, "select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap, t_animal_ani))"))
  #updateSelectizeInput(session, "idRFID_new", choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")) 
  #updateSelectizeInput(session, "age", choices = dbGetQuery(con,"select dent_valeur from lu_tables.tr_denture_dent")) 
  updateSelectizeInput(session, "numSabot", choices = dbGetQuery(con,"select sab_valeur from lu_tables.tr_sabots_sab order by sab_id")) 
  
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
      updateNumericInput(session, "pSabotPlein" , value = 0)}}
  
  output$out_sabot_vide <- renderUI({
    if (!is.na(input$pSabotVide)) {
      if (input$pSabotVide>50) {
        shinyalert("STOP!", " Poids Sabot vide elevé!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_sabot_vide )
      }} }) 
  
  modalCallback_sabot_vide <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "pSabotVide" , value = 0)}}
  
  
  ### Cou
  
  output$out_cirCou <- renderUI({
    if (!is.na(input$cirCou)) {
    if (input$cirCou > dbGetQuery(con,"select max(cap_circou) from public.t_capture_cap")) {
      shinyalert("STOP!", "Circonference élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_circou)
    }}})
  
  modalCallback_circou <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "cirCou" , value = 0)}}
  
  ### Patte
  
  output$out_lPattArriere <- renderUI({
    if(!is.na(input$lPattArriere)) {
    if (input$lPattArriere > dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")) {
      shinyalert("STOP!", "Longueur patte élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_lg_patte)
    }}})
  
  modalCallback_lg_patte <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "lPattArriere" , value = 0)}}
  
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
    updateTimeInput(session, "time_caract", value = Sys.time())
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
    if (!is.null(input$nAnimal2)) {
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
    else {selectizeInput("idTagOrG2", h4("Tag Oreille Gauche"), choices = dbGetQuery(con,"select distinct cap_tag_gauche from public.t_capture_cap"),options=list(placeholder='Choisir une valeur :',create=TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
  })
  
  output$conditionalInput2 <- renderUI({
    if(input$newTagD){
      textInput("idTagOrD3", h4("New Tag Droite"),value="")}
    else {selectizeInput("idTagOrD2", h4("Tag Oreille Droite"), choices = dbGetQuery(con,"select distinct cap_tag_droit from public.t_capture_cap"),options=list(placeholder='Choisir une valeur :',create=TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
  })
  
  output$conditionalInput3 <- renderUI({
    if(input$newRFIDbox){
      selectizeInput("idRFID_new", h4("RFID_new"), choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
    else {selectizeInput("idRFID2", h4("RFID"), choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id"), 
                         options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)}
    })
  
  output$conditionalInput4 <- renderUI({
    if(input$newTagG){
      checkboxInput("metal_tag_g3", "New Tag G. métal", value = FALSE ) }
    else {checkboxInput("metal_tag_g2", "Tag G. métal", value = FALSE )}
  })
  
  output$conditionalInput5 <- renderUI({
    if(input$newTagD){
      checkboxInput("metal_tag_d3", "New Tag D. métal", value = FALSE )}
    else {checkboxInput("metal_tag_g2", "Tag G. métal", value = FALSE )}
  })
  
  # output$conditionalInput6 <- renderUI({
  #   if(input$newRFIDbox == FALSE){
  # })
  
  
  
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
  })
  
  observeEvent(input$ajoutBle, {
    i=1
    liste_blessures =""
    while (i <= nrow(blessure)) {
      liste_blessures <- paste0(liste_blessures, blessure[i,]$Liste, "~")
      i=i+1
      updateTextInput(session, "liste_blessures", value = liste_blessures)
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
    updateTextInput(session, "remarques_prel", value = "")
  
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
    cat_prelevement1 = paste0( c(input$typetype), "_" , c(input$localoca), "_", c(input$condi), "_", c(input$solsol))
    cat_prelevement = paste0( c(input$typetype), "_" , c(input$localoca), "_", c(input$condi), "_", c(input$solsol), "_",c(input$nbre_echant))
    liste_prelevement[nrow(prelevement)] <<- cat_prelevement
    liste_prelevement2[nrow(prelevement)] <<- cat_prelevement1
    
  })
  
  liste_prelevement=list()
  liste_prelevement2=list()
  liste_prel_db = dbGetQuery(con,"select sav_intitule from lu_tables.tr_samples_verification_sav")
  
  ##################           RUBRIQUE COLLIER                         #################
  
  liste_collier <- dbGetQuery(con,"select eqc_annee_suivi, teq_nom_court, eqc_remarque, eqt_id_usuel,eqc_drop_off,sen_association, eqc_couleur_boitier, eqc_couleur_collier,eqt_frequence, eqc_memoire FROM public.t_equipement_eqt, public.t_equipement_conf_eqc, public.tr_type_equipement_teq,lu_tables.tr_sensors_sen  where eqc_eqt_id = eqt_id
                              and teq_id = eqt_teq_id and eqc_sen_id=sen_id and eqc_annee_suivi = extract(year from now()) order by teq_nom_court")
  
  output$tablecollier = DT::renderDataTable(expr = liste_collier, selection = 'single')
  
  affichage_choix_collier <- observeEvent(input$tablecollier_rows_selected, {
    if (!is.null(input$tablecollier_rows_selected)) {
      ligne_selection = input$tablecollier_rows_selected
      collier_tech = liste_collier[ligne_selection,2]
      collier_col_c = liste_collier[ligne_selection,8]
      collier_col_b = liste_collier[ligne_selection,7]
      cat_col = paste(toupper(collier_tech),": collier ", toupper(collier_col_c)," boitier ", toupper(collier_col_b) )
      output$collier_choisi = renderText(cat_col)
    }
  })
  
  
  ##################           RUBRIQUE TABLE                           #################
  
  updateSelectizeInput(session, "Notation_euro_table", choices = dbGetQuery(con,"select (ect_comportement) from lu_tables.tr_eurodeer_comp_table_ect"))
  updateSelectizeInput(session, "position_temp1", choices = dbGetQuery(con,"select tel_localisation from lu_tables.tr_temperatures_localisation_tel"), 
                       options=list(create= TRUE), selected = 'anus')
  updateSelectizeInput(session, "position_temp2", choices = dbGetQuery(con,"select tel_localisation from lu_tables.tr_temperatures_localisation_tel"), 
                       options=list(create= TRUE), selected = 'exterieur')
  
  
  observeEvent(input$identifie, {
    if (input$identifie == "oui") {
      updateAwesomeRadio(session,"cribague", selected = "NA")}  
  })
  
  observeEvent(input$to_current_time_table, {
    updateTimeInput(session, "time_table", value = Sys.time())
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
      tempr <- t(read.delim("C:/Users/marie/Desktop/w1_slave2"))[,1]
      tempr <- as.numeric(substr(tempr,as.numeric(regexpr("t=",tempr)[1])+2,as.numeric(nchar(tempr))))/1000
      tempb <- t(read.delim("C:/Users/marie/Desktop/w1_slave1"))[,1]
      tempb <- as.numeric(substr(tempb,as.numeric(regexpr("t=",tempb)[1])+2,as.numeric(nchar(tempb))))/1000
      table_temp <<- data.frame(rv$i, tempr, tempb)
      plot_temp <<- rbind(data.frame(plot_temp), table_temp)
      plot(x = plot_temp$rv.i, y = plot_temp$tempr,xlab = "Temps (sec)", ylab="Temperatures (°C)",  type = "b", xlim=c(rv$i-30,rv$i), ylim=c(20,45), col="red", pch = 2 )
      lines(x = plot_temp$rv.i, y = plot_temp$tempb, col="blue", type = "b", pch = 1)
      legend(x = "left", y = "left", legend = c("Sonde rouge", "Sonde Blanche"), col = c("red","blue"),pch = c(2,1), lty = c(1,1))
      } }
  })
  
  observeEvent(input$suivi_temp, {
    if (input$suivi_temp == T) {
      rv$i <- 0
      observe({
        isolate({
          rv$i <- rv$i + 1
        })
        
        if (isolate(rv$i) < maxIter){
          invalidateLater(1000, session)
        }
      })
 } 
    else if (input$suivi_temp == F) {
      rv$i <- 0 }
    })
  
  
  
  
  
  
  ##################           RUBRIQUE HISTORIQUE                      #################
  
  output$historique <- DT::renderDataTable({
    
    # outp <- dbGetQuery(con,paste0("select t.ani_etiq as ani, t.ani_sexe as s, t.cap_date as date, t.cap_poids as poids, t.cap_lpa as lpa, t.cap_age_classe as age, t.sit_nom_court as site,
    #                               t.teq_nom_court as teq, t.eqa_date_debut as debut, t.eqa_date_fin as fin, t.cap_annee_suivi as an, round(t.temps_suivi/30.43) as mois,  count(t.cpos_id) as locs, t.eqt_id_usuel as equip, t.mar_libelle as marque, t.mod_libelle as modele, t.sen_association as capteurs from (SELECT eqc_sen_id, cpos_id, ani_etiq, ani_sexe, cap_date, cap_poids, cap_lpa, cap_age_classe, sit_nom_court,
    #                               teq_nom_court, cap_annee_suivi, eqa_date_debut, eqa_date_fin, eqa_date_fin - eqa_date_debut as temps_suivi, eqt_id_usuel, mar_libelle, mod_libelle, sen_association, sen_id,eqc_annee_suivi, cpt_annee_suivi,cpt_ani_etiq
    #                               FROM public.v_aniposi_gpsgsm, public.t_equipement_conf_eqc, lu_tables.tr_sensors_sen, cmpt.t_capture_cpt ) as t where t.ani_etiq =  '",input$nAnimal2,"' and eqc_sen_id=sen_id and cpt_annee_suivi=eqc_annee_suivi and cpt_ani_etiq=ani_etiq group by t.ani_etiq, t.ani_sexe, t.cap_date, t.cap_poids, t.cap_lpa, t.cap_age_classe, t.sit_nom_court,
    #                               t.teq_nom_court, t.cap_annee_suivi, t.eqa_date_debut, t.eqa_date_fin, t.temps_suivi, t.eqt_id_usuel, t.mar_libelle, t.mod_libelle,t.sen_association order by cap_date"))
    #
    
    # ret <- DT::datatable(outp)
    # return(ret)
    
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
    
    if ((input$nAnimal)=="" & (input$estNouvelAnimal == 'oui')) {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$nAnimal2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Numéro de l'animal")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$idSite2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idSite)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nom du site")))}
    
    if ((input$identifie == 'non') & (input$idRFID)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("RFID")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$idRFID2)=="" & (input$idRFID_new)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau RFID")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idRFID2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Nouveau RFID")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idTagOrG)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'oui') & (input$idTagOrD)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idTagOrD)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'non') & (input$idTagOrG)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$idTagOrD2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Droit")))}
    
    if ((input$estNouvelAnimal == 'non') & (input$identifie == 'oui') & (input$idTagOrG2)=="") {
      checklist1 = rbind(checklist1,data.frame("VALEUR_MANQUANTE_ANIMAL"= c("Tag Gauche")))}
    
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
  
    for (i in (1:nrow(liste_prel_db))){
      temp = liste_prel_db[i,1]
      if (!(temp %in% liste_prelevement2)) {
        checklist_prel = rbind(checklist_prel,data.frame("PRELEVEMENT_MANQUANT"= c(temp)))}}
    
      #else {checklist_prel = checklist_prel }}
    

    if (nrow(checklist_prel)==0) {
      checklist_prel =  rbind(checklist_prel,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist_prel = DT::renderDT(checklist_prel) 
    
    ### Collier
    
    checklist_collier = data.frame()
    
    if (is.null(input$tablecollier_rows_selected)) {
      checklist_collier = data.frame("COLLIER_MANQUANT"= c("Pas de collier choisi"))}
    
    if (!is.null(input$tablecollier_rows_selected)) {
      checklist_collier = data.frame("PARFAIT"= c("Collier bien sélectionné"))}
    
    output$tablechecklist_collier = DT::renderDT(checklist_collier,server = F) 
    
    
    ### Bilan
    
    observeEvent(input$valid_checklist1, ignoreInit = T, {
      if ( ((checklist_prel[1][1])!="PAS DE DONNEES MANQUANTES") || ((checklist_table[1][1])!="PAS DE DONNEES MANQUANTES") || ((checklist1[1][1])!="PAS DE DONNEES MANQUANTES") || ((checklist_collier[1][1])!="PAS DE DONNEES MANQUANTES")) 
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE,  callbackR = modalCallback_check1 )}
      else      
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_check1 )}
      
    })
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
  
  updateSelectizeInput(session, "habitat", choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt order by cpt_lache_habitat_lache ASC"))
  updateSelectizeInput(session, "habitat_perte", choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt order by cpt_lache_habitat_pertevue ASC"))
  updateSelectizeInput(session, "Notation_euro", choices = dbGetQuery(con,"select (ecl_comportement_lache) from lu_tables.tr_eurodeer_comp_lache_ecl"))
  
  
  observeEvent(input$to_current_time, {
    updateTimeInput(session, "time", value = Sys.time())
  })
  
  observeEvent(input$to_current_time2, {
    updateTimeInput(session, "time2", value = Sys.time())
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
    
    if ((input$time2)=="") {
      checklist2 = rbind(checklist2,data.frame("DONNNES_LACHER_MANQUANTES" = c("Heure de 2nd lacher")))}
    
    if (nrow(checklist2)==0) {
      checklist2 =  data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES"))}

    
    output$tablechecklist2 = DT::renderDT(checklist2,server = F) 
 
    ### Bilan
    
    observeEvent(input$valid_checklist2, ignoreInit = T, {
      if  ((checklist2[1][1])!="PAS DE DONNEES MANQUANTES")
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE)}
      else      
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE)}
      
    })
  })
  
  ##################           RUBRIQUE CAPTURE                         #################
  
  updateSelectizeInput(session, "numSabot_capture", choices = dbGetQuery(con,"select distinct cap_num_sabot FROM public.t_capture_cap"))
  
  observeEvent(input$checklist_capture, { 
    
    cpt_capt=0
    
    tmp_time2= as.character(input$cpt_temps_filet)
    tmp_time2=strsplit(tmp_time2, " ")[[1]]
    tmp_time2=tmp_time2[2]
    tmp_time3= as.character(input$cpt_heure_debut_filet)
    tmp_time3=strsplit(tmp_time3, " ")[[1]]
    tmp_time3=tmp_time3[2]
    
    
    if ((input$numSabot_capture)=="")  
    {shinyalert("STOP!", "Numero Sabot manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_capt=cpt_capt+1} 
    
    if ((as.character(input$date_capture))!='2017-01-01') {
      # date_capture <<- as.character(input$date_capture)
      cpt_capt=cpt_capt+1
    }
    else {shinyalert("STOP!", "Date manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    
    if ((input$nom_capteur_txt)=="") 
    {shinyalert("STOP!", "Nom Capteur manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1}  
    
    if ((input$Nbre_pers_experimentes)==0)
    {shinyalert("STOP!", "Nbre personnes experimentees non entré", type = "warning",confirmButtonText="Valider quand même", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)}
    else {cpt_capt=cpt_capt+1}
    
    if (is.null(input$cpt_filet_vitesse)) 
    {shinyalert("STOP!", "Donnee vitesse au filet manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (is.null(input$cpt_filet_allure)) 
    {shinyalert("STOP!", "Donnee allure au filet manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (is.null(input$cpt_filet_lutte)) 
    {shinyalert("STOP!", "Donnee lutte au filet manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (is.null(input$cpt_filet_halete)) 
    {shinyalert("STOP!", "Donnee halete au filet manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (is.null(input$cpt_filet_cri)) 
    {shinyalert("STOP!", "Donnee cri au filet manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (is.na(tmp_time2)) 
    {shinyalert("STOP!", "Temps passé au filet manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (is.na(tmp_time3)) 
    {shinyalert("STOP!", "Temps début filet manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_capt=cpt_capt+1} 
    
    if (cpt_capt==11)
    {shinyalert("PARFAIT!", "Toutes les donnees rentrees", type="success",confirmButtonText="Enregistrer les données", showCancelButton=T,cancelButtonText="Annuler", callbackR = modalCallback_capture)} 
    
  })
  
  observeEvent(input$time_debut_filet, {
    updateTimeInput(session, "cpt_heure_debut_filet", value = Sys.time())
  })
  
  observeEvent(input$time_filet, {
    updateTimeInput(session, "cpt_temps_filet", value = Sys.time())
  })
  
  
  
  ##################           RUBRIQUE SABOT                           #################
  
  updateSelectizeInput(session, "cpt_dose_acepromazine", choices = dbGetQuery(con,"select distinct cpt_dose_acepromazine from cmpt.t_capture_cpt order by cpt_dose_acepromazine"))
  
  observeEvent(input$time_sabot, {
    updateTimeInput(session, "cpt_heure_mise_sabot", value = Sys.time())
  })
  
  observeEvent(input$time_fin, {
    updateTimeInput(session, "cpt_heure_fin_surv", value = Sys.time())
  })
  
  observeEvent(input$checklist_sabot, { 
    
    cpt_sabot=0
    
    if ((input$cpt_dose_acepromazine)=="")  
    {shinyalert("STOP!", "Dose acepromazine manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_sabot=cpt_sabot+1} 
    
    if ((input$Observateur)=="") 
    {shinyalert("STOP!", "Observateur manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_sabot=cpt_sabot+1}  
    
    if (is.null(input$cpt_sabot_retournement)) 
    {shinyalert("STOP!", "Donnee retournement sabot manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_sabot=cpt_sabot+1} 
    
    if (is.null(input$cpt_sabot_couche)) 
    {shinyalert("STOP!", "Donnee couche sabot manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_sabot=cpt_sabot+1} 
    
    if (is.null(input$cpt_sabot_agitation)) 
    {shinyalert("STOP!", "Donnee agitation sabot manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else {cpt_sabot=cpt_sabot+1} 
    
    if (cpt_sabot==5)
    {shinyalert("PARFAIT!", "Toutes les donnees rentrees", type="success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",callbackR = modalCallback_sabot)} 
    
    
  })
  
  
  ##################           RUBRIQUE CHECKLIST 3                     #################
  
  checklist3 = data.frame()
  row.names(checklist3) = NULL
  output$tablechecklist3 = DT::renderDT(expr = checklist3,server = F)
  
  checklist_sabot = data.frame()
  row.names(checklist_sabot) = NULL
  output$tablechecklist_sabot = DT::renderDT(expr = checklist_sabot,server = F)
  
  output$checklist_3 <- renderUI( {
    #cat(file=stderr(), "testttt2t", input$titube, "\n")
    
    checklist3 = data.frame()
    
    if ((input$numSabot_capture)=="")  {
      checklist3 = data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Numéro de sabot"))}
    
    if ((input$date_capture)=='2017-01-01')  {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Date")))}
    
    if ((input$cpt_heure_debut_filet)=="")  {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Heure début filet")))}
    
    if ((input$cpt_temps_filet)=="")  {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Temps passé au filet")))}
    
    if ((input$nom_capteur_txt)=="") {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Capteurs")))}
    
    if ((input$Nbre_pers_experimentes)=="") {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Nombre de personnes expérimentées")))}
    
    if (is.null(input$cpt_filet_vitesse)) {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Vitesse filet")))}
    
    if (is.null(input$cpt_filet_allure)) {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Allure filet")))}
    
    if (is.null(input$cpt_filet_lutte)) {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Lutte filet")))}
    
    if (is.null(input$cpt_filet_halete)) {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Halete")))}
    
    if (is.null(input$cpt_filet_cri)) {
      checklist3 = rbind(checklist3,data.frame("DONNNES_CAPTURE_MANQUANTES" = c("Cri")))}
    
    if (nrow(checklist3)==0) {
      checklist3 =  rbind(checklist3,data.frame("PARFAIT"= c("PAS DE DONNEES MANQUANTES")))}
    
    output$tablechecklist3 = DT::renderDT(checklist3,server = F)
    
    checklist_sabot = data.frame()
    
    if ((input$cpt_heure_mise_sabot)=="") {
      checklist_sabot = rbind(checklist_sabot,data.frame("DONNNES_SABOT_MANQUANTES" = c("Heure mise en sabot")))}

    if ((input$cpt_heure_fin_surv)=="") {
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
      if  ((checklist3[1][1]!="PAS DE DONNEES MANQUANTES") || (checklist_sabot[1][1]!="PAS DE DONNEES MANQUANTES")) 
      {shinyalert("ATTENTION!", "Toutes les mesures ou echantillons ne sont pas saisis", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler l'ajout",html=TRUE)}
      else      
      {shinyalert("PARFAIT!", "Toutes les mesures ont été saisies", type = "success",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE)}
      
    })
    
  })
  
  
  ##################           INTEGRATION DES DONNEES                  #################
  
  # pour obtenir le cpt_id suivant
  
  # max_value=dbGetQuery(con,paste0('SELECT cpt_id FROM cmpt.t_capture_cpt order by cpt_id desc limit 1'))
  # max_value=as.integer((max_value[1,1])+1)
  # max_valuebis= dbGetQuery(con,paste0('SELECT cpt_cap_id FROM cmpt.t_capture_cpt order by cpt_cap_id desc limit 1'))
  # max_valuebis=as.integer((max_valuebis[1,1])+1)
  
  modalCallback2 <- function(value) {
    if (value == TRUE) {
      gettime= as.character(input$time)
      gettime=strsplit(gettime, " ")[[1]]
      gettime=gettime[2]
      gettime2= as.character(input$time2)
      gettime2=strsplit(gettime2, " ")[[1]]
      gettime2=gettime2[2]
      
      if (!is.na(gettime2)) {
        dbSendQuery(con,sprintf("INSERT INTO cmpt.t_capture_cpt ( cpt_heure_lache, cpt_lache_course, cpt_lache_bolide, cpt_lache_gratte_collier, 
                                cpt_lache_tombe,cpt_lache_cabriole,cpt_lache_nbre_stop, cpt_lache_aboiement_cri,cpt_lache_titube, cpt_lache_couche,cpt_lache_visibilite,cpt_lache_habitat_lache, 
                                cpt_lache_habitat_pertevue,cpt_lache_public,cpt_lache_eurodeer,cpt_heure_second_lache,cpt_ani_etiq,cpt_date,cpt_annee_suivi,cpt_cap_id)
                                VALUES ('%s',' %s','%s', '%s','%s', '%s',%s, '%s',' %s','%s',' %s', '%s','%s',' %s',' %s','%s','%s','%s','%s','%s')", gettime,input$vitesse,input$allure,
                                input$gratte_collier,input$tombe, input$cabriole_saut, input$nbre_stops,input$cri,input$titube,input$couche,
                                input$visibilite,input$habitat,input$habitat_perte,input$nbre_personnes,input$Notation_euro,gettime2,110,Sys.Date(),format(Sys.time(), "%Y"),max_valuebis))
      }    
      else {
        dbSendQuery(con,sprintf("INSERT INTO cmpt.t_capture_cpt ( cpt_heure_lache, cpt_lache_course, cpt_lache_bolide, cpt_lache_gratte_collier, 
                                cpt_lache_tombe,cpt_lache_nbre_stop, cpt_lache_aboiement_cri,cpt_lache_titube, cpt_lache_couche,cpt_lache_visibilite,cpt_lache_habitat_lache, 
                                cpt_lache_habitat_pertevue,cpt_lache_public,cpt_lache_eurodeer,cpt_ani_etiq,cpt_date,cpt_annee_suivi,cpt_cap_id)
                                VALUES ('%s',' %s','%s', '%s', '%s',%s, '%s',' %s','%s',' %s', '%s','%s',' %s',' %s','%s','%s','%s','%s')", gettime,input$vitesse,input$allure,
                                input$gratte_collier,input$tombe,input$nbre_stops,input$cri,input$titube,input$couche,
                                input$visibilite,input$habitat,input$habitat_perte,input$nbre_personnes,input$Notation_euro,110,Sys.Date(),format(Sys.time(), "%Y"),max_valuebis))
      }
    }}
  
  modalCallback_capture <- function(value) {
    if (value == TRUE) {
      
      gettime3= as.character(input$cpt_heure_debut_filet)
      gettime3=strsplit(gettime3, " ")[[1]]
      gettime3=gettime3[2]
      gettime4= as.character(input$cpt_temps_filet)
      gettime4=strsplit(gettime4, " ")[[1]]
      gettime4=gettime4[2]    
      
      
      dbSendQuery(con,sprintf("update cmpt.t_capture_cpt SET cpt_nom_capteur = '%s' , cpt_nbre_pers_experimentes = '%s', 
                              cpt_heure_debut_filet = '%s', 
                              cpt_temps_filet = '%s', cpt_arrivee_filet_course = '%s', 
                              cpt_arrivee_filet_panique = '%s', cpt_filet_lutte = '%s', 
                              cpt_filet_haletement = '%s', cpt_filet_cri = '%s'
                              where cpt_id = 771",input$nom_capteur_txt, input$Nbre_pers_experimentes, gettime3, gettime4, input$cpt_filet_vitesse, 
                              input$cpt_filet_allure, input$cpt_filet_lutte, input$cpt_filet_halete,input$cpt_filet_cri))
    }
  }
  
  modalCallback_sabot <- function(value) {
    if (value == TRUE) {
      
      gettime5= as.character(input$cpt_heure_mise_sabot)
      gettime5=strsplit(gettime5, " ")[[1]]
      gettime5=gettime5[2]
      gettime6= as.character(input$cpt_heure_fin_surv)
      gettime6=strsplit(gettime6, " ")[[1]]
      gettime6=gettime6[2] 
      
      dbSendQuery(con,sprintf("update cmpt.t_capture_cpt SET cpt_heure_mise_sabot = '%s' , cpt_dose_acepromazine = '%s', 
                              cpt_sabot_retournement = '%s', 
                              cpt_sabot_couche = '%s', cpt_sabot_agitation = '%s', 
                              cpt_hre_fin_surv = '%s', cpt_remarque = '%s'
                              where cpt_id = 771", gettime5, input$cpt_dose_acepromazine ,input$cpt_sabot_retournement,input$cpt_sabot_couche, 
                              input$cpt_sabot_agitation,gettime6, input$Remarques))
      
      # # (cap_id from public.t_capture_cap where cap_num_sabot = '",input$numSabot_capture,"' and 
      #                             cap_date =  '",input$date_capture,"')"))
      
    }
  }
  
  idSite2=''
  idTagOrG=''
  idTagOrD=''
  
  #### Créer le csv  :
  
      modalCallback_check1 = function(value) {
        if (value == TRUE) {
          
          save1 = data.frame()
          
          if (startsWith(input$nAnimal, "F")){
            faon1 =  "oui" }
          else {faon1=""}
          
          if (startsWith(input$nAnimal2, "F")){
            faon2 =  "oui" }
          else {faon2=""}
          
          date_mod = input$date_caract
          date_mod = format(date_mod, "%d/%m/%Y")
          date_mod = as.character(date_mod)
          
          jour = strsplit(date_mod, "/")[[1]][1]
          mois = strsplit(date_mod, "/")[[1]][2]
          annee = strsplit(date_mod, "/")[[1]][3]
          
          if (input$age == '0.5') {
            cat_age_all = "jeune" 
            cat_age = "j"}
          else if (input$age=='1.5') {
            cat_age_all = "yearling"
            cat_age = "y" }
          else if (input$age=='2.5' || input$age=='3.5' || input$age=='4.5-5.5' || input$age=='>=6.5') {cat_age_all="adulte"
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
          collier_tech = liste_collier[ligne_selection,2]
          collier_col_b = liste_collier[ligne_selection,8]
          collier_col_c = liste_collier[ligne_selection,7]
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
          
          sen_association =  dbGetQuery(con,"select sen_association from lu_tables.tr_sensors_sen")
          sen_id_acc <- sen_association[grep("accelerometre",as.character(sen_association[,1])),1]
          sen_id_prox <- sen_association[grep("proximite",as.character(sen_association[,1])),1]
          sen_id_act <- sen_association[grep("activite",as.character(sen_association[,1])),1]
          
          if (!is.null(ligne_selection)) {
            collier_test = liste_collier[ligne_selection,6]
          collier_id_usuel = liste_collier[ligne_selection,4] }
          else { collier_id_usuel = ""}

          collier_tech_test=""
          collier_acc = ""
          collier_prox =""
          collier_act=""

        if (exists("collier_test")) {
            if (!is.null(ligne_selection)) {
              if (collier_test %in% sen_id_acc) {
               collier_acc = 1}}}
          
        if (exists("collier_test")) {
          if (!is.null(ligne_selection)) {
            if (collier_test %in% sen_id_prox) {
              collier_prox = 1 }}}
            
        if (exists("collier_test")) {
          if (!is.null(ligne_selection)) {                
            if (collier_test %in% sen_id_act) {
              collier_act = 1}}
          }
          
          diarrhee = paste("diarrhee/",input$diarrhee, sep="")
          bledia = paste(input$liste_blessures, diarrhee)
          
          if (!is.null(input$time2)){
            heure_totale = input$time2}
          else {heure_totale = as.integer(input$time) - as.integer(input$cpt_heure_debut_filet)}
          
          remarque_tot = paste0(input$remarques_capt, input$Remarques, input$remarques_table, input$remarques_lacher, collapse = "~")
          
          if (!is.null(input$time2)){
            remise_sabot = 1}
          else {remise_sabot = ""}
          
          if(input$nAnimal!="") {
            save1 = data.frame("N°Animal" = c(input$nAnimal))
            save1 = cbind(save1,data.frame("N°Animal telemetrie" = c(paste0(tolower(input$sexe),cat_age,"_",input$nAnimal))))
            save1 = cbind(save1,data.frame("N° bague annee capture" = c("")))
            save1 = cbind(save1,data.frame("Nombre capture" = c(1)))
            save1 = cbind(save1,data.frame("inconnue" = c("")))
            save1 = cbind(save1,data.frame("Site Capture" = c(input$idSite)))
            save1 = cbind(save1,data.frame("capture faon" = c(faon1)))
            save1 = cbind(save1,data.frame("Date" = c(input$date_caract)))
            save1 = cbind(save1,data.frame("jour" = c(jour)))
            save1 = cbind(save1,data.frame("mois" = c(mois)))
            save1 = cbind(save1,data.frame("annee" = c(annee)))
            save1 = cbind(save1,data.frame("annee  de suivi" = c(annee)))
            save1 = cbind(save1,data.frame("Sexe" = c(input$sexe)))
            save1 = cbind(save1,data.frame("Age cahier" = c(input$age)))
            save1 = cbind(save1,data.frame("Age corrige" = c(input$age)))
            save1 = cbind(save1,data.frame("categorie d'age" = c(cat_age_all)))
            save1 = cbind(save1,data.frame("etat_sante" = c(bledia)))
            save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD)))
            save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG)))
            save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d)))
            save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g)))
            save1 = cbind(save1,data.frame("cap_pertinent" = c(TRUE)))
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
            save1 = cbind(save1,data.frame(if (exists("peau")) {"Peau" = c(peau)} else {"Peau" = ""}))
            save1 = cbind(save1,data.frame(if (exists("poils")) {"poils" = c(poils)} else {"poils" = ""}))
            save1 = cbind(save1,data.frame(if (exists("sang")) {"sang" = c(sang)} else {"sang" = ""}))
            save1 = cbind(save1,data.frame(if (exists("feces")) {"feces" = c(feces)} else {"feces" = ""}))
            save1 = cbind(save1,data.frame(if (exists("tiques")) {"tiques" = c(tiques)} else {"tiques" = ""}))
            save1 = cbind(save1,data.frame(if (exists("vagin")) {"vaginal" = c(vagin)} else {"vaginal" = ""}))
            save1 = cbind(save1,data.frame(if (exists("nez")) {"Nasal" = c(nez)} else {"Nasal" = ""}))
            save1 = cbind(save1,data.frame("remarque" = c(input$remarques_prel)))
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
            save1 = cbind(save1,data.frame("probleme collier" = c(input$remarque_collier)))
            save1 = cbind(save1,data.frame("site vie" = c("")))
            save1 = cbind(save1,data.frame("secteur" = c("")))
            save1 = cbind(save1,data.frame("Mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort arrondie" = c("")))
            save1 = cbind(save1,data.frame("Cause detaille" = c("")))
            save1 = cbind(save1,data.frame("cause categories" = c("")))
            save1 = cbind(save1,data.frame("nom capteur" = c(input$nom_capteur_txt)))
            save1 = cbind(save1,data.frame("nombre d'experimentes (n)" = c(input$Nbre_pers_experimentes)))
            if (is.null(input$cpt_filet_vitesse)) { save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(input$cpt_filet_vitesse))))}
            if (is.null(input$cpt_filet_allure)) { save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(input$cpt_filet_allure))))}
            if (is.null(input$cpt_filet_lutte)) { save1 = cbind(save1,data.frame("lutte" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte" = (c(input$cpt_filet_lutte))))}
            if (is.null(input$cpt_filet_halete)) { save1 = cbind(save1,data.frame("haletement (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("haletement (1/0)" = (c(input$cpt_filet_halete))))}
            if (is.null(input$cpt_filet_cri)) { save1 = cbind(save1,data.frame("cri (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("cri (1/0)" = (c(input$cpt_filet_cri))))}
            save1 = cbind(save1,data.frame(" acepromazine (1=0,3cc)" = c(input$cpt_dose_acepromazine)))
            save1 = cbind(save1,data.frame("num_sabot" = c(input$numSabot)))
            if (is.null(input$cpt_sabot_couche)) { save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("couche (1/0)" = (c(input$cpt_sabot_couche))))}
            if (is.null(input$cpt_sabot_agitation)) { save1 = cbind(save1,data.frame("agitation (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("agitation (1/0)" = (c(input$cpt_sabot_agitation))))}
            if (is.null(input$cpt_sabot_retournement)) { save1 = cbind(save1,data.frame("retournement (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("retournement (1/0)" = (c(input$cpt_sabot_retournement))))}
            save1 = cbind(save1,data.frame("hre fin surv" = c(input$cpt_heure_fin_surv)))
            save1 = cbind(save1,data.frame("surveillance (mn)" = c(as.integer(input$cpt_heure_fin_surv) - as.integer(input$cpt_heure_mise_sabot))))
            save1 = cbind(save1,data.frame("distance (KM)" = c("")))
            if (is.null(input$lutte)) { save1 = cbind(save1,data.frame("lutte (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte (1/0)" = (c(input$lutte))))}
            if (is.null(input$halete)) { save1 = cbind(save1,data.frame("halete (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("halete (1/0)" = (c(input$halete))))}
            save1 = cbind(save1,data.frame("cri" = c(cri_total)))
            save1 = cbind(save1,data.frame("T°C 1" = c("")))
            save1 = cbind(save1,data.frame("T°C 2" = c("")))
            save1 = cbind(save1,data.frame("Cœur 1" = c("")))
            save1 = cbind(save1,data.frame("Cœur 2" = c("")))
            save1 = cbind(save1,data.frame("localisation sonde temperature" = c("")))
            save1 = cbind(save1,data.frame("eurodeer" = c(input$Notation_euro_table)))
            if (is.null(input$titube)) { save1 = cbind(save1,data.frame("titube (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("titube (1/0)" = (c(input$titube))))}
            if (is.null(input$couche)) { save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("couche (1/0)" = (c(input$couche))))}
            if (is.null(input$vitesse)) { save1 = cbind(save1,data.frame("course (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("course (1/0)" = (c(input$vitesse))))}
            if (is.null(input$tombe)) { save1 = cbind(save1,data.frame("tombe (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("tombe (1/0)"  = (c(input$tombe))))}
            if (is.null(input$gratte_collier)) { save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(input$gratte_collier))))}
            if (is.null(input$cabriole_saut)) { save1 = cbind(save1,data.frame("cabriole (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("cabriole (1/0)" = (c(input$cabriole_saut))))}
            if (is.null(input$allure)) { save1 = cbind(save1,data.frame("bolide (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("bolide (1/0)" = (c(input$allure))))}
            if (is.null(input$cri)) { save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c(input$cri))))}
            
            save1 = cbind(save1,data.frame("filet" = c(input$cpt_temps_filet)))
            save1 = cbind(save1,data.frame("sabot sur place" = c("")))
            save1 = cbind(save1,data.frame("transport+attente" = c("")))
            save1 = cbind(save1,data.frame("marquage" = c(as.integer(input$time_table) - as.integer(input$time_caract))))
            save1 = cbind(save1,data.frame("total" = c(heure_totale)))
            save1 = cbind(save1,data.frame("capture" = c(input$cpt_heure_debut_filet)))
            save1 = cbind(save1,data.frame("sabot" = c(input$cpt_heure_mise_sabot)))
            save1 = cbind(save1,data.frame("acepro" = c(input$cpt_heure_mise_sabot)))
            save1 = cbind(save1,data.frame("transport" = c("")))
            save1 = cbind(save1,data.frame("table" = c(input$time_caract)))
            save1 = cbind(save1,data.frame("lache" = c(input$time)))
            save1 = cbind(save1,data.frame("remarque_lacher" = c(remarque_tot)))
            if (is.null(input$cribague)) { save1 = cbind(save1,data.frame("bague" = (c(""))))} else {save1 = cbind(save1,data.frame("bague" = (c(input$cribague))))}
            if (is.null(input$criautre)) { save1 = cbind(save1,data.frame("autre" = (c(""))))} else {save1 = cbind(save1,data.frame("autre" = (c(input$criautre))))}
            
            save1 = cbind(save1,data.frame("stop" = c(input$nbre_stops)))
            save1 = cbind(save1,data.frame("habitat lacher" = c(input$habitat)))
            save1 = cbind(save1,data.frame("habite perte vue" = c(input$habitat_perte)))
            save1 = cbind(save1,data.frame("visibilite" = c(input$visibilite),options(stringsAsFactors = F)))
            save1 = cbind(save1,data.frame("nb_public" = c(input$nbre_personnes),options(stringsAsFactors = F)))
            save1 = cbind(save1,data.frame("eurodeer_lacher" = c(input$Notation_euro)))
            save1 = cbind(save1,data.frame("remise sabot" = c("")))
            save1 = cbind(save1,data.frame("heure_lacher_2" = c(input$time2)))
          }
          
          if(input$nAnimal2!="") {
            save1 = data.frame("N°Animal" = c(input$nAnimal2))
            save1 = cbind(save1,data.frame("N°Animal telemetrie" = c(paste0(tolower(input$sexe),cat_age,"_",input$nAnimal2))))
            save1 = cbind(save1,data.frame("N° bague annee capture" = c("")))
            save1 = cbind(save1,data.frame("Nombre capture" = c(nbre_capt)))
            save1 = cbind(save1,data.frame("inconnue" = c("")))
            save1 = cbind(save1,data.frame("Site Capture" = c(input$idSite2)))
            save1 = cbind(save1,data.frame("capture faon" = c(faon2)))
            save1 = cbind(save1,data.frame("Date" = c(input$date_caract)))
            save1 = cbind(save1,data.frame("jour" = c(jour)))
            save1 = cbind(save1,data.frame("mois" = c(mois)))
            save1 = cbind(save1,data.frame("annee" = c(annee)))
            save1 = cbind(save1,data.frame("annee  de suivi" = c(annee)))
            save1 = cbind(save1,data.frame("Sexe" = c(input$sexe)))
            save1 = cbind(save1,data.frame("Age cahier" = c(input$age)))
            save1 = cbind(save1,data.frame("Age corrige" = c(input$age)))
            save1 = cbind(save1,data.frame("categorie d'age" = c(cat_age_all)))
            save1 = cbind(save1,data.frame("etat_sante" = c(bledia)))
            save1 = cbind(save1,data.frame("cap_tag_droit" = c(input$idTagOrD2)))
            save1 = cbind(save1,data.frame("cap_tag_gauche" = c(input$idTagOrG2)))
            save1 = cbind(save1,data.frame("cap_tag_droit_metal" = c(input$metal_tag_d2)))
            save1 = cbind(save1,data.frame("cap_tag_gauche_metal" = c(input$metal_tag_g2)))
            save1 = cbind(save1,data.frame("cap_pertinent" = c(cap_pertinent)))
            if (input$idRFID2!=""){
              save1 = cbind(save1,data.frame("RFID" = c(input$idRFID2)))}
            if (input$idRFID_new!=""){
              save1 = cbind(save1,data.frame("RFID" = c(input$idRFID_new)))}
            save1 = cbind(save1,data.frame("Poids" = c(input$pSabotPlein - input$pSabotVide)))
            save1 = cbind(save1,data.frame("Cir Cou" = c(input$cirCou)))
            save1 = cbind(save1,data.frame("Long patte Ar" = c(input$lPattArriere)))
            save1 = cbind(save1,data.frame("machoire" = c("")))
            save1 = cbind(save1,data.frame("long bois gauche" = c(input$lBoisGauche)))
            save1 = cbind(save1,data.frame("long bois droit" = c(input$lBoisDroit)))
            save1 = cbind(save1,data.frame("glucose" = c(input$tglucose)))
            save1 = cbind(save1,data.frame("T°C_ext" = c("")))
            save1 = cbind(save1,data.frame("TIQUES FIXES" = c(input$tiques)))
            save1 = cbind(save1,data.frame(if (exists("peau")) {"Peau" = c(peau)} else {"Peau" = ""}))
            save1 = cbind(save1,data.frame(if (exists("poils")) {"poils" = c(poils)} else {"poils" = ""}))
            save1 = cbind(save1,data.frame(if (exists("sang")) {"sang" = c(sang)} else {"sang" = ""}))
            save1 = cbind(save1,data.frame(if (exists("feces")) {"feces" = c(feces)} else {"feces" = ""}))
            save1 = cbind(save1,data.frame(if (exists("tiques")) {"tiques" = c(tiques)} else {"tiques" = ""}))
            save1 = cbind(save1,data.frame(if (exists("vagin")) {"vaginal" = c(vagin)} else {"vaginal" = ""}))
            save1 = cbind(save1,data.frame(if (exists("nez")) {"Nasal" = c(nez)} else {"Nasal" = ""}))
            save1 = cbind(save1,data.frame("remarque" = c(input$remarques_prel)))
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
            save1 = cbind(save1,data.frame("probleme collier" = c(input$remarque_collier)))
            save1 = cbind(save1,data.frame("site vie" = c("")))
            save1 = cbind(save1,data.frame("secteur" = c("")))
            save1 = cbind(save1,data.frame("Mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort" = c("")))
            save1 = cbind(save1,data.frame("Date mort arrondie" = c("")))
            save1 = cbind(save1,data.frame("Cause detaille" = c("")))
            save1 = cbind(save1,data.frame("cause categories" = c("")))
            save1 = cbind(save1,data.frame("nom capteur" = c(input$nom_capteur_txt)))
            save1 = cbind(save1,data.frame("nombre d'experimentes (n)" = c(input$Nbre_pers_experimentes)))
            if (is.null(input$cpt_filet_vitesse)) { save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("arrivee filet course (1/0)" = (c(input$cpt_filet_vitesse))))}
            if (is.null(input$cpt_filet_allure)) { save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("arrivee filet panique (1/0)" = (c(input$cpt_filet_allure))))}
            if (is.null(input$cpt_filet_lutte)) { save1 = cbind(save1,data.frame("lutte" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte" = (c(input$cpt_filet_lutte))))}
            if (is.null(input$cpt_filet_halete)) { save1 = cbind(save1,data.frame("haletement (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("haletement (1/0)" = (c(input$cpt_filet_halete))))}
            if (is.null(input$cpt_filet_cri)) { save1 = cbind(save1,data.frame("cri (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("cri (1/0)" = (c(input$cpt_filet_cri))))}
            save1 = cbind(save1,data.frame(" acepromazine (1=0,3cc)" = c(input$cpt_dose_acepromazine)))
            save1 = cbind(save1,data.frame("num_sabot" = c(input$numSabot)))
            if (is.null(input$cpt_sabot_couche)) { save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("couche (1/0)" = (c(input$cpt_sabot_couche))))}
            if (is.null(input$cpt_sabot_agitation)) { save1 = cbind(save1,data.frame("agitation (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("agitation (1/0)" = (c(input$cpt_sabot_agitation))))}
            if (is.null(input$cpt_sabot_retournement)) { save1 = cbind(save1,data.frame("retournement (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("retournement (1/0)" = (c(input$cpt_sabot_retournement))))}
            save1 = cbind(save1,data.frame("hre fin surv" = c(input$cpt_heure_fin_surv)))
            save1 = cbind(save1,data.frame("surveillance (mn)" = c(as.integer(input$cpt_heure_fin_surv) - as.integer(input$cpt_heure_mise_sabot))))
            save1 = cbind(save1,data.frame("distance (KM)" = c("")))
            if (is.null(input$lutte)) { save1 = cbind(save1,data.frame("lutte (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("lutte (1/0)" = (c(input$lutte))))}
            if (is.null(input$halete)) { save1 = cbind(save1,data.frame("halete (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("halete (1/0)" = (c(input$halete))))}
            save1 = cbind(save1,data.frame("cri" = c(cri_total)))
            save1 = cbind(save1,data.frame("T°C 1" = c("")))
            save1 = cbind(save1,data.frame("T°C 2" = c("")))
            save1 = cbind(save1,data.frame("Cœur 1" = c("")))
            save1 = cbind(save1,data.frame("Cœur 2" = c("")))
            save1 = cbind(save1,data.frame("localisation sonde temperature" = c("")))
            save1 = cbind(save1,data.frame("eurodeer" = c(input$Notation_euro_table)))
            if (is.null(input$titube)) { save1 = cbind(save1,data.frame("titube (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("titube (1/0)" = (c(input$titube))))}
            if (is.null(input$couche)) { save1 = cbind(save1,data.frame("couche (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("couche (1/0)" = (c(input$couche))))}
            if (is.null(input$vitesse)) { save1 = cbind(save1,data.frame("course (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("course (1/0)" = (c(input$vitesse))))}
            if (is.null(input$tombe)) { save1 = cbind(save1,data.frame("tombe (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("tombe (1/0)"  = (c(input$tombe))))}
            if (is.null(input$gratte_collier)) { save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("gratte collier (1/0)" = (c(input$gratte_collier))))}
            if (is.null(input$cabriole_saut)) { save1 = cbind(save1,data.frame("cabriole (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("cabriole (1/0)" = (c(input$cabriole_saut))))}
            if (is.null(input$allure)) { save1 = cbind(save1,data.frame("bolide (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("bolide (1/0)" = (c(input$allure))))}
            if (is.null(input$cri)) { save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c(""))))} else {save1 = cbind(save1,data.frame("aboiement/cri (1/0)" = (c(input$cri))))}

            save1 = cbind(save1,data.frame("filet" = c(input$cpt_temps_filet)))
            save1 = cbind(save1,data.frame("sabot sur place" = c("")))
            save1 = cbind(save1,data.frame("transport+attente" = c("")))
            save1 = cbind(save1,data.frame("marquage" = c(as.integer(input$time_table) - as.integer(input$time_caract))))
            save1 = cbind(save1,data.frame("total" = c(heure_totale)))
            save1 = cbind(save1,data.frame("capture" = c(input$cpt_heure_debut_filet)))
            save1 = cbind(save1,data.frame("sabot" = c(input$cpt_heure_mise_sabot)))
            save1 = cbind(save1,data.frame("acepro" = c(input$cpt_heure_mise_sabot)))
            save1 = cbind(save1,data.frame("transport" = c("")))
            save1 = cbind(save1,data.frame("table" = c(input$time_caract)))
            save1 = cbind(save1,data.frame("lache" = c(input$time)))
            save1 = cbind(save1,data.frame("remarque_lacher" = c(remarque_tot)))
            if (is.null(input$cribague)) { save1 = cbind(save1,data.frame("bague" = (c(""))))} else {save1 = cbind(save1,data.frame("bague" = c(input$cribague)) )}
            if (is.null(input$criautre)) { save1 = cbind(save1,data.frame("autre" = (c(""))))} else {save1 = cbind(save1,data.frame("autre" = c(input$criautre)))}

            save1 = cbind(save1,data.frame("stop" = c(input$nbre_stops)))
            save1 = cbind(save1,data.frame("habitat lacher" = c(input$habitat)))
            save1 = cbind(save1,data.frame("habite perte vue" = c(input$habitat_perte)))
            save1 = cbind(save1,data.frame("visibilite" = c(input$visibilite)))
            save1 = cbind(save1,data.frame("nb_public" = c(input$nbre_personnes)))
            save1 = cbind(save1,data.frame("eurodeer_lacher" = c(input$Notation_euro)))
            save1 = cbind(save1,data.frame("remise sabot" = c(remise_sabot)))
            save1 = cbind(save1,data.frame("heure_lacher_2" = c(input$time2)))
            
          }

          write.table(x = save1, file = paste0("captures_",gsub("-","_",Sys.Date()), ".csv"), append=T, row.names=F, col.names=!file.exists(paste0("captures_",gsub("-","_",Sys.Date()), ".csv")), sep=";", na="")
          
          shinyjs::js$refresh()
          
  
  ##################           BASE DE DONNEES                          #################
  
    date_mod = input$date_caract
    date_mod = format(date_mod, "%d/%m/%Y")
    date_mod = as.character(date_mod)
    annee = strsplit(date_mod, "/")[[1]][3]
    
    if (startsWith(input$nAnimal, "F")){
      faon =  TRUE }
    else {faon= FALSE}
    
    if (input$age == '<1' || input$age == '1' ) {
      cat_age_all = "jeune" }
    else if (input$age=='1-2' || input$age=='2') {
      cat_age_all = "yearling"}
    else if (input$age=='2-4' || input$age=='3' || input$age=='4.5' || input$age=='4-6' || input$age=='6' || input$age=='>=6') {cat_age_all="adulte"}
    else {cat_age_all="" }
    
    if (input$nAnimal2!="") {
    cap_pertinent = dbGetQuery(con,paste0("select cap_annee_suivi from public.t_capture_cap, public.t_animal_ani where cap_ani_id=ani_id and ani_etiq = '",input$nAnimal2,"' order by cap_annee_suivi DESC"))
    cap_pertinent <- as.character(cap_pertinent[1,1])
    if (annee == cap_pertinent) {cap_pertinent = FALSE} else {cap_pertinent = TRUE} }
    
    gettime= as.character(Sys.time())
    gettime=strsplit(gettime, " ")[[1]]
    gettime=gettime[2]

#### Nouvel animal ####
    
  if (input$estNouvelAnimal == 'oui') {
    dbSendQuery(con,sprintf("INSERT INTO public.t_animal_ani( ani_etiq, ani_sexe, ani_remarque ) values ('%s', '%s', '%s')", input$nAnimal, input$sexe, input$remarque_ani))
    
    find_ani_id = dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal,"'"))
    find_ani_id <- find_ani_id[1,1]
    find_site_id = dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite,"'"))
    find_site_id <- find_site_id[1,1]
    
    dbSendQuery(con,sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe,
                            cap_poids, cap_circou, cap_lpa, cap_etat_sante cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal,
                            cap_tag_gauche_metal) values ('%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                            find_ani_id, find_site_id, "test", as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, (input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
                            bledia ,gettime, TRUE, input$numSabot, input$idTagOrD, input$idTagOrG, input$metal_tag_d, input$metal_tag_g))

    find_cap_id = dbGetQuery(con, paste0("select cap_id from public.t_capture_cap where cap_ani_id= '",find_ani_id,"' order by cap_id DESC"))
    find_cap_id <- find_cap_id[1,1]
    
    ## Faire une boucle sur le tableau des blessures 
    
    dbSendQuery(con, sprintf("INSERT INTO public.t_blessure_capture_blc (blc_cap_id, blc_bll_id, blc_blg_id, blc_blt_id, blc_remarque) values ('%s', '%s','%s', '%s', '%s')",
                             find_cap_id ))  }
    
#### Ancien animal  ####
    
  else if (input$estNouvelAnimal == 'non' && input$identifie == 'non') {
    dbSendQuery(con,sprintf("INSERT INTO public.t_animal_ani( ani_etiq, ani_sexe, ani_remarque ) values ('%s', '%s', '%s')", input$nAnimal, input$sexe, input$remarque_ani))
    
    find_ani_id = dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal,"'"))
    find_ani_id <- find_ani_id[1,1]
    find_site_id = dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '",input$idSite,"'"))
    find_site_id <- find_site_id[1,1]
    
    if (!is.null(ligne_selection)) { cap_pertinent2 = TRUE} else { cap_pertinent2 = FALSE}
    
    dbSendQuery(con,sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe,
                            cap_poids, cap_circou, cap_lpa, cap_etat_sante,cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal,
                            cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                            find_ani_id, find_site_id, "test", as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, (input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
                            bledia,gettime, cap_pertinent2, input$numSabot, input$idTagOrD, input$idTagOrG, input$metal_tag_d, input$metal_tag_g))
      
    dbSendQuery(con, sprintf("INSERT INTO public.t_correspondance_animal_cor(cor_ancien, cor_valide) values ('%s','%s')", input$nAnimal, input$nAnimal))}

#### Ancien animal mais sans identifiant ####
    
  else if (input$estNouvelAnimal == 'non' && input$identifie == 'oui') {
    
    find_ani_id2 = dbGetQuery(con, paste0("select ani_id from public.t_animal_ani where ani_etiq= '",input$nAnimal2,"'"))
    find_ani_id2 <- find_ani_id2[1,1]
    find_site_id2 = dbGetQuery(con, paste0("select sit_id from public.tr_site_capture_sit where sit_nom_court= '", input$idSite2,"'"))
    find_site_id2 <- find_site_id2[1,1]
    
    dbSendQuery(con,sprintf("INSERT INTO public.t_capture_cap(cap_ani_id, cap_sit_id, cap_bague, cap_date, cap_annee_suivi, cap_faon, cap_age, cap_age_corrige, cap_age_classe,
                            cap_poids, cap_circou, cap_lpa, cap_etat_sante, cap_heure_lacher, cap_pertinent, cap_num_sabot, cap_tag_droit, cap_tag_gauche, cap_tag_droit_metal,
                            cap_tag_gauche_metal) values ('%s','%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s','%s', '%s', '%s', '%s', '%s', '%s', '%s')",
                            find_ani_id2, find_site_id2, "test", as.character(input$date_caract), annee, faon, input$age, input$age, cat_age_all, (input$pSabotPlein - input$pSabotVide), input$cirCou, input$lPattArriere,
                            bledia,gettime, cap_pertinent, input$numSabot, input$idTagOrD, input$idTagOrG, input$metal_tag_d, input$metal_tag_g))}
      
    }}
  
}
