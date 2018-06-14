source("connect.R")

##################                  SERVER                 ################# 

## Dataframe pour les prélevements :

df_prelevement <- data.frame(dbGetQuery(con, "select sat_type from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sat_id"), 
                              dbGetQuery(con, "select sal_localisation from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sat_id"),
                              dbGetQuery(con, "select sac_conditionnement from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sat_id"))
                              
colnames(df_prelevement)<-c("prel_type","prel_local","prel_condi")
 
option4 <- dbGetQuery(con,"select distinct (sas_solvant) from lu_tables.tr_samples_solvant_sas, lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac where sas_sat_id=sat_id and sas_sac_id=sac_id")

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
  updateSelectizeInput(session, "idTagOrG2", choices = dbGetQuery(con, "select distinct cap_tag_gauche from public.t_capture_cap"))
  updateSelectizeInput(session, "idTagOrD2", choices = dbGetQuery(con, "select distinct cap_tag_droit from public.t_capture_cap"))
  updateSelectizeInput(session, "idRFID2", choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id"))
  updateSelectizeInput(session, "idSite2", choices = dbGetQuery(con, "select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap, t_animal_ani))"))
  updateSelectizeInput(session, "idRFID_new", choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")) 
  updateSelectizeInput(session, "dents", choices = dbGetQuery(con,"select dent_valeur from lu_tables.tr_denture_dent")) 
  updateSelectizeInput(session, "numSabot", choices = dbGetQuery(con,"select sab_valeur from lu_tables.tr_sabots_sab order by sab_id")) 
  
  
    #########          Sélection site/RFID/tag à partir du n°animal                   #########
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_gauche from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagOrG2 <<- resres[1,1]
      updateSelectizeInput(session, "idTagOrG2",  selected = (idTagOrG2))
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_droit from public.t_capture_cap, t_animal_ani where cap_ani_id = ani_id and  ani_etiq = '", input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagOrD2 <<- resres[1,1]
      updateSelectizeInput(session, "idTagOrD2", selected = idTagOrD2)
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id and ani_etiq='",input$nAnimal2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      idTagRfid <<- resres[1, 1]
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
      idSite2 <<- resres[1, 1]
      updateSelectizeInput(session, "idSite2", selected = idSite2)
    }
  })
  
  observeEvent(input$nAnimal2, {
    if ((input$nAnimal2)!="") {
      str = paste0("select ani_sexe from public.t_animal_ani where ani_etiq ='", input$nAnimal2, "'")
      resres = dbGetQuery(con,str)
      sexe <<- resres[1, 1]
      updateRadioButtons(session, "sexe", selected = sexe)
    }
  })
  
  testNouvelAnimal = observeEvent(input$estNouvelAnimal, {
    if (input$estNouvelAnimal=="non"){
      updateRadioButtons(session, "identifié", choices = c("oui","non"), selected = "oui")
    }
    if (input$estNouvelAnimal=="oui"){
      updateRadioButtons(session, "identifié", choices = c("oui","non"), selected = "non")
    }  
  })
  
    #########          Sélection nAnimal/RFID/site/tagG à partir du tagD              #########
  
  observeEvent(input$idTagOrD2,{
    if ((input$idTagOrD2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap where cap_ani_id = ani_id and cap_tag_droit ='", input$idTagOrD2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      nAnimalFound <<- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
    #########          Sélection nAnimal/RFID/site/tagD à partir du tagG              #########
  
  observeEvent(input$idTagOrG2,{
    if ((input$idTagOrG2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap where cap_ani_id = ani_id and cap_tag_gauche ='", input$idTagOrG2,"' order by cap_date DESC")
      resres = dbGetQuery(con,str)
      nAnimalFound <<- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
    #########          Sélection nAnimal/site/tagD/tagG à partir du RFID              #########
  
  observeEvent(input$idRFID2,{
    if ((input$idRFID2)!="") {
      str = paste0("select ani_etiq from public.t_animal_ani, public.t_capture_cap, public.t_rfid_rfi where cap_id = rfi_cap_id and cap_ani_id = ani_id and rfi_tag_code ='", input$idRFID2,"'")
      resres = dbGetQuery(con,str)
      nAnimalFound <<- resres[1,1]
      updateSelectizeInput(session, "nAnimal2", selected = nAnimalFound)
    }
  })
  
    #########          Vérification des tags  (metal ou non)                          ######### 
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_droit_metal from public.t_capture_cap, public.t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"'")
      resres = dbGetQuery(con,str)
      tag_droit_metal <<- resres[1,1]
      updateCheckboxInput(session, "metal_tag_d2", value = tag_droit_metal)
    }
  })
  
  observeEvent(input$nAnimal2,{
    if ((input$nAnimal2)!="") {
      str = paste0("select cap_tag_gauche_metal from public.t_capture_cap, public.t_animal_ani where cap_ani_id = ani_id and  ani_etiq ='", input$nAnimal2,"'")
      resres = dbGetQuery(con,str)
      tag_gauche_metal <<- resres[1,1]
      updateCheckboxInput(session, "metal_tag_g2", value = tag_gauche_metal)
    }
  })

listTagD = dbGetQuery(con,"select distinct cap_tag_droit from public.t_capture_cap")
names(listTagD)<-c("nom")
listTagG = dbGetQuery(con,"select distinct cap_tag_gauche from public.t_capture_cap")
names(listTagG)<-c("nom")
listTag = rbind(listTagD,listTagG)

  output$tagDroitExiste <- renderUI({
    if (!is.null(input$idTagOrD)) {
      for (i in listTag) {
        if (input$idTagOrD %in% i)
        {shinyalert("TAG DROIT DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
      }
    }
  })
  
  output$tagGaucheExiste <- renderUI({
    if (!is.null(input$idTagOrG)) {
      for (i in listTag) {
        if (input$idTagOrG %in% i)
        {shinyalert("TAG GAUCHE DEJA EXISTANT!", "Vérifier le numéro du tag", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
      }
    }
  })
      
    #########          Vérification existence numéro nouvel animal                    ##########   
    
listAnimal = dbGetQuery(con,"select distinct ani_etiq from public.t_animal_ani")
  
  output$animalExiste <- renderUI({
    if (!is.null(input$nAnimal)) {
      for (i in listAnimal) {
        if (input$nAnimal %in% i)
        {shinyalert("ANIMAL DEJA EXISTANT!", "Décocher '1ere capture' ou choisir un autre 'ani_etiq'", type = "warning", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
      }
    }
  })
  
  
    #########          Date capture                                                   ##########   
 
   output$bla <- renderUI ({
    print("ba")
    print(input$date_capture)
  })
  
    #########          Test données: poids, num sabot , tour de cou, lg patte, bois   ######### 
  
  output$poids_ani = renderText({input$pSabotPlein-input$pSabotVide})
  
  output$alert_poids <- renderUI({
    if (!is.na(input$pSabotPlein) && !is.na(input$pSabotVide)) {
      if ((input$pSabotPlein-input$pSabotVide)>40) {
        shinyalert("STOP!", "Poids supérieur à 40kgs!", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
      }} })
  
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
  
  output$out_cirCou <- renderUI({
    if (input$cirCou > dbGetQuery(con,"select max(cap_circou) from public.t_capture_cap")) {
      shinyalert("STOP!", "Circonference élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_circou)
    }})
  
  modalCallback_circou <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "cirCou" , value = 0)}}
  
  output$out_lPattArriere <- renderUI({
    if (input$lPattArriere > dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")) {
      shinyalert("STOP!", "Longueur patte élevée", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE, callbackR = modalCallback_lg_patte)
    }})

  modalCallback_lg_patte <- function(value) {
    if (value == FALSE) {
      updateNumericInput(session, "lPattArriere" , value = 0)}}
  
  output$out_lBoisGauche <- renderUI({
    if (input$lBoisGauche > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
      shinyalert("STOP!", "Longueur bois gauche elevee", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
    }})

  output$out_lBoisDroit <- renderUI({
    if (input$lBoisDroit > dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")) {
      shinyalert("STOP!", "Longueur bois droit elevee", type = "warning",confirmButtonText="Valider", showCancelButton=T,cancelButtonText="Annuler",html=TRUE )
    }})
  
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

    #########          Récupération de l'heure                                        #########    
 
   observeEvent(input$to_current_time_caract, {
    updateTimeInput(session, "time_caract", value = Sys.time())
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
        list_ble = paste(u,list_ble,sep=" ")
      }
      blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(list_ble), "Liste" = paste0(c(input$locali)," - ",c(input$grave), " - ",c(list_ble))))
      updateSelectizeInput(session,"locali", options=list(selected=NULL))
      updateSelectizeInput(session,"traitement", options=list(selected=NULL))
    }
      
    if ((length(input$traitement))==1)
      {
        blessure <<- rbind(blessure,data.frame("Localisation" = c(input$locali), "Gravite" =c(input$grave), "Traitement" = c(input$traitement), "Liste" = paste0(c(input$locali)," - ",c(input$grave), " - ",c(input$traitement))))
    }
    
    output$tableblessure = DT::renderDT(blessure,server = F)
    #print(blessure[1][1])
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
  
  updateSelectizeInput(session,"traitement", choices = (dbGetQuery(con,"select blt_traitement from lu_tables.tr_blessure_traitement_blt")))
  
  
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
    prelevement <<- rbind(prelevement, data.frame("Type" = c(input$typetype), "Localisation" =c(input$localoca), "Contenant" = c(input$concon),"Solvant" = c(input$solsol),"Nombre d'echantillons" = c(input$nbre_echant)))
    output$tableprelevement = DT::renderDT(prelevement,server = F)
    updateSelectizeInput(session,"typetype", options=list(selected=NULL))
    updateSelectizeInput(session,"solsol", options=list(selected=NULL))
  })
  
  ### Mise en forme des prélevements en cascade :
  
  output$table_prel <- renderTable({df_prelevement})
  
  
    output$control1 <- renderUI({
    selectizeInput("typetype", h4("Type"), choices = df_prelevement$prel_type,options=list(placeholder='Choisir une valeur :',create= TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)

    })
  
  output$control2 <- renderUI({
    x <- input$typetype
    if (any(
      is.null(x)
    ))
      return("Select")
    choice2 <- df_prelevement[df_prelevement$prel_type == x,  "prel_local"]
    selectizeInput("localoca", h4("Localisation"), choices = choice2, options=list(create= TRUE))
    
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
    selectizeInput("concon", h4("Conditionnement"), choices = choice3, options=list(create= TRUE))
    
  })
  
   output$control4 <- renderUI({
  #   x <- input$typetype
  #   y <- input$localoca
  #   z <- input$concon
  #   if (any(
  #     is.null(x),
  #     is.null(y),
  #     is.null(z)
  #   ))
  #     return("Select")
  # 
  #   choice4 <- df_prelevement[df_prelevement$a == x & df_prelevement$b == y & df_prelevement$c == z, "d"]
     selectizeInput("solsol", h4("Solvant"), choices = option4, options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'), create= TRUE), selected = NULL)
   })
  
  
  ##################           RUBRIQUE TABLE                           #################
  
  updateSelectizeInput(session, "Notation_euro_table", choices = dbGetQuery(con,"select (ect_comportement) from lu_tables.tr_eurodeer_comp_table_ect"))
  
  observeEvent(input$to_current_time_table, {
    updateTimeInput(session, "time_table", value = Sys.time())
  })
  
  ##################           RUBRIQUE HISTORIQUE                      #################
  
  output$historique <- DT::renderDataTable({
    outp <- dbGetQuery(con,paste0("select t.ani_etiq as ani, t.ani_sexe as s, t.cap_date as date, t.cap_poids as poids, t.cap_lpa as lpa, t.cap_age_classe as age, t.sit_nom_court as site, 
                                  t.teq_nom_court as teq, t.eqa_date_debut as debut, t.eqa_date_fin as fin, t.cap_annee_suivi as an, round(t.temps_suivi/30.43) as mois,  count(t.cpos_id) as locs, t.eqt_id_usuel as equip, t.mar_libelle as marque, t.mod_libelle as modele, t.sen_association as capteurs from (SELECT eqc_sen_id, cpos_id, ani_etiq, ani_sexe, cap_date, cap_poids, cap_lpa, cap_age_classe, sit_nom_court, 
                                  teq_nom_court, cap_annee_suivi, eqa_date_debut, eqa_date_fin, eqa_date_fin - eqa_date_debut as temps_suivi, eqt_id_usuel, mar_libelle, mod_libelle, sen_association, sen_id,eqc_annee_suivi
                                  FROM public.v_aniposi_gpsgsm, public.t_equipement_conf_eqc, lu_tables.tr_sensors_sen ) as t where t.ani_etiq = '",input$nAnimal2,"' and eqc_sen_id=sen_id and t.cap_annee_suivi=eqc_annee_suivi group by t.ani_etiq, t.ani_sexe, t.cap_date, t.cap_poids, t.cap_lpa, t.cap_age_classe, t.sit_nom_court, 
                                  t.teq_nom_court, t.cap_annee_suivi, t.eqa_date_debut, t.eqa_date_fin, t.temps_suivi, t.eqt_id_usuel, t.mar_libelle, t.mod_libelle,t.sen_association order by cap_date"))
    
    ret <- DT::datatable(outp)
    return(ret)
  })
  
  ##################           RUBRIQUE CHECKLIST 1                     #################
  
  ###### Animal  
  
  checklist1 = data.frame()
  row.names(checklist1) = NULL
  output$tablechecklist1 = DT::renderDT(expr = checklist1,server = F)
  
  observeEvent(input$checklist_1, { 
    
    if ((input$numSabot)!=0) {
      checklist1 <<- data.frame("Numero Sabot" = input$numSabot)}
    else {checklist1 <<- data.frame("Numero Sabot"= c("NULL"))}
    
    if (!is.na(input$nAnimal) & (input$estNouvelAnimal == 1)) {
      checklist1 <<- cbind(checklist1,data.frame("N°Animal" = input$nAnimal))}
    else if (is.na(input$nAnimal) & (input$estNouvelAnimal == 1)) {checklist1 <<- cbind(checklist1,data.frame("N°Animal"= c("NULL")))}
    
    if ((input$estNouvelAnimal == 0) & !is.na(input$nAnimal2)) {
      checklist1 <<- cbind(checklist1,data.frame("N°Animal" = input$nAnimal2))}
    else if ((input$estNouvelAnimal == 0) & is.na(input$nAnimal2)){checklist1 <<- cbind(checklist1,data.frame("n°Animal"= c("NULL")))}
    
    if ((input$estNouvelAnimal == 0) & (idSite2)!="") {
      checklist1 <<- cbind(checklist1,data.frame("Site" = idSite2))}
    else if ((idSite2=="") & (input$estNouvelAnimal==0)){checklist1 <<- cbind(checklist1,data.frame("Site"= c("NULL")))}
    
    if ((input$estNouvelAnimal == 1) & (input$idSite)!="") {
      checklist1 <<- cbind(checklist1,data.frame("Site" = input$idSite))}
    else if ((input$idSite)=="" & (input$estNouvelAnimal==1)){checklist1 <<- cbind(checklist1,data.frame("Site"= c("NULL")))}
    
    # if ((input$idRFID)!="") {
    #   checklist1 <<- cbind(checklist1,data.frame("idRFID" = input$idRFID))}
    # else {checklist1 <<- cbind(checklist1,data.frame("idRFID"= c("NULL")))}
    
    if ((input$idTagOrG)!="0" & input$estNouvelAnimal == 1) {
      checklist1 <<- cbind(checklist1,data.frame("Tag Gauche" = input$idTagOrG))}
    else if ((input$idTagOrG)=="0" & input$estNouvelAnimal == 1){checklist1 <<- cbind(checklist1,data.frame("Tag Gauche"= c("NULL")))}
    
    if ((input$idTagOrD)!="0" & input$estNouvelAnimal == 1) {
      checklist1 <<- cbind(checklist1,data.frame("Tag Droit" = input$idTagOrD))}
    else if ((input$idTagOrD=="0") & input$estNouvelAnimal == 1){checklist1 <<- cbind(checklist1,data.frame("Tag Droit"= c("NULL")))}
    
    if (((idTagOrG!="")|(!is.na(idTagOrG))|!is.null(idTagOrG)) & input$estNouvelAnimal == 0) {
      checklist1 <<- cbind(checklist1,data.frame("Tag Gauche" = idTagOrG))}
    else if ((idTagOrG=='') & input$estNouvelAnimal == 0) {checklist1 <<- cbind(checklist1,data.frame("Tag Gauche"= c("NULL")))}
    
    if (((idTagOrD!="")|(!is.na(idTagOrD))|!is.null(idTagOrD)) & input$estNouvelAnimal == 0) {
      checklist1 <<- cbind(checklist1,data.frame("Tag Droit" = idTagOrD))}
    else if (idTagOrD=="" & input$estNouvelAnimal == 0) {checklist1 <<- cbind(checklist1,data.frame("Tag Droit"= c("NULL")))}
    
    if (!is.na(input$lPattArriere)) {
      checklist1 <<- cbind(checklist1,data.frame("Longueur patte" = input$lPattArriere))}
    else {checklist1 <<- cbind(checklist1,data.frame("Longueur patte"= c("NULL")))}
    
    if (!is.null(input$sexe)) {
      checklist1 <<- cbind(checklist1,data.frame("Sexe" = input$sexe))}
    else {checklist1 <<- cbind(checklist1,data.frame("Sexe"= c("NULL")))}
    
    if ((input$lBoisGauche!=0) & (input$sexe=='M')) {
      checklist1 <<- cbind(checklist1,data.frame("Longueur bois G" = input$lBoisGauche))}
    else if ((input$lBoisGauche==0)& (input$sexe=='M')) {checklist1 <<- cbind(checklist1,data.frame("Longueur bois G"= c("NULL")))}
    
    if ((input$lBoisDroit!=0) & (input$sexe=='M')) {
      checklist1 <<- cbind(checklist1,data.frame("Longueur bois D" = input$lBoisDroit))}
    else if ((input$lBoisDroit==0) & (input$sexe=='M')) {checklist1 <<- cbind(checklist1,data.frame("Longueur bois D"= c("NULL")))}
    
    if (((input$etatBois)!="") &(input$sexe=='M')){
      checklist1 <<- cbind(checklist1,data.frame("Etat bois" = input$etatBois))}
    else if (((input$etatBois)=="")& (input$sexe=='M')) {checklist1 <<- cbind(checklist1,data.frame("Etat bois"= c("NULL")))}
    
    if (!is.na(input$tglucose)) {
      checklist1 <<- cbind(checklist1,data.frame("Glucose" = input$tglucose))}
    else {checklist1 <<- cbind(checklist1,data.frame("Glucose"= c("NULL")))}
    
    if ((input$cirCou)!=0) {
      checklist1 <<- cbind(checklist1,data.frame("Cou" = input$cirCou))}
    else {checklist1 <<- cbind(checklist1,data.frame("Cou"= c("NULL")))}
    
    output$tablechecklist1 = DT::renderDT(checklist1,server = F) 
    
  })
  
  ###### Table                           
  
  checklist_table = data.frame()
  row.names(checklist_table) = NULL
  output$tablechecklist_table = DT::renderDT(expr = checklist_table,server = F)
  
  observeEvent(input$checklist_tab, { 
    #cat(file=stderr(), "test", class(input$time), "\n")
    
    if (!is.na(input$ExtTemp)) {
      checklist_table <<- data.frame("T° Ext" = input$ExtTemp)}
    else {checklist_table <<- data.frame("T° Ext"= c("NULL"))}
    
    if (!is.na(input$rectTemp)) {
      checklist_table <<- cbind(checklist_table,data.frame("T° rectale" = input$rectTemp))}
    else {checklist_table <<- cbind(checklist_table,data.frame("T° rectale"= c("NULL")))}
    
    if (!is.null(input$lutte)) {
      checklist_table <<- cbind(checklist_table,data.frame("Lutte" = input$lutte))}
    else {checklist_table <<- cbind(checklist_table,data.frame("Lutte"= c("NULL")))}
    
    if (!is.null(input$halete)) {
      checklist_table <<- cbind(checklist_table,data.frame("Halete" = input$halete))}
    else {checklist_table <<- cbind(checklist_table,data.frame("Halete"= c("NULL")))}
    
    if (!is.null(input$cribague)) {
      checklist_table <<- cbind(checklist_table,data.frame("Cri bague" = input$cribague))}
    else {checklist_table <<- cbind(checklist_table,data.frame("Cri bague"= c("NULL")))}
    
    if (!is.null(input$criautre)) {
      checklist_table <<- cbind(checklist_table,data.frame("Cri autre" = input$criautre))}
    else {checklist_table <<- cbind(checklist_table,data.frame("Cri autre"= c("NULL")))}
    
    if ((input$Notation_euro_table)!="") {
      checklist_table <<- cbind(checklist_table,data.frame("Eurodeer" = input$Notation_euro_table))}
    else {checklist_table <<- cbind(checklist_table,data.frame("Eurodeer"= c("NULL")))}
    
    output$tablechecklist_table = DT::renderDT(checklist_table,server = F) 
    
  })
  
  ##################           RUBRIQUE LACHER                          #################
  
  updateSelectizeInput(session, "habitat", choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt"))
  updateSelectizeInput(session, "habitat_perte", choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt"))
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
    
    cpt_lache=0
    
    if (is.na(tmp_time))  
    {shinyalert("STOP!", "Heure lâcher manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}
    
    if (is.null(input$vitesse))  
    {shinyalert("STOP!", "Vitesse manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}
    
    if (is.null(input$titube)) 
    {shinyalert("STOP!", "Titube ? manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if (is.null(input$couche)) 
    {shinyalert("STOP!", "Couche ? manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if (is.null(input$cabriole_saut)) 
    {shinyalert("STOP!", "cabriole-saut manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if (is.null(input$cri)) 
    {shinyalert("STOP!", "Cri ? manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if (is.null(input$allure)) 
    {shinyalert("STOP!", "Allure manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if (is.null(input$gratte_collier)) 
    {shinyalert("STOP!", "Gratte-collier ? manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if (is.null(input$tombe)) 
    {shinyalert("STOP!", "Tombe ? manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if ((input$habitat)=="") 
    {shinyalert("STOP!", "Habitat manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if ((input$Notation_euro)=="") 
    {shinyalert("STOP!", "Notation eurodeer manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if ((input$habitat_perte)=="") 
    {shinyalert("STOP!", "Habitat perte de vue manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if ((input$nbre_stops)==0 || is.na(input$nbre_stops) ) 
    {shinyalert("STOP!", "Nombre stops manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}  
    
    if ((input$visibilite)=="") 
    {shinyalert("STOP!", "visibilite manquante", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1}   
    
    if (is.na(input$nbre_personnes)) 
    {shinyalert("STOP!", "Nombre personnes manquant", type = "warning",confirmButtonText="Valider quand meme", showCancelButton=T,cancelButtonText="Annuler",showConfirmButton = FALSE)} 
    else
    {cpt_lache=cpt_lache+1} 
    
    if (cpt_lache==15) 
    {shinyalert("PARFAIT!", "Toutes les donnees rentrees", type="success",confirmButtonText="Enregistrer les données", showCancelButton=T,cancelButtonText="Annuler",callbackR = modalCallback2)} }
  )
  
  ##################           RUBRIQUE CHECKLIST 2                     #################
  checklist2 = data.frame()
  row.names(checklist2) = NULL
  output$tablechecklist2 = DT::renderDT(expr = checklist2,server = F)
  
  
  observeEvent(input$checklist_2, { 
    #cat(file=stderr(), "testttt2t", input$titube, "\n")
    
    if (!is.null(input$vitesse))  {
      checklist2 <<- data.frame("Vitesse" = input$vitesse)}
    else {checklist2 <<- data.frame("Vitesse"= c("NULL"))}
    
    if (!is.null(input$titube)) {
      checklist2 <<- cbind(checklist2,data.frame("Titube" = input$titube))}
    else {checklist2 <<- cbind(checklist2,data.frame("Titube"= c("NULL")))}
    
    if (!is.null(input$couche)) {
      checklist2 <<- cbind(checklist2,data.frame("Couche" = input$couche))}
    else {checklist2 <<- cbind(checklist2,data.frame("Couche"= c("NULL")))}
    
    if (!is.null(input$cabriole_saut)) {
      checklist2 <<- cbind(checklist2,data.frame("cabriole saut" = input$cabriole_saut))}
    else {checklist2 <<- cbind(checklist2,data.frame("cabriole saut"= c("NULL")))}
    
    if (!is.null(input$cri)) {
      checklist2 <<- cbind(checklist2,data.frame("Cri" = input$cri))}
    else {checklist2 <<- cbind(checklist2,data.frame("Cri"= c("NULL")))}
    
    if (!is.null(input$allure)) {
      checklist2 <<- cbind(checklist2,data.frame("Allure" = input$allure))}
    else {checklist2 <<- cbind(checklist2,data.frame("Allure"= c("NULL")))}
    
    if (!is.null(input$gratte_collier)) {
      checklist2 <<- cbind(checklist2,data.frame("Gratte_Collier" = input$gratte_collier))}
    else {checklist2 <<- cbind(checklist2,data.frame("Gratte_Collier"= c("NULL")))}
    
    if (!is.null(input$tombe)) {
      checklist2 <<- cbind(checklist2,data.frame("Tombe" = input$tombe))}
    else {checklist2 <<- cbind(checklist2,data.frame("Tombe"= c("NULL")))}
    
    if ((input$habitat)!="") {
      checklist2 <<- cbind(checklist2,data.frame("Habitat" = input$habitat))}
    else {checklist2 <<- cbind(checklist2,data.frame("Habitat"= c("NULL")))}
    
    if ((input$Notation_euro)!="") {
      checklist2 <<- cbind(checklist2,data.frame("Eurodeer" = input$Notation_euro))}
    else {checklist2 <<- cbind(checklist2,data.frame("Eurodeer"= c("NULL")))}
    
    if ((input$habitat_perte)!="") {
      checklist2 <<- cbind(checklist2,data.frame("Habitat perte" = input$habitat_perte))}
    else {checklist2 <<- cbind(checklist2,data.frame("Habitat perte"= c("NULL")))}
    
    if (!is.na(input$nbre_stops)) {
      checklist2 <<- cbind(checklist2,data.frame("Stops" = input$nbre_stops))}
    else {checklist2 <<- cbind(checklist2,data.frame("Stops"= c("NULL")))}
    
    if ((input$visibilite)!="") {
      checklist2 <<- cbind(checklist2,data.frame("Visibilite" = input$visibilite))}
    else {checklist2 <<- cbind(checklist2,data.frame("Visibilite"= c("NULL")))}
    
    if (!is.na(input$nbre_personnes)) {
      checklist2 <<- cbind(checklist2,data.frame("Nbre_personnes" = input$nbre_personnes))}
    else {checklist2 <<- cbind(checklist2,data.frame("Nbre_personnes"= c("NULL")))}
    
    
    output$tablechecklist2 = DT::renderDT(checklist2,server = F) 
    
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
  
  
  ##################           INTEGRATION DES DONNEES                  #################
  
  # pour obtenir le cpt_id suivant
  
  max_value=dbGetQuery(con,paste0('SELECT cpt_id FROM cmpt.t_capture_cpt order by cpt_id desc limit 1'))
  max_value=as.integer((max_value[1,1])+1)
  max_valuebis= dbGetQuery(con,paste0('SELECT cpt_cap_id FROM cmpt.t_capture_cpt order by cpt_cap_id desc limit 1'))
  max_valuebis=as.integer((max_valuebis[1,1])+1)
  
  
  modalCallback2 <- function(value) {
    if (value == TRUE) {
      gettime= as.character(input$time)
      gettime=strsplit(gettime, " ")[[1]]
      gettime=gettime[2]
      gettime2= as.character(input$time2)
      gettime2=strsplit(gettime2, " ")[[1]]
      gettime2=gettime2[2]
      
      if (!is.na(gettime2)) {
        dbSendQuery(con,sprintf("INSERT INTO cmpt.t_capture_cpt (cpt_id, cpt_heure_lache, cpt_lache_course, cpt_lache_bolide, cpt_lache_gratte_collier, 
                                cpt_lache_tombe,cpt_lache_cabriole,cpt_lache_nbre_stop, cpt_lache_aboiement_cri,cpt_lache_titube, cpt_lache_couche,cpt_lache_visibilite,cpt_lache_habitat_lache, 
                                cpt_lache_habitat_pertevue,cpt_lache_public,cpt_lache_eurodeer,cpt_heure_second_lache,cpt_ani_etiq,cpt_date,cpt_annee_suivi,cpt_cap_id)
                                VALUES ('%s','%s',' %s','%s', '%s','%s', '%s',%s, '%s',' %s','%s',' %s', '%s','%s',' %s',' %s','%s','%s','%s','%s','%s')",max_value, gettime,input$vitesse,input$allure,
                                input$gratte_collier,input$tombe, input$cabriole_saut, input$nbre_stops,input$cri,input$titube,input$couche,
                                input$visibilite,input$habitat,input$habitat_perte,input$nbre_personnes,input$Notation_euro,gettime2,110,Sys.Date(),format(Sys.time(), "%Y"),max_valuebis))
      }    
      else {
        dbSendQuery(con,sprintf("INSERT INTO cmpt.t_capture_cpt (cpt_id, cpt_heure_lache, cpt_lache_course, cpt_lache_bolide, cpt_lache_gratte_collier, 
                                cpt_lache_tombe,cpt_lache_nbre_stop, cpt_lache_aboiement_cri,cpt_lache_titube, cpt_lache_couche,cpt_lache_visibilite,cpt_lache_habitat_lache, 
                                cpt_lache_habitat_pertevue,cpt_lache_public,cpt_lache_eurodeer,cpt_ani_etiq,cpt_date,cpt_annee_suivi,cpt_cap_id)
                                VALUES ('%s','%s',' %s','%s', '%s', '%s',%s, '%s',' %s','%s',' %s', '%s','%s',' %s',' %s','%s','%s','%s','%s')",max_value, gettime,input$vitesse,input$allure,
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
  
  }