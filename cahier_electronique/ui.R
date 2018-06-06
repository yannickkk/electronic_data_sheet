library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyBS)
library(shinyTime)
library(RPostgreSQL)
library(shinyalert)
library(chron) 

############################ CONNECTION A LA BDD LOCALE    ##################################
con<- dbConnect(PostgreSQL(), host="localhost", dbname="postgres", user="postgres", password="****")

############################ LISTES DE CHOIX               #################################
#               requêtes sql permettant d'alimenter les listes de choix
############################ rubrique animal               ###############################
etatboischoices <- dbGetQuery(con,"select distinct etb_description from lu_tables.tr_etat_bois_etb order by etb_description")
############################ rubrique blessures            ############################
blegravchoices <- dbGetQuery(con,"select distinct blg_gravite from lu_tables.tr_blessure_gravite_blg order by  blg_gravite")

bletraitchoices <- dbGetQuery(con,"select distinct blt_traitement from lu_tables.tr_blessure_traitement_blt order by blt_traitement ")

blelocalisationchoices <- dbGetQuery(con,"select distinct bll_localisation from lu_tables.tr_blessure_localisation_bll order by bll_localisation")
############################ rubrique Prélèvements         #########################
pretypechoice <- dbGetQuery(con,"select distinct (sat_type) from lu_tables.tr_samples_types_sat")
############################ rubrique Collier              ##############################

############################ rubrique Table                #################################

############################ rubrique Historique           ############################

############################ rubrique cheklist 1           ############################

############################ rubrique Lâcher               ################################

############################ rubrique Cheklist 2           ############################

############################ rubrique Capture              ############################

############################ rubrique Sabot                ############################


##################              FORMULAIRES                ############################
#               création de la mise ne page des formulaires
##################           rubrique animal               ############################

contentcaractanimal = fluidPage(
  #titlePanel("Caract. de l'animal"),
  
  fluidRow(
    uiOutput("out_sabot"),
    column(2, numericInput(inputId = "numSabot", value =0,label = h4("N Sabot"),min=0,max=28 )),
    column(2, numericInput(inputId = "pSabotPlein", value = " ",label = h4("Poids Sabot Plein"),min=0,max=65 )),
    column(2, numericInput(inputId = "pSabotVide", value = " ",label = h4("Poids Sabot Vide"),min=0,max=50 )),
    column(2, h4("Poids Animal"),textOutput("value")),
    column(12),
    column(2,timeInput("time_caract", h4("Heure table:"), seconds = FALSE),
           actionButton("to_current_time_caract", "Afficher l'heure")),
    #hr(),   
    column(2, dateInput('date_caract',label=h4("Date"),value = Sys.Date())),
    
    column(1, radioButtons(inputId = "estNouvelAnimal", choices = c("oui","non"), selected = "oui",label = h4("Capture"))),
    column(1, radioButtons(inputId = "identifié", choices = c("oui","non"), selected = "non",label = h4("Identifé"))),
    column(1, radioButtons("sexe",h4("Sexe"),choiceNames = list("M","F"), choiceValues = list("M","F"))),
    column(12,hr()),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'oui'",
                              numericInput(inputId = "nAnimal", value = " ",label = h4("N° Animal"),min=0 ))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui'", numericInput("idTagOrG", h4("Tag Oreille Gauche"),value="0"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui'", numericInput("idTagOrD", h4("Tag Oreille Droite"),value="0"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui'", selectizeInput("idRFID", h4("RFID"),
                                                                                            choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'oui'", selectizeInput("idSite", h4("Site"),choices = dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit"),
                                                                                           options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(12),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non'",
                              selectizeInput("nAnimal2",h4("N° Animal"), choices = dbGetQuery(con,"select ani_etiq from public.t_animal_ani order by ani_id DESC"),
                                             options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    ####retravailler il faut que ans ces cas on puisse faire un choix dans la liste avec par défaut la valeur de l'ancien lieu de capture  
    # column(2,conditionalPanel(condition = "input.estNouvelAnimal == 0",h4("Site"), textOutput("out_nAnimal2" ))),
    
    #######ici il faut que l'on puisse rentrer une valeur avec la valer de l'ancienne bague par défaut.
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non'", numericInput("idTagOrG", h4("Tag Oreille Gauche"), value= "out_idTagOrG"))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non'", numericInput("idTagOrG", h4("Tag Oreille Droite"), value= "out_idTagOrD"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non'", h4("RFID"),textOutput("out_idTagRfid"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non'", selectizeInput("idRFID", h4("RFID"),
                                                                                            choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non'", selectizeInput("idSite", h4("Site"), choices = dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit"),
                                                                                           options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = "out_nAnimal2" )))
    
    # column(2,conditionalPanel(condition = "input.estNouvelAnimal != 1 & input.nAnimal2 != \"\"", selectizeInput("idSite_new", , h4("Nouveau Site"),choices = dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit"), selected = "out_nAnimal2"))),
    # column(2,conditionalPanel(condition = "input.estNouvelAnimal != 1 & input.nAnimal2 != \"\"", h4("Tag Oreille Gauche"), textOutput("out_idTagOrG"))),
    # column(2,conditionalPanel(condition = "input.estNouvelAnimal != 1 & input.nAnimal2 != \"\"", h4("Tag Oreille Droite"), textOutput("out_idTagOrD"))),
    # column(2, conditionalPanel(condition = "input.estNouvelAnimal != 1 & input.nAnimal2 != \"\"", selectizeInput("idRFID", h4("Nouveau RFID"),
    # choices = dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    
    #  column(12,hr()),
    # column(2, radioButtons("sexe",h4("Sexe"),choiceNames = list("M","F"), choiceValues = list("M","F")))
  ),
  
  hr(),
  
  fluidRow(
    column(2, numericInput("cirCou", value='0', h4("Circonference cou"),min=0, max=(dbGetQuery(con,"select max(cap_circou) from t_capture_cap")))),
    uiOutput("out_cirCou"),
    column(2, numericInput("lPattArriere", value='0', h4("Longueur patte arriere"),min=0, max=(dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")))),
    uiOutput("out_lPattArriere"),
    column(2, numericInput("tglucose", value="", h4("Taux de Glucose sanguin"), min=0))
  ),
  
  conditionalPanel(
    condition = "input.sexe == 'M'",
    
    fluidRow(
      column(2, numericInput("lBoisGauche", value='0', h4("Longueur bois gauche"),min=0, max=(dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")))),
      column(2, numericInput("lBoisDroit", value='0', h4("Longueur bois droit"),min=0, max=(dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")))),
      #uiOutput("out_lBoisGauche"), 
      #uiOutput("out_lBoisDroit"),
      column(2, selectizeInput("etatBois", h4("etat bois"), choices = etatboischoices , options = list(create = TRUE)))
    )
  )
)

##################           rubrique blessures            #################

contentblessures = fluidPage( 
  # titlePanel("Blessures"),
  fluidRow(
    
    column(3, selectizeInput("blelocalisation_sel", h4("Localisation"), 
                             choices = blelocalisationchoices,options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL) 
           #bsModal("nouvelleLocalization_modal", "Entrer la localisation","", size = "large",wellPanel(
           # textInput("nouvelle_localisation_txt",""),
           #actionButton("ok_button", "OK"),
           #actionButton("quit_button", "Quitter")
           #))
           #textInput("blelocalisation_txt","")
    ),
    
    column(3, selectInput("bleGrav_sel", h4("Gravite"), choices = blegravchoices, selected = "superficielle")),
    #textInput("bleGrav_txt","") ),
    column(3, selectizeInput("bleTrait_sel", h4("Traitement"), choices = bletraitchoices,options = list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    #textInput("bleTrait_txt","")),
    column(3, actionButton("ajoutBle","Ajouter une blessure"))
  ),
  
  hr(),
  
  fluidRow(
    sidebarLayout(
      mainPanel(
        dataTableOutput("tableblessure")),
      sidebarPanel(actionButton("sup_Bles", "Supprimer blessure"))
    )
  ))


##################           rubrique Prélèvements         #################

contentprelevement = fluidPage(
  
  fluidRow(
    column(2,selectizeInput("diarrhee", h4("Diarrhee ?"),choices = list(TRUE,FALSE),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,selectizeInput("tiques", h4("Nombre Tiques"), choices = c(1:30,'>30'), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))
  ),
  
  fluidRow(
    
    column(2, selectizeInput("type_prelev", h4("Type de prelevement"), 
                             choices = pretypechoice ,options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("local_prelev", h4("Localisation"), 
                             choices = dbGetQuery(con,"select distinct (sal_localisation) from lu_tables.tr_samples_localisation_sal"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("cont_prelev", h4("Contenant"), 
                             choices = dbGetQuery(con,"select distinct (sac_conditionnement) from lu_tables.tr_samples_contenant_sac"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("solv_prelev", h4("Solvant"), 
                             choices = dbGetQuery(con,"select distinct (sas_solvant) from lu_tables.tr_samples_solvant_sas"),options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("nbre_echant", h4("Nombre d'echantillons"), 
                             choices =list( 1,2,3,4,5) ,selected = NULL)),
    column(3, actionButton("ajout_prelev","Ajouter un prelevement"))
  ),
  
  # if (input.type_prelev=="sang" && input.local_prelev=="jugulaire" && input.cont_prelev=="tube rouge" && input.solv_prelev=="sec") {} 
  
  
  hr(),
  
  fluidRow(
    sidebarLayout(
      mainPanel(
        dataTableOutput("tableprelevement")),
      sidebarPanel(actionButton("sup_prelev", "Supprimer prelevement"))
    ))
)





##################           rubrique Collier              #################

contentcollier = fluidPage(
  #titlePanel("Caracteristique du collier"),
  fluidRow(
    #titlePanel("Pose de collier"),
    column(3, checkboxInput(inputId = "new_collier", value = F,label = h4("Nouveau collier"))),
    column(3, actionButton("ajoutColl","Confirmer la nouvelle pose"))
  ))

##################           rubrique Table                #################


contenttable = fluidPage(
  #titlePanel("Comportement sur table"),
  
  fluidRow(
    
    column(2,timeInput("time_table", h4("Heure:"),seconds = FALSE),
           actionButton("to_current_time_table", "Afficher l'heure")),  
    column(3,numericInput("rectTemp", value=" ", h4("Temperature rectale"),step = 1)),  
    column(3,numericInput("ExtTemp", value=" ", h4("Temperature exterieure"),step = 1)),
    column(12,hr()),
    column(2,radioButtons("lutte",h4("Lutte"),choiceNames = list("Oui","Non"),choiceValues = list(T,F), selected = character(0))),
    column(2,radioButtons("halete",h4("Halete"),choiceNames = list("Oui","Non"),choiceValues = list(T,F), selected =character(0))),
    column(2,radioButtons("cribague",h4("Cri Bague"), choices  = list(NA,"0", "1-2", ">2"))),
    column(2,radioButtons("criautre", h4("Cri Autre"), choices = list("0", "1-2", ">2"), selected = F)),
    column(12,hr()),
    column(2,selectizeInput("Notation_euro_table", h4("Notation Eurodeer"), 
                            choices = dbGetQuery(con,"select (ect_comportement) from lu_tables.tr_eurodeer_comp_table_ect"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)) 
  ))

##################           rubrique Historique           #################

contenthistorique <- fluidPage(
  #titlePanel("Historique"),
  #fluidRow(
  
  tabPanel("Historique de capture",
           DT::dataTableOutput("historique")
           #         conditionalPanel(
           #    condition = "input.estnouvelanimal == 0",h4("Historique de capture"),DT::dataTableOutput("historique")
  )
  
  # )
)

##################           rubrique cheklist 1           #################


contentcheck1 =  fluidPage(fluidRow(
  titlePanel("Checklist - Caracteristiques"),
  tabPanel("Cheklist 1", DT::dataTableOutput("tablechecklist1")), 
  column(12,useShinyalert(),
         actionButton("checklist_1", "Checklist",icon('eye'),width='25%')),
  
  #  titlePanel("Checklist - Prelevements"),
  
  column(12,hr()),
  
  #conditionalPanel(
  #  condition = "input.new_collier == 1",
  #  fluidRow(titlePanel("Checklist - Collier"))) ,
  
  
  titlePanel("Checklist - Table"),
  tabPanel("Checklist Table",DT::dataTableOutput("tablechecklist_table")), 
  column(12,useShinyalert(),
         actionButton("checklist_tab", "Checklist",icon('eye'),width='25%'))
))


##################           rubrique Lâcher               #################


###submitButton(format(Sys.time(), "%X"))
#timeInput("time2", "Heure lâcher:", value = Sys.time(),seconds = FALSE))

contentlacher = fluidPage(
  # titlePanel("Comportement au lâcher"),
  fluidRow(
    column(2,timeInput("time", h4("Heure de lâcher:"),seconds = FALSE),
           actionButton("to_current_time", "Afficher l'heure")),
    
    column(2, timeInput("time2", h4("Heure de 2nd lâcher:"),seconds = FALSE),
           actionButton("to_current_time2", "Afficher l'heure")),
    
    column(1,numericInput("nbre_stops",value=0, h4("Nombre de stops"),min=0)),
    column(1,numericInput("nbre_personnes", value=NULL, h4("Nbre de personnes"),min=1)),
    
    column(12,hr()),
    
    column(1,radioButtons("vitesse",h4("Vitesse"),choiceNames = list("Pas","Course"),choiceValues = list(0,1), selected = character(0))),
    column(1,radioButtons("allure",h4("Allure"),choiceNames = list("Reflechi","Bolide"),choiceValues = list(0,1), selected = character(0))),
    column(1,radioButtons("cabriole_saut",h4("Cabriole"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("gratte_collier", h4("Gratte collier"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("tombe", h4("Tombe"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cri",h4("Cri"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("titube",h4("Titube"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("couche",h4("Couche"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    
    column(12,hr()),
    
    column(2,selectizeInput("visibilite", h4("Visibilite fuite"), 
                            choices = list("0-10","11-50","51-100",">100","Nuit"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    
    column(2,selectizeInput("habitat", h4("Habitat lâcher"), 
                            choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt"), options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    
    column(2, selectizeInput("habitat_perte", h4("Habitat perte de vue"), 
                             choices = dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    
    column(2,selectizeInput("Notation_euro", h4("Notation Eurodeer"), 
                            choices = dbGetQuery(con,"select (ecl_comportement_lache) from lu_tables.tr_eurodeer_comp_lache_ecl"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,useShinyalert())
  ))



##################           rubrique Cheklist 2           #################


contentcheck2 = fluidPage(fluidRow(
  tabPanel("Checklist 2", DT::dataTableOutput("tablechecklist2")), 
  
  column(12,useShinyalert(),
         actionButton("checklist_2", "Checklist",icon('eye'),width='25%'))),
  column(12,hr()),
  column(12, actionButton("save_checklist2","Enregistrer les données"))
)


##################           rubrique Capture              #################


contentcapture = fluidPage(
  
  #titlePanel("Comportement Capture"),
  fluidRow(
    
    column(2,dateInput('date_capture',label=h4("Date"),value ='2017-01-01')),
    column(2,selectizeInput("numSabot_capture",label = h4("N° Sabot"), choices = dbGetQuery(con,"select distinct cap_num_sabot FROM public.t_capture_cap"),options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
    column(2,timeInput("cpt_heure_debut_filet",h4("Heure arrivee filet"),seconds = FALSE),
           actionButton("time_debut_filet", "Afficher l'heure")),
    
    column(12,hr()),
    
    column(2,timeInput("cpt_temps_filet", h4("Temps passe filet"),seconds = FALSE),
           actionButton("time_filet", "Afficher l'heure")),
    column(2,textInput("nom_capteur_txt",label=h4("Nom des capteurs",""))),
    column(2,selectInput("Nbre_pers_experimentes",h4("Nombre de capteurs experimentes"),choices = list("0"=0,"1"=1,"2"=2,"3"=3,"4"=4,"5"=5),selected = 0)),
    
    column(12,hr()),
    
    column(1,radioButtons("cpt_filet_vitesse",h4("Vitesse"),choiceNames = list("Pas","Course"),choiceValues = list(0,1), selected = character(0))),
    column(1,radioButtons("cpt_filet_allure",h4("Allure"),choiceNames = list("Reflechi","Bolide"),choiceValues = list(0,1),selected = character(0))),
    column(1,radioButtons("cpt_filet_lutte", h4("Lutte"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cpt_filet_halete",h4("Halete"), choiceNames = list("Oui","Non"), choiceValues = list(1,0), selected = character(0))),
    column(1,radioButtons("cpt_filet_cri",h4("Cri"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected = character(0))),
    column(12,hr()),
    #column(2,textInput("Remarques",label=h4("Remarques",""))),
    #column(1),
    column(4,useShinyalert(),
           actionButton("checklist_capture", "Checklist",icon('eye')))
    
    
  ))


##################           rubrique Sabot                #################


contentsabot = fluidPage(
  # titlePanel("Comportement sabot"), 
  fluidRow(
    
    #Heure de mise en sabot
    column(3, timeInput("cpt_heure_mise_sabot", h4("Heure de mise en sabot:"),seconds = FALSE),
           actionButton("time_sabot", "Afficher l'heure")),
    
    #Fin de surveillance
    column(3,timeInput("cpt_heure_fin_surv", h4("Fin de surveillance"),seconds = FALSE),
           actionButton("time_fin", "Afficher l'heure")),
    
    column(12,hr()),
    
    #Acepromazine
    column(2,selectizeInput("cpt_dose_acepromazine",h4("Acepromazine"), choices = dbGetQuery(con,"select distinct cpt_dose_acepromazine from cmpt.t_capture_cpt order by cpt_dose_acepromazine"),options = (list(create = TRUE,placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))), selected = NULL)),
    
    #Sur le dos
    column(1,radioButtons("cpt_sabot_retournement",h4("Sur le dos"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    
    #Couche
    column(1, radioButtons("cpt_sabot_couche",h4("Couche"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    
    #Agite
    column(1, radioButtons("cpt_sabot_agitation",h4("Agite"),choiceNames = list("Oui","Non"),choiceValues = list(1,0), selected =c("None selected" = ""))),
    
    column(12,hr()),
    
    #Observateur
    column(3,textInput("Observateur",label=h4("Observateurs",""))),
    
    #Remarque
    column(3,textInput("Remarques",label=h4("Remarque",""))),
    
    column(12,hr()),
    
    column(4,useShinyalert(),
           actionButton("checklist_sabot", "Checklist",icon('eye')))
    
  )
)


##################        ORGANISATION DES RUBRIQUES       #################


caractanimal = tabPanel("Caract. de l'animal",contentcaractanimal)
blessures = tabPanel("Blessures",contentblessures)
prelevement= tabPanel("Prelevements",contentprelevement)
caractcollier = tabPanel("Caract. du collier",contentcollier)
comportable = tabPanel("Comportement table",contenttable)
historique = tabPanel("Historique captures",contenthistorique)
checklist1 = tabPanel("checklist 1",contentcheck1)
comporlacher = tabPanel("Comportement lâcher",contentlacher)
checklist2 = tabPanel("checklist 2",contentcheck2)
comporcapture = tabPanel("Comportement capture",contentcapture)
comporsabot = tabPanel("Comportement sabot",contentsabot)


##################                    UI                   #################

##Lumen or cerulean or sandstone

ui <- shinyUI(navbarPage("Formulaires",
                         #theme=shinytheme("sandstone"),
                         # Application title
                         # titlePanel("Carnet Electronique"),
                         #tabsetPanel(
                         tabPanel  ("Animal", caractanimal),
                         tabPanel  ("Blessures", blessures),
                         tabPanel  ("Prelevement", prelevement),
                         tabPanel  ("Collier",caractcollier),
                         tabPanel  ("Table",comportable),
                         tabPanel  ("historique",historique),
                         tabPanel  ("Checklist 1",checklist1),
                         tabPanel  ("Lâcher",comporlacher),
                         tabPanel  ("Checklist 2",checklist2),
                         tabPanel  ("Capture",comporcapture),
                         tabPanel  ("Sabot",comporsabot)
                         #tabPanel("Summary", verbatimTextOutput("summary")),
                         #tabPanel("Table", tableOutput("table"))
)
)

