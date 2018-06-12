##################              FORMULAIRES                ############################
#               création de la mise en page des formulaires

##################           rubrique animal               ############################

contentcaractanimal = fluidPage(
  #titlePanel("Caract. de l'animal"),
  tags$head(
    tags$style(HTML("#idTagOrG2 + div> div>.item {
                    color: blue !important;}

                    #idTagOrD2 + div> div>.item {
                    color: blue !important;}

                    #idRFID2 + div> div>.item {
                    color: blue !important;}

                    #idSite2 + div> div>.item {
                    color: blue !important;}

                    #nAnimal2 + div> div>.item {
                    color: blue !important;}       "))
    ),
  
  fluidRow(
    uiOutput("out_sabot"),
    uiOutput("animalExiste"),
    uiOutput("alert_poids"),
    column(2, numericInput(inputId = "numSabot", value =0,label = h4("N Sabot"),min=0,max=28 )),
    column(2, numericInput(inputId = "pSabotPlein", value = "",label = h4("Poids Sabot Plein"),min=0,max=65 )),
    column(2, numericInput(inputId = "pSabotVide", value = "",label = h4("Poids Sabot Vide"),min=0,max=50 )),
    column(2, h4("Poids Animal"),textOutput("poids_ani")),
    column(12),
    column(2,timeInput("time_caract", h4("Heure table:"), seconds = FALSE),
           actionButton("to_current_time_caract", "Afficher l'heure")),
    column(2, dateInput('date_caract',label=h4("Date"),value = Sys.Date())),
    
    column(2, radioButtons(inputId = "estNouvelAnimal", choices = c("oui","non"), selected = "oui",label = h4("1ere Capture"))),
    column(2, radioButtons(inputId = "identifié", choices = c("oui","non"), selected = "non",label = h4("Identifé"))),
    column(1, radioButtons("sexe",h4("Sexe"),choiceNames = list("M","F"), choiceValues = list("M","F"), selected = character(0))),
    column(12,hr()),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifié == 'non') ",
                              textInput(inputId = "nAnimal", value = "",label = h4("N° Animal")))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifié == 'non')", textInput("idTagOrG", h4("Tag Oreille Gauche"),value="0"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifié == 'non')", textInput("idTagOrD", h4("Tag Oreille Droite"),value="0"))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || (input.estNouvelAnimal == 'non' && input.identifié == 'non')", selectizeInput("idRFID", h4("RFID"),
                               choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'oui' || input.estNouvelAnimal == 'non' && input.identifié == 'non'", selectizeInput("idSite", h4("Site"),choices = "",
                              options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(12),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifié == 'oui'",
                              selectizeInput("nAnimal2",h4("N° Animal"), choices = "",
                                             options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    
    #######ici il faut que l'on puisse rentrer une valeur avec la valeur de l'ancienne bague par défaut.
    
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifié == 'oui'", selectizeInput("idTagOrG2", h4("Tag Oreille Gauche"), choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifié == 'oui'", selectizeInput("idTagOrD2", h4("Tag Oreille Droite"), choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifié == 'oui'", selectizeInput("idRFID2", h4("RFID"), choices = "", options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2,conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifié == 'oui'", selectizeInput("idSite2", h4("Site"), choices = "", options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL))),
    column(2, conditionalPanel(condition = "input.estNouvelAnimal == 'non' && input.identifié == 'oui'", selectizeInput("idRFID_new", h4("RFID_new"), choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)))
    ),
  
  column(12,hr()),
  
  fluidRow(
    column(2, numericInput("cirCou", value='0', h4("Circonference cou"),min=0, max=1)),
    uiOutput("out_cirCou"),
    column(2, numericInput("lPattArriere", value='0', h4("Longueur patte arriere"),min=0, max=1)),
    uiOutput("out_lPattArriere"),
    column(2, numericInput("tglucose", value="", h4("Taux de Glucose sanguin"), min=0))
    
  ),
  
  conditionalPanel(
    condition = "input.sexe == 'M'",
    
    fluidRow(
      column(2, numericInput("lBoisGauche", value='0', h4("Longueur bois gauche"),min=0, max=1)),
      column(2, numericInput("lBoisDroit", value='0', h4("Longueur bois droit"),min=0, max=1)),
      #uiOutput("out_lBoisGauche"), 
      #uiOutput("out_lBoisDroit"),
      column(2, selectizeInput("etatBois", h4("etat bois"), choices = "" , options = list(create = TRUE)))
    )
  )
)

##################           rubrique blessures            #################

contentblessures = fluidPage( 
  # titlePanel("Blessures"),
  fluidRow(
    
    column(3, selectizeInput("blelocalisation_sel", h4("Localisation"), 
                             choices = "",options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL) 
           #bsModal("nouvelleLocalization_modal", "Entrer la localisation","", size = "large",wellPanel(
           # textInput("nouvelle_localisation_txt",""),
           #actionButton("ok_button", "OK"),
           #actionButton("quit_button", "Quitter")
           #))
           #textInput("blelocalisation_txt","")
    ),
    
    column(3, selectInput("bleGrav_sel", h4("Gravite"), choices = "", selected = "superficielle")),
    #textInput("bleGrav_txt","") ),
    column(3, selectizeInput("bleTrait_sel", h4("Traitement"), choices = "" ,options = list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
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
                             choices = "" ,options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("local_prelev", h4("Localisation"), 
                             choices = "",options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("cont_prelev", h4("Contenant"), 
                             choices = "",options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
    column(2, selectizeInput("solv_prelev", h4("Solvant"), 
                             choices = "",options=list(placeholder='Choisir une valeur :',create = TRUE, onInitialize = I('function() { this.setValue(""); }')), selected = NULL)), 
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
                            choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)) 
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
                            choices = "", options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    
    column(2, selectizeInput("habitat_perte", h4("Habitat perte de vue"), 
                             choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'),create = TRUE), selected = NULL)),
    
    column(2,selectizeInput("Notation_euro", h4("Notation Eurodeer"), 
                            choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
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
    column(2,selectizeInput("numSabot_capture",label = h4("N° Sabot"), choices = "",options=list(placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }')), selected = NULL)),
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
    column(2,selectizeInput("cpt_dose_acepromazine",h4("Acepromazine"), choices = "",options = (list(create = TRUE,placeholder='Choisir une valeur :', onInitialize = I('function() { this.setValue(""); }'))), selected = NULL)),
    
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

ui <- shinyUI(
                         #theme=shinytheme("sandstone"),
                         # Application title
                         # titlePanel("Carnet Electronique"),
                  tabsetPanel(
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
                 )
            
)

