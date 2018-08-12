library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyBS)
library(shinyTime)
library(RPostgreSQL)
library(shinyalert)
library(chron)
library(shinyWidgets)
library(shinyjs)
library(V8)
library(stringr)
library(serial)
library(audio)

source("connect.R")

noms_colonnes<- c("N°Animal","ani_nom","N°Animal telemetrie","N° bague annee capture","Nombre capture","inconnue","Site Capture","capture faon","Date","jour","mois","annee","annee  de suivi","Sexe","Age cahier","Age corrige","categorie d'age","etat_sante","cap_tag_droit","cap_tag_gauche","cap_tag_droit_metal","cap_tag_gauche_metal","cap_pertinent","cap_lactation","RFID","Poids","Cir Cou","Long patte Ar","machoire","long bois gauche","long bois droit","glucose","T°C_ext","TIQUES FIXES","autres parasites", "Peau","poils","sang","feces","tiques","vaginal","Nasal","remarque","Collier","accelero","proximite","id_collier","date_deb","date_fin","date_fin arrondie","date_fin_capteur","suivi_GPS oui si>60jours","jrs_suivi","capteur Activite","probleme collier","site vie","secteur","Mort","Date mort","Date mort arrondie","Cause detaillle","cause categories","Pds mort","nom capteur","nombre d'experimentes (n)","arrivee filet course (1/0)","arrivee filet panique (1/0)","lutte","haletement (1/0)","cri (1/0)","acepromazine (1=0,3cc)","num_sabot","couche_sabot (1/0)","agitation (1/0)","retournement (1/0)","hre fin surv","surveillance (mn)","distance (KM)","lutte (1/0)","halete (1/0)","cri (1/0)","T°C 1","T°C 2","Cœur 1","Cœur 2","remarque_table","localisation sonde temperature","eurodeer","titube (1/0)","couche (1/0)","course (1/0)","tombe (1/0)","gratte collier (1/0)","cabriole (1/0)","bolide (1/0)","aboiement/cri (1/0)","filet","sabot sur place","transport+attente","marquage","total","capture","sabot","acepro","transport","table","lache","remarque_generale","bague","autre","stop","habitat lacher","habitat perte vue","visibilite","nb_public","eurodeer_lacher","remise sabot","hre_lacher_2")

dbSendQuery(con, paste0("update lu_tables.tr_sabots_sab set sab_disponible = DEFAULT"))

choix<- list()
choix[["visibilite"]]<-c(choisir = "", "0-10","11-50","51-100",">100","Nuit")
choix[["habitat"]]<-c(choisir = "", dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_lache) from cmpt.t_capture_cpt order by cpt_lache_habitat_lache ASC")[,1])
choix[["habitat_perte"]]<-c(choisir = "", dbGetQuery(con,"select distinct (t_capture_cpt.cpt_lache_habitat_pertevue) from cmpt.t_capture_cpt order by cpt_lache_habitat_pertevue ASC")[,1])
choix[["tiques"]]<-c(choisir = "",0:30,'>30')
choix[["diarrhee"]]<-c(choisir = "", c(TRUE,FALSE))
choix[["lactation"]]<-c(choisir = "", "oui", "non", "indeterminé")
choix[["Nbre_pers_experimentes"]]<-c(choisir = "", 0:5)
choix[["sexe"]]<-c("M","F")
choix[["names_oui_non"]]<-c("Oui","Non")
choix[["values_oui_non"]]<-c(1,0)
choix[["nbre_personnes"]]<-c(choisir = "", "4-5","6-10","11-20", "21-50",">50")
####ancienne notation avec selectizeInput pas besoin de valeur nulle car il y a un placeholder dans les options
#choix[["Notation_euro"]]<-dbGetQuery(con,"select (ecl_comportement_lache) from lu_tables.tr_eurodeer_comp_lache_ecl")
choix[["Notation_euro"]]<-c(choisir = "", dbGetQuery(con,"select (ecl_comportement_lache) from lu_tables.tr_eurodeer_comp_lache_ecl")[,1])
choix[["Notation_euro_table"]]<-c(choisir = "", dbGetQuery(con,"select (ect_comportement) from lu_tables.tr_eurodeer_comp_table_ect")[,1])
#choix[["numSabot_capture"]]<-dbGetQuery(con,paste0("select toto.sab_valeur from (select distinct sab_id, sab_valeur FROM lu_tables.tr_sabots_sab order by sab_id) as toto"))
choix[["idRFID"]]<-c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")[,1])
choix[["idSite"]]<-c(choisir = "", dbGetQuery(con,"select sit_nom_court from public.tr_site_capture_sit")[,1])
choix[["nAnimal2"]]<-c(choisir = "", dbGetQuery(con,"select ani_etiq from public.t_animal_ani order by ani_id DESC")[,1])
choix[["cirCou"]]<-dbGetQuery(con,"select max(cap_circou) from t_capture_cap")
choix[["lPattArriere"]]<-dbGetQuery(con,"select max(cap_lpa) from t_capture_cap")
choix[["lBoisGauche"]]<-dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")
choix[["lBoisDroit"]]<-dbGetQuery(con,"select max(nca_valeur) from public.tj_mesureenum_capture_nca")
choix[["etatBois"]]<-dbGetQuery(con,"select distinct etb_description from lu_tables.tr_etat_bois_etb order by etb_description")
choix[["idTagOrG2"]]<-c(choisir = "", dbGetQuery(con, "select distinct cap_tag_gauche from public.t_capture_cap")[,1])
choix[["idTagOrD2"]]<-c(choisir = "", dbGetQuery(con, "select distinct cap_tag_droit from public.t_capture_cap")[,1])
choix[["idRFID2"]]<-c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi, public.t_capture_cap, public.t_animal_ani where cap_id = rfi_cap_id and cap_ani_id = ani_id")[,1])
choix[["idSite2"]]<-c(choisir = "", dbGetQuery(con, "select sit_nom_court from public.tr_site_capture_sit where (sit_id in (select cap_sit_id from public.t_capture_cap, t_animal_ani))")[,1])
choix[["idRFID_new"]]<-c(choisir = "", dbGetQuery(con,"select rfi_tag_code from public.t_rfid_rfi where rfi_cap_id is null")[,1])
choix[["age"]]<-c(choisir = "", "0.5" = "<1", "1.5" = "1", "2.5" = "2", "3.5" = '3', "4.5-5.5"= "4-5", ">6.5" = ">=6")
choix[["numSabot"]]<-c(choisir = "", dbGetQuery(con,"select sab_valeur from lu_tables.tr_sabots_sab where sab_disponible order by sab_id")[,1])
choix[["position_temp1"]]<-dbGetQuery(con,"select tel_localisation from lu_tables.tr_temperatures_localisation_tel")
choix[["position_temp2"]]<-dbGetQuery(con,"select tel_localisation from lu_tables.tr_temperatures_localisation_tel")
choix[["cpt_dose_acepromazine"]]<- c(choisir = "", dbGetQuery(con,"select distinct cpt_dose_acepromazine from cmpt.t_capture_cpt order by cpt_dose_acepromazine")[,1])
choix[["cribague"]]<-c("NA","0", "1-2", ">2")
choix[["criautre"]]<-c("0", "1-2", ">2")
choix[["vitesse"]]<-c("Pas","Course")
choix[["allure"]]<-c("Reflechi","Bolide")
choix[["values_vitesse"]]<-c(0,1)
choix[["values_allure"]]<-c(0,1)
choix[["cpt_filet_lutte"]]<-c(0,1,2)
choix[["nbre_echant"]]<-c(1,2,3,4,5)
choix[["typetype"]]<-c(choisir = "",dbGetQuery(con, "select sat_type from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sac_id")[,1])
choix[["localoca"]]<-c(choisir = "",dbGetQuery(con, "select sal_localisation from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sac_id")[,1])
choix[["condi"]]<-c(choisir = "", dbGetQuery(con, "select sac_conditionnement from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal where sat_id=sac_sat_id and sat_id=sal_sat_id  order by sac_id")[,1])
choix[["solsol"]]<-c(choisir = "",dbGetQuery(con, "select sas_solvant from lu_tables.tr_samples_types_sat, lu_tables.tr_samples_contenant_sac, lu_tables.tr_samples_localisation_sal, lu_tables.tr_samples_solvant_sas where sat_id=sac_sat_id and sat_id=sal_sat_id and sas_sat_id=sat_id and sac_id=sas_sac_id order by sac_id")[,1])
 

#decomment for rapberry pi
# tousb<-"/media/pi/USB2/captures_chevreuils"
# tosd<-"/home/pi/Desktop/App"

#decomment for pc
#tousb<-"H:/captures_chevreuils"
tousb<-"C:/Users/ychaval/Documents/Collegues/Etudiants/JEREMIE_Marie/Programmes/R/electronic_data_sheet/cahier_electronique"
tosd<-"C:/Users/ychaval/Documents/Collegues/Etudiants/JEREMIE_Marie/Programmes/R/electronic_data_sheet/cahier_electronique"



# dbSendQuery(con,"
# DROP SCHEMA IF EXISTS historique cascade;
# CREATE SCHEMA historique;
# 
# DROP TABLE IF EXISTS historique.t_ani_gpsgsm;
# 
# CREATE TABLE historique.t_ani_gpsgsm AS
# SELECT t_animal_ani.ani_id,
# t_animal_ani.ani_etiq,
# --t_capture_cap.cap_bague,
# t_capture_cap.cap_annee_suivi,
# t_capture_cap.cap_date,
# t_animal_ani.ani_sexe,
# t_capture_cap.cap_age_classe,
# t_capture_cap.cap_poids,
# t_capture_cap.cap_lpa,
# tr_site_capture_sit.sit_nom_court,
# tr_type_equipement_teq.teq_nom_court,
# t_equipement_eqt.eqt_id_usuel,
# t_equipement_eqt.eqt_id,
# tr_sensors_sen.sen_association,
# tr_eqtmarque_mar.mar_libelle,
# tr_eqtmodel_mod.mod_libelle,
# tj_equipement_animal_eqt_ani_eqa.eqa_date_debut,
# tj_equipement_animal_eqt_ani_eqa.eqa_date_fin,
# --t_animal_ani.ani_mortalite,
# --t_animal_ani.ani_date_mort,
# --t_animal_ani.ani_cause_mort,
# --tj_equipement_animal_eqt_ani_eqa.eqa_activite,
# tj_equipement_animal_eqt_ani_eqa.eqa_probleme,
# --tj_equipement_animal_eqt_ani_eqa.eqa_date_fin_text,
# --tj_equipement_animal_eqt_ani_eqa.eqa_date_fin_arrondi,
# --t_animal_ani.ani_date_mort_arrondi,
# --t_animal_ani.ani_date_mort_text,
# --t_animal_ani.ani_poids_mort,
# --t_animal_ani.ani_poids_mort_na,
# --t_animal_ani.ani_remarque,
# --t_animal_ani.ani_mort_x,
# --t_animal_ani.ani_mort_y,
# --t_animal_ani.ani_inconnu,
# t_capture_cap.cap_faon,
# --t_capture_cap.cap_age,
# --t_capture_cap.cap_age_corrige,
# t_capture_cap.cap_circou
# --t_capture_cap.cap_etat_sante,
# --t_capture_cap.cap_heure_lacher,
# --tr_site_capture_sit.sit_id
# FROM t_capture_cap
# LEFT JOIN t_animal_ani ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
# LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
# LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
# LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
# LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
# LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
# LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
# LEFT JOIN t_equipement_conf_eqc ON t_equipement_eqt.eqt_id = t_equipement_conf_eqc.eqc_eqt_id and t_equipement_conf_eqc.eqc_annee_suivi = t_capture_cap.cap_annee_suivi
# LEFT JOIN lu_tables.tr_sensors_sen ON tr_sensors_sen.sen_id = t_equipement_conf_eqc.eqc_sen_id
# WHERE (tr_type_equipement_teq.teq_id = ANY (ARRAY[2, 3])) AND (t_capture_cap.cap_id IN ( SELECT min(t_capture_cap_1.cap_id) AS min
#                                                                                          FROM t_capture_cap t_capture_cap_1
#                                                                                          GROUP BY t_capture_cap_1.cap_ani_id, t_capture_cap_1.cap_annee_suivi)) AND (tr_type_equipement_teq.teq_id = ANY (ARRAY[2, 3]))
# ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;
# 
# ALTER TABLE historique.t_ani_gpsgsm
# OWNER TO ychaval;
# --GRANT ALL ON TABLE historique.t_ani_gpsgsm TO ychaval;
# --GRANT SELECT ON TABLE historique.t_ani_gpsgsm TO cefs_lecture;
# --GRANT ALL ON TABLE historique.t_ani_gpsgsm TO cefs_ecriture;
# 
# COMMENT ON TABLE historique.t_ani_gpsgsm
# IS 'Table contenant toutes les informations des individus equipes de GPS et de GSM (en dehors des locs) - si 2 captures la meme annee seule la premiere est conservee';
# 
# 
# DROP TABLE IF EXISTS historique.t_aniposi_gpsgsm;
# 
# CREATE TABLE historique.t_aniposi_gpsgsm AS
# SELECT v_posi_gpsgsm.cpos_id,
# --v_ani_gpsgsm.ani_id,
# t_ani_gpsgsm.ani_etiq,
# --v_ani_gpsgsm.cap_bague,
# t_ani_gpsgsm.cap_annee_suivi,
# t_ani_gpsgsm.cap_date,
# t_ani_gpsgsm.ani_sexe,
# t_ani_gpsgsm.cap_age_classe,
# t_ani_gpsgsm.cap_poids,
# t_ani_gpsgsm.cap_lpa,
# t_ani_gpsgsm.sit_nom_court,
# t_ani_gpsgsm.teq_nom_court,
# t_ani_gpsgsm.eqt_id_usuel,
# t_ani_gpsgsm.eqt_id,
# t_ani_gpsgsm.sen_association,
# t_ani_gpsgsm.mar_libelle,
# t_ani_gpsgsm.mod_libelle,
# t_ani_gpsgsm.eqa_date_debut,
# t_ani_gpsgsm.eqa_date_fin
# --v_ani_gpsgsm.ani_mortalite,
# --v_ani_gpsgsm.ani_date_mort,
# --v_ani_gpsgsm.ani_cause_mort,
# --v_ani_gpsgsm.eqa_activite,
# --v_ani_gpsgsm.eqa_probleme,
# --v_ani_gpsgsm.eqa_date_fin_text,
# --v_ani_gpsgsm.eqa_date_fin_arrondi,
# --v_ani_gpsgsm.ani_date_mort_arrondi,
# --v_ani_gpsgsm.ani_date_mort_text,
# --v_ani_gpsgsm.ani_poids_mort,
# --v_ani_gpsgsm.ani_poids_mort_na,
# --v_ani_gpsgsm.ani_remarque,
# --v_ani_gpsgsm.ani_mort_x,
# --v_ani_gpsgsm.ani_mort_y,
# --v_ani_gpsgsm.ani_inconnu,
# --v_ani_gpsgsm.cap_faon,
# --v_ani_gpsgsm.cap_age,
# --v_ani_gpsgsm.cap_age_corrige,
# --v_ani_gpsgsm.cap_circou,
# --v_ani_gpsgsm.cap_etat_sante,
# --v_ani_gpsgsm.cap_heure_lacher,
# --v_ani_gpsgsm.sit_id,
# --v_posi_gpsgsm.the_geom,
# --v_posi_gpsgsm.pos_x,
# --v_posi_gpsgsm.pos_y,
# --v_posi_gpsgsm.pos_systeme,
# --v_posi_gpsgsm.pos_z,
# --v_posi_gpsgsm.pos_x_corrige,
# --v_posi_gpsgsm.pos_y_corrige,
# --v_posi_gpsgsm.pos_z_corrige,
# --v_posi_gpsgsm.pos_nb_sat,
# --v_posi_gpsgsm.pos_dop,
# --v_posi_gpsgsm.pos_dop_corrige,
# --v_posi_gpsgsm.pos_fixstatus,
# --v_posi_gpsgsm.pos_validated,
# --v_posi_gpsgsm.cpos_date,
# --v_posi_gpsgsm.cpos_heure,
# --v_posi_gpsgsm.cpos_delta,
# --v_posi_gpsgsm.cpos_prog6heure,
# --v_posi_gpsgsm.cpos_prog4heure,
# --v_posi_gpsgsm.cpos_prog3heure,
# --v_posi_gpsgsm.cpos_prog1heure,
# --v_posi_gpsgsm.cpos_prog10minutes,
# --v_posi_gpsgsm.date_capture,
# --v_posi_gpsgsm.pos_distance_route,
# --v_posi_gpsgsm.pos_distance_bois,
# --v_posi_gpsgsm.pos_distance_bati,
# --v_posi_gpsgsm.pos_distance_haie,
# --v_posi_gpsgsm.pos_localisation_par_id,
# --tr_parcellaire_par.par_os,
# --tr_parcellaire_par.par_grd_cat,
# --tr_parcellaire_par.par_annee
# FROM historique.t_ani_gpsgsm
# JOIN v_posi_gpsgsm ON t_ani_gpsgsm.ani_id = v_posi_gpsgsm.ani_id AND t_ani_gpsgsm.cap_annee_suivi = v_posi_gpsgsm.cpos_annee_suivi
# --LEFT JOIN tr_parcellaire_par ON v_posi_gpsgsm.pos_localisation_par_id = tr_parcellaire_par.par_id AND v_posi_gpsgsm.cpos_annee_suivi =
#   -- tr_parcellaire_par.par_annee
# ORDER BY t_ani_gpsgsm.ani_id, v_posi_gpsgsm.cpos_date, v_posi_gpsgsm.cpos_heure
# WITH DATA;
# 
# ALTER TABLE historique.t_aniposi_gpsgsm
# OWNER TO ychaval;
# --GRANT ALL ON TABLE historique.t_aniposi_gpsgsm TO ychaval;
# --GRANT SELECT ON TABLE historique.t_aniposi_gpsgsm TO cefs_lecture;
# --GRANT ALL ON TABLE historique.t_aniposi_gpsgsm TO cefs_ecriture;
# 
# COMMENT ON table historique.t_aniposi_gpsgsm
# IS 'Table contenant toutes les informations des individus equipes de GPS et de GSM y compris les localisations, les distances et l assolement lorsque present';
# ")