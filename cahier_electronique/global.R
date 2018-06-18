library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(shinyBS)
library(shinyTime)
library(RPostgreSQL)
library(shinyalert)
library(chron)
source("connect.R")


dbSendQuery(con,"
DROP SCHEMA IF EXISTS historique cascade;
CREATE SCHEMA historique;

DROP TABLE IF EXISTS historique.t_ani_gpsgsm;

CREATE TABLE historique.t_ani_gpsgsm AS
SELECT t_animal_ani.ani_id,
t_animal_ani.ani_etiq,
--t_capture_cap.cap_bague,
t_capture_cap.cap_annee_suivi,
t_capture_cap.cap_date,
t_animal_ani.ani_sexe,
t_capture_cap.cap_age_classe,
t_capture_cap.cap_poids,
t_capture_cap.cap_lpa,
tr_site_capture_sit.sit_nom_court,
tr_type_equipement_teq.teq_nom_court,
t_equipement_eqt.eqt_id_usuel,
t_equipement_eqt.eqt_id,
tr_sensors_sen.sen_association,
tr_eqtmarque_mar.mar_libelle,
tr_eqtmodel_mod.mod_libelle,
tj_equipement_animal_eqt_ani_eqa.eqa_date_debut,
tj_equipement_animal_eqt_ani_eqa.eqa_date_fin,
--t_animal_ani.ani_mortalite,
--t_animal_ani.ani_date_mort,
--t_animal_ani.ani_cause_mort,
--tj_equipement_animal_eqt_ani_eqa.eqa_activite,
tj_equipement_animal_eqt_ani_eqa.eqa_probleme,
--tj_equipement_animal_eqt_ani_eqa.eqa_date_fin_text,
--tj_equipement_animal_eqt_ani_eqa.eqa_date_fin_arrondi,
--t_animal_ani.ani_date_mort_arrondi,
--t_animal_ani.ani_date_mort_text,
--t_animal_ani.ani_poids_mort,
--t_animal_ani.ani_poids_mort_na,
--t_animal_ani.ani_remarque,
--t_animal_ani.ani_mort_x,
--t_animal_ani.ani_mort_y,
--t_animal_ani.ani_inconnu,
t_capture_cap.cap_faon,
--t_capture_cap.cap_age,
--t_capture_cap.cap_age_corrige,
t_capture_cap.cap_circou
--t_capture_cap.cap_etat_sante,
--t_capture_cap.cap_heure_lacher,
--tr_site_capture_sit.sit_id
FROM t_capture_cap
LEFT JOIN t_animal_ani ON t_capture_cap.cap_ani_id = t_animal_ani.ani_id
LEFT JOIN tj_equipement_animal_eqt_ani_eqa ON tj_equipement_animal_eqt_ani_eqa.eqa_ani_id = t_capture_cap.cap_ani_id AND tj_equipement_animal_eqt_ani_eqa.eqa_annee_suivi = t_capture_cap.cap_annee_suivi
LEFT JOIN t_equipement_eqt ON t_equipement_eqt.eqt_id = tj_equipement_animal_eqt_ani_eqa.eqa_eqt_id
LEFT JOIN tr_type_equipement_teq ON tr_type_equipement_teq.teq_id = t_equipement_eqt.eqt_teq_id
LEFT JOIN tr_site_capture_sit ON t_capture_cap.cap_sit_id = tr_site_capture_sit.sit_id
LEFT JOIN tr_eqtmarque_mar ON t_equipement_eqt.eqt_mar_id = tr_eqtmarque_mar.mar_id
LEFT JOIN tr_eqtmodel_mod ON t_equipement_eqt.eqt_mod_id = tr_eqtmodel_mod.mod_id
LEFT JOIN t_equipement_conf_eqc ON t_equipement_eqt.eqt_id = t_equipement_conf_eqc.eqc_eqt_id and t_equipement_conf_eqc.eqc_annee_suivi = t_capture_cap.cap_annee_suivi
LEFT JOIN lu_tables.tr_sensors_sen ON tr_sensors_sen.sen_id = t_equipement_conf_eqc.eqc_sen_id
WHERE (tr_type_equipement_teq.teq_id = ANY (ARRAY[2, 3])) AND (t_capture_cap.cap_id IN ( SELECT min(t_capture_cap_1.cap_id) AS min
                                                                                         FROM t_capture_cap t_capture_cap_1
                                                                                         GROUP BY t_capture_cap_1.cap_ani_id, t_capture_cap_1.cap_annee_suivi)) AND (tr_type_equipement_teq.teq_id = ANY (ARRAY[2, 3]))
ORDER BY t_capture_cap.cap_annee_suivi, t_capture_cap.cap_date, t_animal_ani.ani_etiq;

ALTER TABLE historique.t_ani_gpsgsm
OWNER TO ychaval;
--GRANT ALL ON TABLE historique.t_ani_gpsgsm TO ychaval;
--GRANT SELECT ON TABLE historique.t_ani_gpsgsm TO cefs_lecture;
--GRANT ALL ON TABLE historique.t_ani_gpsgsm TO cefs_ecriture;

COMMENT ON TABLE historique.t_ani_gpsgsm
IS 'Table contenant toutes les informations des individus équipés de GPS et de GSM (en dehors des locs) - si 2 captures la même année seule la première est conservée';


DROP TABLE IF EXISTS historique.t_aniposi_gpsgsm;

CREATE TABLE historique.t_aniposi_gpsgsm AS
SELECT v_posi_gpsgsm.cpos_id,
--v_ani_gpsgsm.ani_id,
t_ani_gpsgsm.ani_etiq,
--v_ani_gpsgsm.cap_bague,
t_ani_gpsgsm.cap_annee_suivi,
t_ani_gpsgsm.cap_date,
t_ani_gpsgsm.ani_sexe,
t_ani_gpsgsm.cap_age_classe,
t_ani_gpsgsm.cap_poids,
t_ani_gpsgsm.cap_lpa,
t_ani_gpsgsm.sit_nom_court,
t_ani_gpsgsm.teq_nom_court,
t_ani_gpsgsm.eqt_id_usuel,
t_ani_gpsgsm.eqt_id,
t_ani_gpsgsm.sen_association,
t_ani_gpsgsm.mar_libelle,
t_ani_gpsgsm.mod_libelle,
t_ani_gpsgsm.eqa_date_debut,
t_ani_gpsgsm.eqa_date_fin
--v_ani_gpsgsm.ani_mortalite,
--v_ani_gpsgsm.ani_date_mort,
--v_ani_gpsgsm.ani_cause_mort,
--v_ani_gpsgsm.eqa_activite,
--v_ani_gpsgsm.eqa_probleme,
--v_ani_gpsgsm.eqa_date_fin_text,
--v_ani_gpsgsm.eqa_date_fin_arrondi,
--v_ani_gpsgsm.ani_date_mort_arrondi,
--v_ani_gpsgsm.ani_date_mort_text,
--v_ani_gpsgsm.ani_poids_mort,
--v_ani_gpsgsm.ani_poids_mort_na,
--v_ani_gpsgsm.ani_remarque,
--v_ani_gpsgsm.ani_mort_x,
--v_ani_gpsgsm.ani_mort_y,
--v_ani_gpsgsm.ani_inconnu,
--v_ani_gpsgsm.cap_faon,
--v_ani_gpsgsm.cap_age,
--v_ani_gpsgsm.cap_age_corrige,
--v_ani_gpsgsm.cap_circou,
--v_ani_gpsgsm.cap_etat_sante,
--v_ani_gpsgsm.cap_heure_lacher,
--v_ani_gpsgsm.sit_id,
--v_posi_gpsgsm.the_geom,
--v_posi_gpsgsm.pos_x,
--v_posi_gpsgsm.pos_y,
--v_posi_gpsgsm.pos_systeme,
--v_posi_gpsgsm.pos_z,
--v_posi_gpsgsm.pos_x_corrige,
--v_posi_gpsgsm.pos_y_corrige,
--v_posi_gpsgsm.pos_z_corrige,
--v_posi_gpsgsm.pos_nb_sat,
--v_posi_gpsgsm.pos_dop,
--v_posi_gpsgsm.pos_dop_corrige,
--v_posi_gpsgsm.pos_fixstatus,
--v_posi_gpsgsm.pos_validated,
--v_posi_gpsgsm.cpos_date,
--v_posi_gpsgsm.cpos_heure,
--v_posi_gpsgsm.cpos_delta,
--v_posi_gpsgsm.cpos_prog6heure,
--v_posi_gpsgsm.cpos_prog4heure,
--v_posi_gpsgsm.cpos_prog3heure,
--v_posi_gpsgsm.cpos_prog1heure,
--v_posi_gpsgsm.cpos_prog10minutes,
--v_posi_gpsgsm.date_capture,
--v_posi_gpsgsm.pos_distance_route,
--v_posi_gpsgsm.pos_distance_bois,
--v_posi_gpsgsm.pos_distance_bati,
--v_posi_gpsgsm.pos_distance_haie,
--v_posi_gpsgsm.pos_localisation_par_id,
--tr_parcellaire_par.par_os,
--tr_parcellaire_par.par_grd_cat,
--tr_parcellaire_par.par_annee
FROM historique.t_ani_gpsgsm
JOIN v_posi_gpsgsm ON t_ani_gpsgsm.ani_id = v_posi_gpsgsm.ani_id AND t_ani_gpsgsm.cap_annee_suivi = v_posi_gpsgsm.cpos_annee_suivi
--LEFT JOIN tr_parcellaire_par ON v_posi_gpsgsm.pos_localisation_par_id = tr_parcellaire_par.par_id AND v_posi_gpsgsm.cpos_annee_suivi =
  -- tr_parcellaire_par.par_annee
ORDER BY t_ani_gpsgsm.ani_id, v_posi_gpsgsm.cpos_date, v_posi_gpsgsm.cpos_heure
WITH DATA;

ALTER TABLE historique.t_aniposi_gpsgsm
OWNER TO ychaval;
--GRANT ALL ON TABLE historique.t_aniposi_gpsgsm TO ychaval;
--GRANT SELECT ON TABLE historique.t_aniposi_gpsgsm TO cefs_lecture;
--GRANT ALL ON TABLE historique.t_aniposi_gpsgsm TO cefs_ecriture;

COMMENT ON table historique.t_aniposi_gpsgsm
IS 'Table contenant toutes les informations des individus équipés de GPS et de GSM y compris les localisations, les distances et l assolement lorsque présent';
")