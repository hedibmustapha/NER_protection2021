rm(list = ls())
# Load libraries and internal functions
library(readxl)
library(stringr)
library(readr)
library(dplyr)
library(writexl)
library(questionr)
library(tidyverse)
library(crayon)
library(composr)
source("code/functions.R")
source("code/table_function.R")
source("code/agg_functions.R")

# read non compiled data and fix all options that were wrongly coded in the tool (replace / with _)
general<-read_excel("input/NER2102_dataset_GEN.xlsm", sheet = "Clean Data") %>% prepdata(.,T)
fix_slash<-c("source_info","sourceinfoah","pop_affectee","groupe_popconcerne","pers_vulnerables","manque_pieces")
general[,fix_slash]<- purrr::map_df(general[,fix_slash],fixslash)

# read questionnaire/ how to aggregate variables and correction file for skip logic
questions<-read_excel("tools/REACH_Evaluation_Protection_General.xlsx","survey")
choices<-read_excel("tools/REACH_Evaluation_Protection_General.xlsx","choices")
champs_synthese<-read_excel("input/asset/champs_synthese.xlsx","Prot generale")
recode_sl<-read_excel("input/asset/recode_sl.xlsx","general")

# to have compiled data that match the input data
template_data <- general[0,]
template_data<-data.frame(lapply(template_data, as.character), stringsAsFactors=FALSE)

# group by
groupGeo<-c("departement","commune","localite","sexe_enquete")
groupGeoadmin<-c("departement","commune","localite")

# classify variables with their respective aggregation variable
champs_absence_bad <- na.omit(ifelse(champs_synthese[["fct"]]=="absence_bad",champs_synthese[["qname"]],NA))
champs_presence_bad <- na.omit(ifelse(champs_synthese[["fct"]]=="presence_bad",champs_synthese[["qname"]],NA))
champs_sitsecu <- na.omit(ifelse(champs_synthese[["fct"]]=="sitsecu",champs_synthese[["qname"]],NA))
champs_nbreincident <- na.omit(ifelse(champs_synthese[["fct"]]=="nbreincident",champs_synthese[["qname"]],NA))
champs_evolutionsecu <- na.omit(ifelse(champs_synthese[["fct"]]=="evolutionsecu",champs_synthese[["qname"]],NA))
champs_occurrence_enlevement <- na.omit(ifelse(champs_synthese[["fct"]]=="occurrence_enlevement",champs_synthese[["qname"]],NA))
champs_absence_bad_no <- na.omit(ifelse(champs_synthese[["fct"]]=="absence_bad_no",champs_synthese[["qname"]],NA))
champs_majority_bad <- na.omit(ifelse(champs_synthese[["fct"]]=="majority_bad",champs_synthese[["qname"]],NA))
champs_temps <- na.omit(ifelse(champs_synthese[["fct"]]=="temps",champs_synthese[["qname"]],NA))
champs_nb_deplcement <- na.omit(ifelse(champs_synthese[["fct"]]=="nb_deplcement",champs_synthese[["qname"]],NA))
champs_relation <- na.omit(ifelse(champs_synthese[["fct"]]=="relation",champs_synthese[["qname"]],NA))
champs_mean_agg <- na.omit(ifelse(champs_synthese[["fct"]]=="mean_agg",champs_synthese[["qname"]],NA))
champs_collapse_sm<-na.omit(ifelse(champs_synthese[["fct"]]=="collapse_sm",champs_synthese[["qname"]],NA))
champs_aok_mode<-na.omit(ifelse(champs_synthese[["fct"]]=="aok_mode",champs_synthese[["qname"]],NA))

# apply aggregation
sites_absence_bad <- sites_agg(general,groupGeo,champs_absence_bad,absence_bad)
sites_presence_bad <- sites_agg(general,groupGeo,champs_presence_bad,presence_bad)
sites_sitsecu <- sites_agg(general,groupGeo,champs_sitsecu,sitsecu)
sites_nbreincident <- sites_agg(general,groupGeo,champs_nbreincident,nbreincident)
sites_evolutionsecu <- sites_agg(general,groupGeo,champs_evolutionsecu,evolutionsecu)
sites_occurrence_enlevement <- sites_agg(general,groupGeo,champs_occurrence_enlevement,occurrence_enlevement)
sites_absence_bad_no <- sites_agg(general,groupGeo,champs_absence_bad_no,absence_bad_no)
sites_majority_bad <- sites_agg(general,groupGeo,champs_majority_bad,majority_bad)
sites_temps <- sites_agg(general,groupGeo,champs_temps,temps)
sites_nb_deplcement <- sites_agg(general,groupGeo,champs_nb_deplcement,nb_deplcement)
sites_relation <- sites_agg(general,groupGeo,champs_relation,relation)
sites_mean_agg <- sites_agg(general,groupGeo,champs_mean_agg,mean_agg)
sites_collapse_sm<-sites_agg(general,groupGeo,champs_collapse_sm,collapse_sm)
sites_aok_mode<-sites_agg(general,groupGeo,champs_aok_mode,aok_mode)


# join all pieces into one dataset
sites<- sites_absence_bad %>%
  left_join(sites_presence_bad,by = groupGeo)%>%
  left_join(sites_sitsecu,by = groupGeo) %>%
  left_join(sites_nbreincident,by = groupGeo) %>%
  left_join(sites_evolutionsecu,by = groupGeo) %>%
  left_join(sites_occurrence_enlevement,by = groupGeo) %>%
  left_join(sites_absence_bad_no,by = groupGeo) %>%
  left_join(sites_majority_bad,by = groupGeo) %>%
  left_join(sites_temps,by = groupGeo)%>%
  left_join(sites_nb_deplcement,by=groupGeo) %>%
  left_join(sites_relation,by=groupGeo)%>%
  left_join(sites_mean_agg,by=groupGeo)%>%
  left_join(sites_collapse_sm,by=groupGeo)%>%
  left_join(sites_aok_mode,by=groupGeo)

# fix all binary columns for select_multiple question and correct for skip logic
sites<-bind_rows(template_data,sites) %>%prepdata(.,T)
sites<-split_multiple_choice(sites,questions,choices,".")
# Skip Logic as empty cells
# sites_slempty<-sl_correction(sites,recode_sl,questions,sl = "")
# Skip Logic as SL
sites_sl<-sl_correction(sites,recode_sl,questions,sl = "SL")
# Skip Logic as NA (for analysis purpose)
sites_slna<-sl_correction(sites,recode_sl,questions)

# apply aggregation admin
sites_absence_bad_admin <- sites_agg(sites_slna,groupGeoadmin,champs_absence_bad,absence_bad)
sites_presence_bad_admin <- sites_agg(sites_slna,groupGeoadmin,champs_presence_bad,presence_bad)
sites_sitsecu_admin <- sites_agg(sites_slna,groupGeoadmin,champs_sitsecu,sitsecu)
sites_nbreincident_admin <- sites_agg(sites_slna,groupGeoadmin,champs_nbreincident,nbreincident)
sites_evolutionsecu_admin <- sites_agg(sites_slna,groupGeoadmin,champs_evolutionsecu,evolutionsecu)
sites_occurrence_enlevement_admin <- sites_agg(sites_slna,groupGeoadmin,champs_occurrence_enlevement,occurrence_enlevement)
sites_absence_bad_no_admin <- sites_agg(sites_slna,groupGeoadmin,champs_absence_bad_no,absence_bad_no)
sites_majority_bad_admin <- sites_agg(sites_slna,groupGeoadmin,champs_majority_bad,majority_bad)
sites_temps_admin <- sites_agg(sites_slna,groupGeoadmin,champs_temps,temps)
sites_nb_deplcement_admin <- sites_agg(sites_slna,groupGeoadmin,champs_nb_deplcement,nb_deplcement)
sites_relation_admin <- sites_agg(sites_slna,groupGeoadmin,champs_relation,relation)
sites_mean_agg_admin <- sites_agg(sites_slna,groupGeoadmin,champs_mean_agg,mean_agg)
sites_collapse_sm_admin<-sites_agg(sites_slna,groupGeoadmin,champs_collapse_sm,collapse_sm)
sites_aok_mode_admin<-sites_agg(sites_slna,groupGeoadmin,champs_aok_mode,aok_mode)

# join all pieces into one dataset
sites_admin<- sites_absence_bad_admin %>%
  left_join(sites_presence_bad_admin,by = groupGeoadmin)%>%
  left_join(sites_sitsecu_admin,by = groupGeoadmin) %>%
  left_join(sites_nbreincident_admin,by = groupGeoadmin) %>%
  left_join(sites_evolutionsecu_admin,by = groupGeoadmin) %>%
  left_join(sites_occurrence_enlevement_admin,by = groupGeoadmin) %>%
  left_join(sites_absence_bad_no_admin,by = groupGeoadmin) %>%
  left_join(sites_majority_bad_admin,by = groupGeoadmin) %>%
  left_join(sites_temps_admin,by = groupGeoadmin)%>%
  left_join(sites_nb_deplcement_admin,by=groupGeoadmin) %>%
  left_join(sites_relation_admin,by=groupGeoadmin)%>%
  left_join(sites_mean_agg_admin,by=groupGeoadmin)%>%
  left_join(sites_collapse_sm_admin,by=groupGeoadmin)%>%
  left_join(sites_aok_mode_admin,by=groupGeoadmin)

# fix all binary columns for select_multiple question and correct for skip logic
sites_admin<-bind_rows(template_data,sites_admin) %>%prepdata(.,T)
sites_admin<-split_multiple_choice(sites_admin,questions,choices,".")
# Skip Logic as empty cells
# sites_admin_slempty<-sl_correction(sites_admin,recode_sl,questions,sl = "")
# Skip Logic as SL
sites_admin_sl<-sl_correction(sites_admin,recode_sl,questions,sl = "SL")
# Skip Logic as NA (for analysis purpose)
sites_admin_slna<-sl_correction(sites_admin,recode_sl,questions)


# Analysis on aggregated data
analyze_sl <- sites_sl %>% select_if(~ !(all(is.na(.x)))) %>% select(-localite) %>%
  mutate(departement = paste0(departement,"_dep"),
         commune = paste0(commune,"_comm"))
xml_sl <-table_maker(data = analyze_sl,
                             questionnaire = questions,
                             choices = choices,
                             labels = F,
                             language = NULL,
                             main_col_name = "Overall",
                             count = NULL,
                     aggregated = TRUE,
                     "departement",
                     "commune",
                     "sexe_enquete")

label_sl <-table_maker(data = analyze_sl,
                               questionnaire = questions,
                               choices = choices,
                               weighting_function = NULL,
                               labels = T,
                               language = NULL,
                               main_col_name= "Overall",
                               count = NULL,
                       aggregated = TRUE,
                       "departement",
                       "commune",
                       "sexe_enquete")

xml_count_sl <-table_maker(data = analyze_sl,
                             questionnaire = questions,
                             choices = choices,
                             weighting_function = NULL,
                             labels = F,
                             language = NULL,
                             main_col_name = "Overall",
                             count = TRUE,
                           aggregated = TRUE,
                           "departement",
                           "commune",
                           "sexe_enquete")

label_count_sl <-table_maker(data = analyze_sl,
                               questionnaire = questions,
                               choices = choices,
                               weighting_function = NULL,
                               labels = T,
                               language = NULL,
                               main_col_name= "Overall",
                               count = TRUE,
                             aggregated = TRUE,
                             "departement",
                             "commune",
                             "sexe_enquete")

##### Analysis on aggregated data with skip logic coded as NAs
analyze_slna <- sites_slna %>% select_if(~ !(all(is.na(.x)))) %>% select(-localite) %>% 
  mutate(departement = paste0(departement,"_dep"),
         commune = paste0(commune,"_comm"))
xml_slna <-table_maker(data = analyze_slna,
                             questionnaire = questions,
                             choices = choices,
                             weighting_function = NULL,
                             labels = F,
                             language = NULL,
                             main_col_name = "Overall",
                             count = NULL,
                       aggregated = TRUE,
                       "departement",
                       "commune",
                       "sexe_enquete")

label_slna <-table_maker(data = analyze_slna,
                               questionnaire = questions,
                               choices = choices,
                               weighting_function = NULL,
                               labels = T,
                               language = NULL,
                               main_col_name= "Overall",
                               count = NULL,
                         aggregated = TRUE,
                         "departement",
                         "commune",
                         "sexe_enquete")

xml_count_slna <-table_maker(data = analyze_slna,
                                   questionnaire = questions,
                                   choices = choices,
                                   weighting_function = NULL,
                                   labels = F,
                                   language = NULL,
                                   main_col_name = "Overall",
                                   count = TRUE,
                             aggregated = TRUE,
                             "departement",
                             "commune",
                             "sexe_enquete")

label_count_slna <-table_maker(data = analyze_slna,
                                     questionnaire = questions,
                                     choices = choices,
                                     weighting_function = NULL,
                                     labels = T,
                                     language = NULL,
                                     main_col_name= "Overall",
                                     count = TRUE,
                               aggregated = TRUE,
                               "departement",
                               "commune",
                               "sexe_enquete")

# Analysis on non aggregated data
analyze_data <- general %>% select(names(analyze_sl)) %>% select_if(~ !(all(is.na(.x)))) %>%
  mutate(departement = paste0(departement,"_dep"),
         commune = paste0(commune,"_comm"))
xml <-table_maker(data = analyze_data,
                             questionnaire = questions,
                             choices = choices,
                             weighting_function = NULL,
                             labels = F,
                             language = NULL,
                             main_col_name = "Overall",
                             count = NULL,
                             aggregated = FALSE,
                  "departement",
                  "commune",
                  "sexe_enquete")

label <-table_maker(data = analyze_data,
                               questionnaire = questions,
                               choices = choices,
                               weighting_function = NULL,
                               labels = T,
                               language = NULL,
                               main_col_name= "Overall",
                               count = NULL, 
                              aggregated = FALSE,
                    "departement",
                    "commune",
                    "sexe_enquete")

xml_count <-table_maker(data = analyze_data,
                                   questionnaire = questions,
                                   choices = choices,
                                   weighting_function = NULL,
                                   labels = F,
                                   language = NULL,
                                   main_col_name = "Overall",
                                   count = TRUE, 
                                  aggregated = FALSE,
                        "departement",
                        "commune",
                        "sexe_enquete")

label_count <-table_maker(data = analyze_data,
                                     questionnaire = questions,
                                     choices = choices,
                                     weighting_function = NULL,
                                     labels = T,
                                     language = NULL,
                                     main_col_name= "Overall",
                                     count = TRUE, 
                                    aggregated = FALSE,
                          "departement",
                          "commune",
                          "sexe_enquete")

# Analysis on aggregated data admin
analyze_admin_sl <- sites_admin_sl %>% select_if(~ !(all(is.na(.x)))) %>% select(-localite) %>%
  mutate(departement = paste0(departement,"_dep"),
         commune = paste0(commune,"_comm"))
admin_xml_sl <-table_maker(data = analyze_admin_sl,
                                questionnaire = questions,
                                choices = choices,
                                weighting_function = NULL,
                                labels = F,
                                language = NULL,
                                main_col_name = "Overall",
                                count = NULL,
                           aggregated = TRUE,
                           "departement",
                           "commune")

admin_label_sl <-table_maker(data = analyze_admin_sl,
                                  questionnaire = questions,
                                  choices = choices,
                                  weighting_function = NULL,
                                  labels = T,
                                  language = NULL,
                                  main_col_name= "Overall",
                                  count = NULL,
                             aggregated = TRUE,
                             "departement",
                             "commune")

admin_xml_count_sl <-table_maker(data = analyze_admin_sl,
                                      questionnaire = questions,
                                      choices = choices,
                                      weighting_function = NULL,
                                      labels = F,
                                      language = NULL,
                                      main_col_name = "Overall",
                                      count = TRUE,
                                 aggregated = TRUE,
                                 "departement",
                                 "commune")

admin_label_count_sl <-table_maker(data = analyze_admin_sl,
                                        questionnaire = questions,
                                        choices = choices,
                                        weighting_function = NULL,
                                        labels = T,
                                        language = NULL,
                                        main_col_name= "Overall",
                                        count = TRUE,
                                   aggregated = TRUE,
                                   "departement",
                                   "commune")

##### Analysis on aggregated data admin with skip logic coded as NAs
analyze_admin_slna <- sites_admin_slna %>% select_if(~ !(all(is.na(.x)))) %>% select(-localite)%>%
  mutate(departement = paste0(departement,"_dep"),
         commune = paste0(commune,"_comm"))
admin_xml_slna <-table_maker(data = analyze_admin_slna,
                                  questionnaire = questions,
                                  choices = choices,
                                  weighting_function = NULL,
                                  labels = F,
                                  language = NULL,
                                  main_col_name = "Overall",
                                  count = NULL,
                             aggregated = TRUE,
                             "departement",
                             "commune")

admin_label_slna <-table_maker(data = analyze_admin_slna,
                                    questionnaire = questions,
                                    choices = choices,
                                    weighting_function = NULL,
                                    labels = T,
                                    language = NULL,
                                    main_col_name= "Overall",
                                    count = NULL,
                               aggregated = TRUE,
                               "departement",
                               "commune")

admin_xml_count_slna <-table_maker(data = analyze_admin_slna,
                                        questionnaire = questions,
                                        choices = choices,
                                        weighting_function = NULL,
                                        labels = F,
                                        language = NULL,
                                        main_col_name = "Overall",
                                        count = TRUE,
                                   aggregated = TRUE,
                                   "departement",
                                   "commune")

admin_label_count_slna <-table_maker(data = analyze_admin_slna,
                                          questionnaire = questions,
                                          choices = choices,
                                          weighting_function = NULL,
                                          labels = T,
                                          language = NULL,
                                          main_col_name= "Overall",
                                          count = TRUE,
                                     aggregated = TRUE,
                                     "departement",
                                     "commune")

writexl::write_xlsx(
  x = list(data=general,
           xml_results=xml,
           labeled_results=label,
           xml_counts=xml_count,
           labeled_counts=label_count),
  path = paste0("output/analysis/Prot_General_nonaggrege-",humanTime(),".xlsx"),
  col_names = T,
  format_headers = T
)

writexl::write_xlsx(
  x = list(data=sites_sl,
           xml_results=xml_sl,
           labeled_results=label_sl,
           xml_counts=xml_count_sl,
           labeled_counts=label_count_sl,
           xml_results_slNA=xml_slna,
           labeled_results_slNA=label_slna,
           xml_counts_slNA=xml_count_slna,
           labeled_counts_slNA=label_count_slna),
  path = paste0("output/analysis/Prot_General_aggrege_sexe-",humanTime(),".xlsx"),
  col_names = T,
  format_headers = T
)

writexl::write_xlsx(
  x = list(data=sites_admin_sl,
           xml_results=admin_xml_sl,
           labeled_results=admin_label_sl,
           xml_counts=admin_xml_count_sl,
           labeled_counts=admin_label_count_sl,
           xml_results_slNA=admin_xml_slna,
           labeled_results_slNA=admin_label_slna,
           xml_counts_slNA=admin_xml_count_slna,
           labeled_counts_slNA=admin_label_count_slna),
  path = paste0("output/analysis/Prot_General_aggrege_admin-",humanTime(),".xlsx"),
  col_names = T,
  format_headers = T
)

writexl::write_xlsx(
  x = list(xml_results=admin_xml_sl,
           labeled_results=admin_label_sl,
           xml_counts=admin_xml_count_sl,
           labeled_counts=admin_label_count_sl),
  path = paste0("output/analysis/Prot_General_aggrege_admin-",humanTime(),".xlsx"),
  col_names = T,
  format_headers = T
)
