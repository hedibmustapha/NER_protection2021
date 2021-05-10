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

# read non compiled data and all options that were wrongly coded in the tool (replace / with _)
vbg<-read_excel("input/NER2102_dataset_VBG-PE.xlsm", sheet = "Clean Data") %>% prepdata(.,T)

# read questionnaire/ how to aggregate variables and correction file for skip logic
questions<-read_excel("tools/REACH_Evaluation_Protection_E_VBG.xlsx","survey")
choices<-read_excel("tools/REACH_Evaluation_Protection_E_VBG.xlsx","choices")
champs_synthese<-read_excel("input/asset/champs_synthese.xlsx","VBG_PE")
recode_sl<-read_excel("input/asset/recode_sl.xlsx","vbg")

# to have compiled data that match the input data
template_data <- vbg[0,]
template_data<-data.frame(lapply(template_data, as.character), stringsAsFactors=FALSE)

# group by
groupGeo<-c("departement","commune","localite")

# classify variables with their respective aggregation variable
champs_absence_bad <- na.omit(ifelse(champs_synthese[["fct"]]=="absence_bad",champs_synthese[["qname"]],NA))
champs_presence_bad <- na.omit(ifelse(champs_synthese[["fct"]]=="presence_bad",champs_synthese[["qname"]],NA))
champs_absence_bad_no <- na.omit(ifelse(champs_synthese[["fct"]]=="absence_bad_no",champs_synthese[["qname"]],NA))
champs_absence_bad2 <- na.omit(ifelse(champs_synthese[["fct"]]=="absence_bad2",champs_synthese[["qname"]],NA))
champs_worse_togood <- na.omit(ifelse(champs_synthese[["fct"]]=="worse_togood",champs_synthese[["qname"]],NA))
champs_majority_bad<-na.omit(ifelse(champs_synthese[["fct"]]=="majority_bad",champs_synthese[["qname"]],NA))
champs_evolution<-na.omit(ifelse(champs_synthese[["fct"]]=="evolution",champs_synthese[["qname"]],NA))
champs_collapse_sm<-na.omit(ifelse(champs_synthese[["fct"]]=="collapse_sm",champs_synthese[["qname"]],NA))

# apply aggregation
sites_absence_bad <- sites_agg(vbg,groupGeo,champs_absence_bad,absence_bad)
sites_presence_bad <- sites_agg(vbg,groupGeo,champs_presence_bad,presence_bad)
sites_absence_bad_no <- sites_agg(vbg,groupGeo,champs_absence_bad_no,absence_bad_no)
sites_absence_bad2 <- sites_agg(vbg,groupGeo,champs_absence_bad2,absence_bad2)
sites_worse_togood <- sites_agg(vbg,groupGeo,champs_worse_togood,worse_togood)
sites_majority_bad <- sites_agg(vbg,groupGeo,champs_majority_bad,majority_bad)
sites_evolution <- sites_agg(vbg,groupGeo,champs_evolution,evolution)
sites_collapse_sm<-sites_agg(vbg,groupGeo,champs_collapse_sm,collapse_sm)

# join all pieces into one dataset
sites<- sites_absence_bad %>%
  left_join(sites_presence_bad,by = groupGeo)%>%
  left_join(sites_absence_bad_no,by = groupGeo) %>%
  left_join(sites_absence_bad2,by = groupGeo) %>%
  left_join(sites_worse_togood,by = groupGeo) %>%
  left_join(sites_majority_bad,by = groupGeo) %>%
  left_join(sites_evolution,by = groupGeo)%>%
  left_join(sites_collapse_sm,by=groupGeo)

# fix all binary columns for select_multiple question and correct for skip logic
sites<-bind_rows(template_data,sites) %>%prepdata(.,T)
sites<-split_multiple_choice(sites,questions,choices,".")
# Skip Logic as empty cells
# sites_slempty<-sl_correction(sites,recode_sl,questions,sl = "")
# Skip Logic as SL
sites_sl<-sl_correction(sites,recode_sl,questions,sl = "SL")
# Skip Logic as NA (for analysis purpose)
sites_slna<-sl_correction(sites,recode_sl,questions)


# Analysis on aggregated data
analyze_sl <- sites_sl %>% select_if(~ !(all(is.na(.x)))) %>% select(-localite) %>%
  mutate(departement = paste0(departement,"_dep"),
         commune = paste0(commune,"_comm"))
xml_sl <-table_maker(data = analyze_sl,
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
                                  "commune")

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
                                      "commune")

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
                                        "commune")

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
                                  "commune")
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
                                    "commune")

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
                                        "commune")

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
                                          "commune")

# Analysis on non aggregated data
analyze_data <- vbg %>% select(names(analyze_sl))%>% select_if(~ !(all(is.na(.x)))) %>%
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
                         "commune")

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
                           "commune")

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
                               "commune")

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
                                 "commune")

writexl::write_xlsx(
  x = list(noncompiled_data=vbg,
           compiled=sites_sl,
           noncompiled_xml_results=xml,
           noncompiled_labeled_results=label,
           noncompiled_xml_results_counts=xml_count,
           noncompiled_labeled_counts=label_count,
           compiled_xml_results=xml_sl,
           compiled_labeled_results=label_sl,
           compiled_xml_results_counts=xml_count_sl,
           compiled_labeled_counts=label_count_sl,
           compiled_xml_results_slna=xml_slna,
           compiled_labeled_slna=label_slna,
           compiled_xml_counts_slna=xml_count_slna,
           compiled_labeled_counts_slna=label_count_slna),
  path = paste0("output/analysis/Prot_VBG-",humanTime(),".xlsx"),
  col_names = T,
  format_headers = T
)
