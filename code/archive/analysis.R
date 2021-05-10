library(readxl)
library(stringr)
library(readr)
library(dplyr)
library(writexl)
library(questionr)
library(tidyverse)
library(crayon)
source("code/functions.R")
source("code/table_function.R")

general<-read_excel("input/Prot_Gen.xlsx") %>% prepdata(.,T) %>% select_if(~ !(all(is.na(.x))))
referencement<-read_excel("input/Referencement.xlsx") %>% prepdata(.,T) %>% select_if(~ !(all(is.na(.x))))
vbg<-read_excel("input/VBG_PE.xlsx") %>% prepdata(.,T) %>% select_if(~ !(all(is.na(.x))))


questions_gen<-read_excel("tools/REACH_Evaluation_Protection_General.xlsx",1)
choices_gen<-read_excel("tools/REACH_Evaluation_Protection_General.xlsx",2)

questions_ref<-read_excel("tools/REACH_Evaluation_Protection_referencement.xlsx",1)
choices_ref<-read_excel("tools/REACH_Evaluation_Protection_referencement.xlsx",2)

questions_vbg<-read_excel("tools/REACH_Evaluation_Protection_E_VBG.xlsx",1)
choices_vbg<-read_excel("tools/REACH_Evaluation_Protection_E_VBG.xlsx",2)


tokeep_gen<-questions_gen$name[grepl(pattern = "(^select_|^calculate$|^integer$|^decimal$)",questions_gen$type)]
analyze_gen<-general %>% select(starts_with(tokeep_gen)) %>% select(-all_of(names(.)[grepl("_autre",names(.))]))%>% select_if(~ !(all(is.na(.x))))

tokeep_ref<-questions_ref$name[grepl(pattern = "(^select_|^calculate$|^integer$|^decimal$)",questions_ref$type)]
analyze_ref<- referencement %>% select(starts_with(tokeep_ref)) %>% select(-all_of(names(.)[grepl("_autre",names(.))]))

tokeep_vbg<-questions_vbg$name[grepl(pattern = "(^select_|^calculate$|^integer$|^decimal$)",questions_vbg$type)]
analyze_vbg<-vbg %>% select(starts_with(tokeep_vbg)) %>% select(-all_of(names(.)[grepl("_autre",names(.))]))

output_gen_xml <-table_maker(data = analyze_gen,
                         questionnaire = questions_gen,
                         choices = choices_gen,
                         weighting_function = NULL,
                         labels = F,
                         language = NULL,
                         main_col_name = "Overall",
                         "sexe_enquete")

output_gen_label <-table_maker(data = analyze_gen,
                         questionnaire = questions_gen,
                         choices = choices_gen,
                         weighting_function = NULL,
                         labels = T,
                         language = NULL,
                         main_col_name = "Overall",
                         "sexe_enquete")

output_ref_xml <-table_maker(data = analyze_ref,
                             questionnaire = questions_ref,
                             choices = choices_ref,
                             weighting_function = NULL,
                             labels = F,
                             language = NULL,
                             main_col_name = "Overall")

output_ref_label <-table_maker(data = analyze_ref,
                               questionnaire = questions_ref,
                               choices = choices_ref,
                               weighting_function = NULL,
                               labels = T,
                               language = NULL,
                               main_col_name = "Overall")

output_vbg_xml <-table_maker(data = analyze_vbg,
                             questionnaire = questions_vbg,
                             choices = choices_vbg,
                             weighting_function = NULL,
                             labels = F,
                             language = NULL,
                             main_col_name = "Overall")

output_vbg_label <-table_maker(data = analyze_vbg,
                               questionnaire = questions_vbg,
                               choices = choices_vbg,
                               weighting_function = NULL,
                               labels = T,
                               language = NULL,
                               main_col_name = "Overall")

writexl::write_xlsx(
  x = list(general_xml_results=output_gen_xml,
           general_labeled_results=output_gen_label,
           referencement_xml_results=output_ref_xml,
           referencement_labeled_results=output_ref_label,
           vbg_xml_results=output_vbg_xml,
           vbg_labeled_results=output_vbg_label),
  path = paste0("output/protection_analysis-",humanTime(),".xlsx"),
  col_names = T,
  format_headers = T
)
