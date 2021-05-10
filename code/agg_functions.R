collapse_sm <- function(x) {
  if(all(is.na(x))){
    return(NA_character_)
  }else{
    x <- x[which(x!=""&!is.na(x))]
    x <- paste(x, collapse = ' ')
    x<-vapply(lapply(strsplit(x, " "), unique), paste, character(1L), collapse = " ")
    return(x)
    }
}

absence_bad<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("non"%in%x){
      return("non")
  } else if("oui_minorite"%in%x){
      return("oui_minorite")
  } else if("oui_moitie"%in%x){
      return("oui_moitie")
  } else if("oui_majorite"%in%x){
      return("oui_majorite")
  } else if("oui_ensemble"%in%x){
      return("oui_ensemble")
  } else {
      return("ne_sait_pas")
    }
}

presence_bad<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("oui"%in%x){
    return("oui")
  } else if("non"%in%x){
    return("non")
  } else if("refus"%in%x){
    return("refus")
  }else{
    return("ne_sait_pas")
  }
}

sitsecu<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("incidents_frequents"%in%x){
    return("incidents_frequents")
  } else if("incidents_sporadiques"%in%x){
    return("incidents_sporadiques")
  } else if("incidents_rares"%in%x){
    return("incidents_rares")
  } else if("autre"){
    return("autre")
  } else {
    return("ne_sait_pas")
  }
}

nbreincident<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("plusde5"%in%x){
    return("plusde5")
  } else if("5"%in%x){
    return("5")
  } else if("4"%in%x){
    return("4")
  } else if("3"%in%x){
    return("3")
  } else if("2"%in%x){
    return("2")
  } else if("1"%in%x){
    return("1") 
  } else{
    return("aucun")
  }
}

evolutionsecu<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("augmente"%in%x){
    return("augmente")
  } else if("stable"%in%x){
    return("stable")
  }else if("diminue"%in%x){
    return("diminue")
  }else {
    return("ne_sait_pas")
  }
}

occurrence_enlevement<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("augmentation"%in%x){
    return("augmentation")
  }else if("inchange"%in%x){
    return("inchange")
  }else{
    return("diminution")
  }
}

absence_bad_no<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("non"%in%x){
    return("non")
  }else if("oui"%in%x){
    return("oui")
  }else if("ne_sait_pas"%in%x){
    return("ne_sait_pas")
  }else{
    return("refus")
  }
}


majority_bad<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("oui_ensemble"%in%x){
    return("oui_ensemble")
  } else if("oui_majorite"%in%x){
    return("oui_majorite")
  } else if("oui_moitie"%in%x){
    return("oui_moitie")
  } else if("oui_minorite"%in%x){
    return("oui_minorite")
  } else if("non"%in%x){
    return("non")
  } else {
    return("ne_sait_pas")
  }
}

temps<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("plus_de_1_an"%in%x){
    return("plus_de_1_an")
  }else if("entre_7_mois_et_1_an"%in%x){
    return("entre_7_mois_et_1_an")
  }else if("entre_4_et_6_mois"%in%x){
    return("entre_4_et_6_mois")
  }else if("entre_1_et_3_mois"%in%x){
    return("entre_1_et_3_mois")
  }else if("moins_de_1_mois"%in%x){
    return("moins_de_1_mois")
  }else{
    return("ne_sait_pas")
  }
}

nb_deplcement<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  }else if("4_fois_ou_plus"%in%x){
    return("4_fois_ou_plus")
  }else if("3_fois"%in%x){
    return("3_fois")
  }else if("2_fois"%in%x){
    return("2_fois")
  }else if("1_fois"%in%x){
    return("1_fois")
  }else{
    return("ne_sait_pas")
  }
}


relation<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("existence_de_tensions"%in%x){
    return("existence_de_tensions")
  }else if("accueil_periode_limitee"%in%x){
    return("accueil_periode_limitee")
  }else if("accueil_longtemps"%in%x){
    return("accueil_longtemps")
  }else if("autre"%in%x){
    return("autre")
  }else if("ne_sait_pas"%in%x){
    return("ne_sait_pas")
  }else{
    return(
      "refuse_de_repondre"
    )
  }
}

worse_togood<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  }else if("tres_mauvaise"%in%x){
    return("tres_mauvaise")
  }else if("mauvaise"%in%x){
    return("mauvaise")
  }else if("mitigee"%in%x){
    return("mitigee")
  }else if("bonne"%in%x){
    return("bonne")
  }else if("tres_bonne"%in%x){
    return("tres_bonne")
  }else{
    return("ne_sait_pas")
  }
}

evolution<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  }else if("augmentation"%in%x){
    return("augmentation")
  }else if("diminution"%in%x){
    return("diminution")
  }else if("stabilisation"%in%x){
    return("stabilisation")
  }else {
    return("ne_sait_pas")
  }
}

absence_bad2<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("non"%in%x){
    return("non")
  } else if("minorite"%in%x){
    return("minorite")
  } else if("moitie"%in%x){
    return("moitie")
  } else if("majorite"%in%x){
    return("majorite")
  } else if("ensemble"%in%x){
    return("ensemble")
  } else {
    return("Ne_sait_pas")
  }
}

absence_bad3<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  } else if("aucun"%in%x){
    return("aucun")
  } else if("minorite"%in%x){
    return("minorite")
  } else if("moitie"%in%x){
    return("moitie")
  } else if("majorite"%in%x){
    return("majorite")
  } else if("ensemble"%in%x){
    return("ensemble")
  } else {
    return("Ne_sait_pas")
  }
}

max_agg<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  }else{
    return(as.character(max(x,na.rm = T)))
  }
}

mean_agg<-function(x){
  if(all(is.na(x))){
    return(NA_character_)
  }else{
    return(as.character(mean(x,na.rm = T)))
  }
}

sites_agg<-function(db,by,champs,fct){
  if(length(champs)>0){
    db %>%
      group_by_at(by) %>%
      summarize_at(all_of(champs), ~fct(.)) %>%
      ungroup()
  } else{db[!duplicated(db[,by]),][,by]}
}

aok_mode <-function(x) {
  if(all(is.na(x))){
    return("")
  }else{
    x <- x[x %!in% c("")]
    if(length(x) == 0){
      return("")
    }else{
      x = table(x)
      modes = sum(x == max(x))
      if(modes == 1){
        return(names(which(x == max(x))))
      }else{
        return("NC")
      }
    }
  }
}





