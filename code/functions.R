remove_blank_headings<-function(data){data[,names(data)!=""]}
remove_vars<-function(data,vars){data[,names(data) %!in%vars]}

`%!in%` = Negate(`%in%`)

humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

rec_missing<-function(x,missings=c(NULL,'NULL','N/A','n/a',999,998,888,' ','(vide)','d/m','','NA','na',""," ")) {
  x[x %in% missings] <- NA
  return(x)
}

rec_missing_all<-function(data){lapply(data,rec_missing) %>% bind_cols}

cleanheaders<-function(data,slashtodot){
  if(slashtodot){
    names(data)[str_count(names(data),"/")>1]<-sub("/([^/]*)$", "_\\1", names(data)[str_count(names(data),"/")>1])
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
    names(data)<-gsub("\\/",".",names(data)) 
  } else {
    names(data)<-gsub("^X_","",names(data))
    names(data)<-gsub("^_","",names(data))
  }
  return(data)}

prepdata<-function(data,slashtodot){data %>% cleanheaders(.,slashtodot) %>% rec_missing_all %>% remove_blank_headings %>% type_convert}

fixslash<-function(x){
  gsub("\\/","_",x)
}

split_multiple_choice<-function(hh,questions,choices,sep="."){
  
  questions$type %>% ch %>% strsplit(.," ") %>% do.call(rbind,.)-> tosplit
  questions$choices <- ifelse(tosplit[,1]==tosplit[,2],NA,tosplit[,2])
  names(choices)<-paste0("ch_",names(choices))
  questionnaires<-merge(questions,choices,by.x="choices",by.y="ch_list_name",all=T)
  
  
  splitsmult<-function(hh,questionnaires,varname,sep=sep){
    chlist<-questionnaires$ch_name[which(questionnaires$name %in% varname)]
    binarysmult<-lapply(chlist,
                        function(x,hh,varname,sep){
                          filt<-grep(paste0("^",x," ","|"," ",x,"$","|","^",x,"$","|"," ",x," "),hh[[varname]])
                          hh[[paste0(varname,sep,x)]]<-c()
                          hh[[paste0(varname,sep,x)]][is.na(hh[[varname]])|hh[[varname]]=="NA"]<-NA
                          hh[[paste0(varname,sep,x)]][!is.na(hh[[varname]])&hh[[varname]]!="NA"]<-0
                          hh[[paste0(varname,sep,x)]][filt]<-1
                          return(hh[[paste0(varname,sep,x)]])
                        },hh=hh,varname=varname,sep=sep) %>% bind_cols
    names(binarysmult)<-paste(varname,chlist,sep=sep)
    return(binarysmult)
  }
  
  varname=questionnaires$name[grep("select_multiple",questionnaires$type)] %>% unique
  varname<-varname[varname%in%names(hh)]
  
  lapply(varname,splitsmult,hh=hh,questionnaires=questionnaires,sep=sep) %>% bind_cols -> splitteddata
  
  for (j in names(splitteddata)){
    hh[[j]]<-splitteddata[[j]]
  }
  return(hh)
}

ch<-as.character
chr<-as.character

sl_correction<-function(db,sl_definition,survey,sl_name="name",sl_condition="condition", sl=NA){
  multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
  for (i in 1:nrow(sl_definition)) {
    qname<-sl_definition[[sl_name]][i]
    if(qname%in%names(db)){
      db[[qname]]<-ifelse(eval(parse(text=sl_definition[[sl_condition]][[i]]),envir = db),db[[qname]],sl)
      if(qname%in%multiple_choices){
        sm<-names(db)[str_detect(names(db),paste0(qname,"[.]"))]
        for(j in 1:length(sm)){
          if(!is.na(sl)){
            db[[sm[j]]]<-ifelse(db[[qname]]==sl&!is.na(db[[qname]]),sl,db[[sm[j]]]) 
          } else {
            db[[sm[j]]]<-ifelse(is.na(db[[qname]]),sl,db[[sm[j]]])
          }
        }
      }
    }
  }
  return(db)
}

# sl_correction<-function(db,sl_definition,survey,sl_name="name",sl_condition="condition"){
#   multiple_choices <- filter(survey, str_detect(type, "(\\bselect_multiple\\b)"))$name
#   for (i in 1:nrow(sl_definition)) {
#     qname<-sl_definition[[sl_name]][i]
#     if(qname%in%names(db)){
#       db[[qname]]<-ifelse(eval(parse(text=sl_definition[[sl_condition]][[i]]),envir = db),db[[qname]],"")
#       if(qname%in%multiple_choices){
#         sm<-names(db)[str_detect(names(db),paste0(qname,"."))]
#         for(j in 1:length(sm)){
#           db[[sm[j]]]<-ifelse(db[[qname]]=="SL"&!is.na(db[[qname]]),"SL",db[[sm[j]]])
#         }
#       }
#     }
#   }
#   return(db)
# }
