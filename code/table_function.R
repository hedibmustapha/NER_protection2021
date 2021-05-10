#' Create table for question
#' 
#' @param question character of name of vectr in data.frame
#' @param in_questionnaire logical vector indicating if question is in questionnaire or not
#' @param data data frame
#' @param weighting_function function applied to dataset to generate weights
#' @param name name for overall data in the area
#' @param ... additional stratification variables to be applied
analyzer <- function(x, in_questionnaire, data, main_col_name, count, ...) {
  cat(blue(paste0("----",round(match(x, colnames(data)) / ncol(data) *100,2),"%\n\n")))
  # strata <- list(...)
  # strata <- (...)
  # strata <- strata[!is.na(strata)]
  strata <- list(...)
  strata <- strata[!is.na(strata)]
  strata <- unlist(strata)
  data <- filter(data, !is.na(!!sym(x)))
  # if (is.null(weighting_function)) {
  #   weights <- rep(1, nrow(data))
  # } else {
  #   weights <- weighting_function(data)
  # }
  
  x_data <- data[[x]]
  if(is.null(count)){
    if (class(x_data) %in% c("logical", "numeric", "integer")) {
      if (in_questionnaire) {
        avg <- round(wtd.mean(x_data),0)
      } else if (min(x_data) >= 0 & max(x_data) <= 1) {
        avg <- round(100 * wtd.mean(x_data),0)
      } else {
        avg <- round(wtd.mean(x_data),0)
      }
      table <- tibble("indicateurs" = c(x, "Moyenne"), !!main_col_name := c(main_col_name, avg))
    } else {
      table <- wtd.table(x_data, rep(1, length(x_data)))
      if(is_empty(table)) {
        table <- tibble("indicateurs" = "", !!main_col_name := "")
      } else {
        table <- prop(table)
        table <- as.data.frame.matrix(table)
        table[,1] <- row.names(table)
        table <- table[-nrow(table),]
        table <- table[order(as.numeric(table[,ncol(table)]),
                             decreasing = T), c(1, ncol(table))] %>%
          mutate(Total = round(Total,0))
        table <- rbind(c(x, main_col_name), table)
        table <- as.data.frame(table)
        names(table) <- c("indicateurs", main_col_name)
      }
    } 
    if (length(strata) > 0) {
      for (i in 1:length(strata)) {
        groups <- unique(unlist(data[strata[[i]]]))
        groups <- groups[!is.na(groups)]
        for (j in 1:length(groups)) {
          new_data <- filter(data, !!sym(strata[[i]]) == groups[j])
          new_table <- analyzer(x, in_questionnaire, new_data, groups[j], count, NA)
          table <- left_join_NA(table, new_table, by = "indicateurs")
        }
      }
    }
  } else{
    if (class(x_data) %in% c("logical", "numeric", "integer")) {
      if (in_questionnaire) {
        avg <- round(wtd.mean(x_data),0)
      } else if (min(x_data) >= 0 & max(x_data) <= 1) {
        avg <- sum(x_data, na.rm = T)
      } else {
        avg <- round(wtd.mean(x_data),0)
      }
      table <- tibble("indicateurs" = c(x, "Moyenne"), !!main_col_name := c(main_col_name, avg))
    } else {
      table <- wtd.table(x_data, rep(1, length(x_data)))
      if(is_empty(table)) {
        table <- tibble("indicateurs" = "", !!main_col_name := "")
      } else {
        table <- as.data.frame.matrix(table)
        table[,2] <- table[,1]
        table[,1] <- row.names(table)
        table <- table[order(as.numeric(table[,2]),
                             decreasing = T), c(1, ncol(table))]
        table <- rbind(c(x, main_col_name), table)
        table <- as.data.frame(table)
        names(table) <- c("indicateurs", main_col_name)
      }
    } 
    if (length(strata) > 0) {
      for (i in 1:length(strata)) {
        groups <- unique(unlist(data[strata[[i]]]))
        groups <- groups[!is.na(groups)]
        for (j in 1:length(groups)) {
          new_data <- filter(data, !!sym(strata[[i]]) == groups[j])
          new_table <- analyzer(x, in_questionnaire, new_data, groups[j], count, NA)
          table <- left_join_NA(table, new_table, by = "indicateurs")
        }
      }
    }
  }
  
  return(table)
}

left_join_NA <- function(x, y, ...) {
  left_join(x = x, y = y, by = ...) %>% 
    mutate_each(list(~replace(., which(is.na(.)), 0)))
}

table_maker <- function(data, questionnaire, choices, labels = T, language = NULL, 
                        main_col_name, count = NULL, aggregated =TRUE, ...) {
  # collecting strata information
  strata <- list(...)
  strata <- strata[!is.na(strata)]
  strata <- unlist(strata)
  
  # remove the text column for select multiple
  sel_mul <- filter(questionnaire, str_detect(type, "select_multiple"))$name
  data <- select(data, -one_of(sel_mul))
  
  # getting analysis names, don't analyze strata
  if (!is.null(strata)) {
    analysis_names <- names(select(data, -one_of(strata)))
  } else {
    analysis_names <- names(data)
  }
  
  # detect which are in the questionnaire
  in_questionnaire <- analysis_names %in% questionnaire$name
  
  # get initial table output
  
  if (!is.null(strata)) {
    table_output <- bind_rows(map2(analysis_names, 
                                   in_questionnaire, 
                                   analyzer, 
                                   data,
                                   main_col_name,
                                   count,
                                   strata))
  } else {
    table_output <- bind_rows(map2(analysis_names, 
                                   in_questionnaire, 
                                   analyzer, 
                                   data,
                                   main_col_name,
                                   count))
  }
  
  # Editing select multiple binary options
  # Removes extra rows so that we end up with # of rows for # of options
  
  # if(aggregated){
  #   sel_mul<-sel_mul[sel_mul%!in% recode_sl$name]
  #   }
  sel_mul_rgx <- paste0(sel_mul, "(\\/|\\.)")
  sel_mul_extract_rgx <- paste0("(", str_c(sel_mul, collapse = "|"), ")")
  sel_mul_remove_rgx <- paste0("(", str_c(sel_mul_rgx, collapse = "|"), ")")

  table_output <- table_output %>%
    mutate(sel_mul = str_extract(indicateurs, sel_mul_extract_rgx))
  
  avg_indices <- which(table_output[, 1] == "Moyenne")
  avg_indices <- avg_indices[str_detect(table_output[avg_indices - 1, 1], sel_mul_remove_rgx)]
  
  # if(length(avg_indices)>0){
    table_output[avg_indices, 1] <- table_output[avg_indices - 1, 1]
    match_previous <- (table_output[avg_indices - 3, "sel_mul"] == table_output[avg_indices - 1, "sel_mul"])
    match_previous[is.na(match_previous)] <- FALSE
    rem_avg_indices <- avg_indices[match_previous]
    table_output <- table_output[-(rem_avg_indices - 1), ] %>%
      select(-sel_mul) 
    table_output[,1] <- ifelse(table_output[,2] == main_col_name & str_detect(table_output[,1], sel_mul_remove_rgx),
                               str_extract(table_output[,1], sel_mul_extract_rgx),
                               table_output[,1])
    
    table_output[,1] <- ifelse(table_output[,2] != main_col_name,
                               str_remove_all(table_output[,1], sel_mul_remove_rgx),
                               table_output[,1])
  # } else{
  #   table_output <- table_output %>% select(-sel_mul) 
  # }
  
  # if(aggregated){
  #   
  #   sl_mul_rgx <- paste0(recode_sl$name, "(\\/|\\.)")
  #   sl_mul_extract_rgx <- paste0("(", str_c(recode_sl$name, collapse = "|"), ")")
  #   sl_mul_remove_rgx <- paste0("(", str_c(sl_mul_rgx, collapse = "|"), ")")
  #   
  #   table_output <- table_output %>%
  #     mutate(sl_mul = str_extract(indicateurs, sl_mul_extract_rgx))
  #   
  #   avg_indices <- which(table_output[, 1] == "Moyenne")
  #   if(length(avg_indices)>0){
  #     avg_indices <- avg_indices[str_detect(table_output[avg_indices - 1, 1], sl_mul_remove_rgx)]
  #     table_output[avg_indices, 1] <- table_output[avg_indices - 1, 1]
  #     match_previous <- (table_output[avg_indices - 3, "sl_mul"] == table_output[avg_indices - 1, "sl_mul"])
  #     match_previous[is.na(match_previous)] <- FALSE
  #     rem_avg_indices <- avg_indices[match_previous]
  #     table_output <- table_output[-(rem_avg_indices - 1), ] %>%
  #       select(-sl_mul) 
  #   } else {
  #     table_output <- table_output %>% select(-sl_mul) 
  #   }
  # }
  
  # Removing select_multiple question names from binary vars
  # Also removing select multiple binary option from the main question
  
  # table_output[,1] <- ifelse(table_output[,2] == main_col_name & str_detect(table_output[,1], sel_mul_remove_rgx),
  #                            str_extract(table_output[,1], sel_mul_extract_rgx),
  #                            table_output[,1])
  # 
  # table_output[,1] <- ifelse(table_output[,2] != main_col_name,
  #                            str_remove_all(table_output[,1], sel_mul_remove_rgx),
  #                            table_output[,1])
    
  # if(aggregated&length(avg_indices)>0){
  #   table_output[,1] <- ifelse(table_output[,2] == main_col_name & str_detect(table_output[,1], sl_mul_remove_rgx),
  #                              str_extract(table_output[,1], sl_mul_extract_rgx),
  #                              table_output[,1])
  #   
  #   table_output[,1] <- ifelse(table_output[,2] != main_col_name,
  #                              str_remove_all(table_output[,1], sl_mul_remove_rgx),
  #                              table_output[,1])
  # }
  
  # Getting question labels if requested
  
  if (labels) {
    if (is.null(language)) {
      label_col <- "label"
    } else {
      cols <- names(questionnaire)
      label_col <- str_detect(cols, paste0("label[\\W]{2}(?i)", language))
      label_col <- cols[label_col]
    }
    
    
    choice_indices <- match(table_output$indicateurs, choices$name)
    choice_labels <- choices[[label_col]]
    question_indices <- match(table_output$indicateurs, questionnaire$name)
    question_labels <- questionnaire[[label_col]]
    
    table_output <- table_output %>%
      mutate(indicateurs = ifelse(is.na(question_indices), 
                                  indicateurs, 
                           ifelse(is.na(question_labels[question_indices]) | question_labels[question_indices] == "",
                                  indicateurs,
                                  question_labels[question_indices])),
             indicateurs = ifelse(is.na(choice_indices),
                                  indicateurs,
                           choice_labels[choice_indices]))
    
    # Fixing select multiple labels
    
    sel_mul_indices <- match(sel_mul, questionnaire$name)
    
    for (i in 1:length(sel_mul_indices)) {
      table_output[,1] <- str_replace(table_output[,1], paste0(questionnaire$name[sel_mul_indices[i]],"\\b"), question_labels[sel_mul_indices[i]])
    }
  }
  
  # Cleaning rows with question names
  
  split_rows <- table_output[,2] == main_col_name
  table_output[split_rows, 2:ncol(table_output)] <- ""

  return(table_output)
}
