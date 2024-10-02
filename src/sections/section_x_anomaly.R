non_gis_check <- TRUE
source('src/sections/section_2_3_x_helper_load_audits.R')

# ------------------------------testing ---------------------------



# macro <- read.xlsx('C:/Users/reach/Desktop/Git/CINM/data/MSNA2403_2024_final_anonymized_data_19July2024_cooccurence_added.xlsx') %>% 
#   select(uuid, macroregion)
# 
# 
# raw.main <- raw.main %>%
#   left_join(macro %>%
#               select(uuid,!!sym(geo_column)) %>%
#               distinct())


audits <- audits %>% left_join(
  raw.main %>% 
    select(uuid,!!sym(geo_column)) %>% 
    distinct()
  ) 


# test of individual interviews
issues <- data.frame()
for (lst in ls_group){

  res <- anomaly_test_tmln_int(ls = lst,audit = audits,data = raw.main,
                         geo_column = geo_column,enum_name =directory_dictionary$enum_colname)
  
  issues <- rbind(res, issues)
}


# test of enumerators
issues_test <- data.frame()
for (lst in ls_group){

  res <- anomaly_test_enum_diffr(variable_list = lst,
                               audit = audits, 
                               data_frame = raw.main, 
                               enum_name = directory_dictionary$enum_colname, 
                               geo_column = geo_column)
  
  issues_test <- rbind(res, issues_test)
}

issues_full <- full_join(issues, issues_test) %>% 
  mutate(
         s1 = (perc_issues-mean(perc_issues, na.rm=T))/sd(perc_issues, na.rm=T),
         s2 = (issues_per_questions-mean(issues_per_questions, na.rm=T))/sd(issues_per_questions, na.rm=T),
         s3 = (problematic_interviews_perc-mean(problematic_interviews_perc, na.rm=T))/sd(problematic_interviews_perc, na.rm=T),

         s1 = ifelse(s1>0,s1,NA),
         s2 = ifelse(s2>0,s2,NA),
         s3 = ifelse(s3>0,s3,NA)
         ) %>% 
  rowwise() %>% 
  mutate(problem_definer = mean(c(s1,s2,s3), na.rm = T)) %>% 
  ungroup() %>% 
  select(-(starts_with('s')))
  

problematic_enums <- issues_full %>% 
  distinct(!!sym(directory_dictionary$enum_colname)) %>% 
  mutate(problematic_enumerator = TRUE) %>% 
  left_join(issues_full %>% 
              filter(problem_definer>2) %>% 
              distinct(!!sym(directory_dictionary$enum_colname)) %>% 
              mutate(problematic_enumerator_to_check = TRUE)) %>% 
  mutate(problematic_enumerator_to_check = problematic_enumerator_to_check %in% TRUE)


df_enum_tests <- list(
  'Problematic enumerators' = problematic_enums,
  'Interview duration problems' = issues %>% arrange(desc(issues_per_questions)),
  'Enumerator duration problems' = issues_test %>% arrange(desc(perc_issues))
)

write.xlsx(df_enum_tests, paste0(directory_dictionary$dir.audits.check, 'Enumerator_anomalies_time.xlsx'))


################## chain problems ################## 
# check.questions <- c("H_2_water_source", "H_5_water_assess", "H_6_water_treat",
#                      "H_10_wash_issues", "H_12_hand_washing", "H_14_toilet", "H_17_safe_toilet")

# check.questions <- c("fsl_fcs_cereal", "fsl_fcs_legumes", "fsl_fcs_dairy", "fsl_fcs_meat",
#                    "fsl_fcs_veg", "fsl_fcs_fruit", "fsl_fcs_oil", "fsl_fcs_sugar", "I_11_struggle_enough_money")

check.logic.questions <- c("B_15_wgss_seeing", "B_16_wgss_hearing",
                     "B_17_wgss_walking", "B_18_wgss_cognit", "B_19_wgss_selfcare",
                     "B_20_wgss_comm")

geo_column <- "macroregion"

calculate.markov.outliers <- function(data, questions, geo_column) {
  
  fit_markov_chain <- function(interviews, states) {
    transition_matrix <- matrix(0, nrow = length(states), ncol = length(states))
    rownames(transition_matrix) <- states
    colnames(transition_matrix) <- states
    
    for (interview in interviews) {
      for (i in 1:(length(interview) - 1)) {
        from_state <- interview[i]
        to_state <- interview[i + 1]
        transition_matrix[from_state, to_state] <- transition_matrix[from_state, to_state] + 1
      }
    }
    
    row_sums <- rowSums(transition_matrix)
    
    for (i in 1:nrow(transition_matrix)) {
      if (row_sums[i] == 0) {
        transition_matrix[i, ] <- 1 / ncol(transition_matrix)
      } else {
        transition_matrix[i, ] <- transition_matrix[i, ] / row_sums[i]
      }
    }
    
    mc <- new("markovchain", states = states, transitionMatrix = transition_matrix)
    
    return(mc)
  }
  
  calculate_likelihood <- function(interview, mc_model) {
    log_prob <- 0
    for (i in 1:(length(interview) - 1)) {
      log_prob <- log_prob + log(mc_model[as.character(interview[i]), as.character(interview[i+1])])
    }
    return(log_prob)
  }
  
  geovalues <- unlist(unique(data[geo_column]))
  res_list <- c()
  
  suspected_data_combined <- data.frame()
  
  for(geovalue in geovalues) {
    logic.check.data <- data %>%
      dplyr::filter(!!sym(geo_column) %in% geovalue) %>%
      dplyr::select(c("uuid", directory_dictionary$enum_colname, questions))
    
    logic.check.data <- logic.check.data %>% drop_na()
    
    interviews <- list()
    for (i in 1:nrow(logic.check.data)) {
      row <- logic.check.data[i, ]
      interview <- row[questions] %>% as.list() %>% unlist()
      for (idx in 1:length(interview)) {
        interview[idx] <- paste0(questions[idx], ":", interview[idx])
      }
      interviews[[i]] <- interview
    }
    
    states <- c(unique(unlist(interviews)))
    
    mc_model <- fit_markov_chain(interviews, states)
    
    likelihoods <- sapply(interviews, calculate_likelihood, mc_model = mc_model)
    
    outliers <- which(likelihoods <= quantile(likelihoods, 0.05))
    
    suspected_data <- logic.check.data[outliers, ]
    
    suspected_data$probs <- likelihoods[outliers]
    
    suspected_data %>% dplyr::select(c("uuid", directory_dictionary$enum_colname, questions, "probs"))
    
    enum_outlier_count <- suspected_data %>%
      dplyr::group_by(enum) %>%
      dplyr::summarize(outlier_count = n()) 
    
    
    enum_interview_count <- logic.check.data %>%
      dplyr::group_by(enum) %>%
      dplyr::summarize(count = n()) %>%
      dplyr::left_join(enum_outlier_count, by = "enum") %>%
      dplyr::mutate(outlier_count = ifelse(is.na(outlier_count), 0, outlier_count),
                    probs = outlier_count / count)
    
    enum_interview_count$probs <- enum_interview_count$outlier_count / enum_interview_count$count
    
    enum_interview_count <- enum_interview_count %>%
      dplyr::filter(count > 15 & probs > 0.075)
    
    suspected_data <- suspected_data %>%
      dplyr::filter(!!sym(directory_dictionary$enum_colname) %in% enum_interview_count$enum)
    
    suspected_data$geovalue <- geovalue
    
    suspected_data_combined <- rbind(suspected_data_combined, suspected_data)
    
    res_list <- append(res_list, setNames(list(enum_interview_count), geovalue))
  }
  
  return(list(res_list = res_list, suspected_data = suspected_data_combined))
}

raw.main <- data

if (length(check.logic.questions) != 0) {
  
  res <- calculate.markov.outliers(raw.main, check.questions, geo_column)
  
  prob_res <- res$res_list
  suspected_data <- res$suspected_data
  suspected_data$probs <- exp(suspected_data$probs)
  suspected_data$probs <- paste0(round(suspected_data$probs * 100, 6), "%")
  
  res_df <- data.frame()
  for (geovalue in names(prob_res)) {
    prob_res[[geovalue]]$geovalue <- geovalue
    res_df <- rbind(res_df, prob_res[[geovalue]])
  }
  
  res_df$probs <- paste0(round(res_df$probs * 100, 2), "%")
  
  write.xlsx(res_df, 'Enumerator_anomalies_logic.xlsx')
  write.xlsx(suspected_data, 'Enumerators_anomalies_logic_data.xlsx')
} else {
  cat("Columns for logic check wasn't defined, process skipped")
}

################## entropy check ################## 

entropy.process <- function(data, exclude_columns = c("uuid"), enum_id.column = "enum_id") {
  
  calculate_entropy <- function(responses) {
    responses <- na.omit(responses)
    if (length(responses) == 0) {
      return(0)
    }
    
    split_responses <- strsplit(responses, " ")
    all_answers <- unlist(split_responses)
    
    response_freq <- table(all_answers)
    probabilities <- response_freq / sum(response_freq)
    
    entropy::entropy(probabilities, unit = "log2")
  }
  
  enumerator_entropy <- data %>%
    dplyr::group_by(!!sym(directory_dictionary$enum_colname)) %>%
    dplyr::summarize(
      across(
        .cols = -all_of(exclude_columns),
        .fns = ~calculate_entropy(.x),
        .names = "{col}_entropy"
      ),
      num_interviews = n()
    )
  
  return(enumerator_entropy)
}


if (length(check.logic.questions) != 0) {
  entropy_res_list <- list()
  geovalues <- unlist(unique(data[geo_column]))
  
  for (geovalue in geovalues) {
    entropy_data <- raw.main %>%
      dplyr::filter(!!sym(geo_column) %in% geovalue) %>%
      dplyr::select(c(check.logic.questions, "uuid", directory_dictionary$enum_colname))
    
    print(colnames(entropy_data))
    
    entropy_sum <- entropy.process(entropy_data, exclude_columns = c("uuid"), enum_id.column = directory_dictionary$enum_colname) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        entropy_sum = sum(
          across(
            .cols = -c(enum, num_interviews)
          ),
          na.rm = TRUE
        )
      ) %>%
      dplyr::select("enum", "entropy_sum", "num_interviews")
    
    entropy_sum <- entropy_sum %>%
      dplyr::filter(num_interviews > 15)
    
    entropy_res_list <- append(entropy_res_list, setNames(list(entropy_sum), geovalue))
  }
}

################## cross check ################## 

