library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

function(input, output, session) {
  
  options(shiny.maxRequestSize = 10*1024^2)
  
  names_contracts <- c('contract_title', 'buyer_name', 'winner_name', 'buyer_city', 'final_value', 'bids_count', 'bid_deadline', 'firstcall_date', 'procedure_type', 'award_date')
  
  names_survey <- c('buyer_city', 'year', 'n', 'bribes')
  
  names_keywords <- c('keywords_cui', 'keywords_op', 'keywords_inv')
  
  output$no_survey_message <- renderText({
    "Continue without survey data."
  })
  output$select_option_message <- renderText({
    "Please select an option to continue."
  })
  
  file_message1 <- reactiveVal()
  
  output$file_message1 <- renderText({
    file_message1()
  })
  
  output$ui_survey <- renderUI({
    if (input$survey_tf == "Yes") {
      fluidPage(
        fileInput("file1", "Choose a CSV file"),
        textOutput("file_message1")
      )
      
    } else if (input$survey_tf == "No") {
      fluidPage(textOutput("no_survey_message"))
      
    } else {
      fluidPage(textOutput("select_option_message"))
    }
  })
  
  input_survey <- reactiveVal()
  
  observe({
    if (input$survey_tf == "No") {
      input_survey(
        data.frame(
          buyer_city = NA,
          year = NA,
          n = NA,
          bribes = NA
        )
      )
    }
  })
  
  observeEvent(input$file1, {
    req(input$file1)  
    
    if (tolower(tools::file_ext(input$file1$name)) != "csv") {
      file_message1("File is not a CSV")
      return()  
    }
    
    data1 <- read.csv(input$file1$datapath)
    
    if (all(colnames(data1) %in% names_survey) && length(colnames(data1)) == length(names_survey)) {
      file_message1("File is correct.")
      input_survey(data1)  
    } else {
      file_message1("Variable names are incorrect")
    }
  })
  
  # ----------------------------------------------------------------------------
  
  output$contracts_ui <- renderUI({
    req(input_survey())
    if (!is.null(input_survey()) && input$survey_tf != "Select Option") {
      
      fluidPage(
        titlePanel("Procurement Data"),
        fileInput("file2", "Choose a CSV file"),
        textOutput("file_message2")
      )
      
    }
  })
  
  observeEvent(input$file2, {
    req(input$file2)  
    
    if (tolower(tools::file_ext(input$file2$name)) != "csv") {
      output$file_message2 <- renderText("File is not a CSV")
      return()  # Exit the observeEvent, as the file is not a CSV
    }
    
    data2 <- read.csv(input$file2$datapath)
    
    if (all(colnames(data2) %in% names_contracts) && length(colnames(data2)) == length(names_contracts)) {
      output$file_message2 <- renderText("File is correct.")
    } else {
      output$file_message2 <- renderText("Variable names are incorrect")
    }
  })
  
  input_contracts <- reactive({
    req(input$file2)  
    read.csv(input$file2$datapath)  
  })
  
  
  # ----------------------------------------------------------------------------
  
  prev_selected <- reactiveVal("Select Option")
  
  output$keywordsmessage <- renderText({
    if (input$keywords == "Select Option") {
      return("Please select an option to continue.")
    } else if (input$keywords %in% c("Spanish", "English")) {
      return("Click submit and continue.")
    } else if (input$keywords == "Custom") {
      return("Upload custom keywords file.")
    }
  })
  
  output$keywords_dropdown <- renderUI({
    req(input_contracts())
    if (!is.null(input_contracts())) {

      fluidPage(h2("Keywords"),
                
                selectInput(
                  inputId = "keywords", 
                  label = "Select keywords:",
                  choices = c("Select Option","Spanish", "English", "Custom"), 
                  selected = prev_selected()
                ),
                textOutput("keywordsmessage")
                )
      
    }
  })
  
  output$keywords_fileinput <- renderUI({
    req(input_contracts())
    if (!is.null(input$keywords) && input$keywords == "Custom") {
      fileInput("customkeys", "Choose a CSV file")
    }
  })

  output$action <- renderUI({
    req(input_survey())
    if (!is.null(input$keywords) && input$keywords != "Select Option") {
      actionButton("submit_btn", "Submit")
    }
  })
  
  input_keywords <- reactiveVal()
  
  observe({
    req(input$keywords) 
    prev_selected(input$keywords)
    
    if (input$keywords == "Spanish") {
      input_keywords(
        readr::read_csv("data/wiri_keywords_spanish.csv")
        )
    } else if (input$keywords == "English") {
      input_keywords(
        readr::read_csv("data/wiri_keywords_english.csv")
        )
    } 
  })
  
  file_message3 <- reactiveVal()
  
  output$file_message3 <- renderText({
    file_message3()
  })
  
  output$key_message2 <- renderUI({
    req(input_survey())
    if (!is.null(input$keywords) && input$keywords == "Custom") {
        textOutput("file_message3")
    }
  })
  
  observeEvent(input$customkeys, {
    req(input$customkeys)

    if (tolower(tools::file_ext(input$customkeys$name)) != "csv") {
      file_message3("File is not a CSV.")
      return()
    }

    data3 <- read.csv(input$customkeys$datapath)

    if (all(colnames(data3) %in% names_keywords) && length(colnames(data3)) == length(names_keywords)) {
      file_message3("File is correct. Click submit and continue.")
      input_keywords(data3)
    } else {
      file_message3("Variable names are incorrect.")
    }
  })
  
  # ----------------------------------------------------------------------------

  runClick <- reactiveVal(0)
  
  observeEvent(input$submit_btn, {
    req(input_keywords())
    prev_selected(input$keywords)

    df_keys = input_keywords()
    
    cui_k <- df_keys %>%
      filter(!is.na(keywords_cui)) %>%
      pull(keywords_cui) %>%
      paste(collapse = "|") %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII")
    
    op_k <- df_keys %>%
      filter(!is.na(keywords_op)) %>%
      pull(keywords_op) %>%
      paste(collapse = "|") %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII")
    
    in_k <- df_keys %>%
      filter(!is.na(keywords_inv)) %>%
      pull(keywords_inv) %>%
      paste(collapse = "|") %>%
      tolower() %>%
      stringi::stri_trans_general("Latin-ASCII")
    
    output$output_text <- renderPrint({
      c("Keywords used:",
      paste("Investments:", in_k),
      paste("Operations:", op_k),
      paste("Interactions:", cui_k)
      )
      
    })
    runClick(runClick() + 1)
  })
  
  # ----------------------------------------------------------------------------
  
  text_dashboard <- reactiveVal()
  
  output$text_dashboard <- renderText({
    text_dashboard()
  })
  
  observe({
    req(input_keywords())
    
    if (!is.null(input_keywords())) {
      
      if (all(colnames(input_keywords()) %in% names_keywords)){ # change to condition! 
        text_dashboard("All files are correct. Continue to the dashboard (above).")
      }
      
    } else {
      return()
    }
    
  })
  
  output$diagnostics_ui <- renderUI({
    req(runClick() > 0) 
    if (!is.null(input_keywords())) {
      
      fluidPage(
        h2("Diagnostics"),
        textOutput("text_dashboard")
        # textOutput("merge_message"),
        # textOutput("weights_message")
      )
      
    }
  })
  
  # ----------------------------------------------------------------------------
  # Keywords Direct Input START
  # ----------------------------------------------------------------------------
  
  # output$keywords_ui <- renderUI({
  #   req(input_contracts())
  #   if (!is.null(input_contracts())) {
  #     
  #     fluidPage(h2("Enter Keywords"),
  #     
  #     # Input field for text
  #     textInput("keywords_cui", "Interactions:", value = "agua+|acua+|hidr+"),
  #     textInput("keywords_op", "Operations:", value = "maquinaria+|equip+"),
  #     textInput("keywords_inv", "Investments:", value = "construc+"),
  #     
  #     # Submit button
  #     actionButton("submit_btn", "Submit"))
  # 
  #   }
  # })

  # submitted_values <- reactiveValues(keywords_cui = "agua+|acua+|hidr+", keywords_op = "maquinaria+|equip+", keywords_inv = "construc+")
  # runClick <- reactiveVal(0)
  # 
  # observeEvent(input$submit_btn, {
  # 
  #   if (!is.null(input$keywords_cui) && input$keywords_cui != "" &
  #       !is.null(input$keywords_op) && input$keywords_op != "" &
  #       !is.null(input$keywords_inv) && input$keywords_inv != "") {
  #     output$key_message <- renderText("Keywords are correct")
  #   } else {
  #     output$key_message <- renderText("Please enter text before submitting.")
  #   }
  # 
  #   validate(
  #     need(!is.null(input$keywords_cui) && input$keywords_cui != "", "Please enter text before submitting."),
  #     need(!is.null(input$keywords_op) && input$keywords_op != "", "Please enter text before submitting."),
  #     need(!is.null(input$keywords_inv) && input$keywords_inv != "", "Please enter text before submitting.")
  #   )
  # 
  #   submitted_values$keywords_cui <- input$keywords_cui
  #   submitted_values$keywords_op <- input$keywords_op
  #   submitted_values$keywords_inv <- input$keywords_inv
  # 
  #   output$output_text <- renderPrint({
  #     paste("Keyords used. ",
  #           "Interactions:", submitted_values$keywords_cui,
  #           "Operations:", submitted_values$keywords_op,
  #           "Investments:", submitted_values$keywords_inv)
  #   })
  #   runClick(runClick() + 1)
  # })
  
  # ----------------------------------------------------------------------------
  # Keywords Direct Input END
  # ----------------------------------------------------------------------------

  
  # ----------------------------------------------------------------------------
  # WIRI Calculations
  # ----------------------------------------------------------------------------
  
  df_summaries <- reactive({
    
    req(runClick() > 0) 
    req(input_survey())
    req(input_contracts())

    set.seed(42)
    
  # General Parameters -----------------------------------------------------------
  n_areas = 10
  df_contracts = input_contracts() 
  df_survey =  input_survey() 
  df_keywords = input_keywords()

  int_keywords = df_keywords %>%
    filter(!is.na(keywords_cui)) %>%
    pull(keywords_cui) %>%
    paste(collapse = "|") %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  op_keywords = df_keywords %>%
    filter(!is.na(keywords_op)) %>%
    pull(keywords_op) %>%
    paste(collapse = "|") %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  inv_keywords = df_keywords %>%
    filter(!is.na(keywords_inv)) %>%
    pull(keywords_inv) %>%
    paste(collapse = "|") %>%
    tolower() %>%
    stringi::stri_trans_general("Latin-ASCII")
  
  # Convert to Date objects
  df_contracts$bid_deadline <- as.Date(df_contracts$bid_deadline, format = "%Y-%m-%d")
  df_contracts$firstcall_date <- as.Date(df_contracts$firstcall_date, format = "%Y-%m-%d")
  df_contracts$award_date <- as.Date(df_contracts$award_date, format = "%Y-%m-%d")
  df_contracts$year <- as.integer(format(df_contracts$bid_deadline, "%Y"))
  
  # Categorical Casting ----------------------------------------------------------
  
  # Single bidder
  df_contracts$singleb <- ifelse(df_contracts$bids_count > 1, 0, 1)
  df_contracts$singleb <- as.factor(df_contracts$singleb)
  
  # Advert Period
  df_contracts$submp <- as.numeric(df_contracts$bid_deadline - df_contracts$firstcall_date)
  df_contracts$submp <- ifelse(df_contracts$submp > 365, NA, df_contracts$submp)
  df_contracts$submp <- ifelse(df_contracts$submp < 0, NA, df_contracts$submp)
  df_contracts$submp10 <- dplyr::ntile(df_contracts$submp, 10)
  
  # No Call for Tenders
  df_contracts$ncft <- ifelse(is.na(df_contracts$bid_deadline), 1, 0)
  
  # Decision Period
  df_contracts$decp <- as.numeric(df_contracts$award_date - df_contracts$bid_deadline)
  df_contracts$decp <- ifelse(df_contracts$decp > 365, NA, df_contracts$decp)
  df_contracts$decp <- ifelse(df_contracts$decp < 0, NA, df_contracts$decp)
  df_contracts$decp10 <- ntile(df_contracts$decp, 10)
  
  # Procedure Type
  df_contracts$proc <- NA
  df_contracts$proc[df_contracts$procedure_type == "OPEN"] <- 0
  df_contracts$proc[df_contracts$procedure_type %in% c("OTHER", 
                                                       "NEGOTIATED",
                                                       "SEMIOPEN",
                                                       "SEMI OPEN",
                                                       "SEMI-OPEN")] <- 1
  df_contracts$proc[df_contracts$procedure_type == "RESTRICTED"] <- 2
  
  # Risk Parameters (supervised) -------------------------------------------------
  
  # singleb
  df_contracts$singleb <- ifelse(df_contracts$singleb == 0, 0,
                                 ifelse(df_contracts$singleb == 1, 100,
                                        ifelse(is.na(df_contracts$singleb), 99, NA)))
  
  df_contracts$singleb <- factor(df_contracts$singleb)
  
  # ncft
  df_contracts$ncft <- ifelse(df_contracts$ncft == 0, 0,
                              ifelse(df_contracts$ncft == 1, 100,
                                     ifelse(is.na(df_contracts$ncft), 99, NA)))
  
  df_contracts$ncft <- factor(df_contracts$ncft)
  
  # corr_proc
  df_contracts$corr_proc <- ifelse(df_contracts$proc == 0, 0,
                                   ifelse(df_contracts$proc == 1, 50,
                                          ifelse(df_contracts$proc == 2, 100,
                                                 ifelse(is.na(df_contracts$proc), 99, NA))))
  
  df_contracts$corr_proc <- factor(df_contracts$corr_proc)
  
  # corr_submp
  df_contracts$corr_submp <- ifelse(df_contracts$submp10 %in% c(1,10), 100,
                                   ifelse(df_contracts$submp10 %in% c(2,3,8,9), 50,
                                          ifelse(df_contracts$submp10 %in% c(4,5,6,7), 0,
                                                 ifelse(is.na(df_contracts$submp10), 99, NA))))
  
  df_contracts$corr_submp <- factor(df_contracts$corr_submp)
  
  # corr_decp
  df_contracts$corr_decp <- ifelse(df_contracts$decp10 %in% c(1,10), 100,
                                     ifelse(df_contracts$decp10 %in% c(2,3,8,9), 50,
                                            ifelse(df_contracts$decp10 %in% c(4,5,6,7), 0,
                                                   ifelse(is.na(df_contracts$decp10), 99, NA))))
  
  df_contracts$corr_decp <- factor(df_contracts$corr_decp)
  
  
  # # Risk Parameters (unsupervised, uncomment for custom decile bins) ---------
  # 
  # generate_random_sets <- function() {
  #   # Generate a random permutation of numbers from 1 to 10
  #   perm <- sample(1:10)
  #   # Split the permutation into three sets
  #   set1 <- perm[1:3]
  #   set2 <- perm[4:6]
  #   set3 <- perm[7:10]
  #   return(list(set1, set2, set3))
  # }
  # 
  # # Decision Period
  # decp_condition <- FALSE
  # 
  # while (decp_condition == FALSE) {
  #   
  #   sets <- generate_random_sets()
  #   set1_decp <- sets[[1]]
  #   set2_decp <- sets[[2]]
  #   set3_decp <- sets[[3]]
  #   
  #   df_contracts$corr_decp <- ifelse(df_contracts$decp10 %in% set3_decp, 100,
  #                                    ifelse(df_contracts$decp10 %in% set2_decp, 50,
  #                                           ifelse(df_contracts$decp10 %in% set1_decp, 0,
  #                                                  ifelse(is.na(df_contracts$decp10), 99, NA))))
  #   
  #   df_contracts$corr_decp <- factor(df_contracts$corr_decp)
  #   
  #   model_decp <- glm(singleb ~ corr_decp +
  #                       log(final_value) +
  #                       as.factor(year),
  #                     family = binomial,
  #                     data = df_contracts)
  #   
  #   model_decp <- broom::tidy(model_decp)
  #   corr_decp50 <- model_decp[model_decp$term == "corr_decp50", ]$estimate
  #   corr_decp100 <- model_decp[model_decp$term == "corr_decp100", ]$estimate
  #   decp_condition <- ifelse(corr_decp50 > 0 &
  #                              corr_decp100 > corr_decp50, TRUE, FALSE)
  #   
  # }
  # 
  # # Advert Period
  # submp_condition <- FALSE
  # 
  # while (submp_condition == FALSE) {
  #   
  #   sets <- generate_random_sets()
  #   set1_submp <- sets[[1]]
  #   set2_submp <- sets[[2]]
  #   set3_submp <- sets[[3]]
  #   
  #   df_contracts$corr_submp <- ifelse(df_contracts$submp10 %in% set3_submp, 100,
  #                                     ifelse(df_contracts$submp10 %in% set2_submp, 50,
  #                                            ifelse(df_contracts$submp10 %in% set1_submp, 0,
  #                                                   ifelse(is.na(df_contracts$submp10), 99, NA))))
  #   
  #   df_contracts$corr_submp <- factor(df_contracts$corr_submp)
  #   
  #   model_submp <- glm(singleb ~ corr_submp +
  #                        log(final_value) +
  #                        as.factor(year),
  #                      family = binomial,
  #                      data = df_contracts)
  #   
  #   model_submp <- broom::tidy(model_submp)
  #   corr_submp50 <- model_submp[model_submp$term == "corr_submp50", ]$estimate
  #   corr_submp100 <- model_submp[model_submp$term == "corr_submp100", ]$estimate
  #   submp_condition <- ifelse(corr_submp50 > 0 &
  #                               corr_submp100 > corr_submp50, TRUE, FALSE)
  #   
  # }
  
  # Calculate the CRI ------------------------------------------------------------
  
  main_cri_vars = c("singleb","corr_proc","corr_submp","corr_decp","ncft")
  
  df_contracts[main_cri_vars] <- lapply(df_contracts[main_cri_vars], as.character)
  
  df_contracts[main_cri_vars] <- lapply(df_contracts[main_cri_vars], function(x) ifelse(x == "99", NA_character_, x))
  
  df_contracts[main_cri_vars] <- lapply(df_contracts[main_cri_vars], as.numeric)
  
  df_contracts$cri <- rowMeans(df_contracts[, main_cri_vars], na.rm = TRUE)
  
  df_contracts$cri_integrity <- 100 - df_contracts$cri
  
  df_contracts$contract_title <- tolower(df_contracts$contract_title)
  df_contracts$contract_title <- stringi::stri_trans_general(df_contracts$contract_title, "Latin-ASCII")
  
  df_contracts$buyer_name <- tolower(df_contracts$buyer_name)
  df_contracts$buyer_name <- stringi::stri_trans_general(df_contracts$buyer_name, "Latin-ASCII")
  
  df_contracts$winner_name <- tolower(df_contracts$winner_name)
  df_contracts$winner_name <- stringi::stri_trans_general(df_contracts$winner_name, "Latin-ASCII")
  
  df_contracts$inv_contract <- ifelse(grepl(inv_keywords, df_contracts$contract_title), 1, 0)
  df_contracts$op_contract <- ifelse(grepl(op_keywords, df_contracts$contract_title), 1, 0)
  df_contracts$inter_contract <- ifelse(grepl(int_keywords, df_contracts$winner_name), 1, 0)
  
  df_water <- df_contracts[df_contracts$inv_contract == 1 | df_contracts$op_contract == 1 | df_contracts$inter_contract == 1, ]
  
  dfw_sums <- df_water %>%
    group_by(year, buyer_city) %>%
    summarise(
      count_total = n(),
      count_inv = sum(inv_contract == 1),
      count_op = sum(op_contract == 1),
      count_int = sum(inter_contract == 1),
      avg_int_all = mean(cri_integrity, na.rm = TRUE),
      avg_int_inv = mean(cri_integrity[inv_contract == 1], na.rm = TRUE),
      avg_int_op = mean(cri_integrity[op_contract == 1], na.rm = TRUE),
      avg_int_inter = mean(cri_integrity[inter_contract == 1], na.rm = TRUE),
      contract_value_total = sum(final_value, na.rm = TRUE),
      contract_value_inv = sum(final_value[inv_contract == 1], na.rm = TRUE),
      contract_value_op = sum(final_value[op_contract == 1], na.rm = TRUE),
      contract_value_int = sum(final_value[inter_contract == 1], na.rm = TRUE)
    )
  
  # Add Survey Data --------------------------------------------------------------
  
  dfw_sums = left_join(dfw_sums, df_survey)  %>% #changed
    mutate(cui_bribery = (bribes/n)*100,
           cui_survey_int = (100-cui_bribery))
  
  dfw_sums[dfw_sums=="NaN"] <- NA
  
  dfw_sums <- ungroup(dfw_sums)
  
  return(dfw_sums)
  
  })
  
  output$merge_message <- renderText({
    req(df_summaries()) 
    if ("cui_survey_int" %in% names(df_summaries())) {
      "Files are merged correctly"
    } else {
      "Files are not merged correctly"
    }
  })

pillar_weights <- reactive({

  req(df_summaries())
  
  dfw_sums <- df_summaries()

  x_missing <- as.data.frame(sapply(dfw_sums, function(x) sum(is.na(x)))) %>%
    tibble::rownames_to_column("column_name") %>%
    rename("missing_x" = 2) %>%
    mutate(number_of_rows = nrow(dfw_sums)) %>%
    mutate(rate = 1-(missing_x/number_of_rows))

  weights <- x_missing %>%
    filter(column_name %in% c("avg_int_inv",
                              "avg_int_op",
                              "avg_int_inter",
                              "cui_survey_int")) %>%
    mutate(weight=rate/(sum(.$rate)))

  rownames(weights) <- weights$column_name

  cui_w <- weights["cui_survey_int", "weight"] + weights["avg_int_inter", "weight"]
  op_w  <- weights["avg_int_op", "weight"]
  inv_w <- weights["avg_int_inv", "weight"]
  pillar_weights <- c(inv_w, op_w, cui_w)
  
  return(pillar_weights)
  
  })

output$weights_message <- renderText({
    req(pillar_weights()) 
    if (sum(pillar_weights()) == 1) {
      "Pillar weights are calculated correctly"
    } else {
      "Pillar weights are not calculated correctly"
    }
  })

df_WIRI <- reactive({

  req(pillar_weights()) 
  dfw_sums <- df_summaries()
  pillar_weights <- pillar_weights()

  WIRI <- dfw_sums %>%
    group_by(buyer_city) %>%
    summarise(
      wiri_inv = mean(avg_int_inv, na.rm = TRUE)
    )
  
  WIRI <- dfw_sums %>%
    group_by(buyer_city) %>%
    summarise(
      wiri_ops = mean(avg_int_op, na.rm = TRUE)
    ) %>%
    left_join(WIRI, by = "buyer_city")
  
  WIRI <- dfw_sums %>%
    group_by(buyer_city) %>%
    summarise(
      wiri_cui = mean(c(avg_int_inter, cui_survey_int), na.rm = TRUE)
    ) %>%
    left_join(WIRI, by = "buyer_city")
  
  WIRI <- WIRI %>%
    filter(
      (!is.na(wiri_inv) & !is.na(wiri_ops)) |
        (!is.na(wiri_inv) & !is.na(wiri_ops) & !is.na(wiri_cui)) |
        (!is.na(wiri_inv) & !is.na(wiri_cui)) |
        (!is.na(wiri_cui) & !is.na(wiri_ops))
    )
  
  WIRI <- WIRI %>%
    mutate(
      wiri_inv = ifelse(is.na(wiri_inv), 0, wiri_inv),
      wiri_ops = ifelse(is.na(wiri_ops), 0, wiri_ops),
      wiri_cui = ifelse(is.na(wiri_cui), 0, wiri_cui)
    )
  
  WIRI <- WIRI %>%
    rowwise() %>%
    mutate(
      WIRI = weighted.mean(c(wiri_inv, wiri_ops, wiri_cui), w = pillar_weights, na.rm = TRUE)
    )
  
  return(WIRI)

})

df_WIRI_ts <- reactive({
  
  req(df_WIRI())
  req(pillar_weights()) 
  dfw_sums <- df_summaries()
  pillar_weights <- pillar_weights()

  WIRI_ts = dfw_sums %>%
    rowwise() %>%
    mutate(
      wiri_inv_ts = mean(avg_int_inv, na.rm = TRUE),
      wiri_ops_ts = mean(avg_int_op, na.rm = TRUE),
      wiri_cui_ts = mean(c(avg_int_inter, cui_survey_int), na.rm = TRUE)
    )
  
  WIRI_ts[is.na(WIRI_ts)] = NA
  
  WIRI_ts = WIRI_ts %>%
    filter((!is.na(wiri_inv_ts) & !is.na(wiri_ops_ts)) |
             (!is.na(wiri_inv_ts) & !is.na(wiri_ops_ts) & !is.na(wiri_cui_ts)) |
             (!is.na(wiri_inv_ts) & !is.na(wiri_cui_ts)) |
             (!is.na(wiri_cui_ts) & !is.na(wiri_ops_ts))
    ) %>%
    mutate(wiri_inv_ts = ifelse(is.na(wiri_inv_ts),0,wiri_inv_ts),
           wiri_ops_ts = ifelse(is.na(wiri_ops_ts),0,wiri_ops_ts),
           wiri_cui_ts = ifelse(is.na(wiri_cui_ts),0,wiri_cui_ts)
    ) %>%
    rowwise() %>%
    mutate(
      WIRI_ts = weighted.mean(c(wiri_inv_ts, wiri_ops_ts, wiri_cui_ts), w = pillar_weights, na.rm = T)
    ) %>%
    select(year, buyer_city, WIRI_ts, wiri_inv_ts, wiri_ops_ts, wiri_cui_ts)

  return(WIRI_ts)

})
  

df_wiri_cross <- reactive({
  req(df_WIRI())
  df_WIRI()
})

df_wiri_time <- reactive({
  req(df_WIRI_ts())
  df_WIRI_ts()
})


output$wiri_cross <- renderPlotly({
  
  indicator = case_when(
    input$dropdown == "WIRI" ~ "WIRI",
    input$dropdown == "Operations" ~ "wiri_ops",
    input$dropdown == "Investments" ~ "wiri_inv",
    input$dropdown == "Interactions" ~ "wiri_cui"
  )
  
  mycolor = case_when(
    input$dropdown == "WIRI" ~ "#14b795",
    input$dropdown == "Operations" ~ "#8b0000",
    input$dropdown == "Investments" ~ "#088efc",
    input$dropdown == "Interactions" ~ "#ffb90f"
  )
  
  myline = case_when(
    input$dropdown == "WIRI" ~ mean(df_wiri_cross()$WIRI),
    input$dropdown == "Operations" ~ mean(df_wiri_cross()$wiri_ops),
    input$dropdown == "Investments" ~ mean(df_wiri_cross()$wiri_inv),
    input$dropdown == "Interactions" ~ mean(df_wiri_cross()$wiri_cui)
  )

  top_cities <- df_summaries() %>% 
    group_by(buyer_city) %>% 
    summarise(counts = sum(count_total)) %>% 
    arrange(desc(counts)) %>% 
    head(10)
  
  data_bar <- df_wiri_cross() %>% 
    filter(buyer_city %in% top_cities$buyer_city)
  
  bar <- data_bar %>%
    mutate(Variable = paste0("<br>", "The bar represents the avg. score for the <br> composite WIRI indicator or one of its <br> sub-components (Investments, Operations, Interactions). <br> Top cities by total number of contracts are shown. <br> In ", buyer_city, " the average ", input$dropdown, " score is ", round(get(indicator)), ". <br> The vertical line is the country indicator average. <br> Bars in gray are cities below the country average."),
    fact = ifelse(get(indicator) > myline, "a","b")
    ) %>% 
    ggplot(aes(x = get(indicator), 
               y = reorder(buyer_city, get(indicator)),
               txt = Variable)
               ) +
    geom_col(aes(fill = fact), show.legend = F) +
    scale_fill_manual(values = c(mycolor, "gray")) +
    geom_vline(xintercept = myline, linetype = "dashed") +
    guides(fill = "none") +
    labs(y = NULL,
         x = NULL,
         title = sprintf("%s score", input$dropdown)
         )+
    theme_classic() 
  
  p <- ggplotly(bar, tooltip = "txt") 
  
  p <- layout(
    p,
    annotations = list(
      x = 0.5,
      y = -0.15,
      text = sprintf("Average %s score in top 10 cities, vertical line is the indicator average.", input$dropdown),
      showarrow = F,
      xref = 'paper',
      yref = 'paper',
      xanchor = 'center',
      yanchor = 'top',
      font = list(size = 8)
    )
  )
  
  p
  

})

output$wiri_time <- renderPlotly({

  indicator4 = case_when(
    input$dropdown == "WIRI" ~ "WIRI_ts",
    input$dropdown == "Operations" ~ "wiri_ops_ts",
    input$dropdown == "Investments" ~ "wiri_inv_ts",
    input$dropdown == "Interactions" ~ "wiri_cui_ts"
  )
  
  mycolor = case_when(
    input$dropdown == "WIRI" ~ "#14b795",
    input$dropdown == "Operations" ~ "#8b0000",
    input$dropdown == "Investments" ~ "#088efc",
    input$dropdown == "Interactions" ~ "#ffb90f"
  )

  top_cities <- df_summaries() %>%
    group_by(buyer_city) %>%
    summarise(counts = sum(count_total)) %>%
    arrange(desc(counts)) %>%
    head(5)
  
  myline = case_when(
    input$dropdown == "WIRI" ~ mean(df_wiri_cross()$WIRI),
    input$dropdown == "Operations" ~ mean(df_wiri_cross()$wiri_ops),
    input$dropdown == "Investments" ~ mean(df_wiri_cross()$wiri_inv),
    input$dropdown == "Interactions" ~ mean(df_wiri_cross()$wiri_cui)
  )

  data_time <- df_wiri_time() %>%
    filter(buyer_city %in% top_cities$buyer_city)

  time <- data_time %>%
    mutate(Variable = paste0("<br>", "The line represents the score for the composite WIRI indicator <br> or one of its sub-components (Investments, Operations, Interactions) <br> over time for the top cities by total number of contracts. <br> In ", buyer_city, " the ", input$dropdown, " score is ",round(get(indicator4)), " for the year ", year
                             )) %>% 
    ggplot(aes(x = year, y = get(indicator4), group = 1,
               txt = Variable
               )) +
    geom_line(color = mycolor) +
    geom_hline(yintercept = myline, linetype = "dashed") +
    facet_wrap(~buyer_city,
               nrow = 1) +
    labs(x = NULL,
         title = sprintf("%s score over time", input$dropdown),
         y = NULL
         ) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)
          )
  
  p <- ggplotly(time, tooltip = "txt")
  
  p <- layout(
    p,
    annotations = list(
      x = 0.5,
      y = -0.3,
      text = sprintf("Average %s score for top 5 cities by year, horizontal line represents the global average by indicator.", input$dropdown),
      showarrow = F,
      xref = 'paper',
      yref = 'paper',
      xanchor = 'center',
      yanchor = 'top',
      font = list(size = 8)
    )
  )
  
  p

})

output$number_contracts <- renderPlotly({
  
  indicator2 = case_when(
    input$dropdown == "WIRI" ~ "count_total",
    input$dropdown == "Operations" ~ "count_op",
    input$dropdown == "Investments" ~ "count_inv",
    input$dropdown == "Interactions" ~ "count_int"
  )
  
  indicator3 = case_when(
    input$dropdown == "WIRI" ~ "WIRI",
    input$dropdown == "Operations" ~ "wiri_ops",
    input$dropdown == "Investments" ~ "wiri_inv",
    input$dropdown == "Interactions" ~ "wiri_cui"
  )
  
  mycolor = case_when(
    input$dropdown == "WIRI" ~ "#14b795",
    input$dropdown == "Operations" ~ "#8b0000",
    input$dropdown == "Investments" ~ "#088efc",
    input$dropdown == "Interactions" ~ "#ffb90f"
  )

  top_cities <- df_summaries() %>%
    group_by(buyer_city) %>%
    summarise(counts = sum(count_total)) %>%
    arrange(desc(counts)) %>%
    head(10)

  data_contracts <- df_summaries() %>%
    filter(buyer_city %in% top_cities$buyer_city) %>%
    group_by(buyer_city) %>%
    summarise(counts = sum(get(indicator2)))

  contracts <- data_contracts %>%
    # mutate(Variable = paste0("<br>", counts, " ", input$dropdown, " Contracts")) %>% 
    
    mutate(Variable = paste0("<br>", "The points represents the number of contracts for the <br> composite WIRI indicator or one of its <br> sub-components (Investments, Operations, Interactions). <br> Top cities by total number of contracts are shown. <br> In ", buyer_city, " there are ", counts," ",input$dropdown, "-related contracts.")
    ) %>% 
    
    ggplot(aes(x = counts, y = reorder(buyer_city, counts),
               txt = Variable
               )) +
    geom_point(color = mycolor) +
    geom_segment(aes(x = 0, xend = counts, y = buyer_city, yend = buyer_city),
                 color = mycolor) +
    labs(y = NULL, 
         x = NULL,
         title = sprintf("%s contracts", input$dropdown)) +
    theme_classic()
  
  p <- ggplotly(contracts, tooltip = "txt")
  
  
  p <- layout(
    p,
    annotations = list(
      x = 0.5,
      y = -0.15,
      text = sprintf("Total number of %s contracts in top 10 cities", input$dropdown),
      showarrow = F,
      xref = 'paper',
      yref = 'paper',
      xanchor = 'center',
      yanchor = 'top',
      font = list(size = 8)
    )
  )
  
  p
  
  })

output$wiri_table <- renderTable({
  req(df_WIRI())
  df_WIRI() %>% 
    rename(
      City = buyer_city,
      Interactions = wiri_cui,
      Operations = wiri_ops,
      Investments = wiri_inv
    ) %>% 
    select(City, WIRI, Investments, Operations, Interactions) 
})

output$download_button <- renderUI({
  if (!is.null(df_wiri_cross())) {
    downloadButton("wiri_csv", "Download WIRI Timeseries")
  }
})

output$kpi_averages <- renderUI({
  
  req(df_WIRI())
  df <- df_WIRI()
  m_wiri <- round(mean(df$WIRI, na.rm = T))
  m_inv <- round(mean(df$wiri_inv, na.rm = T))
  m_op <- round(mean(df$wiri_ops, na.rm = T))
  m_cui <- round(mean(df$wiri_cui, na.rm = T))
  
  markdown(
    sprintf(
      "### Averages (all years): 
      #### WIRI <span style='color: #14b795;'>%s</span>, Investments <span style='color: #088efc;'>%s</span>, Operations <span style='color: #8b0000;'>%s</span>, Interactions <span style='color: #ffb90f;'>%s</span>",
      m_wiri, m_inv, m_op, m_cui
    ))

})

output$wiri_csv <- downloadHandler(
  filename = function() {
    "wiri.csv"  
  },
  content = function(file) {
    write.csv(df_wiri_time(), file, row.names = FALSE)
  }
)

}

