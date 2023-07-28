library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# General Parameters -----------------------------------------------------------
n_areas = 10
df_contracts = read.csv("data/peru_contracts_example.csv")
df_survey = read.csv("data/peru_survey_example.csv")
inv_keywords = "construc+"
op_keywords = "maquinaria+|equip+"
int_keywords = "agua+|acua+|hidr+"

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
df_contracts$proc[df_contracts$procedure_type %in% c("OTHER", "NEGOTIATED")] <- 1
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


# Risk Parameters (unsupervised) --------------

generate_random_sets <- function() {
  # Generate a random permutation of numbers from 1 to 10
  perm <- sample(1:10)
  # Split the permutation into three sets
  set1 <- perm[1:3]
  set2 <- perm[4:6]
  set3 <- perm[7:10]
  return(list(set1, set2, set3))
}

# Desicion Period
decp_condition <- FALSE

while (decp_condition == FALSE) {

  sets <- generate_random_sets()
  set1_decp <- sets[[1]]
  set2_decp <- sets[[2]]
  set3_decp <- sets[[3]]
  
  df_contracts$corr_decp <- ifelse(df_contracts$decp10 %in% set3_decp, 100,
                                   ifelse(df_contracts$decp10 %in% set2_decp, 50,
                                          ifelse(df_contracts$decp10 %in% set1_decp, 0,
                                                 ifelse(is.na(df_contracts$decp10), 99, NA))))
  
  df_contracts$corr_decp <- factor(df_contracts$corr_decp)
  
  model_decp <- glm(singleb ~ corr_decp +
                      log(final_value) +
                      as.factor(year),
                    family = binomial,
                    data = df_contracts)
  
  model_decp <- broom::tidy(model_decp)
  corr_decp50 <- model_decp[model_decp$term == "corr_decp50", ]$estimate
  corr_decp100 <- model_decp[model_decp$term == "corr_decp100", ]$estimate
  decp_condition <- ifelse(corr_decp50 > 0 &
                             corr_decp100 > corr_decp50, TRUE, FALSE)
  
}

# Advert Period
submp_condition <- FALSE

while (submp_condition == FALSE) {
  
  sets <- generate_random_sets()
  set1_submp <- sets[[1]]
  set2_submp <- sets[[2]]
  set3_submp <- sets[[3]]
  
  df_contracts$corr_submp <- ifelse(df_contracts$submp10 %in% set3_submp, 100,
                                   ifelse(df_contracts$submp10 %in% set2_submp, 50,
                                          ifelse(df_contracts$submp10 %in% set1_submp, 0,
                                                 ifelse(is.na(df_contracts$submp10), 99, NA))))
  
  df_contracts$corr_submp <- factor(df_contracts$corr_submp)
  
  model_submp <- glm(singleb ~ corr_submp +
                      log(final_value) +
                      as.factor(year),
                    family = binomial,
                    data = df_contracts)
  
  model_submp <- broom::tidy(model_submp)
  corr_submp50 <- model_submp[model_submp$term == "corr_submp50", ]$estimate
  corr_submp100 <- model_submp[model_submp$term == "corr_submp100", ]$estimate
  submp_condition <- ifelse(corr_submp50 > 0 &
                             corr_submp100 > corr_submp50, TRUE, FALSE)
  
}

# Calculate the CRI ------------------------------------------------------------

main_cri_vars = c("singleb","corr_proc","corr_submp","corr_decp","ncft")

df_contracts[main_cri_vars] <- lapply(df_contracts[main_cri_vars], as.character)

df_contracts[main_cri_vars] <- lapply(df_contracts[main_cri_vars], function(x) ifelse(x == "99", NA_character_, x))

df_contracts[main_cri_vars] <- lapply(df_contracts[main_cri_vars], as.numeric)

df_contracts$cri <- rowMeans(df_contracts[, main_cri_vars], na.rm = TRUE)

df_contracts$cri_integrity <- 100 - df_contracts$cri

# Classify Water Contracts -----------------------------------------------------

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

locality_counts <- as.data.frame(table(df_water$buyer_city))
names(locality_counts) <- c("buyer_city", "n_df_water")
locality_counts <- locality_counts[locality_counts$n_df_water >= 10, ]
locality_counts <- locality_counts[order(locality_counts$n_df_water, decreasing = TRUE), ]
localities <- head(locality_counts, n = n_areas)

# Summarize Water Data Frame ---------------------------------------------------

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

dfw_sums = left_join(dfw_sums, df_survey)  %>%
  mutate(cui_bribery = (bribes/n)*100,
         cui_survey_int = (100-cui_bribery))

dfw_sums[dfw_sums=="NaN"] <- NA

dfw_sums <- ungroup(dfw_sums)

# Calculating WIRI Weights -----------------------------------------------------
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

# Calculating the Cross Sectional WIRI -----------------------------------------

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


# Calculating the Timeseries WIRI ----------------------------------------------

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


function(input, output, session) {
  
  # Maximum upload size 10mb
  options(shiny.maxRequestSize = 10*1024^2)
  
  # Upload FILE TEST
  observeEvent(input$file, {
    req(input$file)  # Check if the user has uploaded a file
    
    if (tolower(tools::file_ext(input$file$name)) == "csv") {
      output$file_message <- renderText("File is a CSV")
      # Add code to process the CSV file here, if required
    } else {
      output$file_message <- renderText("File is not a CSV")
    }
  })

  # WIRI Cross sectional View
    output$wiri_cross <- renderPlot({
      
      WIRI_small <- WIRI %>% 
        filter(buyer_city %in% localities$buyer_city)
      
      WIRI_small %>%
        select(buyer_city, WIRI, wiri_inv, wiri_ops, wiri_cui) %>% 
        gather(Indicator, Value, wiri_inv:wiri_cui) %>% 
        ggplot(aes(x = reorder(buyer_city, desc(WIRI)), y = Value)) +
        geom_col(aes(fill = Indicator), position = position_dodge(width=.9), width = .8) +
        geom_col(data = WIRI_small, aes(x = buyer_city, y = WIRI), color = "black", fill = "#14b795", alpha = 0.2) +
        geom_hline(yintercept = mean(WIRI_small$WIRI, na.rm = T), linetype="dashed")+
        geom_label(data = WIRI_small, aes(x = buyer_city, y = WIRI, label = round(WIRI, digits = 1)), 
                   color = "black",
                   size = 4)+
        coord_cartesian(ylim = c(0,100)) +
        labs(x=NULL, 
             y="WIRI Score",
             fill="WIRI Pillar:",
             subtitle = sprintf("Sample WIRI mean = %s", round(mean(WIRI_small$WIRI, na.rm = T), digits = 1))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

    })

}

