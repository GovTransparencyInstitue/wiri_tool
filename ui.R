library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("flatly"),

          titlePanel(markdown("[Water and Sanitation Sector Integrity Risk Index](http://www.govtransparency.eu/water-and-sanitation-sector-integrity-risk-index/)")),
          
          navbarPage(fluidPage(" "),
                               
                     tabPanel("Control Panel",
                              
                              fluidRow(
                              
                                column(4,  
                              
                              titlePanel("Upload Survey File"),
                              fileInput("file1", "Choose a CSV file"),
                              textOutput("file_message1"),
                              uiOutput("contracts_ui"),
                              uiOutput("keywords_ui"),
                              
                              br(),
                              h2("Diagnostics"),
                              textOutput("key_message"),
                              textOutput("merge_message"),
                              textOutput("weights_message")

                              ),
                              
                              column(8,
                              
                                     markdown("
                          ## What is [WIRI](http://www.govtransparency.eu/water-and-sanitation-sector-integrity-risk-index/)?
                          
                          
                          WIRI is a novel measurement of integrity in the water and sanitation (W&S) sector at the level of cities. This involves a data-driven approach to develop a composite Water Integrity Risk Index (WIRI) made up of a host of objective proxy indicators as well as survey-based measures of corruption experience to identify and assess integrity risks in the urban W&S sector in selected urban areas across different countries and regions around the world.
                          
                          The novelty of our approach comes from applying Big Data methods to administrative data and survey datasets collected in collaboration with local organizations in order to develop a comprehensive, replicable, and actionable integrity risk indicator. To our knowledge, there is no integrity risk index for the W&S sector to date. Existing indexes focus on two aspects. The first is country-level reports of perception of corruption from experts rather than citizens. The second focuses on state-owned enterprises’ transparency which is related to integrity but only partially overlaps with it. By contrast, we propose an indicator based on objective data (public procurement contracts) and direct citizen perceptions (locally sourced survey data) at the city level.
                          
                          ## How to use this tool?
                          
                          To use the tool please follow these steps:
                          
                          1. Upload the survey data (if you do not have any survey data, upload a blank .csv)
                          2. Upload the procurement data 
                          3. Submit the keywords for each of the pillars using regular expressions (regex)
                          4. Make sure the diagnostics section verifies that pillars are calculated correctly
                          5. Go to the dashboard in the next tab and view the results
                          6. Change the parameters to observe different WIRI sub indicators 
                   
                          ")))),

                    tabPanel("Dashboard",
                             selectInput("dropdown", "Select an option:", 
                                         choices = c("WIRI", "Operations", "Investments", "Interactions"),
                                         selected = "WIRI"),
                             fluidRow(
                               column(5,
                                      fluidPage(
                                        div(style = "height: 800px; overflow-y: scroll;",
                                            tableOutput("wiri_table")
                                        ))),
                               column(7,
                                      plotOutput("wiri_cross"),
                                      plotOutput("number_contracts")
                                      )
                             ),
                             br(),
                             plotOutput("wiri_time"),
                             br(),
                             verbatimTextOutput("output_text"),
                             br(),
                             uiOutput("download_button")
                       )
                     ),
          
          fluidRow(
            br(),
            column(4,
                   img(src = 'gti.png', height = '70px')
            ),
            column(4),
            column(4,
                   img(src = 'win.jpg', height = '70px')
            )
          )

)
