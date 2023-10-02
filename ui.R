library(shiny)
library(shinythemes)
library(plotly)

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
                                     
                                     markdown(
                                       "
                          ## What is [WIRI](http://www.govtransparency.eu/water-and-sanitation-sector-integrity-risk-index/)?
                          
                          The Water Integrity Risk Index (WIRI) is a measurement tool for measuring integrity in the water and sanitation sector at the level of cities. It relies on public procurement and survey data in order to calculate a composite indicator which ranges between 0 and 100, where *cities with scores closer to 100 are those that have lower risks of corruption* in this critical sector. 

                          Unlike existing indexes, WIRI incorporates both Big Data (for procurement) and traditional statistical (for surveys) methods to develop a multidimensional indicator based on objective data rather than **perceptions** of corruption. To use this tool, two data sets are required, one containing public procurement contracts related to water and sanitation, and another one containing survey data on direct experiences with corruption (i.e. bribery). 

                          In order to get as clear a picture as possible of corruption and integrity trends, WIRI is calculated at the level of cities. This enables the assessment of smaller changes in integrity both across cities and over time. Though WIRI has been implemented in several countries already, this tool is designed to calculate the index for cities within a single country. 

                          The public procurement data that is uploaded into this online tool is classified into three pillars: a) *operations* contracts for day-to-day water and sanitation activities, b) *investments* for large scale projects, and c) *interactions* for contracts signed with water utilities. This classification is based on keywords (see below), which can be defined in this tool once the data has been uploaded correctly. Survey data consists of ratios of bribery admissions for water and sanitation services at the city level. 

                          This online tool and the accompanying [manual]() will guide you through the process of constructing these two data sets in a specific format in order to upload them to the control panel on the left. Once they have been created and uploaded correctly, the tool will automatically calculate the WIRI score for each city and display a summary of the results in the dashboard panel (above). You may find example data sets [here]() and a walkthrough of the main steps below. 
                                       "
                                     ),
                              

                                     tags$head(tags$style(HTML("
                                                                .embed-container { 
                                                                  position: relative;
                                                                  padding-bottom: 56.25%; 
                                                                  height: 0; 
                                                                  overflow: hidden;
                                                                  max-width: 100%;
                                                                } 
                                                                
                                                                .embed-container iframe,
                                                                .embed-container object,
                                                                .embed-container embed { 
                                                                  position: absolute;
                                                                  top: 0;
                                                                  left: 0;
                                                                  width: 100%;
                                                                  height: 100%;
                                                                }
                                                              "))),
                                     
                                     div(class="embed-container", 
                                         HTML('<iframe src="https://www.youtube.com/embed/ss4mRCAijRI" frameborder="0" allowfullscreen></iframe>')),
                                     
                          markdown("
                          ## How to use this tool?
                          
                          To use this tool, you will need two data sets (procurement and survey) which are constructed in the format detailed in the [WIRI tool manual](). You may also use the example data sets (see below).
                          
                          Once you have the two required data sets, please follow these steps:
                          
                          1. Upload survey data (CSV)
                          2. Upload procurement data (CSV)
                          3. Submit the keywords for each of the pillars using regular expressions (see below)
                          4. Make sure the **diagnostics** section verifies that pillars are calculated correctly
                          5. Click `submit`
                          6. Go to the **dashboard** in the next tab and view the results
                          7. Change the parameters to observe different WIRI sub indicators
                          
                          #### You may download the **example data sets** [here](https://drive.google.com/drive/folders/1eUMiFvCRGX3KOMx8GmL2kJ0qkNTM8xRs?usp=drive_link).
                          
                          "),
                          
                         div(class="embed-container",
                             HTML('<iframe src="https://www.youtube.com/embed/r9ZeF3J2T7Y" frameborder="0" allowfullscreen></iframe>')),

                                              
                          markdown("

                          ### Keywords and Regex
                          
                          One of the main advantages of WIRI is its built-in flexibility. The procurement contracts that are related to water and sanitation which are uploaded to the tool are classified into three pillars based on keywords. However, since these keywords can be very context-specific, this tool allows users to modify and expand on the list of keywords that are used to classify contracts into one of the three pillars. To achieve this, it uses **regex** or regular expressions. 
                          
                          Regular Expression (regex) is a powerful and flexible text-searching tool that is used to match patterns within strings (i.e. text as data). It provides a concise and flexible means for matching strings of text, such as particular characters, words, or patterns of characters, facilitating the ability to search within text-based data efficiently. 
                          
                          In the command box (left panel), the section **Keywords** will appear after you have uploaded the procurement and survey data sets. The three boxes will allow you to define keywords to flag water contracts into one of three categories: a) operations, b) investments, and c) interactions. Below is an example on how to (re)define such keywords:
                          * 'agua+': This will match the substring 'agua' followed by one or more occurrences of the character 'a'. For example, it will match 'agua', 'aguaa', 'aguaaa', etc.
                          * '|': This is the OR operator in regex, which allows for matching one of the alternatives.
                          * 'agua+|hidr+': This will match all characters containing 'agua' OR 'hidr' such as 'aguas' OR 'hidrografia'.
                          
                          For example, if one of the water utilities in your data set is named *calidad y servicio*, then you may change the default `agua+|acua+|hidr+` in the **Interactions** box to include this: `agua+|acua+|hidr+|calidad y servicio`. Based on the language and content of your own procurement data set, you may modify these keywords as you see fit. 
                          
                          You may read more about the theory behind these categories in the user [manual]() or in the [working paper](https://www.govtransparency.eu/water-and-sanitation-sector-integrity-risk-index/).
                      
                          ## Troubleshooting 
                          
                          In order for the WIRI tool to work properly, it requires two CSV data sets with specific variable names. The diagnostics box under the file upload will tell you if the files uploaded are correct or not. It may specify that the 'file is not a CSV' or that the 'variable names are incorrect'. To fix these issues, please consider the following:
                          
                          * Make sure that you are uploading CSV files and not Excel files.
                          * Make sure that you are uploading the data sets to their specified box (e.g. procurement in the procurement upload section)
                          * Verify that the names of the files are written exactly as specified.
                          * Verify that no varibles are missing. If there is no information for them, leave them as empty columns (i.e., `NA`) with the correct name. 
                          * Verify that all of the keyword boxes (three pillars) are specified in the format detailed above `word1|word1|prefix+`
                          
                          Variable names should read **exactly** as follows, the order does not matter:
                          
                          * For **Surveys**: `buyer_city`, `year`, `n`, `bribes`
                          
                          * For **Procurement**: `contract_title`, `buyer_name`, `winner_name`, `buyer_city`, `final_value`, `bids_count`, `bid_deadline`, `firstcall_date`, `procedure_type`, `award_date`
                                             
                          ")))),

                    tabPanel("Dashboard",
                             h2("Water Integrity Risk Indicator Results"),
                             fluidRow(
                               column(10,
                                      uiOutput("kpi_averages")
                               ),
                               column(2,
                                      selectInput("dropdown", "Select an indicator:",
                                         choices = c("WIRI", "Operations", "Investments", "Interactions"),
                                         selected = "WIRI"))
                               ),
                             fluidRow(
                               column(5,
                                      fluidPage(
                                        div(style = "height: 500px; overflow-y: scroll;",
                                            tableOutput("wiri_table")
                                        ))),
                               column(7,
                                      
                                      fluidRow(
                                      column(6,
                                            plotlyOutput("wiri_cross", height = "250px")
                                            ),
                                      column(6,
                                             plotlyOutput("number_contracts", height = "250px")
                                             )
                                      ),
                                      plotlyOutput("wiri_time", height = "250px")                                      )
                             ),
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
