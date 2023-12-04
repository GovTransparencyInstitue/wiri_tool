library(shiny)
library(shinythemes)
library(plotly)

fluidPage(theme = shinytheme("flatly"),

          titlePanel(markdown("[Water and Sanitation Sector Integrity Risk Index](http://www.govtransparency.eu/water-and-sanitation-sector-integrity-risk-index/)")),
          
          navbarPage(fluidPage(" "),
                               
                     tabPanel("Control Panel",
                              
                              fluidRow(
                              
                                column(4,  

                                fluidPage(h2("Survey Data"),
                                          selectInput(
                                                   inputId = "survey_tf", 
                                                   label = "Do you have survey data?",
                                                   choices = c("Select Option","No", "Yes"),
                                                   selected = "Select Option"
                                                 )
                                       ),

                              uiOutput("ui_survey"),
                              uiOutput("contracts_ui"),
                              # uiOutput("keywords_ui"), # uncomment for inline keywords
                              uiOutput("keywords_dropdown"),
                              br(),
                              fluidPage(
                              uiOutput("keywords_fileinput"),
                              uiOutput("key_message2"),
                              ),
                              br(),
                              fluidPage(uiOutput("action")),
                              uiOutput("diagnostics_ui")

                              ),
                              
                              column(8,
                                     
                                     markdown(
                                       '
                          ## What is <a href="http://www.govtransparency.eu/water-and-sanitation-sector-integrity-risk-index/" target="_blank">WIRI</a>?
                          
                          The Water Integrity Risk Index (WIRI) is a tool for measuring integrity in the water and sanitation sector at the city level. It allows you to assess smaller changes in integrity across cities within a country and over time.

                          WIRI produces a score between 0 and 100. *Cities with scores closer to 100 have lower risks of corruption.*

                          WIRI is not based on perceptions, but on both Big Data on procurement and (if available) surveys. To use this tool, you need data: 1) on public procurement contracts related to water and sanitation, and 2) (optionally) survey data on direct experiences with corruption (i.e. bribery) in the sector.

                          Once your data set(s) are in the the necessary format, you can upload it/them to the **control panel** (on the **left**). The manual will guide you in creating new water and sanitation procurement data sets from publically available information, as well as the necessary format for uploading survey data. **<a href="https://drive.google.com/drive/folders/1eUMiFvCRGX3KOMx8GmL2kJ0qkNTM8xRs?usp=drive_link" target="_blank">Here</a>** you can find the **the example data sets** on procurement and survey data for Peru (Spanish). **<a href="https://drive.google.com/drive/folders/16kbRFHP3DL2b6p28eUf6VS_TCgEoi1Pg" target="_blank">Here</a>** you can find a **public the repository of procurement and survey data sets** created by other users as well as the team behind WIRI. You may contribute to this repository by uploading your own data sets and notifying us using this **<a href="https://docs.google.com/forms/d/1Qc1HpNohV7tntYITGQD7r3ozxMmz0-Wu0NXz5rX8SEI/viewform?edit_requested=true">submission form</a>**. 
                          
                          Once all data sets are formatted correctly, they should be uploaded to the tool and it will calculate the WIRI score for each city/year. You will see the results summary in the **dashboard** panel (**above**).

                                       '
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
                                     
                          markdown(
                          '
                          ## How to use this tool?
                          
                          To use this tool, you will need to upload some data sets: procurement (**mandatory**), survey (**optional**) and keywords (**optional**). These data sets must be constructed in the format detailed in the **<a href="https://drive.google.com/drive/folders/1eUMiFvCRGX3KOMx8GmL2kJ0qkNTM8xRs?usp=drive_link" target="_blank">WIRI tool manual</a>**. We provide some **<a href="https://drive.google.com/drive/folders/1eUMiFvCRGX3KOMx8GmL2kJ0qkNTM8xRs?usp=drive_link" target="_blank">example data sets</a>** (Peru) but you may also upload your own.
                          
                          Once you have the required data set(s), please follow these steps:
                          
                          1. Select `no survey` OR upload **survey data** (CSV) when available
                          2. Upload **procurement data** (CSV)
                          3. Select **keywords** language (`Spanish`, `English`) OR upload your own `custom` keywords (CSV)
                          4. Click `submit`
                          5. The section **diagnostics** will inform you if the calculations were successfull
                          6. Go to the **dashboard** tab and view the results
                          7. In the dashboard, you can explore the composite **WIRI score** and its sub-components.
                          
                          '
                          ),
                          
                         div(class="embed-container",
                             HTML('<iframe src="https://www.youtube.com/embed/pQ72iamT1YI" frameborder="0" allowfullscreen></iframe>')),

                         markdown('
                                                            
                          ## WIRI Data Repository
                          
                          You may also consult (*and contribute to*) the repository of WIRI data sets <a href="https://drive.google.com/drive/folders/16kbRFHP3DL2b6p28eUf6VS_TCgEoi1Pg" target="_blank">here</a>. If you wish to share your data sets with other users, please fill out the <a href="https://docs.google.com/forms/d/1Qc1HpNohV7tntYITGQD7r3ozxMmz0-Wu0NXz5rX8SEI/" target="_blank">data sharing form</a> **before** you upload them to the shared repository. Keep in mind that data sets uploaded to the tool are **not automatically shared** in the repository. 
                          
                                  ')                     

                         ))),

                    tabPanel("Dashboard",
                             h2("Water Integrity Risk Indicator Results"),
                             fluidRow(
                               column(10,
                                      uiOutput("kpi_averages")
                               ),
                               column(2,
                                      selectInput("dropdown", "Select an indicator:",
                                         choices = c("WIRI",  "Investments", "Operations", "Interactions"),
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
                                             plotlyOutput("wiri_cross", height = "250px"),
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
                       ),
                    
                    tabPanel("WIRI Manual",
                             
                             h2("How to contruct data sets for WIRI"),
                             br(),
                             tags$iframe(style="height:600px; width:100%", src="WIRI_manual.pdf"),
                             br(),
                             h2("Data Sharing Form"),
                             br(),
                             HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLScBM6QARyb0aCQTmC4IFW9ccAyLFKj8czr7RReiegxYV9m2Yw/viewform?embedded=true" width="100%" height="500px" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>')
                    )
                
                     ),
          br(),
          
          fluidPage(
          fluidRow(style = "background-color: #2d3d50;",
                   br()),
          fluidRow(
            br(),
            column(4,
                   
                   tags$a(href = "https://www.govtransparency.eu/", 
                          target = "_blank",
                          tags$img(src = 'gti.png', height = '70px')
                   )
                   # img(src = 'gti.png', height = '70px')
            ),
            column(4,
                   
    
            ),
            column(4,
                   
                   tags$a(href = "https://www.waterintegritynetwork.net/", 
                          target = "_blank",
                          tags$img(src = 'WIN_logo_new.jpeg', height = '55px')
                   )
                   # img(src = 'WIN_logo_new.jpeg', height = '55px') 
            )

          ),
          fluidRow(
            HTML('
    <div style="text-align: center; margin-top: 20px;">
        <p style="font-size: 16px; color: #333;">
            For any issues or questions regarding this online tool, please email the developer: 
            <a href="mailto:alhdzsz@gmail.com" style="color: #29c0a1; text-decoration: none;">
                Alfredo Hernandez Sanchez
            </a>
        </p>
    </div>
'),
            
            HTML('
            <div style="text-align: center; margin-top: 20px;">
        <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
            <img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" />
        </a>
        <br />
        This work is licensed under a
        <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">
            Creative Commons Attribution-ShareAlike 4.0 International License
        </a>.
    ')
          )
          )

)
