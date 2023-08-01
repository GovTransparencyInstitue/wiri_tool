library(shiny)

fluidPage(

    titlePanel("WIRI Tool Prototype"),

    sidebarLayout(
        sidebarPanel(
          
          titlePanel("Upload Survey CSV File"),
          fileInput("file1", "Choose a CSV file"),
          textOutput("file_message1"),
          
          titlePanel("Upload Contracts CSV File"),
          fileInput("file2", "Choose a CSV file"),
          textOutput("file_message2"),
          
          textOutput("merge_message"),
          textOutput("weights_message"),

          uiOutput("download_button")

        ),

        mainPanel(
            plotOutput("wiri_cross"),
            plotOutput("histogram_plot")
        )
    )
)
