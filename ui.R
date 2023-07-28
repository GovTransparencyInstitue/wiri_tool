library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Test of Plot"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          titlePanel("Upload CSV File"),
          fileInput("file", "Choose a CSV file"),
          textOutput("file_message")

        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("wiri_cross")
        )
    )
)
