library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

#Load Data
adult <- read.csv("AdultCare_StatisticalReport.csv")

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Adult Care Facility Statistical Report: 2013-2018", titleWidth = 450)

# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Gender Stats", icon = icon("bar-chart"), tabName = "genders"),
    menuItem("Count Stats", icon = icon("bar-chart"), tabName = "counts"),
    menuItem("Organization Type Stats", icon = icon("bar-chart"), tabName = "organizations"),
    menuItem("Table", icon = icon("th"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    
    # Inputs: select county to plot ----------------------------------------------
    selectInput(inputId = "county",
                label = "Select County",
                choices = sort(unique(adult$County)),
                selected = "Albany"),
   
    # Inputs: select quarter to plot ----------------------------------------------
    checkboxGroupInput(inputId = "type",
                       label = "Select Organziation Type",
                       choices = sort(unique(adult$Certified.Type)),
                       selected = c("S","F","E","N")),
                
    # Reporting year Selection ----------------------------------------------
    sliderInput(inputId = "year",
                label = "Year Range",
                min = min(adult$Reporting.Year),
                max = max(adult$Reporting.Year),
                value = c(min(adult$Reporting.Year), max(adult$Reporting.Year)),
                step = 1,
                sep = "")
    )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Quarter Stats page ----------------------------------------------
  tabItem("genders",
          # Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("female"),
            infoBoxOutput("male")
          ), 
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(width = 12,
                   tabPanel("Female Resident Distribution By Reporting Years", plotlyOutput("plot_gender")),
                   tabPanel("Male Resident Distribution By Reporting Years", plotlyOutput("plot_gender2")))
            )),
  
  # Quarter Stats page ----------------------------------------------
  tabItem("counts",
          # Input Boxes ----------------------------------------------
          fluidRow(
            valueBoxOutput("total"),
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(width = 8,
                   tabPanel("Overall Distribution by the Number of Resident", plotlyOutput("plot_count")))
  ))),
  
  # Organization Stats page ----------------------------------------------
  tabItem("organizations",
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(width = 8,
                   tabPanel("Resident Distribution by Organzaition Type", plotlyOutput("plot_type")))
  )),
  
  # Data Table Page ----------------------------------------------
  tabItem("table",
          fluidPage(
            box(title = "Statisical Report for Your Selection", DT::dataTableOutput("table"), width = 12)))
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")
  
# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  adultInput <- reactive({
    adult <- adult %>%
      
      #Year Filter
      filter(Reporting.Year >= input$year[1] & Reporting.Year <= input$year[2])
    
    #County Filter
    adult <- subset(adult, County %in% input$county)
   
    #Quarter Filter
    adult <- subset(adult, Certified.Type %in% input$type)
    
    # Return dataframe ----------------------------------------------
    return(adult)
  })
  
  # Plots showing the Gender stats -----------------------------------
  # Female
  output$plot_gender <- renderPlotly({
      ggplot(data = adultInput(),
             aes(Female.Census)) +
      geom_histogram(binwidth = 20) +
      facet_wrap(~Reporting.Year) +
      labs(x = "The Number of Female Residents", y = "Count")+
      theme_classic()
  })

  #Male
  output$plot_gender2 <- renderPlotly({
    ggplot(data = adultInput(),
           aes (Male.Census)) +
      geom_histogram(binwidth = 20) +
      facet_wrap(~Reporting.Year) +
      labs(x = "The Number of Male Residents", y = "Count") +
      theme_classic()
  })
  
  # A plot showing the Resident count stats -----------------------------------
  output$plot_count <- renderPlotly({
    ggplot(data = adultInput(),
           aes(x = End.Census)) +
      geom_histogram(aes(y = ..density..), colour = "black", fill = "white")+
      geom_density(color = "darkblue", fill = "lightblue", alpha = 0.4) + 
      labs (x = "Number of Residents per Organization") +
      theme_classic()
  })

  # A plot showing the Organization Type stats -----------------------------------
  output$plot_type <- renderPlotly({
    ggplot(data = adultInput(),
           aes(x = Certified.Type,
               y = End.Census)) +
      geom_boxplot(aes(fill = Certified.Type)) + 
      labs(fill="Type" , x = "Organization Type",  y = "The number of residents") +
      scale_fill_brewer(palette = "Blues") + 
      theme_classic()
  })
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    ad <-subset(adultInput(), select = c(Reporting.Year, Reporting.Quarter, Reporting.Organization, Certified.Type, County, Certified.Capacity, 
                                    End.Census, Male.Census, Female.Census))
    DT::datatable(ad, options = list(scrollX = TRUE))
  })
  
  # Male Average box ----------------------------------------------
  output$male <- renderValueBox({
    ai <- adultInput()
    
    infoBox("Avg Male Count:", value = round(mean(ai$Male.Census), 0), color = "blue", width = 6)
  })

  # Female Average box ----------------------------------------------
  output$female <- renderValueBox({
    ai <- adultInput()
    
    infoBox("Avg Female Count:", value = round(mean(ai$Female.Census), 0), color = "maroon", width = 6)
  })
  
  # Total Census box ----------------------------------------------
  output$total <- renderValueBox({
    ai <- adultInput()
    
    valueBox("Total Number of Residents", value = sum(ai$End.Census), color = "olive", width = 4)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)