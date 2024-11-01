
library(rsconnect)
library(shiny)
library(leaflet)
library(DT)
library(plotly)
library(sf)
library(readxl)

shpEnt      <- st_read("data/spatial/mexico.shp")
shpEnt      <- shpEnt %>% select(state,geometry)

# Loading regional info
regions_ordered <- read_excel("data/regions_ordered.xlsx") %>% select(-Subregion)

# Upload malnutrition data
# load("data/spatial/Data.RData")
Data <- read_excel("data/data_Feb24.xlsx")
Data$Pctg <- round(Data$Pctg,2)

indicadores <- unique(Data$indicator)
periodos    <- unique(Data$year)
regiones    <- unique(Data$Region)
sexo        <- unique(Data$gender)
edad        <- unique(Data$age)

navbarPage("Dashboard", id="nav",

## Page 1

  tabPanel("Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
      ),

      leafletOutput("map", width="98%", height="98%"),
      # conditionalPanel(
      #   condition = "input.region != 'National'",
      #   # h5("Evolution by sex ", textOutput("region_text")
      #   # ),
      #   plotlyOutput("plot3", width="50%", height="40%")
      #   
      # ),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
        width = 750, height = "auto",

#        tags$p(
#          "Child health indicators", 
#          style = "font-size: 24px; color: #5397C5;"
#        ),
        
            # "Select Indicators"
            fluidRow(
              column(6,
                     selectInput("indicator", "Indicator:", indicadores),
              ),
              column(6,
                     selectInput("year", "Year:", periodos)
              )
            ),
             
            fluidRow(
              column(4,
                     selectInput("region", "Region:", c("National", regiones))
              ),
              column(4,
                     selectInput("sex", "Sex:", sexo)
              ),
              column(4,
                     selectInput("age", "Age:", edad)
              )
            ),

        # Plot 1
        h5("Comparison by sex and region"),
        plotlyOutput("Plot1", height = 250),
        
        # Plot 2
        # h5("Evolution by sex "#, textOutput("region")
        #    ),
        # plotlyOutput("Plot2", height = 300)
      conditionalPanel(
        condition = "input.region != 'National'",
       
        h5("Evolution by sex ", textOutput("region_text")
        ),
        plotlyOutput("plot2", height = 100, width = 200)
        # plotOutput("plot2")
                )
      ),

    )
  ),


## Page 2

  tabPanel("Data explorer",
           
           DT::dataTableOutput("Table")
           
           
  ),


## Page 3. Documentation

# tabPanel("Documentation",
#          tags$p(
#            "All ¸information comes from the Mexican Encuesta Nacional de Salud y Nutrición (ENSANUT)",
#            HTML(paste0(tags$a(href = "https://www.inegi.org.mx/programas/ensanut/2018/", "2018", target = "_blank"), " ")),
#            
#          ),
#          
#          fluidRow(
#            column(6,
#                   h3("Methodologies"),
#                   tags$p("Here, we describe the methodologies used in gathering and analyzing the data."),
#                   tags$ul(
#                     tags$li(tags$a(href = "path_to_methodology1.pdf", "Methodology 1", target = "_blank")),
#                     tags$li(tags$a(href = "path_to_methodology2.pdf", "Methodology 2", target = "_blank")),
#                     tags$li(tags$a(href = "path_to_methodology3.pdf", "Methodology 3", target = "_blank"))
#                   )
#            ),
#            column(6,
#                   h3("About us"),
#                   tags$p("Brief biographies and resumes of the individuals behind this project:"),
#                   tags$ul(
#                     tags$li(tags$a(href = "path_to_resume1.pdf", "Creator 1", target = "_blank")),
#                     tags$li(tags$a(href = "path_to_resume2.pdf", "Creator 2", target = "_blank"))
#                   )
#            )
#          ),
#          fluidRow(
#            column(12,
#                   h3("Additional Notes"),
#                   tags$p("Important notes and other details related to the data and visualizations presented in the other tabs."),
#                   tags$ul(
#                     tags$li("Note 1: ..."),
#                     tags$li("Note 2: ...")
#                   )
#            )
#          )
# ),

)
