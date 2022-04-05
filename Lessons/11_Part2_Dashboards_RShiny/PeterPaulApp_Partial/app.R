#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
# Read in PeterPaul processed dataset for nutrients. 
# Specify the date column as a date
# Remove negative values for depth_id 
# Include only lakename and sampledate through po4 columns
nutrient_data <- read.csv("./Data/Processed/NTL-LTER_Lake_Nutrients_PeterPaul_Processed.csv", stringsAsFactors = TRUE)
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate)
nutrient_data <- nutrient_data   %>%
  filter(depth_id >= 0) %>%
  select(lakename, sampledate:po4)
  

#### Define UI ----
ui <- fluidPage(theme = shinytheme("yeti"),
  # Choose a title
  titlePanel("Peter Paul Data"),
  sidebarLayout(
    sidebarPanel(
      
      # Select nutrient to plot
      selectInput(inputId = "nutrient",
                  label = "Nutrient",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tp_ug"),
      
      # Select depth
      checkboxGroupInput(inputId = "depth",
                         label = "Depth",
                         choices = "depth_id",
                         selected = "depth_id",
      
      # Select lake
      checkboxGroupInput(inputId = "lake",
                         label = "Lake",
                         choices = "lakename",
                         selected = "lakename",

      # Select date range to be plotted
      sliderInput(inputId = "date",
                  label = "Date",
                  min = "2010-05-20",
                  max = "2016-08-16",
                  value = "2016-08-16",

    # Output: Description, lineplot, and reference
    mainPanel(
      # Specify a plot output
      plotOutput( "plot", brush = brushOpts(id = "scatterplot_brush")), 
      # Specify a table output
      tableOutput("plot_clickedpoints")
    )))

#### Define server  ----
server <- function(input, output) {
  
    # Define reactive formatting for filtering within columns
     filtered_nutrient_data <- reactive({
        nutrient_data %>%
         # Filter for dates in slider range
         filter(sampledate >"2010-05-20") %>%
         # Filter for depth_id selected by user
         filter(depth_id) %>%
         # Filter for lakename selected by user
         filter(lakename) 
     })
    
    # Create a ggplot object for the type of plot you have defined in the UI  
       output$scatterplot <- renderPlot({
        ggplot( ,nutrient_data
               aes_string(x = , y = , 
                          fill = , shape = )) +
          geom_point() +
          theme_classic() +
          scale_shape_manual() +
          labs(x = , y = , shape = , fill = ) +
          scale_fill_distiller()
          #scale_fill_viridis_c()
      })
       
    # Create a table that generates data for each point selected on the graph  
       output$mytable <- renderTable({
         brush_out <- brushedPoints( ,# dataset, 
                                     ) # input
       }) 
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

#### Questions for coding challenge ----
#1. Play with changing the options on the sidebar. 
    # Choose a shinytheme that you like. The default here is "yeti"
    # How do you change the default settings? 
    # How does each type of widget differ in its code and how it references the dataframe?
#2. How is the mainPanel component of the UI structured? 
    # How does the output appear based on this code?
#3. Explore the reactive formatting within the server.
    # Which variables need to have reactive formatting? 
    # How does this relate to selecting rows vs. columns from the original data frame?
#4. Analyze the similarities and differences between ggplot code for a rendered vs. static plot.
    # Why are the aesthetics for x, y, fill, and shape formatted the way they are?
    # Note: the data frame has a "()" after it. This is necessary for reactive formatting.
    # Adjust the aesthetics, playing with different shapes, colors, fills, sizes, transparencies, etc.
#5. Analyze the code used for the renderTable function. 
    # Notice where each bit of code comes from in the UI and server. 
    # Note: renderTable doesn't work well with dates. "sampledate" appears as # of days since 1970.
