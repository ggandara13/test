# ---- Module 10 Assignment ----

# ---- Set Up ----
# Import libraries
# Remember that if you need to install a package locally,
# use install.packages("shiny")
library(shiny)
library(dplyr)
library(ggplot2)

# Save this script and the income.csv file (available on Canvas under Module 10)
# in the same folder on your computer
# Use setwd() to point R to this correct folder
# Import income.csv to an object called income

setwd("/Users/gandara/Documents/R-Course/web10")
income_data <- read.csv("income.csv")

# ---- Submission ----
# 1. Fill in the 13 blanks in the code below
## NOTE: The numbers (e.g. 1_____) are used as placeholders to identify 
## the blank spaces - make sure you delete them as you fill in the appropriate code
# 2. Go to Canvas and open the Atomic Exercise for Module 10
# 3. Answer the questions based on the completed code
## NOTE: Each question will ask something like "What piece of code goes in blank #4?"
# 4. Run your shiny app and take a screenshot
# 5. Submit the screenshot to the Canvas Module 10 Discussion Board

# ---- Context ----
# Your goal is to create a shiny application that can be used
# to explore how occupation is related to hours worked per week
# and capital loss, based on which income category an individual
# belongs to (<=50K or > 50K).

# ---- Instructions: ui object ----
# 1. Set up the app to have a title panel, and a sidebar layout with a side panel and main panel

# 2. Set the title to "Module 10 Assignment"

# 3. On the sidebar panel, include three interactive pieces:
# (a) A drop down menu where users can choose either "<=50K" or "50K" as the income category
# - call this input "subset_income"
# (b) A drop down menu where users can choose either "hours_per_week" or "capital_loss" as the y-axis variable
# - call this input "set_yaxis"
# (c) A set of checkboxes where users can select which occupations should be included in the figure
# - by default, when the app starts, have all of the boxes checked
# - call this input "select_occupation"
# - hint: use checkboxGroupInput()

# 4. On the main panel, include a figure that reactis to changes in the user input
# - more details can be found in the server function instructions

# ---- Assignment Code: ui object ----
# Define the ui object

ui <- fluidPage(
  titlePanel("Module 10 Assignment"),
  sidebarLayout(
    sidebarPanel
    (
      selectInput(inputId = "subset_income", label = "Select Income", 
                  choices = unique(income_data$income), multiple = TRUE),
      selectInput(inputId = "set_yaxis", label = "Select variable by:", 
                  choices = c(HoursPerWeek = "hours_per_week", CapitalLoss = "capital_loss")),
      selectInput(inputId = "select_occupation", label = "Include Occupations:", 
                  choices = unique(income_data$occupation), multiple = TRUE),
      selectInput(inputId = "subset_country", label = "Select Countries", 
                  choices = unique(income_data$country), multiple = TRUE),
      selectInput(inputId = "mynumber", label = "Choose a Number", choices = 1:5),
    ),
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)
# Define server function


server <- function(input, output) {
  create_subset <- reactive(income_data %>%
             filter(capital_loss > 0) &
             income == input$subset_income & 
             occupation %in% input$subset_occupation
  )
  #output$myfigure <- renderPlot(ggplot(create_subset()) +
  #                                geom_point(aes_string(x = "subset_occupation", y = "set_yaxis"))
  #)
  
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                            geom_point(aes_string(x = "age", y = "capital_gain", col = "gender")))
  
  #output$myfigure <- renderPlot(
  #  plot(x = input$mynumber, y = 1, xlim = c(0,5))
  #  )
}

shinyApp(ui, server)


#### ------arriba probado





server <- function(input, output) {
  create_subset <- reactive(income_data %>%
                              country %in% input$subset_country
                            )
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  geom_point(aes_string(x = "1", y = "1"))
  )
}

shinyApp(ui, server)



#-----
ui <- fluidPage(
  titlePanel("Module 10 Assignment"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "subset_income", label = "Select Income", 
                  choices = unique(income$income), multiple = TRUE)),
    selectInput(inputId = "set_yaxis", label = "Select variable by:", 
                choices = c(HoursPerWeek = "hours_per_week", CapitalLoss = "capital_loss")),
    checkboxGroupInput(inputId = "subset_occupation", label = "Select Ocuppation", 
                       choices = unique(income$occupation), multiple = TRUE),
    mainPanel(plotOutput(outputId = "myfigure"))
  )
)



ui <- fluidPage(
  # App Title
  #1
  titlePanel("Module 10 Assignment"),
  # Page Layout
  sidebarLayout(
    # Side Panel
    sidebarPanel(
      # Interactive piece 1: inputID = "subset_income"
      #2_____(3_____),
      selectInput(inputId = "subset_income", label = "Select Income", 
                  choices = unique(income$income), multiple = TRUE),
      
      # Interactive piece 2: inputId = "set_yaxis"
      #4_____(5_____),
      selectInput(inputId = "set_yaxis", label = "Select variable by:", 
                  choices = c(HoursPerWeek = "hours_per_week", CapitalLoss = "capital_loss")),
      
      # Interactive piece 3: inputId = "subset_occupation"
      #6_____(7_____)),
      checkboxGroupInput(inputId = "subset_occupation", label = "Select Ocuppation", 
                         choices = unique(income$occupation), multiple = TRUE)
    # Main panel
    #mainPanel(8_____)
    
    mainPanel(plotOutput(outputId = "myfigure"))
    
  )
)

# ---- Instructions: server function ----
# 1. Subset the income data to only include records where:
# - capital loss is greater than 0
# - income category is the input selected above ("subset_income"), and 
# - occupation is in the input selected above ("subset_occupation")

# 2. Render a boxplot with occupation on the x axis and the input selected above ("set_yaxis") on the y axis

# ---- Assignment Code: server function ----
# Define server function
server <- function(input, output) {
  create_subset <- reactive(income %>%
                              filter(capital_loss > 0) &
                              income == input$subset_income & 
                              occupation %in% input$subset_occupation
                            )
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                              geom_point(aes_string(x = "subset_occupation", y = "set_yaxis"))
                              )
}

shinyApp(ui, server)

##
#---- cortado
  
  output$myfigure <- renderPlot(ggplot(create_subset()) +
                                  geom_boxplot(aes_string(x = "occupation", y = "set_yaxis")) +
                                  theme_bw(18) +
                                  theme(axis.text.x = element_text(angle = 90, hjust = 1)
                                  ))

------

  #output$myfigure <- 10_____(ggplot(11_____) +
                                  # Boxplot of x = occupation, y = defined by input
                                  #12_____(13_____) +
                                  #theme_bw(18) +
                                  #theme(axis.text.x = element_text(angle = 90, hjust = 1)))
#}

# ---- Run App ----
shinyApp(ui, server)
