library(shiny)
library(tidyverse)
library(DT)

df <- read.csv("CSVnflbasicstats.csv")

df_position <- df%>% group_by(Position) %>% summarize(number_of_players=n())
df_position <- df_position[order(df_position$number_of_players,decreasing = TRUE),] 


df_college <- df%>% group_by(College) %>% summarize(number_of_players=n())
df_college <- df_college[order(df_college$number_of_players,decreasing = TRUE),] 

defense_positions <- c("CB", "DB", "DE", "DL", "DT", "FS", "ILB", "LB", "MLB", 
                       "NT", "SAF", "SS")
offense_positions <- c("C", "FB", "G", "OG", "OL", "OLB", "OT", "QB", "RB", 
                       "WR", "T", "TE")
specialteams <- c("K", "LS", "P")

ui <- fluidPage(
  titlePanel("INFO 201 - Final Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               br(),
               p("BH3 group numbers: Tawsif Ahmed, Maggie O'Brien, Carol Zhao, Yishi Zheng")),
      tabPanel("page 2",
               sidebarPanel(
                 selectInput(inputId = "groups", label = "Select offense, etc.", 
                             choices = c("Defense" = paste(defense_positions, 
                                                           collapse = ", "), 
                                         "Offense" = paste(offense_positions, 
                                                           collapse = ", "),
                                         "Special Teams" = paste(specialteams,
                                                                 collapse = ", ")),
                             selectize = FALSE),
                 checkboxGroupInput(inputId = "position_select",
                                    label = "Select position(s)",
                                    choices = unique(df$Position),
                                    selected = "CB",
                                    inline = FALSE,
                                    width = NULL),
               ),
               mainPanel(
                 plotOutput("plot1", width = "800px", height = "600px")
               )
      ),
      tabPanel("page 3"),
      tabPanel("college and position",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("n", 
                               "Choose sample size", 
                               min = 1, 
                               max = 1000,
                               value = 10),
                   fluidRow(
                     column(6, uiOutput("collegeCheckBox"))
                   )
                 ),
                 mainPanel(plotOutput("position"),
                           br(),
                           br(),
                           
                           DT::dataTableOutput("position_table"),
                           br(),
                           br(),
                           
                           DT::dataTableOutput("college_table"),
                           br(),
                           br(),
                           
                           p("We want to answer the question of '",strong("What college do professional football players most frequently hail from, 
                        and what positions do they play?"),"'."),
                           p("This page contains one bar chart and two tables. The bar shows the number of players
                        of each position. The two widgets on the side allow users to control the number of observations as well as filter the collges."),
                           p("The first table shows the number of players of each position in decreasing scale. The second table shows 
                        the number of players of each college, also in decreasing scale."),
                           br(),
                           
                           p(strong("The top 5 positions are:"),
                             tags$li("WR (Wide receiver)"),
                             tags$li("DE (Defensive end)"), 
                             tags$li("CB (Cornerback)"),       
                             tags$li("RB (Running back)"),       
                             tags$li("DB (Defensive back")),
                           br(),
                           
                           p(strong("The top 5 College with most players are:"),
                             tags$li("Notre Dame"),
                             tags$li("USC"),
                             tags$li("Ohio State"),
                             tags$li("Michigan"),
                             tags$li("Penn State"))
                 )
               )
      ),
      tabPanel("Conclusion"),
    )
  )
)


server <- function(input, output) {
  output$collegeCheckBox <- renderUI({
    checkboxGroupInput("College","Choose college", choices = unique(df$College))
    
  })
  
  sample <- reactive({
    s <- df %>% 
      filter(College %in% input$College)
    if(nrow(s) > input$n)
      sample_n(s, input$n)
    else
      s
  })
  
  sample1 <- reactive({
    s1 <- df %>% 
      filter(Position %in% input$Position)
    if(nrow(s1) > input$n)
      sample_n(s1, input$n)
    else
      s1
  })
  
  output$position <- renderPlot({
    ggplot(sample(), aes(x = Position,col= College))+
      geom_bar() + 
      labs(x = "Positions", y = "Number of players")
  })
  
  output$position_table <- DT::renderDataTable(df_position,options = list(pageLength = 5))
  
  output$college_table <- DT::renderDataTable(df_college,options = list(pageLength = 5))
  
  data_subset <- reactive({
    df %>%
      filter(Position %in% input$groups,
             Position %in% input$position_select)
  })
  
  output$plot1 <- renderPlot ({
    data_subset() %>% 
      filter(!is.na(Weight..lbs.), !is.na(Height..inches.)) %>%
      group_by(Position)%>%
      summarize(meanweight = mean(Weight..lbs.),
                meanheight= mean(Height..inches.)) %>%
      filter(Position %in% input$position_select) %>%
      ggplot(aes(x=meanweight, y=meanheight, size=(meanweight/meanheight), 
                 col=Position)) +
      geom_point() +
      labs(title = "Weight vs. Height per position",
           x = "Mean weight (lbs)", y = "Mean height (in)",
           size = "Weight/height (lbs/in)",
           color = "Position")
  })
}

shinyApp(ui = ui, server = server)
