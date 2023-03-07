library(shiny)
library(tidyverse)
library(DT)

df <- read.csv("CSVnflbasicstatsNEW.csv")

df_position <- df %>% group_by(Position) %>% summarize(number_of_players=n())
df_position <- df_position[order(df_position$number_of_players,decreasing = TRUE),] 


df_college <- df%>% group_by(College) %>% summarize(number_of_players=n())
df_college <- df_college[order(df_college$number_of_players,decreasing = TRUE),] 

defense_positions <- c("CB", "DB", "DE", "DL", "DT", "FS", "ILB", "LB", "MLB", 
                       "NT", "SAF", "SS")
offense_positions <- c("C", "FB", "G", "OG", "OL", "OLB", "OT", "QB", "RB", 
                       "WR", "T", "TE")
specialteams <- c("K", "LS", "P")
pos_choice <- unique(df$Position)
no_blanks <- pos_choice[pos_choice != ""]
coloroptions <- c("cornflowerblue", "darkorchid4", "darkred", "darkorange3", 
                                  "darkolivegreen4")

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        "body {
          background-color: #D6EAF8;
        }"
      )
    )
  ),
  titlePanel("INFO 201 - Final Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               mainPanel(
                 br(),
                 h3("The Dataset"),
                 p("The dataset we will be using is the basic information on NFL 
               players and their statistics.  
               We found this dataset on Kaggle.  
               There are three groups of data (basic stats, career stats, 
               and game logs).  This data was collected by the National 
               Football League.  However, this data was last updated 6 years ago, 
               according to Kaggle, so there will likely be some out-of-date 
               information."),
                 h3("The Audience"),
                 p("Some audiences for this data could be anyone who is interested 
                 in football statistics, an individual in a fantasy football league, 
                 or an avid fan of America’s pastime.  These audiences overlap, 
                 so the target audience is ", tags$b("an enthusiastic football fan.")),
                 h3("Questions of focus"),
                 tags$ul(
                   tags$li("What is average height and weight for professional
                         football players, and does this vary by position?"),
                   tags$li("What college do professional football players most 
                         frequently hail from, and what positions do they play?"),
                   tags$li("What teams are more likely to do a rush play rather 
                         than a pass play? What are the average yards gained 
                         for rushing per yard?"))
               ),
               sidebarPanel(
                 p("BH3 group: Tawsif Ahmed, Maggie O'Brien, Carol Zhao, 
                 Yishi Zheng"),
                 img(alt = "NFL Logo", 
                     src = "https://upload.wikimedia.org/wikipedia/en/a/a2/National_Football_League_logo.svg"),
               )
      ),
      tabPanel("Height/Weight Analysis",
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
                                    choices = no_blanks, 
                                    selected = "CB",
                                    inline = FALSE,
                                    width = NULL),
                 radioButtons(inputId = "color",
                              label = "Select a color",
                              choices = coloroptions,
                              selected = coloroptions[1]),
               ),
               mainPanel(
                 plotOutput("plot1"),
                 verbatimTextOutput("summary"),
                 plotOutput("plot2")
               )
      ),
      tabPanel("Rushing/Passing"),
      tabPanel("College Data",
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
  
  output$summary <- renderPrint({ 
    sample <- sample_n(df, 10, replace = FALSE)
    print(sample)
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
      filter(!is.na(Weight), !is.na(Height)) %>%
      group_by(Position)%>%
      summarize(meanweight = mean(Weight),
                meanheight= mean(Height)) %>%
      ggplot(aes(x=meanweight, y=meanheight, size=(meanweight/meanheight), 
                 col=Position)) +
      geom_point() +
      labs(title = "Weight vs. Height per position",
           x = "Mean weight (lbs)", y = "Mean height (in)",
           size = "Weight/height (lbs/in)",
           color = "Position")
  })
  
  output$plot2 <- renderPlot ({
    df %>%
      filter(!is.na(Weight), Position != "",
             !is.na(Height), Current.Status == "Active") %>%
      ggplot(aes(x=Weight, y=Height)) +
      geom_point(col=input$color) +
      labs(title = "Weight vs. Height")
  })
}
names(df)

shinyApp(ui = ui, server = server)