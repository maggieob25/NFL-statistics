library(shiny)
library(tidyverse)
library(DT)

df <- read_delim("CSVnflbasicstatsNEW.csv")
df2 <- read_delim("Game-Plays.csv")

df_position <- df%>% filter(!is.na(Position)) %>% group_by(Position) %>% summarize(number_of_players=n())
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
  titlePanel("INFO 201 - Final Project"),
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction",
               br(),
               h3("The Data"),
               p("The", tags$a("dataset", href = "https://www.kaggle.com/datasets/kendallgillies/nflstatistics"),
               "we will be using is the basic information on 
               NFL players and their statistics.  We found this dataset on Kaggle.  There are three groups of data (basic stats, career stats, and game logs).  This data was collected by the National Football League.  However, this data was last updated 6 years ago, according to Kaggle, so there will likely be some out-of-date information.  
"),
               h3("The Audience"),
               p("Some audiences for this data could be anyone who is interested 
                 in football statistics, an individual in a fantasy football league, 
                 or an avid fan of America’s pastime.  These audiences overlap, so the 
                 target audience is an", tags$b("enthusiastic football fan.")),
               h3("The Questions"),
               tags$li("What college do professional football players most 
               frequently hail from, and what positions do they play?"),
               tags$li("What teams are more likely to do a rush play rather than
                       a pass play? What are the average yards gained for rushing 
                       per yard?"),
               tags$li("What is the average weight and height of each football
                       position?"),
               tags$img(src = "https://upload.wikimedia.org/wikipedia/en/a/a2/National_Football_League_logo.svg"),
               h3("The Group"),
               p("BH3 group numbers: Tawsif Ahmed, Maggie O'Brien, 
                 Carol Zhao, Yishi Zheng")
                ),
      tabPanel("Height/Weight Info",
               sidebarPanel(
                 checkboxGroupInput(inputId = "position_select",
                                    label = "Select position(s)",
                                    choices = no_blanks, 
                                    selected = c("QB", "RB", "WR", "TE"),
                                    inline = FALSE,
                                    width = NULL),
                 selectInput(inputId = "groups", label = "Select offense, etc.", 
                             choices =  c("Defense","Offense","Special Teams"),
                             selected = "Defense")
               ),
               mainPanel(
                 plotOutput("plot1"),
                 verbatimTextOutput("summary"),
                 tags$hr(),
                 plotOutput("plot2")
               )
      ),
      tabPanel("Game plays",
               sidebarPanel(
                 selectInput(inputId = "Decades", label = "Select a Decade", 
                             choices = c("Before 2000", "After 2000")),
                 p(em("Some years are missing due to lack of data from those years")),
                 uiOutput("CheckboxTeam")
               ),
               mainPanel(
                 textOutput("Info1"),
 #                textOutput("Info2"),
                 tableOutput("Game_Table")
               )
      ),
      tabPanel("College and Position",
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
      tabPanel("Conclusion",
               br(),
               h2("Conclusion"),
               p("Among all the positions,", strong("Tackle (T)"),",", strong("Offensive Line (OL)"),"are typically 
                 the tallest and heaviest according to the dataset.", strong("Wide Receiver (WR)"), 
                 "and,", strong("Right-back (RB)"), "tend to be the opposite. "),
               p("Although most of the teams tend to", strong("pass more than rush"), "there are a few 
                 teams that are found to attempt more rushing than passing. For example,", 
                 em("St Louis Rams"), "attempted more rushing than passing in 2003 and 2004.", 
                 em("Tennesse Titans"), "attempted more rushing than passing in 2001 and 2003."),
               p("Our data showed that professional football players most frequently hail 
                 from", strong("Notre Dame, USC, Ohio State, Michigan, and Penn State."), "The most popular
                 positions are wide receiver, defensive end, cornerback, running back, and defensive back."),
               h2("Quality of Data"),
               p("We are able to answer most of our proposed questions accurately to an extent. There are a 
                 lot of missing years and unuseful variables that would affect the outcome of the analysis. 
                 For example, the passing vs. rushing data for", em("Cleveland Indians"), "is completely missing both before 
                 and after 2000, because", em("Cleveland Indians"), "played only one season in 1931 before it turned into a 
                 baseball team. Yet, the earliest year in this dataset is 1933, which means the data for the team 
                 is not properly recorded. There are a lot of other similar cases in the dataset, which might affect 
                 the overall team statistics. \n"),
               h2("Future Ideas"),
               p("For future research, we can explore the", strong("salary of players"), "to see what positions or teams are making 
                 the most money. We can also explore the", strong("demographics"), "of the players.")
      )
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
    sample() %>% 
      filter(!is.na(Position)) %>% 
      group_by(Position) %>% 
      mutate(count = n()) %>% 
      ggplot(aes(x = Position, y = count, fill = factor(College)))+
      geom_bar(stat = "identity", position = "dodge") + 
      labs(x = "Positions", y = "Number of players")
  })
  
  output$position_table <- DT::renderDataTable(df_position,options = list(pageLength = 5))
  
  output$college_table <- DT::renderDataTable(df_college,options = list(pageLength = 5))
  
  data_subset <- reactive({
    df %>%
      filter(Position %in% input$position_select) 
  })
  
  output$plot1 <- renderPlot ({
    data_subset() %>% 
      filter(!is.na(Weight),
             !is.na(Height)) %>%
      group_by(Position)%>%
      summarize(meanweight = mean(Weight),
                meanheight= mean(Height)) %>%
      ggplot(aes(x=meanweight, y=meanheight, size=(meanweight/meanheight), 
                 col=Position)) +
      geom_point() +
      labs(title = "Weight vs. Height per position",
           x = "Mean weight (lbs)", y = "Mean height (in)",
           size = "Weight/height (lbs/in)",
           color = "Position")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  data_subset2 <- reactive({
    if(input$groups == "Defense"){
    s3 <- df %>%
      filter(Position %in% defense_positions) 
    }
    else if(input$groups == "Offense"){
      s3 <- df %>%
        filter(Position %in% offense_positions) 
    }
    else if(input$groups == "Special Teams"){
      s3 <- df %>%
        filter(Position %in% specialteams) 
    }
  })
  
  output$plot2 <- renderPlot ({
    data_subset2() %>% 
      filter(!is.na(Weight),
             !is.na(Height)) %>%
      group_by(Position) %>% 
      summarize(meanweight = mean(Weight),
                meanheight= mean(Height)) %>%
      ggplot(aes(x=meanweight, y=meanheight, size=(meanweight/meanheight),
             col=Position))+
      geom_point() +
      labs(title = "Weight vs. Height per 'team'",
           x = "Weight (lbs)", y = "Height (in)", color = "Position",
           size = "Weight/height (lbs/in)")+
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$CheckboxTeam <- renderUI({
    
    checkboxGroupInput("Team_Select", "Choose Team",
                       choices = unique(df2$Team), 
                       selected = "Seattle Seahawks")
  })
  
  Team_data<- reactive({
    s2 <- df2 %>% 
      filter(Team %in% input$Team_Select) %>% 
      mutate(Passing = Pass_Attempts, Rushing = Rush_attempts) %>% 
      filter(!is.na(Passing) & !is.na(Rushing))
  })
  
  output$Game_Table <- renderTable({
    if(input$Decades == "Before 2000"){
    t <- Team_data() %>% 
      filter(Year < 2000) %>% 
      group_by(Team, Year) %>% 
      summarize(Passing_Attempts = mean(Pass_Attempts), Pass_Completion_Rate = mean(Pass_Completion_Rate),
                Avg_Passing_Yds = mean(Avg_Passing_Yards), Rushing_Attempts = mean(Rush_attempts),
                Avg_Rushing_Yds = mean(Avg_Rushing)
                )
    }
    else{
      t <- Team_data() %>% 
        filter(Year > 2000) %>% 
        group_by(Team, Year) %>% 
        summarize(Passing_Attempts = mean(Pass_Attempts), Pass_Completion_Rate = mean(Pass_Completion_Rate),
                  Avg_Passing_Yds = mean(Avg_Passing_Yards), Rushing_Attempts = mean(Rush_attempts),
                  Avg_Rushing_Yds = mean(Avg_Rushing)
        )
    }
    
  })
  
  output$Info1 <- renderText({
    if(input$Decades == "Before 2000"){
      avg_Pass <- Team_data() %>% 
        filter(Year < 2000) %>% 
        summarize(Passing_Attempts = mean(Pass_Attempts))
      avg_Rush <- Team_data() %>%
        filter(Year < 2000) %>% 
        summarize(Rushing_Attempts = mean(Rush_attempts))
    if(avg_Pass > avg_Rush){
      paste("The NFl team/teams had more pass attempts than rush
            attempts. The total pass attempts over the years was",
            round(avg_Pass,2), "and rush attempts was", round(avg_Rush,2),
            "\n")
    }
    else{
      paste("The NFl team/teams had more pass attempts than rush
            attempts. The total pass attempts over the years was",
            `round(avg_Rush,2)`, "and rush attempts was", round(avg_Pass,2),
            "\n")
      }
    }
    else if(input$Decades == "After 2000"){
      avg_Pass1 <- Team_data() %>%
        filter(Year > 2000) %>% 
        summarize(Passing_Attempts = mean(Pass_Attempts))
      avg_Rush1 <- Team_data() %>% 
        filter(Year > 2000) %>% 
        summarize(Rushing_Attempts = mean(Rush_attempts))
      if(avg_Pass1 > avg_Rush1){
        paste("The NFl team/teams had more pass attempts than rush
            attempts. The total pass attempts over the years was",
              round(avg_Pass1,2), "and rush attempts was", round(avg_Rush1,2),
              "\n")
      }
      else{
        paste("The NFl team/teams had more pass attempts than rush
            attempts. The total pass attempts over the years was",
              `round(avg_Rush1,2)`, "and rush attempts was", round(avg_Pass1,2),
              "\n")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
