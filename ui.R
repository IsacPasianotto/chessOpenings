ui <- fluidPage(
  
  useShinyjs(),
  
  # Load external files: 
  tags$head(tags$link(rel="stylesheet", type = "text/css", href="style.css")),
  tags$head(tags$script(src = "jsAddins.js")),
  
  h1("Chess opening explorer", class = "mainTitle"),
  
  actionButton("it", img(src = "italian.png", height = "30px"), class = "languageButton"),
  actionButton("en", img(src = "english.png", height = "28px"), class = "languageButton"),
  
  tabsetPanel(
    id = "menu",
    selected = "Main",
    
    # DATA & HELP tab --------------------------------------------------------
    
    tabPanel(title = "Data & help",
      tags$br(), 
      
      navlistPanel(
        id = "helpNavlist",
        well = FALSE, 
        widths = c(2, 10),
        fluid = FALSE,
        
        tabPanel("About",
          uiOutput("aboutPanel")
        ), # tabPanel "About"
        
        tabPanel("How to move",
          uiOutput("movePanel")
        ), # tabPanle "How to move
        
        tabPanel("Change dataset",
          uiOutput("datasetPanel"),
          fileInput("inputFile",  "Select a custom chess dataset", accept = ".csv"),
          textOutput("inputFileError")
        ), #tabPanel "Change dataset"
        
      ), # navlistPanel 
      
    ), # tabPanel "data & help"
    
    # MAIN tab ---------------------------------------------------------------
    
    tabPanel("Main",
      
      sidebarLayout(
        
        # sidebar panel ........................................................
        
        sidebarPanel(
          id = "boardPanel",
          width = 4,
          
          textInput("nextMove", "Insert the next move here:"),
          p(
            actionButton("doIt", label = "Move", class = "mainButton"),
            actionButton("undo", label = "Undo", class = "mainButton"),
            actionButton("reset", label = "Reset", class = "mainButton"),
            tags$img(src = "whiteIcon.svg", width="28px", id = "whiteIndicator"),     
            tags$img(src = "blackIcon.svg", width="28px", id = "blackIndicator"), #One of them should always be hidden
            
          ),
          span("Moves:", class = "infoValues"),
          textOutput("playedMain"),
          textOutput("doItError"),
          uiOutput("allowedMoves"),
          tags$br(),
          chessboardjsOutput("board", height = "auto"),
          tags$br(),
          p(
            span("Opening name: ", class = "infoValues"),
            textOutput("openingName", inline = TRUE)
          ),
          p(
            span("Opening ECO: ", class = "infoValues"),
            textOutput("openingECO", inline = TRUE)
          ),
          p(
            span("Games found: ", class = "infoValues"),
            textOutput("nGamesInDF", inline = TRUE)
          ),
          
        ), # sidebarPanel
        
        # main panel ......... ................................................
        
        mainPanel(
          
          tabsetPanel(
            id = "infoTypeNavbar",
            
            tabPanel(
              "Winning odds",
              tags$br(), 
              
              verticalLayout(
                fluid = FALSE, 
                
                span("Winning odds trend: ", class = "infoValues"),
                textOutput("winPlotError"), 
                plotOutput("winnerPlot", width = "50vw", height = "60vh"),
                numericInput("nCommon", min = 1, max = 5, value = 3, label = "Most common continuation: "),
                textOutput("commonContinuationsError"),
                tableOutput("commonContinuations"),
                
              ) # verticalLayout
              
            ), # tabPanel "Winning Odds"
            
            tabPanel(
              "ELO infos",
              
              br(),   
              selectInput("eloPlotType", "Which chart to display?",
                c("Generale ELO trend" = "plt1",
                  "Actual ELO focus" = "plt2",
                  "Average ELO trend" = "plt3")
              ), # selectInput 
              
              textOutput("eloPlotError"),
              plotOutput("eloPlot", width = "62vw", height = "62vh")
              
            ) # tabPanel "ELO infos"
            
          ) # tabsetPanel panel mainPanel (dx)
          
        ) # mainPanel
        
      ) # sidebarLayout
      
    ), # tabPanel "Main"
    
    # DETAILS tab ---------------------------------------------------------------
    
    tabPanel("Detalis", 
      
      span("Moves:", class = "infoValues"),
      textOutput("playedDetails"),
      span("Winning odds: ", class = "infoValues"),
      tableOutput("winningOddsDetails"),
      splitLayout(
        p("Details of white ELO distributions", class = "infoValues"),
        p("Details of black ELO distributions", class = "infoValues"),
      ), # splitLayout
      splitLayout(
        tableOutput("whiteEloDetails"),
        tableOutput("blackEloDetails")
      ) # splitLayout 
      
    ), # tabPanel "Details"
    
  ), # tabsetPanel
  
) # fluidPage ui

shinyUI(ui)
