###    STARTUP STUFF    ##################################################################
#The code below must be manually run before the app
## START OF REQUIRED CODE ##########
list.of.packages <-
    c(
        "shiny",
        "shinyBS",
        "shinyjs",
        "shinydashboardPlus",
        "dagitty",
        "ggdag",
        "openxlsx",
        "plyr",
        "dplyr",
        "tidyr",
        "magrittr",
        "stringr",
        "data.table",
        "ggplot2",
        "plotly",
        "shinycssloaders",
        "logging",
        "rstudioapi",
        "MazamaCoreUtils"
        ,
        "shinyalert",
        "broom",
        "tidyverse"
    )
new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages))
    install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# Sets the working directory to the current directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
tmpDir <- getwd()

# Used for debugging. Errors will be saved to the files below. Can set the debugging level to different levels.
#
logger.setup(
    errorLog = file.path(tmpDir, "ERROR.log"),
    warnLog = file.path(tmpDir, "WARN.log"),
    infoLog = file.path(tmpDir, "INFO.log"),
    debugLog = file.path(tmpDir, "DEBUG.log"),
    traceLog = file.path(tmpDir, "TRACE.log")
)
logger.setLevel(DEBUG)
logger.setLevel(TRACE)

logger.info("Logger Successfully loaded")

# Prevents the CSVs and other files from importing everything as factors
options(stringsAsFactors = F)
## END OF REQUIRED CODE ##########


# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyjs(),
    useShinyalert(),
    # Application title
    titlePanel("DAGstats V0.1"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textAreaInput(
                "DAG.text.input",
                label = h3("DAG input"),
                value = ifelse(exists("DAGdefault"), DAGdefault, "Enter DAG..."),
                rows = 5
            ),
            fileInput(
                "DAGdata.file.input",
                "Input Data File",
                multiple = TRUE,
                buttonLabel = ">"
            ),
            actionButton("DAG.button.load", label = "Load"),
            
            disabled(selectInput(
                "exposure.selectinput",
                h3("Exposure"),
                choices = "",
            )),
            
            disabled(selectInput(
                "outcome.selectinput",
                h3("Outcome"),
                choices = "",
            )),
            
            disabled(actionButton("logit.button", label = "Do logit"))
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(fluidPage(fluidRow(
            column(width = 12,
                   fluidRow(plotOutput("DAGplot")),
                   fluidRow(
                       tabsetPanel(id = "results.tabset", type = "tabs")
                   ))
        ))),
        
    )
)







# Define server logic required to draw a histogram
server <- function(input, output, session) {
    observeEvent(input$DAG.button.load, {
        logger.debug("Load button hit")
        
        # Load the DAG
        DAG <<-
            dagitty(paste("dag {", input$DAG.text.input, "}", sep = ""))
        DAGvars <<- names(DAG)
        DAGdefault <<- input$DAG.text.input
        
        # Load the data file
        
        infile <- input$DAGdata.file.input
        
        rm(AGOGdata)
        AGOGdata <<- read.xlsx(infile$datapath, colNames = TRUE)
        
        # Make sure all the DAG nodes are in the datafile
        missing.vars <- DAGvars[which(!(DAGvars %in% names(AGOGdata)))]
        logger.trace(paste("Missing vars: ", missing.vars))
        
        # Show error for missing args and return
        if (length(DAGvars[missing.vars]) != 0) {
            missing.vars.str <-
                paste("Missing data columns for:\n\n",
                      paste(as.character(missing.vars), collapse = ", "))
            logger.trace(missing.vars.str)
            shinyalert("Invalid Input Data!", missing.vars.str, type = "error")
            #return()
        }
        
        DAGplot <- ggdag(DAG) + theme_dag()
        
        output$DAGplot <- renderPlot({
            DAGplot
        })
        
        enable("exposure.selectinput")
        enable("outcome.selectinput")
        enable("logit.button")
        
        updateSelectInput(session, "exposure.selectinput", choices = DAGvars)
        
        updateSelectInput(session, "outcome.selectinput", choices = DAGvars)
        
    })
    
    
    observeEvent(input$logit.button, {
        logger.debug("Logit button hit")
        
        #fit.sets <<- calculate(DAG, AGOGdata, input$exposure.selectinput, input$outcome.selectinput)
        
        fit.summary <- lapply(fit.sets, summary)
        fit.oddsconf <-
            lapply(fit.sets, function (i) {
                exp(cbind(OR = coef(i), confint(i)))
            })
        
        for (i in 1:length(fit.summary)) {
            logger.debug(paste("Adding tab", i))
            
            appendTab("results.tabset",
                      tabPanel(paste("Result", i),
                               
                               fluidRow(
                                   column(6, renderPrint({
                                       fit.summary[i]
                                   })),
                                   column(6, renderPrint({
                                       fit.oddsconf[i]
                                   }))
                               )),
                      select = (i == 1))
            
            
        }
        
    })
    
}

calculate <- function (dag, df, exposure, outcome) {
    adjSets <- adjustmentSets(dag, exposure, outcome, effect = "total")
    
    fitSets <- c()
    
    #adjSet <- adjSets[1]
    
    for (adjSet in adjSets) {
        adjSet %<>% unlist(use.names = FALSE)
        
        confounding <-
            if (length(adjSet) == 0)
                ""
        else
            paste("+", paste(adjSet, collapse = " + "))
        
        formula <-
            paste(paste(outcome, "~", exposure, sep = " "),
                  confounding,
                  sep = "")
        
        print(formula)
        
        formula %<>% as.formula()
        
        fit <- glm(formula, data = df, family = binomial)
        
        fitSets %<>% append(list(fit))
        
    }
    
    return(fitSets)
}

# Run the application
shinyApp(ui = ui, server = server)
