#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # Application title
  fluidRow(column(width=4,
                  img(src="ClassMapR_logo.png", height=75)),
           column(width=8,
                  h5("How-to-cite"),
                  HTML("<p> Vaudor L. (2017) <i>classMapR 2.0: an R-shiny application for the unsupervised hierarchical clustering of pixels in multilayered rasters.</i></p>"))),
    tabsetPanel(
    tabPanel("Calculation",br(),
                 fluidRow(
                   column(width=4,
                          wellPanel(
                              h3("Data directory"),
                              p("The directory containing raw data files is:"),
                              textOutput("dataDirectory"),
                              br(),
                              shinyDirButton("dir","Change raw data directory","Choose directory")
                          ),
                          hr(),
                          wellPanel(
                              h3("Sample size"),
                              p("The sample size is:"),
                              textOutput("n"),
                              br(),
                              numericInput("nInd","Change sample size to",min=100,value=1000,max=20000)
                          ),
                          hr(),
                              wellPanel(
                              h3("Results directory"),
                              textInput("resultsFile","Choose a name for the directory where results will be written:","ClassMapR_Results"),
                              textOutput("resultDirectory")
                              )
                          ),
                     column(width=8,
                         wellPanel(
                             fluidRow(
                                   column(width=6,
                                          h3("Descriptors"),
                                          p("The descriptors characteristics are set to:"),
                                          tableOutput("showParams")),
                                   column(width=6,
                                          h4("Change the descriptors characteristics:"),
                                          uiOutput("filesUI"))
                             )
                         )
                    )
                 ),#fluidRow
                 hr(),
                 actionButton("readFiles","Step 1: read files and initiate process"),
                 actionButton("calculate_distances","Step 2: Calculate distances between all data and subset"),
                 sliderInput("nclust","final number of classes",min=2, max=30,value=2),
                 actionButton("extrapolate_to_nclust","Step 3: Calculate resulting raster")
      ),#tabPanel Calculation
            tabPanel("Exploration",br(),
            navlistPanel(
              tabPanel("User-defined parameters",
                       tableOutput("params")),
              tabPanel("Variables description",
                       tabsetPanel(
                         tabPanel("table",tableOutput("data")),
                         tabPanel("map",
                                  fluidRow(column(width=2,uiOutput("numMap")),
                                           column(width=10,plotOutput("map")))),
                         tabPanel("plot",plotOutput("imdesc"))
                       )#tabsetPanel
                       ),#tabPanel
              tabPanel("Statistical analysis",
                       tabsetPanel(
                       tabPanel("quanti: PCA",
                                plotOutput("imPCA")),
                       tabPanel("cat: MCA",
                                plotOutput("imMCA")),
                       tabPanel("all: coinertia",
                                plotOutput("imCoin"))
                       )#tabsetPanel
                       ),#tabPanel

              tabPanel("Classification",
                       sliderInput("nclust_exp",
                                   "number of classes",
                                   min=2,max=30,value=2),
                       tabsetPanel(
                         tabPanel("Classification tree",
                                  plotOutput("tree")),
                         tabPanel("Cluster description",
                                  textInput("clusterlabel","Name cluster you want to describe","a"),
                                  plotOutput("clustDesc")),
                         tabPanel("Cut tree",
                                  plotOutput("inertia"))
                       )#tabsetPanel
                       ),#tabPanel
              tabPanel("Result",
                       plotOutput("resultmap")),
              widths=c(2,10)
            )#navlistPanel
            )#tabPanel Exploration
  )#tabsetPanel
))
