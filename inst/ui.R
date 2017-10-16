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
  fluidRow(
    column(width=4,
           img(src="ClassMapR_logo.png", height=75)),
    column(width=8,
           br(),
           HTML("<b>How-to-cite</b>"),
           HTML("<p> Vaudor L. (2017) <i>classMapR 2.0: an R-shiny application for the unsupervised hierarchical clustering of pixels in multilayered rasters.</i></p>"))),
  hr(),
  fluidRow(
    column(width=4,
           h4("Results directory"),
           textOutput("resultDirectory"),
           br(),
           textInput("resultsFile","Change results directory to:","ClassMapR_Results")
    ),
   column(width=4,
          h4("Data directory"),
          p("The directory containing raw data files is:"),
          textOutput("rawdataDirectory"),
          br(),
          shinyDirButton("dir","Change raw data directory","Choose directory")),
   column(width=4,
          h4("Sample size"),
          textOutput("sampleSize"),
          br(),
          numericInput("nInd","Change sample size to",min=100,value=1000,max=20000)
          )
   ),#fluidRow
  hr(),
  fluidRow(
     column(width=4,
            h4("Descriptors"),
            p("The descriptors characteristics are set to:"),
            tableOutput("showParams")),
     column(width=8,
            h4("Change the descriptors characteristics:"),
            fluidRow(column(width=6,
                            uiOutput("filesUI1")),
                     column(width=6,
                            uiOutput("filesUI2"))
            ))
 ),#fluidRow
   hr(),h4("Step 1: read files and initiate process"),
   fluidRow(
     column(width=1,
            actionButton("doStep1","Do Step 1")),
     column(width=3,
            HTML("Once step 1 is done, you can explore the other panels regarding factorial analyses and hierarchical clustering, which are based on the subset.You can use these results to adjust the variables you want to include in the final clustering as well as their weights. You can then re-run Step 1")),
     column(width=8,
            h5("Outputs:"),
            HTML("<li>The variables' paths and characteristics (type, weight) are saved in <i>params.csv</i></li><li> A ff data table is created : <i>xxxxx.ff</i></li><li>A subset data table is created: <i>mydata.csv</i> </li><li>A subset data table with all categorical variables is created: <i>mydatacat.csv</i> </li><li>A subset data table with all quantitative variables is created: <i>mydataquanti.csv</i> </li>")
            )
  ),
  hr(),h4("Explore Step 1 results"),
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
  ),
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
  widths=c(2,10)
  ),#navlistPanel
  hr(),h4("Step2: Calculate distances between all data and subset"),
  fluidRow(
    column(width=1,
           actionButton("doStep2","Do Step 2")
           ),
    column(width=3,
           p("Step 2 is the most time-consuming of the three steps. Once it is done, you can use its results to run step 3 multiple times, with various number of classes for the resulting map.")
           ),
           column(width=8,
                  h5("Outputs:"),
                  HTML("<li>A data file containing information about the construction of the hierarchical tree is saved in <i>Mclust.csv</i></li><li>A data table with as many rows as there are pixels in the rasters, and the index of the closest individual in the subset.</li>"))
           ),
   hr(),h4("Step3: Calculate resulting raster"),
   fluidRow(column(width=1,
                   actionButton("doStep3","Do Step 3")),
            column(width=3,
                   sliderInput("nclust","final number of classes",min=2, max=30,value=2)
                   ),
            column(width=4,
                   h5("Outputs:"),
                   HTML("<li>An ASCII raster file with <i>k</i> classes: <i>result_</i>k<i>classes.asc</i></li>")
                   ),
            column(width=4,
                    h5("Already calculated:"),
                    uiOutput("listMaps"))
            ),
    hr(),h4("Explore Step 3 results"),
    plotOutput("resultmap")


))
