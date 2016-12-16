shinyUI(fluidPage(

  # Application title
  titlePanel("Create Excell file (for input in COMETS Analytics) from 3 input CSV"),

  sidebarLayout(
    sidebarPanel(
                h4("Select Template"),
                selectInput("template","Select Template",c("Age","Basic"),multiple=FALSE,selected="Age"),
                h4("Select Input Files"),
                p("Select Metabolite Meta-Data CSV File:"),
                shinyFilesButton('metabfile',
                                 'Select File',
                                 'Select Metabolite Metab-Data File',
                                 FALSE),
                br(),
                textOutput("file1"),
                br(),
                p("Select Metabolite Abundances CSV File:"),
                shinyFilesButton('abundfile',
                                 'Select File',
                                 'Select Metabolite Abundance File',
                                 multiple=FALSE),
                br(),
                textOutput("file2"),
                br(),
                p("Select Sample Meta-Data CSV File:"),
                shinyFilesButton('subjfile',
                                 'Select File',
                                 'Select Sample Meta-Data CSV File',
                                 multiple=FALSE),
                br(),
                textOutput("file3"),
                br(),
                h4("Map variable names to ids (optional):"),
                uiOutput("OtherVarMaps")
    ),
    mainPanel(
                downloadButton("downloadExcell",
                                 strong("Download Template Excell sheet")
                )
    )
  )
))

