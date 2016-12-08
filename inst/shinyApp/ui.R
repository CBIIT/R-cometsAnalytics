shinyUI(fluidPage(

  # Application title
  titlePanel("Create Excell file (for input in COMETS Analytics) from 3 input CSV"),

  sidebarLayout(
    sidebarPanel(
                h4("Select Input Files"),
                p("Select Metabolite Meta-Data CSV File:"),
                shinyFilesButton('metabfile',
                                 'Select File',
                                 'Select Metabolite Metab-Data File',
                                 FALSE),
                p("Select Metabolite Abundances CSV File:"),
                shinyFilesButton('abundfile',
                                 'Select File',
                                 'Select Metabolite Abundance File',
                                 multiple=FALSE),
                p("Select Sample Meta-Data CSV File:"),
                shinyFilesButton('subjfile',
                                 'Select File',
                                 'Select Sample Meta-Data CSV File',
                                 multiple=FALSE),
		h4("Map variable names to ids (optional):"),
                p(""),
                htmlOutput("selectMETABid"),
                p(""),
                htmlOutput("selectSUBJid"),
                p(""),
                htmlOutput("selectAGEid"),
                p(""),
                htmlOutput("selectBMIid")
    ),
    mainPanel(
                downloadButton("downloadExcell",
                                 strong("Download Template Excell sheet")
                )
    )
  )
))

