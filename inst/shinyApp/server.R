shinyServer(function(input, output,session) {

  # rootVolumes <- c(Home = normalizePath("~"), getVolumes()(), WD = '.')
  rootVolumes <- c(Home = normalizePath("~/Documents/COMETS/TestSets/"), getVolumes()(), WD = '.')

  shinyFileChoose(input,'metabfile',
                  roots = rootVolumes,
                  session = session,
                  filetypes=c('','csv'))

  shinyFileChoose(input,'abundfile',
                  roots = rootVolumes,
                  session = session,
                  filetypes=c('','csv'))

  shinyFileChoose(input,'subjfile',
                  roots = rootVolumes,
                  session = session,
                  filetypes=c('','csv'))

  loadInputFiles <- reactive({
       metabfile=req(as.character(parseFilePaths(rootVolumes,input$metabfile)$datapath))
       subjfile=req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath))
       abundfile=req(as.character(parseFilePaths(rootVolumes,input$abundfile)$datapath))
       print("Input files are loaded")
       return(list(metabfile=metabfile,subjfile=subjfile,abundancesfile=abundfile))
})


 output$downloadExcell <- downloadHandler(
 	filename = "MyData.xlsx",
         content = function(outputfile) {
              COMETS::createCOMETSinput(
                 filenames= req(loadInputFiles()), outputfile=outputfile)
         })

 output$selectMETABid <- renderUI({
        mychoices=colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$metabfile)$datapath))))
        selectInput("metabids","Select Metabolite Id",mychoices)
  })

 output$selectSUBJid <- renderUI({
        mychoices=colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath))))
        selectInput("metabids","Select Subject Id",mychoices)
  })

 output$selectAGEid <- renderUI({
        mychoices=colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath))))
        selectInput("metabids","Select Age Id",mychoices)
  })

 output$selectBMIid <- renderUI({
        mychoices=colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath))))
        selectInput("metabids","Select BMI Id",mychoices)
  })

})
