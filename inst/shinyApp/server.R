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
             myvarmap=c()
             if(input$metabid != "Optional") {
		myvarmap$metabid=input$metabid }
             if(input$subjid != "Optional"){
                myvarmap$id=input$subjid }
             if(input$ageid != "Optional"){
                myvarmap$age=input$ageid }
             if(input$bmiid != "Optional"){
                myvarmap$bmi=input$bmiid }
          
             if(!is.null(myvarmap)) {myvarmap=as.data.frame(myvarmap)}

              COMETS::createCOMETSinput(
                 filenames= req(loadInputFiles()), varmap=myvarmap,outputfile=outputfile)
         })

 output$selectMETABid <- renderUI({
        mychoices=c("Optional",colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$metabfile)$datapath)))))
        selectInput("metabid","Select Metabolite Id",mychoices)
  })

 output$selectSUBJid <- renderUI({
        mychoices=c("Optional",colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath)))))
        selectInput("subjid","Select Subject Id",mychoices)
  })

 output$selectAGEid <- renderUI({
        mychoices=c("Optional",colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath)))))
        selectInput("ageid","Select Age Id",mychoices)
  })

 output$selectBMIid <- renderUI({
        mychoices=c("Optional",colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath)))))
        selectInput("bmiid","Select BMI Id",mychoices)
  })

})
