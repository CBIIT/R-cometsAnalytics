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

  getIDChoices <- reactive({
    mychoices=c(colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath)))),
       colnames(read.csv(req(as.character(parseFilePaths(rootVolumes,input$metabfile)$datapath)))))
    return(mychoices)
  }) 

  getVarToMap <- reactive({
    templatefile=NULL
    dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
    if(input$template == "Age") {
          templatefile <- file.path(dir, "cometsInputAge.xlsx")
    }
    else if(input$template == "Basic") {
          templatefile <- file.path(dir, "cometsInputBasic.xlsx")
    }
    else{return(NULL)}
    tomap<-suppressWarnings(fixData(readxl::read_excel(templatefile, 4)))$varreference
    return(tomap)
  })

output$OtherVarMaps <- renderUI({
    mychoices=req(getIDChoices())
    tomap=req(getVarToMap())
    lapply(tomap, function(i) {
           selectInput(i,paste0("Select ",i),c("Optional",mychoices))
    })
  })

 output$downloadExcell <- downloadHandler(
        filename = "MyData.xlsx",
         content = function(outputfile) {
             mychoices=req(getIDChoices())
             tomap=req(getVarToMap())
print(tomap)
             myvarmap=c() 
             myvarmap=as.data.frame(lapply(tomap, function(x) {
#                print(paste("Grabbing", eval(x), "which is ",input[[eval(x)]])) 
                as.character(unlist(input[[eval(x)]]))
                #myvarmap[[eval(x)]]=input[[eval(x)]]
                #print(paste("myvarmap entry",myvarmap[[eval(x)]]))
             }))
             colnames(myvarmap)=tomap
             print("Myvarmap before changing Optional")
             print(myvarmap)
	     myvarmap[which(myvarmap=="Optional")]="Needs User Input"

             for (i in 1:length(myvarmap)) {
                  myvarmap[[i]]=as.character(myvarmap[[i]])
             }

             print(paste("Dim",dim(myvarmap)))
	     print("Printing myvarmap")
             print(myvarmap)

             if(!is.null(myvarmap)) {myvarmap=as.data.frame(myvarmap)}

              COMETS::createCOMETSinput(template=tolower(req(input$template)),
                 filenames= req(loadInputFiles()), varmap=myvarmap,outputfile=outputfile)
   })

   output$file1 <- renderText({
             metabfile=as.character(parseFilePaths(rootVolumes,input$metabfile)$datapath)
	     if(length(metabfile)==0) {return(NULL)}
             else {return(paste("Loaded ",metabfile))}
   })

   output$file2 <- renderText({
             abundfile=as.character(parseFilePaths(rootVolumes,input$abundfile)$datapath)
             if(length(abundfile)==0) {return(NULL)}
             else {return(paste("Loaded ",abundfile))}
   })

   output$file3 <- renderText({
             subjfile=as.character(parseFilePaths(rootVolumes,input$subjfile)$datapath)
             if(length(subjfile)==0) {return(NULL)}
             else {return(paste("Loaded ",subjfile))}
   })

})
