list.of.packages <- c("shiny","plotly","BBmisc","factoextra", "NbClust", "dplyr", "sqldf", "class", "mclust")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)



library(shiny)
library(plotly)
library(BBmisc)
library(factoextra)
library(NbClust)
library(dplyr)
library(sqldf)
library(class)
library(mclust)
assign("holderList", list(), envir = globalenv())
assign("nbClustoutput", list(), envir = globalenv())

# holderList=list()
# nbClustoutput=list()
# Define server logic required to draw a histogram
shinyServer(function(input, output) {


  trainData <- reactive({
    file <- input$file1
    if(is.null(file)){return()}
    clusterTrainData= read.csv(file=file$datapath)
    clusterTrainData
  })
  testDataFileInput <- reactive({
    file <- input$file2
    if(is.null(file)){return()}
    clusterTestData= read.csv(file=file$datapath)
    clusterTestData
  })


    output$scatter3DOutput <- renderPlotly({
      scatter3dPerson(trainData())
    })


    output$cluster3dOutput <- renderPlotly({
      Normalized=FALSE;
      if(!input$normalization=="No"){
        Normalized=TRUE;
      }
      clusterPlot3d(trainData(),input$numOfClusters,Normalized)
    })

    output$optimumClusterDynamicsKmeans <- renderPlot({
     plot(nbClustoutput[["nbObject"]])

    })


    output$optimumClusterGraphKMeans <- renderPlotly({
      print(11)
      Normalized=FALSE;
      if(!input$normalization=="No"){
        Normalized=TRUE;
      }
      print(22)
      op <- optimumClusters(trainData(),Normalized)
      print(op)
      holderList[["OptimumClustersKMeans"]] <<-op
      clusterPlot3d(trainData(),op,Normalized)
    })


    output$testDataClusters <- renderPlotly({

      clusters=3
      if(!is.null(holderList[["OptimumClustersKMeans"]])){
        clusters=holderList[["OptimumClustersKMeans"]]
      }

      testDataForClassification=  testDataFileInput()
      classifyKNN(trainData = trainData(), testData = testDataForClassification,clusters = clusters, normalized = input$normalization, nearPoints = 5)
      plotClusterDatasetFromPredicted(holderList[["testDataWithCluster"]])

    })

    output$testDataClassification <- renderPrint({
      print(holderList[["KmeansClassificationResut"]])
    })

    output$testDataClassificationAcc <- renderPrint({

      accSum=(holderList[["KmeansClassificationResut"]])
      sumofDiag= accSum[1,1]+accSum[2,2]
      total=accSum[1,2]+accSum[2,1]+sumofDiag
      print((sumofDiag/total)*100)

    })
    output$testDataClassificationFP <- renderPrint({
      fpr=(holderList[["KmeansClassificationResut"]])
      print(paste0("False Positive: ",fpr[1,2]))
      print(paste0("False Positive Rate: ",fpr[1,2]/(fpr[1,2]+fpr[1,1])*100))
    })

    output$testDataClassificationFN <- renderPrint({
      fnr=(holderList[["KmeansClassificationResut"]])
      print(paste0("False Negative: ",fnr[2,1]))
      print(paste0("False Negative Rate: ",fnr[2,1]/(fnr[2,1]+fnr[2,2])*100))
      })



    ####GMM Begins Here#####
    output$cluster3dOutpuGMM <- renderPlotly({

      Normalized=FALSE;
      if(!input$normalizationGMM=="No"){
        Normalized=TRUE;
      }
      assignToClusterGMM(trainData = trainData(),normalized = Normalized,testData=testDataFileInput())
      clusterPlot3dGMM(trainData(),Normalized)
    })


    output$clusterWithProbabilitiesGMMDT <- renderDataTable({

      if(is.null(holderList[["testDataWithClusterGMM"]])){
        Normalized=FALSE;
        if(!input$normalizationGMM=="No"){
          Normalized=TRUE;
        }
        assignToClusterGMM(trainData = trainData(),normalized = Normalized,testData=testDataFileInput())
      }

     print(head(holderList[["testDataWithClusterGMM"]]))
      holderList[["testDataWithClusterGMM"]]

    })


    output$testDataClassificationGMM <- renderPrint({
      if(is.null( holderList[["trainDataWithClusterGMM"]])){
        assignToClusterGMM(trainData = trainData(), testData = testDataFileInput(),normalized = input$normalizationGMM)
      }
      trainDataGMMCLust = holderList[["trainDataWithClusterGMM"]]
      testDataGMMCLust= holderList[["testDataWithClusterGMM"]][,c(1,2,3,4)]
      holderList[["testDataWithClusterGMMResult"]] <<-findGender (trainDataGMMCLust,testDataGMMCLust, 5,originalTrainData = trainData(), originalTestData = testDataFileInput())
      print(holderList[["testDataWithClusterGMMResult"]])
    })


    output$testDataClassificationGMMAcc <- renderPrint({
      accSum=(holderList[["testDataWithClusterGMMResult"]])
      sumofDiag= accSum[1,1]+accSum[2,2]
      total=accSum[1,2]+accSum[2,1]+sumofDiag
      print((sumofDiag/total)*100)

    })
    output$testDataClassificationGMMFP <- renderPrint({
      fpr=(holderList[["testDataWithClusterGMMResult"]])
      print(paste0("False Positive: ",fpr[1,2]))
      print(paste0("False Positive Rate: ",fpr[1,2]/(fpr[1,2]+fpr[1,1])*100))
    })

    output$testDataClassificationGMMFN <- renderPrint({
      fnr=(holderList[["testDataWithClusterGMMResult"]])
      print(paste0("False Negative: ",fnr[2,1]))
      print(paste0("False Negative Rate: ",fnr[2,1]/(fnr[2,1]+fnr[2,2])*100))
    })


    #### Conclusions Output ###
    output$conclusionsOutput <- renderUI({
      tags$div(tags$h4("The optimum number of clusters,"),
               tags$h4("In KMeans: ", holderList[["OptimumClustersKMeans"]] ),
               tags$h4("In GMM: ",holderList[["numberOfGMMClusts"]]),
               tags$h4("Both the algorithms perform almost similar in terms of accuracy. However, GMM has an upper edge when compared to KMeans.") )
    })





})


