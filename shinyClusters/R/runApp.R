
#' Multi Access Variable function
#'
#' This function creates list for data sharing within the functions in Global Environment.
#' @keywords GlobalVariables
#' @export
#' @examples
#' createGlobalVars()
createGlobalVars <- function(){
  assign("holderList", list(), envir = globalenv())
  assign("nbClustoutput", list(), envir = globalenv())
}



#' Run the Shiny Cluster Application
#'
#' This function runs the shiny application using the runApp command.
#' @keywords runApplication
#' @export
#' @examples
#' runClusterApplication()
runClusterApplication <- function(){
library(shiny)
  createGlobalVars()
  # assign("holderList", list(), envir = globalenv())
  # assign("nbClustoutput", list(), envir = globalenv())
  appDir <- system.file("shiny-examples", "myapp", package = "shinyClusters")
  runApp(appDir, display.mode = "normal")
}



        ################################################
####  ####  ####  #### Kmeans Functions   ####  ####  ####  ####
       ################################################
#' A 3D Cluster Plot function using Kmeans
#'
#' This function allows you to plot the test data points in the clusters, formed by train dataset
#' @param trainDataWithCluster Test dataset with assigned clusters
#' @keywords plotCluster
#' @export
#' @examples
#' plotClusterDatasetFromPredicted(dataset)

plotClusterDatasetFromPredicted <-function(trainDataWithCluster){
  p <- plot_ly(trainDataWithCluster, opacity=0.7 ,x = ~age, y = ~height, z = ~weight,
               color = ~as.factor(Cluster),
               marker = list(symbol = 'circle', size = 3)) %>% add_markers() %>% layout
  (scene = list(xaxis = list(title = 'Age'),yaxis = list(title = 'Height'),zaxis = list(title = 'Weight')))
  p
}


#' A 3D Scatterplot function
#'
#' This function allows you to plot the data points in 3D with its color based on gender.
#' @param trainData Dataset to be plotted
#' @keywords scatter3d
#' @export
#' @examples
#' scatter3dPerson(trainData = dataset)

scatter3dPerson <- function(trainData){
  p <- plot_ly(trainData,hovertext=c("age")
               ,opacity=0.7 ,x = ~age, y = ~height, z = ~weight ,color = ~Gender, colors = c('#BF382A', '#0C4B8E'),
               marker = list(symbol = 'circle', size = 3))  %>% layout
  (scene = list(xaxis = list(title = 'Age'),yaxis = list(title = 'Height'),zaxis = list(title = 'Weight')))
  p
}

#' A Normalization function
#'
#' This function allows you to normalize the variables of the dataset
#' @param trainData Dataset to be normalized
#' @keywords normalize
#' @export
#' @examples
#' normalizeData(trainData = dataset)

normalizeData <- function(trainData){
  trainDataScaled=as.data.frame(scale(trainData[,-c(4,5)], center = TRUE, scale = TRUE))
  trainDataScaled
}


#' A 3D Cluster Plot function
#'
#' This function allows you to plot the data points segregated by clusters created using Kmeans
#' @param trainData Dataset to be passed for clustering
#' @param clusters Denotes number of clusters. Takes input range 1-20. Defaults to 2
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @keywords ClusterPlot
#' @export
#' @examples
#' clusterPlot3d(dataset, clusters = 3, normalized = FALSE)
clusterPlot3d <- function(trainData, clusters, normalized){

  if(normalized==TRUE){
    trainData=normalizeData(trainData)
  }
  else{
    trainData=trainData[,-c(4,5)]
  }
  res=kmeans(trainData ,as.numeric(clusters))
  trainDataClusters=trainData
  trainDataClusters$Clusters=res$cluster
  trainDataClusters$size=10
  centeroids <- as.data.frame(res$centers)
  centeroids$size=15
  centeroids$Clusters='Centers'
  trainDataClusters = rbind(trainDataClusters,centeroids)
  p <- plot_ly(trainDataClusters,hovertext=c("age") ,opacity=0.7 ,x = ~age, y = ~height,
               z = ~weight ,color = ~as.factor(Clusters),size=~as.numeric(size),sizes = c(5, 10),
               marker = list(symbol = 'circle',sizemode = 'diameter'))%>% layout(scene = list(xaxis = list(title = 'Age'),
                                                                                              yaxis = list(title = 'Height'),zaxis = list(title = 'Weight')))
  p
}

#' An Optimum Clusters function
#'
#' This function allows you to find the optimum number of clusters in Kmeans algorithm using NBClust function
#' @param trainData Dataset to be considered
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @keywords OptimumClusters
#' @export
#' @examples
#' optimumClusters(trainData = dataset, normalized = FALSE)

optimumClusters <- function(trainData, normalized){
  if(normalized==TRUE){
    trainData=normalizeData(trainData)
  }
  else{
    trainData=trainData[,-c(4,5)]
  }
  nb <- NbClust(trainData, distance = "euclidean", min.nc = 2,
                max.nc = 10, method = "kmeans")
  p=fviz_nbclust(nb)
  nbClustoutput[["nbObject"]] <<-p
  num_clusters = p$data %>% arrange(freq) %>% top_n(1)
  as.integer(as.character(num_clusters$Number_clusters))
}

#' A Cluster Assignment function
#'
#' This function allows you to assign the test data points to the clusters that are formed by train dataset in KMeans
#' @param trainData Dataset to be considered for clustering
#' @param clusters Denotes number of clusters. Takes input range 1-20. Defaults to 2
#' @param testData Dataset to be considered to assign its data points on clusters
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @keywords OptimumClusters
#' @export
#' @examples
#' assignToCluster(trainData = dataset1, clusters = 2, normalized = FALSE, testData = dataset2)

assignToCluster <- function(trainData, clusters ,normalized, testData){
  fullDataSet <- trainData
  if(normalized==TRUE){
    trainData=normalizeData(trainData)
  }
  else{
    trainData=trainData[,-c(4,5)]
  }
  res=kmeans(trainData ,clusters)
  trainDataClusters=fullDataSet
  trainDataClusters$Cluster=res$cluster

  holderList[["data"]]<<-trainDataClusters
  holderList[["result"]]<<-res

  if(normalized==TRUE){
    testData=normalizeData(testData)
  }
  else{
    testData=testData[,-c(4,5)]
  }
  testData$Cluster = mapDataToCluster(testData,holderList[["result"]]$centers)[,1]
  holderList[["trainDataWithCluster"]]<<-trainDataClusters
  holderList[["testDataWithCluster"]]<<-testData
  holderList

}

#' Mapping of datapoints to Clusters function(Called internally by 'assignToCluster' function)
#'
#' This function allows you to find appropriate cluster for the test data points using distance from center of the clusters
#' @param testData Dataset to be considered to assign its data points on clusters
#' @param centers Takes value. Defaults to 2
#' @keywords mapDataToClusters
#' @export
#' @examples
#' mapDataToCluster(testData = dataset, centers = 2 )

#function to find appropriate cluster using distance from center
mapDataToCluster <- function(testData, centers){
  centers = as.data.frame(centers)
  minDistCluster=data.frame(matrix(ncol = 1, nrow = nrow(testData)))

  for(i in 1:nrow(testData)){
    tempHolder=  data.frame(matrix(ncol = 2, nrow = nrow(centers))   )
    for(j in 1:nrow(centers)){
      tempHolder[j,]=c(j,dist(rbind(centers[j,],testData[i,])))
    }
    minDistCluster[i,]=tempHolder[tempHolder[,2]== min(tempHolder[,2]),][1]
  }
  minDistCluster
}

#' A Classification function
#'
#' This function allows you to predict the gender of the test data points.
#' @param trainData Dataset to be considered for clustering
#' @param clusters Denotes number of clusters. Takes input range 1-20. Defaults to 2
#' @param testData Dataset to be considered to assign its data points on clusters
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @param nearPoints Number of neighbor train data points to be considered
#' @keywords classifyKNN
#' @export
#' @examples
#' classifyKNN(trainData = dataset1, clusters = 3, normalized = FALSE, testData = dataset2, nearPoints = 3)


classifyKNN <- function(trainData,clusters,normalized,testData,nearPoints){

  assignToCluster (trainData,clusters ,normalized,testData)
  KmeansClassificationResut=findGender(holderList[["trainDataWithCluster"]], holderList[["testDataWithCluster"]], nearPoints, originalTrainData = trainData, originalTestData = testData)
  holderList[["KmeansClassificationResut"]]<<- KmeansClassificationResut
}


#' A findGender function(Called internally by 'classifyKNN' function)
#'
#' This function allows you to find nearest n points in a cluster to the test data point to predict the Gender
#' @param trainData Dataset to be considered for clustering using Kmeans
#' @param clusters Denotes number of clusters. Takes input range 1-20. Defaults to 2
#' @param testData Dataset to be considered to assign its data points on clusters
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @param nearPoints Number of neighbor train data points to be considered. Defaults to sqrt of no. of observations in a train dataset
#' @keywords gender
#' @export
#' @examples
#' findGender(trainDataWithCluster, testDataWithCluster, nearPoints)

#Function to find nearest n points in a cluster to the test data point to predict the Gender
findGender <- function(trainDataWithCluster, testDataWithCluster, nearPoints, originalTrainData, originalTestData ){
  # Execute assignToCluster first and use the outputs as inputs for this function

  # trainDataWithCluster
  # height   weight age male Gender Cluster
  # 1 145.415 39.37746  42    0 Female       1
  # 2 162.560 56.75570  30    0 Female       1

  # testDataWithCluster
  # height   weight age Cluster
  # 1 136.525 31.86484  65       1
  # 2 163.830 62.99259  35       1

  # print(head(trainDataWithCluster))
  # print(head(testDataWithCluster))

  testData= originalTestData
  trainData= originalTrainData

  testDataWithCluster$ActualGender = testData$Gender
  trainClusters = (table(trainDataWithCluster$Cluster))
  testClusters = (table(testDataWithCluster$Cluster))
  trainClusterHolder= list()
  testClusterHolder= list()

  for(i in 1: length(trainClusters) ){
    trainClusterHolder [[names(trainClusters[i])]]= sqldf( paste("select * from trainDataWithCluster where Cluster==",names(trainClusters[i])) )
  }


  for(i in 1: length(testClusters) ){
    testClusterHolder [[names(testClusters[i])]]= sqldf( paste("select * from testDataWithCluster where Cluster==",names(testClusters[i])) )
  }


  for(i in 1: length(testClusters) ){

    for(j in 1: length(trainClusters)){
      if( names(testClusters[i]) == names(trainClusters[j]) ){
        nearPoints=as.integer(sqrt(nrow(trainClusterHolder[[names(trainClusters[j])]])))
        predictedGender=knn(trainClusterHolder[[names(trainClusters[j])]][,c(1,2,3)], testClusterHolder[[names(testClusters[i])]][,c(1,2,3)], cl = trainClusterHolder[[names(trainClusters[j])]][,5] ,k=nearPoints)
        testClusterHolder[[names(testClusters[i])]]$PredictedGender=predictedGender
      }
    }
  }

  tempMat=matrix(data = c(0,0,0,0),nrow = 2, ncol=2)

  for(i in 1: length(testClusterHolder) ){
    tempMat= tempMat+ as.matrix(table (testClusterHolder[[i]]$PredictedGender,testClusterHolder[[i]]$ActualGender ))
  }
  tempMat
  # testClusterHolder[["confusionMat"]]<<-tempMat
  # testClusterHolder

  # >  table (finalRes$`1`$PredictedGender,finalRes$`1`$ActualGender )
  #
  # Female Male
  # Female     20   14
  # Male       13   15
  # >  table (finalRes$`2`$PredictedGender,finalRes$`2`$ActualGender )
  #
  # Female Male
  # Female     80   18
  # Male        8   50

}

      ################################################
####  ####  ####  #### GMM Functions   ####  ####  ####  ####
      ################################################


#' A 3D Cluster Plot function
#'
#' This function allows you to plot the data points in clusters using GMM
#' @param trainData Dataset to be passed for clustering
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @keywords ClusterPlot
#' @export
#' @examples
#' clusterPlot3d(dataset, clusters = 3, normalized = FALSE)
clusterPlot3dGMM <- function(trainData, normalized){
  if(normalized==TRUE){
    trainData=normalizeData(trainData)
  }
  else{
    trainData=trainData[,-c(4,5)]
  }
  em <- Mclust(trainData[,c(1,2,3)], G = 1:10,
               control = emControl(tol = 1e-3),
               initialization = NULL,
               warn = mclust.options("warn"),
               verbose = interactive())
  trainDataClusters=trainData
  trainDataClusters$Clusters = em$classification
  p <- plot_ly(trainDataClusters,hovertext=c("age") ,opacity=0.7 ,
               x = ~age, y = ~height, z = ~weight ,color = ~as.factor(Clusters),
               marker = list(symbol = 'circle', size = 3))%>% layout(scene = list(xaxis = list(title = 'Age'),
                                                                                  yaxis = list(title = 'Height'),
                                                                                  zaxis = list(title = 'Weight')))
  p
}

#' A Cluster Assignment function
#'
#' This function allows you to assign the test data points to the clusters that are formed by train dataset using GMM.
#' @param trainData Dataset to be considered for clustering
#' @param normalized Takes input TRUE/FALSE to normalize the dataset. Defaults to FALSE
#' @param testData Dataset to be considered to assign its data points on clusters
#' @keywords OptimumClusters
#' @export
#' @examples
#' assignToClusterGMM(trainData = dataset1, normalized = FALSE, testData = dataset2)

assignToClusterGMM <- function(trainData ,normalized,testData){
  fullDataSet <- trainData
  if(normalized==TRUE){
    trainData=normalizeData(trainData)
  }
  else{
    trainData=trainData[,-c(4,5)]
  }
  em <- Mclust(trainData[,c(1,2,3)], G = 1:10,
               control = emControl(tol = 1e-3),
               initialization = NULL,
               warn = mclust.options("warn"),
               verbose = interactive())
  trainDataClusters=fullDataSet
  trainDataClusters$Cluster = em$classification
  holderList[["numberOfGMMClusts"]]<<-  em$G
  if(normalized==TRUE){
    testData=normalizeData(testData)
  }
  else{
    testData=testData[,-c(4,5)]
  }
  predictClasses <- predict(em, testData[,c(1,2,3)])
  save(predictClasses,file="something")
  testData$Cluster <-  predictClasses$classification
  testData=cbind(testData,as.data.frame(predictClasses$z ))
  holderList[["trainDataWithClusterGMM"]]<<-trainDataClusters
  holderList[["testDataWithClusterGMM"]]<<-testData


}

