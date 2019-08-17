list.of.packages <- c("shiny","shinydashboard","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(skin="red",

  dashboardHeader(),

  dashboardSidebar(
      fileInput("file1", "Upload Train Dataset",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      ,
      fileInput("file2", "Upload Test Dataset",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      sidebarMenu( id="clusterAnalysisSideBar",
                   menuItem("3d Scatter Plot", tabName="3dScatter"),
                   menuItem("Kmeans",
                            menuSubItem("Clusters", tabName = "kmClusters"),
                            menuSubItem("Optimum Clusters", tabName = "kmOptimumClusters"),
                            menuSubItem("Classification", tabName = "kmClassification"),
                            tabName="kmeans"),


                   menuItem("GMM",
                            menuSubItem("Clusters",tabName = "gmmClusters"),
                            menuSubItem("Cluster With Probabilities", tabName = "clusterWithProbabilitiesGMM"),
                            menuSubItem("Classification",tabName = "gmmClassification"),
                            tabName="gmm"),

                   menuItem("Conclusion",tabName="Conclusion")
      )
  ),

  dashboardBody(
    tabItems(
                #3d Scatter Plot Tab#
                tabItem("3dScatter",box(width = "400",
                  title = "3d Scatter Plot", status = "danger", solidHeader = TRUE,
                  collapsible = TRUE,collapsed = FALSE,
                  plotlyOutput("scatter3DOutput", height = 610)
                )),

                #KMeans Plot Tab#
                tabItem("kmClusters",fluidRow(
                  fluidRow( style ="margin:20px",
                    box(width = "400",title = "Input Parameters", status = "danger", solidHeader = TRUE,
                      fluidRow(
                    column(5,radioButtons("normalization",selected = "No",label = "Normalize Data",choices = c("Yes","No"))),
                    column(7,sliderInput("numOfClusters",label = "Number of Clusters",min = 1,max = 20,value = 2))

                   ))),
                  fluidRow(style ="margin:20px",box(width = "400",
                               title = "KMeans Cluster Plot", status = "danger", solidHeader = TRUE,
                               collapsible = TRUE,collapsed = FALSE,
                               plotlyOutput("cluster3dOutput", height = 610))
                  ))),

                  #Optimum Clusters Tab#
                  tabItem("kmOptimumClusters",
                    fluidRow(
                         column(5, wellPanel(box(width = "400",title = "Optimum Clusters", status = "danger", solidHeader = TRUE, plotOutput("optimumClusterDynamicsKmeans")))),
                         column(7, wellPanel( box(width = "400",title = "Cluster 3d Plot", status = "danger", solidHeader = TRUE,
                                        collapsible = TRUE,collapsed = FALSE, plotlyOutput("optimumClusterGraphKMeans")))
                            ))
                    ),


                #Test Dataset clusters and classifictaion#
                tabItem("kmClassification",
                        tabsetPanel(
                          tabPanel("Test Data Clusters",
                                   fluidRow(wellPanel( box(width = "400",title = "", status = "danger", solidHeader = TRUE, plotlyOutput("testDataClusters")) ))),
                          tabPanel("Test Data Classification",
                                   fluidRow(wellPanel( box(width = "400",title = "Confusion Matrix", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassification")) )),
                                   fluidRow(wellPanel( box(width = "400",title = "Accuracy", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationAcc")) )),
                                   fluidRow(wellPanel( box(width = "400",title = "False Positive", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationFP")) )),
                                   fluidRow(wellPanel( box(width = "400",title = "False Negative", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationFN")) ))
                                   )
                          )
                ),



                #GMM Plot Tab#
                tabItem("gmmClusters",fluidRow(
                  fluidRow( style ="margin:20px",
                            box(width = "400",title = "Input Parameters", status = "danger", solidHeader = TRUE,
                                fluidRow(
                                  column(12,radioButtons("normalizationGMM",selected = "No",label = "Normalize Data",choices = c("Yes","No")))
                                ))),
                  fluidRow(style ="margin:20px",box(width = "400",
                                                    title = "GMM Cluster Plot With Optimum Parameters", status = "danger", solidHeader = TRUE,
                                                    collapsible = TRUE,collapsed = FALSE,
                                                    plotlyOutput("cluster3dOutpuGMM", height = 610))
                  ))),



                #Cluster Probability GMM#
                tabItem("clusterWithProbabilitiesGMM",
                        fluidRow(
                          column(12, wellPanel(box(width = "400",title = "Clustering with probabilities", status = "danger", solidHeader = TRUE, dataTableOutput("clusterWithProbabilitiesGMMDT")))
                         ))
                ),



                #Test Dataset clusters and classifictaion#
                tabItem("gmmClassification",
                          fluidRow(style="margin:20px",
                                   fluidRow(wellPanel( box(width = "400",title = "Confusion Matrix", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationGMM")) )),
                                   fluidRow(wellPanel( box(width = "400",title = "Accuracy", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationGMMAcc")) )),
                                   fluidRow(wellPanel( box(width = "400",title = "False Positive", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationGMMFP")) )),
                                   fluidRow(wellPanel( box(width = "400",title = "False Negative", status = "danger",
                                                           solidHeader = TRUE, verbatimTextOutput("testDataClassificationGMMFN")) ))
                          )

                ),



                tabItem("Conclusion",
                        fluidRow(
                          column(12, wellPanel(box(width = "400",title = "Conclusion", status = "danger", solidHeader = TRUE, htmlOutput("conclusionsOutput")))
                          ))
                )











                )

    )
  )

