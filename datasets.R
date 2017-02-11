library(shiny)

productList <- function(dataset) {
  c("All",sort(unique(unlist(strsplit(dataset$Products,",")))))
}

datasetStatic <- read.csv("data/testData.csv",stringsAsFactors = FALSE)

values <- reactiveValues(
  dataset = datasetStatic,
  products = productList(datasetStatic))
  

saveDataSet <- function() {
  write.csv(values$dataset,"data/testData.csv",row.names=FALSE)
}