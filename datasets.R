library(shiny)

productList <- function(dataset) {
  c("All",sort(unique(unlist(strsplit(dataset$Products,",")))))
}

datasetStatic <- read.csv("data/testData.csv",stringsAsFactors = FALSE)

values <- reactiveValues(
  dataset = list(datasetStatic),
  products = list(productList(datasetStatic)))
  

saveDataSet <- function() {
  write.csv(values$dataset[[1]],"data/testData.csv",row.names=FALSE)
}