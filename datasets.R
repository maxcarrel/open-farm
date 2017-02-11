library(shiny)

productList <- function(dataset) {
  c("All",sort(unique(unlist(strsplit(dataset$Products,",")))))
}

datasetStatic <- read.csv("data/OpenFarmDataset.csv",stringsAsFactors = FALSE,encoding="utf-8")

datasetStatic$ProductsTruncated <- paste0(strtrim(datasetStatic$Products,50),"...")

values <- reactiveValues(
  dataset = list(datasetStatic),
  products = list(productList(datasetStatic)))
  

saveDataSet <- function() {
  write.csv(values$dataset[[1]],"data/OpenFarmDataset.csv",row.names=FALSE)
}