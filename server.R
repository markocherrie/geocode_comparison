# geocoding shiny
require(RCurl)
require(RJSONIO)
require(plyr)
require(leaflet)
library(shiny)



## functions
Google <- function(str,verbose=FALSE) {
  construct.geocode.url <- function(str, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", str, "&sensor=", sensor, sep = "")
    return(URLencode(u))
  }
  if(verbose==FALSE) cat(str,"\n")
  u <- construct.geocode.url(str)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    data<-(c(lat, lng))
  } else {
    data<-(c(NA,NA,NA, NA))
  }
  Sys.sleep(5)
  data[3]<-"Google"
  return(data)
} 
BING <- function(str){
  require(RCurl)
  require(RJSONIO)
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=Apo4HssxpmYvVbDEUA464pmX5Y30xsQNlJ4pES6Z6D056puS63D90MLZlQ1yVeTG"))
  d <- getURL(u)
  j <- fromJSON(d,simplify = FALSE) 
  if (j$resourceSets[[1]]$estimatedTotal > 0) {
    lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
    lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  }
  else {    
    lat <- lng <- NA
  }
  data<-c(lat,lng)
  data[3]<-"BING"
  Sys.sleep(5)
  return(data)
}  
Mapquest <- function(str){
  require(RCurl)
  require(RJSONIO)
  url <- paste0("http://www.mapquestapi.com/geocoding/v1/address?key=aGO27r4vYdEZP9yZzSxz2Vw4Akhbyb6i&location=", str, "&outFormat=json")
  raw_json <- scan(url, "", sep="\n")
  j <- fromJSON(raw_json) 
  if (j$info$statuscode == 0) {
    coord <- j$results[[1]]$locations[[1]]$latLng
    latlong<-t(data.frame(coord))
  }
  else {    
    latlong <- NA
  }
  data<-c(latlong)
  data[3]<-"Mapquest"
  Sys.sleep(5)
  return(data)
}  
Nomatim <- function(str){
  require(RCurl)
  require(RJSONIO)
  str<-gsub(" ", "+", str)
  url <- paste0("http://nominatim.openstreetmap.org/search?q=", str,"&addressdetails=1&format=json&limit=1")
  raw_json <- scan(url, "", sep="\n")
  if (nchar(raw_json)<3){
    data<-NA}
  else{
    j <- fromJSON(raw_json) 
    if (j[[1]]$lat>0) {
      lat <- j[[1]]$lat
      lng <- j[[1]]$lon
    }
    else {    
      lat <- lng <- NA
    }
  }
  data<-c(lat,lng)
  data[3]<-"Nomatim"
  Sys.sleep(5)
  return(data)
}  


#############################################
shinyServer(server <- function(input, output) {

  ############ set up
observeEvent(input$goButton, {
      str <- as.character(input$str)
    G<-Google(str) 
    B<-BING(str)
    M<-Mapquest(str)
    N<-Nomatim(str)
    map<-rbind(G, B, M, N)
    map[is.na(map)] = 99 
    colnames(map)<-c("lat", "long", "provider")
    map<-as.data.frame(map)
    map$lat<-as.numeric(as.character(map$lat))
    map$long<-as.numeric(as.character(map$long))
 
output$map <- renderLeaflet({
      mapleaflet <- sp::SpatialPointsDataFrame(
        cbind(
          map$long,  # lng
          map$lat # lat
        ),
        data.frame(type = factor(
          map$provider)
        )
      )
      pal <- colorFactor(c("chartreuse1", "blueviolet", "darkgoldenrod1", "firebrick1"), domain = c("Google", "BING", "Mapquest", "Nomatim"))
      leaflet(mapleaflet) %>% addProviderTiles("CartoDB.Positron") %>%
        addCircleMarkers(
          color = ~pal(type),
          stroke = FALSE, fillOpacity = 0.75,radius=10) %>%
        addLegend("bottomright", pal = pal, values=~type,
                  title = "Geocoding Provider",
                  opacity = 1
        )
  })
  })
})