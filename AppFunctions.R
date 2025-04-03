#Function to get ECan river flow data
#Ideally the Environmental data API would be used, but this currently (as of April 2025) doesn't provide stage data
#and the required site is stage only.
#So in the interim, use the URL provided on the data download page e.g, https://data.ecan.govt.nz/Catalogue/Method?MethodId=79&SiteNo=64608#tab-data

#' Very Brief description 
#' 
#' [GetHopeRiverAtGlynnWyeStage()] gets the last 2 days of stage data for the Hope River at Glynn Wye. 
#' 
#' This function downloads data from ECAN, see https://data.ecan.govt.nz/Catalogue/Method?MethodId=79&SiteNo=64608#tab-data
#' 
#' @param Period A string giving the length of data to return default is "2_Days"
#'  other choices are "1_Week","2_Weeks","1_Month"
#' @author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#' @seealso
#' @return Describe type of object that is returned
#' @keywords Keywords 

GetHopeRiverAtGlynnWyeStage <- function(Period="2_Days")
{
  
  #Load any required libraries
  list.of.packages <- c("jsonlite")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')
  
  librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
  if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)

  BaseURL <- "https://data.ecan.govt.nz:443/data/79/Water/River%20stage%20flow%20data%20for%20individual%20site/JSON?SiteNo=64608&Period=PeriodString&StageFlow=River%20Stage"
  CompleteURL <- sub("PeriodString",Period,BaseURL)
  NewData <- jsonlite::fromJSON(url(CompleteURL))
  NewData$data$item$Value <- as.numeric(NewData$data$item$Value)
  
  return(NewData$data$item)
}



#' HopeRiverStageGraph() plots observed river stage for the Hope River
#' @author Tim Kerr, \email{T.Kerr@Aqualinc.co.nz}
#' @param StageData The dataframe that holds the observed river level data to be plotted.
#' @export

HopeRiverStageGraph <- function(StageData = HopeRiverStageData)
{
  
  #Load any required libraries
  list.of.packages <- c("xts","plotly")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')
  
  librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
  if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)
  
  ObservedData <- xts(StageData$Value, order.by= as.POSIXct(StageData$DateTime, format="%Y-%m-%dT%H:%M:%S"))

  LatestDate <- max(index(ObservedData))
  EarliestDate <- min(index(ObservedData))
  
  ds <- data.frame(DateTime = index(ObservedData), Stage = ObservedData[,1])
  
  #Calculate the initial view x an y limits, which is taken to be 80 days into the future and 90 days into the past
  XAxisInitialRange <- c(EarliestDate,LatestDate)

  YAxisInitialRange <- c(0,max(1,max(ObservedData)))
  
  t <- list(
    family = "sans serif",
    size = 14,
    color = toRGB("grey50"))
  
  p <- plot_ly(ds, x = ~DateTime) %>%

    add_trace(y=20,type='scatter',name="Danger",fill='tozeroy',fillcolor = 'rgba(219, 67, 37, 1)',mode='none',hoverinfo='none') %>%
    add_trace(y=0.8,type='scatter',name="Caution",fill='tozeroy',fillcolor = 'rgba(237, 162, 71, 1)',mode='none',hoverinfo='none') %>%
    add_trace(y= 0.5,type='scatter', name = "OK", fill='tozeroy',fillcolor = 'rgba(87, 196, 173, 1)',mode="none",hoverinfo='none') %>%
    add_lines(y = ~Stage, name = "River depth",line = list(color = "black",width = 2),hovertemplate="%{y:.1f} m at %{x|%H:%m %p on %b %d}") %>%
    add_annotations(text = "Cross with care",textfont= t, showarrow=FALSE,xanchor='right',yanchor='top', xref="paper", x=1,y=0.5) %>%
    add_annotations(text = "Cross with caution",textfont= t, showarrow=FALSE,xanchor='right',yanchor='bottom', xref="paper", x=1,y=0.5) %>%
    add_annotations(text = "Do not cross",textfont= t, showarrow=FALSE,xanchor='right',yanchor='bottom', xref="paper", x=1,y=0.8) %>%
    layout(
      showlegend=FALSE,
      margin = list(t=50),
      title = "",
      
      xaxis = list(title="",range = XAxisInitialRange,
                   rangeselector = list(
                     buttons = list(
                       list(
                         count = 1,
                         label = "24 Hour",
                         step = "day",
                         stepmode = "backward"),
                       list(
                         count = 7,
                         label = "Last week",
                         step = "day",
                         stepmode = "backward"),
                       list(
                         count = 14,
                         label = "Last fortnight",
                         step = "day",
                         stepmode = "backward"))),
                   
                   rangeslider = list(type = "date")),
      
      yaxis = list(title = "River Depth (m)",
                   fixedrange=FALSE, range = YAxisInitialRange),#make the limits based on the original view
      annotations = list(text = "<a href = 'https://www.ecan.govt.nz/data/riverflow//sitedetails/64608'>ECan data source</a>", x= 1, y=-0.5, xref="paper",yref="paper", showarrow=FALSE)) 
  
  return(p)
}


#' Determine crossing danger level 
#' 
#' [CrossingDanger()] determines from the latest stream flow levels whether the Hope
#' River is OK to cross at the Doubtful Confluence carpark. 
#' 
#' The Hope river needs to be crossed to access the Doubtful River. This crossing
#' can be dangerous if the river is high.
#' 
#' @param PARAMNAME1 Description of what PARAMNAME1 is for and what it's default is
#' @param PARAMNAME1 Description of each of the parameters
#' @author Tim Kerr, \email{Tim.Kerr@@Rainfall.NZ}
#' @seealso
#' @return Describe type of object that is returned
#' @keywords Keywords 

CrossingDanger <- function(StageData = HopeRiverStageData,
                          DangerThreshold = 0.8,
                          OKThreshold = 0.5)
{
  
  #Load any required libraries
  list.of.packages <- c("xts","tidyr")
  #new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  #if(length(new.packages)) install.packages(new.packages,repos='https://cloud.r-project.org')
  
  librariesToLoad <- list.of.packages[!(list.of.packages %in% (.packages()))]
  if(length(librariesToLoad)) sapply(librariesToLoad, library, character.only = TRUE)
  
  #Get the latest 30 mins of stage height, average it, and check it against thresholds 
  ObservedData <- xts(StageData$Value, order.by= as.POSIXct(StageData$DateTime, format="%Y-%m-%dT%H:%M:%S"))
  LatestLevel <- window(ObservedData,start = max(index(ObservedData))- 30*60) %>% mean()
  
  #See whether the latest level is above the danger threshold, or below the OK threshold, or in between
  if(LatestLevel > DangerThreshold) CrossingDangerStatus = "Danger" else
    if(LatestLevel < OKThreshold) CrossingDangerStatus = "OK" else
      CrossingDangerStatus = "Caution"
  
  return(CrossingDangerStatus)
}
