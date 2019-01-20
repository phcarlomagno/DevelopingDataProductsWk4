#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Author Carlomagno Anastacio
library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  #create the dataset, just random for demo on shiny
  daysRep <- 100
  rndMinsRep <- daysRep * 5
  set.seed(20190115)
  mainData <- data.frame(rep(c("Monday","Tuesday","Wednesday","Thursday","Friday"),times=daysRep),  
                         data.frame((runif(rndMinsRep,min=40,max=60))),
                         data.frame((runif(rndMinsRep,min=30,max=50))))
  colnames(mainData) <- c("Day","TravelMinBus","TravelMinCar")
  
  #create a linear model for both modes of transport, with the day as argument
  modelFitBus <- lm(TravelMinBus ~ Day, data=mainData)
  modelFitCar <- lm(TravelMinCar ~ Day, data=mainData)
  
  
  #predict
  predValBus <- reactive({predict(modelFitBus,
                                  newdata = data.frame(Day=input$dayID),
                                  interval = "prediction")  #predict based on day
  })
  predValCar <- reactive({predict(modelFitCar,
                                  newdata = data.frame(Day=input$dayID),
                                  interval = "prediction")  #predict based on day
  })
  
  output$strArrivalTime <- renderUI({
    if(input$commuteType=="Bus") 
      finalTimeFrame <- as.data.frame(predValBus())
    else
      finalTimeFrame <- as.data.frame(predValCar())
    
    dateTime <- as.POSIXct(sprintf("%04d",600), format="%H%M")  #as time variable
    dateTime <- dateTime + (60*as.integer(finalTimeFrame[1,1]))
    outLine1 <- "Likely to arrive around: "
    outLine2 <- format(dateTime, format="%H:%M")
    outLine3 <- " AM</b></font> on a "
    outLine4 <- "when travelling by "
    HTML(paste(outLine1,"<b><font color='blue' size=9>",outLine2,outLine3,"<u>",
               input$dayID,"</u>",outLine4,"<i>",tolower(input$commuteType),"</i>."))
    

  })

  output$sourceData <- renderTable({
    mainData
  })
  
  output$strFinalTime <- renderTable({
    #display the fitted, lower, and upper values of either bus or car based on radio
    if(input$commuteType=="Bus")
      outData<-as.data.frame(predValBus())
    else
      outData<-as.data.frame(predValCar())
    
    
    colnames(outData) <- c("Fitted","Lower","Upper")
    outData <- outData[c("Lower","Fitted","Upper")]
    outData[] <- lapply(outData,as.integer)  #change all numeric values to integers
    outData

  })
  
  output$historicalPlot <- renderPlot({
    #sort the days properly
    mainData$Day=factor(mainData$Day , levels=levels(mainData$Day)[c(2,4,5,3,1)])
    
    #plot boxplot
    #plot(mainData$Day, mainData$TravelMinBus,main="Boxplot")
    
    #plot boxplot trendline
    #abline(modelFitBus, col = "red", lwd = 2)
    
    
    
    #filter only the day selected & plot histogram
    dayFiltered <- mainData[which(mainData$Day==input$dayID),]
    
    if(input$commuteType=="Bus")
      hist(dayFiltered$TravelMinBus,
           main=paste0(input$dayID," Historical Data Using a ",input$commuteType),
           xlab="Travel Time (minutes)",xlim = c(30, 60), ylim = c(0, 20))
    else
      hist(dayFiltered$TravelMinCar,
           main=paste0(input$dayID," Historical Data Using a ",input$commuteType),
           xlab="Travel Time (minutes)",xlim = c(30, 60), ylim = c(0, 20))
  })
  
  output$strTitle <- renderUI({
    title1 <- "<font color='blue'><center>What Time Will I Arrive If I Leave at 6AM?</center></font>"
    HTML(title1)
  })
  
  output$strPrediction <- renderText({
    ("Prediction")
  })
  
  output$documentation <- renderUI({
    docLine1 <- "<b><font size='4'>Overview</font></b><br>
                This app predicts the possible arrival time depending on the 
                day of the week and mode of transportation, when departing at 
                06:00 AM.  It makes a prediction using a linear model (<b>lm()</b>), 
                with the <font color='green'>time</font> as outcome based on the 
                <font color='red'>day</font> and 
                <font color='red'>mode of transport</font> input<br>."
    docLine2 <- "The app layout is separated into 2: 
                <ol type='i'><li>Controller (left side)</li>
                    <li>View (right side)</li></ol>"
    docLine3 <- "<b><font size='4'>I. Controller</font></b><br> 
                <table border='1' width='100%'><tr>
                <th>Control Name</th><th>Type</th><th>Description</th><tr>
                <tr><td>Choose Day</td><td>Dropdown</td>
                    <td>Lets the user select the day to predict</td></tr>
                <tr><td>Choose Mode of Transport</td>
                    <td>Radio Button</td><td>Select from either: bus or car</td></tr>
                <tr><td>Get the time!</td><td>Submit Button</td>
                    <td>Triggers the computation/prediction to start</td></tr>
                </table>"
    docLine4 <- "<b><font size='4'>II. View</font></b><br>
                This section contains all information visible to the user. It is 
                separated into the tabs:<br>
                <ol><li>Prediction - displays the result when the [Get the time!] 
                    button is pressed (default view is Monday by Bus)</li>
                <li>Documentation - this screen</li>
                <li>Data - contains the data set used to build the prediction 
                    model</li>
                <li>Resources - contains links to the pitch,git repository, 
                    and R Markdown</li>
                </ol>"
    docLine5 <- "<b><font size='2'>Prediction Sub-section</font></b><br>
                This part of the app shows the following, top to bottom: 
                <font color='blue'>histogram</font> of the data used showing the 
                frequency of elapsed minutes, the <font color='blue'>prediction 
                results</font>(fit, lower, upper) using lm(), and the 
                <font color='blue'>predicted arrival time</font> based on the 
                predicted fit value and the input values from the controller."
    
    HTML(paste(docLine1,docLine2,docLine3,docLine4,docLine5,sep="<br>"))
  })
  
  output$resources <- renderUI({
      urlPitch <- "<a href='http://rpubs.com/phcarlomagno/DDPWk4-Shiny'>The Pitch</a>"
      urlGit <- "<a href='https://github.com/phcarlomagno/DevelopingDataProductsWk4'>Github Repository</a>"

      author <- "<br><br><br>Created by:<br>Carlomagno Anastacio<br>16 January 2019"
      HTML(paste(urlPitch,urlGit,author,sep="<br>"))
  })
  
  output$usage <- renderUI({
    usageText <- "<br> <b><font size='3' color='red'>Usage</font></b><br>
                <ol><li>Select a day from <b>Choose day</b></li>
    <li>Select a mode of transport from <b>Choose Mode of 
    Transport</b></li>
    <li>Click on the <b>Get the time!</b> button</li>
    <li>View the results in the <b>Prediction</b> tab</li>
    </ol>"
    HTML(usageText)
  })
  
})

