library(RColorBrewer)
library(htmltools)
library(htmlwidgets)
library(scales)
library(lattice)
library(dplyr)
library(scales)
library(ggplot2)
library(caret)
library(e1071)
library(ranger)
library(leaflet.extras)


server <- function(input, output, session) {
  
  observeEvent(input$about, {
    shinyalert(
      title = "Data kecelakaan lalu-lintas di Jakarta",
      text = "Aplikasi ini dapat dipergunakan untuk menganalisa data kecelakaan di wilayah Jakarta berdasarkan data IRSMS. <br> Silakan gunakan pilihan pada panel di sebelah kiri untuk mengubah <b>area</b>, <b>tahun</b> dan <b>hari</b>. <br> Pada bagian bawah halaman dapat dilihat prediksi kecelakaan untuk 48 jam kedepan.",
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
  # preparing reactive datasets
  
  datamap1<-reactive({
    accidentData%>%
      filter(
      Local_Authority_.District. %in% input$district 
      & Year %in% input$year 
      & Day_of_Week %in% input$day
      #tambahan
      & Severity %in% input$Severity) 
  })
  
  datamap2<-reactive({
    accidentData%>%
      filter(
      Local_Authority_.District. %in% input$district2 
      &Year %in% input$year2 
      & Day_of_Week %in% input$day2
      #tambahan
      & Severity %in% input$Severity2) 
  })

  accidents1<-reactive({
    accidentData%>%filter(
      Local_Authority_.District. %in% input$district &
        Year %in% input$year &
        Day_of_Week %in% input$day
      #tambahan
      & Severity %in% input$Severity) 
  })
  
  accidents2<-reactive({
    accidentData%>%filter(
      Local_Authority_.District. %in% input$district2 &
        Year %in% input$year2 &
        Day_of_Week %in% input$day2
      #tambahan
      & Severity %in% input$Severity2) 
  })
  
  vehicles1<-reactive({
    accidentData%>%filter(Accident_Index %in% accidents1()$Accident_Index)
  })
  
  vehicles2<-reactive({
    accidentData%>%filter(Accident_Index %in% accidents2()$Accident_Index)
  })
  
  riskPredictionData<-reactive({ 
    
    stepsAhead<-60*60*6*c(0:7)
    timeSteps<-Sys.time()+stepsAhead
    
    dataset<-expand.grid(datetime=timeSteps, Area=c(input$district, input$district2))%>%
      mutate(Area=factor(Area, levels=districtList),
             Day_of_Week=factor(weekdays(datetime), levels=dayList),
             Month=factor(month.abb[as.numeric(format(datetime, "%m"))], levels = month.abb),
             TimeSegment=as.numeric(gsub(":.*", "", as.character(strftime(datetime, format="%H:%M"))))%/%6+1,
             datetime=NULL)
    
  })
  
  # produce predictions reactively
  
  riskPredictions<-reactive({
    
    preds <- predict(riskModel, riskPredictionData(), type="prob")
    
    
    riskPredictions<-data.frame(Area=riskPredictionData()["Area"],
                                TimeSegment=riskPredictionData()["TimeSegment"],
                                Day_of_Week=riskPredictionData()["Day_of_Week"],
                                Month=riskPredictionData()["Month"],
                                predictions=preds[,2])%>%
      mutate(xLabels=paste(Day_of_Week, hourList[TimeSegment]),
             plotOrder=rep(seq(1:(nrow(preds)/2)), 2))
    
    
  })
  
  # reactive graph labeling
  
  labels1<-reactive({
    if(length(input$year)==1)
    {
      years<-as.character(input$year)
    } else
    {
      yearListLength<-length(input$year)
      years<-paste0(input$year[1], " - ", input$year[yearListLength])
    }
    
    if(length(input$day)==1)
    {
      days<-as.character(input$day)
    } else
    {
      dayListLength<-length(input$day)
      days<-paste0(input$day[1], " - ", input$day[dayListLength])
    }
    
    c(input$district, years, days)
    
  })
  
  labels2<-reactive({
    if(length(input$year2)==1)
    {
      years<-as.character(input$year2)
    } else
    {
      yearListLength<-length(input$year2)
      years<-paste0(input$year2[1], " - ", input$year2[yearListLength])
    }
    
    if(length(input$day2)==1)
    {
      days<-as.character(input$day2)
    } else
    {
      dayListLength<-length(input$day2)
      days<-paste0(input$day2[1], " - ", input$day2[dayListLength])
    }
    
    c(input$district2, years, days)
    
  })
  
  graphLabel1 <- reactive({
    
    thisLabel<-c()
    if(labels1()[1]!=labels2()[1]) return(paste(labels1()[1], collapse = "_"))
    if(labels1()[2]!=labels2()[2]) return(paste(labels1()[2], collapse = "_"))
    if(labels1()[3]!=labels2()[3]) return(paste(labels1()[3], collapse = "_"))
    
    return(labels1()[1])
  })
  
  graphLabel2 <- reactive({
    
    if(labels2()[1]!=labels1()[1]) return(paste(labels2()[1], collapse = "_"))
    if(labels2()[2]!=labels1()[2]) return(paste(labels2()[2], collapse = "_"))
    if(labels2()[3]!=labels1()[3]) return(paste(labels2()[3], collapse = "_"))
    
    return(labels2()[1])
  })
  
  
  # outputs (valueboxes + plots)
  
  output$info1 <- renderValueBox({
    addTooltip(session, "info1", paste0("Currently comparing ", graphLabel1(), " to ", graphLabel2()),
               placement = "bottom", trigger = "hover",
               options = NULL)
    
    valueBox(
      paste(labels1()[1])
      ,paste0(labels1()[2], ", ", labels1()[3])
      ,icon = icon("map-marker",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$info2 <- renderValueBox({
    addTooltip(session, "info2", paste0("Currently comparing ", graphLabel1(), " to ", graphLabel2()),
               placement = "bottom", trigger = "hover",
               options = NULL)
    
    valueBox(
      paste(labels2()[1])
      ,paste0(labels2()[2], ", ", labels2()[3])
      ,icon = icon("map-marker",lib='glyphicon')
      ,color = "light-blue")   
  })

  output$Design <- renderText({
    paste(input$district)
  })
  
  output$Design2 <- renderText({
    paste(input$district2)
  })
  
  output$fatal1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Berat"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=2), "%")
      ,paste('Laka Fatal')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$fatal2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Berat"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=2), "%")
      ,paste('Laka Fatal')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$severe1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Sedang"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Laka LB')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$severe2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Sedang"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Laka LB')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$minor1 <- renderValueBox({
    fatalityPerc<-nrow(accidents1()%>%filter(Accident_Severity=="Ringan"))/nrow(accidents1())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Laka Ringan/ Material')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  output$minor2 <- renderValueBox({
    fatalityPerc<-nrow(accidents2()%>%filter(Accident_Severity=="Ringan"))/nrow(accidents2())*100
    valueBox(
      paste0(formatC(fatalityPerc, digits=3), "%")
      ,paste('Laka Ringan/ Material')
      ,icon = icon("info-sign",lib='glyphicon')
      ,color = "light-blue")  
  })
  
  
  output$overTime <- renderPlot({
    allAccidents<-rbind(accidents1(), accidents2())
    allAccidents$group<-c(rep(graphLabel1(), nrow(accidents1())), rep(graphLabel2(), nrow(accidents2())))
    
    allAccidents<-allAccidents%>%mutate(timeOrder=as.numeric(gsub(":.*", "", TimeSegment)))
    
    ggplot(allAccidents, aes(x=timeOrder, fill=group))+
      geom_density(alpha = 0.3)+
      scale_y_continuous(labels=percent)+
      labs(x="", y="", fill="")+
      theme_light()+
      scale_x_continuous(breaks=c(0:23))+
      theme(axis.text.x=element_text(angle=0)) +
      theme(legend.position = "bottom")
  })
  
  output$overMonth <- renderPlot({
    allAccidents<-rbind(accidents1(), accidents2())
    allAccidents$group<-c(rep(graphLabel1(), nrow(accidents1())), rep(graphLabel2(), nrow(accidents2())))
    
    allAccidents<-allAccidents%>%group_by(group)%>%mutate(totalNumOfAccidents=n())%>%
      group_by(group, Month)%>%summarise(percentage=n()/max(totalNumOfAccidents))%>%filter(!is.na(Month))
    
    ggplot(allAccidents, aes(x=reorder(as.character(Month), as.numeric(Month)), y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "dodge", alpha = 0.3)+
      scale_y_continuous(labels=percent, limits = c(0, 0.12))+
      labs(x="", y="", fill="")+
      theme_light()+
      theme(legend.position = "bottom")
  })
  
  output$byDay <- renderPlot({
    allAccidents<-rbind(accidents1(), accidents2())
    allAccidents$group<-c(rep(graphLabel1(), nrow(accidents1())), rep(graphLabel2(), nrow(accidents2())))
    
    allAccidents<-allAccidents%>%group_by(group)%>%mutate(totalNumOfAccidents=n())%>%
      group_by(group, Day_of_Week)%>%summarise(percentage=n()/max(totalNumOfAccidents))%>%
      mutate(Day_of_Week=factor(Day_of_Week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday" )))
    
    ggplot(allAccidents, aes(x=Day_of_Week, y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5)+

      scale_y_continuous(labels=percent)+
      labs(x="", y="", fill="")+
      theme_light() +
      theme(legend.position = "bottom")
  })
  
  output$byRoadType <- renderPlot({
    allAccidents<-rbind(accidents1(), accidents2())
    allAccidents$group<-c(rep(graphLabel1(), nrow(accidents1())), rep(graphLabel2(), nrow(accidents2())))
    
    allAccidents<-allAccidents%>%group_by(group)%>%mutate(totalNumOfAccidents=n())%>%
      filter(!is.na(RoadType)) %>% 
      group_by(group, RoadType)%>%summarise(percentage=n()/max(totalNumOfAccidents))%>%

      mutate(RoadType=factor(RoadType)) 

    
    ggplot(allAccidents, aes(x=RoadType, y=percentage, fill=group))+
      geom_bar(stat = "identity", position = "dodge", alpha = 0.5)+
      scale_y_continuous(labels=percent, limits = c(0, 0.2))+
      labs(x="", y="", fill="")+
      theme_light()+
      coord_flip()+
      theme(legend.position = "bottom")
  })
  
  output$riskEstimation <- renderPlot({
    
    ggplot(riskPredictions(), aes(x=reorder(xLabels, plotOrder) , y=predictions, colour=Area))+
      geom_line(aes(group=Area), size=1)+
      labs(x="", y="Risk", fill="")+
      scale_y_continuous(labels=scales::percent, limits = c(0, 1))+
      theme_light()+
      theme(axis.text.x=element_text(angle=0))+
      theme(legend.position = "bottom")
    
  })
 
  output$map1 <- leaflet::renderLeaflet({
    datamap1() %>% 
    leaflet() %>%
      addCircles() %>% 
      addTiles() %>% 
      setView( 106.81 , -6.20, zoom= 10 ) %>%
    addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                            autoCenter = TRUE, maxZoom = 10, 
                                            setView = TRUE)) %>%
    activateGPS()
  })
  
  output$map2 <- leaflet::renderLeaflet({
    datamap2() %>% 
    leaflet() %>%
      addCircles() %>% 
      addTiles() %>% 
      setView( 106.81 , -6.20, zoom= 10 ) %>%
    addControlGPS(options = gpsOptions(position = "topleft", activate = TRUE, 
                                       autoCenter = TRUE, maxZoom = 10, 
                                       setView = TRUE)) %>%
      activateGPS()
 
  })

}


