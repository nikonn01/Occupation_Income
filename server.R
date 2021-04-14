#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$SummaryA1 <- renderPrint({
        str(master_data)
    })
    output$SummaryA2 <- renderPrint({
        summary(master_data)
    })
    
    output$SummaryI1 <- renderPrint({
      str(WIP_Income)
    })
    output$SummaryI2 <- renderPrint({
      summary(WIP_Income)
    })
    
    output$SummaryO1 <- renderPrint({
      str(WIP_Occupation)
    })
    output$SummaryO2 <- renderPrint({
      summary(WIP_Occupation)
    })
    #output$RawData <- DT::renderDataTable({
     #   DT::datatable(data = as.data.frame(dat))
    #})
    # https://datatables.net/reference/option/dom
    
    # https://datatables.net/extensions/index
    ext <- list(Responsive = TRUE)
    
    dat_Occupation1 <- reactive({
      dat <- WIP_Occupation  
      if(input$occ1=="TRUE"){
        dat$Sex[dat$Sex==11] <- 1
        dat$Sex[dat$Sex==12] <- 2
        dat
      }
      else {dat<-dat 
      dat}
      
      if(input$occ2=="TRUE"){
        unique_occupation1<-unique(dat$Occupation) #define variable for unique occupation
        Occcupation_exceptions1<-dat$Occupation[c(1:2)] #define variable for occupation exceptions
        dat$Occupation[dat$Occupation == Occcupation_exceptions1] <- NA #replace occupation with NA if it is in the exception list
        unique_occupation1 <- unique_occupation[c(-1,-2)] #remove exceptions from occupation table
        dat
      }
      else {dat<-dat 
      dat}
      
      
    })
    
    dat_Income1 <- reactive({
      dat <- WIP_Income  
      if(input$inc1=="TRUE"){
        dat$Income <- ifelse(dat$Income==2000000 & dat$Transaction_Date==as.Date('2015-01-01'), NA, dat$Income)
        dat
      }
      else {dat<-dat 
      dat}
      
      if(input$inc2=="TRUE"){
        dat$Income <- ifelse(dat$Income>2500000 & dat$Transaction_Date==as.Date('2017-01-01'), NA, dat$Income)
        dat
      }
      else {dat<-dat 
      dat}
      
      if(input$inc3=="TRUE"){
        dat$Rounded_Incomes<-round(dat$Income, -4)
        dat
      }
      else {dat<-dat 
      dat}
    }) 
    
    output$RawDataO <- DT::renderDataTable({
      
      DT::datatable(data = dat_Occupation1(),
                    rownames = input$rownames,
                    selection = input$selection,
                    filter = list(position = input$filter),
                    options = list(searching = TRUE,
                                   pageLength = 10,
                                   lengthMenu = c(5, 10, 100),
                                   dom = paste(input$dom, collapse = ""),
                                   ordering = input$order
                    )
                    #,extensions = ext
      )  %>%
        formatStyle(columns = c("ID"), backgroundColor = "lightblue")  
      
    })
    output$SelRows <- renderTable({
      req(input$RawData_rows_selected)
      print(mt[input$RawData_rows_selected,"ID"])
    })
    
    output$RawDataI <- DT::renderDataTable({
      DT::datatable(data = dat_Income1(),
                    rownames = input$rownames1,
                    selection = input$selection1,
                    filter = list(position = input$filter1),
                    options = list(searching = TRUE,
                                   pageLength = 10,
                                   lengthMenu = c(5, 10, 100),
                                   dom = paste(input$dom1, collapse = ""),
                                   ordering = input$order1
                    )
                    #,extensions = ext
      )  %>%
        formatStyle(columns = c("ID"), backgroundColor = "lightblue")  
      
    })
    output$SelRows <- renderTable({
      req(input$RawData_rows_selected)
      print(mt[input$RawData_rows_selected,"ID"])
    })
    
    output$RawData <- DT::renderDataTable({
        DT::datatable(data = master_data,
                      rownames = input$rownames2,
                      selection = input$selection2,
                      filter = list(position = input$filter2),
                      options = list(searching = TRUE,
                                     pageLength = 10,
                                     lengthMenu = c(5, 10, 100),
                                     dom = paste(input$dom2, collapse = ""),
                                     ordering = input$order2
                      )
                      #,extensions = ext
        )  %>%
            formatStyle(columns = c("ID"), backgroundColor = "lightblue")  
           
    })
    output$SelRows <- renderTable({
        req(input$RawData_rows_selected)
        print(mt[input$RawData_rows_selected,"ID"])
    })
    
    output$RawDataE <- DT::renderDataTable({
      
      DT::datatable(data = Occupation_Income_Exception,
                    rownames = input$rownames3,
                    selection = input$selection,
                    filter = list(position = input$filter3),
                    options = list(searching = TRUE,
                                   pageLength = 10,
                                   lengthMenu = c(5, 10, 100),
                                   dom = paste(input$dom3, collapse = ""),
                                   ordering = input$order3
                    )
                    #,extensions = ext
      )  %>%
        formatStyle(columns = c("ID"), backgroundColor = "lightblue")  
      
    })
    output$SelRows <- renderTable({
      req(input$RawData_rows_selected)
      print(mt[input$RawData_rows_selected,"ID"])
    })
    
  
    output$Corrgram2 <- renderPlot({
        master_cor <- master_data[,input$VariablesD]
        corrgram( master_cor, 
                 order = input$Group2, 
                 abs = input$abs2, 
                 cor.method = input$CorrMeth2,
                 text.panel = panel.txt,
                 main = "Correlation of data")
    })
    
    
    output$Missing <- renderPlot({
        master_missing <- master_data[,input$VariablesE]  
        vis_miss(master_missing, cluster = input$cluster) +
            labs(title = "Missingness of data")
        
        
    })
    output$Missing1 <- renderPlot({
      master_missing <- master_data[,input$VariablesE]  
      
      naniar::gg_miss_upset(data = master_missing, nsets = 6)  # parameter nsets has a default of 5 
    })
    
    output$Boxplot <- renderPlot({
        master_boxplot <- master_quant_cal[,input$VariablesF]  
        master_boxplot <- scale(master_boxplot, center = input$standardise, scale = input$standardise)
        boxplot(x = master_boxplot, use.cols = TRUE, notch = FALSE, varwidth = FALSE,  
                horizontal = FALSE, outline = input$outliers, 
                col = brewer.pal(n = ncol(master_boxplot), name = "RdBu"),
                range = input$range, main = "Boxplots of data")
    })
    
    output$RisingChart <- renderPlot({
      master_RisingChart <- master_quant_cal[,input$VariablesG]  
      master_RisingChart <- scale(x = master_RisingChart, center = TRUE, scale = FALSE)
      mypalette <- rainbow(ncol(master_RisingChart))
      matplot(x = seq(1, 100, length.out = nrow(master_RisingChart)), y = master_RisingChart, type = "l", xlab = "Percentile (%)", ylab = "Standardised Values", lty = 1, lwd = 1, col = mypalette, main = "Rising value chart")
      legend(legend = colnames(master_RisingChart), x = "topright", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(master_RisingChart)^0.3))
    })
    
    output$Distrib <- renderPlot({
      master_quant_cal_dist<-master_quant_cal%>%na.omit()
      DistribData<-master_quant_cal_dist[,input$VariablesH]
      DistribData<- DistribData%>%na.omit()
      ggplot2::ggplot(data = master_quant_cal_dist, mapping = aes(x = DistribData)) + geom_histogram(bins = 50) +
        labs(title = "Histogram of a variable")
     
    })
    
    output$Matpl <- renderPlot({
    master_Matplt <- master_quant_cal[,input$VariablesK]
    numData <- scale(master_Matplt, center = input$center, scale = input$scale) 
    matplot(numData, type = "l", col = rainbow(ncol(numData)), xlab = "Observations in sequence", ylab = "Value") 
    
    })
    })
