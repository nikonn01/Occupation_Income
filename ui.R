#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

  #remove ID 

library(shiny)

domChoices <- c("l","f","r","t","i","p")


# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        useShinyjs(),
        titlePanel("Stats NZ Interview Presentation Nadia Nikoniuk"),
        h6("Full code can be downloaded "),
        a("https://github.com/nikonn01/Occupation_Income"),
        tabsetPanel(
            tabPanel("Data Overview", icon = icon("table"),
                    
                     tabsetPanel(
                       tabPanel("Tasks Overview",icon = icon("tasks"),
                                h4("Loading data for report"),
                                code("dat_Income <- read.delim('Income.txt', header = TRUE)", style = "color:blue"),
                                br(),
                                code("dat_Occupation<-read.delim('Occupations.txt', header = TRUE)", style = "color:blue"),
                                br(),
                                h4("Clean'Occupations.txt'"),
                                strong("-The sex column contains '11' for males and '12' for females. Change this to '1' for males and '2' for females."),
                                br(),
                                code("dat_Occupation$Sex[dat_Occupation$Sex==11] <- 1", style = "color:blue"),
                                br(),
                                code("dat_Occupation$Sex[dat_Occupation$Sex==12] <- 2", style = "color:blue"),
                                br(),
                                strong("-Remove the invalid occupations and replace with a null value."),
                                br(),
                                p("I have created a variable to record unique occupations which I finalised manually and discovered two invalid values. I decided to keep valid and invalid occupations in two different variables as it will help to classify qualifications for validity in the future for a big dataset."),
                                br(),
                                code("unique_occupation<-unique(dat_Occupation$Occupation) #define variable for unique occupation", style = "color:blue"),
                                br(),
                                code("Occcupation_exceptions<-dat_Occupation$Occupation[c(1:2)] #define variable for exceptions", style = "color:blue"),
                                br(),
                                code("dat_Occupation$Occupation[dat_Occupation$Occupation == Occcupation_exceptions] <- NA #replace occupation with NA if it is in the exception list",style = "color:blue"),
                                br(),
                                code("unique_occupation <- unique_occupation[c(-1,-2)] #remove exceptions from unique occupations", style = "color:blue"),
                                br(),
                                h4("Clean'Income.txt'"),
                                strong("-For Date of Transaction equal to 01/01/2015 remove any income values equal to 2,000,000."),
                                br(),
                                code("dat_Income$Income <- ifelse(dat_Income$Income==2000000 & dat_Income$Transaction_Date==as.Date('2015-01-01'), NA, dat_Income$Income)", style = "color:blue"),
                                br(),
                                strong("-For Date of Transaction equal to 1/1/2017 remove any income values over 2,500,000."),
                                br(),
                                code("dat_Income$Income <- ifelse(dat_Income$Income>2500000 & dat_Income$Transaction_Date==as.Date('2017-01-01'), NA, dat_Income$Income)", style = "color:blue"),
                                br(),
                                strong("-Create a new column called 'Rounded Incomes' and populate it with income values rounded to the nearest 10,000."),
                                br(),
                                code("dat_Income$Rounded_Incomes<-round(dat_Income$Income, -4)", style = "color:blue"),
                                br(),
                                h4("Create a new table that joins the two tables supplied"),
                                strong("Create a new table that joins 'Occupations' and 'Income'"),
                                br(),
                                p("Some observations are duplicated in Occupation table with only one difference in Birth_Date column. I have used full join to get all possible variations. As two datasets 'Income' and 'Occupation' have Birth_Date column, I have decided to leave the observations where the Birth_Date is the same for both tables. Observations with different Birth_Date have been removed from the working dataset and moved to exception dataset."),
                                br(),
                                p("Overwrite the gender column in Occupation table"),
                                br(),
                                code("dat_Income$Sex[dat_Income$Sex==11] <- 1", style = "color:blue"),
                                br(),
                                code("dat_Income$Sex[dat_Income$Sex==12] <- 2", style = "color:blue"),
                                br(),
                                p("Create a new table that joins 'Occupations' and 'Income'"),
                                br(),
                                code("Occupation_Income<-merge(x = dat_Income, y = dat_Occupation, by = 'ID', all = TRUE)", style = "color:blue"),
                                br(),
                                code("Occupation_Income_Exception<-Occupation_Income[duplicated(Occupation_Income$ID), ] #creating exception table for values where date of birth is different", style = "color:blue"),
                                br(),
                                code("Occupation_Income<-Occupation_Income[!duplicated(Occupation_Income$ID), ]#creating a data set where date of birth is the same for both tables", style = "color:blue")

                            
                       ),
                         tabPanel("Occupation Overview",icon = icon("user-md"),
                                  h4("Data exploration:"),
                                  p("-Occupation dataset contains 100 observations of 7 variables;"),
                                  p("-Three of the 7 variables are categorical while others are numeric;"),
                                  p("-There is no missing data in the original dataset;"),
                                  p("-There is a data duplication for observation ID's 81:90 with the difference in only one variable Birth_Date"),
                                  verbatimTextOutput(outputId = "SummaryO1"),
                                  verbatimTextOutput(outputId = "SummaryO2")
                         ),
                         
                         tabPanel("Occupation Table", icon = icon("user-md"),
                         sidebarLayout(
                             sidebarPanel(
                                 checkboxInput("rownames", "Show row names", value=T),
                                 checkboxInput("order", "Column ordering", value=T),
                                 selectInput("selection", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                 selectInput("filter", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                 selectInput("dom", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices),
                                 strong("Clean'Occupation.txt'"),
                                 checkboxInput(inputId = "occ1", label = "The sex column contains '11' for males and '12' for females. Change this to '1' for males and '2' for females", value = TRUE),
                                 checkboxInput(inputId = "occ2", label = "Remove the invalid occupations and replace with a null value", value = TRUE),
                                 width = 3
                             ),
                             
                             # Show a plot of the generated distribution
                             mainPanel(
                                 DT::dataTableOutput("RawDataO"),
                                 tableOutput("SelRowsO")       
                                      )
                                        )
                                        ),
                         tabPanel("Income Overview",icon = icon("comment-dollar"),
                                  h4("Data exploration:"),
                                  p("-Income dataset contains 100 observations of 7 variables;"),
                                  p("-Two of the 7 variables are categorical while others are numeric;"),
                                  p("-There is no missing data in the original dataset;"),
                                  p("-There is a possible outliers for some observations (Income is 2m and over);"),
                                  verbatimTextOutput(outputId = "SummaryI1"),
                                  verbatimTextOutput(outputId = "SummaryI2")
                                  
                         ),
                         
                         tabPanel("Inclome Table",icon = icon("comment-dollar"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      checkboxInput("rownames1", "Show row names", value=T),
                                      checkboxInput("order1", "Column ordering", value=T),
                                      selectInput("selection1", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                      selectInput("filter1", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                      selectInput("dom1", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices),
                                      strong("Clean'Income.txt'"),
                                      checkboxInput(inputId = "inc1", label = "For Date of Transaction equal to 01/01/2015 remove any income values equal to 2,000,000", value = TRUE),
                                      checkboxInput(inputId = "inc2", label = "For Date of Transaction equal to 1/1/2017 remove any income values over 2,500,000", value = TRUE),
                                      checkboxInput(inputId = "inc3", label = "Create a new column called Rounded Incomes and populate it with income values rounded to the nearest 10,000", value = TRUE),
                                      width = 3
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      DT::dataTableOutput("RawDataI"),
                                      tableOutput("SelRowsI")       
                                    )
                                  )
                         ),
                         tabPanel("Summary Overview ", icon = icon("object-ungroup"),
                                  h4("Data exploration:"),
                                  p("-Merged dataset contains 150 observations of 10 variables;"),
                                  p("-Three of the 10 variables are categorical while others are numeric;"),
                                  p("-Missing data chart is showing there is 87.2% of data is presented which is high and there is no pattern
                                     found in data missingness;"),
                                  p("-Observations with ID from 51 to 99 have no missing data;"),
                                  p("-There are possible outliers for some observations (Income is 2m and over);"),
                                  verbatimTextOutput(outputId = "SummaryA1"),
                                  verbatimTextOutput(outputId = "SummaryA2")
                         ),
                         
                         tabPanel("Summary Table", icon = icon("object-ungroup"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      checkboxInput("rownames2", "Show row names", value=T),
                                      checkboxInput("order2", "Column ordering", value=T),
                                      selectInput("selection2", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                      selectInput("filter2", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                      selectInput("dom2", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices),
                                      width = 3
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      DT::dataTableOutput("RawData"),
                                      tableOutput("SelRows")       
                                    )
                                  )
                         ),
                         tabPanel("Exceptions", icon = icon("concierge-bell"),
                                  
                                  sidebarLayout(
                                    sidebarPanel(
                                      checkboxInput("rownames3", "Show row names", value=T),
                                      checkboxInput("order3", "Column ordering", value=T),
                                      selectInput("selection3", "Selection type", choices=c("none","single","multiple"), selected = "none"),
                                      selectInput("filter3", "Filter type", choices=c("none","top","bottom"), selected = "none"),
                                      selectInput("dom3", "DOM", choices=domChoices, multiple = TRUE, selected=domChoices),
                                      width = 3
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                      p("Observations where Birth_Data don't match has been loaded in exception table for further clarification."),
                                      p("*.x where X is variable assiciated with Income table"),
                                      p("*.y where y is variable assiciated with Occupation table"),
                                      DT::dataTableOutput("RawDataE"),
                                      tableOutput("SelRowsE")       
                                    )
                                  )
                         )
                         
                                 )),
                      
            tabPanel("Visualisation", icon = icon("chart-bar"),
                     tabsetPanel(
                                 tabPanel("Correlation charts",
                                  selectizeInput(inputId = "VariablesD", label = "Select variables:", choices = master_numer,
                                                 multiple = TRUE, selected = master_numer),
                                  hr(),
                                  
                                  withSpinner(
                                  plotOutput(outputId = "Corrgram2")
                                  ),
                                  checkboxInput(inputId = "abs2", label = "Uses absolute correlation", value = TRUE),
                                  selectInput(inputId = "CorrMeth2", label = "Correlation method", choices = c("pearson","spearman","kendall"), selected = "pearson"),
                                  selectInput(inputId = "Group2", label = "Grouping method", choices = list("none"=FALSE,"OLO"="OLO","GW"="GW","HC"="HC"), selected = "OLO")
                                  
                                  
                                  
                         ),
                         tabPanel("Missing Data",
                                  selectizeInput(inputId = "VariablesE", label = "Select variables:", choices = master_all,
                                                 multiple = TRUE, selected = master_all),
                                  checkboxInput(inputId = "cluster", label = "Cluster missingness", value = FALSE),
                                  plotOutput(outputId = "Missing"),
                                  hr(),
                                  p("There are 17 obsevations where Occupation and Income is missing"),
                                  withSpinner(
                                   plotOutput(outputId = "Missing1")
                                  )
                         ),
                         tabPanel("Boxplot",
                                  selectizeInput(inputId = "VariablesF", label = "Select variables:", choices = master_numer,
                                                 multiple = TRUE, selected = master_numer[2:11]),
                                  checkboxInput(inputId = "standardise", label = "Show standardized", value = FALSE),
                                  checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
                                  sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
                                  plotOutput(outputId = "Boxplot"),
                                  p("There are some outliers in Income")
                         ),
                         tabPanel("Rising value chart",
                                  selectizeInput(inputId = "VariablesG", label = "Select variables:", choices = master_numer,
                                                 multiple = TRUE, selected = master_numer[c(4, 5, 14, 18, 23, 25, 28)]),
                                  plotOutput(outputId = "RisingChart"),
                                  p("There is a gap in the values of variable Income that we expect to be continuous")
                         ),
                         tabPanel("Distributions",
                                  selectizeInput(inputId = "VariablesH", label = "Select variable:", choices = master_numer,
                                                 multiple = FALSE, selected = master_numer[2] ),
                                  helpText("N/A values have been removed"),
                                  plotOutput(outputId = "Distrib") 
                         ),
                         tabPanel("Matplots",
                                  checkboxInput("center", "Center", value=T),
                                  checkboxInput("scale", "Scale", value=T),
                                  selectizeInput(inputId = "VariablesK", label = "Select variable:", choices = master_numer,
                                                 multiple = TRUE, selected = master_numer ),
                                  #helpText("N/A values have been removed"),
                                  plotOutput(outputId = "Matpl"),
                                  p("Data belong together.There is no pattern to the data in its supplied order")
                         
                     ),
                     tabPanel("Conclusions",
                              p("-Dataset requires data pre-processing as some observations are missing and some observations have outliers;"),
                              br(),
                              p("-As there is no pattern in missing data I can say data is missing at random MAR and possibly Missing Completely at Random;"),
                              br(),
                              p("-Income has missing observations and outliers"),
                              br(),
                              p("-We can impute missing values and accept a bias or we can use a method which is tolerant to missing values. However these are a small subset of relevant methods;"),
                              br(),
                              p("-There are not many outliers in the data and they can be removed or changed to zero."),
                              br(),
                              br(),
                              br(),
                              strong("Thank you for your attention!")
                                                   ))
                         
                     )
                
                     
           )          
            
))
