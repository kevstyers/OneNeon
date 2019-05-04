# load libraries
# setwd("X:/1_GitHub/BigNeon")


library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(shiny)
library(shinythemes)
library(htmlwidgets)
library(shinycssloaders)
library(DT)

## Set Dir to source location within RStudio
## if you don't use RStudio... uhm set the dir to your github folder with bigNEON in it
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

siteData <- data.table::fread("domain_site_lookup.csv", stringsAsFactors = FALSE)
db <- data.table::fread("vstDataBase.csv", stringsAsFactors = FALSE)
# db<-db[!duplicated(db$uid), ]
# db <- db %>%
#   select(-V1)

db$individualID <- substring(db$individualID, 14)
db$individualID <- as.factor(db$individualID)
db$year <- as.factor(db$year)
db$plotID <- as.factor(db$plotID)
db$domainID.x <- as.factor(db$domainID.x)
db$siteID.x <- as.factor(db$siteID.x)
db$individualID <- as.factor(db$individualID)
db$growthForm <- as.factor(db$growthForm)

# remove NA
sum(is.na(db$stemDiameter))
names(db)
db<-db[!(db$stemDiameter=="NA" | db$stemDiameter=="")]

# testing out name variables, forget why this worked

year1 <- "2014"
year2 <- "2015"
year3 <- "2016"
year4 <- "2017"
year5 <- "2018"

allIn <- db %>%
  filter(siteID.x == "TREE")

allIn<- allIn[!duplicated(allIn$individualID), ] %>%
  select(individualID)


# the UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(title = "VST Individual ID Plotter", fluid = TRUE),
                           #titlePanel(title=div(img(src="lark.jpg", height = 100, width = 100, align = "right"), "Meadowlark is a NEON Phenology Data App")),
                           tabsetPanel(
                             tabPanel("Height vs DBH", fluid = TRUE,
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput("subsystem_1",
                                                      label = "subsystem",
                                                      choices = c("TOS"),
                                                       selected = "TOS"),
                                          selectInput("domainID_1",
                                                      label = "Domain ID",
                                                      choices = sort(unique(db$domainID.x)),
                                                      selected = "D05"
                                          ),
                                          uiOutput("siteID_1"),
                                          selectInput("yearCat",
                                                      multiple = TRUE,
                                                      label = "Select Year",
                                                      choices =  c("2014" = year1, "2015"= year2, "2016"= year3, "2017"= year4, "2018"= year5),
                                                      selected = "2015"
                                                  
                                          ),
                                          uiOutput('columns'),
                                          #selectInput("individualSelect",
                                          #            multiple = TRUE, 
                                          #            label = "Select Individuals to Plot",
                                          #            choices = allIn),
                                          actionButton("refreshPlot_1", "Draw Plot"),
                                          width = 2
                                        ),
                                        
                                        
                                        mainPanel( 
                                          plotlyOutput("plot_1") %>% withSpinner(color="#0dc5c1"))),
                                          h4("Data Table"),
                                      dataTableOutput("table_1")
                             )
                                      # h4("Detailed Phenophase Analysis by Site and Phenophase"),
                                      # plotOutput("plot_2")))
                             ))










## server VST
server <- function(input, output) {
  
  
  
  # Determine sites
  theSites <- reactive({
    # based on the input...
    temp <- if (input$domainID_1==''){
      return(NULL)
    } else {
      # reduce siteIDs to the domainID chosen
      siteData %>% filter(domainid==input$domainID_1, type=="TOS") %>% select(siteid) -> a
      return(a)
    }
  })
  #Define sites
  output$siteID_1 <- renderUI({
    if(input$domainID_1==''){
      selectInput("siteID", "Select a Site", c(Choose = '', choices = theSites()),
                  selectize = TRUE, multiple = FALSE)
    } else {
      if(nrow(theSites())==1){
        # if a domain only has one siteID value, just set it to that siteID for convenience
        textInput("siteID_1", "Select a Site", value = theSites())
      } else {
        selectInput("siteID_1", "Select a Site", c(Choose = '', choices = theSites()),
                    selectize = TRUE, multiple = FALSE, selected = "TREE")
      }
    }
  })
  
  output$filterID = renderUI({
  
    selectInput("individualSelect", "select the ID", 
                choices = 
                  filter(siteID.x == input$siteID_1
                                 & growthForm == "single bole tree"
                                 & taxonRankMapNTag == "species")
                , selected = NULL)
  })
  
  output$columns = renderUI({
    mydata <- db %>%
      filter(domainID.x == input$domainID_1 
             & siteID.x == input$siteID_1 
             & growthForm == "single bole tree" 
             & taxonRankMapNTag == "species" 
             & year %in% input$yearCat)
    mydata <- mydata[!duplicated(mydata$individualID),]%>%
      select(individualID) 
    mydata <- as.list(mydata)
    selectInput('selectID', 'Select Individual IDs to plot', multiple = TRUE, selectize = TRUE, choices = mydata, selected = NULL)
  })

  

  db_f <- reactive({
    if(length(input$selectID) == 0){
      filter(db, domainID.x == input$domainID_1 
             & siteID.x == input$siteID_1 
             & growthForm == "single bole tree" 
             & taxonRankMapNTag == "species" 
             & year %in% input$yearCat)
    } else {
      filter(db, domainID.x == input$domainID_1 
             & siteID.x == input$siteID_1 
             & growthForm == "single bole tree" 
             & taxonRankMapNTag == "species" 
             & year %in% input$yearCat
             & individualID %in% input$selectID)
    }
    })
  

    

  
  ## wont plot d17 TEAK, not sure why 
  ## OMG they're missing bole heights!
  pp <- eventReactive((input$refreshPlot_1),{
    ggplot(db_f(), aes(x=stemDiameter, y=height, color = year, group=individualID)) +
      geom_point(size=1) + 
      stat_summary(aes(group=individualID), fun.y=mean, geom="line", colour="green", alpha = .5, width = .5) +
      xlab("Stem Diameter At DBH(130cm)") +
      ylab("Tree Height (m)") +
      #geom_vline(xintercept =10,color="red") +
      labs(title = "NEON's Single Bole Trees",
           subtitle = "Each point represents an IndividualID. A green line is drawn from each individualID for each year it was sampled.")# This can be used to find erroneuous data points, where the green line is very long.") 
    })
  
  output$plot_1 <- renderPlotly({
    pp()
  })
  

  
  output$table_1 <- renderDataTable(db_f(),
                                    options = list(
                                      pageLength = 25
                                    )
  )
}

shinyApp(ui = ui, server = server)
