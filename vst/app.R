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
library(DT)# Load
library(wesanderson)
library(shinyWidgets)
"BART" -> BART
"HARV" -> HARV
"SCBI" -> SCBI
"SERC"	-> SERC
"BLAN"->BLAN
"OSBS"->OSBS
"DSNY"->DSNY
"JERC"->JERC
"GUAN"->GUAN
"LAJA"->LAJA
"UNDE"->UNDE
"STEI"->STEI
"TREE"->TREE
"KONZ"->KONZ
"UKFS"->UKFS
"KONA"->KONA
"ORNL"->ORNL
"ORNL_2"->ORNL_2
"MLBS"->MLBS
"GRSM"->GRSM
"TALL"->TALL
"LENO"->	LENO
"DELA"->DELA
"WOOD"->WOOD
"DCFS"->DCFS
"NOGP"->NOGP
"ARIK"->ARIK
"STER"->STER
"CPER"->CPER
"RMNP"->RMNP
"BLUE"->BLUE
"PRIN"->PRIN
"CLBJ"->CLBJ
"CLBJ_2"->CLBJ_2
"OAES"->OAES
"YELL"->YELL
"NIWO"->NIWO
"MOAB"->MOAB
"SRER"->SRER
"JORN"->JORN
"REDB"->REDB
"ONAQ"->ONAQ	
"WREF"->WREF
"WREF_2"->WREF_2
"ABBY"->ABBY
"SJER"->SJER
"SOAP"->SOAP
"TEAK"->TEAK
"TOOL"->TOOL
"BARR"->BARR
"BONA"->BONA
"DEJU"->DEJU	
"HEAL"->HEAL
"PUUM"->PUUM

l <- "Live"
ldd <- "Live, disease damaged"
lpd <- "Live, physically damaged"
dbb <- "Dead, broken bole"
sd <- "Standing dead"
lod <- "Live,  other damage"
nlq <- "No longer qualifies"     
lbb <- "Live, broken bole"       
r <- "Removed"   
d <- "Downed"

## Set Dir to source location within RStudio
## if you don't use RStudio... uhm set the dir to your github folder with bigNEON in it
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
db$scientificNameMapNTag <- as.factor(db$scientificNameMapNTag)
db$taxonIDMapNTag <- as.factor(db$taxonIDMapNTag)

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

# Create list of 50 unique species ID with highest counts 
oneOff <-  select(db, scientificNameMapNTag)
speciesList <-  as.data.frame(summary(oneOff, maxsum = 50))

speciesList <- gsub('[[:digit:]]+', '', speciesList$Freq)
# colnames(speciesList) <- "scientificName"
speciesList <- as.data.table(gsub(":","",speciesList))
# colnames(speciesList) <- "TaxonID"
# speciesList$TaxonID <- as.factor(speciesList$TaxonID)

# the UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(title = "VST Individual ID Plotter", fluid = TRUE),
                           #titlePanel(title=div(img(src="lark.jpg", height = 100, width = 100, align = "right"), "Meadowlark is a NEON Phenology Data App")),
                           tabsetPanel(
                             tabPanel("Height vs DBH", fluid = TRUE,
                                      sidebarLayout(
                                        sidebarPanel(
                                          pickerInput("siteID_1","Site Select", c("BART" = BART,"HARV" = HARV,"SCBI" = SCBI,"SERC"	= SERC,"BLAN"=BLAN,"OSBS"=OSBS,"DSNY"=DSNY,"JERC"=JERC,"GUAN"=GUAN,
                                                                                  "LAJA"=LAJA,"UNDE"=UNDE,"STEI"=STEI,"TREE"=TREE,"KONZ"=KONZ,"UKFS"=UKFS,"KONA"=KONA,"ORNL"=ORNL,
                                                                                  "MLBS"=MLBS,"GRSM"=GRSM,"TALL"=TALL,"LENO"=LENO,"DELA"=DELA,"WOOD"=WOOD,"DCFS"=DCFS,"NOGP"=NOGP,
                                                                                  "ARIK"=ARIK,"STER"=STER,"CPER"=CPER,"RMNP"=RMNP,"BLUE"=BLUE,"PRIN"=PRIN,"CLBJ"=CLBJ,"CLBJ_2"=CLBJ_2,
                                                                                  "OAES"=OAES,"YELL"=YELL,"NIWO"=NIWO,"MOAB"=MOAB,"SRER"=SRER,"JORN"=JORN,"REDB"=REDB,"ONAQ"=ONAQ,	
                                                                                  "WREF"=WREF,"WREF_2"=WREF_2,"ABBY"=ABBY,"SJER"=SJER,"SOAP"=SOAP,"TEAK"=TEAK,"TOOL"=TOOL,"BARR"=BARR,
                                                                                  "BONA"=BONA,"DEJU"=DEJU,"HEAL"=HEAL,"PUUM"=PUUM),
                                                      options = list(`actions-box` = TRUE),multiple = T, selected = c("BART","HARV")
                                          ),
                                          pickerInput("yearCat","Select Year", choices =  c("2014" = year1, "2015"= year2, "2016"= year3, "2017"= year4, "2018"= year5),
                                                      options = list(`actions-box` = TRUE),multiple = T, selected = c("2014","2015","2016","2017","2018")
                                                                                                                      
                                          ),       
                                          pickerInput("treeStatus",label = "Select Tree Status", choices = c("Live" = l, 
                                                                                                             "Live, disease damaged" = ldd,
                                                                                                             "Live, physically damaged" = lpd,
                                                                                                             "Dead, broken bole" = dbb ,
                                                                                                             "Standing dead" = sd,
                                                                                                             "Live,  other damage" = lod,
                                                                                                             "No longer qualifies" = nlq,
                                                                                                             "Live, broken bole" = lbb,
                                                                                                             "Removed" = r ,
                                                                                                             "Downed"= d), options = list(`actions-box` = TRUE),multiple = T, selected = "Live"),
  
                                          uiOutput('columns'),
                                          actionButton("refreshPlot_1", "Draw Plot"),
                                          width = 2
                                        ),
                                        mainPanel( 
                                          plotlyOutput("plot_1") %>% withSpinner(color="#0dc5c1"))),
                                          h4("Data Table"),
                                          h5("This table shows the data that is used to plot the graph above. Use the 'Search:' function to filter the data table."),
                                      dataTableOutput("table_1")
                                      ## End Tab1
                             ),
                                      ## Start Tab2
                             tabPanel("Species", fluid = TRUE,
                                      sidebarLayout(
                                          sidebarPanel(
                                              pickerInput("yearCat2","Select Year", choices =  c("2014" = year1, "2015"= year2, "2016"= year3, "2017"= year4, "2018"= year5),
                                                          options = list(`actions-box` = TRUE),multiple = T, selected = c("2014","2015","2016","2017","2018")
                                                          
                                              ),       
                                              pickerInput("species", "Select Species",multiple = TRUE, selected = "Fagus grandifolia Ehrh.", options = list(`actions-box` = TRUE),
                                                          choices = c("Acer rubrum L.","Fagus grandifolia Ehrh.","Lindera benzoin (L.) Blume","Populus tremuloides Michx.",
                                                                      "Picea mariana (Mill.) Britton, Sterns & Poggenb.","Liquidambar styraciflua L.","Pinus palustris Mill.",
                                                                      "Tsuga canadensis (L.) CarriÃ¨re" )),
                                              
                                              pickerInput("treeStatus2",label = "Select Tree Status", choices = c("Live" = l, 
                                                                                                                 "Live, disease damaged" = ldd,
                                                                                                                 "Live, physically damaged" = lpd,
                                                                                                                 "Dead, broken bole" = dbb ,
                                                                                                                 "Standing dead" = sd,
                                                                                                                 "Live,  other damage" = lod,
                                                                                                                 "No longer qualifies" = nlq,
                                                                                                                 "Live, broken bole" = lbb,
                                                                                                                 "Removed" = r ,
                                                                                                                 "Downed"= d), options = list(`actions-box` = TRUE),multiple = T, selected = "Live"),
                                              
                                              actionButton("refreshPlot_2", "Draw Plot"),
                                              width = 2
                                          ),
                                          mainPanel( 
                                              plotlyOutput("plot_2") %>% withSpinner(color="#0dc5c1"))),
                                      h4("Data Table"),
                                      h5("This table shows the data that is used to plot the graph above. Use the 'Search:' function to filter the data table."),
                                      dataTableOutput("table_2")
) #side bar
)
)
## server VST
server <- function(input, output) {
  output$filterID = renderUI({
    selectInput("individualSelect", "select the ID", 
                choices = 
                  filter(siteID.x == input$siteID_1
                                 & growthForm == "single bole tree"
                                 & taxonRankMapNTag == "species")
                , selected = NULL)
  })

  # Tab 1
output$columns = renderUI({
    mydata <- db %>%
      filter(siteID.x == input$siteID_1 
             & growthForm == "single bole tree" 
             & taxonRankMapNTag == "species" 
             & year %in% input$yearCat
             & plantStatus %in% input$treeStatus)
    mydata <- mydata[!duplicated(mydata$individualID),]%>%
      select(individualID) 
    mydata <- as.list(mydata)
    selectInput('selectID', 'Select Individual IDs to plot', multiple = TRUE, selectize = TRUE, choices = mydata, selected = NULL)
  })
db_f <- reactive({
    if(length(input$selectID) == 0){
      filter(db, siteID.x == input$siteID_1 
             & growthForm == "single bole tree" 
             & taxonRankMapNTag == "species" 
             & year %in% input$yearCat
             & plantStatus %in% input$treeStatus)

    } else {
      filter(db, siteID.x == input$siteID_1 
             & growthForm == "single bole tree" 
             & taxonRankMapNTag == "species" 
             & year %in% input$yearCat
             & individualID %in% input$selectID
             & plantStatus %in% input$treeStatus)
        
    }
    })
  pp <- eventReactive((input$refreshPlot_1),{
    ggplot(db_f(), aes(x=stemDiameter, y=height, color = year, group=individualID)) +
      geom_point(size=1.4, alpha =.7) + 
      stat_summary(aes(group=individualID), fun.y=mean, geom="line", colour="green", alpha = .35, width = .5) +
      xlab("Stem Diameter At DBH(130cm)") +
      ylab("Tree Height (m)") +
      #geom_vline(xintercept =10,color="red") +
          scale_color_manual(breaks = c("2014","2015", "2016", "2017", "2018"),
                             values=c("#006d2c", "#8856a7", "#41b6c4", "#2c7fb8","#253494"))+
      theme_light()+
      labs(title = "NEON's Single Bole Trees",
           subtitle = "Each point represents an IndividualID. A green line is drawn from each individualID for each year it was sampled.",
           color='Legend:')  +
      facet_wrap(~siteID.x)
    
    })
  
  output$plot_1 <- renderPlotly({
    pp()
  })
  tableData <- reactive({
    db_f() %>%
    select(domainID.x, siteID.x, plotID, individualID, scientificNameMapNTag, date, year, height, stemDiameter, baseCrownHeight, plantStatus, growthForm, taxonRankMapNTag)
  })
  output$table_1 <- renderDataTable(tableData(),
                                    options = list(
                                      pageLength = 25
                                    ))

# End Tab 1
# Start Tab 2
db_f2 <- reactive({
        filter(db, scientificNameMapNTag == input$species
               & growthForm == "single bole tree" 
               & taxonRankMapNTag == "species" 
               & year %in% input$yearCat2
               & plantStatus %in% input$treeStatus2)

})
qq <- eventReactive((input$refreshPlot_2),{
    ggplot(db_f2(), aes(x=stemDiameter, y=height, color = year,group=individualID)) +
        geom_point(alpha =.7) + 
        stat_summary(aes(group=individualID), fun.y=mean, geom="line", colour="green", alpha = .35, width = .5) +
        xlab("Stem Diameter At DBH(130cm)") +
        ylab("Tree Height (m)") +
        #geom_vline(xintercept =10,color="red") +
        theme_light()+
        labs(title = "NEON's Single Bole Trees",
             subtitle = "Each point represents an IndividualID. A green line is drawn from each individualID for each year it was sampled.",
             color='Legend:')  +
        facet_wrap(~siteID.x)
    
})

output$plot_2 <- renderPlotly({
    qq()
})
tableData2 <- reactive({
    db_f2() %>%
        select(domainID.x, siteID.x, plotID, individualID, scientificNameMapNTag, date, year, height, stemDiameter, baseCrownHeight, plantStatus, growthForm, taxonRankMapNTag)
})
output$table_2 <- renderDataTable(tableData2(),
                                  options = list(
                                      pageLength = 25
                                  )) # End of Tab 2
} #end of server


shinyApp(ui = ui, server = server)

#



