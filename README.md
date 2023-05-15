#install.packages(c("shiny", "shinydashboard", "ggplot2", "DBI", "RSQLite"))


library(shiny)
library(shinydashboard)
library(ggplot2)
library(DBI)
library(RSQLite)


# Connect to database and execute querySelect function

querySelect <- function(dbName, query){
  
  #connect to the db
  dbConn <- dbConnect(drv = SQLite(), dbname = dbName)
  
  #query the db; dbGetQuery; returns results in a df
  queryResultsDF <- dbGetQuery(conn = dbConn, statement = query)
  
  #disconnect from the db
  dbDisconnect(dbConn)
  
  
  return(queryResultsDF)
  
  
}




#The first four data frames below are used for the real estate menuItem, two of them are used in the first tab called market outlook

marketDF <- querySelect("estate.sqlite", "SELECT strftime('%m-%Y', Properties.DateListed) AS MonthYear,
                        AVG(Properties.ListPrice) AS AvgListPrice
                        FROM Properties
                        GROUP BY MonthYear")

market2DF <- querySelect("estate.sqlite", "SELECT strftime('%Y-%m', p.DateListed) AS MonthYear2, p.SubDivisionID, s.Subdivision, AVG(p.ListPrice) AS AvgListPrice2
                         FROM Properties p
                         INNER JOIN SubDivision s ON p.SubDivisionID = s.SubDivisionID
                         GROUP BY MonthYear2, p.SubDivisionID, s.Subdivision
                         ORDER BY MonthYear2, p.SubDivisionID;")


agentDF <- querySelect("estate.sqlite", "SELECT
                       Agents.FirstName || ' ' || Agents.LastName AS FullName, 
                       COUNT(Properties.SellingAgent) AS sPropertyCount, 
                       julianday(COALESCE(Properties.DateSold, '11/30/2023')) - julianday(Properties.DateListed) AS DaysOnMarket
                       FROM Agents
                       LEFT JOIN Properties ON Agents.ID = Properties.SellingAgent
                       LEFT JOIN Agents AS ListingAgents ON Properties.ListingAgent = ListingAgents.ID
                       GROUP BY FullName")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Time-series"),
  dashboardSidebar(
    
    #Decided to make menuItems in the sidebar,
    #Under the impression that it would make the sidebar look appealing to the user 
    #and also thinking it may help clean up the server area below 
    
    sidebarMenu(
      
      menuItem("Real Estate", tabName = "estate", icon = icon("house"), 
               menuSubItem("Market Outlook", tabName = "sub1a", icon = icon("magnifying-glass")),    #MenuSubItems are rendered
               menuSubItem("Agent Performance", tabName = "sub1b", icon = icon("person"))))),
  dashboardBody(dashboardBody( tabItems(
    #Tabitems for when a submenu is selected
    #First 3 are for realestate
    
    tabItem(tabName = "sub1a",
            fluidRow(
              column(width = 12,
                     selectInput("select1", "Select an outlook",
                                 c("Subdivision" = "sub",
                                   "Overall" = "ovr"),
                                 selected = "sub"),
                     checkboxGroupInput("checkbox1", "Select subdivisions to display",     #Select checkbox to display different subdivision average list prices 
                                        choices = unique(market2DF$Subdivision),
                                        selected = unique(market2DF$Subdivision),
                                        inline = TRUE),
                     plotOutput("plot1a"))
            )
    ),
    
    tabItem(tabName = "sub1b",
            fluidRow(
              column(width = 12, height = 12,
                     plotOutput("plot1b")
              ))))))
)


server <- function(input, output) {
  
  # Define plot 1 output
  output$plot1a <- renderPlot({
    
    if(input$select1 == "sub"){
      ggplot(subset(market2DF, Subdivision %in% input$checkbox1),
             aes(x = MonthYear2, y = AvgListPrice2, group = Subdivision, color = Subdivision)) +
        geom_point()+
        geom_line()+
        geom_text(aes(y = AvgListPrice2, label = round(AvgListPrice2)), vjust = -0.5) +
        scale_y_continuous(labels = scales::dollar_format()) +
        labs(title = "Average List Price per Month by Subdivision",
             x = "Month",
             y = "Price", 
             color = "Subdivision") +
        theme_bw()
      
      
    }  else if(input$select1 == "ovr"){
      ggplot(marketDF, aes(x = MonthYear)) + 
        geom_point(aes(y = AvgListPrice, color = "List Price"), size = 3, alpha = 0.8) +
        geom_text(aes(y = AvgListPrice, label = ifelse(AvgListPrice >= 1000000, paste0("$", format(round(AvgListPrice / 1000000, 2), nsmall = 2), "M"), paste0("$", format(round(AvgListPrice / 100), big.mark = ","), "K"))), vjust = -0.5, size = 3, color = "blue", fontface = "bold") +
        scale_y_continuous(labels = scales::dollar_format(), expand = expansion(mult = c(0.05, 0.05))) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 18),
              axis.title = element_text(face = "bold", size = 14),
              axis.text = element_text(size = 12),
              legend.title = element_blank(),
              legend.text = element_text(face = "bold", size = 12)) +
        labs(title = "Average Overall List Price per Month",
             x = "Month",
             y = "Price",
             color = "") 
      
      
      
      
    }
  })
  
  
}

shinyApp(ui, server)


