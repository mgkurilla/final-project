#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(viridis)

# Define UI for application that draws a histogram

csi_index <- 
  read_excel("CSI_1953_2014.xls")

  # use view(csi_index) to ensure file displays properly

csi_index_by_year <- csi_index %>%
 select(caseId, CSI) %>%
 mutate(caseId = str_sub(caseId, 1, 4)) %>%
 group_by(caseId, CSI) %>%
 tally() %>%
 rename(totalCSI = n)

ui <- fluidPage(
  navbarPage(theme = shinytheme("yeti"),
             "Milestone 6",
    tabPanel("CSI Index by Newspaper",
             titlePanel("CSI Index by Newspaper"), 
                         selectInput(
                             inputId = "var",
                             label = "Newspaper",
                             choices = c("LA Times" = "laScore", "Chicago Tribune" = "chScore", 
                               "Washington Post" = "washScore", 
                               "New York Times" = "nyScore")),
                         mainPanel(plotOutput("line_plot"))),
    
    tabPanel("CSI Index by Year",
               titlePanel("CSI Index Frequency by Year"), 
        selectInput(
          inputId = "csi_index_by_year",
          label = "Year",
          choices = c("1953", "1954", "1955", "1956", 
                      "1957", "1958", "1959", "1960", 
                      "1961", "1962", "1963", "1964", 
                      "1965", "1966", "1967", "1968", 
                      "1969", "1970", "1971", "1972", 
                      "1973", "1974", "1975", "1976", 
                      "1977", "1978", "1979", "1980", 
                      "1981", "1982", "1983", "1984", 
                      "1985", "1986", "1987", "1988", 
                      "1989", "1990", "1991", "1992", 
                      "1993", "1994", "1995", "1996", 
                      "1997", "1998", "1999", "2000", 
                      "2001", "2002", "2003", "2004", 
                      "2005", "2006", "2007", "2008", 
                      "2009", "2010", "2011", "2012", 
                      "2013", "2014")),
      mainPanel(plotOutput("csi_hist_year"))),

   # decided to use yeti theme because it looked the best
   # remember to name newspapers as choices 
   # be mindful of the names assigned
   # note that tabPanel() creates a new tab
   
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("I wanted to show how case salience may vary by newspaper.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, I am working on this project to 
               determine Supreme Court majority opinion assignments by the Chief
               Justice as they relate to case salience."),
             h3("About Me"),
             p("My name is Michelle Kurilla and I study English and Government.")),
    tabPanel("Methodology", 
             titlePanel("Methodology"),
             h3("Methods and Data Wrangling"),
             p("My project includes data from the Harvard Dataverse and the 
               Supreme Court database."))))

    server <- function(input, output) {
        output$line_plot <- renderPlot({
            data <- csi_index %>% select(.data[[input$var]])
            ggplot(data, mapping = aes(x = .data[[input$var]])) + 
            geom_bar() + 
            labs(title = "Case Salience Index by Newspaper",
                 x = "CSI", 
                 y = "Total Count of CSI")
            
        })
        output$csi_hist_year <- renderPlot({
          data <- csi_index_by_year %>% 
            filter(caseId == input$csi_index_by_year)
          ggplot(data, mapping = aes(x = totalCSI, fill = factor(CSI))) + 
                   geom_bar() + 
            labs(title = "CSI by Year", 
                 x = "CSI", 
                 y = "Total Count of CSI")
      
        })
    }
    
# Add what 0, 1, and 2 mean 
# Add what CSI means
# Probably add subtitle and title
# Per decade
    # Run the application -----

shinyApp(ui, server)
