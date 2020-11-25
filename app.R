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
library(ggthemes)
library(gt)

# Define UI for application that draws a histogram

csi_index <-
  read_excel("CSI_1953_2014.xls")

mean_median_csi <- csi_index %>%
  select(caseId, CSI) %>%
  mutate(caseId = str_sub(caseId, 1, 4)) %>%
  rename(year = caseId) %>%
  group_by(year) %>%
  summarize(mean = mean(CSI), 
            median = median(CSI))

justice_opinion_excel <- read_csv("SCDB_2020_01_justiceCentered_Citation_2.csv",
                                  col_types = cols(
                                    .default = col_double(),
                                    caseId = col_character(),
                                    docketId = col_character(),
                                    caseIssuesId = col_character(),
                                    voteId = col_character(),
                                    dateDecision = col_character(),
                                    usCite = col_character(),
                                    sctCite = col_character(),
                                    ledCite = col_character(),
                                    lexisCite = col_character(),
                                    chief = col_character(),
                                    caseName = col_character(),
                                    dateArgument = col_character(),
                                    dateRearg = col_character(),
                                    lawMinor = col_character(),
                                    justiceName = col_character()
                                  ))

# Cases Assigned by Chief Justices

judge_cases_sum <-
  justice_opinion_excel %>%
  select(caseId, majOpinWriter, majOpinAssigner) %>%
  distinct() %>%
  mutate(caseId = str_sub(caseId, 1, 4)) %>%
  drop_na() %>%
  filter(majOpinWriter %in% c(90, 99, 102, 111), 
         majOpinAssigner %in% c(90, 99, 102, 111)) %>%
  filter(majOpinAssigner == majOpinWriter) %>%
  mutate(writer_name = case_when(majOpinWriter == 90 ~ "Warren", 
                                 majOpinWriter == 99 ~ "Burger", 
                                 majOpinWriter == 102 ~ "Rhenquist", 
                                 majOpinWriter == 111 ~ "Roberts")) %>% 
  mutate(assigner_name = case_when(majOpinAssigner == 90 ~ "Warren", 
                                   majOpinAssigner == 99 ~ "Burger", 
                                   majOpinAssigner == 102 ~ "Rhenquist", 
                                   majOpinAssigner == 111 ~ "Roberts")) %>%
  group_by(writer_name, caseId) %>%
  tally() %>%
  rename(total_cases_written = n) %>%
  rename(year = caseId)

# use view(csi_index) to ensure file displays properly

ui <- fluidPage(
  navbarPage(
    theme = shinytheme("flatly"),
    "Case Salience and Chief Justices",
    tabPanel("Home",
             titlePanel("Welcome"),
             h3("Elevator Pitch")),
    
    tabPanel(
      "CSI Index Breakdown",
      titlePanel("CSI Index Breakdown"),
      h3("Total CSI Index by Newspaper"),
      p("explanation of why it's valuable to see csi index by paper"),
      sidebarPanel(
      selectInput(
        inputId = "var",
        label = "Newspaper",
        choices = c(
          "LA Times" = "laScore",
          "Chicago Tribune" = "chScore",
          "Washington Post" = "washScore",
          "New York Times" = "nyScore")),
      p("explanation of the newspapers and their readership")),
      mainPanel(plotOutput("csi_newspaper")),
      h3("Average CSI Index by Year"), 
      p("explanation of mean realted to csi index"), 
      sidebarPanel(
      selectInput(
        inputId = "mean_median", 
        label = "Mean vs. Median", 
        choices = c("mean", "median")), 
      p("explanation")), 
      mainPanel(plotOutput("mean_median_csi"), width = "100%")),

tabPanel("Chief Justice Case Frequency", 
         titlePanel("Chief Justice Case Frequency"),
         p("this is more about the project I'm working on and why chief justices 
    are important to study"), 
         sidebarPanel(
           p("explanation"))),
mainPanel(plotOutput("judge_cases")),

  tabPanel("Modeling and More", 
  titlePanel("Modeling and More")),
 
  tabPanel("About",
    titlePanel("About"),
    h3("Project Summary"),
    p("Hello, I am working on this project to
               determine Supreme Court majority opinion assignments by the Chief
               Justice as they relate to case salience."),
    h3("Methods and Data Wrangling"),
    p("My project includes data from the Harvard Dataverse and the
      Supreme Court database."),
    h3("About Me"),
    p("My name is Michelle Kurilla and I am a junior studying English and 
    Government at Harvard College. I am interested in storytelling, literature, 
      law, ethics, public policy, philosophy, and their intersection. I am 
      also interested in data's role in journalism. You can find me at
      mgkurilla@college.harvard.edu, check out my code on my GitHub account, 
      or connect with me on LinkedIn."))))


server <- function(input, output) {
  output$csi_newspaper <- renderPlot({
    data <- csi_index %>% select(.data[[input$var]])
    ggplot(data, mapping = aes(x = .data[[input$var]])) +
      geom_bar() +
      theme_classic() + 
      labs(title = "Case Salience Index by Newspaper",
           x = "CSI",
           y = "Total Count of CSI")
  })
  
  output$mean_median_csi <- renderPlot({
    data <- mean_median_csi
    ggplot(data, mapping = aes(x = year, y = case_when(
      input$mean_median == "mean" ~ mean, 
      input$mean_median == "median" ~ median))) +
      geom_col() + 
      scale_x_discrete(breaks = c("1953","1954","1955","1956","1957","1958",
                                    "1959","1960","1961","1962","1963","1964",
                                    "1965","1966","1967","1968","1969","1970",
                                    "1971","1972","1973","1974","1975","1976",
                                    "1977","1978","1979","1980","1981","1982",
                                    "1983","1984","1985","1986","1987","1988",
                                    "1989","1990","1991","1992","1993","1994",
                                    "1995","1996","1997","1998","1999","2000",
                                    "2001","2002","2003","2004","2005","2006",
                                    "2007","2008","2009","2010","2011","2012",
                                    "2013","2014"),
                         labels = c("1953","","","","","1958","","",
                                    "","","1963","","","","","1968",
                                    "","","","","1973","","","",
                                    "","1978","","","","","1983","",
                                    "","","","1988","","","","",
                                    "1993","","","","","1998","","",
                                    "","","2003","","","","","2008",
                                    "","","","","2013","")) + 
      geom_smooth(method = "lm") + 
      theme_classic() + 
      labs(title = "Mean vs. Median CSI Index by Year", 
           x = "Year", 
           y = "Mean vs. Median CSI")
    
  })
  
  output$judge_cases <- renderPlot({
    data <- judge_cases_sum
      ggplot(data, mapping = aes(x = year, y = total_cases_written, 
                                 fill = writer_name, color = writer_name)) +
      geom_col() + 
      theme_classic() + 
      labs(title = "Mean vs. Median CSI Index by Year", 
           x = "Justices", 
           y = "Total Cases")
  })
  
  
}

    # Run the application -----

shinyApp(ui, server)
