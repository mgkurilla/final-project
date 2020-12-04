# load libraries

library(shiny)
library(readxl)
library(tidyverse)
library(ggplot2)
library(shinythemes)
library(viridis)
library(ggthemes)
library(gt)
library(rstanarm)

# read excel of csi index from Harvard dataverse

csi_index <-
  read_excel("CSI_1953_2014.xls")

# set mean_csi by selecting and mutating relevant data

mean_median_csi <- csi_index %>%
  select(caseId, CSI) %>%
  mutate(caseId = str_sub(caseId, 1, 4)) %>%
  rename(year = caseId) %>%
  group_by(year) %>%
  summarize(mean = mean(CSI), 
            median = median(CSI)) %>%
  mutate(year = as.factor(year))

# Read justice_opinion_excel

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

# Create Sum of Cases Justices Assigned to Themselves

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
                                 majOpinWriter == 102 ~ "Rehnquist", 
                                 majOpinWriter == 111 ~ "Roberts")) %>% 
  mutate(assigner_name = case_when(majOpinAssigner == 90 ~ "Warren", 
                                   majOpinAssigner == 99 ~ "Burger", 
                                   majOpinAssigner == 102 ~ "Rehnquist", 
                                   majOpinAssigner == 111 ~ "Roberts")) %>%
  group_by(writer_name, caseId) %>%
  tally() %>%
  rename(total_cases_written = n) %>%
  rename(year = caseId)

# Create Joined CSV

joined_csv <- inner_join(csi_index, justice_opinion_excel, by = "caseId")

# Joined CSV Equal (for Test in Model)

joined_csv_equal <- joined_csv %>%
  select(CSI, caseId, majOpinWriter, majOpinAssigner) %>%
  distinct() %>%
  drop_na() %>%
  mutate(test = if_else(majOpinAssigner == majOpinWriter, 1, 0)) 

# Create regression tibble to manually add gt

regression_1 <- tibble(coefficient = 0.8438, 
                       intercept = 2.6709)

# Assign image to add to ui 

myImage <- img(src="Supreme_Court_Image.jpg")

# Add DT Table on Home

case_by_justice <- joined_csv %>%
  select(CSI, caseId, majOpinWriter, majOpinAssigner, laScore, chScore, 
         washScore, nyScore) %>%
  distinct() %>%
  drop_na() %>%
  mutate(test = if_else(majOpinAssigner == majOpinWriter, 1, 0)) %>%
  mutate(year = str_sub(caseId, 1, 4)) %>%
  mutate(year = as.numeric(year)) %>%
  relocate(year, .after = CSI) %>%
  rename(Year = year, 
         Test = test)

# Create tibble for regression plot

regression_data <- tibble(CSI = c("2.6709", "3.514"), 
                          test = c("0", "1"))


# set up UI

ui <- fluidPage(
  
  navbarPage(
    
  # add shiny theme
    
    theme = shinytheme("flatly"),
    "Supreme Court Chief Justices and Case Salience",
    
  # name tab to match its content
  
    tabPanel("Introduction",
             
  # add title to tab
  
             titlePanel("Introduction"),
             
  # explain project layout and why it matters 
  
             sidebarLayout(
             mainPanel(
             p("Chief Justices are charged with assigning each of the nine 
               justices to write the opinion when they side with the majority.
               This happens more often than not. But how often do Chief Justices 
               assign the case to themselves?",
              strong("How does this case assignment vary with case 
              significance or perceived significance of the case?")), 
              p("This project analyzes more than 60,000 majority opinions 
              assigned and and written from 1953 to 2014 and their relationship 
              to a case salience index ranging from 0 to 8. Case salience index, 
              which is described more in-depth on the CSI Index Breakdown tab, 
              correlates to newspaper coverage by The Los Angeles Times, 
              The Chicago Tribune, The Washington Post, and The New York Times. 
              Higher scores indicate more media attention."), 
             p("Before jumping into the analysis of the data, it is worth noting
               that this project only looks at majority opinion assignments
               when the Chief Justice sided with the majority of justices. It 
               also includes the period of tiem when Chief Justices (Burger, 
               Warren, Rehnquist, and Roberts) were on the Supreme Court but had
               not yet been named Chief Justice."), 
             p("This project mostly looks at instances in which the Chief 
               Justice assigned and wrote the opinion himself. Under the 
               Chief Justice Frequency tab, I sum the total number of cases 
               written by Warren, Burger, Rehnquist, and Roberts, including 
               instances where other justices assigned the cases to them."),
             p("The datatable below provides a snapshot into the data I am working
               with in this project. It lists all of the cases heard by the 
               Supreme Court from 1953 to 2014. This datatable isn't limited
               to only cases written and assigned by the Chief Justices."), 
             p("From left to right, the columns refer to the case salience
               index, case year, the case ID number (which is unique to each 
               case), the majority opinion writer, the majority opinion assigner
               (which is often times different from the majority opinion 
               writer),the case salience index from The Los Angeles Times, the 
               case salience index from The Chicago Tribune, the CSI Score from 
               The Washington Post, the case salience index from The New York 
               Times, and the test column that indicates whether the majority 
               opinion assigner and the majority opinion writer are the same 
               person.")),
             
  # set imageOutput for server 
  
             imageOutput("myImage"))),  

  # name tab to match its content
    
    tabPanel("Chief Justice Case Frequency", 
             
  # add title to tab
  
             titlePanel("Chief Justice Case Frequency"),
             
  # explain chart and its findings
  
            p("Here's an aggregation of the total number of majority opinions 
         Supreme Court Justices assigned and wrote themselves from 1953 to 2014 
         broken down by year. It is worth noting that this dataset includes 
         opinions written from 1954 to 2019, extending beyond the 2014 cutoff
         for CSI index."), 
            p("Chief Justice Warren wrote the",
            strong("most cases in 1954, 1956, 1962, and 1968 with 12 
                   total cases.")),
            p("Chief Justice Burger wrote the",
            strong("most cases in 1972 with 19 total cases.")), 
            p("Chief Justice Rhenquist wrote the",
            strong("most cases from 1987 to 1989 with 15 total cases.")), 
            p("Chief Justice Roberts wrote the",
            strong("a total of 8 cases consistently in 2005, 2008-2010, 2012, 
                   and 2016.")),
  
  # set plotOutput for server
  # set width to 100% so it takes up tab
        
             mainPanel(plotOutput("judge_cases"), width = "100%")),
    
  # name tab to match its content 

    tabPanel(
      "CSI Index Breakdown",
      
  # add title to tab 
  
      titlePanel("CSI Index Breakdown"),
      
  # add heading for first part of section 
  
      h3("Total Case Salience Index by Newspaper"),
      
    # explain CSI and bold highlights
    
      p("Here's a breakdown of the total number of cases that were assigned a 
      case salience index of 0, 1, or 2 in The Los Angeles Times, The Chicago
      Tribune, The Washington Post, and The New York Times."), 
      p("Each paper has the possibility of", 
        strong("scoring 0 to 2."),
        "A case salience index number of 0 means the case was not reported in 
        the paper. A case salience index number of 1 means the case was reported 
        in the newspaper but not the front page. A case salience index was 
        reported on the front page. It is worth noting that this section does 
        not look at the total case salience index across the four papers, which
        ranges from 0 to 8."),
      p("The New York Times had the",
        strong("most reported stories with a case salience index of 2.")), 
      p("The Chicago Tribune had the", 
        strong("least reported stories with a case salience index of 2.")),
  
  # set up sidebar selection panel
    
      sidebarPanel(
        
  # set up selection function 
        
      selectInput(
        
  # set input id for server 
        
        inputId = "var",
        
  # set label for sidebar selection panel
  
        label = "Newspaper",
  
  # set choices of the sidebar selection panel
  # rename choices from default column name to newspaper
  
        choices = c(
          "LA Times" = "laScore",
          "Chicago Tribune" = "chScore",
          "Washington Post" = "washScore",
          "New York Times" = "nyScore")),
  
  # add text to sidebar selction panel
  
      p("Choose a variable to compare case salience index by newspaper. The 
      dropdown menu includes four newspaper options to choose from: The Los 
      Angeles Times (LA Times), The Chicago Tribune (Chicago Tribune), The 
      Washington Post (Washington Post), and The New York Times 
      (New York Times)")),
  
  # set plot output id for server
  
      mainPanel(plotOutput("csi_newspaper")),
  
  # set title of second portion of section 
  
      h3("Average Case Salience Index by Year"), 
  
  # add explanation to chart
  
      p("Here's a breakdown of the mean and median case salience index of 
        Supreme Court cases per year. Unlike the section above, this includes
        the total case salience index ranging from 0 to 8. A case salience index
        score of 0 means that none of the four papers (The LA Times, The 
        Chicago Tribune, The Washington Post, or The New York Times) covered the
        case, whereas a score of 8 means that all four papers covered the case, 
        and it recieved front page coverage."), 

  # use "strong" to bold the findings
  
      p("The mean and median case salience index are",
        strong("not consistently rising nor falling over time.")), 
  
  # set sidebar Panel for layout purposes
  
      sidebarPanel(
        
  # set up selection function 
        
      selectInput(
        
  # set id for server 
        
        inputId = "mean_median", 
        
  # set label for legend 
  
        label = "Mean vs. Median", 
  
  # set choices of the sidebar selection panel
  # rename choices so they are capitalized
  
        choices = c("Mean" = "mean", "Median" = "median")), 
  
  # explain sidebar selection menu 
  
      p("Choose a variable to compare case salience index. This dropdown menu 
      includes two options: mean (average) and median (middle).")), 
  
  # set plotOutput Id for server 
  # set width so it extends across the bottom of the page
  
      mainPanel(plotOutput("mean_median_csi"), width = "100%")),

  # name tab to match content 

   tabPanel("Modeling Case Salience", 
            
  # list the title for the page
  
   titlePanel("Modeling Case Salience"), 
  
  # add text 
  
   p("The two models below interpret the impact of different variables on an output
   of case salience index."),
   p("The test variable indicates whether the majority opinion assigner and the 
   majority opinion writer are the same justice. It is worth noting that this 
     model is limited to the four Chief Justices who opined from 1953 to 2014
     (Burger, Warren, Rehnquist, and Roberts)."),
  
  # set plotOutput id for regression 
  
   plotOutput("regression_1_chart"),
  
  # explain regression and how it was structured 
  
   p("This model was created by", 
     strong("linear regression of one variable (test) to predict the output of 
   case salience index.")), 
  
  # use br() to set linebreaks 
  
   br(),
  
  # set gt_output for server 
  
   gt_output(outputId = "regression_1_model"), 
  
  # use br() to set linebreaks
  
   br(),
  
  # interpret model and how it relates back to csi 
  
   p("In this model, the intercept (which represents the case salience 
     index) is 2.6709 when test is equal to 0. In other words",
     strong("the case salience index is 2.6709 when the Chief Justice does not assign the opinion to 
     themselves.")),
   p("As a reminder, a case salience index of 2 means that a case
     receieved front page coverage in one newspaper or some coverage in two 
     newspapers. On the other hand, the case salience index increases by 0.8436
     when test is equal to 1."), 
   p("This means",
     strong("the cases salience index is equal to 3.5147 when the Chief Justice
            assigns the opinion to themselves.")), 
   p("As a reminder, a case salience index of 3 means that a case received front
      page coverage in one newspaper and some coverage in another newspaper, or 
      some newspaper coverage in three of the newspapers. A case salience index
      of 4 means that a case received front page coverage in two newspapers, 
      front page coverage in one newspaper and some coverage in two other 
      newspapers, or some news coverage in all four newspapers.")),
   

  # name tab to match its content 

  tabPanel("About",
           
  # title the tab appropriately 
  
    titlePanel("About"),
  
  # add heading 
  
    h3("Project Summary"),
  
  # explain the purpose of the project
  
    p("I am working on this project to determine the relationship between case 
      salience index and the instances in which Chief Justices assign themselves
      majority opinions."),
  
  # add heading

    h3("Methods and Data Wrangling"),
  
  # include links to where my data is from 
  
    p("My project includes data from the", 
      a(href = "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/UR2KYE", 
        "Harvard Dataverse,"),
      "and the University of Washington Law School's", 
      a(href = "http://scdb.wustl.edu/data.php", 
        "Supreme Court justice-centered database.")),
  
  # add heading

    h3("About Me"),
  
  # explain a little more about me 
  
    p("My name is Michelle Kurilla and I am a junior studying English and 
    Government at Harvard College."), 
    p("I am interested in storytelling, literature, 
      law, ethics, public policy, philosophy, and their intersection. I am 
      also interested in data's role in journalism."),
  
  # add links so people can email me questions, head to my github, or connect
  # with me on LinkedIn
  
    p("You can find me at",
      a("mgkurilla@college.harvard.edu,", 
        href = "malito:mgkurilla@college.harvard.edu"), 
        "check out my code on my",
    a("GitHub account,", href = "https://github.com/mgkurilla/supreme-court-csi"),
    "or connect with me on ",
    a("LinkedIn.", href = "https://www.linkedin.com/in/michellekurilla1/")))))

server <- function(input, output) {
  
  # match { at the end
  
  output$myImage <- renderImage({
    
    # use renderImage instead of renderPlot
    # set desired height and width of image
    
    list(src = "Supreme_Court_Image.jpg",
         width = 400,
         height = 300,
         alt = "This is alternate text", 
         align = "left")
  }, deleteFile = FALSE)
  
  # use renderPlot to create chart
  # match plotOutput id from above
 
  output$judge_cases <- renderPlot({
    
  # use data assigned at the top of the shiny app 
    
    data <- judge_cases_sum
    
  # set fill and color to writer_name to see the associations 
    
    ggplot(data, mapping = aes(x = year, y = total_cases_written, 
                               fill = writer_name, color = writer_name)) +
      geom_col() + 
      
   # use "" to label out every 5 years
      
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
                                  "2013","2014", "2015", "2016", "2017", "2018", 
                                  "2019"),
                       labels = c("1953","","","","","1958","","",
                                  "","","1963","","","","","1968",
                                  "","","","","1973","","","",
                                  "","1978","","","","","1983","",
                                  "","","","1988","","","","",
                                  "1993","","","","","1998","","",
                                  "","","2003","","","","","2008",
                                  "","","","","2013","", "", "", "", "2018", 
                                  "")) + 
      theme_classic() + 
      labs(title = "Total Number of Cases Assigned and Written per Justice", 
           x = "Year", 
           y = "Total Cases Written and Assigned")
  })
  
  # use renderPlot to create chart
  # match plotOutput id from above
  # use theme_classic() to be consistent 
  
  output$csi_newspaper <- renderPlot({
    data <- csi_index %>% select(.data[[input$var]])
    ggplot(data, mapping = aes(x = .data[[input$var]])) +
      geom_bar() +
      theme_classic() + 
      labs(title = "Case Salience Index by Newspaper",
           x = "CSI",
           y = "Total Count of CSI")
  })
  
  # use renderPlot to create chart
  # match plotOutput id from above
  # use theme_classic() to be consistent 
  
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
      labs(title = "Mean vs. Median Supreme Court Case Salience Index by Year", 
           x = "Year", 
           y = "Mean vs. Median Case Salience Index")
    
  })
  
  # use render_gt to create chart
  # match gt_output from above
  
  output$regression_1_model <- render_gt({
    gt(regression_1) %>%
    tab_header(title = "Linear Regression of Supreme Court Case Salience Index",              
               subtitle = "The Effect of the Majority Opinion Writer and the 
               Majority Opinion Assigner on the Supreme Court Case 
               Salience Index") 
  
  })
  
  # use renderPlot to create chart
  # match plotOutput id from above
  # use theme_classic() to be consistent 
  
  output$regression_1_chart <- renderPlot({
    data <- regression_data
    ggplot(data, aes(x = CSI, y = test, group = 1)) +
      geom_point() + 
      geom_line() + 
      theme_classic() + 
      labs(title = "Linear Regression of Supreme Court Case Salience",
           x = "Median CSI", 
           y = "Test")
  
  })
  
}

    # Run the application -----

shinyApp(ui, server)
