### FIT5147 Data Visualisation Project - Visualisation Code 
# Author:         Armin Berger
# Date created:   19/10/2021


#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("leaflet")
#install.packages('fmsb')

library(fmsb)
library(readr)
library(dplyr)
library(ggplot2)
library(forcats)
library(leaflet)
library(shiny)
library(hrbrthemes)



### 1. Step: Load in the data 

# get current directory 
current_dir = getwd()
current_dir

# read in data as a df
indicator_data <- read.csv(paste(current_dir,'/indicator_data.csv', sep = ""))


# show head of the df
head(indicator_data)


# read in data as a df
budget_data <- read.csv(paste(current_dir,'/all_budget_clean.csv', sep = ""))

head(budget_data)



### 2. Step: Data processing functions for later plotting


# function that plots the circular bar plot
# REFERENCE: The plotting and data processing for this graph was taken from
# https://www.r-graph-gallery.com/297-circular-barplot-with-groups.html
# on the 24.10.2021
create_label_data <- function(year.input, country.input) {

# select the country and year that is desired 
data <- indicator_data %>% filter(year == year.input) %>% filter(Country.Name == country.input) %>% select('Indicator.Name', 'group', 'value') %>% rename(individual = Indicator.Name)
data$group <- as.factor(data$group)

# process the data so that it can be plotted
empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# code for the actual plotting
pl <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+10, label=individual, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) 

# return the graph
return(pl)
}


# function used for the radar plot
transpose_data <- function(data) {
  
  graph.df <- data.frame(t(data.frame(max(data$value), min(data$value), data$value)))
  names(graph.df) <- data$budget
  
  return(graph.df)
}






### 3. Step: Create Shiny Schema , both ui and server


## ELEMENET 
# Define UI for app that creates the visualizations

ui <- fluidPage(
  
  # give the page a title
  titlePanel(strong('Socio-Economic Effects & Fiscal Policy Responses of the Great Financial Crisis',
             style = 'font-family: Calibri;', align = 'center',  style="color:navy" )),
  
  
  navbarPage('',
             
             # VIS 1, create the overview page
             tabPanel('Overview', icon = icon("info-circle"),
                      
                      titlePanel(h2('Aims and Goals of the Visulisation',
                                    style = 'font-family: Calibri;', align = 'left')),
                      br(),
                      fluidRow(   # reading in the overview text and placing images
                                  column(5,includeHTML('overview_gfc.Rhtml')),
                                  column(4,
                                         img(src = "stock_1.png", height = 300, width = 300),img(src = "stock_2.png", height = 300, width = 300))
                              )
                      ),
             
             
             # creating a menu for all the indicators 
             navbarMenu('Socioeconomic Indicators', icon = icon("poll"),
                        
                        # VIS 2, create the visualizations for the big picture indicator overview
                        tabPanel('Big Picture Overview',
                                 titlePanel(h2('Comparison of Indicators over Time',
                                               style = 'font-family: Calibri;',align = 'left')),
                                 br(),
                                 fluidRow(
                                   column(4,includeHTML('indicator_big_pic.Rhtml')),
                                   column(5,
                                          selectInput("indicator_box2", "Select country:",
                                                      c("Germany", "United States", "South Africa", "India")),
                                          sliderInput("year_slider2", h5("Select year:"), value = 2000, min = 2000, max = 2015, sep =''), plotOutput('vis2'))
                                   
                                )
                                ),
                        

                        # VIS 3, create the visualizations for the individual indicators
                        tabPanel('Individual Indicators',
                                 titlePanel(h2('Individual Indicators over Time',
                                style = 'font-family: Calibri;',align = 'left')),
                                 br(),
                                fluidRow(
                                  column(3,includeHTML('indicator_individual.Rhtml')),
                                  column(5,  
                                         selectInput("indicator_box1", "Select indicator:",
                                                     c("Adolescent fertility rate (x/1000)",
                                                       "Consumer price index (2010 = 100)",        
                                                       "Health expenditure (% of GDP)",           
                                                       "Death rate (x/1000)",                      
                                                       "Exports as a capacity to import",          
                                                       "Final consumption expenditure (% of GDP)",
                                                       "FDI, net inflows (% of GDP)",              
                                                       "GDP growth (annual %)",                    
                                                       "Government final consumption (%)",        
                                                       "Gini index",                               
                                                       "Gross domestic savings (% of GDP)",        
                                                       "Income share of top 20%",                 
                                                       "Income share of bottom 20%",               
                                                       "Inflation (annual %)",                     
                                                       "Labor force participation rate (%)",      
                                                       "Life expectancy at brith (years)",         
                                                       "Market cap of domestic companies (%)",     
                                                       "Mortality rate under-5 (x/1000)",         
                                                       "Suicide mortality rate (x/100,000)",       
                                                       "Tax revenue (%)",                          
                                                       "Unemployment total (%)",                  
                                                       "Unemployment youth total (%)",             
                                                       "Vulnerable employment (%))",               
                                                       "Real interest rate (%)" )),
                                         
                                         sliderInput("year_slider1", h5("Select years:"), value = c(2000, 2010),min = 2000, max = 2015, sep ='')
                                         ,plotOutput('vis3')),
                                  column(1, textOutput('year_test'))
                                )
                                )
                        ),
             
             # VIS 4, create the visualizations for the government budget graphs
             tabPanel('Government Budgets', icon = icon("chart-pie"),
                      titlePanel(h2('Budget Developments over Time',
                                    style = 'font-family: Calibri;', align = 'left')),
                      br(),
                      fluidRow(
                        column(4,includeHTML('gov_budget.Rhtml')),
                        column(6, selectInput("indicator_box3", "Select country:",
                                             c("Germany", "United States of America", "South Africa", "India")),
                                  sliderInput("year_slider3", h5("Select year:"), value = 2000, min = 2000, max = 2012, sep =''),
                                  plotOutput('vis4')
                              
                              )
                      )
                      
             ),
             
             # create the last panel for the refrence area
             tabPanel('References', icon = icon("quote-right"),
                      br(),
                      fluidRow(
                        column(5,includeHTML('references.Rhtml'))
                      )
                      
             )
                      
            )
  
)



## ELEMENET II
# define server logic 

server <- function(input, output) {
  
  
  ## Plot 2, create circular bar plot
  
  # create a reactive variable that takes the input from the year slider
  year_react2 <- reactive(input$year_slider2)
  
  # create a reactive variable that takes the input from the 
  indicator_react2 <- reactive(input$indicator_box2)
  
  output$vis2 <- renderPlot(
    {create_label_data(year_react2(), indicator_react2())}
              )
  
  
  ## Plot 3, create the line graphs
  
  # create a reactive variable that takes the input from the year slider
  year_react1 <- reactive(c(input$year_slider1[1]: input$year_slider1[2]))
  
  # create a reactive variable that takes the input from the 
  indicator_react1 <- reactive(input$indicator_box1)
  
  output$vis3 <- renderPlot(
    {indicator_data %>% filter(Indicator.Name == indicator_react1()) %>% 
        filter(year %in% year_react1()) %>% ggplot(aes(x = year, y = value, color = Country.Name))+
        geom_smooth(se =F)+ 
        facet_wrap(~Indicator.Name, nrow = 3 , scales = "free", strip.position = "top") + theme(legend.title=element_text(face="bold"))}
  )
  
  
  
  ## Plot 4, create the radar charts
  
  # create a reactive variable that takes the input from the year slider
  year_react3 <- reactive(input$year_slider3)
  
  # create a reactive variable that takes the input from the 
  indicator_react3 <- reactive(input$indicator_box3)
  
  output$vis4 <- renderPlot({radarchart(transpose_data(budget_data %>%  filter(country == indicator_react3()) %>% filter(year == year_react3()) ))})
  
}


# run the actual app
shinyApp(ui = ui, server = server)




