# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
#install.packages("scales")
library(scales)
library(ggpmisc)
library(splus2R)

# Load data
FinalData <- read.csv("FinalData.csv")
#Select 60 Random scripts and save it in df

df <- head(FinalData, 60)

fluidPage(titlePanel("Shareholder change and return"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "company", label = strong('Select Company'),
                                choices = df$name.of.the.scrip,
                                selected = "Choose a company"
                    )
                  ),
                  
                  mainPanel(
                    plotOutput(outputId = "plot", height = "300px"),
                    tableOutput('df'),
                    tableOutput('results')
                  )
                )
)


