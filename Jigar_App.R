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
View(FinalData)

#Select 60 Random scripts and save it in df

df <- head(FinalData, 60)
#View(df)

#Extract prices and shp 
colnames(FinalData)
vecs <- c()
for (column in colnames(FinalData)){
  if (grepl('no.of.shareholder.below.1.lakh.nominal', column) == TRUE){
    vecs <- append(vecs, column)
  }
}
vecs <- append(vecs, c("Index","name.of.the.scrip"))
Prices <- df[,1:32]
shp <- df[,vecs]
#View(Prices)
#View(shp)

#flip
Premium_price <- Prices[,order(ncol(Prices):1)]
shp_premium <- shp[,order(ncol(shp):1)]
View(Premium_price)
View(shp_premium)

#check for na values 
sum(is.na(Premium_price))
sum(is.na(shp_premium))

#store indices with na values 
na_rows <- as.numeric(rownames(shp_premium[rowSums(is.na(shp_premium)) > 0,]))
na_rows
#remove those rwos from both the sets 
shp_premium <- shp_premium[-na_rows,]
sum(is.na(shp_premium))
Premium_price <- Premium_price[-na_rows,]
sum(is.na(Premium_price))


#plotting
install.packages("scales")
plotter <- function(company_name) {
  library(scales)
  #create x index values 
  x <- 1:(ncol(Premium_price)-2)
  
  #create price vector for the company 
  qc_price <- unlist(unname(Premium_price[Premium_price$name.of.the.scrip == company_name, 1: (ncol(Premium_price)-2)]))
  qc_price <- as.numeric(gsub(",","",qc_price))
  qc_price
  
  #create shp vector for the company 
  shp_v <- unlist(unname(shp_premium [shp_premium$name.of.the.scrip == company_name , 3: (ncol(shp_premium))]))
  shp_v <- as.numeric(gsub(",","",shp_v))
  shp_v 
  
  #rescale
  rescaled <- rescale(shp_v, to = c(min(qc_price),max(qc_price)))
  
  #plot
  plot(x,qc_price, type = 'l', col = 'blue', lwd = 3, lend = 2, xaxt = 'n', ylab = '', main = company_name, las = 2)
  lines(x, rescaled, type = 'l', col = 'red', lwd = 3, lend = 2)
  axis(1, at = 1:(ncol(Premium_price)-1), labels= colnames(Premium_price)[-ncol(Premium_price)], las = 2)
  legend("bottomright", legend = c("price",'shareholder pattern'),
         lwd = "3", bty = "n",
         col = c('blue','red'))
}

plotter('ABB India Ltd.')
company_names <- Premium_price$name.of.the.scrip
for (i in company_names){
  plotter(i)
}

#Hypothesis 

library(ggpmisc)
library(splus2R)

range <- 1: (ncol(Premium_price)-2)
for(column in range){
  Premium_price[,column] <-  as.numeric(gsub(",","",Premium_price[,column]))
}

range <- 3: (ncol(shp_premium))
for(column in range){
  shp_premium[,column] <-  as.numeric(gsub(",","",shp_premium[,column]))
}



library(ggpmisc)
library(splus2R)
hypo <- function(company_name){
  #create price vector for the company 
  Price <- unlist(unname(Premium_price[Premium_price$name.of.the.scrip == company_name , 1: (ncol(Premium_price)-2)]))
  
  #create shp vector for the company 
  Shp <- unlist(unname(shp_premium [shp_premium$name.of.the.scrip == company_name , 3: (ncol(shp_premium))]))
  
  Quarter <- colnames(Premium_price)[-c(ncol(Premium_price),ncol(Premium_price)-1)]
  
  Peaks <- peaks(Shp,span = 3, strict = TRUE)
  
  df <- data.frame(Quarter,Price,Shp,Peaks)
  my_range <- 1: (nrow(df) - 4)
  count <- 0
  begin <- c()
  end <- c()
  return <- c()
  shareholder_change <- c()
  for (i in my_range){
    if (df$Peaks[i] == TRUE){
      #     sub_df <- df[i:(i+4),]
      one_year_return <- ((df$Price[i+4] - df$Price[i])/df$Price[i])* 100
      Shareholder_change <- df$Shp[i+4] - df$Shp[i]
      #y <- paste("During the quarters", df$Quarter[i],"and",df$Quarter[i+4],"Return :", one_year_return , "%","Shareholder change", Shareholder_change , sep = " ")
      #results <- append(results,y)
      begin <- append(begin,df$Quarter[i])
      end <- append(end, df$Quarter[i+4])
      return <- append(return, one_year_return)
      shareholder_change <- append(shareholder_change,Shareholder_change)
      if (Shareholder_change < 0 & one_year_return > 0){
        count = count + 1
      }
    }
  }
  results <- data.frame(begin,end,return,shareholder_change)
  names(results) <- c('From','To','Return(%)', 'Shareholder Change')
  return(results)
}

datatable <- function(company_name){
  #create price vector for the company 
  Price <- unlist(unname(Premium_price[Premium_price$name.of.the.scrip == company_name , 1: (ncol(Premium_price)-2)]))
  
  #create shp vector for the company 
  Shp <- unlist(unname(shp_premium [shp_premium$name.of.the.scrip == company_name , 3: (ncol(shp_premium))]))
  
  Quarter <- colnames(Premium_price)[-c(ncol(Premium_price),ncol(Premium_price)-1)]
  
  df <- data.frame(Quarter,Price,Shp)
  return(df)
}









#UI 
ui <- fluidPage(titlePanel("Shareholder change and return"),
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
                

server <- function(input, output){
  output$plot <- renderPlot({
    plotter(input$company)
  })
  
  datat <- reactive({datatable(input$company)})
  output$df <- renderTable(datat())
  
  results <- reactive({hypo(input$company)})
  output$results <- renderTable(results())
}

shinyApp(ui = ui, server = server)





