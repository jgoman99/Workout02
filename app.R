library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

future_value <- function(amount = 100, rate = 0.05, years = 1)
{
  return(amount*(1+rate)^years)
}

#' @title Annuity
#' @desc Compounds Interest with annuity
#' @param contrib contribution value (numeric)
#' @param rate the interest rate (numeric)
#' @param years the number of years to be evaluated (numeric)
#' @return returns numeric constant value
#' @title Annuity
#' @desc Compounds Interest with annuity
#' @param contrib contribution value (numeric)
#' @param rate the interest rate (numeric)
#' @param years the number of years to be evaluated (numeric)
#' @return returns numeric constant value
annuity <- function(contrib,rate,years)
{
  contrib*(((1+rate)^years-1)/rate)
}

#' @title Growing Annuity
#' @desc Compounds Interest with growing annuity
#' @param contrib contrib value (numeric)
#' @param rate the interest rate (numeric)
#' @param years the number of years to be evaluated (numeric)
#' @param growth the amount the contrib grows by
#' @return returns numeric constant value
growing_annuity <- function(contrib, rate, growth, years)
{
  contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Saving Over Time"),
  fluidRow(
    column(4,
           
    sliderInput("amount","Initial Amount",min=0,max=100000,step = 500,value=1000),
    sliderInput("contribution","Annual Contribution",min=0,max=50000,step = 500,value=2000)
    ),
    column(4,
           
           sliderInput("rate","Return Rate (in %)",min=0,max=20,step = .1,value=5),
           sliderInput("growth","Growth Rate (in %)",min=0,max=20,step = .1,value=2)
    ),
    column(4,
           
           sliderInput("years","Years",min=1,max=50,step = 1,value=20),
           selectInput("facet","Facet?",choices = c("no","yes"))
    )
  ),
  fluidRow(
    headerPanel(h3("Timeline")),
    mainPanel(plotOutput("distPlot"))
  ),
  fluidRow(

    headerPanel(h3("Balances")),
    verbatimTextOutput("myText",placeholder = FALSE)
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   dat <- reactive({
     no_contrib <- input$amount
     fixed_contrib <- input$amount
     growing_contrib <- input$amount
     for (i in 2:(input$years+1))
     {
       no_contrib[i] <- future_value(input$amount,(input$rate)/100,i-1)
       fixed_contrib[i] <- annuity(input$contribution,(input$rate)/100,i-1) + no_contrib[i]
       growing_contrib[i] <- growing_annuity(input$contribution,(input$rate)/100,(input$growth/100),i-1)+ no_contrib[i]
       
     }
     myData <- data.frame(1:(input$years+1),no_contrib,fixed_contrib,growing_contrib)
     names(myData) <- c("Year","no_contrib","fixed_contrib","growing_contrib")
     return(myData)
   })
   output$myText <- renderPrint(dat())
   
   output$distPlot <- renderPlot({
      myData <- gather(dat(),key="type","amount",2:4)
      myData$type = factor(myData$type, levels=c("no_contrib","fixed_contrib","growing_contrib"))
      
        if (input$facet=="yes")
        {
          ggplot(myData,aes(x=Year,y=amount, color=type)) + geom_line() + facet_wrap(~type) +
            geom_area(aes(fill=type),alpha=.4) + labs(title="Three modes of investing", x="Year", y = "Value") + geom_point()
        }
      else
      {
        ggplot(myData,aes(x=Year,y=amount, color=type)) + geom_line() + labs(title="Three modes of investing", x="Year", y = "Value") +
          geom_point()
      }

   })

}

# Run the application 
shinyApp(ui = ui, server = server)

