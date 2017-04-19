library(shiny)
library(shinythemes)
ui <- fluidPage(theme=shinytheme("flatly"),
  titlePanel("P-curves"),
  sidebarLayout(
    sidebarPanel(numericInput("N", "Subject per group:", 50, min = 1, max = 1000),
                 sliderInput("d", "Cohen's d effect size:", min = 0, max = 3, value = 0.5, step= 0.01),
                 sliderInput("p_upper", "P-value (upper limit):", min = 0, max = 1, value = 0.05, step= 0.01),
                 uiOutput("p_low")
    ),
    mainPanel(
      h4(textOutput("pow")),
      tabsetPanel(
      tabPanel("Plot",
               plotOutput("pdf"),
               plotOutput("cdf")
      )
    )
    )
  )
)
server <- function(input, output) {
  output$pdf <- renderPlot({
    N<-input$N
    d<-input$d
    p<-0.05
    p_upper<-input$p_upper+0.00000000000001
    p_lower<-input$p_lower+0.00000000000001
#    if(p_lower==0){p_lower<-0.0000000000001}
    ymax<-25 #Maximum value y-scale (only for p-curve)
    
    #Calculations
    se<-sqrt(2/N) #standard error
    ncp<-(d*sqrt(N/2)) #Calculate non-centrality parameter d
    
    #p-value function
    pdf2_t <- function(p) 0.5 * dt(qt(p/2,2*N-2,0),2*N-2,ncp)/dt(qt(p/2,2*N-2,0),2*N-2,0) + dt(qt(1-p/2,2*N-2,0),2*N-2,ncp)/dt(qt(1-p/2,2*N-2,0),2*N-2,0)
    par(bg = "aliceblue")
    plot(-10,xlab="P-value", ylab="density of p-values", axes=FALSE,
         main=paste("P-curve"), xlim=c(0,1),  ylim=c(0, ymax))
    abline(v = seq(0,1,0.1), h = seq(0,ymax,5), col = "lightgray", lty = 1)
    axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
    axis(side=2)
    cord.x <- c(p_lower,seq(p_lower,p_upper,0.001),p_upper) 
    cord.y <- c(0,pdf2_t(seq(p_lower, p_upper, 0.001)),0)
    polygon(cord.x,cord.y,col=rgb(1, 0, 0,0.5))
    curve(pdf2_t, 0, 1, n=1000, col="black", lwd=2, add=TRUE)
    p_u<-1 + pt(qt(p_upper/2,2*N-2,0),2*N-2,ncp) - pt(qt(1-p_upper/2,2*N-2,0),2*N-2,ncp) #two-tailed
    p_l<-1 + pt(qt(p_lower/2,2*N-2,0),2*N-2,ncp) - pt(qt(1-p_lower/2,2*N-2,0),2*N-2,ncp) #two-tailed
  })
  output$cdf <- renderPlot({
    N<-input$N
    d<-input$d
    p_upper<-input$p_upper
    p_lower<-input$p_lower
    ymax<-25 #Maximum value y-scale (only for p-curve)
    
    #Calculations
    se<-sqrt(2/N) #standard error
    ncp<-(input$d*sqrt(N/2)) #Calculate non-centrality parameter d
    
    cdf2_t<-function(p) 1 + pt(qt(p/2,2*N-2,0),2*N-2,ncp) - pt(qt(1-p/2,2*N-2,0),2*N-2,ncp)
  
    par(bg = "aliceblue")
    plot(-10,xlab="P-value", ylab="density of p-values", axes=FALSE,
         main=paste("Cumulative P-curve"), xlim=c(0,1),  ylim=c(0, 1))
    abline(v = seq(0,1,0.1), h = seq(0,1,0.1), col = "lightgray", lty = 1)
    axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
    axis(side=2)
    cord.x <- c(p_lower,seq(p_lower,p_upper,0.001),p_upper) 
    cord.y <- c(0,cdf2_t(seq(p_lower, p_upper, 0.001)),0)
    polygon(cord.x,cord.y,col=rgb(1, 0, 0,0.5))
    curve(cdf2_t, 0, 1, n=1000, col="black", lwd=2, add=TRUE)
  })
  # make dynamic slider 
  output$p_low <- renderUI({
    sliderInput("p_lower", "P-value (lower limit):", min = 0, max = input$p_upper, value = 0, step= 0.01)
  })
  output$pow <- renderText({
    N<-input$N
    d<-input$d
    ncp<-(input$d*sqrt(N/2)) #Calculate non-centrality parameter d
    paste("Statistical Power:",round((1 + pt(qt(input$p_upper/2,2*N-2,0),2*N-2,ncp) - pt(qt(1-input$p_upper/2,2*N-2,0),2*N-2,ncp)),digits=2),".")
  })
}
shinyApp(ui = ui, server = server)