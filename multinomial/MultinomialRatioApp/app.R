#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Multinomial Ratio Exploration"),
   sidebarLayout(
   mainPanel(
     fluidRow(
       column(width=3,
              sliderInput("p", "Numerator(p)", min = 0, max = 1, value = 0.4,step=0.05)),
       column(width=3,
              sliderInput("q", "Denominator(q)", min = 0, max = 1, value = 0.4,step=0.05)),
       column(width=3,
              sliderInput("ul", "Upper limit", min = 0.5, max = 10, value = 2,step=0.5))
     ),
     fluidRow(
       column(width=3,
              sliderInput("n", "Object count(n)", min = 10, max = 1000, value = 100,step=10)),
       column(width=3,
              sliderInput("pt","Point Size",min=0,max=2,value=1,step=0.1))
     ),
     plotOutput("density2",height="700px",width="600px")
   ),
   sidebarPanel(
    tags$h3("Information:"),
    tags$p("This app calculates the probability distribution of multinomial ratios in a trinomial distribution."),
    tags$p("The number of objects to be categorized is determined by the Object count slider."),
    tags$p("The probability an object falls in the numerator category, p,  is determined by the numerator slider."),
    tags$p("The probability an object falls in the denominator category, q,  is determined by the denominator slider."),
    tags$p("The remaining probability is automatically assigned to the third, *other* category."),
    tags$p("NOTE 1: The program does not ensure that p+q <= 1"),
    tags$p("NOTE 2: Infinity (defined as n/0) is a legal ratio.  As is 0/0.  Consequently the mean and the variance are not well-defined for this distribution"),
    tags$p("NOTE 3: Ratio values can include all integers up to n.")
   ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$density2=renderPlot({
    source("make.table.R")
    n=input$n
    ul=input$ul
    tmp<-make.table(n,input$p,input$q)
    scale=attr(tmp$index,"scale")
    t2<-aggregate(c(tmp$count),by=list(c(tmp$index)),sum)
    drop=-1*c(nrow(t2)) #Drop Infinity
    x=t2[drop,1]/scale
    #theta=atan2(t2[drop,1],scale)
    p=t2[drop,2]
    ev=sum(x*p)
    v=sum(p*(x-ev)^2)
    ev.txt=round(ev,3)
    sd.txt=round(sqrt(v),3)
    ratio.txt=as.character(round(input$p/input$q,3))
    msg=paste0("true ratio: ",ratio.txt)
    par(mfrow=c(2,1))
    plot(x,p,xlim=c(0,input$ul),xlab="Observed Ratio",sub=msg,main=paste0("Probability Density n=",input$n," (p=",input$p," , q=",input$q,")"),ylab="Probability",cex=input$pt)
    abline(v=input$p/input$q)
    plot(x,cumsum(p),xlim=c(0,input$ul),main="Cumulative Distribution",ylab="Cumulative Probability",xlab="Observed Ratio",cex=input$pt)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

