# Load libraries needed
library(shiny)
library(ggplot2)
library(purrr)
library(rootSolve)
library(readr)
library(plotly)
source("pieceFunctions.R")




spruce.df <- read_csv("SPRUCE.csv")
d = spruce.df$BHDiameter



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Spruce Data Set: Piecewise Regression"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("xk",
                  "Choose knot:",
                  min = min(d),
                  max = max(d),
                  value = 15,
                  step=0.01),

        sliderInput("xk2",
                    "Choose knot:",
                    min = min(d),
                    max = max(d),
                    value = 18,
                    step=0.01),
      
      sliderInput("xk_int",
                  "choose L and U for the first knot:",
                  min = min(d),
                  max = max(d),
                  value = c(9,14),
                  step=0.01),
      
      sliderInput("xk2_int",
                  "choose L and U for the second knot:",
                  min = min(d),
                  max = max(d),
                  value = c(16,22),
                  step=0.01),
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("regressPlot"),
      plotlyOutput("R2"),
      tableOutput("root"),

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Plot that shows curve, interactive points, and points which give the max R value. 
  #Also displays current R value
  output$regressPlot <- renderPlot({
    plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
    

    tmp<- spruce.df %>%
      mutate(X=(BHDiameter-input$xk)*(BHDiameter>input$xk))%>%
      mutate(X2=(BHDiameter-input$xk2)*(BHDiameter>input$xk2)) %>%
      lm(Height~BHDiameter + X + X2, data=.)%>%
      summary()
    
    
    curve(twoKnotReg(x,xk=input$xk, xk2=input$xk2, coef=tmp$coefficients[,"Estimate"] ),
        add=TRUE,
       lwd=2,
      col="Blue")
    
     points(input$xk,twoKnotReg(input$xk,input$xk, input$xk2, coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2)
     points(input$xk2,twoKnotReg(input$xk2,input$xk, input$xk2, coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2)
     
     points(maxR2$par[1],twoKnotReg(maxR2$par[1],maxR2$par[1], maxR2$par[2],coef=maxEst ),col="black",pch=21,bg="purple",cex=2)
     points(maxR2$par[2],twoKnotReg(maxR2$par[2],maxR2$par[1], maxR2$par[2],coef=maxEst ),col="black",pch=21,bg="purple",cex=2)
     
     
     text((input$xk+input$xk2)/2,14,
          paste("R sq.=",round(tmp$r.squared,4) ))
     
   }) 
  
  
  maxR2=optim(c(5,22), rsq, data=spruce.df, control=c(fnscale=-1))
  maxEst=coeff(maxR2$par[1], maxR2$par[2],spruce.df)

# Interactive 3d plot of knot1, knot2, and R2
  output$R2 <- renderPlotly({
    plot_ly(grid(input$xk_int, input$xk2_int,spruce.df), x=~x, y=~y, z=~r2, type="scatter3d", mode="markers", size=0.3,
            color=~r2)

  })

# Table gives values of the knots where R2 is maximized. It also gives max R2.
  output$root<-renderTable({
    s=table(Knot_1=maxR2$par[1], Knot_2=maxR2$par[2], Max_R2_Value=rsq(maxR2$par, spruce.df))
    s
  })
  
  #Second knot depends on the first
  observe({
  xk2min<-input$xk+0.01
  updateSliderInput(session, "xk2", min=xk2min)
  })

# Second interval depends on the first 
observe({
  xk2IntMin<-input$xk_int[2]
  updateSliderInput(session, "xk2_int", min=xk2IntMin)
})
}

# Run the application 
shinyApp(ui = ui, server = server)

