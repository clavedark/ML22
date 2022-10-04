################
#shiny app to illustrate intercept and derivative shifts in cdf
#dc - 9.25.22
################
library(shiny)
library(ggplot2)

# Define UI for application that draws a pdf/cdf
ui <- fluidPage(

    # Application title
    titlePanel("Intercept shifts and Multiplicative Interactions"),

    # Sidebar with a slider input for intercept and multiplier
    sidebarLayout(
        sidebarPanel(
            sliderInput("intercept",
                        "Intercept shift:",
                        min = -4.00,
                        max = 4.00,
                        value = 0),
            sliderInput("interaction",
                        "multiplier:",
                        min = -4.00,
                        max = 4.00,
                        value = 1)
        ),

        # Show plots of the generated distributions
        mainPanel(
           plotOutput("distPlot1")
    ))
)


# Define server logic required to draw distributions
server <- function(input, output) {
    output$distPlot1 <- renderPlot({
        # generate parameters based on input$  from ui.R
        x    <- runif(1000, min = -4, max = 4)
        mean <-  input$intercept
        interaction <- input$interaction
        ncdf <- pnorm((x+mean)*interaction, mean = 0, sd = 1)
        plotdata1 <- data.frame(x, ncdf)
        # draw the cdf with the specified mean/sd
        ggplot()+ 
          geom_line(data = plotdata1, aes(x=x, y=ncdf), color="gray20", lty=2)+
          labs(x="xB", y="Probability(z)") 
    })
    
    # output$distPlot2 <- renderPlot({
    #   # generate parameters based on input$  from ui.R
    #   x    <- runif(1000, min = -4, max = 4)
    #   mean <-  input$mean
    #   sd <- input$sd
    #   npdf <- dnorm(x, mean = mean, sd = sd)
    #   lpdf <- dlogis(x, location=mean, scale=sd)
    #   plotdata2 <- data.frame(x, npdf, lpdf)
    #   # draw the pdf with the specified mean/sd
    #   ggplot()+ geom_line(data = plotdata2, aes(x=x, y=lpdf, lty=1), color="gray20", lty=1)+
    #     geom_line(data = plotdata2, aes(x=x, y=npdf), color="gray20", lty=2)+
    #     labs(x="z", y="Probability(z)") 
    # 
    #   
    # })
}


# Run the application 
shinyApp(ui = ui, server = server)
