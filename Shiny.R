library(shiny)

ui <- fluidPage(
  #Titulo principal
  titlePanel("Meu primeiro shiny app!"),
  
  # Barra lateral com as definições do input e do output.
  sidebarLayout(
    
    # Barra lateral para os inputs.
    sidebarPanel(
      
      # Input: número de classes do histograma.
      sliderInput(inputId = "classes",
                  label = "Número de classes:",
                  min = 1,
                  max = 30,
                  value = 10)
    ),
    
    # Painel principal para mostrar os outputs.
    mainPanel(
      plotOutput(outputId = "hist")
    )
  )
  
  
  )

server <- function(input,output){
  output$hist <- renderPlot(
    {x <- mtcars$mpg
    bins <- seq(min(x),max(x),length.out = input$classes +1)
    
    hist(x,breaks = bins,col='#151515' , border='white',
         xlab='Milhas por galão', main='Histograma do número de milhas rodadas por galão de combustivel')
    }
  )
  
}

shinyApp(ui=ui,server=server)

