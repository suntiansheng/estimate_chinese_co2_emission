library(shiny)
library(ggplot2)

mydate <- read.csv('engeldate.csv')
names(mydate) <- c('x' , 'income' , 'co2' , 'area')
mydate$area <- factor(mydate$area)



ui <- fluidPage(
  titlePanel("Environment Engel Curve"),
  
  sidebarLayout(
    sidebarPanel(
      h4(helpText("Environment Engel curves(EEcs) plot relationship between households' incomes and the
               pollution embodied in the goods and services they consume")),
      br(),
      strong(helpText("You can select three types of method that can show defference curves")),
      selectInput('select' , 'select',
                  choices = c('lm',"loess", "glm", "gam"),
                  selected = 'lm')
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
)

server <- function(input , output){
  
  p<- reactive({
    ggplot(aes(x = income , y = co2,color = mydate$area),data = mydate) + 
      geom_point() +
      geom_smooth(method = input$select)
  })
  output$plot1 <- renderPlot({p()})
}

shinyApp(ui , server)