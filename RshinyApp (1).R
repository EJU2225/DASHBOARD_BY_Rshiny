
library(ggplot2)
library(shiny)
library(dplyr)

ui <- basicPage(
  h1("Summarize Country Wise"),
  selectInput(inputId = "sel_country",
              label="Choose Country","Names"),
              plotOutput("plot")
)
server <- function(input, output,session) {
    data<-reactive({
      req(input$sel_country)
      df10 <- df4 %>% filter(df4$Country %in% input$sel_country) %>% group_by(year) %>% summarize(number = sum(number))
    })
    
    observe({
      updateSelectInput(session,"sel_country",choices = df4$Country)
    })
    output$plot<-renderPlot({
      ggplot(data(),aes(year,number))+
      geom_bar(stat="identity",color="black",fill="cyan")+
      theme(legend.position = "none")
    })
}
shinyApp(ui = ui, server = server)
