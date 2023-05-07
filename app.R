# devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
library(DT)
library(devtools)

options(timeout=600)
library(devtools)
devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')


devtools::install_github('https://github.com/catboost/catboost/releases/download/v1.2/catboost-R-Windows-1.2.tgz')



# devtools::install_github('https://github.com/catboost/catboost/releases/download/v1.2/catboost-R-Windows-1.2.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
library(catboost)
library(shinythemes)
library(shiny)

setwd("C:/Koweps")
myModel <- readRDS("catmodel.rda")

# User Interface

ui <- fluidPage(theme = shinytheme("yeti"),
                
                # page header
                headerPanel("Suicidal Ideation Predictions"),
                
                # input values
                sidebarPanel(
                  HTML("<h5>Input the test scores</h5>")
                  ),
                
                mainPanel(
                  numericInput("cesd", "CES-D score", 0),
                  numericInput("selfes", "Self-Esteem score", 0),
                  # action button
                  actionButton(inputId = "go", 
                  label = "Get Prediction"),
                  htmlOutput ("pred"),
                  htmlOutput ("feedback"))
                )

                

server <- shinyServer(function(input, output) {
  
  ee <- eventReactive(input$go, {
    sample.obs <- cbind(input$cesd,input$selfes)
    colnames(sample.obs) <- c('cesd','selfes')
    cat.cols = c('cesd','selfes')
    sample.obs <- data.frame(sample.obs)
    sample.obs <- transform(
      sample.obs,
      cesd=as.numeric(cesd),
      selfes=as.numeric(selfes)
      )
    real_pool <- catboost.load_pool(sample.obs)
    round(catboost.predict(myModel, real_pool, prediction_type = "Probability"), digits = 2)
  })
  
  observeEvent(input$go,output$pred <- renderUI({
    #catboost.predict(myModel, newdata=ee(),type='prob')[,1]
    str1 <- paste("<br>","<b>","Based on your current reports, you have a ", ee()*100,"% probability of having a suicidal ideaion.",sep="","<b>")
    HTML(paste(str1))
  }))
  
  output$feedback <- renderUI({
    if (ee()>0.5){
      "We recommend you to visit suicide prediction agents"
    } else{
      "It seems like you are fit and have lower chances of having a suicidal ideation."
    }
  })
})
  
# Run the application 
shinyApp(ui = ui, server = server)

