library(shiny)

ui <- fluidPage(
    title = "Practise math", 
    withMathJax(),
    h4("English"),
    uiOutput('problem'),
    
    hr(),
    
    h4("Enter your answer"),
    textInput('answer_x', label = 'Enter what df / dx is', width = '100%'),
    actionButton("go", "That is my answer!"),
    
    hr(),
    
    uiOutput('feedback')
)

shinyUI(ui)
