library(shiny)


shinyUI(
pageWithSidebar(
  headerPanel("Swift key Next Word Prediction"),
  sidebarPanel(
textInput("phrase", label = h3("Enter your text here:"),value = ),
submitButton('Submit')
),
mainPanel(
h3('How to use this app:'),
h4('This app predicts the next word Per given user enterd text.'),
h4('In order to predict the next word please enter at least one  '),
h4('word in the requested text on the left panel and press the '), 
h4('submit button.  The predicted next word will appear below'), 
h4('----------------------------------------------------------'),
h4('Text Entered:'),
verbatimTextOutput("ophrase"),
h4('Which resulted in prediction of the next word'),
verbatimTextOutput("onextword")
)
)
)

                      