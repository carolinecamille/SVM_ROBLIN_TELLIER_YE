#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(navbarPage(title="SUPPORT VECTOR MACHINE",
  
  # Application title
  tabPanel("Data presentation",
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton(inputId="click",label="NOTICE")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      dataTableOutput("dim"),
      dataTableOutput("name"),
      dataTableOutput("sum")
      
    )
  )
),
tabPanel("Description of SVM",
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId="kernel1",label="choose the kernel type",choices=c('linear','polynomial','radial basis','sigmoid'),multiple = F,selected = 'linear'),
             numericInput(inputId='degree1', label='degree',value=3,min=0),
             numericInput(inputId='coef01', label='b',value=0,min=0),
             sliderInput(inputId='c1', label='C',min=1,max=100,value=50,step=1),
             actionButton("submit1" ,"submit", icon("refresh"))
           ),
           mainPanel(
            plotOutput('plot1'),
            textOutput('explication1'),
            plotOutput('plot2'),
            textOutput('explication2')
           )
         )
)
,
  tabPanel("SVM",
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="kernel",label="choose the kernel type",choices=c('linear','polynomial','radial basis','sigmoid'),multiple = F,selected = 'linear'),
      numericInput(inputId='degree', label='degree',value=3,min=0),
      numericInput(inputId='coef0', label='b',value=0,min=0),
      sliderInput(inputId='c', label='C',min=1,max=100,value=50,step=1),
      actionButton("submit" ,"submit", icon("refresh"),)
    ),
    mainPanel(
      dataTableOutput("tablesvm"),
      textOutput('text1'),
      textOutput("predsvm")
    )
  )
           )
,
tabPanel("Comparison",
         sidebarLayout(
           sidebarPanel(
             selectInput(inputId='Model', label='choose the model', choices=c('logistic regression','linear discriminant analysis','quadratic discriminant analysis','tree') ,multiple = F, selected='logistic regression')
           ),
           mainPanel(
             dataTableOutput("table"),
             textOutput("text"),
             textOutput("pred"),
             textOutput("conclusion")
           )
         )
)
)))