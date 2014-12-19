if ("shiny" %in% row.names(installed.packages())  == FALSE) 
{install.packages("shiny")}
if ("zoo" %in% row.names(installed.packages())  == FALSE) 
{install.packages("zoo")}
if ("lattice" %in% row.names(installed.packages())  == FALSE) 
{install.packages("lattice")}
library(shiny)
library(zoo)
library(lattice)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  # Application title.
  titlePanel("Bootstrapping default probability curves"),

  h4("Input CDS Spread"),
  
  fluidRow(column(6,
  numericInput(inputId="s1",
               label="1 year",value=0.0480),
  numericInput(inputId="s2",
               label="2 year",value=0.0550),
  numericInput(inputId="s3",
               label="3 year",value=0.0600),
  
  h4("Input Risk Free Rate"),
  
  numericInput(inputId="rf",
               label="Risk Free Rate",value=0.05)
                  ),
            column(6,
  numericInput(inputId="s4",
               label="5 year",value=0.0750),
  numericInput(inputId="s5",
               label="7 year",value=0.0810),
  numericInput(inputId="s6",
               label="10 year",value=0.0896),
  
  h4("Input Recovery Rate"),
  
  numericInput(inputId="R",
               label="Recovery Rate",value=0.3)
                  )
            ),
  
  h4("Input Alpha"),
  
  numericInput(inputId="alpha",
               label="Alpha",value=1),
  
  actionButton("run","Start Bootstrapping"),
  
  
  # Show a summary of the dataset and an HTML table with the
  # requested number of observations. Note the use of the h4
  # function to provide an additional header above each output
  # section.
  
    
  h3(textOutput("text1")),
  fluidRow(column(4,
                  tableOutput("method1")),
           column(4,
                  plotOutput("plot1",width = 800, height=600)
                  )),
  
  h3(textOutput("text2")),
  fluidRow(column(4,
                  tableOutput("method2")),
           column(4,
                  plotOutput("plot2",width = 800, height=600)
  ))
  
  
  #plotOutput("ccarplot1",width=800,height=600),
  #plotOutput("ccarplot2",width=800,height=600)
  #verbatimTextOutput("summary"),
  
  #h4("Observations"),
  #tableOutput("view")
  
)
  )
