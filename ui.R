# load libraries
library(shiny)
library(plotly)
library(colourpicker)
library(RMySQL)
library(shinythemes)

# Define UI for application that draws the BMI chart and adds datapoints according to user input
shinyUI(fluidPage(theme = shinytheme("yeti"),
  fluidRow(
    column(width=4,
           tags$br(),
           tags$ul(h1(tags$strong(tags$span(style="color:black", "BMI Age Percentile Chart"), sep="")),
                   tags$blockquote(p("BMI is a figure which represents a relationship between height and weight.
                      The height and weight measurements are combined to give a figure which lets us know if the child is the 
                      right weight for their height and age.
                      For the best health, children need to be within a certain weight range for their height, age and gender, 
                      BMI tells us whether they are within this range")),
                   h3("What does 'percentile' mean?"),
                   tags$blockquote(p("Percentiles compare your child with other children of the same age. 
                                     Higher percentages indicate a larger or taller child. Lower percentages indicate 
                                     a smaller or shorter child."),
                                   tags$p("For example, if your daughter is on the 75th percentile for weight, 
                                          she weighs the same or more than 75 per cent of girls her age 
                                          (and less than 25 per cent)."),
                                   tags$i("For more information, visit", 
                                          tags$a(href="http://www.education.vic.gov.au/childhood/parents/mch/Pages/charts.aspx", 
                                                "Victoria -- Education and Training"))),
                    h3("Meaning of each percentile"),
                    tags$blockquote(tags$ul(p(tags$li("Below the 5th percentile -- Underweight"),
                                            tags$li("Between the 5th and 85th percentile -- Normal"),
                                            tags$li("On or above the 85th percentile but below the 95 percentile -- Overweight"),
                                            tags$li("On or above the 95 percentile -- Obese")))))
           ),
    column(width=4,
           plotlyOutput("chart", width = "auto", height="auto")),
    column(width=4,
           tags$br(), tags$br(),
           h3(htmlOutput("text", container=span)))
    )
))
