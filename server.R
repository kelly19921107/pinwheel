# load libraries
library(shiny)
library(plotly)
library(colourpicker)
library(RMySQL)
library(shinyjs)

# database config
options(mysql=list(
  "host"="35.189.11.78",
  "port"=3306,
  "user"="root",
  "password"="1kikJQpS0m33"
))

# database name
databaseName <- "child_obesity"
# 
# # table names
# kid_table <- "kid"

# function to read in tables from MySQL
fetch_table <- function(tablename, fetch_kid_data=FALSE, kid_id=NULL) {
  db <- dbConnect(MySQL(), dbname=databaseName, host=options()$mysql$host,
                  port=options()$mysql$port, user=options()$mysql$user,
                  password=options()$mysql$password)
  if (fetch_kid_data==FALSE){
    query <- sprintf("SELECT * FROM %s", 
                     tablename)
  }
  else {
    query <- sprintf("SELECT dob, gender, weight, height, input_date
                     FROM kid, bmirecord
                     WHERE kid.id=%s
                     AND kid.id=bmirecord.kid_id
                     ORDER BY input_date DESC
                     LIMIT 1;",
                     kid_id)
  }
  data <- dbGetQuery(db, query)
  
  dbDisconnect(db)
  
  return(data)
}

# read in tables
boy_bmi <- fetch_table("cleaned_boy_bmi_v2")
girl_bmi <- fetch_table("cleaned_girl_bmi_v2")


# default
BMI <<- boy_bmi

# title <- "BMI Age Percentiles for Boys"
# kid_id <- 0

shinyServer(function(input, output, session){
  observe({
    #url <- "https://yke13.shinyapps.io/bmi_chart_v3/?gender=0&age_weeks=779.4&bmi=25.51&height=140&weight=50"
    query <<- parseQueryString(session$clientData$url_search)
    #query <<- parseQueryString(url)
    if(!is.null(query[["bmi"]])){
      gender <<- as.numeric(query[[1]])
      age_weeks <<- as.numeric(query[["age_weeks"]])
      bmi <<- as.numeric(query[["bmi"]])
      height <<- as.numeric(query[["height"]])
      weight <<- as.numeric(query[["weight"]])
      
      # switch charts
      if(gender==0){
        BMI <<- boy_bmi
        #title <<- "BMI Age Percentiles for Boys"
      }
      else{
        BMI <<- girl_bmi
        #title <<- "BMI Age Percentiles for Girls"
      }
      
      # calculations
      age_months <<- round(age_weeks * 0.23 / 0.5)*0.5
      closest_age <<- BMI[abs(BMI$age_months - age_months) == min(abs(age_months - BMI$age_months)), "age_months"][1]
      subset <- BMI[BMI$age_months==closest_age, ]
      L <- subset$L[1]
      M <- subset$M[1]
      S <- subset$S[1]
      closest_bmi <- subset[abs(subset$value - bmi) == min(abs(bmi - subset$value)), "value"]
      closest_percentile <- subset[subset$value==closest_bmi, "variable"]
      exact_percentile <- round(pnorm((((bmi/M)^L) - 1) / (L*S)) * 100)
      group <- subset[subset$value==closest_bmi, "group"]
      
      if(group=="overweight"){
        target_bmi <- subset[subset$variable=="85_percentile", "value"]
        target_weight <- target_bmi * (height/100)**2
        threshold_text <<- paste("Your child is currently on the", "<br>",
                                 "<font size = 14, color=\"#33658A\">", exact_percentile, "percentile.", "</font>","<br>", "<br>",
                                 "For their height and gender, they are approximately:", "<br>",  
                                 "<font size = 6, color=\"F26419\">", round(weight-target_weight),"kg from the normal range.", "</font>", 
                                 "<br>", "<br>",
                                 "It is not recommended that children should lose weight if they are a bit overweight, 
                                 but rather slow their weight gain compared to height growth", "<br>", "<br>",
                                 "Other then an appropriate diet, children need at least", 
                                 "<b> 2hr of physical activity every day </b>",
                                 "usually carried out in short blocks of time.", "<br>", "<br>",
                                 "<i> Find out more about sport facilities around your area! </i>"
        )
        
      }
      if(group=="obese"){
        target_bmi <- subset[subset$variable=="85_percentile", "value"]
        target_weight <- target_bmi * (height/100)**2
        threshold_text <<- paste("Your child is currently on the", "<br>",
                                 "<font size = 14, color=\"#33658A\">", exact_percentile, "percentile.", "</font>","<br>", "<br>",
                                 "For their height and gender, they are approximately:", "<br>",  
                                 "<font size = 6, color=\"F26419\">", round(weight-target_weight),"kg from the normal range.", "</font>", 
                                 "<br>", "<br>",
                                 "Children grow very fast, (something to reassure the parents :/ )",
                                 "It's important to remember that children need at least", 
                                 "<b> 2hr of physical activity every day </b>",
                                 "usually carried out in short blocks of time.", "<br>", "<br>",
                                 "<i> Find out more about sport facilities around your area! </i>"
        )
      }
      if(group=="normal"){
        threshold_text <<- paste("Your child is currently on the", "<br>",
                                 "<font size = 14, color=\"#33658A\">", exact_percentile, "percentile.", "</font>","<br>", "<br>",
                                 "For their height and gender, they are in the", "<br>",  
                                 "<font size = 6, color=\"F26419\">", "normal range!", "</font>", 
                                 "<br>", "<br>",
                                 "Still, it is important to remember that children need at least", 
                                 "<b> 2hr of physical activity every day </b>",
                                 "usually carried out in short blocks of time.", "<br>", "<br>",
                                 "<i> Find out more about sport facilities around your area! </i>"
        )
      }
      if(group=="underweight"){
        target_bmi <- subset[subset$variable=="5_percentile", "value"]
        target_weight <- target_bmi * (height/100)**2
        threshold_text <<- paste("Your child is currently on the", "<br>",
                                 "<font size = 14, color=\"#33658A\">", exact_percentile, "percentile.", "</font>","<br>", "<br>",
                                 "For their height and gender, they are approximately:", "<br>",  
                                 "<font size = 6, color=\"F26419\">", round(target_weight-weight),"kg from the normal range.", "</font>", 
                                 "<br>", "<br>",
                                 "They are currently considered to be underweight, 
                                 (some diet links)"
                                 
        )
      }
    }
    
  })

  #print(gender)
  # plotly -- chart
  output$chart <- renderPlotly({
    
    base_chart <- ggplot(BMI, aes(x=age_months, y=value, text='')) +
      geom_ribbon(data=BMI[BMI$variable=="3_percentile", ],
                  aes(ymin=BMI[BMI$variable=="3_percentile", "value"],
                      ymax=BMI[BMI$variable=="5_percentile", "value"],
                      fill=group)) +
      geom_ribbon(data=BMI[BMI$variable=="5_percentile", ],
                  aes(ymin=BMI[BMI$variable=="5_percentile", "value"],
                      ymax=BMI[BMI$variable=="85_percentile", "value"],
                      fill=group)) +
      geom_ribbon(data=BMI[BMI$variable=="90_percentile", ],
                  aes(ymin=BMI[BMI$variable=="85_percentile", "value"],
                      ymax=BMI[BMI$variable=="95_percentile", "value"],
                      fill=group)) +
      geom_ribbon(data=BMI[BMI$variable=="97_percentile", ],
                  aes(ymin=BMI[BMI$variable=="95_percentile", "value"],
                      ymax=BMI[BMI$variable=="97_percentile", "value"],
                      fill=group)) +
      scale_fill_manual(values=c(rgb(52,175,143, maxColorValue = 255), rgb(239,71,111, maxColorValue = 255), 
                                 rgb(255,209,102, maxColorValue = 255), rgb(9,143,196, maxColorValue = 255))) +
      scale_x_discrete(limits=c(24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216,
                                228, 240),
                       labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                                "16", "17", "18", "19", "20")) +
      labs(title="", x="Age (years)", y="BMI") +
      theme_bw() + theme(panel.background = element_blank(),
                         panel.border = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank()) 
    
    
    base_chart <- ggplotly(source="chart", base_chart, tooltip="text", height=800) %>%
      layout(legend=list(orientation = 'h', y=0.9, x=0))

    if(!is.null(query[["bmi"]])){
        
      base_chart <- ggplot(BMI, aes(x=age_months, y=value, text='')) +
        geom_ribbon(data=BMI[BMI$variable=="3_percentile", ],
                    aes(ymin=BMI[BMI$variable=="3_percentile", "value"],
                        ymax=BMI[BMI$variable=="5_percentile", "value"],
                        fill=group)) +
        geom_ribbon(data=BMI[BMI$variable=="5_percentile", ],
                    aes(ymin=BMI[BMI$variable=="5_percentile", "value"],
                        ymax=BMI[BMI$variable=="85_percentile", "value"],
                        fill=group)) +
        geom_ribbon(data=BMI[BMI$variable=="90_percentile", ],
                    aes(ymin=BMI[BMI$variable=="85_percentile", "value"],
                        ymax=BMI[BMI$variable=="95_percentile", "value"],
                        fill=group)) +
        geom_ribbon(data=BMI[BMI$variable=="97_percentile", ],
                    aes(ymin=BMI[BMI$variable=="95_percentile", "value"],
                        ymax=BMI[BMI$variable=="97_percentile", "value"],
                        fill=group)) + # normal, obese, overweight, underweight
        scale_fill_manual(values=c(rgb(52,175,143, maxColorValue = 255), rgb(239,71,111, maxColorValue = 255), 
                                    rgb(255,209,102, maxColorValue = 255), rgb(9,143,196, maxColorValue = 255))) +
        scale_x_discrete(limits=c(24, 36, 48, 60, 72, 84, 96, 108, 120, 132, 144, 156, 168, 180, 192, 204, 216,
                                  228, 240),
                         labels=c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15",
                                  "16", "17", "18", "19", "20")) +
        labs(title="", x="Age (years)", y="BMI") +
        theme_bw() + theme(panel.background = element_blank(),
                           panel.border = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank())
      
      base_chart <- ggplotly(source="chart", base_chart, tooltip="text", height=800) %>%
        layout(legend= list(orientation = 'h')) %>%
        add_trace(x=age_months, y=bmi, mode="markers", type="scatter", showlegend=FALSE,
                  text=paste("Click for more information"),
                  hoverinfo="text",
                  marker=list(color="black",
                              size=20,
                              symbol=17))
    }
    
    event <- event_data("plotly_click", source = "chart")
    if(!is.null(event)){
      output$text <- renderText({
        paste("<font size=26, color=\"#70C1B3\">","Breakdown", "</font>", "<br>", "<br>",
              threshold_text)
        })
    }
    
    base_chart

    
  })
  threshold_text <<- ""
})