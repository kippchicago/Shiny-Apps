
library(silounloadr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(devtools)
library(kippcolors)
library(formattable)
library(googleCloudStorageR)
library(bigrquery)

# load files from idea_sqrp_ontrack bucket

# App UI

ui_track_student <- dashboardPage(
  
  dashboardHeader(title = "On-Track Status Finder"),
  
  dashboardSidebar(selectizeInput(inputId = "school",
                                  label = "Select School",
                                  choices = unique(track_student$schoolabbreviation),
                                  selected = NULL,
                                  multiple = FALSE),
                   
                   selectizeInput(inputId = "grade",
                                  label = "Select Grade Level",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE),
                   
                   selectizeInput(inputId = "homeroom",
                                  label = "Homeroom",
                                  choices = NULL,
                                  selected = NULL,
                                  multiple = TRUE),
                   
                   selectizeInput(inputId = "track",
                                  label = "Track Status",
                                  choices = c("On-Track", "Almost On-Track", "Near On-Track", "Far from On-Track", "Off-Track"),
                                  selected = c("On-Track", "Almost On-Track", "Near On-Track", "Far from On-Track", "Off-Track"),
                                  multiple = TRUE)),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "kippcolors_blue.css" )),
    tags$head(tags$link(rel = "stylesheet",
                        type = "text/css",
                        href = "kippcolors_green.css")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "kippcolors_green.css")),
    
    fluidRow(box("Select your school from the dropdown menu, then filter by grade or Track Status.")),
    fluidRow(
      column(width = 6,
             DTOutput(outputId = "table")))
  )
)

server_track_student <- function(input, output, session) {       # added session
  
  school <- reactive({
    filter(track_student, schoolabbreviation == input$school)
  })
  
  observeEvent(school(), {
    choices <- unique(school()$grade_level)
    updateSelectInput(session, "grade", choices = choices, selected = sort(choices))
  })
  
  grade <- reactive({
    req(input$grade)
    filter(school(), grade_level == input$grade)
  })
  
  observeEvent(grade(), {
    choices <- unique(grade()$home_room)
    updateSelectInput(session, "homeroom", choices = choices, selected = choices)
  })
  
  output$table <- renderDT({
    req(input$homeroom)
    table <-  grade() %>%
      filter(home_room %in% input$homeroom) %>%
      filter(current_track_status %in% input$track) %>%
      select("Student Number" = student_number,
             "First Name" = first_name,
             "Last Name" = last_name,
             "Grade Level" = grade_level,
             "School Abbreviation" = schoolabbreviation,
             "Homeroom" = home_room,
             ADA,
             "Attendance Bucket" = attendance_bucket,
             "Q1 GPA" = Q1,
             "Q2 GPA" = Q2,
             "Cumulative GPA" = y_avg_gpa,
             "GPA Bucket" = gpa_bucket,
             "Current Track Status" = current_track_status,
             "Point Contribution to Average" = points)
    
    # return(
    as.datatable(formattable(table, list(
      "ADA" = formatter("span",
                        style = x ~ style(color = case_when(x <= 85 ~ "red",
                                                            x > 85 & x < 97.5 ~ "orange",
                                                            x >= 97.5 ~ "green"),
                                          font.weight = "bold")),
      "Attendance Bucket" = formatter("span",
                                      style = x ~ style(color = case_when(x == "< 85 ADA" ~ "red",
                                                                          x == ">= 85 and < 87.5 ADA"~ "orange",
                                                                          x == ">= 87.5 and < 90 ADA"~ "orange",
                                                                          x == ">= 90 and < 92.5 ADA"~ "orange",
                                                                          x == ">= 92.5 and < 95 ADA"~ "orange",
                                                                          x == ">= 95 and < 97.5 ADA"~ "orange",
                                                                          x == ">= 97.5 ADA"~ "green"),
                                                        font.weight = "bold")),
      "Cumulative GPA" = formatter("span",
                                   style = x ~ style(color = case_when(x < 2 ~ "red",
                                                                       x >= 2 & x < 3.5 ~ "orange",
                                                                       x >= 3.5 ~ "green"),
                                                     font.weight = "bold")),
      "GPA Bucket" = formatter("span",
                               style = x ~ style(color = case_when(x == "Below 2" ~ "red",
                                                                   x == "2.0 < 2.5"| x == "2.5 < 3.0" | x == "3.0 < 3.5" ~ "orange",
                                                                   x == "3.5 - 4.0" ~ "green"),
                                                 font.weight = "bold"))
    )))#)
    
  },
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = list('copy', 'print', list(
      extend = 'collection',
      buttons = c('csv', 'excel', 'pdf'),
      text = 'Download'))),
  rownames = FALSE
  )}



shinyApp(ui = ui_track_student, server = server_track_student)
