
library(silounloadr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(DT)
library(devtools)
library(kippcolors)
library(formattable)
library(googleCloudStorageR)
library(bigrquery)


# App

# App that only shows students who will age out
# and their grades in core subjects

ui_only_aged_out <- dashboardPage(
  dashboardHeader(title = "Students Aging Out"),
  dashboardSidebar(selectizeInput(inputId = "school_name",  
                                  label = "Select School",
                                  choices = c("KAMS", "KOA", "KBCP", "KAC", "KAP", "KOP", "KBP", "KACP"),
                                  selected = c("KAMS", "KOA", "KBCP", "KAC", "KAP", "KOP", "KBP", "KACP"),
                                  multiple = TRUE)),  
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet",
    #             type = "text/css",
    #             href = "kippcolors_blue.css" )),
    # tags$head(tags$link(rel = "stylesheet", 
    #                     type = "text/css", 
    #                     href = "kippcolors_green.css")),
    fluidRow(box("KIPP Chicago Promotion Policy states: 
                 Students cannot turn 16 years old during their 8th grade year")),
    fluidRow(box("Select your school from the dropdown menu. Any student who will 
                 turn 16 during their 8th grade year will appear, along with their
                 current grades in core subjects")), 
    fluidRow(
      column(width = 6,
             DTOutput(outputId = "table"))) 
  )
)

server_only_aged_out <- function(input, output) {
  if(nrow(final_ela_math_science_grades) == 0){
    
    output$table <- renderDT ({
      
      #table <- 
      students_date_16 %>% 
        # left_join(enrollments %>%
        #             select(studentid,
        #                    entrydate)) %>%
        mutate(date_enters_eighth = ymd(paste(((7 - grade_level) + next_year), 8, 15, sep = "/")),
               date_leaves_eighth = ymd(paste(((8 - grade_level) + next_year), 6, 20, sep = "-")),
               entrydate = paste(months(as.Date(entrydate)), year(as.Date(entrydate)), sep = " ")) %>%
        filter(grade_level < 8,
               date_turns_16 <= date_enters_eighth,  
               school_abbreviation %in% input$school_name) %>% 
        select("Student Number" = student_number, 
               "First Name" = first_name, 
               "Last Name" = last_name, 
               "Grade Level" = grade_level,
               "School Abbreviation" = school_abbreviation,
               "Homeroom" = home_room,
               "Date of Birth" = dob,
               "Date Student Turns 16" = date_turns_16,
               "Projected Eighth Grade Entry Date" = date_enters_eighth,
               "Projected Eighth Grade Exit Date" = date_leaves_eighth,
               "Date Student Entered KIPP" = entrydate)},
      
      # return(as.datatable(formattable(table, list(
      
      extensions = 'Buttons',
      options = list(
        dom = 'Bi',
        buttons = list('copy', 'print', list(
          extend = 'collection',
          buttons = c('csv', 'excel', 'pdf'),
          text = 'Download'))),
      rownames = FALSE
      
    )} else {
      output$table <- renderDT ({
        table <- students_date_16 %>%
          left_join(final_ela_math_science_grades,
                    by = "student_number") %>%
          #left_join(enrollments %>%
          #              select(studentid,
          #                    entrydate)) %>%
          mutate(date_enters_eighth = ymd(paste(((7 - grade_level) + next_year), 8, 15, sep = "/")),
                 date_leaves_eighth = ymd(paste(((8 - grade_level) + next_year), 6, 20, sep = "-")),
                 entrydate = paste(months(as.Date(entrydate)), year(as.Date(entrydate)), sep = " ")) %>%
          filter(grade_level < 8,
                 date_turns_16 <= date_leaves_eighth,  
                 school_abbreviation %in% input$school_name) %>%
          select("Student Number" = student_number,
                 "First Name" = first_name,
                 "Last Name" = last_name,
                 "Grade Level" = grade_level,
                 "School Abbreviation" = school_abbreviation,
                 "Homeroom" = home_room,
                 "Date of Birth" = dob,
                 "Date Student Turns 16" = date_turns_16,
                 "Projected Eighth Grade Entry Date" = date_enters_eighth,
                 "Projected Eighth Grade Exit Date" = date_leaves_eighth,
                 # "Date Student Entered KIPP" = entrydate,
                 "Current ELA Grade" = mark_ELA,
                 "Current ELA Percentage" = percentage_ELA,
                 "Current Math Grade" = mark_Math,
                 "Current Math Percentage" = percentage_Math,
                 "Current Science Grade" = mark_Science,
                 "Current Science Percentage" = percentage_Science)
        
        
        return(as.datatable(formattable(table, list(
          #  "Grade Level" = formatter("span",
          #                            style = x ~ style(color = ifelse(x > 5, "red", "black"), font.weight = ifelse(x > 5, "bold", "regular"))),
          "Current Science Percentage" = formatter("span",
                                                   style = x ~ style(color = ifelse(x < 60, "red", "black"), font.weight = ifelse(x < 60, "bold", "regular"))),
          "Current ELA Percentage" = formatter("span",
                                               style = x ~ style(color = ifelse(x <60, "red", "black"), font.weight = ifelse(x < 60, "bold", "regular"))),
          "Current Math Percentage" = formatter("span",
                                                style = x ~ style(color = ifelse(x < 60, "red", "black"), font.weight = ifelse(x < 60, "bold", "regular"))),
          "Current Science Grade" = formatter("span",
                                              style = x ~ style(color = ifelse(x == "F", "red", "black"), font.weight = ifelse(x == "F", "bold", "regular"))),
          "Current ELA Grade" = formatter("span",
                                          style = x ~ style(color = ifelse(x == "F", "red", "black"), font.weight = ifelse(x == "F", "bold", "regular"))),
          "Current Math Grade" = formatter("span",
                                           style = x ~ style(color = ifelse(x == "F", "red", "black"), font.weight = ifelse(x == "F", "bold", "regular")))
        ))))},
        extensions = 'Buttons',
        options = list(
          dom = 'Bi',
          buttons = list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'))),
        rownames = FALSE
      )
      
      
    }
}

shinyApp(ui = ui_only_aged_out, server = server_only_aged_out)

