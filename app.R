#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(tm)
library(rsconnect)


# Files to store words persistently and survey data
survey_file <- "survey_responses.csv"
storage_file <- "words.rds"

# Load existing words or initialize
load_words <- function() {
    if (file.exists(storage_file)) {
        readRDS(storage_file)
    } else {
        character(0)
    }
}

# Save words to file
save_words <- function(words) {
    saveRDS(words, storage_file)
}





# Define UI
ui <- navbarPage("Lab Retreat 2025",
                 
                 # Welcome Tab
                 tabPanel("Welcome",
                          fluidPage(theme = shinytheme("united"),
                                    titlePanel(title = div(style = "text-align: center; width: 100%;", h1("Zamanian Lab Retreat 2025"),
                                                           fluidRow(
                                                               column(12,
                                                                      #img(src = "https://www.example.com/retreat_image.jpg", height = "300px"),
                                                                      
                                                                      # Smaller subtitle
                                                                      div(
                                                                          style = "text-align: center; margin-bottom: 30px;",
                                                                          h3("Use the tabs above to complete the pre-retreat survey and view the retreat schedule. In the mean time...")),
                                                                      # WordCloud
                                                                      h3("What do you value about the current Zamanian Lab culture?", style = "color: #2E86C1;"),
                                                                      div(style = "display: flex; justify-content: center; width: 100%;",
                                                                          textInput("user_input", "", placeholder = "e.g., creativity, cool_science", width = "50%")),
                                                                      actionButton("submit_word", "Add Word"),
                                                                      br(), 
                                                                      mainPanel(
                                                                          div(style = "text-align: center;",
                                                                              plotOutput("wordcloud", height = "500px", width = "130%")
                                                                          )
                                                                          
                                                                          ))))))),
                 
                 # Survey Tab
                 
                 tabPanel("Survey",
                          fluidPage(
                              titlePanel("Activity Survey"),
                              helpText("Please complete this anonymous survey to help us plan activities and meals. Your input is important for shaping a productive retreat intended to renew vision for the lab, foster creativity, and strengthen lab community. Please take the time to thoughtfully provide your input."),
                              
                              tags$style(HTML("
.custom-checkbox label {
font-weight: normal !important;
}
")),
                              
                              # Hidden input to store selected activities
                              tags$div(
                                  style = "display: none;",
                                  checkboxGroupInput("places", label = NULL, choices = c(
                                      "The Mill Paoli" = "mill",
                                      "UW Adventure Learning Program Workshop" = "learning",
                                      "Group Volunteer Activity (e.g. Habitat for Humanity, Second Harvest, etc.)" = "volunteer",
                                      "Kayaking" = "kayaking",
                                      "Bike ride" = "bike",
                                      "Group hike" = "hike",
                                      "Axe Throwing" = "axe",
                                      "Wheelhouse Studio Group Activity" = "wheelhouse",
                                      "Picnic Point Bonfire" = "bonfire",
                                      "Elena's Quote Cahoot" = "quote"
                                  ))
                              ),
                              
                              # Custom checkboxes with links
                              tags$div(
                                  class = "custom-checkboxes",
                                  lapply(list(
                                      list("mill", 'The Mill Paoli <a href="https://themillpaoli.com/" target="_blank">(Visit)</a>'),
                                      list("learning", 'UW Adventure Learning Program Workshop <a href="https://recwell.wisc.edu/alps/" target="_blank">(Visit)</a>'),
                                      list("volunteer", 'Group Volunteer Activity (e.g. Habitat for Humanity, Second Harvest, etc.)'),
                                      list("kayaking", 'Kayaking (Lake Mendota) <a href="https://union.wisc.edu/events-and-activities/outdoor-uw/group-paddling/" target="_blank">(Visit)</a>'),
                                      list("bike", 'Bike ride'),
                                      list("hike", 'Group hike'),
                                      list("axe", 'Axe Throwing <a href="https://www.madisonaxe.com/" target="_blank">(Visit)</a>'),
                                      list("wheelhouse", 'Wheelhouse Studio Group Activity <a href="https://union.wisc.edu/events-and-activities/open-art-studio-and-classes/host-your-group-event-at-wheelhouse-studios/" target="_blank">(Visit)</a>'),
                                      list("bonfire", 'Picnic Point Bonfire'),
                                      list("quote", "Elena's Quote Cahoot")
                                  ), function(opt) {
                                      tags$div(
                                          class = "custom-checkbox",
                                          tags$label(
                                              tags$input(type = "checkbox", value = opt[[1]]),
                                              HTML(paste0(" ", opt[[2]]))
                                          )
                                      )
                                  })
                              ),
                              
                              # JavaScript to sync custom checkboxes with Shiny input
                              tags$script(HTML("
$(document).on('change', '.custom-checkbox input', function() {
var selected = [];
$('.custom-checkbox input:checked').each(function() {
selected.push($(this).val());
});
Shiny.setInputValue('places', selected);
});
$(document).on('shiny:connected', function() {
$('label a').on('click', function(e) {
e.stopPropagation();
});
});
")),
                              
                              # Additional survey inputs
                              
                              selectInput("rank1", "1st Choice Activity:",
                                          choices = c("", "The Mill Paoli", "UW Adv. Learning Program Workshop", "Group Volunteer Activity", "Kayaking", "Bike ride", "Group hike", "Axe Throwing", "Wheelhouse Studio Group Activity", "Picnic Point Bonfire", "Lab Olympics", "Elena's Quote Cahoot")),
                              selectInput("rank2", "2nd Choice Activity:",
                                          choices = c("", "The Mill Paoli", "UW Adv. Learning Program Workshop", "Group Volunteer Activity", "Kayaking", "Bike ride", "Group hike", "Axe Throwing", "Wheelhouse Studio Group Activity", "Picnic Point Bonfire", "Lab Olympics", "Elena's Quote Cahoot")),
                              selectInput("rank3", "3rd Choice Activity:",
                                          choices = c("", "The Mill Paoli", "UW Adv. Learning Program Workshop", "Group Volunteer Activity", "Kayaking", "Bike ride", "Group hike", "Axe Throwing", "Wheelhouse Studio Group Activity", "Picnic Point Bonfire", "Lab Olympics", "Elena's Quote Cahoot")),
                              fluidRow(
                                  column(12,
                                         textAreaInput("questions", "Any activity comments?", "", width = "100%", height = "100px"))),
                              fluidRow(
                                  column(12,
                                         textAreaInput("questions", "What topics and/or tutorials are you interested in discussing this lab retreat?", "", width = "100%", height = "100px"))),
                              fluidRow(
                                  column(12,
                                         textAreaInput("questions", "What have you liked/disliked about past retreats? What changes do you suggest?", "", width = "100%", height = "100px"))),
                              #  textAreaInput("questions", "Any activity comments?", "", rows = 4),
                              # checkboxGroupInput("lunch", "Please select a lunch option and indicate your order in the comment box provided.",
                              #                    choices = c("Cassetta Kitchen", "Thai Basil", "Gluten-Free", "Chicken", "Fish", "Beef", "No Preference")),
                              
                              helpText(HTML("<span style='color: black; font-weight: bold;'>Please select a lunch option and indicate your order in the comment box provided.</span>")),
                              
                              # Hidden input to store selected activities
                              tags$div(
                                  style = "display: none;",
                                  checkboxGroupInput("places", label = NULL, choices = c(
                                      "Cassetta Kitchen" = "cassetta",
                                      "Thai Basil" = "thai",
                                      "Forage Kitchen" = "forage"))),
                              
                              # Custom checkboxes with links
                              tags$div(
                                  class = "custom-checkboxes",
                                  lapply(list(
                                      list("cassetta", 'Cassetta Kitchen <a href="https://www.casettakitchen.com/menus/" target="_blank">(Menu)</a>'),
                                      list("thai", 'Thai Basil <a href="https://www.thaibasilfoodmadison.com/menu" target="_blank">(Menu)</a>'),
                                      list("forage", 'Forage Kitchen <a href="https://www.eatforage.com/menu" target="_blank">(Menu)</a>')),
                                      function(opt) {
                                          tags$div(
                                              class = "custom-checkbox",
                                              tags$label(
                                                  tags$input(type = "checkbox", value = opt[[1]]),
                                                  HTML(paste0(" ", opt[[2]]))
                                              )
                                          )
                                      })
                              ),
                              
                              fluidRow(
                                  column(12,
                                         textAreaInput("comment", "Order:", "", width = "100%", height = "100px"))),
                              
                              actionButton("submit_survey", "Submit Survey"),
                              br(), br(),
                              textOutput("submitMessage"))),
                              
                              # _______________________________________________________
                              # 1. What do you think we should highlight in this lab retreat? (Relax and have fun, Foster feelings of belonging and community, Brainstorm grant ideas, Discuss future of the lab, Other (please specify), etc)
                              #
                              # lab vision through its past, present and future -- What main questions defined the lab in the past and what's in store for the future?
                              #
                              # Small group - what is your problem and what skills and ideas do others have to elevate/help experiments?
                              # Branch into related fields--
                              # Idea generator --
                              # If you weren't a researcher, what would you be doing?
                              # important to make the point that no other work (where possible) should be done on these days.
                              # What are the biggest challenges you face in your research and balancing lab tasks/culture?
                              # Opportunities for individual development?
                              # What challenges or areas for improvement have been identified? (Consider discussing communication styles, workload management, feedback processes, and overall work-life balance. )
                              # What aspects of the lab environment are most supportive of your work and personal development? (This could include access to resources, training opportunities, mentorship, and collaborative spaces. )
                              # What skills or knowledge do you want to gain or share with others in the lab?
                              #
                              # _______________________________________________________
                              
                              # Schedule Tab
                              tabPanel("Schedule",
                                       titlePanel(title = div(style = "text-align: center; width: 100%;", h3("A detailed retreat schedule will be posted at a later date.")))),
                              
                              # Admin Tab
                              tabPanel("Admin",
                                       fluidPage(
                                           titlePanel("Download Survey Data"),
                                           passwordInput("admin_pass", "Enter passcode to download data:"),
                                           actionButton("check_pass", "Download"),
                                           br(), br(),
                                           uiOutput("download_ui")
                                       ))
                              )

# Define Server
server <- function(input, output, session) {
    
    # Schedule
    # schedule <- data.frame(
    #     Time = c("9:00 AM", "10:30 AM", "12:00 PM", "2:00 PM", "4:00 PM"),
    #     Activity = c("Welcome Session", "Yoga & Meditation", "Lunch Break", "Workshop: Mindfulness", "Nature Walk")
    # )
    # output$scheduleTable <- renderTable({ schedule })
    #
    activities <- c("The Mill Paoli", "UW Adv. Learning Program Workshop", "Group Volunteer Activity", "Kayaking", "Bike ride", "Group hike", "Axe Throwing", "Wheelhouse Studio Group Activity", "Picnic Point Bonfire", "Lab Olympics", "Elena's Quote Cahoot")
    
    # Dynamic dropdowns
    observeEvent(input$rank1, {
        updateSelectInput(session, "rank2",
                          choices = c("", setdiff(activities, input$rank1)),
                          selected = if (input$rank2 %in% setdiff(activities, input$rank1)) input$rank2 else "")
    })
    
    observeEvent(c(input$rank1, input$rank2), {
        updateSelectInput(session, "rank3",
                          choices = c("", setdiff(activities, c(input$rank1, input$rank2))),
                          selected = if (input$rank3 %in% setdiff(activities, c(input$rank1, input$rank2))) input$rank3 else "")
    })
    
    # Save survey to CSV
    #
    # observeEvent(input$submit, {
    #     # Combine all responses into a data frame
    #     response <- data.frame(
    #         Selected_Activities = paste(input$places, collapse = "; "),
    #         First_Choice = input$rank1,
    #         Second_Choice = input$rank2,
    #         Third_Choice = input$rank3,
    #         Comments = input$questions,
    #         Lunch_Preferences = paste(input$lunch, collapse = "; "),
    #         stringsAsFactors = FALSE
    #     )
    #
    #     # Append to CSV file
    #     if (!file.exists(survey_file)) {
    #         write.csv(response, survey_file, row.names = FALSE)
    #     } else {
    #         write.table(response, survey_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    #     }
    #
    #     output$submitMessage <- renderText("Thank you! Your response has been recorded.")
    # })
    
    # observeEvent(input$submit, {
    #     new_entry <- data.frame(
    #         Timestamp = Sys.time(),
    #         Interested_Activities = paste(input$activities, collapse = ", "),
    #         Rank_1 = input$rank1,
    #         Rank_2 = input$rank2,
    #         Rank_3 = input$rank3,
    #         Comments = input$questions,
    #         Lunch_Preferences = paste(input$lunch, collapse = ", "),
    #         stringsAsFactors = FALSE
    #     )
    #
    #     if (!file.exists(survey_file)) {
    #         write.csv(new_entry, survey_file, row.names = FALSE)
    #     } else {
    #         write.table(new_entry, survey_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    #     }
    #
    #     output$submitMessage <- renderText("Thank you! Your response has been recorded.")
    # })
    
    observeEvent(input$submit_survey, {
        response <- data.frame(
            Selected_Activities = paste(input$places, collapse = "; "),
            First_Choice = input$rank1,
            Second_Choice = input$rank2,
            Third_Choice = input$rank3,
            Comments = input$questions,
            Lunch_Preferences = paste(input$lunch, collapse = "; "),
            stringsAsFactors = FALSE
        )
        
        if (!file.exists(survey_file)) {
            write.csv(response, survey_file, row.names = FALSE)
        } else {
            write.table(response, survey_file, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
        }
        
        output$submitMessage <- renderText("Thank you! Your response has been recorded.")
    })
    
    #WordCloud
    
    # Reactive value to store and update words
    user_texts <- reactiveVal(load_words())
    
    observeEvent(input$submit_word, {
        req(input$user_input)
        new_word <- trimws(input$user_input)
        if (nchar(new_word) > 0) {
            updated_words <- c(user_texts(), new_word)
            user_texts(updated_words)
            save_words(updated_words)
            updateTextInput(session, "user_input", value = "")
        }
    })
    
    
    output$wordcloud <- renderPlot({
        words_vector <- user_texts()
        if (length(words_vector) == 0) return(NULL)
        
        word_freqs <- table(words_vector)
        word_freqs <- sqrt(word_freqs) # normalize to reduce dominance
        
        wordcloud(
            words = names(word_freqs),
            freq = as.numeric(word_freqs),
            scale = c(7, 1),# adjust size range
            min.freq = 1,
            max.words = 100,
            colors = brewer.pal(8, "Dark2"),
            random.order = FALSE
        )
    })
    
    
 
    
    
    
    # Admin download logic
    correct_pass <- reactiveVal(FALSE)
    
    observeEvent(input$check_pass, {
        if (input$admin_pass == "retreat2025") {
            correct_pass(TRUE)
        } else {
            correct_pass(FALSE)
            showModal(modalDialog("Incorrect passcode. Please try again.", easyClose = TRUE))
        }
    })
    
    output$download_ui <- renderUI({
        if (correct_pass()) {
            downloadButton("downloadData", "Download Survey CSV")
        }
    })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("retreat_survey_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            file.copy(survey_file, file)
        })
}

# Run the app
shinyApp(ui = ui, server = server)
