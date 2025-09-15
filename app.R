
options(repos = c(CRAN = "https://cloud.r-project.org"))
#-----
#PSSI Triage Tool app - June 2025
#----
library(jose)
library(rsconnect)
library(shiny)
library(shinyjs)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(fmsb)
library(scales)
library(stringr)

# Full data setup (all 46 stressors)
pssi_items <- tibble::tribble(
  ~id, ~domain, ~item,
  1, "Academic", "Preparing for exams",
  2, "Academic", "Writing exams",
  3, "Academic", "Writing multiple exams around the same time",
  4, "Academic", "Exams worth more than 50% of course grade",
  5, "Academic", "Heavily weighted assignments",
  6, "Academic", "Having multiple assignments due around the same time",
  7, "Academic", "Managing my academic workload",
  8, "Academic", "Receiving a bad grade",
  9, "Academic", "Managing a high GPA",
  10, "Academic", "Working on my thesis",
  11, "Academic", "Performing well at my professional placement (i.e., practicum, clerkship, etc.)",
  12, "Learning Environment", "Poor communication from professor",
  13, "Learning Environment", "Unclear expectations from professor",
  14, "Learning Environment", "Lack of guidance from professor",
  15, "Learning Environment", "Meeting with my professor",
  16, "Learning Environment", "Meeting my thesis/placement supervisor's expectations",
  17, "Learning Environment", "Lack of mentoring from my thesis/placement supervisor",
  18, "Campus Culture", "Adjusting to the post-secondary lifestyle",
  19, "Campus Culture", "Adjusting to my program",
  20, "Campus Culture", "Academic competition among my peers",
  21, "Campus Culture", "Feeling like I'm not working hard enough",
  22, "Campus Culture", "Feeling like my peers are smarter than I am",
  23, "Campus Culture", "Pressure to succeed",
  24, "Campus Culture", "Discrimination on campus",
  25, "Campus Culture", "Sexual harassment on campus",
  26, "Interpersonal", "Making new friends",
  27, "Interpersonal", "Maintaining friendships",
  28, "Interpersonal", "Networking with the \"right\" people",
  29, "Interpersonal", "Feeling pressured to socialize",
  30, "Interpersonal", "Balancing a social life with academics",
  31, "Interpersonal", "Comparing myself to others",
  32, "Interpersonal", "Comparing my life to others' on social media",
  33, "Interpersonal", "Meeting other peoples' expectations of me",
  34, "Interpersonal", "Meeting my own expectations",
  35, "Personal", "Making sure that I get enough sleep",
  36, "Personal", "Making sure that I get enough exercise",
  37, "Personal", "Making sure that I eat healthy",
  38, "Personal", "Having to prepare meals for myself",
  39, "Personal", "Balancing working at my job with my academics",
  40, "Personal", "Balancing my extracurriculars with academics",
  41, "Personal", "Feeling guilty about taking time for my hobbies/interests",
  42, "Personal", "Having to take student loans",
  43, "Personal", "Worrying about paying off debt",
  44, "Personal", "Worrying about getting a job after graduating",
  45, "Personal", "Worrying about getting into a new program after graduating",
  46, "Personal", "Worrying about reaching major \"life events\" (i.e., buying a house, marriage, children)"
)

graph_labels <- c(
  "Preparing for exams" = "Exam Prep",
  "Writing exams" = "Exams",
  "Writing multiple exams around the same time" = "Multi Exams",
  "Exams worth more than 50% of course grade" = "Heavy Exams",
  "Heavily weighted assignments" = "Heavy Assign",
  "Having multiple assignments due around the same time" = "Mult Assign",
  "Managing my academic workload" = "Workload",
  "Receiving a bad grade" = "Bad Grade",
  "Managing a high GPA" = "GPA",
  "Working on my thesis" = "Thesis",
  "Performing well at my professional placement (i.e., practicum, clerkship, etc.)" = "Placement",
  "Poor communication from professor" = "Poor comm",
  "Unclear expectations from professor" = "Clarity",
  "Lack of guidance from professor" = "Guidance",
  "Meeting with my professor" = "Meet Professor",
  "Meeting my thesis/placement supervisor's expectations" = "Meet Expec (Advisor)",
  "Lack of mentoring from my thesis/placement supervisor" = "Mentoring",
  "Adjusting to the post-secondary lifestyle" = "Lifestyle",
  "Adjusting to my program" = "Program",
  "Academic competition among my peers" = "Competition",
  "Feeling like I'm not working hard enough" = "Work Hard",
  "Feeling like my peers are smarter than I am" = "Smarter",
  "Pressure to succeed" = "Succeed",
  "Discrimination on campus" = "Discrimination",
  "Sexual harassment on campus" = "Harassment",
  "Making new friends" = "New Friends",
  "Maintaining friendships" = "Maintain Friends",
  "Networking with the \"right\" people" = "Networking",
  "Feeling pressured to socialize" = "Pressure (Social)",
  "Balancing a social life with academics" = "Balance (Social)",
  "Comparing myself to others" = "Comparing",
  "Comparing my life to others' on social media" = "Soc Media",
  "Meeting other peoples' expectations of me" = "Meet Expec (Others)",
  "Meeting my own expectations" = "Meet Expec (Self)",
  "Making sure that I get enough sleep" = "Sleep",
  "Making sure that I get enough exercise" = "Exercise",
  "Making sure that I eat healthy" = "Nutrition",
  "Having to prepare meals for myself" = "Cooking",
  "Balancing working at my job with my academics" = "Balance (Work)",
  "Balancing my extracurriculars with academics" = "Balance (XCs)",
  "Feeling guilty about taking time for my hobbies/interests" = "Hobbies",
  "Having to take student loans" = "Loans",
  "Worrying about paying off debt" = "Debt",
  "Worrying about getting a job after graduating" = "Job",
  "Worrying about getting into a new program after graduating" = "New Program",
  "Worrying about reaching major \"life events\" (i.e., buying a house, marriage, children)" = "Life Events"
)

domain_colors <- c(
  "Academic" = "#E41A1C",
  "Learning Environment" = "#377EB8",
  "Campus Culture" = "#4DAF4A",
  "Interpersonal" = "#984EA3",
  "Personal" = "#FF7F00"
)


ui <- navbarPage("PSSI Student Triage Tool",
                 id = "main_navbar",
                 
                 tabPanel("Survey",
                          sidebarLayout(
                            sidebarPanel(
                              helpText(HTML("The PSSI is a tool made <em>for students, by students</em>, designed to assess stressors across five domains. Use the colored tabs to the right to rate each stressor by <strong>severity</strong> using the slider. Leave it unrated if it's not applicable to you!")),
                              helpText(strong("Make sure to browse through the stressors in all tabs before clicking submit below.")),
                              br(),
                              actionButton("submit", "Submit Responses", class = "btn-primary")
                            ),
                            mainPanel(
                              tabsetPanel(
                                id = "tabs",
                                tabPanel("Academic", uiOutput("academic_ui")),
                                tabPanel("Learning Environment", uiOutput("learning_env_ui")),
                                tabPanel("Campus Culture", uiOutput("campus_culture_ui")),
                                tabPanel("Interpersonal", uiOutput("interpersonal_ui")),
                                tabPanel("Personal", uiOutput("personal_ui"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("Results",
                          fluidRow(
                            column(4,
                                   h4("Stress by Domain"),
                                   textOutput("radar_interpretation"),
                                   plotOutput("radar_plot", height = "400px"),
                                   br(),
                                   uiOutput("domain_text"),
                                   br(),       
                            ),
                            column(8,
                                   h4("Your Stressors"),
                                   checkboxGroupInput("domain_filter", "Filter by Domain:",
                                                      choices = unique(pssi_items$domain),
                                                      selected = unique(pssi_items$domain),
                                                      inline = TRUE
                                   ),
                                   plotOutput("lollipop_plot", height = "600px")
                            )
                          )
                 ),
                 
                 tabPanel("Recommendations",
                          h4("Recommended Resources"),
                          p("Based on your responses on the PSSI, here are some recommended resources mapped directly to your top ten stressors."),
                          uiOutput("recommendation_output")
                 )
)

server <- function(input, output, session) {
  
  render_domain_ui <- function(domain_name) {
    items <- pssi_items %>% filter(domain == domain_name)
    tagList(
      tags$h4(strong("Rate each of the following stressors by severity."), style = "margin-bottom: 10px;"),
      lapply(seq_len(nrow(items)), function(i) {
        id <- items$id[i]
        full_item <- items$item[i]
        fluidRow(
          column(8, tags$div(style = "margin-top: 10px;", strong(full_item))),
          column(4, sliderInput(inputId = paste0("sev_", id), label = NULL, min = 0, max = 10, value = 0))
        )
      })
    )
  }
  
  output$academic_ui <- renderUI({ render_domain_ui("Academic") })
  output$learning_env_ui <- renderUI({ render_domain_ui("Learning Environment") })
  output$campus_culture_ui <- renderUI({ render_domain_ui("Campus Culture") })
  output$interpersonal_ui <- renderUI({ render_domain_ui("Interpersonal") })
  output$personal_ui <- renderUI({ render_domain_ui("Personal") })
  
  responses <- eventReactive(input$submit, {
    tibble(
      id = pssi_items$id,
      item = pssi_items$item,
      domain = pssi_items$domain,
      severity = sapply(pssi_items$id, function(i) input[[paste0("sev_", i)]] %||% 0)
    ) %>%
      mutate(label = graph_labels[item]) %>%
      filter(severity > 0)
  })
  
  output$radar_plot <- renderPlot({
    req(responses())
    data <- responses()
    
    # Calculate average severity per domain (include all domains)
    domain_avg <- pssi_items %>%
      select(domain) %>%
      distinct() %>%
      left_join(
        data %>% group_by(domain) %>% summarise(avg_sev = mean(severity)), 
        by = "domain"
      ) %>%
      mutate(avg_sev = ifelse(is.na(avg_sev), 0, avg_sev)) %>%
      arrange(domain)
    
    max_vals <- rep(10, nrow(domain_avg))
    min_vals <- rep(0, nrow(domain_avg))
    user_vals <- domain_avg$avg_sev
    
    radar_df <- data.frame(rbind(max_vals, min_vals, user_vals))
    colnames(radar_df) <- domain_avg$domain
    rownames(radar_df) <- c("max", "min", "You")
    
    par(mar = c(1,1,1,1))
    fmsb::radarchart(
      radar_df,
      axistype = 1,
      pcol = "#56B4E9",
      pfcol = scales::alpha("#56B4E9", 0.4),
      plwd = 4,
      plty = 1,
      cglcol = "grey",
      cglty = 1,
      axislabcol = "grey",
      caxislabels = seq(0, 10, 2),
      cglwd = 0.8,
      vlcex = 0.7
    )
  })
  
  output$domain_text <- renderUI({
    tagList(
      tags$ul(
        tags$li(strong("Academic:"), " Stressors related to exams, assignments, workload, and academic performance."),
        tags$li(strong("Learning Environment:"), " Stressors related to professor communication, expectations, and supervision."),
        tags$li(strong("Campus Culture:"), " Stressors related to social environment, competition, discrimination, and program adjustment."),
        tags$li(strong("Interpersonal:"), " Stressors related to friendships, social pressure, and expectations."),
        tags$li(strong("Personal:"), " Stressors related to self-care, lifestyle, finances, and concerns for the future.")
      )
    )
  })
  
  output$radar_interpretation <- renderText({
    req(responses())
    data <- responses()
    domain_avg <- data %>%
      group_by(domain) %>%
      summarise(avg_sev = mean(severity))
    max_domain <- domain_avg$domain[which.max(domain_avg$avg_sev)]
    
    paste0("Based on your responses, it looks like the majority of your stressors fall under the '", max_domain, "' domain. Take a look at the Recommendations tab for tailored support suggestions.")
  })
  
  output$lollipop_plot <- renderPlot({
    req(responses())
    data <- responses() %>% filter(domain %in% input$domain_filter)
    
    data <- data %>%
      mutate(label = factor(label, levels = rev(unique(label))))
    
    ggplot(data) +
      geom_segment(aes(x = 0, xend = severity, y = label, yend = label), color = "gray80", size = 2) +
      geom_point(aes(x = severity, y = label, color = domain), size = 5) +
      scale_color_brewer(palette = "Set1", name = "Domain") +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), name = "Severity") +
      labs(y = NULL) +
      theme_minimal(base_size = 15) +
      theme(axis.text.y = element_text(size = 13))
  })
  
  output$recommendation_output <- renderUI({
    data <- responses() %>% arrange(desc(severity)) %>% head(10)
    lapply(seq_along(data$item), function(i) {
      stressor <- data$item[i]
      domain <- data$domain[i]
      tags$p(HTML(paste0("<strong>", i, ". ", graph_labels[stressor], ":</strong> Suggested campus resource for ", domain)))
    })
  })
  
  observeEvent(input$submit, {
    updateNavbarPage(session, inputId = "main_navbar", selected = "Results")
  })
}

shinyApp(ui, server)

