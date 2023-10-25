library(shiny)
library(shinyWidgets)
library(shinydashboard)


# Filter Choices ---------------------------------------------------------------
## Reporting Month Choices
month_choices <- unique(sort(cpsc_data$Month, decreasing = T))
default_month <- month_choices[1]
## Reporting Department Choices
dept_choices <- sort(unique((cpsc_data %>% filter(Month == default_month))$MasterDept))
default_dept <- dept_choices[1]
## Reporting CPSC Specialty Choices
specialty_choices <- sort(unique((cpsc_data %>% 
                                    filter(Month == default_month) %>% 
                                    filter(MasterDept %in% default_dept))$Specialty))
default_specialty <- specialty_choices
## Reporting Provider Choices
provider_choices <- sort(unique((cpsc_data %>% 
                                   filter(Month == default_month) %>% 
                                   filter(MasterDept %in% default_dept) %>%
                                   filter(Specialty %in% default_specialty))$PROVIDER))
default_provider <- provider_choices 


ui <- 
  fluidPage(
    shinyjs::useShinyjs(),
    
    tags$style(type = 'text/css', 
               '.navbar { background-color: #dddedd; color: black; font-size: 24px; font-weight: bold;}',
               '.navbar-default .navbar-brand{color: black; font-size: 24px;}'
    ),
    
    tags$style(HTML("
    .box.box-solid.box-primary>.box-header {
    color:#fff;
    background:#221f72
    }
    .box.box-solid.box-primary{
    border-bottom-color:#ffffff;
    border-left-color:#ffffff;
    border-right-color:#ffffff;
    border-top-color:#ffffff;
    }
                    ")),
    
    tags$style(
      "h3{
      margin-top: -0.7em;
      }"
    ),
    
    tags$style(
      "h4{
      font-weight: bold;
      margin-top: -0.7em;
      }"
    ),
    navbarPage("MSHS Physician CFTE/CARTS Dashboard",
               
               header = tagList(
                 useShinydashboard()
               ),
               
               tabsetPanel(  
                 # First Tab - Summary - All Sites -----------------------
                 tabPanel("CPSC CFTE Breakdown", value = "cpsc",
                          fluidRow(
                            column(2,
                                   box(
                                     title = NULL, solidHeader = FALSE, width = 12,
                                     pickerInput("selectedMonth", label = h4("Select Reporting Month:"), 
                                                 choices = month_choices,
                                                 multiple = FALSE,
                                                 options = pickerOptions(
                                                   liveSearch = TRUE,
                                                   actionsBox = TRUE,
                                                   dropupAuto = FALSE,
                                                   size = 10),
                                                 selected = default_month))),
                            column(2, 
                                   box(
                                     title = NULL, solidHeader = FALSE, width = 12,
                                     pickerInput("selectedDept", label = h4("Select Department:"),
                                                 choices = dept_choices,
                                                 multiple = TRUE,
                                                 options = pickerOptions(
                                                   liveSearch = TRUE,
                                                   actionsBox = TRUE,
                                                   dropupAuto = FALSE,
                                                   size = 10),
                                                 selected = default_dept))),
                            column(2, 
                                   box(
                                     title = NULL, solidHeader = FALSE, width = 12,
                                     pickerInput("selectedSpecialty", label = h4("Select CPSC Specialty:"),
                                                 choices = specialty_choices,
                                                 multiple = TRUE,
                                                 options = pickerOptions(
                                                   liveSearch = TRUE,
                                                   actionsBox = TRUE,
                                                   dropupAuto = FALSE,
                                                   size = 10),
                                                 selected = default_specialty))),
                            column(2,
                                   box(
                                     title = NULL, solidHeader = FALSE, width = 12,
                                     pickerInput("selectedProvider", label = h4("Select Provider:"), 
                                                 choices = provider_choices,
                                                 multiple = TRUE,
                                                 options = pickerOptions(
                                                   liveSearch = TRUE,
                                                   actionsBox = TRUE,
                                                   dropupAuto = FALSE,
                                                   size = 10),
                                                 selected = default_provider)))
                          ),
                          hr(),
                          # fluidRow(textOutput("siteComp_title")),
                          # tags$head(tags$style("#siteComp_title{color: #black; font-family:Calibri; font-weight: bold; 
                          #                    font-size: 30px; margin-top: -0.2em; margin-bottom: 0.5em; margin-left: 30px}")), br(),
                          fluidRow(
                            column(12, 
                                   reactableOutput("cpsc_table") %>%
                                     withSpinner(type = 8, color = "#dddedd"))
                          )
                 ), # Close tabPanel Summary
                 
                 # Second Tab - All Sites by KPI -----------------------
                 tabPanel("Comprehensive CFTE Breakdown", value = "outpatient"
                          
                 ), # Close tabPanel Comparison
                 
                 # Third Tab - All Sites by KPI -----------------------
                 tabPanel("CARTS Allocation", value = "carts"
                          
                 ), # Close tabPanel Comparison
                 # Fourth Tab - Breakout-----------------------
                 tabPanel("Summary by Provider", value = "summary"
                          
                 ), # Close tabPanel Breakdout
                 # Fourth Tab - Breakout-----------------------
                 # tabPanel("Data Submission", value = "data"
                 #          
                 # ) # Close tabPanel Breakdout
                 

                 navbarMenu("Data Submission", 
                            # Finance Data Submission ----
                            tabPanel("CPSC CFTE & Specialty", hr(),
                                     fluidRow(
                                       column(12,
                                              div(
                                                id = "cpsc_data_submission",
                                                fluidRow(
                                                  column(2,
                                                         textInput("cpsc_data_submission_name", (labelMandatory("1. Enter your name:")), "")
                                                  )
                                                ),
                                                fluidRow(
                                                  h2("2. Select Department CPSC CFTE data is submitted for:"),
                                                  column(2, 
                                                         box(
                                                           title = NULL, solidHeader = FALSE, width = 12,
                                                           pickerInput("selectedDept_cpsc_submission", label = h4("Select Department:"),
                                                                       choices = dept_choices,
                                                                       multiple = TRUE,
                                                                       options = pickerOptions(
                                                                         liveSearch = TRUE,
                                                                         actionsBox = TRUE,
                                                                         dropupAuto = FALSE,
                                                                         size = 10),
                                                                       selected = default_dept))),
                                                ),
                                                h2("3. Update any changes in the table below:"),
                                                h2("4. When updates are complete, click on Submit Button below:"),
                                                br(),
                                                div(id = "header_custom_biomed",
                                                    h4("Your submitted changes will be reviewed for approval."),
                                                    br()
                                                ),
                                                h3("Option 1"),
                                                rHandsontableOutput("cpsc_data_for_update"),
                                                br(),
                                                h3("Option 2"),
                                                rHandsontableOutput("cpsc_data_for_update_2"),
                                                br(),
                                                hr(),
                                                actionButton("submit_biomedkpis", "Submit", class = "btn-primary")
                                                
                                              )
                                       )
                                     ), value = "biomed_kpi"

                            ),

                            tabPanel("CARTS Allocation"

                            )

                 ) # Close navbar Menu
                 
               ) # Close tabPanel Breakout
               
    ) # Close NavBar
    
  ) # Close navbarPage