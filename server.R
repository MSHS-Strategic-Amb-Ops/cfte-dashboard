
server <- function(input, output, session) {
  
  # CPSC CFTE Breakdown Tab ----------------------------------------------------
  ## ObserveEvent Filters ======================================================
  ### Update fiters based on selected month ++++++++++++++++++++++++++++++++++++
  observeEvent(input$selectedMonth,{
    if(!is.null(input$selectedMonth)){
      
      selected_month <- input$selectedMonth
      
      dept_choices <- sort(unique((cpsc_data %>% filter(Month == selected_month))$MasterDept))
      
      updatePickerInput(session,
                        inputId = "selectedDept",
                        choices = dept_choices,
                        selected = dept_choices[1]
      )
    }
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  ### Update filters based on selected month & master dept +++++++++++++++++++++++
  observeEvent(input$selectedDept,{
    if(!is.null(input$selectedDept)){
      
      selected_month <- input$selectedMonth
      selected_dept <- input$selectedDept
      
      specialty_choices <- sort(unique((cpsc_data %>% 
                                          filter(Month == selected_month) %>%
                                          filter(MasterDept %in% selected_dept))$Specialty))
      
      updatePickerInput(session,
                        inputId = "selectedSpecialty",
                        choices = specialty_choices,
                        selected = specialty_choices
      )
    }
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  ### Update filters based on selected month, master dept, specialty ++++++++++++
  observeEvent(input$selectedSpecialty,{
    if(!is.null(input$selectedSpecialty)){
      
      selected_month <- input$selectedMonth
      selected_dept <- input$selectedDept
      selected_specialty <- input$selectedSpecialty
      
      provider_choices <- sort(unique((cpsc_data %>% 
                                         filter(Month == selected_month) %>%
                                         filter(MasterDept %in% selected_dept) %>%
                                         filter(Specialty %in% selected_specialty))$PROVIDER))
      
      updatePickerInput(session,
                        inputId = "selectedProvider",
                        choices = provider_choices,
                        selected = provider_choices
      )
    }
    
  },
  ignoreInit = TRUE,
  ignoreNULL = FALSE)
  
  
  ## Reactive Data =============================================================
  cpsc_data_filtered <- reactive({
    
    data <- cpsc_data %>%
      filter(Month == input$selectedMonth) %>%
      filter(MasterDept %in% input$selectedDept) %>%
      filter(Specialty %in% input$selectedSpecialty) %>%
      filter(PROVIDER %in% input$selectedProvider)
    
  })
  
  ## Table Output ==============================================================
  output$cpsc_table <-  renderReactable({
    
    month_input <- input$selectedMonth
    dept_input <- input$selectedDept
    specialty_input <- input$selectedSpecialty
    provider_input <- input$selectedProvider # *Need to make this dependent on dept_input*
    

    # month_input <- "2023-08"
    # dept_input <- "Cardiology"
    # specialty_input <- "Cardiology: General"

    
    provider_filter <- unique((cpsc_data_filtered() %>% 
                                 filter(Month == month_input) %>%
                                 filter(MasterDept %in% dept_input) %>%
                                 filter(Specialty %in% specialty_input))$PROVIDER)
    
    # Filter data based on month and provider only 
    cpsc_breakdown <- cpsc_data_filtered() %>% 
      filter(Month == month_input,
             PROVIDER %in% provider_filter)

    validate(
      need(!(is.na(dept_input)), "Please select at least one department")
    )
    
    
    ## Process Table Output ====================================================
    cpsc_breakdown_tbl <- cpsc_breakdown %>%
      dplyr::select(PROVIDER, NPI, Department, Specialty, Month, ReportedCFTE) %>%
      group_by(Department) %>%
      mutate(inactive_dept = sum(ReportedCFTE)) %>%
      filter(inactive_dept > 0) %>%
      dplyr::select(-inactive_dept) %>%
      pivot_wider(names_from = "Department",
                  values_from = ReportedCFTE) %>%
      mutate(NPI = as.character(NPI)) %>%
      mutate(total_cfte = rowSums(across(where(is.numeric)), na.rm=TRUE))
    
    ## Average YTD CFTE ========================================================
    avg_cfte <- cpsc_data_filtered() %>% 
      filter(PROVIDER %in% provider_filter) %>%
      filter(Year == substr(month_input, 1, 4)) %>%
      group_by(PROVIDER, NPI, Month) %>%
      summarise(total_cfte = sum(ReportedCFTE)) %>%
      group_by(PROVIDER, NPI) %>%
      summarise(avg_cfte = mean(total_cfte, na.rm = TRUE))
    
    cpsc_breakdown_tbl$avg_cfte <- avg_cfte$avg_cfte[match(cpsc_breakdown_tbl$PROVIDER, avg_cfte$PROVIDER)]
    
    ## YTD % Clinical ==========================================================
    # cpsc_breakdown_tbl <- cpsc_breakdown_tbl %>%
    #   mutate(blank = "",
    #          perc_clinical = 0.3)
    
    
    ## Table Output ============================================================
    reactable(
      cpsc_breakdown_tbl,
      style = list(fontFamily = 'Calibri',
                   fontSize = '14px'),
      defaultColDef = colDef(align = "center",
                             maxWidth = 180,
                             headerStyle = list(background = "#210070", color = "white", fontWeight = "Bold", fontSize = "14px"),
                             headerClass = "bar-sort-header",
                             style = function(value) {
                               if (is.na(value)) {
                                 background <- "#E0E0E0"
                               } else {
                                 background <- "white"
                               }
                               list(background = background)
                             }),

      highlight = TRUE,
      # filterable = TRUE,
      pagination = FALSE,
      height = 700,
      wrap = TRUE,
      searchable = TRUE,
      
      columns = list(
        
        PROVIDER = colDef(
          name = "Physician",
          minWidth = 200,
          align = "left",
          headerStyle = list(background = "white", color = "black", fontWeight = "Bold", fontSize = "14px")
        ),
        
        NPI = colDef(
          name = "NPI",
          minWidth = 90,
          align = "left",
          headerStyle = list(background = "white", color = "black", fontSize = "14px")
        ),
        
        Specialty = colDef(
          name = "CPSC Specialty",
          minWidth = 200,
          align = "left",
          headerStyle = list(background = "white", color = "black", fontSize = "14px")
        ),
        
        Month = colDef(
          name = "Month",
          maxWidth = 90,
          align = "left",
          headerStyle = list(background = "white", color = "black", fontSize = "14px"),
          style = list(borderRight = "1.5px solid rgb(230, 230, 230)")
        ),
        
        total_cfte = colDef(
          name = "Total CFTE",
          maxWidth = 130,
          headerStyle = list(background = "#d80b8c", color = "white", fontWeight = "Bold", fontSize = "14px"),
          format = colFormat(digits = 1),
          # style = list(borderLeft = "1.5px solid rgb(230, 230, 230)")
          style = function(value) {
            if (value > 1) {
              background <- "#f7c0bc"
            } else if (value <= 1) {
              background <- "#bbfcbb"
            } else {
              color <- "#777"
            }
            list(background = background, fontWeight = "bold", borderLeft = "1.5px solid rgb(230, 230, 230)")
          }
        ),
        
        avg_cfte = colDef(
          name = paste0("Avg ",substr(month_input, 1, 4), " CFTE"),
          maxWidth = 130,
          align = "center",
          headerStyle = list(background = "#d80b8c", color = "white", fontWeight = "bold", color = "white", fontSize = "14px"),
          format = colFormat(digits = 1),
          style = list(borderRight = "1.5px solid rgb(230, 230, 230)", fontWeight = "bold")
        )
        
        # blank = colDef(
        #   maxWidth = 30,
        #   align = "center",
        #   headerStyle = list(background = "white", fontWeight = "bold", color = "white", fontSize = "14px"),
        #   style = list(borderRight = "1.5px solid rgb(230, 230, 230)")
        # )
        
        # perc_clinical = colDef(
        #   name = paste0(substr(month_input, 1, 4), " % Clinical"),
        #   maxWidth = 130,
        #   align = "center",
        #   headerStyle = list(background = "#00aeef", fontWeight = "bold", color = "white", fontSize = "14px"),
        #   format = colFormat(percent = TRUE, digits = 0)
        # )
        
        
      )
    
      ) # Close Reactable
  }) # Close cpsc_table
  
  
  # Data Submission Tab --------------------------------------------------------
  ## CPSC CFTE Data Submission =================================================
  cpsc_data_for_update <- reactive({
    
    data <- cpsc_data %>%
      filter()
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} # Close Server
