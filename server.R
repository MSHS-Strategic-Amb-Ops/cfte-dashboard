
server <- function(input, output, session) {
  
  # CPSC CFTE Breakdown Tab ----------------------------------------------------
  ## ObserveEvent Filters ======================================================
  ### Update filters based on selected month +++++++++++++++++++++++++++++++++++
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
    provider_input <- input$selectedProvider 
    

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
      cpsc_breakdown_tbl %>%
        arrange(PROVIDER),
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
  
  ## Reactive Data =============================================================
  cpsc_data_submission_filtered <- reactive({
    
    data <- cpsc_data %>%
      filter(MasterDept %in% input$selectedDept_cpsc_submission) 
    
  })
  
  ## Table Output ==============================================================
  ### OPTION 1
  output$cpsc_data_for_update <- renderRHandsontable({
    
    dept_input <- input$selectedDept_cpsc_submission
    months_update <- tail(sort(unique(cpsc_data$Month)),2)
    
    provider_filter <- unique((cpsc_data_submission_filtered() %>% 
                                 filter(MasterDept %in% dept_input))$PROVIDER)
    
    # Filter data based on month and provider only 
    cpsc_data_submission <- cpsc_data_submission_filtered() %>% 
      filter(Month %in% months_update,
             PROVIDER %in% provider_filter)
    
    # dept_input <- "BIMG"
    # months_update <- tail(sort(unique(cpsc_data$Month)),2)
    # 
    # provider_filter <- unique((cpsc_data%>% 
    #                              filter(MasterDept %in% dept_input))$PROVIDER)
    # 
    # # Filter data based on month and provider only 
    # cpsc_data_submission <- cpsc_data %>% 
    #   filter(Month %in% months_update,
    #          PROVIDER %in% provider_filter)
    
    validate(
      need(!(is.na(dept_input)), "Please select at least one department")
    )
    
    data <- cpsc_data_submission %>%
      dplyr::select(PROVIDER, NPI, Hired, Deactivated, Specialty, Department, Month, ReportedCFTE) %>%
      group_by(Department) %>%
      mutate(inactive_dept = sum(ReportedCFTE)) %>%
      filter(inactive_dept > 0) %>%
      dplyr::select(-inactive_dept) %>%
      group_by(PROVIDER, Month) %>%
      mutate(total_month_CFTE = sum(ReportedCFTE)) %>%
      pivot_wider(names_from = "Month",
                  values_from = ReportedCFTE:total_month_CFTE) %>%
      mutate(NPI = as.character(NPI)) %>%
      arrange(PROVIDER)
    
    
    col_order <- c("PROVIDER","NPI","Hired", "Deactivated", "Specialty","Department", 
                   paste0("ReportedCFTE_",months_update[1]), paste0("total_month_CFTE_",months_update[1]),
                   paste0("ReportedCFTE_",months_update[2]), paste0("total_month_CFTE_",months_update[2]))
    data <- data[match(col_order, colnames(data))]
    colnames(data) <- c("PROVIDER","NPI","Hired Date", "Deactivated Date", "Specialty","Department", 
                        paste0(months_update[1], "\nCFTE Allocated to Department"), paste0(months_update[1], "\nTotal Provider CFTE"),
                        paste0(months_update[2], "\nCFTE Allocated to Department"), paste0(months_update[2], "\nTotal Provider CFTE"))
    data[is.na(data)] <- 0.00
    
    col_highlight <- c(7:10)
    
    rhandsontable(data, overflow= 'visible',
                  height = 800,
                  col_highlight = col_highlight, rowHeaders = TRUE, readOnly = FALSE) %>%
      # hot_cols(6:10, halign = "htCenter") %>%
      hot_col(c(1:2,6:8), readOnly = T) %>%
      hot_rows(fixedRowsTop = 0) %>%
      hot_validate_numeric(col = 7:10, min = 0, max = 1) %>%
      hot_col(3:4, dateFormat = "MM/DD/YYYY", type = "date") %>%
      hot_col("Specialty", type = "dropdown", source = specialty_choices) %>%
      hot_cols(colWidths = c(350,80,110,110,280,250,120,120,120,120)) %>%
      hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);

             if (value == 0.00) {
              td.style.background = 'lightgrey';
              td.style.color = 'lightgrey';
             } else if (value > 1 && value < 1900) {
              td.style.background = 'pink';
              td.style.color = 'red';
             } else if (value == 1900) {
              td.style.background = 'white';
              td.style.color = 'white';
             } else {
              td.style.background = 'white';
              td.style.color = 'black';
             } 
           }") 
      # hot_table(customBorders = list(list(
      #   range = list(from = list(row = 0, col = 8),
      #                to = list(row = nrow(data)-1, col = 9)),
      #   top = list(width = 3, color = "red"),
      #   left = list(width = 3, color = "red"),
      #   bottom = list(width = 3, color = "red"),
      #   right = list(width = 3, color = "red"))))
    
  })
  
  
  
  ### OPTION 2
  output$cpsc_data_for_update_2 <- renderRHandsontable({
    
    dept_input <- input$selectedDept_cpsc_submission
    months_update <- tail(sort(unique(cpsc_data$Month)),1)
    
    provider_filter <- unique((cpsc_data_submission_filtered() %>% 
                                 filter(MasterDept %in% dept_input))$PROVIDER)
    
    # Filter data based on month and provider only 
    cpsc_data_submission <- cpsc_data_submission_filtered() %>% 
      filter(Month %in% months_update,
             PROVIDER %in% provider_filter)
    
    # dept_input <- "BIMG"
    # months_update <- tail(sort(unique(cpsc_data$Month)),1)
    # 
    # provider_filter <- unique((cpsc_data%>%
    #                              filter(MasterDept %in% dept_input))$PROVIDER)
    # 
    # # Filter data based on month and provider only
    # cpsc_data_submission <- cpsc_data %>%
    #   filter(Month %in% months_update,
    #          PROVIDER %in% provider_filter)

    validate(
      need(!(is.na(dept_input)), "Please select at least one department")
    )
    
    data <- cpsc_data_submission %>%
      dplyr::select(PROVIDER, NPI, Hired, Deactivated, Specialty, Department, Month, ReportedCFTE) %>%
      group_by(Department) %>%
      mutate(inactive_dept = sum(ReportedCFTE)) %>%
      filter(inactive_dept > 0) %>%
      dplyr::select(-inactive_dept) %>%
      pivot_wider(names_from = Department,
                  values_from = ReportedCFTE) %>%
      mutate(NPI = as.character(NPI)) %>%
      arrange(PROVIDER) 
    
    data <- data %>%
      rowwise() %>%
      mutate(total_CFTE = sum(c_across(c(7:length(data))), na.rm = T)) %>%
      dplyr::select(-Month) %>%
      rename(`Hired Date` = Hired,
             `Deactivated Date` = Deactivated)
    
    
    colnames(data) <- c(colnames(data)[1:length(data)-1], paste0(months_update, "\nTotal CFTE"))
    data[is.na(data)] <- 0.00
    
    col_highlight <- c(6:length(data))
    
    rhandsontable(data, overflow= 'visible',
                  height = 800,
                  col_highlight = col_highlight, rowHeaders = TRUE, readOnly = FALSE) %>%
      # hot_cols(6:10, halign = "htCenter") %>%
      hot_col(c(1:2), readOnly = T) %>%
      hot_rows(fixedRowsTop = 0) %>%
      hot_cols(fixedColumnsLeft = 1) %>%
      hot_validate_numeric(col = 6:length(data), min = 0, max = 1) %>%
      hot_col(3:4, dateFormat = "MM/DD/YYYY", type = "date") %>%
      hot_col("Specialty", type = "dropdown", source = specialty_choices) %>%
      hot_cols(colWidths = c(250,80,110,110,280,rep(100,length(data)-5))) %>%
      hot_cols(renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.TextRenderer.apply(this, arguments);

             if (value == 0.00) {
              td.style.background = 'lightgrey';
              td.style.color = 'lightgrey';
             } else if (value == 1900) {
              td.style.background = 'white';
              td.style.color = 'white';
             } else {
              td.style.background = 'white';
              td.style.color = 'black';
             } 
           }") 
      # hot_table(customBorders = list(list(
      #   range = list(from = list(row = 0, col = length(data)-2),
      #                to = list(row = nrow(data)-1, col = length(data)-1)),
      #   top = list(width = 3, color = "red"),
      #   left = list(width = 3, color = "red"),
      #   bottom = list(width = 3, color = "red"),
      #   right = list(width = 3, color = "red"))))
      # 
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} # Close Server
