

shinyApp(ui = ui, server = server)


# Load packages ----------------------------------------------------------------
suppressMessages({
  memory.limit(size = 8000000)
  library(readxl)
  library(writexl)
  library(plyr)
  library(dplyr)
  library(data.table)
  library(zoo)
  library(shiny)
  library(shinydashboard)
  library(shinydashboardPlus)
  library(shinyWidgets)
  library(htmlwidgets)
  library(lubridate)
  library(tcltk)
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  library(leaflet)
  library(grid)
  library(gridExtra)
  library(eeptools)
  library(ggQC)
  library(zipcodeR)
  library(utils)
  library(scales)
  library(chron)
  library(bupaR)
  library(shiny)
  library(DT)
  library(DiagrammeR)
  library(shinyalert)
  library(edeaR)
  library(processmapR)
  library(processmonitR)
  library(processanimateR)
  library(tidyr)
  library(lubridate)
  library(RColorBrewer)
  library(DiagrammeR)
  library(ggplot2)
  library(leaflet)
  library(readr)
  library(highcharter)
  library(ggforce) # for 'geom_arc_bar'
  library(packcircles) # for packed circle graph
  library(viridis)
  library(ggiraph)
  library(treemapify)
  library(treemap)
  library(broom)
  library(extrafont)
  library(tis) # for US holidays
  library(vroom)
  library(sjmisc)
  library(tools)
  library(here)
  library(shinyBS)
  library(shinyscreenshot)
  library(fasttime)
  library(shinycssloaders)
  library(feather)
  # library(zipcodeR)
  library(formattable)
  library(shinyjs)
  library(janitor)
  library(patchwork)
  library(flexdashboard)
  # library(tidyverse)
  # library(viridis)
  # library(hrbrthemes)
  # library(plotly)
  # install.packages("bsts")
  library(bsts)
  library(reactable)
  # install.packages("reactablefmtr")
  library(reactablefmtr)
  library(svDialogs)
  # library(openxlsx)
  library(flextable)
  library(officedown)
  library(officer)
  library(magrittr)
  library(webshot) 
  library(png)
  library(ggh4x)
  library(RODBC)
  library(DBI)
  library(odbc)
  library(dbplyr)
  library(pool)
  library(emojifont)
  library(rhandsontable)
})


# Process CPSC Data ------------------------------------------------------------
cpsc_data <- read_excel("~/cfte-dashboard/Dummy Data/cpsc_data_sample.xlsx")
master_dept_mapping <- read_excel("~/cfte-dashboard/Dummy Data/Master Dept Mapping.xlsx")


## Map Master Dept by CPSC Department ------------------------------------------
cpsc_data$`Master Dept` <- master_dept_mapping$`Master Dept`[match(trim(cpsc_data$Department), trim(master_dept_mapping$Department))]

cpsc_data <- cpsc_data %>%
  mutate(NPI = as.numeric(NPI)) %>%
  filter(!is.na(NPI)) %>%
  filter(nchar(Month) != 4) %>%
  mutate(Year = format(as.Date(as.numeric(Month), origin = "1900-01-01"), "%Y"),
         Month = format(as.Date(as.numeric(Month), origin = "1900-01-01"), "%Y-%m"))

colnames(cpsc_data) <- gsub(" ","",colnames(cpsc_data))
colnames(cpsc_data) <- str_trim(colnames(cpsc_data))

# Merge with Hire & Deactivation Date Data -------------------------------------
cpsc_data <- cpsc_data %>%
  mutate(Hired = 1900,
         Deactivated = NA)


## Global Variables ------------------------------------------------------------

## Global Function -------------------------------------------------------------
### Add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    h2(label, style = "display: inline;"),
    span("*", class = "mandatory_star")
  )
}

# Code for making name filed Mandatory -----------------------------------------
# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "
