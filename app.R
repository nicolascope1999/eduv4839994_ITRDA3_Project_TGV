# ----------------------------------------------------------------------------------------------------- UI
library(shiny)
library(shinydashboard)
library(bslib)
library(httpuv)
library(shinylive)
library(tidyverse)
library(shinyWidgets)
library(markdown)


ui <- dashboardPage(
  dashboardHeader(title = "Tech Usage Dashboard",
                  tags$li(class = "dropdown", 
                          style = "padding: 10px;",
                          input_switch("toggleNA", label = "Include 'Not Applicable'", value = FALSE)
                  )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Guide", tabName = "user_guide", icon = icon("book")),
      menuItem("Top Technologies", icon = icon("chart-bar"), startExpanded = TRUE,
               menuSubItem("Programming Languages", tabName = "ProgLang"),
               menuSubItem("Databases", tabName = "Databases"),
               menuSubItem("Platforms", tabName = "Platform"),
               menuSubItem("Web Frameworks", tabName = "WebFramework"),
               menuSubItem("AI Search Tools", tabName = "AISearch"),
               menuSubItem("AI Tools", tabName = "AITool")
      ),
      menuItem("Industry and Employment Insights", icon = icon("briefcase"), startExpanded = TRUE,
             menuSubItem("Industries by Study Field", tabName = "industry_by_study_field", icon = icon("industry")),
             menuSubItem("Roles by Study Field", tabName = "roles_by_study_field", icon = icon("table")),
             menuSubItem("Employment by Study Field", tabName = "employment_by_study_field", icon = icon("table"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      # Programming Languages
      tabItem(tabName = "ProgLang",
              fluidRow(
                box(title = "Top 10 Programming Languages (Percentage)", width = 6, plotOutput("bar_ProgLang")),
                box(title = "Top Programming Language Share", width = 6, plotOutput("pie_ProgLang"))
              ),
              fluidRow(
                box(title = "Distribution of Top 10 Programming Languages by Study Field", width = 6, plotOutput("stacked_ProgLang")),
                box(title = "Table of Top Programming Languages by Study Field", width = 6, tableOutput("list_ProgLang"))
              )
      ),
      # Databases
      tabItem(tabName = "Databases",
              fluidRow(
                box(title = "Top 10 Databases (Percentage)", width = 6, plotOutput("bar_Databases")),
                box(title = "Top Database Share", width = 6, plotOutput("pie_Databases"))
              ),
              fluidRow(
                box(title = "Distribution of Top 10 Databases by Study Field", width = 6, plotOutput("stacked_Databases")),
                box(title = "Table of Top Databases by Study Field", width = 6, tableOutput("list_Databases"))
              )
      ),
      # Platforms
      tabItem(tabName = "Platform",
              fluidRow(
                box(title = "Top 10 Platforms (Percentage)", width = 6, plotOutput("bar_Platform")),
                box(title = "Top Platform Share", width = 6, plotOutput("pie_Platform"))
              ),
              fluidRow(
                box(title = "Distribution of Top 10 Platforms by Study Field", width = 6, plotOutput("stacked_Platform")),
                box(title = "Table of Top Platforms by Study Field", width = 6, tableOutput("list_Platform"))
              )
      ),
      # Web Frameworks
      tabItem(tabName = "WebFramework",
              fluidRow(
                box(title = "Top 10 Web Frameworks (Percentage)", width = 6, plotOutput("bar_WebFramework")),
                box(title = "Top Web Framework Share", width = 6, plotOutput("pie_WebFramework"))
              ),
              fluidRow(
                box(title = "Distribution of Top 10 Web Frameworks by Study Field", width = 6, plotOutput("stacked_WebFramework")),
                box(title = "Table of Top Web Frameworks by Study Field", width = 6, tableOutput("list_WebFramework"))
              )
      ),
      # AI Search Tools
      tabItem(tabName = "AISearch",
              fluidRow(
                box(title = "Top 10 AI Search Tools (Percentage)", width = 6, plotOutput("bar_AISearch")),
                box(title = "Top AI Search Tool Share", width = 6, plotOutput("pie_AISearch"))
              ),
              fluidRow(
                box(title = "Distribution of Top 10 AI Search Tools by Study Field", width = 6, plotOutput("stacked_AISearch")),
                box(title = "Table of Top AI Search Tools by Study Field", width = 6, tableOutput("list_AISearch"))
              )
      ),
      # AI Tools
      tabItem(tabName = "AITool",
              fluidRow(
                box(title = "Top 10 AI Tools (Percentage)", width = 6, plotOutput("bar_AITool")),
                box(title = "Top AI Tool Share", width = 6, plotOutput("pie_AITool"))
              ),
              fluidRow(
                box(title = "Distribution of Top 10 AI Tools by Study Field", width = 6, plotOutput("stacked_AITool")),
                box(title = "Table of Top AI Tools by Study Field", width = 6, tableOutput("list_AITool"))
              )
      ),
      tabItem(tabName = "industry_by_study_field",
              fluidRow(
                column(6, tableOutput("industryByStudyField")),  
                column(6, fluidRow(uiOutput("industryPieCharts")))
              )
      ),
      tabItem(tabName = "roles_by_study_field",
              fluidRow(
                box(title = "Roles by Study Field", width = 6, tableOutput("rolesByStudyField")),
                box(title = "Stacked Bar Chart: Roles by Study Field", width = 6, plotOutput("stacked_rolesByStudyField"))
              )
      ),
      tabItem(tabName = "employment_by_study_field",
              fluidRow(
                box(title = "Employment by Study Field", width = 6, tableOutput("employmentByStudyField")),
                box(title = "Stacked Bar Chart: Employment by Study Field", width = 6, plotOutput("stacked_employmentByStudyField"))
              )
      ),
      tabItem(tabName = "user_guide",
        fluidRow(
          box(title = "User Guide", width = 12, 
              htmlOutput("userGuideContent"))
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------------------------------------------------ SERVER

# Read in the data from a CSV file
data <- read.csv("graduate_data_clean.csv", stringsAsFactors = FALSE)

# Define the server function for the Shiny app
server <- function(input, output, session) {
  
  # Total number of respondents used for calculating averages etc. later
  total_respondents <- nrow(data)
  
  # List of column names to process and their more user friendly display names
  columns <- c("ProgLang", "Databases", "Platform", "WebFramework", "AISearch", "AITool")
  display_names <- c("Programming Languages", "Databases", "Platforms", "Web Frameworks", 
                     "AI Tools", "AI Search Tools")
  
  # To keep code concise, the same 4 visualizations are made for all the above mentioned columns
  for (i in seq_along(columns)) {
    local({
      
      column_name <- columns[i]
      display_name <- display_names[i]
      
      # Reactive expression to split the list in the column so that each individual 
      # tool or technology can be counted. the reactive function also takes into account
      # whether the 'Not Applicable' button is checked so that the data can dynamically 
      # change
      split_list <- reactive({
        # Split the string values into individual items using ";" as delimiter
        all_values <- unlist(strsplit(data[[column_name]], ";"))
        # Remove "Not Applicable" values if the toggle is off
        if (!input$toggleNA) {
          all_values <- all_values[all_values != "Not Applicable"]
        }
        # Create a frequency table of the values
        freq_table <- as.data.frame(table(all_values))
        colnames(freq_table) <- c("Tool", "Count")
        # Calculate the percentage and arrange in descending order
        freq_table <- freq_table %>%
          mutate(Percentage = (Count / total_respondents) * 100) %>%
          arrange(desc(Percentage))
        freq_table
      })
      
      # Render a horizontal bar chart for the top 10 tools in the column
      output[[paste0("bar_", column_name)]] <- renderPlot({
        bar_top10 <- split_list() %>% head(10)
        # use reorder below as ggplot defaults to alphabetical
        ggplot(bar_top10, aes(x = reorder(Tool, Percentage), y = Percentage, fill = Tool)) +
          geom_bar(stat = "identity", show.legend = FALSE) +
          # use coord_flip to create a horizontal bar graph
          coord_flip() +
          labs(x = "Tool", y = "Percentage of Respondents",
               title = paste("Top 10", display_name, "(%)"),
               subtitle = paste("Respondents who reported using any one of these", display_name)) +
          theme_minimal()
      })
      
      # Render a pie chart comparing the top tool vs all others
      output[[paste0("pie_", column_name)]] <- renderPlot({
        pie_data <- split_list()
        top_tool <- as.character(pie_data$Tool[1])
        top_count <- pie_data$Count[1]
        others_count <- sum(pie_data$Count) - top_count
        pie_data <- data.frame(Category = c(top_tool, "Others"),
                               Count = c(top_count, others_count))
        ggplot(pie_data, aes(x = "", y = Count, fill = Category)) +
          geom_bar(stat = "identity", width = 1) +
          coord_polar(theta = "y") +
          labs(title = paste("Share of", top_tool, "vs Other", display_name)) +
          theme_void()
      })
      
      # Render a stacked bar chart showing distribution by Study Field for top 10 tools
      output[[paste0("stacked_", column_name)]] <- renderPlot({
        
        top10_tools <- head(split_list()$Tool, 10)
        
        # Filter data for the top tools and compute counts and percentages by Study Field
        filtered_data <- data %>%
          separate_rows(.data[[column_name]], sep = ";") %>%
          filter(.data[[column_name]] %in% top10_tools) %>%
          count(StudyField, tool = !!sym(column_name), name = "Count") %>%
          group_by(StudyField) %>%
          mutate(Percentage = (Count / sum(Count)) * 100) %>%
          ungroup() %>%
          arrange(desc(Count))
        
        ggplot(filtered_data, aes(x = reorder(StudyField, -Count), y = Count, fill = reorder(tool, -Count))) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = ifelse(Percentage > 2, paste0(round(Percentage, 1), "%"), "")), 
                    position = position_stack(vjust = 0.5), size = 3, color = "white") +
          coord_flip() +
          labs(x = "Study Field", y = "Number of Respondents",
               title = paste("Top 10", column_name, "Distribution by Study Field"),
               fill = "Tool") +
          theme_minimal()
      })
      
      # Render a table combining tool percentages with their breakdown by Study Field
      output[[paste0("list_", column_name)]] <- renderTable({
        # Basic tool data with percentages
        tool_data <- split_list() %>% 
          rename(!!column_name := Tool) %>% 
          select(!!sym(column_name), Percentage)
        # Calculate percentages for each Study Field per tool
        study_field_data <- data %>%
          separate_rows(!!sym(column_name), sep = ";") %>%
          group_by(!!sym(column_name), StudyField) %>%
          summarise(Count = n(), .groups = "drop") %>%
          group_by(!!sym(column_name)) %>%
          mutate(Percentage_Field = (Count / sum(Count)) * 100) %>%
          select(!!sym(column_name), StudyField, Percentage_Field) %>%
          pivot_wider(names_from = StudyField, values_from = Percentage_Field, values_fill = 0)
        # Merge the two split_lists and arrange by percentage
        final_output <- tool_data %>% 
          left_join(study_field_data, by = column_name) %>% 
          arrange(desc(Percentage))
        final_output
      }, striped = TRUE, hover = TRUE, bordered = TRUE)
    })
  }
  
  # Render a table showing roles distribution by Study Field
  output$rolesByStudyField <- renderTable({
    # Split multiple roles into separate rows
    employment_data <- data %>% separate_rows(Role, sep = ";")
    # Filter out "Prefer not to say" if toggle is off
    if (!input$toggleNA) {
      employment_data <- employment_data %>% filter(Role != "Prefer not to say")
    }
    roles_by_field <- employment_data %>%
      group_by(StudyField) %>%
      mutate(Total_StudyField = n()) %>%
      ungroup() %>%
      group_by(Role, StudyField) %>%
      summarise(Count = n(),
                Total_StudyField = first(Total_StudyField), .groups = "drop") %>%
      mutate(Percentage = (Count / Total_StudyField) * 100) %>%
      select(Role, StudyField, Percentage) %>%
      pivot_wider(names_from = StudyField, values_from = Percentage, values_fill = 0) %>%
      rowwise() %>%
      mutate(Average_Percentage = mean(c_across(-Role), na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(Average_Percentage)) %>%
      select(-Average_Percentage)
    roles_by_field
  })
  
  # Render a table showing industry distribution by Study Field
  output$industryByStudyField <- renderTable({
    industry_data <- data
    # Filter out "Prefer not to say" if toggle is off
    if (!input$toggleNA) {
      industry_data <- industry_data %>% filter(Industry != "Prefer not to say")
    }
    industry_by_field <- industry_data %>%
      group_by(StudyField) %>%
      mutate(Total_StudyField = n()) %>%
      ungroup() %>%
      group_by(Industry, StudyField) %>%
      summarise(Count = n(),
                Total_StudyField = first(Total_StudyField), .groups = "drop") %>%
      mutate(Percentage = (Count / Total_StudyField) * 100) %>%
      select(Industry, StudyField, Percentage) %>%
      pivot_wider(names_from = StudyField, values_from = Percentage, values_fill = 0) %>%
      rowwise() %>%
      mutate(Average_Percentage = mean(c_across(-Industry), na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(Average_Percentage)) %>%
      select(-Average_Percentage)
    industry_by_field
  })
  
  # Dynamically generate UI elements for industry pie charts by Study Field
  output$industryPieCharts <- renderUI({
    study_fields <- unique(data$StudyField)
    plot_output_list <- lapply(study_fields, function(field) {
      plotname <- paste0("pieChart_", gsub(" ", "_", field))
      plotOutput(plotname, height = "400px", width = "100%")
    })
    do.call(tagList, plot_output_list)
  })
  
  # Observe changes and render a pie chart for each Study Field using base R pie charts
  observe({
    industry_data <- data
    # Filter out "Prefer not to say" if toggle is off
    if (!input$toggleNA) {
      industry_data <- industry_data %>% filter(Industry != "Prefer not to say")
    }
    industry_by_field <- industry_data %>%
      group_by(StudyField) %>%
      mutate(Total_StudyField = n()) %>%
      ungroup() %>%
      group_by(Industry, StudyField) %>%
      summarise(Count = n(),
                Total_StudyField = first(Total_StudyField), .groups = "drop") %>%
      mutate(Percentage = (Count / Total_StudyField) * 100) %>%
      select(Industry, StudyField, Percentage)
    study_fields <- unique(industry_by_field$StudyField)
    for (field in study_fields) {
      local({
        sf <- field
        plotname <- paste0("pieChart_", gsub(" ", "_", sf))
        output[[plotname]] <- renderPlot({
          data_subset <- industry_by_field %>% filter(StudyField == sf)
          if (nrow(data_subset) > 0) {
            pie(x = data_subset$Percentage, 
                labels = paste0(data_subset$Industry, " (", round(data_subset$Percentage, 1), "%)"),
                main = paste("Industry Distribution for", sf),
                col = rainbow(nrow(data_subset)))
          }
        })
      })
    }
  })
  
  # Render a table showing employment distribution by Study Field
  output$employmentByStudyField <- renderTable({
    # Split multiple employment statuses into separate rows
    employment_data <- data %>% separate_rows(Employment, sep = ";")
    # Filter out "Prefer not to say" if toggle is off
    if (!input$toggleNA) {
      employment_data <- employment_data %>% filter(Employment != "Prefer not to say")
    }
    employment_by_field <- employment_data %>%
      group_by(StudyField) %>%
      mutate(Total_StudyField = n()) %>%
      ungroup() %>%
      group_by(Employment, StudyField) %>%
      summarise(Count = n(),
                Total_StudyField = first(Total_StudyField), .groups = "drop") %>%
      mutate(Percentage = (Count / Total_StudyField) * 100) %>%
      select(Employment, StudyField, Percentage) %>%
      pivot_wider(names_from = StudyField, values_from = Percentage, values_fill = 0) %>%
      rowwise() %>%
      mutate(Average_Percentage = mean(c_across(-Employment), na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(desc(Average_Percentage)) %>%
      select(-Average_Percentage)
    employment_by_field
  })
  
  # Render a stacked bar chart for the top 10 Roles distribution by Study Field
  output$stacked_rolesByStudyField <- renderPlot({
    # Identify top 10 roles
    top_roles <- data %>%
      separate_rows(Role, sep = ";") %>%
      count(Role, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      pull(Role)
    
    # Filter data for these roles and compute counts by Study Field
    filtered_data <- data %>%
      separate_rows(Role, sep = ";") %>%
      filter(Role %in% top_roles) %>%
      count(StudyField, Role, name = "Count")
    
    ggplot(filtered_data, aes(x = reorder(StudyField, -Count), y = Count, fill = reorder(Role, -Count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Study Field", y = "Number of Respondents",
           title = "Top 10 Roles Distribution by Study Field", fill = "Role") +
      theme_minimal()
  })
  
  # Render a stacked bar chart for the top 10 Employment statuses distribution by Study Field
  output$stacked_employmentByStudyField <- renderPlot({
    # Identify top 10 employment statuses
    top_employment <- data %>%
      separate_rows(Employment, sep = ";") %>%
      count(Employment, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      pull(Employment)
    
    # Filter data for these statuses and compute counts by Study Field
    filtered_data <- data %>%
      separate_rows(Employment, sep = ";") %>%
      filter(Employment %in% top_employment) %>%
      count(StudyField, Employment, name = "Count")
    
    ggplot(filtered_data, aes(x = reorder(StudyField, -Count), y = Count, fill = reorder(Employment, -Count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Study Field", y = "Number of Respondents",
           title = "Top 10 Employment Distribution by Study Field", fill = "Employment Status") +
      theme_minimal()
  })
  
  output$userGuideContent <- renderUI({
  includeMarkdown("user_guide.md")
  })


}

shinyApp(ui, server)

