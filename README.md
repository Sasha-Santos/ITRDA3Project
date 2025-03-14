library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(tidyr)
library(rlang)  # Needed for sym() function

# Load Data
data <- read.csv("graduate_survey.csv", stringsAsFactors = FALSE)

# Ensure column names are correctly formatted (remove leading/trailing spaces)
names(data) <- trimws(names(data))

# Define pastel lilac shades
pastel_lilac_palette <- c("#D8BFD8", "#E6E6FA", "#C8A2C8", "#DDA0DD", "#BA55D3")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Eduvos Graduate Survey Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Programming Languages", tabName = "prog_lang", icon = icon("code")),
      menuItem("Databases", tabName = "databases", icon = icon("database")),
      menuItem("Web Frameworks", tabName = "web_frameworks", icon = icon("globe")),
      menuItem("AI Tools", tabName = "ai_tools", icon = icon("robot")),
      menuItem("Industry Trends", tabName = "industry", icon = icon("building")),
      menuItem("Employment Trends", tabName = "employment", icon = icon("briefcase")),
      menuItem("Education & Learning", tabName = "education", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              h2("Eduvos Graduate Survey Overview"),
              DTOutput("data_table")
      ),
      tabItem(tabName = "prog_lang",
              h2("Top Programming Languages"),
              plotOutput("prog_lang_plot")
      ),
      tabItem(tabName = "databases",
              h2("Top Databases"),
              plotOutput("database_plot")
      ),
      tabItem(tabName = "web_frameworks",
              h2("Top Web Frameworks"),
              plotOutput("web_framework_plot")
      ),
      tabItem(tabName = "ai_tools",
              h2("Top AI Tools"),
              plotOutput("ai_tool_plot")
      ),
      tabItem(tabName = "industry",
              h2("Industry Trends"),
              plotOutput("industry_plot"),
              plotOutput("job_role_plot"),
              plotOutput("salary_plot")
      ),
      tabItem(tabName = "employment",
              h2("Employment Trends"),
              plotOutput("employment_plot"),
              plotOutput("remote_work_plot"),
              plotOutput("experience_salary_plot")
      ),
      tabItem(tabName = "education",
              h2("Education & Learning"),
              plotOutput("learning_methods_plot"),
              plotOutput("coding_experience_plot")
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$data_table <- renderDT({ datatable(data) })
  
  render_bar_chart <- function(df_col, title) {
    if (!df_col %in% names(data)) {
      return(NULL)
    }
    plot_data <- data %>%
      filter(!is.na(.data[[df_col]]) & .data[[df_col]] != "") %>%
      separate_rows(!!sym(df_col), sep = ";") %>%
      count(!!sym(df_col), sort = TRUE) %>%
      head(10)
    
    ggplot(plot_data, aes(x = reorder(!!sym(df_col), n), y = n, fill = factor(!!sym(df_col)))) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = rep(pastel_lilac_palette, length.out = nrow(plot_data))) +
      theme_minimal() +
      labs(title = title, x = df_col, y = "Count")
  }
  
  render_pie_chart <- function(df_col, title) {
    if (!df_col %in% names(data)) {
      return(NULL)
    }
    plot_data <- data %>%
      filter(!is.na(.data[[df_col]]) & .data[[df_col]] != "") %>%
      separate_rows(!!sym(df_col), sep = ";") %>%
      count(!!sym(df_col), sort = TRUE) %>%
      head(10)
    
    ggplot(plot_data, aes(x = "", y = n, fill = factor(!!sym(df_col)))) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar(theta = "y") +
      scale_fill_manual(values = rep(pastel_lilac_palette, length.out = nrow(plot_data))) +
      theme_void() +
      labs(title = title, fill = df_col)
  }
  
  output$prog_lang_plot <- renderPlot({ render_bar_chart("ProgLang", "Top Programming Languages") })
  output$database_plot <- renderPlot({ render_bar_chart("Databases", "Top Databases") })
  output$web_framework_plot <- renderPlot({ render_pie_chart("WebFramework", "Top Web Frameworks") })
  output$ai_tool_plot <- renderPlot({ render_pie_chart("AITool", "Top AI Tools") })
  output$industry_plot <- renderPlot({ render_pie_chart("Industry", "Top Industries") })
  output$job_role_plot <- renderPlot({ render_pie_chart("Role", "Top Job Roles") })
  output$salary_plot <- renderPlot({
    ggplot(data, aes(x = ConvertedCompYearly)) +
      geom_density(fill = "#D8BFD8", color = "black", alpha = 0.6, na.rm = TRUE) +
      theme_minimal() +
      labs(title = "Salary Distribution", x = "Salary", y = "Density")
  })
  output$employment_plot <- renderPlot({
    emp_rate <- data %>% 
      filter(!is.na(Employment)) %>% 
      group_by(StudyField) %>% 
      summarise(employed = sum(grepl("Employed", Employment, ignore.case = TRUE), na.rm = TRUE) / n() * 100)
    if (nrow(emp_rate) == 0) {
      return(NULL)
    }
    ggplot(emp_rate, aes(x = reorder(StudyField, employed), y = employed, fill = StudyField)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = rep(pastel_lilac_palette, length.out = nrow(emp_rate))) +
      theme_minimal() +
      labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)")
  })
  output$remote_work_plot <- renderPlot({ render_pie_chart("RemoteWork", "Remote Work Trends") })
  output$experience_salary_plot <- renderPlot({
    ggplot(data, aes(x = YearsCodePro, y = ConvertedCompYearly)) +
      geom_point(alpha = 0.5, color = "#BA55D3", na.rm = TRUE) +
      theme_minimal() +
      labs(title = "Experience vs Salary", x = "Years of Professional Coding", y = "Salary")
  })
  output$learning_methods_plot <- renderPlot({ render_pie_chart("LearnCode", "Preferred Learning Methods") })
  output$coding_experience_plot <- renderPlot({ render_pie_chart("YearsCode", "Years of Coding Experience") })
}

# Run App
shinyApp(ui, server)





