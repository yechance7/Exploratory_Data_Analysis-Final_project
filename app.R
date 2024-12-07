library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Student Grade Analysis and Regression"),
  tabsetPanel(
    tabPanel(
      "Visualization",
      sidebarLayout(
        sidebarPanel(
          selectInput("subject1", "Subject", choices = c("Math", "Portuguese")),
          radioButtons("plotType", "Select Plot Type", choices = c("Bar Plot", "Box Plot", "Line Plot")),
          uiOutput("ageInputUI"),
          uiOutput("ageRangeUI"),
          HTML("G3 - final grade<br>numeric: from 0 to 20")
        ),
        mainPanel(
          plotOutput("score_plot"),
          fluidRow(
            column(6, tableOutput("data_count_F")),
            column(6, tableOutput("data_count_M"))
          )
        )
      )
    ),
    tabPanel(
      "Regression Analysis",
      sidebarLayout(
        sidebarPanel(
          HTML("G1 - first period grade<br>G2 - second period grade<br>G3 - final grade<br>numeric: from 0 to 20"),
          selectInput("subject2", "Subject", choices = c("Math" = "d1", "Portuguese" = "d2")),
          selectInput("dependent_var", "Dependent Variable (Y)", choices = c("G1", "G2", "G3")),
          selectizeInput("independent_vars", "Independent Variables (X)", choices = NULL, multiple = TRUE),
          actionButton("run_analysis", "Run Analysis")
        ),
        mainPanel(
          DTOutput("regression_table")
        )
      )
    ),
    tabPanel(
      "Additional Visualizations",
      sidebarLayout(
        sidebarPanel(
          selectInput("subject3", "Subject:", choices = c("Math" = "math", "Portuguese" = "portuguese")),
          selectInput("plot_type", "Visualization Type:",
                      choices = c("Boxplot" = "boxplot",
                                  "Density Plot" = "density",
                                  "Scatter Plot" = "scatter",
                                  "Histogram" = "histogram",
                                  "Violin Plot" = "violin",
                                  "Bar Plot" = "barplot")),
          selectInput("x_var", "X-axis Variable:", 
                      choices = c("Sex" = "sex", "Address" = "address", "Higher Education" = "higher"))
        ),
        mainPanel(
          plotOutput("plot"),
          tableOutput("data_count_by_x")
        )
      )
    ),
    tabPanel(
      "Relationship Between Final Grade and Categorical Variables",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "subject4", 
            "Choose Subject:",
            choices = c("Math", "Portuguese"),
            selected = "Math"
          ),
          selectInput(
            "fill_variable", 
            "Select Categorical Variable:",
            choices = c("Daily Alcohol Consumption" = "Dalc",
                        "Weekend Alcohol Consumption" = "Walc",
                        "Study Time" = "studytime",
                        "Mother's Education" = "Medu",
                        "Father's Education" = "Fedu",
                        "Gender" = "sex",
                        "Travel Time" = "traveltime",
                        "School" = "school",
                        "Family Size" = "famsize",
                        "Parental Status" = "Pstatus",
                        "Mother's Job" = "Mjob",
                        "Father's Job" = "Fjob",
                        "Family Relationship Quality" = "famrel",
                        "Free Time" = "freetime",
                        "Going Out" = "goout",
                        "Health" = "health",
                        "Absences" = "absences"),
            selected = "Dalc"
          )
        ),
        mainPanel(
          plotOutput("histogramPlot")
        )
      )
    ),
    # 다섯 번째 탭을 다른 탭 UI와 분리
    tabPanel(
      "Student Performance Visualization",
      sidebarLayout(
        sidebarPanel(
          selectInput("subject5", "Select Subject:", 
                      choices = list("Math" = "mat", "Portuguese" = "por")),
          helpText("Absences: Number of Absences, G3: Final Grade")
        ),
        mainPanel(
          plotOutput("scorePlot"),
          tableOutput("summaryTable")
        )
      )
    )
  )
)

  
# Server
server <- function(input, output, session) {
  # Load and preprocess data
  d1 <- reactive({ read.csv("data/student-mat.csv", sep = ";", header = TRUE) %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(age = as.integer(age), G1 = as.integer(G1), G2 = as.integer(G2), G3 = as.integer(G3), absences = as.integer(absences)) %>%
      mutate(subject = "Math")
  })
  d2 <- reactive({ read.csv("data/student-por.csv", sep = ";", header = TRUE) %>%
      mutate(across(where(is.character), as.factor)) %>%
    mutate(age = as.integer(age), G1 = as.integer(G1), G2 = as.integer(G2), G3 = as.integer(G3), absences = as.integer(absences)) %>%
    mutate(subject = "Portuguese")
  })
  
  d3 <- reactive({ read.csv("data/student-mat.csv", sep = ";")%>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.integer), as.factor)) %>%
      mutate(across(c(age, G1, G2, G3, absences), as.integer)) %>%
      mutate(subject = "Math") 
  })
  
  d4 <- reactive({ read.csv("data/student-por.csv", sep = ";")%>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.integer), as.factor)) %>%
      mutate(across(c(age, G1, G2, G3, absences), as.integer)) %>%
      mutate(subject = "Portuguese") 
  })
  # 첫 번째 탭에서 선택한 데이터셋
  selected_data_1 <- reactive({
    req(input$subject1)  # dataset이 선택된 후에만 작동
    if (input$subject1 == "Math") {
      return(d1())  # Math 선택 시 d1
    } else {
      return(d2())  # Portuguese 선택 시 d2
    }
  })
  # 두 번째 탭에서 선택한 데이터셋
  selected_data_2 <- reactive({
    req(input$subject2)  # dataset_choice가 선택된 후에만 작동
    if (input$subject2 == "d1") {
      return(d1())  # Math 선택 시 d1
    } else {
      return(d2())  # Portuguese 선택 시 d2
    }
  })
  
  # 세 번째 탭에서 선택한 데이터셋
  selected_data_3 <- reactive({
    req(input$subject3)  # subject가 선택된 후에만 작동
    if (input$subject3 == "math") {
      return(d1())  # Math 선택 시 d1
    } else {
      return(d2())  # Portuguese 선택 시 d2
    }
  })  
  
  #네 번째 탭에서 선택한 데이터셋 
  selected_data_4 <- reactive({
    req(input$subject4)  # subject가 선택된 후에만 작동
    if (input$subject4 == "math") {
      return(d3())  # Math 선택 시 d3
    } else {
      return(d4())  # Portuguese 선택 시 d4
    }
  })  
  # 다섯 번째 탭에서 선택한 데이터셋
  selected_data_5 <- reactive({
    req(input$subject5)  # subject가 선택된 후에만 작동
    if (input$subject5 == "math") {
      return(d1())  # Math 선택 시 d1
    } else {
      return(d2())  # Portuguese 선택 시 d2
    }
  })  
  
  observe({
    req(selected_data_2())
    excluded_vars <- switch(input$dependent_var,
                            "G1" = c("G1", "G2", "G3"),
                            "G2" = c("G2", "G3"),
                            "G3" = c("G3"))
    available_vars <- setdiff(names(selected_data_2()), excluded_vars)
    updateSelectizeInput(session, "independent_vars", 
                         choices = available_vars, 
                         selected = available_vars[1:min(2, length(available_vars))])
  })
  
  # 1번째 탭에서 사용한 그래프 출력
  output$score_plot <- renderPlot({
    req(selected_data_1())  
    data <- selected_data_1()
    
    if (input$plotType == "Bar Plot") {
      req(input$Age)  
      filtered_data <- data %>%
        filter(age == input$Age) %>%
        group_by(sex) %>%
        summarise(mean_score = mean(G3, na.rm = TRUE), .groups = "drop")
      
      min_score <- min(filtered_data$mean_score, na.rm = TRUE)
      max_score <- max(filtered_data$mean_score, na.rm = TRUE)
      
      bar_plot <- ggplot(filtered_data, aes(x = sex, y = mean_score, fill = sex)) +
        geom_bar(stat = "identity") +
        labs(
          title = paste(input$Age, "Years Old Average Grade of Final (G3)"),
          x = "Sex",
          y = "Final Grade (G3)"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
      
      if (min_score != max_score) {
        bar_plot <- bar_plot +
          geom_hline(yintercept = min_score, linetype = "dashed", color = "red", linewidth = 0.3)
      }
      
      return(bar_plot)
      
    } else if (input$plotType == "Box Plot") {
      req(input$Age)  
      filtered_data <- data %>%
        filter(age == input$Age)
      
      ggplot(filtered_data, aes(x = sex, y = G3, fill = sex)) +
        geom_boxplot() +
        theme_minimal()
      
    } else if (input$plotType == "Line Plot") {
      req(input$ageRange)  
      filtered_data <- data %>%
        filter(age >= input$ageRange[1], age <= input$ageRange[2]) %>%
        group_by(age, sex) %>%
        summarise(mean_score = mean(G3, na.rm = TRUE), .groups = "drop")
      
      ggplot(filtered_data, aes(x = age, y = mean_score, color = sex, group = sex)) +
        geom_line() +
        geom_point() +
        theme_minimal()
    }
  })
  
  # Dynamic UI for age selection
  output$ageInputUI <- renderUI({
    if (input$plotType %in% c("Bar Plot", "Box Plot")) {
      numericInput("Age", "Student Age", value = 15, min = 15, max = 22, step = 1)
    }
  })
  
  output$ageRangeUI <- renderUI({
    if (input$plotType == "Line Plot") {
      sliderInput("ageRange", "Select Age Range", min = 15, max = 22, value = c(15, 22))
    }
  })
  
  # Count data by sex
  output$data_count_F <- renderTable({
    req(selected_data_1())
    if (input$plotType %in% c("Bar Plot", "Box Plot")) {
      req(input$Age)
      filtered_data <- selected_data_1() %>%
        filter(age == input$Age, sex == "F")
      data.frame("Sex" = "F", "Count" = nrow(filtered_data))
    } else if (input$plotType == "Line Plot") {
      req(input$ageRange)
      filtered_data <- selected_data_1() %>%
        filter(age >= input$ageRange[1], age <= input$ageRange[2], sex == "F") %>%
        group_by(age) %>%
        summarise(count = n(), .groups = "drop")
    }
  })
  
  output$data_count_M <- renderTable({
    req(selected_data_1())
    if (input$plotType %in% c("Bar Plot", "Box Plot")) {
      req(input$Age)
      filtered_data <- selected_data_1() %>%
        filter(age == input$Age, sex == "M")
      data.frame("Sex" = "M", "Count" = nrow(filtered_data))
    } else if (input$plotType == "Line Plot") {
      req(input$ageRange)
      filtered_data <- selected_data_1() %>%
        filter(age >= input$ageRange[1], age <= input$ageRange[2], sex == "M") %>%
        group_by(age) %>%
        summarise(count = n(), .groups = "drop")
    }
  })
  
  # Generate plots for Additional Visualizations
  output$plot <- renderPlot({
    req(selected_data_3())
    data <- selected_data_3()
    
    if (input$plot_type == "boxplot") {
      ggplot(data, aes_string(x = input$x_var, y = "G3")) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Boxplot", x = input$x_var, y = "Final Grade (G3)")
    } else if (input$plot_type == "density") {
      ggplot(data, aes_string(x = "G3", fill = input$x_var)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot", x = "Final Grade (G3)", fill = input$x_var)
    } else if (input$plot_type == "scatter") {
      ggplot(data, aes_string(x = input$x_var, y = "G3", color = input$x_var)) +
        geom_jitter(width = 0.2, height = 0.2) +
        labs(title = "Scatter Plot", x = input$x_var, y = "Final Grade (G3)")
    } else if (input$plot_type == "histogram") {
      ggplot(data, aes_string(x = "G3")) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        labs(title = "Histogram", x = "Final Grade (G3)", y = "Frequency")
    } else if (input$plot_type == "violin") {
      ggplot(data, aes_string(x = input$x_var, y = "G3", fill = input$x_var)) +
        geom_violin() +
        labs(title = "Violin Plot", x = input$x_var, y = "Final Grade (G3)")
    } else if (input$plot_type == "barplot") {
      ggplot(data, aes_string(x = input$x_var, fill = input$x_var)) +
        geom_bar() +
        labs(title = "Bar Plot", x = input$x_var, y = "Count")
    }
  })
  
  # Display count of X-axis variable
  output$data_count_by_x <- renderTable({
    req(selected_data_3())
    data <- selected_data_3()
    
    count_data <- data %>%
      group_by_at(input$x_var) %>%
      summarise(count = n(), .groups = "drop")
    
    return(count_data)
  })
  
  # Regression analysis
  regression_result <- eventReactive(input$run_analysis, {
    req(input$dependent_var, input$independent_vars)
    formula <- as.formula(paste(input$dependent_var, "~", paste(input$independent_vars, collapse = " + ")))
    model <- lm(formula, data = selected_data_2())
    coefficients <- as.data.frame(summary(model)$coefficients)
    coefficients <- tibble::rownames_to_column(coefficients, "Variable")
    coefficients <- coefficients %>%
      mutate(across(where(is.numeric), ~ round(., 7)))  # Round numeric values to 7 decimal places
    return(coefficients)
  })
  
  output$regression_table <- renderDT({
    req(regression_result())
    datatable(regression_result(), options = list(pageLength = 5))
  })
  # Mosaic Plot
  output$histogramPlot <- renderPlot({
    req(selected_data_4())  
    data <- selected_data_4()  
    
    ggplot(data, aes(x = G3, fill = .data[[input$fill_variable]])) +
      geom_histogram(breaks = seq(0, 18, 2), position = "fill") +
      xlab("Final Grade") +
      labs(fill = input$fill_variable) +
      theme_minimal()
  })
  output$scorePlot <- renderPlot({
    data <- selectedData()
    ggplot(data, aes(x = absences, y = G3)) +
      geom_point(color = "blue", size = 3, alpha = 0.6) +
      labs(
        title = "Scatter Plot: Absences vs Final Grade", 
        x = "Number of Absences", 
        y = "Final Grade (G3)"
      )
  })
  
  # 다섯 번째 탭
  output$scorePlot <- renderPlot({
    data <- selected_data_5()
    ggplot(data, aes(x = absences, y = G3)) +
      geom_point(color = "blue", size = 3, alpha = 0.6) +
      labs(
        title = "Scatter Plot: Absences vs Final Grade", 
        x = "Number of Absences", 
        y = "Final Grade (G3)"
      )
  })
  
  # 요약 테이블 생성
  output$summaryTable <- renderTable({
    data <- selected_data_5()
    summary_data <- data %>%
      group_by(absences) %>%
      summarise(
        `Average Grade` = round(mean(G3, na.rm = TRUE), 2),
        Count = n()
      ) %>%
      arrange(absences)
    summary_data
  })
}

shinyApp(ui, server)
