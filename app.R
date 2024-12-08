  library(shiny)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # UI
  ui <- fluidPage(
    titlePanel("Exploring Student Grades"),
    # Tab panel for different sections
    tabsetPanel(
      tabPanel("1. Summary Statistics",
      sidebarLayout(
        sidebarPanel(
          selectInput("subject1", "Select Subject:", 
                      choices = c("Math" = "math", "Portuguese" = "portuguese")),
          selectInput("category1", "Select Variable:", 
                      choices = c(
                        "School" = "school",
                        "Gender" = "sex",
                        "Age" = "age",
                        "Home Address" = "address",
                        "Family Size" = "famsize",
                        "Parental Status" = "Pstatus",
                        "Mother's Education" = "Medu",
                        "Father's Education" = "Fedu",
                        "Mother's Job" = "Mjob",
                        "Father's Job" = "Fjob",
                        "Reason for Choosing School" = "reason",
                        "Guardian" = "guardian",
                        "Travel Time" = "traveltime",
                        "Study Time" = "studytime",
                        "Number of Failures" = "failures",
                        "Extra Educational Support" = "schoolsup",
                        "Family Support" = "famsup",
                        "Paid Classes" = "paid",
                        "Extracurricular Activities" = "activities",
                        "Attended Nursery School" = "nursery",
                        "Higher Education Intention" = "higher",
                        "Internet Access" = "internet",
                        "In a Romantic Relationship" = "romantic",
                        "Family Relationship Quality" = "famrel",
                        "Free Time After School" = "freetime",
                        "Going Out with Friends" = "goout",
                        "Weekday Alcohol Consumption" = "Dalc",
                        "Weekend Alcohol Consumption" = "Walc",
                        "Health Status" = "health",
                        "Number of Absences" = "absences"
                      )
          ) ,
          HTML("G3 - final grade<br>numeric: from 0 to 20")         
        ),
        mainPanel(
          h4("Variable Description"),
          verbatimTextOutput("description"),
          h4("Variable Statistics"),
          tableOutput("stats_table"),
          h4("G3 Statistics"),
          tableOutput("g3_summary"),
          )
        )
      ),
      tabPanel(
        "2. Regression Analysis",
        sidebarLayout(
          sidebarPanel(
            HTML("<b>G1</b> - First semester grade<br><b>G2</b> - Second semester grade<br><b>G3</b> - Final grade<br>"),
            selectInput("subject2", "Select Subject", choices = c("Math" = "math", "Portuguese" = "portuguese")),
            selectInput("dependent_var", "Dependent Variable (Y)", choices = c("G3")),
            selectizeInput("independent_vars", "Independent Variables (X)", choices = NULL, multiple = TRUE),
            actionButton("run_analysis", "Run Analysis")
          ),
          mainPanel(
            DTOutput("regression_table")
          )
        )
      ),
      
      tabPanel(
        "3. Grades Across Binary Variables",
        sidebarLayout(
          sidebarPanel(
            selectInput("subject3", "Select Subject", choices = c("Math" = "math", "Portuguese" = "portuguese")),
            selectInput("plot_type", "Select Plot Type", 
                        choices = c("Box Plot" = "boxplot",
                                    "Density Plot" = "density",
                                    "Scatter Plot" = "scatter",
                                    "Violin Plot" = "violin",
                                    "Bar Plot" = "barplot")),
            selectInput("x_var", "Independent Variables (X)", 
                        choices = c("Gender" = "sex", "Address" = "address", "Higher Education" = "higher"))
          ),
          mainPanel(
            plotOutput("plot"),
            tableOutput("data_count_by_x")
          )
        )
      ),
      
      tabPanel(
        "4. Grades Across Categorical Variables",
        sidebarLayout(
          sidebarPanel(
            selectInput("subject4", "Select Subject", choices = c("Math" = "math", "Portuguese" = "portuguese")),
            selectInput("fill_variable", "Select Categorical Variable", choices = c(
              "Weekend Alcohol Consumption" = "Dalc",
              "Weekday Alcohol Consumption" = "Walc",
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
              "Outing Frequency" = "goout",
              "Health Status" = "health",
              "Absences" = "absences"
            ), selected = "Dalc")
          ),
          mainPanel(
            plotOutput("histogramPlot")
          )
        )
      ),
      
      tabPanel(
        "5. Grades by Absences",
        sidebarLayout(
          sidebarPanel(
            selectInput("subject5", "Select Subject", choices = list("Math" = "math", "Portuguese" = "portuguese")),
            helpText("Absences: Number of absences, G3: Final grade")
          ),
          mainPanel(
            plotOutput("scorePlot"),
            tableOutput("summaryTable")
          )
        )
      ),
      
      tabPanel(
        "6. Grades by Age and Gender",
        sidebarLayout(
          sidebarPanel(
            selectInput("subject6", "Select Subject", choices = c("Math" = "math", "Portuguese" = "portuguese")),
            radioButtons("plotType", "Select Plot Type", choices = c("Bar Plot", "Box Plot", "Line Plot")),
            uiOutput("ageInputUI"),
            uiOutput("ageRangeUI"),
            HTML("<br> G3 - Final grade (0-20)"),
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
        "7. Interaction Between Study Time and Categorical Variables",
        sidebarLayout(
          sidebarPanel(
            HTML(paste(
              "<b>Category Information:</b><br>",
              "- <b>Study Time:</b> Weekly study time (1 - <2 hrs, 4 - >10 hrs).<br>",
              "- <b>G3:</b> Final grade (0-20).<br>",
              "<hr>"
            )),
            selectInput("subject7", 
                        label = "Select Subject", 
                        choices = c("Math" = "math", "Portuguese" = "portuguese"),
                        selected = "math"),
            selectInput("category", 
                        label = "Category to Highlight", 
                        choices = c(
                          "Weekend Alcohol Consumption" = "Walc",
                          "Weekday Alcohol Consumption" = "Dalc",
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
                          "Outing Frequency" = "goout",
                          "Health Status" = "health"
                        ),
                        selected = "famrel")
          ),
          mainPanel(
            plotOutput("performancePlot"),
            HTML("<p><b>Note:</b> The columns in the table below represent <b><i>mean(count)</i></b>.</p>"),
            tableOutput("categoryStats")
          )
        )
      )
    )
  )
  
# Server
  server <- function(input, output, session) {
    # Load and preprocess data
    d1 <- reactive({ read.csv("data/student-mat.csv", sep = ";")%>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(across(where(is.integer), as.factor)) %>%
        mutate(across(c(age, G1, G2, G3, absences), as.character)) %>%
        mutate(across(c(age, G1, G2, G3, absences), as.integer)) %>%
        mutate(subject = "Math") 
    })
    
    d2 <- reactive({ read.csv("data/student-por.csv", sep = ";")%>%
        mutate(across(where(is.character), as.factor)) %>%
        mutate(across(where(is.integer), as.factor)) %>%
        mutate(across(c(age, G1, G2, G3, absences), as.character)) %>%
        mutate(across(c(age, G1, G2, G3, absences), as.integer)) %>%
        mutate(subject = "Math") 
    })
    # Selected_subject1  
    selected_data_1 <- reactive({
      req(input$subject1)
      if (input$subject1 == "math") {
        return(d1())   
      } else {
        return(d2())   
      }
    })
    
    # Selected_subject2 
    selected_data_2 <- reactive({
      req(input$subject2)
      if (input$subject2 == "math") {
        return(d1())   
      } else {
        return(d2())   
      }
    })
    
    # Selected_subject3    
    selected_data_3 <- reactive({
      req(input$subject3)
      if (input$subject3 == "math") {
        return(d1())   
      } else {
        return(d2())   
      }
    })  
    
    # Selected_subject4   
    selected_data_4 <- reactive({
      req(input$subject4)
      if (input$subject4 == "math") {
        return(d1())   
      } else {
        return(d2())   
      }
    })  
    # Selected_subject5   
    selected_data_5 <- reactive({
      req(input$subject5)   
      if (input$subject5 == "math") {
        return(d1())  
      } else {
        return(d2()) 
      }
      
    }) 
    # Selected_subject6    
    selected_data_6 <- reactive({
      req(input$subject6) 
      if (input$subject6 == "math") {
        return(d1())  
      } else {
        return(d2())  
      }
    })
    # Selected_subject7
    selected_data_7 <- reactive({
      req(input$subject6)  
      if (input$subject6 == "math") {
        return(d1())  
      } else {
        return(d2())  
      }
    })
    
    # 1. Summary 
    # Variable descriptions
    descriptions <- list(
      school = "Student's school: 'GP' - Gabriel Pereira, 'MS' - Mousinho da Silveira",
      sex = "Student's sex: 'F' - female, 'M' - male",
      age = "Student's age: numeric, from 15 to 22",
      address = "Home address type: 'U' - urban, 'R' - rural",
      famsize = "Family size: 'LE3' - less or equal to 3, 'GT3' - greater than 3",
      Pstatus = "Parental cohabitation status: 'T' - together, 'A' - apart",
      Medu = "Mother's education: 0 - none to 4 - higher education",
      Fedu = "Father's education: 0 - none to 4 - higher education",
      Mjob = "Mother's job: 'teacher', 'health', 'civil services', 'at_home', 'other'",
      Fjob = "Father's job: 'teacher', 'health', 'civil services', 'at_home', 'other'",
      reason = "Reason for choosing the school: 'home', 'reputation', 'course', 'other'",
      guardian = "Student's guardian: 'mother', 'father', 'other'",
      traveltime = "Travel time to school: 1 - <15 min to 4 - >1 hour",
      studytime = "Weekly study time: 1 - <2 hrs to 4 - >10 hrs",
      failures = "Number of past failures: 0 - 4",
      schoolsup = "Extra educational support: 'yes' or 'no'",
      famsup = "Family educational support: 'yes' or 'no'",
      paid = "Extra paid classes: 'yes' or 'no'",
      activities = "Extracurricular activities: 'yes' or 'no'",
      nursery = "Attended nursery school: 'yes' or 'no'",
      higher = "Wants higher education: 'yes' or 'no'",
      internet = "Internet access at home: 'yes' or 'no'",
      romantic = "In a romantic relationship: 'yes' or 'no'",
      famrel = "Quality of family relationships: 1 - very bad to 5 - excellent",
      freetime = "Free time after school: 1 - very low to 5 - very high",
      goout = "Going out with friends: 1 - very low to 5 - very high",
      Dalc = "Weekday alcohol consumption: 1 - very low to 5 - very high",
      Walc = "Weekend alcohol consumption: 1 - very low to 5 - very high",
      health = "Current health status: 1 - very bad to 5 - very good",
      absences = "Number of absences: from 0 to 93"
    )
    
    # Show variable description
    output$description <- renderText({
      req(input$category1)
      descriptions[[input$category1]]
    })
    
    # Calculate selected variable statistics
    output$stats_table <- renderTable({
      req(input$subject1, input$category1)
      data <- if (input$subject1 == "math") d1() else d2()
      
      data %>%
        group_by(.data[[input$category1]]) %>%
        summarise(
          Count = n(),
          Mean = round(mean(G3, na.rm = TRUE), 2),
          SD = round(sd(G3, na.rm = TRUE), 2),
          Min = min(G3, na.rm = TRUE),
          Max = max(G3, na.rm = TRUE)
        )
    })
    
    # Calculate G3 summary statistics
    output$g3_summary <- renderTable({
      req(input$subject1)
      data <- if (input$subject1 == "math") d1() else d2()
      
      data %>%
        summarise(
          Count = n(),
          Mean = round(mean(G3, na.rm = TRUE), 2),
          SD = round(sd(G3, na.rm = TRUE), 2),
          Min = min(G3, na.rm = TRUE),
          Max = max(G3, na.rm = TRUE)
          
        )
    })

    # 2. Regrssion Analysis
    observe({
      req(selected_data_2())
      excluded_vars <- switch(input$dependent_var,
                              "G3" = c("G3"))
      available_vars <- setdiff(names(selected_data_2()), c(excluded_vars,"subject"))
      updateSelectizeInput(session, "independent_vars", 
                           choices = available_vars, 
                           selected = available_vars[1:min(2, length(available_vars))])
    })
    
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
    
    # 3. Grades(Binary)
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
    
    # 4. Grades(Categorical)
    output$histogramPlot <- renderPlot({
      req(selected_data_4())  
      data <- selected_data_4()  
      
      ggplot(data, aes(x = G3, fill = .data[[input$fill_variable]])) +
        geom_histogram(breaks = seq(0, 20, 3), position = "fill") +
        xlab("Final Grade") +
        labs(fill = input$fill_variable) +
        theme_minimal()
    })
    
    # 5. Grades(Absences)
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
    
    # 6. Grades(Age vs Gender)
    output$score_plot <- renderPlot({
      req(selected_data_6())  
      data <- selected_data_6()
      
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
      req(selected_data_6())
      if (input$plotType %in% c("Bar Plot", "Box Plot")) {
        req(input$Age)
        filtered_data <- selected_data_6() %>%
          filter(age == input$Age, sex == "F")
        data.frame("Sex" = "F", "Count" = nrow(filtered_data))
      } else if (input$plotType == "Line Plot") {
        req(input$ageRange)
        filtered_data <- selected_data_6() %>%
          filter(age >= input$ageRange[1], age <= input$ageRange[2], sex == "F") %>%
          group_by(age) %>%
          summarise(count = n(), .groups = "drop")
      }
    })
    
    output$data_count_M <- renderTable({
      req(selected_data_6())
      if (input$plotType %in% c("Bar Plot", "Box Plot")) {
        req(input$Age)
        filtered_data <- selected_data_6() %>%
          filter(age == input$Age, sex == "M")
        data.frame("Sex" = "M", "Count" = nrow(filtered_data))
      } else if (input$plotType == "Line Plot") {
        req(input$ageRange)
        filtered_data <- selected_data_6() %>%
          filter(age >= input$ageRange[1], age <= input$ageRange[2], sex == "M") %>%
          group_by(age) %>%
          summarise(count = n(), .groups = "drop")
      }
    })
    
    # 7. Interaction(Study time - Categorical )
    output$performancePlot <- renderPlot({
      data <- if (input$subject7 == "math") {
        d1()
      } else {
        d2()
      }
      
      category <- input$category
      
      category_label <- switch(category,
                               "famrel" = "Family Relationship Quality",
                               "goout" = "Outing Frequency",
                               "Dalc" = "Weekday Alcohol Consumption",
                               "Walc" = "Weekend Alcohol Consumption",
                               "health" = "Health Status",
                               "freetime" = "Free Time After School",
                               "traveltime" = "Travel Time",
                               "Medu" = "Mother's Education",
                               "Fedu" = "Father's Education",
                               "sex" = "Gender",
                               "school" = "School",
                               "famsize" = "Family Size",
                               "Pstatus" = "Parental Status",
                               "Mjob" = "Mother's Job",
                               "Fjob" = "Father's Job",
                               category)
      
      ggplot(data, aes(x = studytime, y = G3, group = factor(get(category)), color = factor(get(category)))) +
        stat_summary(fun = mean, geom = "line", size = 1) +
        stat_summary(fun = mean, geom = "point", size = 3) +
        labs(
          title = paste("Average Final Grade by Study Time for", category_label),
          x = "Study Time (1-4 Levels)",
          y = "Average Final Grade (G3)",
          color = paste(category_label, "Levels")
        ) +
        theme_minimal()
    })
    
    output$categoryStats <- renderTable({
      data <- if (input$subject7 == "math") {
        d1()
      } else {
        d2()
      }
      category <- input$category
      stats <- data %>%
        group_by(studytime, !!sym(category)) %>%
        summarise(
          Mean = mean(G3, na.rm = TRUE),
          Count = n(),
          .groups = 'drop'
        ) %>%
        mutate(Mean_Count = paste0(round(Mean, 2), " (", Count, ")")) %>%
        select(-Mean, -Count) %>%
        pivot_wider(
          names_from = studytime,
          values_from = Mean_Count,
          names_prefix = "Studytime "
        )
      
      first_col_name <- switch(category,
                               "famrel" = "Family Relationship Quality<br>(1: very bad - 5: excellent)",
                               "goout" = "Outing Frequency<br>(1: very low - 5: very high)",
                               "Dalc" = "Workday Alcohol Consumption<br>(1: very low - 5: very high)",
                               "Walc" = "Weekend Alcohol Consumption<br>(1: very low - 5: very high)",
                               "health" = "Health Status<br>(1: very bad - 5: very good)",
                               "freetime" = "Free Time After School<br>(1: very low - 5: very high)",
                               "traveltime" = "Travel Time<br>(1: <15 min - 4: >1 hour)",
                               "Medu" = "Mother's Education<br>(0: none - 4: higher education)",
                               "Fedu" = "Father's Education<br>(0: none - 4: higher education)",
                               "sex" = "Gender<br>(F: female, M: male)",
                               "school" = "School<br>(GP: Gabriel Pereira, MS: Mousinho da Silveira)",
                               "famsize" = "Family Size<br>(LE3: <=3, GT3: >3)",
                               "Pstatus" = "Parental Status<br>(T: together, A: apart)",
                               "Mjob" = "Mother's Job<br>(teacher, health, services, at_home, other)",
                               "Fjob" = "Father's Job<br>(teacher, health, services, at_home, other)",
                               category)
      
      colnames(stats) <- c(first_col_name, "Studytime 1", "Studytime 2", "Studytime 3", "Studytime 4")
      stats
    }, sanitize.text.function = function(x) x, align = "c")
  }
  

shinyApp(ui, server)