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
                             choices = c("Math" = "math", "Language" = "portuguese")),
                 selectInput("category1", "Select Variable:", 
                             choices = c(
                               "Sex" = "sex",
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
                             ),
                             selected = "Medu"
                 ),
                 p(HTML("<ul>
        <li><b><i>Notes</i></b>
        <li>The first table shows the count of the selected variable and the summary statistics of student grade for each of its values.</li>
        <li>The second table shows the summary statistics of 'Grade', our main response variable.</li>
      </ul>"))
               ),
               mainPanel(
                 h4("Variable Description"),
                 verbatimTextOutput("description"),
                 h4("Summary Statistics of Selected Variable"),
                 tableOutput("stats_table"),
                 h4("Summary Statistics of Grade"),
                 tableOutput("g3_summary")
               )
             )
    ),
    tabPanel("2. Regression Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject2", "Select Subject", 
                             choices = c("Math" = "math", "Language" = "portuguese")),
                 selectizeInput("independent_vars", "Independent Variables (X)", 
                                choices = NULL, multiple = TRUE),
                 actionButton("run_analysis", "Run Analysis")
               ),
               mainPanel(
                 DTOutput("regression_table")
               )
             )
    ),
    
    tabPanel("3. Grades Across Binary Variables",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject3", "Select Subject", choices = c("Math" = "math", "Language" = "portuguese")),
                 selectInput("plot_type", "Select Plot Type", 
                             choices = c("Density Plot" = "density",
                                         "Scatter Plot" = "scatter",
                                         "Violin Plot" = "violin",
                                         "Bar Plot" = "barplot",
                                         "Box Plot" = "boxplot"), selected="density"),
                 selectInput("x_var", "Independent Variables (X)", 
                             choices = c("Sex" = "sex",
                                         "Home Address" = "address",
                                         "Family Size" = "famsize",
                                         "Parental Status" = "Pstatus",
                                         "Extra Educational Support" = "schoolsup",
                                         "Family Support" = "famsup",
                                         "Paid Classes" = "paid",
                                         "Extracurricular Activities" = "activities",
                                         "Attended Nursery School" = "nursery",
                                         "Higher Education Intention" = "higher",
                                         "Internet Access" = "internet",
                                         "In a Romantic Relationship" = "romantic"), selected="higher")
               ),
               mainPanel(
                 plotOutput("plot"),
                 tableOutput("data_count_by_x")
               )
             )
    ),
    
    tabPanel("4. Grades Across Categorical Variables",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject4", "Select Subject", choices = c("Math" = "math", "Language" = "portuguese")),
                 selectInput("fill_variable", "Select Categorical Variable", choices = c(
                   "Mother's Education" = "Medu",
                   "Father's Education" = "Fedu",
                   "Mother's Job" = "Mjob",
                   "Father's Job" = "Fjob",
                   "Reason for Choosing School" = "reason",
                   "Guardian" = "guardian",
                   "Travel Time" = "traveltime",
                   "Study Time" = "studytime",
                   "Number of Failures" = "failures",
                   "Family Relationship Quality" = "famrel",
                   "Free Time After School" = "freetime",
                   "Going Out with Friends" = "goout",
                   "Weekday Alcohol Consumption" = "Dalc",
                   "Weekend Alcohol Consumption" = "Walc",
                   "Health Status" = "health"
                 ), selected = "Medu")
               ),
               mainPanel(
                 plotOutput("histogramPlot")
               )
             )
    ),
    
    tabPanel("5. Grades by Absences",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject5", "Select Subject", choices = list("Math" = "math", "Language" = "portuguese"))
               ),
               mainPanel(
                 plotOutput("scorePlot"),
                 tableOutput("summaryTable")
               )
             )
    ),
    
    tabPanel("6. Grades by Age and Sex",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject6", "Select Subject", choices = c("Math" = "math", "Language" = "portuguese")),
                 radioButtons("plotType", "Select Plot", choices = c("Bar Plot", "Box Plot", "Line Plot"), selected="Line Plot"),
                 uiOutput("ageInputUI"),
                 uiOutput("ageRangeUI")
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
    
    tabPanel("7. Interaction Between Study Time and Categorical Variables",
             sidebarLayout(
               sidebarPanel(
                 HTML(paste(
                   "<b>Weekly Study Time:</b><br>",
                   "- '1' if less than 2 hours<br>",
                   "- '2' if between 2 and 5 hours<br>",
                   "- '3' if between 5 and 10 hours<br>",
                   "- '4' if more than 10 hours<br>",
                   "<hr>"
                 )),
                 selectInput("subject7", 
                             label = "Select Subject", 
                             choices = c("Math" = "math", "Language" = "portuguese"),
                             selected = "math"),
                 selectInput("category", 
                             label = "Category to Highlight", 
                             choices = c(
                               "Sex" = "sex",
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
                               "Health Status" = "health"
                             ),
                             selected = "Mjob")
               ),
               mainPanel(
                 plotOutput("performancePlot"),
                 HTML("<p><b><i>Note:</i></b> The numbers in the table below represents <b><i>average grade (count)</i></b>.</p>"),
                 tableOutput("categoryStats")
               )
             )
    ),
    
    # 8. Drinking by Age
    tabPanel(
      "8. Drinking by Age",
      sidebarLayout(
        sidebarPanel(
          selectInput("subject8", "Select Subject", choices = c("Math" = "math", "Language" = "portuguese")),
          selectInput("days", "Select Days", 
                      choices = c("Weekday Alcohol Consumption" = "Dalc", 
                                  "Weekend Alcohol Consumption" = "Walc"),
                      selected = "Dalc"),
          p(HTML("<ul>
      <li><b><i>Note:</i></b> In Portugal, age 18 is the legal drinking age.</li>
    </ul>"))
        ),
        mainPanel(
          h4("Variable Description"),
          verbatimTextOutput("description8"),
          plotOutput("drinkingAgePlot"),
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Define a mapping from variable codes to descriptive labels
  var_labels <- list(
    # Section 1 and others
    sex = "Sex",
    age = "Age",
    address = "Home Address",
    famsize = "Family Size",
    Pstatus = "Parental Status",
    Medu = "Mother's Education",
    Fedu = "Father's Education",
    Mjob = "Mother's Job",
    Fjob = "Father's Job",
    reason = "Reason for Choosing School",
    guardian = "Guardian",
    traveltime = "Travel Time",
    studytime = "Study Time",
    failures = "Number of Failures",
    schoolsup = "Extra Educational Support",
    famsup = "Family Support",
    paid = "Paid Classes",
    activities = "Extracurricular Activities",
    nursery = "Attended Nursery School",
    higher = "Higher Education Intention",
    internet = "Internet Access",
    romantic = "In a Romantic Relationship",
    famrel = "Family Relationship Quality",
    freetime = "Free Time After School",
    goout = "Going Out with Friends",
    Dalc = "Weekday Alcohol Consumption",
    Walc = "Weekend Alcohol Consumption",
    health = "Health Status",
    absences = "Number of Absences"
  )
  
  # Load and preprocess data
  d1 <- reactive({ 
    read.csv("data/student-mat.csv", sep = ";") %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.integer), as.factor)) %>%
      mutate(across(c(age, G1, G2, G3, absences), as.character)) %>%
      mutate(across(c(age, G1, G2, G3, absences), as.integer)) %>%
      mutate(subject = "Math") 
  })
  
  d2 <- reactive({ 
    read.csv("data/student-por.csv", sep = ";") %>%
      mutate(across(where(is.character), as.factor)) %>%
      mutate(across(where(is.integer), as.factor)) %>%
      mutate(across(c(age, G1, G2, G3, absences), as.character)) %>%
      mutate(across(c(age, G1, G2, G3, absences), as.integer)) %>%
      mutate(subject = "Language")
  })
  
  # Reactive expressions for each section's data
  selected_data_1 <- reactive({
    req(input$subject1)
    if (input$subject1 == "math") {
      return(d1())   
    } else {
      return(d2())   
    }
  })
  
  selected_data_2 <- reactive({
    req(input$subject2)
    if (input$subject2 == "math") {
      return(d1())   
    } else {
      return(d2())   
    }
  })
  
  selected_data_3 <- reactive({
    req(input$subject3)
    if (input$subject3 == "math") {
      return(d1())   
    } else {
      return(d2())   
    }
  })  
  
  selected_data_4 <- reactive({
    req(input$subject4)
    if (input$subject4 == "math") {
      return(d1())   
    } else {
      return(d2())   
    }
  })  
  
  selected_data_5 <- reactive({
    req(input$subject5)   
    if (input$subject5 == "math") {
      return(d1())  
    } else {
      return(d2()) 
    }
  }) 
  
  selected_data_6 <- reactive({
    req(input$subject6) 
    if (input$subject6 == "math") {
      return(d1())  
    } else {
      return(d2())  
    }
  })
  
  selected_data_7 <- reactive({
    req(input$subject7) 
    if (input$subject7 == "math") {
      return(d1())  
    } else {
      return(d2())  
    }
  })
  
  selected_data_8 <- reactive({
    req(input$subject8)
    if (input$subject8 == "math") {
      return(d1())   
    } else {
      return(d2())   
    }
  })
  
  # 1. Summary 
  # Variable descriptions
  descriptions <- list(
    sex = "Student's Sex: 'F' - female, 'M' - male",
    age = "Student's age: numeric, from 15 to 22",
    address = "Home address type: 'U' - urban, 'R' - rural",
    famsize = "Family size: 'LE3' - less or equal to 3, 'GT3' - greater than 3",
    Pstatus = "Parental cohabitation status: 'T' - together, 'A' - apart",
    Medu = "Mother's education: 0 - none, 1 - 4th grade or less, 2 - 5th to 9th grade, 3 - secondary, 4 - higher education",
    Fedu = "Father's education: 0 - none, 1 - 4th grade or less, 2 - 5th to 9th grade, 3 - secondary, 4 - higher education",
    Mjob = "Mother's job: 'teacher', 'health', 'civil services', 'at_home', 'other'",
    Fjob = "Father's job: 'teacher', 'health', 'civil services', 'at_home', 'other'",
    reason = "Reason for choosing the school: 'home', 'reputation', 'course', 'other'",
    guardian = "Student's guardian: 'mother', 'father', 'other'",
    traveltime = "Travel time to school: 1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, 4 - >1 hour",
    studytime = "Weekly study time: 1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, 4 - >10 hours",
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
    data <- selected_data_1()
    
    data %>%
      group_by(.data[[input$category1]]) %>%
      summarise(
        Count = n(),
        Mean_Grade = round(mean(G3, na.rm = TRUE), 2),
        SD_Grade = round(sd(G3, na.rm = TRUE), 2),
        Min_Grade = min(G3, na.rm = TRUE),
        Max_Grade = max(G3, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Calculate G3 summary statistics
  output$g3_summary <- renderTable({
    req(input$subject1)
    data <- selected_data_1()
    
    data %>%
      summarise(
        Count = n(),
        Mean = round(mean(G3, na.rm = TRUE), 2),
        SD = round(sd(G3, na.rm = TRUE), 2),
        Min = min(G3, na.rm = TRUE),
        Max = max(G3, na.rm = TRUE)
      )
  })
  
  
  # 2. Regression Analysis
  observe({
    req(selected_data_2())
    excluded_vars <- c("G3", "G1", "G2", "school")
    available_vars <- setdiff(names(selected_data_2()), c(excluded_vars, "subject"))
    updateSelectizeInput(session, "independent_vars", 
                         choices = setNames(available_vars, sapply(available_vars, function(var) var_labels[[var]])),
                         # This maps the variable codes to labels in the choices
                         selected = available_vars[1:min(2, length(available_vars))])
  })
  
  regression_result <- eventReactive(input$run_analysis, {
    req(input$independent_vars)
    formula <- as.formula(paste("G3", "~", paste(input$independent_vars, collapse = " + ")))
    model <- lm(formula, data = selected_data_2())
    coefficients <- as.data.frame(summary(model)$coefficients)
    coefficients <- tibble::rownames_to_column(coefficients, "Variable")
    coefficients <- coefficients %>%
      mutate(across(where(is.numeric), ~ round(., 3))) %>%
      # Replace variable codes with labels
      mutate(Variable = sapply(Variable, function(var) ifelse(var %in% names(var_labels), var_labels[[var]], var)))
    return(coefficients)
  })
  
  output$regression_table <- renderDT({
    req(regression_result())
    datatable(regression_result(), options = list(pageLength = 10))
  })
  
  # 3. Grades Across Binary Variables
  output$plot <- renderPlot({
    req(selected_data_3())
    data <- selected_data_3()
    
    # Get the descriptive label for the selected x_var
    x_label <- var_labels[[input$x_var]]
    
    if (input$plot_type == "boxplot") {
      ggplot(data, aes_string(x = input$x_var, y = "G3")) +
        geom_boxplot(fill = "lightblue") +
        labs(title = paste("Boxplot of Grade by", x_label), x = x_label, y = "Grade")
    } else if (input$plot_type == "density") {
      ggplot(data, aes_string(x = "G3", fill = input$x_var)) +
        geom_density(alpha = 0.5) +
        labs(title = paste("Density Plot of Grade by", x_label), x = "Grade", fill = x_label)
    } else if (input$plot_type == "scatter") {
      ggplot(data, aes_string(x = input$x_var, y = "G3", color = input$x_var)) +
        geom_jitter(width = 0.2, height = 0.2) +
        labs(title = paste("Scatter Plot of Grade by", x_label), x = x_label, y = "Grade")
    } else if (input$plot_type == "violin") {
      ggplot(data, aes_string(x = input$x_var, y = "G3", fill = input$x_var)) +
        geom_violin() +
        labs(title = paste("Violin Plot of Grade by", x_label), x = x_label, y = "Grade")
    } else if (input$plot_type == "barplot") {
      ggplot(data, aes_string(x = input$x_var, fill = input$x_var)) +
        geom_bar() +
        labs(title = paste("Bar Plot of", x_label), x = x_label, y = "Count")
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
  
  # 4. Grades Across Categorical Variables
  output$histogramPlot <- renderPlot({
    req(selected_data_4())  
    data <- selected_data_4()  
    
    # Get the descriptive label for the fill_variable
    fill_label <- var_labels[[input$fill_variable]]
    
    ggplot(data, aes(x = G3, fill = .data[[input$fill_variable]])) +
      geom_histogram(breaks = seq(0, 20, 3), position = "fill") +
      xlab("Grade") +
      ylab("Proportion") +
      labs(title = paste("Proportion of Each Value of", fill_label, "by Grade"), fill = fill_label) +
      theme_minimal()
  })
  
  # 5. Grades by Absences
  output$scorePlot <- renderPlot({
    data <- selected_data_5()
    ggplot(data, aes(x = absences, y = G3)) +
      geom_point(color = "blue", size = 3, alpha = 0.6) +
      labs(
        title = "Scatter Plot of Grade by Absences", 
        x = "Number of Absences", 
        y = "Grade"
      )
  })
  
  output$summaryTable <- renderTable({
    data <- selected_data_5()
    summary_data <- data %>%
      group_by(absences) %>%
      summarise(
        `Average Grade` = round(mean(G3, na.rm = TRUE), 2),
        Count = n(),
        .groups = "drop"
      ) %>%
      arrange(absences)
    summary_data
  })
  
  # 6. Grades by Age and Sex
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
      
      # Get descriptive labels for sex
      sex_labels <- var_labels[filtered_data$sex]
      
      bar_plot <- ggplot(filtered_data, aes(x = sex, y = mean_score, fill = sex)) +
        geom_bar(stat = "identity") +
        labs(
          title = paste("Average Grade by Sex, for Age", input$Age),
          x = "Sex",
          y = "Grade"
        ) +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_x_discrete(labels = sex_labels)
      
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
        labs(
          title = paste("Box Plot of Grade by Sex, for Age", input$Age),
          x = "Sex",
          y = "Grade"
        ) +
        theme_minimal() +
        scale_x_discrete(labels = var_labels[unique(filtered_data$sex)])
      
    } else if (input$plotType == "Line Plot") {
      req(input$ageRange)  
      filtered_data <- data %>%
        filter(age >= input$ageRange[1], age <= input$ageRange[2]) %>%
        group_by(age, sex) %>%
        summarise(mean_score = mean(G3, na.rm = TRUE), .groups = "drop")
      
      ggplot(filtered_data, aes(x = age, y = mean_score, color = sex, group = sex)) +
        geom_line() +
        geom_point() +
        labs(
          title = "Average Grade by Age and Sex",
          x = "Age",
          y = "Grade",
          color = "Sex"
        ) +
        theme_minimal() +
        scale_color_discrete(labels = var_labels[unique(filtered_data$sex)])
    }
  })
  
  # Dynamic UI for age selection
  output$ageInputUI <- renderUI({
    if (input$plotType %in% c("Bar Plot", "Box Plot")) {
      tagList(
      numericInput("Age", "Student Age (15-22)", value = 15, min = 15, max = 22, step = 1),
      HTML("<ul>
      <li>Age is between 15 and 22.
      <li>Selecting up to 19 is recommended.</li>
      <li>Ages 20 to 22 have very few samples.</li>
    </ul>")
      )
    }
  })
  
  output$ageRangeUI <- renderUI({
    if (input$plotType == "Line Plot") {
      tagList(
        sliderInput("ageRange", "Age Range", min = 15, max = 22, value = c(15, 19)),
        HTML("<ul>
      <li>Selecting up to 19 is recommended.</li>
      <li>Ages 20 to 22 have very few samples.</li>
    </ul>")
      )
    }
  })
  
  # Count data by sex with descriptive labels
  output$data_count_F <- renderTable({
    req(selected_data_6())
    if (input$plotType %in% c("Bar Plot", "Box Plot")) {
      req(input$Age)
      filtered_data <- selected_data_6() %>%
        filter(age == input$Age, sex == "F")
      data.frame("Sex" = "Female", "Count" = nrow(filtered_data))
    } else if (input$plotType == "Line Plot") {
      req(input$ageRange)
      filtered_data <- selected_data_6() %>%
        filter(age >= input$ageRange[1], age <= input$ageRange[2], sex == "F") %>%
        group_by(age) %>%
        summarise(count = n(), .groups = "drop")
      # Rename columns for clarity
      filtered_data <- filtered_data %>%
        rename("Age" = age, "Female Count" = count)
      filtered_data
    }
  })
  
  output$data_count_M <- renderTable({
    req(selected_data_6())
    if (input$plotType %in% c("Bar Plot", "Box Plot")) {
      req(input$Age)
      filtered_data <- selected_data_6() %>%
        filter(age == input$Age, sex == "M")
      data.frame("Sex" = "Male", "Count" = nrow(filtered_data))
    } else if (input$plotType == "Line Plot") {
      req(input$ageRange)
      filtered_data <- selected_data_6() %>%
        filter(age >= input$ageRange[1], age <= input$ageRange[2], sex == "M") %>%
        group_by(age) %>%
        summarise(count = n(), .groups = "drop")
      # Rename columns for clarity
      filtered_data <- filtered_data %>%
        rename("Age" = age, "Male Count" = count)
      filtered_data
    }
  })
  
  # 7. Interaction Between Study Time and Categorical Variables
  output$performancePlot <- renderPlot({
    data <- selected_data_7()
    
    category <- input$category
    
    category_label <- var_labels[[category]]
    
    ggplot(data, aes(x = factor(studytime), y = G3, group = factor(get(category)), color = factor(get(category)))) +
      stat_summary(fun = mean, geom = "line", size = 1) +
      stat_summary(fun = mean, geom = "point", size = 3) +
      labs(
        title = paste("Average Grade by Study Time and", category_label),
        x = "Weekly Study Time",
        y = "Average Grade",
        color = category_label
      ) +
      theme_minimal()
  })
  
  output$categoryStats <- renderTable({
    data <- selected_data_7()
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
                             "goout" = "Going Out with Friends<br>(1: very low - 5: very high)",
                             "Dalc" = "Weekday Alcohol Consumption<br>(1: very low - 5: very high)",
                             "Walc" = "Weekend Alcohol Consumption<br>(1: very low - 5: very high)",
                             "health" = "Health Status<br>(1: very bad - 5: very good)",
                             "freetime" = "Free Time After School<br>(1: very low - 5: very high)",
                             "traveltime" = "Travel Time<br>(1: <15 min - 4: >1 hour)",
                             "Medu" = "Mother's Education<br>(0: none - 4: higher education)",
                             "Fedu" = "Father's Education<br>(0: none - 4: higher education)",
                             "sex" = "Sex<br>(F: female, M: male)",
                             "famsize" = "Family Size<br>(LE3: <=3, GT3: >3)",
                             "Pstatus" = "Parental Status<br>(T: together, A: apart)",
                             "Mjob" = "Mother's Job",
                             "Fjob" = "Father's Job",
                             "address" = "Home Address Type<br>(U: urban, R: rural)",
                             category)
    
    colnames(stats) <- c(first_col_name, "Studytime 1", "Studytime 2", "Studytime 3", "Studytime 4")
    stats
  }, sanitize.text.function = function(x) x, align = "c")
  
  # 8. Drinking by Age - Server Logic
  # Variable descriptions
  descriptions8 <- list(
    Dalc = "Weekday alcohol consumption: 1 - very low to 5 - very high",
    Walc = "Weekend alcohol consumption: 1 - very low to 5 - very high"
  )
  
  # Show variable description
  output$description8 <- renderText({
    req(input$days)
    descriptions[[input$days]]
  })
  
  
  output$drinkingAgePlot <- renderPlot({
    data <- selected_data_8()
    days_var <- input$days
    
    days_label <- ifelse(days_var == "Dalc", "Weekday Alcohol Consumption", "Weekend Alcohol Consumption")
    
    # Determine the position of age 18 for the vertical line
    age_levels <- levels(factor(data$age))
    age_pos <- which(age_levels == "18")
    
    ggplot(data, aes(x = factor(age), fill = .data[[days_var]])) +
      geom_bar(position = "fill", color = "black") +
      labs(
        title = paste(days_label, "by Age"),
        x = "Age",
        y = "Proportion",
        fill = days_label
      ) +
      theme_minimal() +
      # Highlight the legal drinking age of 18 with a vertical dashed red line
      geom_vline(xintercept = age_pos, linetype = "dashed", color = "red") +
      # Annotate the vertical line
      annotate("text", 
               x = age_pos, 
               y = 1, 
               label = "Legal Drinking Age (18)", 
               vjust = -0.5, 
               color = "red")
  })
  
}

shinyApp(ui, server)