# 필요한 라이브러리
library(shiny)
library(ggplot2)
library(dplyr)

# UI 정의
ui <- fluidPage(
  titlePanel("학생 성적 데이터 분석"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subject", "과목 선택:", 
                  choices = c("수학" = "math", "국어" = "portuguese")),
      selectInput("plot_type", "시각화 유형 선택:",
                  choices = c("Boxplot" = "boxplot",
                              "Density Plot" = "density",
                              "Scatter Plot" = "scatter",
                              "Histogram" = "histogram",
                              "Violin Plot" = "violin",
                              "Bar Plot" = "barplot")),
      selectInput("x_var", "X축 변수 선택:", 
                  choices = c("성별(sex)" = "sex", 
                              "주소(address)" = "address", 
                              "대학 희망 여부(higher)" = "higher")),
      selectInput("y_var", "Y축 변수 선택:", 
                  choices = c("최종 성적(G3)" = "G3")),
      checkboxInput("show_summary", "데이터 요약 보기", FALSE)
    ),
    
    mainPanel(
      plotOutput("plot"),
      conditionalPanel(
        condition = "input.show_summary == true",
        verbatimTextOutput("summary")
      )
    )
  )
)

# 서버 로직 정의
server <- function(input, output) {
  # 데이터 로드
  math_data <- read.csv("data/student-mat.csv", sep = ";")
  portuguese_data <- read.csv("data/student-por.csv", sep = ";")
  
  # 데이터 선택
  selected_data <- reactive({
    if (input$subject == "math") {
      math_data
    } else {
      portuguese_data
    }
  })
  
  # 데이터 요약 출력
  output$summary <- renderPrint({
    data <- selected_data()
    summary(data %>% select(input$x_var, input$y_var))
  })
  
  # 시각화 출력
  output$plot <- renderPlot({
    data <- selected_data()
    
    if (input$plot_type == "boxplot") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var)) +
        geom_boxplot(fill = "lightblue") +
        labs(title = "Boxplot", x = input$x_var, y = input$y_var)
    } else if (input$plot_type == "density") {
      ggplot(data, aes_string(x = input$y_var, fill = input$x_var)) +
        geom_density(alpha = 0.5) +
        labs(title = "Density Plot", x = input$y_var, fill = input$x_var)
    } else if (input$plot_type == "scatter") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var, color = input$x_var)) +
        geom_jitter(width = 0.2, height = 0.2) +
        labs(title = "Scatter Plot", x = input$x_var, y = input$y_var)
    } else if (input$plot_type == "histogram") {
      ggplot(data, aes_string(x = input$y_var)) +
        geom_histogram(bins = 30, fill = "skyblue", color = "black") +
        labs(title = "Histogram", x = input$y_var, y = "Frequency")
    } else if (input$plot_type == "violin") {
      ggplot(data, aes_string(x = input$x_var, y = input$y_var, fill = input$x_var)) +
        geom_violin() +
        labs(title = "Violin Plot", x = input$x_var, y = input$y_var)
    } else if (input$plot_type == "barplot") {
      ggplot(data, aes_string(x = input$x_var, fill = input$x_var)) +
        geom_bar() +
        labs(title = "Bar Plot", x = input$x_var, y = "Count")
    }
  })
}

# Shiny 앱 실행
shinyApp(ui = ui, server = server)
