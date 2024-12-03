library(shiny)
library(ggplot2)
library(dplyr)

# 데이터 로드
data <- read.csv("data/student-por.csv", sep = ";", header=TRUE)



# UI 정의
ui <- fluidPage(
    titlePanel("학생 성적 분석"),  # 앱 제목
    sidebarLayout(
        sidebarPanel(
            # 분석할 변수를 선택
            selectInput("variable", "분석할 변수를 선택하세요:",
                        choices = c("성별" = "sex", 
                                    "주소 유형" = "address", 
                                    "공부 시간" = "studytime", 
                                    "대학 진학 의향" = "higher", 
                                    "가족 관계 품질" = "famrel", 
                                    "친구들과 노는 빈도" = "goout", 
                                    "평일 음주량" = "Dalc", 
                                    "건강 상태" = "health")),
            # 시각화 유형을 선택
            selectInput("plotType", "시각화 유형을 선택하세요:",
                        choices = c("박스플롯" = "boxplot", 
                                    "바이올린 플롯" = "violin",
                                    "바 플롯 (평균)" = "bar",
                                    "밀도 플롯" = "density",
                                    "히트맵" = "heatmap",
                                    "산점도 및 추세선" = "scatter"))
        ),
        # 플롯 출력
        mainPanel(
            plotOutput("mainPlot")
        )
    )
)

# 서버 로직 정의
server <- function(input, output) {
    output$mainPlot <- renderPlot({
        # 기본 ggplot 객체 생성
        p <- ggplot(data, aes_string(x = input$variable, y = "G3"))
        
        # 시각화 유형에 따른 플롯 생성
        if (input$plotType == "boxplot") {
            # 박스플롯
            p <- p + geom_boxplot(fill = "lightblue", color = "darkblue")
        } else if (input$plotType == "violin") {
            # 바이올린 플롯
            p <- p + geom_violin(fill = "lightblue", color = "darkblue")
        } else if (input$plotType == "bar") {
            # 바 플롯 (평균)
            p <- p + stat_summary(fun = "mean", geom = "bar", fill = "lightblue", color = "darkblue")
        } else if (input$plotType == "density") {
            # 밀도 플롯
            p <- ggplot(data, aes_string(x = "G3", fill = input$variable)) +
                geom_density(alpha = 0.6) +
                labs(fill = input$variable)
        } else if (input$plotType == "heatmap") {
            # 히트맵
            data_summary <- data %>%
                group_by_at(vars(input$variable)) %>%
                summarise(mean_grade = mean(G3, na.rm = TRUE)) %>%
                mutate(variable = factor(!!sym(input$variable))) # 변수 동적 처리
            p <- ggplot(data_summary, aes(x = variable, y = mean_grade, fill = mean_grade)) +
                geom_tile() +
                scale_fill_gradient(low = "white", high = "blue") +
                labs(y = "평균 성적 (G3)", fill = "평균 성적")
        } else if (input$plotType == "scatter") {
            # 산점도 및 추세선
            p <- ggplot(data, aes_string(x = input$variable, y = "G3")) +
                geom_point(alpha = 0.6, position = position_jitter(width = 0.2)) +
                geom_smooth(method = "lm", color = "red", se = FALSE)
        }
        
        # 공통 테마 및 라벨 추가
        p + labs(title = paste(input$variable, "에 따른 최종 성적"),
                 x = input$variable,
                 y = "최종 성적 (G3)") +
            theme_minimal()
    })
}

# 애플리케이션 실행
shinyApp(ui = ui, server = server)
