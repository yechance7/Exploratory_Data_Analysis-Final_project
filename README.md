# Exploratory_Data_Analysis-Final_project
Exploratory_Data_Analysis-Final_project using R

Exploratory_Data_Analysis-Final_project/
│
├── app.R          # 메인 Shiny 파일
├── data/          # 데이터 파일
├── www/           # (선택 사항) 정적 리소스 (CSS, JS 등)
└── README.md      # 앱 설명

## 가상환경 설정
R
renv::activate() 

## 가상환경 상태 확인
renv::status()

### 가상환경 설정 저장
renv::snapshot()

## 파일 실행
shiny::runApp()

## 배포
install.packages('rsconnect')
rsconnect::setAccountInfo(
  name = Sys.getenv("RSCONNECT_NAME"),
  token = Sys.getenv("RSCONNECT_TOKEN"),
  secret='<SECRET>')
library(rsconnect)
rsconnect::deployApp('path/to/your/app')
