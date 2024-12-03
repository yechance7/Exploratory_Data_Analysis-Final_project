# Exploratory_Data_Analysis-Final_project
Exploratory_Data_Analysis-Final_project using R

## 📁 프로젝트 폴더 구조
```plaintext
Exploratory_Data_Analysis-Final_project/
├── .github/
│   └── workflows/
│       └── deploy.yml # github을 이용한 자동 배포
├── app.R          # 메인 Shiny 파일
├── data/          # 데이터 파일
├── .Renviron
└── README.md      # 앱 설명
```

## 가상환경 설정
R
renv::activate() 

### 가상환경 상태 확인
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
