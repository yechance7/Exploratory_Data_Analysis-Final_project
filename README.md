# Exploratory_Data_Analysis-Final_project
Exploratory_Data_Analysis-Final_project using R

## 📁 프로젝트 폴더 구조
```plaintext
Exploratory_Data_Analysis-Final_project/
├── .github/
│   └── workflows/
│       └── deploy.yml # github을 이용한 자동 배포
├── data/           # 데이터 파일
├── DESCRIPTION.txt # renv 세팅
├── app.R           # 메인 Shiny 파일
├── renv.lock       # renv로 관리된 의존성 파일
└── README.md       # 앱 설명
```

## 가상환경 설정
R

renv::activate()    # 가상환경 실행

renv::status()      # 가상환경 상태 확인

renv::snapshot()    # 가상환경 설정 저장

## 파일 실행
shiny::runApp()

## 배포
GitHub Actions을 통한 CI/CD
