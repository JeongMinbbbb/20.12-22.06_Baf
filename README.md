# 20.12-22.06_Baf
##비어플(동국대학교 빅데이터 학회) 활동
### 프로젝트 자료 모음


**1. [심리 성향 테스트를 통해 국가 선거 투표 여부 예측](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.01_Psychological_Tendency_Data_Analysis)**
>월간 데이콘 심리 성향 예측 AI 경진대회 자료 이요([Dacon](https://dacon.io/competitions/official/235647/overview/description))
 - 데이터 이상값 판단 (ex : pm2.5 = 90에 가까운 값은 논리적으로 발생할 수 있는 값이라 판단하여 제거X)
 - 그룹별 통계량 값을 이용한 결측치 대체
 - GridSearch 하이퍼파라미터 튜닝
 - RandomForest 모델 사용
\

**2. [코로나 대응 정책의 실효성 분석](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.03_Analysis_Of_The_Effectiveness_Of_COVID19)**
>코로나 데이터 시각화 AI 경진대회 자료 이용([Kaggle](https://www.kaggle.com/datasets/kimjihoo/coronavirusdataset),[Dacon](https://dacon.io/competitions/official/235590/overview/description))
 - 주어진 뉴스의 제목을 보고 정해진 토픽 중 어느 곳에 해당하는지 예측하는 모델 생성
 - 자연어처리(NLP) 사용 : kkm, okt 등..
 - one-hot-encoding 방식 적용
 - 분류 모델 사용 : 로지스틱 회귀, LGBM
\

**3. [따릉이 대여 수 예측](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.09_Prediction_Of_The_Number_Of_Ttareungi_Rentals)**
 - 팔꿈치에 센서 부착 후, 3축 가속도 및 3축 자이로스코프(각속도) 값 측정
 - 0.02초 간격으로 600번 총 12초 동안 운동 수행
 - 수행한 운동의 종류를 예측
 - 이상값 처리 및 3차원 데이터 구조 정리
 - 모델링1 : 2차원 모델링, 데이터 Grouping 후 Catboost 모델 적용
 - 모델링2 : 3차원 모델링, 2D CNN 모델 적용
\

**4. [딥러닝 및 이미지AI 스터디](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/22.02_Image-Recognition_AI)**
 - 팔꿈치에 센서 부착 후, 3축 가속도 및 3축 자이로스코프(각속도) 값 측정
 - 0.02초 간격으로 600번 총 12초 동안 운동 수행
 
