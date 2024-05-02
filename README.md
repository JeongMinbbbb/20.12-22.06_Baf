# 20.12-22.06_Baf
## 비어플 동국대학교 빅데이터 교내 학회 활동
**[학회 메인 페이지](https://www.dgubaf.com/)**

## 프로젝트 자료 모음

**1. [심리 성향 테스트를 통해 국가 선거 투표 여부 예측(21.01)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.01_Psychological_Tendency_Data_Analysis)**
>월간 데이콘 심리 성향 예측 AI 경진대회

**2. [코로나 대응 정책의 실효성 분석(21.03)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.03_Analysis_Of_The_Effectiveness_Of_COVID19)**
>코로나 데이터 시각화 AI 경진대회

**3. [따릉이 대여 수 예측(21.09)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.09_Prediction_Of_The_Number_Of_Ttareungi_Rentals)**
>서울시 따릉이 대여량 예측 경진대회
 - 2017년 4,5월 서울시 마포구의 날짜, 시간별 기상상황과 따릉이 대여 수로 6월의 따릉이 대여 수를 예측 
 - 기상 외부데이터를 활용하여 데이터의 시점에 대응시킨 뒤, 기상 관련 정보(기온, 풍속, 시정, 습도)의 결측치를 채워넣음
  -> 테스트 데이터(6월) 내에 독립변수가 전부 주어지지 않은 경우도 예측가능하도록 만듦
 - 변수간의 관계를 고려해 그룹별 통계량 값을 이용한 결측치 대체
 - GridSearch 하이퍼파라미터 튜닝
 - RandomForest 모델 사용
 - 따릉이 수요 증가 패턴에 따른 활용방안 도출
   1. 평일 출퇴근, 등하교 시간대 전후로 따릉이 점검시간 확보(주요 시간은 피하기)
   2. 주말 낮 시간에는 다중이용시설 근처에 따릉이 배치 늘리기
   3. 온도에 높아짐에 따라 따릉이 이용객의 수가 증가하므로 여름에는 사설업체와 협력하여 자전거 추가 배치

**4. [딥러닝 및 이미지AI 스터디(22.02)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/22.02_Image-Recognition_AI)**
 - 이미지 전처리(float, grayscale, resize, augmentation)
 - Neural Network 종류(ANN, DNN, CNN)
 - CNN 알고리즘 개념(Convolution, filter, kernel, pooing layer 등)
 - CNN 알고리즘 중 VGG의 구조
 - 주말 오전에 학회 학생들에게 CNN 파이썬 실습 세션 진행

 
