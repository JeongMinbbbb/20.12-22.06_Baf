# 20.12-22.06_Baf
## 비어플(동국대학교 빅데이터 학회) 활동
### 프로젝트 자료 모음


**1. [심리 성향 테스트를 통해 국가 선거 투표 여부 예측(21.01)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.01_Psychological_Tendency_Data_Analysis)**
>월간 데이콘 심리 성향 예측 AI 경진대회 자료 이용([Dacon](https://dacon.io/competitions/official/235647/overview/description))


**2. [코로나 대응 정책의 실효성 분석(21.03)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.03_Analysis_Of_The_Effectiveness_Of_COVID19)**
>코로나 데이터 시각화 AI 경진대회 자료 이용([Kaggle](https://www.kaggle.com/datasets/kimjihoo/coronavirusdataset),[Dacon](https://dacon.io/competitions/official/235590/overview/description))
 - 코로나 대응 정책의 실효성에 대한 의구심을 갖는 비판이 많음
 - 계속해서 새롭게 발생하는 팬데믹 현상에 대한 대책 피드백이 필요
 - 제한명령/사회적거리두기/마스크/기술/출입국 정책으로 나누어서 실효성을 평가
 - 시계열 분석 및 시각화 활용
 - 세부적으로 특정 집단이나 상황에 대한 정책의 보완점을 제안함
   - (제한명령)
    1. 지방자치단체 행정명령은 고연령층에게 효과적
   - (사회적거리두기)
    2. 전반적인 확진자 수 감소는 효과적
    3. 4-5월 모든 시도국립공원과 하천, 유원지 폐쇄와 같은 기회차단 정책이 필요
    4. 온라인 예배 의무화, 운영자제 권고와 같은 정책 유지 필수 등
   - (마스크)
    5. 공적 마스크 판매와 공적 마스크 5부제는 효과적
   - (기술)
    6. 모바일 자가진단앱 도입은 실효성이 있음
    7. 코로나 관련 교육(주민센터, 복지회관 등), TV공익광고 등을 통해 코로나 증상 의심 시 할 일 알림으로써 고연령층에 대한 검진 접근성 높이는 방법 제안
   - (출입국)
    8. '입국 후 14일 자가격리'와 '미국발 입국자 검역강화' 정책은 효과적
    9. '특별 출입국 제한' 정책 시행의 효과가 미흡
   10. 고위험 국가에 대한 더 엄한 제한 정책이 필요
   - 한계점 : 특정 정책에 대한 효력을 명확하게 구분하기 어려움/ 특정 변수의 NA값이 다수 존재

**3. [따릉이 대여 수 예측(21.09)](https://github.com/JeongMinbbbb/20.12-22.06_Baf/tree/main/21.09_Prediction_Of_The_Number_Of_Ttareungi_Rentals)**
>[서울시 따릉이 대여량 예측 경진대회 자료 이용](https://dacon.io/competitions/open/235576/data)
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

 
