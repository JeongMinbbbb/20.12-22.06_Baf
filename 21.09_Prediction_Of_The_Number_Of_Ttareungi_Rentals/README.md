# 20.12-22.06_Baf
## 비어플(동국대학교 빅데이터 학회) 활동
- 발표 pdf 파일 확인 가능
- 비어플 프로젝트 3회차


***
## 주제 : 따릉이 대여 수 예측(21.09)
>서울시 따릉이 대여량 예측 경진대회 자료([Dacon](https://dacon.io/competitions/open/235576/data)) 이용

>#Python #R 

## 분석 목적
- 서울시 따릉이의 이용률이 증가함에 따라 서울시 대응책이 필요
- 따릉이 이용 현황에 대한 데이터를 분석하여 서울 시민들의 편의성 증대
- 2017년 4,5월의 마포구 데이터를 바탕으로 6월의 따릉이 대여 수 예측 모델 구축


## 분석 과정
1. **전처리**
    1. 결측치 처리
        - 기상 관련 정보(기온, 풍속, 시정, 습도)의 결측치 처리
            - 기상 외부데이터를 활용하여 시계열 흐름을 파악
            - 결측 데이터의 시점을 유추
            - 이를 통해, 테스트 데이터(6월) 내에 독립변수가 전부 주어지지 않은 경우도 예측 가능하도록 만듦    
        - 변수간의 관계를 고려하고 그룹별 통계량 값을 이용함으로써 결측치 대체
    
    2. 파생변수 생성
        - 일자별 따릉이 대여수 패턴을 비교함으로써 주말/평일 여부를 파악하고 변수 생성
        - 대부분의 수치가 0인 강수량 변수를 수치형 변수에서 이진 변수 형태로 변경
        <p align="left">
          <img src="https://github.com/JeongMinbbbb/20.12-22.06_Baf/assets/130365764/f1621540-a24a-453f-988c-9787d1063928" alt="image">
        </p>


2. **모델링**
    - RandomForest, XGBoost, SVM, LGBM 중 성능이 높고 해석이 평이했던 RandomForest 모델 선택
    - GridSearch를 활용한 하이퍼파라미터 튜닝

3. **분석 결과**
    - RMSE : 30.58
    - 따릉이 수요 증가 패턴에 따른 활용방안 도출
        - 평일 출퇴근, 등하교 시간대 전후로 따릉이 점검시간 확보(주요 시간은 피하기)
        - 주말 낮 시간에는 다중이용시설 근처에 따릉이 배치 늘리기
        - 온도에 높아짐에 따라 따릉이 이용객의 수가 증가하므로 여름에는 사설업체와 협력하여 자전거 추가 배치
        <p align="left">
          <img src="https://github.com/JeongMinbbbb/20.12-22.06_Baf/assets/130365764/ad9f00aa-65c5-4c8a-b107-a81280d15f18" alt="image">
        </p>


### 프로젝트를 통해 성장한 점
    1. 데이터 분석 프로젝트가 처음인 팀원들에게 분석의 프로세스를 알려주고, 방향성 설정을 주도함으로써 프로젝트 운영 역량을 향상시킴
    2. R의 라이브러리 dplyr과 ggplot2을 활용한 전처리 숙련도를 높임

    
***
### Team Project
#### 팀 구성: 배정민 (팀장), 김기호, 이가영
#### 소속: 비어플 학회
