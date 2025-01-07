#############
# 환경 설정 #
#############

# 환경 설정 - * 파일 읽어오는 위치 정의해줄 것 *
for (i in sequence(1)) {
  setwd("~/ProjectR/Chapter01/source")
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
  # Sys.setlocale("LC_ALL", "Korean_Korea.949")
}

# 학습용 데이터 셋 정의
for (j in sequence(1)) {
  for (i in sequence(1)) {
    # 훈련용 데이터셋을 train_data에 저장
    training_data <- read.csv("training_dataset.csv", fileEncoding = "CP949")

    # print(names(train_data))
    # "Date", "Mosquito", "Averagetemperature", "Dailyprecipitation", "Averagewindspeed", "Averagerelativehumidity", "Averagevaporpressure",
    # "Totalsunshinehours", "Totalsolarradiation", "Averagegroundtemperature", "Watertemperature", "Ph", "Dissolvedoxygen", "Date_Labeled"
  }

  library(dplyr)
  # 날짜 속성 제거한 데이터를 training으로 정의
  for (i in sequence(1)) {
    training <- dplyr::select(training_data, -Date)
    # print(names(Training))
  }
}

# 검증용 데이터 셋 정의
for (j in sequence(1)) {
  for (i in sequence(1)) {
    # 검증용 데이터셋을 test_data 저장
    test_data <- read.csv("test_dataset.csv", fileEncoding = "CP949")
    # print(names(test_data))
  }

  library(dplyr)
  # 날짜와 모기 데이터를 제거한 데이터 test로 정의
  for (i in sequence(1)) {
    test <- dplyr::select(test_data, -Date, -Mosquito)
    # print(names(Test))
  }
  # 23년도 정답 모기 값을 answer로 정의
  for (i in sequence(1)) {
    answer <- dplyr::select(test_data, Mosquito)
    # print(names(Answer))
  }
}


#########################
# 부스팅(Boosting) 모델 #
#########################

library(gbm)
# 람다 파라미터 튜닝
for (i in 10^(0:-15)) {
  p_Boosting_Model <- gbm(Mosquito ~ ., data = training, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = i, verbose = F)
  p_Boosting_Prediction <- predict(p_Boosting_Model, newdata = test, n.trees = 5000)
  boost_MSE <- mean((answer$Mosquito - p_Boosting_Prediction)^2)
  boost_R <- cor(answer, p_Boosting_Prediction)^2
  cat("람다값이", i, "일때 R^2 값 : ", boost_R, "MSE 값 : ", boost_MSE, "\n")

  # 람다값이 1e-08 일때 R^2 값 :  0.4047885 MSE 값 :  730587.8
  # 람다값이 1e-10 일때 R^2 값 :  0.4061142 MSE 값 :  730613.1
  # 람다값이 1e-08 일때 R^2 값 :  0.4065187 MSE 값 :  730587.8
}

# 최종 Boosting 예측 결과 : Boosting_Prediction
for (j in sequence(1)) {
  Boosting_Model <- gbm(Mosquito ~ ., data = training, distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 10^-8, verbose = F)
  Boosting_Prediction <- predict(Boosting_Model, newdata = test, n.trees = 5000)
  Boosting_MSE <- mean((answer$Mosquito - Boosting_Prediction)^2)
  boosting_R <- cor(answer, Boosting_Prediction)^2
  cat("람다값이 10^-8 일때 R^2 값 : ", boosting_R, "MSE 값 : ", Boosting_MSE, "\n")
  # plot(Boosting_Prediction)
}


#################
# KNN 모델 구축 #
#################

library(class)
# 이진분류를 위해 Travel 속성을 추가하고 데이터 변수 정의
for (j in sequence(1)) {
  Travel <- ifelse(training$Mosquito <= 2000, "No", "Yes")
  knn_data <- data.frame(training, Travel)
  knn_data$Travel <- as.factor(knn_data$Travel)

  Travelanswer <- ifelse(test_data$Mosquito <= 2000, "No", "Yes")
  knn_test_data <- data.frame(test_data, Travelanswer)
  knn_test_data$Travelanswer <- as.factor(knn_test_data$Travelanswer)

  numeric_columns <- sapply(knn_data, is.numeric)
  std_knn <- scale(knn_data[, numeric_columns])

  test_numeric_columns <- sapply(knn_test_data, is.numeric)
  test_std_knn <- scale(knn_test_data[, test_numeric_columns])

  train_knnx <- std_knn
  train_knny <- knn_data$Travel

  test_knnx <- test_std_knn
  test_knny <- knn_test_data$Travelanswer
}

# k 파라미터 튜닝
for (j in sequence(3)) {
  set.seed(1)
  cat("그냥 갈때 가도 되는 날일지의 확률 : ", mean(test_knny != "No"), "\n")
  # 파라미터 튜닝
  for (i in sequence(15)) {
    knn.pred <- knn(train_knnx, test_knnx, train_knny, k = i)
    knn_t <- table(knn.pred, test_knny)
    cat("k가", i, "일때 knn 결과 정확도 : ", mean(test_knny == knn.pred), "\n")
    cat(
      "여행 가도 된다고 예측했을때 정말 가도 되는 확률(Sensitivity) : ",
      knn_t[2, 2] / (knn_t[2, 2] + knn_t[2, 1]),
      ", 여행 가면 안 된다고 예측했을때 정말 가면 안 되는 확률(Sensitivity) : ",
      knn_t[1, 1] / (knn_t[1, 1] + knn_t[1, 2]), "\n"
    )
  }
}

# 최종 KNN 예측 결과 : KNN_prediction
for (i in sequence(5)) {
  set.seed(1)
  KNN_prdiction <- knn(train_knnx, test_knnx, train_knny, k = 2)
  knn_t <- table(KNN_prdiction, test_knny)
  cat("k가", 2, "일때 knn 결과 정확도 : ", mean(test_knny == KNN_prdiction), "\n")
  cat(
    "여행 가도 된다고 예측했을때 정말 가도 되는 확률(Sensitivity) : ",
    knn_t[2, 2] / (knn_t[2, 2] + knn_t[2, 1]), "\n",
    "여행 가면 안 된다고 예측했을때 정말 가면 안 되는 확률(Sensitivity) : ",
    knn_t[1, 1] / (knn_t[1, 1] + knn_t[1, 2]), "\n"
  )
}


#############
# 결과 분석 #
#############

# 모기가 이상기후로 많이 적었음
for (j in sequence(1)) {
  par(mfrow = c(1, 2))
  # plot(test_data$Date_Labeled,test_data$Mosquito)
  plot(test_data$Date_Labeled, test_data$Mosquito, ylim = c(0, 7000))
  plot(training_data$Date_Labeled, training_data$Mosquito, ylim = c(0, 7000))
}


# 비가 너무 많이오고 지금까지 기상이변에서 생태계의 변화가 많아졌다
# 특히 올해는 12월까지 따듯할 정도로 날씨가 평소와 다르기에 같은 환경이여도 모기 개채수가 달라진것 같다
# 또한 모델적으로 아쉬운것은 변수간의 상관관계가 약해서라고 생각된다.
# 최근에 모기가 적은 이유를 조사해 보았을시 기상이변이랑 심한 폭우였는데
# 강수량 데이터가 비 안오는날을 0으로 표시하기에 모델이 잘 적합을 못하고
# 강수량이 적당히 많으면 습도때문에 모기가 늘고 강수량이 심하게 순간적으로 많으면 모기 알들이 씻겨 내려가면서
# 개채수가 줄어드는데 이런 변화를 모델들이 적합을 잘 못하다 보니 아쉬운 정확도가 나온것 같다.
# 특히 이렇게 심한 폭우로 알이 씻겨나간는 경우는 2015년부터 조사한 데이터로는 알아채기 힘든 이벤트이다.


# Boosting에서 정확도가 낮은 이유는
# 이번에 모기가 비정상적으로 적어서 비슷한 환경에서도 모기 개채수가 적어지는것을 모델이 예측하질 못했다.

# KNN에서 정확도가 높은 이유는 먼저 이진분류라서 정확도가 높게 나올수밖에 없고
# 특히 No를 예측하기 위한 데이터는 상대적으로 적기도 하고 모기 개채수가 에초에 적었기 때문에 specificity가 높아지면서
# 정확도가 높아졌다고 생각한다.
# 하지만 sensitivity같은 경우는 훈련데이터에서 yes를 예측할 수 있는 데이터가 많고
# 모기가 많았던 훈련데이터에대해 적합을 하여서 이번에 비슷한 환경에서도 모기의 개채수가 줄어서 yes예측률이 낮다
