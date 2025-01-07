#############
# 환경 설정 #
#############

# 환경 설정 - * 파일 읽어오는 위치 정의해줄 것 *

# 파일 경로 내꺼로 바꿈
for (i in sequence(1)) {
  setwd("~/ProjectR/Chapter01/source")
  Sys.setlocale("LC_ALL", "en_US.UTF-8")
  # Sys.setlocale("LC_ALL", "Korean_Korea.949")
}

# 데이터 셋 train_data로 정의

# 파일 경로 내꺼로 바꿈
for (i in sequence(1)) {
  # 훈련용 데이터셋을 train_data에 저장
  train_data <- read.csv("training_dataset.csv", fileEncoding = "CP949")

  # fix(train_data)
  # print(names(train_data))
  # "Date", "Mosquito", "Averagetemperature", "Dailyprecipitation", "Averagewindspeed", "Averagerelativehumidity", "Averagevaporpressure",
  # "Totalsunshinehours", "Totalsolarradiation", "Averagegroundtemperature", "Watertemperature", "Ph", "Dissolvedoxygen"
}

# Normal Distribution(정규분포화) 시킨 데이터를 ND_data로 정의
for (i in 1:1) {
  ND_data <- train_data
  ND_data$Mosquito <- scale(ND_data$Mosquito)
  ND_data$Averagetemperature <- scale(ND_data$Averagetemperature)
  ND_data$Dailyprecipitation <- scale(ND_data$Dailyprecipitation)
  ND_data$Averagewindspeed <- scale(ND_data$Averagewindspeed)
  ND_data$Averagerelativehumidity <- scale(ND_data$Averagerelativehumidity)
  ND_data$Averagevaporpressure <- scale(ND_data$Averagevaporpressure)
  ND_data$Totalsunshinehours <- scale(ND_data$Totalsunshinehours)
  ND_data$Totalsolarradiation <- scale(ND_data$Totalsolarradiation)
  ND_data$Averagegroundtemperature <- scale(ND_data$Averagegroundtemperature)
  ND_data$Watertemperature <- scale(ND_data$Watertemperature)
  ND_data$Ph <- scale(ND_data$Ph)
  ND_data$Dissolvedoxygen <- scale(ND_data$Dissolvedoxygen)
  ND_data$Date_Labeled <- scale(ND_data$Date_Labeled)
  # print(ND_data)
}

# 날짜 속성 제거한 데이터를 data_no_date로 정의
for (j in c(1)) {
  library(dplyr)
  train_data_no_date <- dplyr::select(train_data, -Date)
  # print(names(train_data_no_date))
  ND_data_no_date <- dplyr::select(ND_data, -Date)
  # print(names(ND_data_no_date))
}

################################
# 선형 회귀(Linear Regression) #
################################

# 선형 회귀
for (j in sequence(1)) {
  # 날짜를 제외한 설명변수들로 종속변수를 설명하는 다중선형회귀 모델
  lR.mod <- lm(
    Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = train_data
  )

  # 평균온도, 평균 상대습도, 평균 증기압, 합계 일사량, 수온의 p-value가 <0.001
  print(summary(lR.mod))
}


###############################
# 교차 검증(Cross Validation) #
###############################

# 검증셋 기법
# 데이터를 triaining data와 Validation data로 나누어서 검정오차를 확인

# set.seed를 이용해 5번의 선형회귀 모델 테스트
# 시쿼스1에서 5로 변경
for (i in sequence(5)) {
  set.seed(i)
  cat(i, "번째 검증셋 기법\n")
  # 결과를 그래프로 보여주기 위한 벡터 생성
  par(mfrow = c(1, 2))
  v_MSE_val <- rep(1:3)
  v_R_val <- rep(1:3)

  # training data/ validation data를 나누기 위한 인덱스 리스트들
  validation_data_index <- (1:1016) # 결국 validation은 216개가 남게됨. All - training = validation
  training_data_index <- sample(validation_data_index, 800) # validation 개수 중에서 800개를 뽑아감.

  # 데이터셋에서 training validation data 추출
  # 정규화한 데이터셋을 이용해 mse를 알아내고 일반 데이터 셋을 이용해 R^2값을 도출
  mse_train_data <- ND_data[training_data_index, ]
  cor_train_data <- train_data[training_data_index, ]

  # 1~3차항의 다중선형회귀 모델을 정규화 데이터중 training data 부분만으로 적합
  lm.fit_mse <- lm(
    Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = ND_data, subset = training_data_index
  )
  lm.fit2_mse <- lm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
    Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
    Dissolvedoxygen, degree = 2, raw = T), data = ND_data, subset = training_data_index)
  lm.fit3_mse <- lm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
    Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
    Dissolvedoxygen, degree = 3, raw = T), data = ND_data, subset = training_data_index)

  # 1~3차항의 다중선형회귀 모델을 일반 데이터중 training data 부분만으로 적합
  lm.fit_cor <- lm(
    Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = train_data, subset = training_data_index
  )
  lm.fit2_cor <- lm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
    Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
    Dissolvedoxygen, degree = 2, raw = T), data = train_data, subset = training_data_index)
  lm.fit3_cor <- lm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
    Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
    Dissolvedoxygen, degree = 3, raw = T), data = train_data, subset = training_data_index)

  # 검증셋 기법을 통한 MSE, R^2값을 도출
  v_MSE_val[1] <- mean((mse_train_data$Mosquito - predict(lm.fit_mse, mse_train_data))[-training_data_index]^2)
  v_R_val[1] <- cor(
    cor_train_data$Mosquito[-training_data_index],
    predict(lm.fit_cor, cor_train_data)[-training_data_index]
  )^2
  cat("1차식 - MSE값 : ", v_MSE_val[1], ", ")
  cat("R제곱값 : ", v_R_val[1], "\n")

  v_MSE_val[2] <- mean((mse_train_data$Mosquito - predict(lm.fit2_mse, mse_train_data))[-training_data_index]^2)
  v_R_val[2] <- cor(
    cor_train_data$Mosquito[-training_data_index],
    predict(lm.fit2_cor, cor_train_data)[-training_data_index]
  )^2
  cat("2차식 - MSE값 : ", v_MSE_val[2], ", ")
  cat("R제곱값 : ", v_R_val[2], "\n")

  v_MSE_val[3] <- mean((mse_train_data$Mosquito - predict(lm.fit3_mse, mse_train_data))[-training_data_index]^2)
  v_R_val[3] <- cor(
    cor_train_data$Mosquito[-training_data_index],
    predict(lm.fit3_cor, cor_train_data)[-training_data_index]
  )^2
  cat("3차식 - MSE값 : ", v_MSE_val[3], ", ")
  cat("R제곱값 : ", v_R_val[3], "\n")

  plot(v_MSE_val, type = "b")
  plot(v_R_val, type = "b")

  cat("\n=================================\n\n")
}


# LOOCV (Leav-One-Out Cross-validation)
# 하나의 데이터만을 validation data로 두고 나머지 데이터를 training

library(boot)
# set.seed를 이용해 LOOCV가 항상 같은 값을 출력할 것을 보여줌
for (j in sequence(1)) {
  set.seed(j)
  cv.err_mse <- rep(0, 10)
  cv.err_R <- rep(0, 10)
  cat(j, "번째 LOOCV\n\n")
  for (i in 1:10) {
    glm.fit_mse <- glm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
      Dissolvedoxygen, degree = i, raw = T), data = ND_data)

    cv_mse_model <- cv.glm(ND_data, glmfit = glm.fit_mse)
    cv.err_mse[i] <- cv_mse_model$delta[1]
    cat(i, "차식의 LOOCV - MSE값 : ", cv.err_mse[i], ", ")

    glm.fit_cor <- glm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity +
      Averagevaporpressure + Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature +
      Watertemperature + Ph + Dissolvedoxygen, degree = i, raw = TRUE), data = train_data)

    cv_R_model <- cv.glm(train_data, glmfit = glm.fit_cor)
    cv.err_R[i] <- 1 - cv_R_model$delta[1] / var(train_data$Mosquito)
    cat("R제곱값", cv.err_R[i], "\n")
  }
  cat("\n=================================\n\n")
  # 결과를 그래프로 출력
  par(mfrow = c(1, 2))
  plot(cv.err_mse, type = "b")
  plot(cv.err_R, type = "b")
}


# K-FOLD
# K개로 데이터를 나눠서 1개의 데이터를 validation data로 두고 k-1개의 데이터로 training

# set.seed를 이용해 테스트
# k폴드 값을 10에서 5로 변경함 그리고 시퀀스를 5로 바꿈
for (j in sequence(5)) {
  set.seed(j)
  cv.err_k10_mse <- rep(0, 5)
  cv.err_k10_R <- rep(0, 5)
  cat(j, "번째 K-FOLD\n\n")
  for (i in 1:10) {
    glm.fit_mse <- glm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
      Dissolvedoxygen, degree = i, raw = T), data = ND_data)

    cv_mse_k10_model <- cv.glm(ND_data, glmfit = glm.fit_mse, K = 5)
    cv.err_k10_mse[i] <- cv_mse_k10_model$delta[1]
    cat(i, "차식의 K-FOLD - MSE : ", cv.err_k10_mse[i], ", ")
    glm.fit_cor <- glm(Mosquito ~ poly(Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity +
      Averagevaporpressure + Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature +
      Watertemperature + Ph + Dissolvedoxygen, degree = i, raw = TRUE), data = train_data)
    cv_R_k10_model <- cv.glm(train_data, glmfit = glm.fit_cor, K = 5)
    cv.err_k10_R[i] <- 1 - cv_R_k10_model$delta[1] / var(train_data$Mosquito)
    cat("R제곱값", cv.err_k10_R[i], "\n")
  }
  cat("\n=================================\n\n")
  # 결과를 그래프로 출력
  par(mfrow = c(1, 2))
  plot(cv.err_k10_mse, type = "b")
  plot(cv.err_k10_R, type = "b")
}


#############
# 모델 개선 #
#############

# Best subset
# 유의미한 설명변수들만을 찾기 위해서 검증

library(leaps)
# 기본적인 subset 정보 출력
for (j in c(1)) {
  regfit.full <- regsubsets(Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
    Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph +
    Dissolvedoxygen, data = train_data, nvmax = 11)
  # print(summary(regfit.full))
  par(mfrow = c(2, 2))
  plot(regfit.full, scale = "r2")
  plot(regfit.full, scale = "adjr2")
  plot(regfit.full, scale = "Cp")
  plot(regfit.full, scale = "bic")
}

# 검증셋 기법
for (j in c(1)) {
  set.seed(1)
  train <- sample(c(TRUE, FALSE), nrow(train_data), rep = TRUE)
  test <- (!train)

  regfit.best <- regsubsets(
    Mosquito ~ Averagetemperature + Dailyprecipitation +
      Averagewindspeed + Averagerelativehumidity +
      Averagevaporpressure + Totalsunshinehours + Totalsolarradiation +
      Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = train_data[train, ], nvmax = 11
  )
  test.mat <- model.matrix(
    Mosquito ~ Averagetemperature + Dailyprecipitation +
      Averagewindspeed + Averagerelativehumidity +
      Averagevaporpressure + Totalsunshinehours + Totalsolarradiation +
      Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = train_data[test, ]
  )
  val.errors <- rep(NA, 11)
  for (i in 1:11) {
    coefi <- coef(regfit.best, id = i)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors[i] <- mean((train_data$Mosquito[test] - pred)^2)
  }
  # print(val.errors)
  # print(which.min(val.errors))
  par(mfrow = c(1, 1))
  plot(val.errors, type = "b")

  print(coef(regfit.best, 6))

  # 결론
  # 검증셋 기법으로는 6번째 subset이 가장 성능 좋음
  #
  #   (Intercept)           Averagetemperature        Averagerelativehumidity   Averagevaporpressure
  #   -5209.74215           242.95522                 44.16168                  -197.74509
  #   Totalsolarradiation   Averagegroundtemperature  Watertemperature
  #   -35.47716             27.45580                  125.94524
  # 강수량, 평균풍속, 합계일조시간, Ph, 용존산소량 제외
}

# 교차 검증
for (k in c(1)) {
  set.seed(1)

  # 부분적 regsubset()에서 predict 멤버함수를 쓰기 위해 함수 선언
  predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
  }

  k <- 10 # k-fold 10번으로 교차검증
  folds <- sample(1:k, nrow(train_data), replace = TRUE)
  cv.errors <- matrix(NA, k, 11, dimnames = list(NULL, paste(1:11)))
  for (j in 1:k) {
    best.fit <- regsubsets(
      Mosquito ~ Averagetemperature + Dailyprecipitation +
        Averagewindspeed + Averagerelativehumidity +
        Averagevaporpressure + Totalsunshinehours + Totalsolarradiation +
        Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
      data = train_data[folds != j, ], nvmax = 11
    )
    for (i in 1:11) {
      pred <- predict(best.fit, train_data[folds == j, ], id = i)
      cv.errors[j, i] <- mean((train_data$Mosquito[folds == j] - pred)^2)
    }
  }
  mean.cv.errors <- apply(cv.errors, 2, mean) # 2차원 행렬에서 각 열에 대해 평균 계산
  # print(mean.cv.errors)
  # print(which.min(mean.cv.errors))
  par(mfrow = c(1, 1))
  plot(mean.cv.errors, type = "b")

  print(coef(best.fit, 5))

  # 결론
  # 검증셋 기법으로는 6번째 subset이 가장 성능 좋음
  #   (Intercept)   Averagetemperature  Averagerelativehumidity   Averagevaporpressure    Totalsolarradiation   Watertemperature
  #   -5826.46500   267.85925           47.64005                  -211.05991              -29.75649             153.84336
  # 강수량, 평균풍속, 합계일조시간, Ph, 용존산소량 + 평균지면온도 제외
}

# 전체 자료에서 변수모델 선택
for (j in c(1)) {
  reg.best <- regsubsets(
    Mosquito ~ Averagetemperature + Dailyprecipitation +
      Averagewindspeed + Averagerelativehumidity +
      Averagevaporpressure + Totalsunshinehours + Totalsolarradiation +
      Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = train_data, nvmax = 11
  )
  # print(coef(reg.best,6)) # 검증셋 기법 최고 서브셋
  # print(coef(reg.best,5)) # 교차검증 최고 서브셋
}


# 능형회귀(ridge)랑 Lasso는 overfitting을 막기 위해 진행하는 최적화

# ridge와 Lasso에서 필요한 변수 정의
for (j in c(1)) {
  # x, y, grid, train, test, y.test를 정의
  x <- model.matrix(Mosquito ~ ., data = ND_data)[, -1]
  y <- ND_data$Mosquito
  grid <- 10^seq(10, -2, length = 100)
  train <- sample(1:nrow(x), nrow(x) / 2)
  test <- (-train)
  y.test <- y[test]
}
library(glmnet)

# 능형회귀 (ridge)
# 설명변수의 제곱수를 제한하기 위한 값을 정해두는 것 - 최소제곱법이지만 람다 값으로 계수가 높아지는걸 패널티를 주는 기법

# 교차 검증을 통해 오차가 가장 적은 람다를 찾고 MSE를 출력
for (j in c(1)) {
  # 교차 검증
  set.seed(1)
  cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
  plot(cv.out)
  bestlam <- cv.out$lambda.min
  # print(bestlam)

  ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
  ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
  cat("람다 값 : ", bestlam, ", 검정 MSE 값 : ", mean((ridge.pred - y.test)^2), "\n")

  # 계수 추정치 출력
  out <- glmnet(x, y, alpha = 0)
  # print(predict(out,type="coefficients",s=bestlam)[1:12,])
}

# Lasso
# 각 계수의 절댓값의 합을 0으로 만들려고 하는 알파를 이용해 계수들의 크기를 제한하고 덜 중요한 계수들을 0으로 만들어서 subset selection 효과도 있음

# 교차검증을 통해 오차가 가장 적은 람다를 찾고 MSE를 출력
for (j in c(1)) {
  # 교차 검증
  set.seed(1)
  cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
  # plot(cv.out)
  bestlam <- cv.out$lambda.min

  lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
  # plot(lasso.mod)
  lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
  cat("Lasso를 이용해서 선형회귀 튜닝")
  # cat('사용한 람다 값 : ', bestlam)
  cat("검정 MSE 값 : ", mean((lasso.pred - y.test)^2), ", R제곱 값 : ", cor(lasso.pred, y.test)^2, "\n")

  out <- glmnet(x, y, alpha = 1, lambda = grid)
  lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:12, ]
  # print(lasso.coef)
  # (Intercept)           Averagetemperature    Dailyprecipitation    Averagewindspeed          Averagerelativehumidity
  # -2702.606511          11.411597             0.000000              0.000000                  0.000000
  # Averagevaporpressure  Totalsunshinehours    Totalsolarradiation   Averagegroundtemperature  Watertemperature
  # -1.304807             -4.924152             -14.845628            28.913962                 163.342553
  # Ph                    Dissolvedoxygen
  # 104.738752            0.000000

  # print(lasso.coef[lasso.coef!=0])
  # 강수량, 평균풍속, 평균상대습도, 용존산소량 제외
}


##################################
# SVR (Suppor Vector Regression) #
##################################

library(e1071)
# SVR
# SVR을 쓰는 이유

# 최적의 파라미터를 구하고 SVR모델 구축 후 R^2값 도출
####
# SVR MSE값 추가
for (j in c(1)) {
  set.seed(1)

  # train, test data로 나누기
  t_index <- sample(1:nrow(train_data), size = nrow(train_data) * 0.7)
  train <- train_data[t_index, ]
  test <- train_data[-t_index, ]

  # 추가부분 mse데이터를위해 ND_data
  mse_index <- sample(1:nrow(ND_data), size = nrow(ND_data) * 0.7)
  mse_train <- ND_data[t_index, ]
  mse_test <- ND_data[-t_index, ]


  # gamma랑 코스트 최적의 값 구해야 함 - 오래 걸려서 주석처리
  # svm_tune <- tune.svm(Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
  #           Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen
  #         , data = train_data, gamma = c(0.1,0.3,0.5,0.7,1), cost=10^(-1:3))
  # print(summary(svm_tune))

  # Parameter tuning of ‘svm’:
  #- sampling method: 10-fold cross validation
  #- best parameters:
  # gamma cost
  #  0.3    1
  #- best performance: 705683.4

  # SVR 모델 학습
  svmfit <- svm(
    Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = train, gamma = 0.3, cost = 1
  )

  # mse_svmfit 추가
  mse_svmfit <- svm(
    Mosquito ~ Averagetemperature + Dailyprecipitation + Averagewindspeed + Averagerelativehumidity + Averagevaporpressure +
      Totalsunshinehours + Totalsolarradiation + Averagegroundtemperature + Watertemperature + Ph + Dissolvedoxygen,
    data = mse_train, gamma = 0.3, cost = 1
  )

  par(mfrow = c(1, 2))
  # SVR 모델 평가
  yhat_test <- predict(svmfit, test)

  # 추가된 MSE코드 부분
  mse_yhat_test <- predict(mse_svmfit, mse_test)
  svr.mse <- mean((mse_yhat_test - mse_test$Mosquito)^2)
  cat("SVR MSE 값 : ", svr.mse)
  plot(x = mse_test$Mosquito, y = mse_yhat_test, main = "SVR_MSE")



  plot(x = test$Mosquito, y = yhat_test, main = "SVR")
  svm.R2 <- cor(test$Mosquito, yhat_test)^2
  cat("\t SVR R제곱값 : ", svm.R2)
}


##################
# 분류/회귀 Tree #
##################

library(tree)

# 분류트리를 이용해 모기양에 따라 여행을 가도 괜찮은지 예측

# 이진분류를 위해 여행가도 괜찮다는 Travel 속성을 추가한 tree_data 정의
for (j in sequence(1)) {
  Travel <- ifelse(train_data_no_date$Mosquito <= 1500, "No", "Yes") # 너무 기준이 낮은거 같긴 함
  tree_data <- data.frame(train_data_no_date, Travel)
  tree_data$Travel <- as.factor(tree_data$Travel)
  tree_data$Mosquito <- as.numeric(tree_data$Mosquito)
}

# 트리구조 출력 후 검정 셋 기법으로 정확도 확인
# 트리 테이블이 나오게 되면 자동으로 정확도가 나오게 하는 코드 추가
for (j in sequence(1)) {
  set.seed(1)
  train <- sample(1:nrow(tree_data), 508)
  tree_data.test <- tree_data[-train, ]
  Travel.test <- Travel[-train]
  tree.tree_data <- tree(Travel ~ . - Mosquito, tree_data, subset = train)
  plot(tree.tree_data)
  text(tree.tree_data, pretty = 0)
  tree.pred <- predict(tree.tree_data, tree_data.test, type = "class")
  print(table(tree.pred, Travel.test))

  #           Travel.test
  # tree.pred         No    Yes
  #             No    63    20
  #             Yes   32    393           -> accuracy = (63+393) / 508 = 0.8976378 = 89.8%

  # 추가된 테이블 정확도 계산 코드
  tree_matrix <- table(tree.pred, Travel.test)

  sumdata <- sum(tree_matrix) # 전체 데이터 개수
  tree_n_n <- tree_matrix[1, 1] # 'No'로 예측하고 'No'로 실제값인 경우
  tree_y_y <- tree_matrix[2, 2] # 'Yes'로 예측하고 'Yes'로 실제값인 경우

  cat("\n정확도 : ", (tree_n_n + tree_y_y) / sumdata)
  cat("\n약", (tree_n_n + tree_y_y) / sumdata * 100, "%")
}

# Pruning(가지치기)을 통해 개선하기 위해 교차검증 기법으로 최적의 트리복잡도 수준 확인 후 정확도 확인
for (j in sequence(1)) {
  set.seed(1)
  cv.tree_data <- cv.tree(tree.tree_data, FUN = prune.misclass)
  # print(cv.tree_data)
  # par(mfrow=c(2,1))
  # plot(cv.tree_data$size,cv.tree_data$dev,type="b")
  # plot(cv.tree_data$k,cv.tree_data$dev,type="b")
  # 값을 보면 6개의 터미널 노드를 가진 트리에서 가장 낮은 교차 검증 오차율이 얻어짐.

  prune.tree_data <- prune.misclass(tree.tree_data, best = 6)
  par(mfrow = c(1, 1))
  plot(prune.tree_data)
  text(prune.tree_data, pretty = 0)
  ptree.pred <- predict(prune.tree_data, tree_data.test, type = "class")
  print(table(ptree.pred, Travel.test))
  #             Travel.test
  # ptree.pred        No  Yes
  #             No    59  15
  #             Yes   36  398           -> accuracy = (59+398) / 508 = 0.8996063 = 90%
  # best값 증가시 정확도는 낮지만 가지치기 더 잘된 트리 얻음

  # 추가된 테이블 정확도 계산 코드
  prune_matrix <- table(ptree.pred, Travel.test)

  sumdata <- sum(prune_matrix) # 전체 데이터 개수
  prune_n_n <- prune_matrix[1, 1] # 'No'로 예측하고 'No'로 실제값인 경우
  prune_y_y <- prune_matrix[2, 2] # 'Yes'로 예측하고 'Yes'로 실제값인 경우

  # cat(prune_n_n, prune_y_y)

  cat("\n정확도 : ", (prune_n_n + prune_y_y) / sumdata)
  cat("\n약", (prune_n_n + prune_y_y) / sumdata * 100, "%")
}

# 회귀트리를 이용해 모기 수를 예측하는 모델 ND_data_no_date
for (j in sequence(1)) {
  set.seed(1)
  train <- sample(1:nrow(train_data_no_date), nrow(train_data_no_date) / 2)
  r_tree <- tree(Mosquito ~ ., train_data_no_date, subset = train)
  # print(summary(r_tree))

  cv.rtree <- cv.tree(r_tree)
  plot(cv.rtree$size, cv.rtree$dev, type = "b")
  prune.rtree <- prune.tree(r_tree, best = 4)
  plot(prune.rtree)
  text(prune.rtree, pretty = 0)

  yhat <- predict(r_tree, newdata = train_data_no_date[-train, ]) # pruning 한 모델이 더 안좋게 나옴...
  rtree.test <- train_data_no_date[-train, "Mosquito"]
  plot(yhat, rtree.test)
  abline(0, 1)

  # MSE 코드 수정
  rtree_mse_train <- sample(1:nrow(ND_data_no_date), nrow(ND_data_no_date) / 2)
  r_mse_tree <- tree(Mosquito ~ ., ND_data_no_date, subset = rtree_mse_train)
  mse_cv.rtree <- cv.tree(r_mse_tree)
  plot(mse_cv.rtree$size, mse_cv.rtree$dev, type = "b")
  mse_prune.rtree <- prune.tree(r_mse_tree, best = 4)
  plot(mse_prune.rtree)
  text(mse_prune.rtree, pretty = 0)
  mse_retre_yhat <- predict(r_mse_tree, newdata = ND_data_no_date[-rtree_mse_train, ])
  mse_rtree.test <- ND_data_no_date[-rtree_mse_train, "Mosquito"]
  plot(mse_retre_yhat, mse_rtree.test)
  abline(0, 1)

  rtree_MSE <- mean((mse_retre_yhat - mse_rtree.test)^2)

  # print(mean(yhat-rtree.test)) 26정도의 오차가 있음을 볼 수 있음
  rtree_R <- cor(rtree.test, yhat)^2
  cat("회귀트리의 R제곱 값 : ", rtree_R, "MSE 값 : ", rtree_MSE, "\n")
}


#####################
# Tree-Based Method #
#####################

library(randomForest)
# 검증셋 기법을 위한 train, train_data.test 정의
for (j in sequence(1)) {
  set.seed(1)
  train <- sample(1:nrow(train_data_no_date), nrow(train_data_no_date) / 2)
  train_data.test <- train_data_no_date[-train, "Mosquito"]

  random_mse_train <- sample(1:nrow(ND_data_no_date), nrow(ND_data_no_date) / 2)
  random_mse_test <- ND_data_no_date[-random_mse_train, "Mosquito"]
}

# 배깅
for (j in sequence(1)) {
  bag.mod <- randomForest(Mosquito ~ ., data = train_data_no_date, subset = train, mtry = 12, importance = TRUE)
  # print(bag.mod)

  yhat.bag <- predict(bag.mod, newdata = train_data_no_date[-train, ])
  # plot(yhat.bag , train_data.test)
  # abline (0,1)
  bag_R <- cor(train_data.test, yhat.bag)^2

  # MSE코드 수정 부분 추가
  mse_bag.mod <- randomForest(Mosquito ~ ., data = ND_data_no_date, subset = random_mse_train, mtry = 12, importance = TRUE)

  mse_yhat.bag <- predict(mse_bag.mod, newdata = ND_data_no_date[-random_mse_train, ])

  # plot(mse_yhat.bag , random_mse_test)
  # abline (0,1)
  bag_MSE <- mean((mse_yhat.bag - random_mse_test)^2)

  cat("배깅의 R제곱 값 : ", bag_R, "MSE 값 : ", bag_MSE, "\n")
  # ntree 25로 해보면 MSE값 증가하므로 제외
}


# 랜덤 포레스트
for (j in sequence(12)) {
  rf.mod <- randomForest(Mosquito ~ ., data = train_data_no_date, subset = train, mtry = j, importance = TRUE)
  yhat.rf <- predict(rf.mod, newdata = train_data_no_date[-train, ])

  # MSE 추가부분
  mse_rf.mod <- randomForest(Mosquito ~ ., data = ND_data_no_date, subset = random_mse_train, mtry = j, importance = TRUE)
  mse_yhat.rf <- predict(mse_rf.mod, newdata = ND_data_no_date[-random_mse_train, ])

  rf_MSE <- mean((mse_yhat.rf - random_mse_test)^2)
  rf_R <- cor(train_data.test, yhat.rf)^2
  cat("mtry가 ", j, "인 랜덤 포레스트의 R제곱 값 : ", rf_R, "MSE 값 : ", rf_MSE, "\n")

  # print(importance (rf.mod))
  # varImpPlot(rf.mod)
  # 우리는 배깅의 성능이 더 좋음(mtry = 12이면 배깅이랑 같음)
}

library(gbm)
# 부스팅
for (j in sequence(1)) {
  # 이진 문제라면 distribution="bernoulli"
  boost.mod <- gbm(Mosquito ~ ., data = train_data_no_date[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
  # print(summary(boost.mod))
  yhat.boost <- predict(boost.mod, newdata = train_data_no_date[-train, ], n.trees = 5000)

  # MSE 코드 추가
  mse_boost.mod <- gbm(Mosquito ~ ., data = ND_data_no_date[random_mse_train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
  mse_yhat.boost <- predict(mse_boost.mod, newdata = ND_data_no_date[-random_mse_train, ], n.trees = 5000)


  cat("일반 부스팅 모델 MSE : ", mean((mse_yhat.boost - random_mse_test)^2), "\n")
  for (i in 10^(-1:-5)) {
    boost.mod <- gbm(Mosquito ~ ., data = train_data_no_date[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = i, verbose = F)
    yhat.boost <- predict(boost.mod, newdata = train_data_no_date[-train, ], n.trees = 5000)
    mse_boost.mod <- gbm(Mosquito ~ ., data = ND_data_no_date[random_mse_train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
    mse_yhat.boost <- predict(mse_boost.mod, newdata = ND_data_no_date[-random_mse_train, ], n.trees = 5000)
    boost_MSE <- mean((mse_yhat.boost - random_mse_test)^2)
    boost_R <- cor(train_data.test, yhat.boost)^2
    cat("람다값이", i, "일때 R^2 값 : ", boost_R, "MSE 값 : ", boost_MSE, "\n")
    # 람다 값이 낮아지면 MSE좋아지는게 과적합? 테스트 해봐야 할듯
  }
}


#######
# KNN #
#######

library(class)
# 이진분류를 Travel 속성을 추가하고 knn 분류모델 성능 확인
for (j in sequence(1)) {
  Travel <- ifelse(train_data_no_date$Mosquito <= 1500, "No", "Yes")
  knn_data <- data.frame(train_data_no_date, Travel)
  knn_data$Travel <- as.factor(knn_data$Travel)
  std_knn <- scale(knn_data[, -13])

  set.seed(1)
  train <- sample(1:nrow(std_knn), nrow(std_knn) / 2)
  train_knnx <- std_knn[train, ]
  test_knnx <- std_knn[-train, ]
  train_knny <- knn_data$Travel[train]
  test_knny <- knn_data$Travel[-train]

  cat("그냥 갈때 가도 되는 날일지의 확률 : ", mean(test_knny != "No"), "\n") # 그냥 가도 되느날이라고 찍으면 나오는 임의 추측 확률
  for (i in sequence(15)) {
    knn.pred <- knn(train_knnx, test_knnx, train_knny, k = i)
    knn_t <- table(knn.pred, test_knny)
    cat(
      "k가", i, "일때 knn 결과 정확도 : ", mean(test_knny == knn.pred), ", 여행 가도 된다고 판단했을때 정말 가도 되는 확률 : ",
      knn_t[2, 2] / (knn_t[2, 2] + knn_t[2, 1]), "\n"
    )
  }
}

# KNN 회귀 모델 성능 확인
# install.packages('FNN')
library("FNN")
for (i in sequence(20)) {
  set.seed(1)
  train <- sample(1:nrow(train_data_no_date), nrow(train_data_no_date) / 2)
  r_knn_train <- train_data_no_date[train, ]
  r_knn_test <- train_data_no_date[-train, ]

  r_knnx_train <- model.matrix(Mosquito ~ ., data = r_knn_train)[, -1]
  Mosquito_train <- r_knn_train$Mosquito
  r_knnx_test <- model.matrix(Mosquito ~ ., data = r_knn_test)[, -1]

  yhat_test <- knn.reg(r_knnx_train, r_knnx_test, Mosquito_train, k = i)

  knn_R <- cor(r_knn_test$Mosquito, yhat_test$pred)^2

  # MSE 코드 추가
  mse_train <- sample(1:nrow(ND_data_no_date), nrow(ND_data_no_date) / 2)
  mse_r_knn_train <- ND_data_no_date[mse_train, ]
  mse_r_knn_test <- ND_data_no_date[-mse_train, ]

  mse_r_knnx_train <- model.matrix(Mosquito ~ ., data = mse_r_knn_train)[, -1]
  mse_Mosquito_train <- mse_r_knn_train$Mosquito
  mse_r_knnx_test <- model.matrix(Mosquito ~ ., data = mse_r_knn_test)[, -1]

  mse_yhat_test <- knn.reg(mse_r_knnx_train, mse_r_knnx_test, mse_Mosquito_train, k = i)

  knn_MSE <- mean((mse_yhat_test$pred - mse_r_knn_test$Mosquito)^2)

  cat("k가", i, "일때 R^2 값 : ", knn_R, "MSE 값 : ", knn_MSE, "\n")
}
