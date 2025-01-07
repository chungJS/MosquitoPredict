# MosquitoPredict

MosquitoPredict is a project that predicts the number of mosquitoes in Seoul and provides a yes-or-no answer to your vacation plans if you’re concerned about mosquitoes.
The mosquito count is influenced by [weather](https://www.riss.kr/search/detail/DetailView.do?p_mat_type=be54d9b8bc7cdb09&control_no=c3a3273d67112aa4ffe0bdc3ef48d419&keyword=%EB%AA%A8%EA%B8%B0) and [water quality](https://www.riss.kr/search/detail/DetailView.do?p_mat_type=be54d9b8bc7cdb09&control_no=d2c2659107fad4e7ffe0bdc3ef48d419&keyword=%EB%AA%A8%EA%B8%B0%20%EC%88%98%EC%A7%88)

Weather data from the National Weather Service, mosquito count data from Seoul and water quality data will be preprocessed using Python.
The models will then be compared using R to determine the best model to provides answer

<!-- vim-markdown-toc GFM -->

- [Prerequisite](#Prerequisite)
- [Features](#features)
- [Conclusion](#Conclusion)
- [Reference](#Reference)

<!-- vim-markdown-toc -->

## Prerequisite

- R
- Jupyter notebook
- [Mosquito count data](https://news.seoul.go.kr/welfare/archives/532165)
- [Weather Data](https://data.kma.go.kr/data/grnd/selectAsosRltmList.do?pgmNo=36)
- [Water quality data](https://swo.seoul.go.kr/water/waterMesntkInfo.do?)

## Features

### Preprocessing

1. extract only the necessary columns from raw dataset
2. convert hourly water quality data into daily data by calculating average
3. merge 2017~2022 data
4. remove outliers

dataset

![dataset](https://github.com/chungJS/predicts_mosquito/raw/main/img/preprocessed_dataset.png)

### Comparing

- test the models accuracy using cross-validation method
- tuning the parameter to find out best parameter

### Result

accuracy of each models using 2017~2022 dataset

![result](https://github.com/chungJS/predicts_mosquito/raw/main/img/result.png)

## Conclusion

The best model was KNN with k=2, achieving an accuracy of 75% when predicting mosquito data using 2023 weather and water quality data.

![knn_result](https://github.com/chungJS/predicts_mosquito/raw/main/img/knn_result.png)

The low accuracy was due to the [abnormal](https://www.phwr.org/journal/view.html?uid=716&vmd=Full) mosquito population data in 2023.

![mosquito_count](https://github.com/chungJS/predicts_mosquito/raw/main/img/mosquito_count.png)

## Reference

[장진영. "모기 밀도와 기후 요인과의 연관성." 국내석사학위논문 고려대학교 보건대학원, 2014. 서울](https://www.riss.kr/search/detail/DetailView.do?p_mat_type=be54d9b8bc7cdb09&control_no=c3a3273d67112aa4ffe0bdc3ef48d419&keyword=%EB%AA%A8%EA%B8%B0)
[한중수. "국내 습지에 분포하는 모기유충의 서식처 특성 및 생물학적 방제기법 연구." 국내석사학위논문 상지대학교, 2019. 강원특별자치도](https://www.riss.kr/search/detail/DetailView.do?p_mat_type=be54d9b8bc7cdb09&control_no=d2c2659107fad4e7ffe0bdc3ef48d419&keyword=%EB%AA%A8%EA%B8%B0%20%EC%88%98%EC%A7%88)
[Byung-Eon Noh, Soeun Shin, Hyunwoo Kim, Jung-Won Ju, Hee-Il Lee. Surveillance of Japanese Encephalitis Vector Mosquito Culex tritaeniorhynchus in the Republic of Korea, 2023. Public Health Weekly Report 2024;17:1021-1033.](https://www.phwr.org/journal/view.html?uid=716&vmd=Full)
