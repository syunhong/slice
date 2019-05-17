###예제

load(file.choose())

a <- chuchool[1:1000]
a <- lapply(a, function(z) {
  hehe <- z
  names(hehe) <- c("household", "individual", "trips")
  return(hehe)}
)


## 1) 오전 5시의 인구분포를 나타내는 데이터 프레임

number1 <- slice(a, at = 500)

number1_df <- translice(number1)

## 2) 오전 오후 6시 48분의 인구분포를 나타내는 데이터 프레임

number2 <- slice(a, at = 1848)

number2_df <- translice(number2)

## 3) 오후 3시의 인구분포를 가구소득별로

number3 <- slice(a, at = 1500)

number3_df <- translice(number3, "income")

## 4) 통근 목적의 인구분포를 가구소득별로

number4 <- slice(a, purpose = 4)

number4_df <- translice(number4, "income")

## 5) 오전 10시의 통근 목적의 인구분포를 직업별로

number5 <- slice(a, at = 1000, purpose = 4)

number5_df <- translice(number5, "occupation")

## slice.another 예시, 성별이 여자이며, 면허가 있는 사람

number6 <- slice.another(a, "sex == 2 & license == 1")

number6_df <- translice(number6)

## slice.mode 예시, 도보 통행인 사람들의 현재 위치 예시

number7 <- slice.mode(a, 1)

number7_df <- translice(number7)
