setwd("C:/Users/locklock/Desktop/이것은 함수 스크립트/2019.06.07/r")

load(file.choose()) ## sampledata
load(file.choose()) ## switch, dongcode data

ASL10 <- new("ASL", sample10)

ASL16 <- new("ASL", sample16)

s1 <- slice(ASL16, 1200) ## 16년도 전과 같은 12시의 분포
s2 <- slice(ASL10, 1200) ## 10년도 전과 같은 12시의 분포

smallasl <- subset.asl(ASL10, list(mode = "1", sex = "1")) ##남성중 도보수단의 
                                                           ##통행만 추출

testslice <- slice(smallasl, 1200, attr = c("mode", "sex", "stay"))

testslice
#area mode sex stay
#1    1101053   NA   1    1                  #이와같이 남성중 도보통행이
#2    1101053   NA   1    1                  #주로 나타나지만
#3    1101053    1   1    0                  #어쩔수 없이 slice()내의 함수로 
#4    1101053   NA   1    1                  #인하여 비는 부분을 stay하는 것으로
#6    1101053   NA   1    1                  #간주하는 것을 확인 할 수 있습니다.
#11   1101053   NA   1    1                  #이는 추후 시각화 과정에서
#12   1101053   NA   1    1                  #해당 부분이 NA인 항을 제외하고
#13   1101053   NA   1    1                  #시각화 하는 방식으로 수정이
#14   1101053   NA   1    1                  #가능할 것으로 생각합니다.
#15   1101053   NA   1    1                  #
#16   1101053   NA   1    1                  #
#17   1101053    1   1    0                  #
#18   1101053   NA   1    1
#19   1101053   NA   1    1
#21   1101053   NA   1    1
#22   1101053   NA   1    1
#23   1101053   NA   1    1
#24   1101053   NA   1    1
#25   1101053   NA   1    1
#26   1101053   NA   1    1
#27   1101053   NA   1    1
#28   1101053   NA   1    1
#29   1101053   NA   1    1
#31   1101053    1   1    0
#32   1101053    1   1    0
#33   1101053    1   1    0
#34   1101053    1   1    0


anotherasl <- subset.asl(ASL16, list(income = "3", sex = "1", haslic = TRUE))
testslice2 <- slice(anotherasl, 1200, attr = c("income", "sex", "haslic"))
testslice2

#area income sex haslic
#1   3109260      3   1   TRUE                 #다만 이와같이
#2   3114052      3   1   TRUE                 #개인의 속성만을 가지고
#3   3114051      3   1   TRUE                 #subset할 경우 제대로 작동하는
#4   3114052      3   1   TRUE                 #것을 확인할 수 있습니다
#5   3119134      3   1   TRUE                 
#6   3109268      3   1   TRUE                 
#8   3104256      3   1   TRUE                 
#11  3114052      3   1   TRUE
#12  3123054      3   1   TRUE
#13  3123054      3   1   TRUE
#21  3114052      3   1   TRUE
#22  3114052      3   1   TRUE
#23  3113056      3   1   TRUE
#24  3113056      3   1   TRUE
