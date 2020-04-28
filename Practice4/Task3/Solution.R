# -- ПУНКТ 1 --
df <- read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
summary(df)
#PassengerId       Survived          Pclass                                         Name         Sex     
#Min.   :  1.0   Min.   :0.0000   Min.   :1.000   Abbing, Mr. Anthony                  :  1   female:314  
#1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000   Abbott, Mr. Rossmore Edward          :  1   male  :577  
#Median :446.0   Median :0.0000   Median :3.000   Abbott, Mrs. Stanton (Rosa Hunt)     :  1               
#Mean   :446.0   Mean   :0.3838   Mean   :2.309   Abelson, Mr. Samuel                  :  1               
#3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000   Abelson, Mrs. Samuel (Hannah Wizosky):  1               
#Max.   :891.0   Max.   :1.0000   Max.   :3.000   Adahl, Mr. Mauritz Nils Martin       :  1               
#(Other)                              :885               
#Age            SibSp           Parch             Ticket         Fare                Cabin     Embarked
#Min.   : 0.42   Min.   :0.000   Min.   :0.0000   1601    :  7   Min.   :  0.00              :687    :  2   
#1st Qu.:20.12   1st Qu.:0.000   1st Qu.:0.0000   347082  :  7   1st Qu.:  7.91   B96 B98    :  4   C:168   
#Median :28.00   Median :0.000   Median :0.0000   CA. 2343:  7   Median : 14.45   C23 C25 C27:  4   Q: 77   
#Mean   :29.70   Mean   :0.523   Mean   :0.3816   3101295 :  6   Mean   : 32.20   G6         :  4   S:644   
#3rd Qu.:38.00   3rd Qu.:1.000   3rd Qu.:0.0000   347088  :  6   3rd Qu.: 31.00   C22 C26    :  3           
#Max.   :80.00   Max.   :8.000   Max.   :6.0000   CA 2144 :  6   Max.   :512.33   D          :  3           
#NA's   :177                                      (Other) :852                    (Other)    :186  

# Качественнная переменная - Sex (Пол): женщин на корабле - 314, мужчин на корабле - 577

# Количественная переменная - Fare (Тариц): минимальная стоимость билета - 0 у.е, максимальная - 512.33 у.е
# Медиана (14.45 у.е.) ощутимо ниже среднего значения (32.20 у.е.), что свидетельствует о большом количестве выбросов сверху

# -- ПУНКТ 2 --
ggplot(df,aes(x=Age)) +
  geom_histogram(binwidth=1, color = "black", fill = "red") + 
  scale_x_continuous(breaks = seq(0, 90, 1)) +
  labs(title = "Возраст пассажиров корабля", 
       y = "Количество", 
       x = "Возраст")
# Наиболее часто встречающийся возраст - 24,
# Наименее часто встречающийся возраст (не считая отсутствующих) - 0, 12, 53, 66, 74, 80

# -- ПУНКТ 3 --
ggplot(df, aes(x=Age)) + geom_boxplot(width=0.1)
# Выбросов - 7

# -- ПУНКТ 4 --
female_survived <-nrow(filter(df, df$Sex == "female" & df$Survived == 1))
survived <- nrow(filter(df, df$Survived == 1))
survived
female_survived
BinomCI(female_survived, survived, conf.level = 0.95)
male_survived <- survived - female_survived
BinomCI(male_survived, survived, conf.level = 0.95)
# С вероятностью 95% доля женщин, выживающих при кораблекрушениях, расположится в промежутке от 63% до 73%
# С вероятностью 95% доля мужчин, выживающих при кораблекрушениях, расположится в промежутке от 27% до 37%

