
df <- read.csv("https://raw.githubusercontent.com/agconti/kaggle-titanic/master/data/train.csv")
View(df)
# ----- ЗАДАНИЕ 1 -----

# ---- ПУНКТ 1 ----

str(df) 
# Количество наблюдений - 891, количество переменных - 12, 
# PassengerId: целое число  значения:  1 2 3 4 5 6 7 8 9 10 
# Survived   : целое число, значения:  0 1 1 1 0 0 0 0 1 1 
# Pclass     : целое число  значения:  3 1 3 1 3 3 1 3 3 2 
# Name       : Факторная переменнная с 891 уровнями, значения:  "Abbing, Mr. Anthony"
# Sex        : Факторная переменная с 2 уровнями: "female","male"
# Age        : число, значения:  22 38 26 35 35 NA 54 2 27 14 
# SibSp      : целое число, значения:  1 1 0 1 0 0 0 3 0 1 
# Parch      : целое число, значения:   0 0 0 0 0 0 0 1 2 0 
# Ticket     : Факторная переменнная с 681 уровнем, значения: "110152","110413"
# Fare       : Число, значения:  7.25 71.28 7.92 53.1 8.05 
# Cabin      : Факторная переменнная с 148 уровнями, значения: "","A10","A14"
# Embarked   : Факторная переменнная с 4 уровнями, значения: "","C","Q","S"

# ---- ПУНКТ 2 ----

nrow(df[complete.cases(df), ]) # количество полность заполненных рядов - 714
df_na <- df[!complete.cases(df), ]
View(df_na)

# ---- ПУНКТ 3 ----

aggr(df)
# значения отсутствуют только в графе Возраст

# ---- ПУНКТ 4 ----

# отсутствие данных о возрасте пассажиров может быть связано с невозможностью восстановить паспорт пассажира 
# или несоверешенной системой продажи билетов, что вполне ожидаемо, учитывая время составления данного набора данных

# ---- ПУНКТ 5 ----

df <- na.omit(df)
View(df)

# ----- ЗАДАНИЕ 2 -----

# ---- ПУНКТ 1 ----
temp <- df$Sex
temp <- as.character(temp)
temp[temp == "female"] <- 1
temp[temp == "male"] <- 0
length(temp)
temp <- as.integer(temp)
temp
df$female <- temp
View(df)

# ---- ПУНКТ 2 ----
df2 <- subset(df, df$Age > 25 & df$Age <= 45 & (df$Pclass == 2 | df$Pclass == 3))
View(df2)

# ---- ПУНКТ 3 ----
print(paste("Женщин на корабле: ", nrow(subset(df, as.character(df$Sex) == "female"))))
print(paste("Мужчин на корабле: ", nrow(subset(df, as.character(df$Sex) == "male"))))
            
# ---- ПУНКТ 4 ----

survivors <- subset(df, df$Survived == 1)
print(paste("Самый молодой выживший в возрастe: ", min(as.numeric(survivors$Age))))
print(paste("Самый старый выживший в возрастe: ", max(as.numeric(survivors$Age))))
survivors_firstclass <- subset(survivors, survivors$Pclass == 1)
print(paste("Средний возраст выживших в первом классе: ", round(mean(as.numeric(survivors_firstclass$Age)), 2)))


