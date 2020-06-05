# -- ПУНКТ 1 --
AppleStore <- read_excel("Practice4/Task1/AppleStore.xlsx")
View(AppleStore)

# -- ПУНКТ 2 --
AppleStoreDF <- select(AppleStore, -c(id, currency))
View(AppleStoreDF)

# -- ПУНКТ 3 --
str(AppleStoreDF)
# Единица наблюдение - приложение, доступное в магазине AppStore
# Переменных - 7
# Количество наблюдений - 7187 
#name            : строки, значения: "Bible" "Candy Crush Saga" "Spotify Music" "Angry Birds"
#size_bytes      : числа, значения: 9.28e+07 2.23e+08 1.33e+08 1.76e+08 1.56e+08 
#price           : числа, значения: 0 0 0 0 0 1.99 0 0 0 0 
#rating_count_tot: числа, значения: 85920 961794 878563 824451 706110 
#user_rating     : числа, значения: 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 4.5 
#prime_genre     : строки, значения: "Reference" "Games" "Music" "Games" 
#lang_num        : числа, значения: 45 24 18 10 1 13 11 10 13 13 

# -- ПУНКТ 4 --
summary(AppleStoreDF[, c("price", "user_rating", "lang_num", "size_bytes")])
#price         user_rating       lang_num        size_bytes       
#Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :5.898e+05  
#1st Qu.: 0.000   1st Qu.:3.500   1st Qu.: 1.000   1st Qu.:4.687e+07  
#Median : 0.000   Median :4.000   Median : 1.000   Median :9.704e+07  
#Mean   : 1.648   Mean   :3.526   Mean   : 5.424   Mean   :1.989e+08  
#3rd Qu.: 1.990   3rd Qu.:4.500   3rd Qu.: 8.000   3rd Qu.:1.817e+08  
#Max.   :99.990   Max.   :5.000   Max.   :75.000   Max.   :4.026e+09  

# -- ПУНКТ 5 --
AppleStoreDF[which.max(AppleStoreDF$lang_num), "name"]
# Google Photos - unlimited photo and video storage

# -- ПУНКТ 6 --
apply(select(AppleStoreDF, price, user_rating, lang_num), 2, quantile)
#      price    user_rating lang_num
#0%    0.00         0.0        0
#25%   0.00         3.5        1
#50%   0.00         4.0        1
#75%   1.99         4.5        8
#100% 99.99         5.0       75


# -- ПУНКТ 7 -- 
variation <- function(x) {paste(round(sd(x)/mean(x)*100, 2), "%")}
only_numerics <- select(AppleStoreDF, -c(name, prime_genre))
print("KURTOSIS")
apply(only_numerics, 2, kurtosis)
print("SKEWNESS")
apply(only_numerics, 2, skewness)
print("VARIATION")
apply(only_numerics, 2, variation)

# -- ПУНКТ 8 --

boxplot(only_numerics$size_bytes, 
        main = "Размер приложения в байтах", 
        ylab = "Байт")
boxplot(only_numerics$price, 
        main = "Цена приложения", 
        ylab = "Долларов")
boxplot(only_numerics$rating_count_tot, 
        main = "Количество поставленных оценок", 
        ylab = "Оценок")
boxplot(only_numerics$user_rating, 
        main = "Итоговая оценка", 
        ylab = "Баллов")
boxplot(only_numerics$lang_num, 
        main = "Количество поддерживаемых языков", 
        ylab = "Языков")
apply(only_numerics, 2, boxplot)

# -- ПУНКТ 9 --
pie(table(AppleStoreDF$prime_genre), main = "Распределение приложений по жанрам", xlab = "Жанры", radius = 1)

# -- ПУНКТ 10 --

current_vector <- AppleStoreDF$size_bytes
hist(current_vector, freq = FALSE, main = "Размер приложения в байтах", xlab = "Байт", ylab = "Приложений", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)

current_vector <- AppleStoreDF$price
hist(current_vector, freq = FALSE, main = "Цена приложения", xlab = "Долларов", ylab = "Приложений", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)

current_vector <- AppleStoreDF$rating_count_tot
hist(current_vector, freq = FALSE, main = "Количество поставленных оценок", xlab = "Оценок", ylab = "Приложений", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)

current_vector <- AppleStoreDF$user_rating
hist(current_vector, freq = FALSE, main = "Итоговая оценка", xlab = "Баллов", ylab = "Приложений", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)

current_vector <- AppleStoreDF$lang_num
hist(current_vector, freq = FALSE, main = "Количество поддерживаемых языков", xlab = "Языков", ylab = "Приложений", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)

# -- ПУНКТ 11 --
factor_genres <- as.factor(AppleStoreDF$prime_genre)
summary(factor_genres)
# Наиболее распространенный жанр - Games (3859)

# -- ПУНКТ 12 -- 
Games <- filter(AppleStoreDF, AppleStoreDF$prime_genre == "Games")
View(Games)
summary(Games[, c("price", "user_rating", "lang_num", "size_bytes")])
#price         user_rating       lang_num        size_bytes       
#Min.   : 0.000   Min.   :0.000   Min.   : 0.000   Min.   :1.126e+06  
#1st Qu.: 0.000   1st Qu.:3.500   1st Qu.: 1.000   1st Qu.:7.102e+07  
#Median : 0.000   Median :4.500   Median : 1.000   Median :1.336e+08  
#Mean   : 1.428   Mean   :3.685   Mean   : 4.586   Mean   :2.834e+08  
#3rd Qu.: 1.990   3rd Qu.:4.500   3rd Qu.: 7.000   3rd Qu.:2.564e+08  
#Max.   :29.990   Max.   :5.000   Max.   :46.000   Max.   :4.026e+09  

# Средняя цена увеличилась, но максимальное значение цены уменьшилось
# Медианный рейтинг увеличился, но средняя цена практически не изменилась, что может говорить об определенном количестве крайне низких оценок
# Количество поддерживаемых языков в среднем незначительно уменьшилось
# Все показатели, указывающие на размер, увеличились, что может говорить о том, что в целом игры занимают больше пространства на телефоне

# -- ПУНКТ 13 -- 
ks <- function(prices){
  ks.test(prices, 'pnorm', mean(prices), sd(prices))
}
Not_games <- filter(AppleStoreDF, AppleStoreDF$prime_genre != "Games")

ks(Games$price)
ks(Not_games$price)

shapiro.test(Games$price)
shapiro.test(Not_games$price)


