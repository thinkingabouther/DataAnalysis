# -- ПУНКТ 1 --
library(readxl)
villa <- read_excel("Practice5/Task1/villa.xls")
View(villa)

# -- ПУНКТ 2 --
str(villa)
# N - номер виллы в последовательности, число
# Price - цена виллы: число (тысяч Долларов), значения: 300 60 14 38 85 85 28 83 80 15 
# Dist - расстоние от кольцевой дороги: число (км), значения: 20 18 90 18 25 19 30 45 25 46 
# house: площадь дома: число (кв. км), значения: 400 170 60 65 320 210 60 228 200 36 
# area : площадь участка: сотки, значения: 22 6 11 6 20 20 5 20 20 10 
# Eco  : есть ли рядом с участком река, бинарная переменная (0 если нет)

# -- ПУНКТ 3 --
villas_info <- subset(villa, select=-c(N))
villas_info <- subset(villas_info, (Eco == 0 | Eco == 1))
villas_info$Eco <- as.factor(villas_info$Eco)
summary(villas_info)

#Price             Dist            house             area             Eco      
#Min.   :  5.00   Min.   :  0.50   Min.   : 22.00   Min.   : 5.000   Min.   :0.00  
#1st Qu.: 16.62   1st Qu.: 25.00   1st Qu.: 61.25   1st Qu.: 8.875   1st Qu.:0.00  
#Median : 46.00   Median : 30.00   Median :160.00   Median :14.000   Median :0.50  
#Mean   : 78.25   Mean   : 44.05   Mean   :192.24   Mean   :13.750   Mean   :0.52  
#3rd Qu.: 99.00   3rd Qu.: 63.75   3rd Qu.:300.00   3rd Qu.:15.000   3rd Qu.:1.00  
#Max.   :320.00   Max.   :105.00   Max.   :600.00   Max.   :40.000   Max.   :2.00

# -- ПУНКТ 4 --
villas_info_numerics <- subset(villas_info, select = -c(Eco))
View(villas_info_numerics)
library(reshape)
meltData <- melt(as.data.frame(villas_info_numerics))
meltData
ggplot(meltData,aes(x = variable, y = value), fill = variable) + 
  geom_boxplot(aes(fill = variable)) + 
  facet_wrap( ~ variable, scales="free") +
  labs(title = "Ящичковые диаграммы для переменных", 
       y = "", 
       x = "") + 
  theme(legend.position = "none")

# -- ПУНКТ 5 --
variation <- function(x) {paste(round(sd(x)/mean(x)*100, 2), "%")}
apply(villas_info_numerics, 2, variation)

# -- ПУНКТ 6 --
# - пунтк a -
current_vector <- villas_info_numerics$Price
hist(current_vector, freq = FALSE, main = "Стоимость вилл", xlab = "Стоимость", ylab = "Доля", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)

# - пунтк b -
library(moments)
print("Коэффициент эксцесса")
kurtosis(current_vector)-3
print("Коэффициент ассиметрии")
skewness(current_vector)

# - пунтк c -
qqnorm(current_vector)
qqline(current_vector)

# - пунтк d -
library(nortest)
ks.test(current_vector, "pnorm", mean = mean(current_vector), sd = sd(current_vector))
shapiro.test(current_vector)
current_vector <- villas_info_numerics$Price
lillie.test(current_vector)
cvm.test(current_vector)
ad.test(current_vector)
sf.test(current_vector)
pearson.test(current_vector)

