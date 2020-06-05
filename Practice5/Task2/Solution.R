# -- ЗАДАНИЕ 1 -- 
library(readxl)
villa <- read_excel("Practice5/Task2/villa.xls")
View(villa)

villas_info <- subset(villa, select=-c(N))
villas_info <- subset(villas_info, (Eco == 0 | Eco == 1))
villas_info$Eco <- as.factor(villas_info$Eco)

wilcox.test(Price ~ Eco, villas_info)

# -- ЗАДАНИЕ 2 -- 
psych_survey <- read.csv("Practice5/Task2/psych_survey.csv", 
                           sep = ";")
psych_survey_selected <- subset(psych_survey, select = c(height, subject))
psych_survey_selected$height <- sub(',', '.', psych_survey_selected$height)
psych_survey_selected$height <- as.numeric(psych_survey_selected$height)
psych_survey_selected$subject <- as.factor(psych_survey_selected$subject)

View(psych_survey_selected)

library(nortest)
psych_survey_selected <- subset(psych_survey_selected, subject != "NA")
current_vector <- psych_survey_selected$height

hist(current_vector, freq = FALSE, main = "Рост респондентов", xlab = "Рост", ylab = "Доля", col = "lightblue")
curve(dnorm(x, mean = mean(current_vector), sd = sd(current_vector)), 
      col = "red", lwd = 2, add = TRUE)
ks.test(current_vector, "pnorm", mean = mean(current_vector), sd = sd(current_vector))
shapiro.test(current_vector)
lillie.test(current_vector)
cvm.test(current_vector)
ad.test(current_vector)
sf.test(current_vector)
pearson.test(current_vector)

anova <- aov(height ~ subject, data = psych_survey_selected)
summary(anova)

