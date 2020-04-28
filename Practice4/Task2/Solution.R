# -- ПУНКТ 1 --
df <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv", encoding = "UTF-8")
View(df)
# -- ПУНКТ 2 --

young_share <- round(df$young_total / df$popul_total * 100, 2)
df <- df %>% mutate(young_share = young_share)

trud_share <- round(df$wa_total/df$popul_total*100, 2)
df <- df %>% mutate(trud_share = trud_share)

old_share = round(df$ret_total/df$popul_total*100, 2)
df <- df %>% mutate(old_share = old_share)

View(df)

# -- ПУНКТ 3 --
ggplot(data = df, aes(x = trud_share)) +
  geom_histogram(color = "black", fill = "red") +
  labs(title = "Доля трудоспособного населения в процентах", y = "Количество", x = "Доля") +
  geom_vline(xintercept = median(df$trud_share), color = "blue") +
  geom_rug()

# -- ПУНКТ 4 --
ggplot(data = df, aes(x = trud_share, group = region, color = region)) + 
  geom_density() +
  labs(title = "Доля трудоспособного населения по регионам", 
       y = "Вероятность", 
       x = "Доля") +
  theme(legend.title = element_blank())

ggplot(data = df, aes(x = region, y = trud_share, group = region, color = region)) + 
  geom_violin(trim = FALSE, aes(fill = region)) + geom_boxplot(color = "black", width = 0.1) + 
  labs(title = "Доля трудоспособного населения по регионам", 
       y = "Доля", 
       x = "Регион") +
  theme(legend.title = element_blank())

# -- ПУНКТ 5 --

ggplot(df, aes(x = young_share, y = old_share)) +
  geom_jitter(color = "blue", shape = "triangle") +
  labs(title = "Рассеивание для доли молодого и пожилого населения", 
     y = "Доля пожилого населения", 
     x = "Доля молодого населения") +
  theme(legend.title = element_blank())  

# -- ПУНКТ 6 --
male_share <- (df$wa_male + df$ret_male + df$young_male)/df$popul_total*100
df <- df %>% mutate(male_share = male_share)
df <- df %>% mutate(male_region = male_share > 50)
View(df)

# -- ПУНКТ 7 --
ggplot(df, aes(young_share, old_share, size = male_share, color = male_region)) + 
  geom_count() + 
  labs(title = "Доля пожилого и молодого населения для городов с учетом пола", 
       y = "Доля пожилого населения", 
       x = "Доля молодого населения", 
       fill = "123",
       size = "Доля мужчин",
       color = "Мужчин больше?") 

# -- ПУНКТ 8 --

ggplot(df, aes(x=factor(region), fill=factor(region))) + 
  geom_bar() + 
  labs(title = "Доля городов из областей", 
       y = "Количество", 
       x = "Область") +
  theme(legend.title = element_blank())  


