# -- ПУНКТ 1 --
villa2 <- read_delim("Practice6/Task1/Task1/villa2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(villa2)

# -- ПУНКТ 2 --
villas_info_numerics <- subset(villa2, select = -c(Eco, N))
villas_info_numerics$Dist <- sub(',', '.', villas_info_numerics$Dist)
villas_info_numerics$Dist <- as.numeric(villas_info_numerics$Dist)
View(villas_info_numerics)
library(reshape)
meltData <- melt(as.data.frame(villas_info_numerics))
ggplot(meltData,aes(x = variable, y = value), fill = variable) + 
  geom_boxplot(aes(fill = variable)) + 
  facet_wrap( ~ variable, scales="free") +
  labs(title = "Ящичковые диаграммы для переменных", 
       y = "", 
       x = "") + 
  theme(legend.position = "none")

villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$Price != max(villas_info_numerics$Price))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$Price != max(villas_info_numerics$Price))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$Price != max(villas_info_numerics$Price))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$Price != max(villas_info_numerics$Price))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$Price != max(villas_info_numerics$Price))

villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$area != max(villas_info_numerics$area))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$area != max(villas_info_numerics$area))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$area != max(villas_info_numerics$area))

villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$house != max(villas_info_numerics$house))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$house != max(villas_info_numerics$house))
villas_info_numerics <- subset(villas_info_numerics, villas_info_numerics$house != max(villas_info_numerics$house))

View(villas_info_numerics)

# -- ПУНКТ 3 --

ggplot(data=villas_info_numerics, aes(x=Price, y=Dist)) +
  labs(title = "График зависимости переменных") + 
  geom_point()

ggplot(data=villas_info_numerics, aes(x=Price, y=house)) +
  labs(title = "График зависимости переменных") + 
  geom_point()

ggplot(data=villas_info_numerics, aes(x=Price, y=area)) +
  labs(title = "График зависимости переменных") + 
  geom_point()

ggplot(data=villas_info_numerics, aes(x=Dist, y=house)) +
  labs(title = "График зависимости переменных") + 
  geom_point()

ggplot(data=villas_info_numerics, aes(x=Dist, y=area)) +
  labs(title = "График зависимости переменных") + 
  geom_point()

ggplot(data=villas_info_numerics, aes(x=area, y=house)) +
  labs(title = "График зависимости переменных") + 
  geom_point()

# -- ПУНКТ №4 --

cor(villas_info_numerics)
corrplot(cor(villas_info_numerics), method="color", diag=FALSE)

# -- ПУНКТ №5 --
rcorr(as.matrix(villas_info_numerics)) 


# -- ПУНКТ №6 --
cov(villas_info_numerics)
pcor(c(1,2), cov(villas_info_numerics))
pcor(c(1,3), cov(villas_info_numerics))
pcor(c(1,4), cov(villas_info_numerics))

pcor(c(2,3), cov(villas_info_numerics))
pcor(c(2,4), cov(villas_info_numerics))

pcor(c(3,4), cov(villas_info_numerics))