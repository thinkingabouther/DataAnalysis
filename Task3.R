velocity <- readline("Введите скорость (в километрах в час): ")
velocity <- as.numeric(sub(",", ".", velocity))
print(paste("Скорость (в метрах в секунду): ", round(velocity / 3.6, 2)))
