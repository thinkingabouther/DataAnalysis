a = readline("Введите первое число: ")
b = readline("Введите второе число: ")
a <- sub(",", ".", a)
b <- sub(",", ".", b)
a <- as.numeric(a)
b <- as.numeric(b)
a + b
print(paste("Сумма чисел: ", a + b))
