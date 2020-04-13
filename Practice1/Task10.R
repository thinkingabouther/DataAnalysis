income <- c(10000, 32000, 28000, 150000, 65000, 1573)
average <- sum(income) / length(income)
income_class <- replace(income, income < average, 0)
income_class <- replace(income_class, income_class >= average, 1)
income_class
