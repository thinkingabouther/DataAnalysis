turnout <- c(100, 124, 121, 130, 150, 155, 144, 132, 189, 145, 125, 110, 118, 129, 127)
indexes <- which(turnout %% 5 == 0)
print("Индексы избирательных участков с подозрительной явкой:")
indexes
print(paste("Доля подозрительных участков: ", round(length(indexes)/length(turnout), 2)*100, "%"))