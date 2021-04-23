# --------
# Тихонова Алёна Серегеевна
# Введение в R
# Урок 2. Работа с данными. 
# --------

# по умолчанию функционал чтения Excel-файлов не установлен. 
# необходимо для начала установить библиотеку
# c расширением .csv таких проблем нет

# --------
# чтение файлов
library(readxl)  #установка библиотеки 
mroz_student <- read_excel("PHD/R intro course/mroz_student.xlsx")
View(mroz_student) #просмотр таблицы
# то же действие можно выполнить через меню import Dataset

fob_price <- read.csv("PHD/R intro course/WTI_Spot_Price_FOB.csv")
View(fob_price) 
fob_price1 <- read.csv("PHD/R intro course/WTI_Spot_Price_FOB.csv", sep = ",")
View(fob_price1)


# --------
#загрузка пакетов для первичного анализа данных

library(psych) # for basic statictics
library(pillar) # для glimpse (возможно, есть выше)
glimpse(mroz_student) 
?glimpse

summary(mroz_student) #основные описательные статистики
describe(mroz_student) #основные описательные статистики более развернуто
head(mroz_student) #первые 6 наблюдений
tail(mroz_student) #для проверки корректности переноса данных - конец таблицы

ncol(mroz_student)
nrow(mroz_student)
str(mroz_student)
class(mroz_student)

mean(mroz_student$AGE) #обращение к столбцу, применение функции к вектору

# --------
# Графики
# шпаргалки https://aakinshin.net/ru/posts/r-functions/#section-math

fob_price1 <- read.csv("PHD/R intro course/WTI_Spot_Price_FOB for line plot.csv", sep = ",", header=FALSE)
View(fob_price1)
plot(fob_price1$V2)
plot(fob_price1$V2,fob_price1$V1)

# --------
# линейная регрессия
hist(mroz_student$AGE) #линейная модель

model_lm <- lm(data=fob_price1, V1~V2)
?lm
summary(model_lm)
print(model_lm)
