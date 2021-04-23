# --------
# Тихонова Алёна Серегеевна
# Введение в R
# Урок 3. Data envelopment analysis in R 
# --------

library(Benchmarking) #load the Benchmarking library for DEA


# --------
# Пример 1
# --------
# имееется 6 фирм-производителей 1 вход- 1 выход
x <- matrix(c(20, 40, 40, 60, 70, 50),ncol=1)  #определим input (зависимые переменные)
y <- matrix(c(20, 30, 50, 40, 60, 20),ncol=1)  # определим output в виде выпуска

plot(x,y) 

e_vrs <- dea(x,y, RTS="vrs", ORIENTATION="in") # решим задачу линейного программирования
eff(e_vrs) # посчитаем оценки эффективности  
summary(e_vrs)

# как мы видим из значений оценок эффективности и из графика,
# большинство фирм эффективны по VRS
# теперь определим эФФективность для модели Farrel
e_crs <- dea(x,y, RTS="crs", ORIENTATION="in")
eff(e_crs)
# как видно из оценок, только C полностью эффективна по CRS
dea.plot.frontier(x,y) # граница эффективности

# то же проделаем для остальных типов моделей
e_fdh <- dea(x,y, RTS="fdh", ORIENTATION="in")
eff(e_fdh)

e_drs <- dea(x,y, RTS="drs", ORIENTATION="in")
eff(e_drs)

e_irs <- dea(x,y, RTS="irs", ORIENTATION="in")
eff(e_irs)

# соседи и лямбда-функция
e_vrs <- dea(x,y, RTS="vrs", ORIENTATION="in", NAMES=TRUE)
peers(e_vrs) #только два соседа, не более
lambda(e_vrs)

# --------
# анализ эффективности весов
# --------
e_vrs <- dea(x,y,RTS='vrs')
e_drs <- dea(x,y,RTS='drs')
e_crs <- dea(x,y,RTS='crs')
se <- eff(e_crs)/eff(e_vrs)
se
abs(eff(e_vrs)- eff(e_drs)) < 1e-4  #test if DRS eff = VRS eff
# Мы видим, что фирмы A и B находятся внизу, фирма C оптимальна, 
# фирма D внизу, фирма E выше
# фирма F ниже оптимального размера шкалы


# --------
# Пример 2 Эффективность размещения. Минимизация затрат 
# --------

x <- matrix(c(2, 2, 5, 10, 10, 3, 12, 8, 5, 4, 6,12),ncol=2)
y <- matrix(rep(1,6), ncol=1)
w <- matrix(c(1.5, 1), ncol=2)
x
y
w
 

te <- dea(x,y,RTS="vrs") # техническая эффективность
te

# Оценим вектор(ы) ввода и/или вывода, 
# которые минимизируют затраты, 
# максимизируют доход или максимизируют прибыль 
# в контексте технологии DEA
xopt <- cost.opt(x,y, w, RTS="vrs")
xopt

# расчет рентабельности
cobs <- x %*% t(w) # t(w) -транспонирование весов
copt <- xopt$xopt %*% t(w)
ce <- copt/cobs
ae <- ce/te$eff
cbind(ce,te$eff,ae) # cbind в данном случае - комбинация векторов

# --------
# Пример 3 Сверхэффективность (Super-efficiency) 
# --------

# Input super efficiency, vrs and crs
sdea(x,y, RTS="vrs", ORIENTATION="in")

sdea(x,y, RTS="crs", ORIENTATION="in")

# Output super efficiency, vrs and crs
sdea(x,y,RTS="vrs",ORIENTATION="out")

sdea(x,y,RTS="crs",ORIENTATION="out")

# --------
# Пример 4 Графическая эффективность 
# --------

x <- matrix(c(20, 40, 40, 60, 70, 50),ncol=1)
y <- matrix(c(20, 30, 50, 40, 60, 20),ncol=1)
dea.plot(x,y)
s_vrs <- dea(x,y,RTS="vrs",ORIENTATION="graph")
s_crs <- dea(x,y,RTS="crs",ORIENTATION="graph")
s_vrs
s_crs

# --------
# Пример 5 модель два входа - один выход
# --------

x <- matrix(c(2,1,2,3,3,4,5,2,2,2,1,1), ncol=2)
y <- matrix(c(1,1,1,1,1,1), ncol=1)
x
y
e_vrs <- dea(x,y,RTS="vrs",ORIENTATION="in",SLACK=TRUE)
e_vrs$eff
[1] 0.5000 1.0000 0.8333 0.7143 1.0000 1.0000
e_vrs$slack
[1] TRUE FALSE FALSE FALSE FALSE TRUE
e_vrs$sx

#   sx1 sx2
[1,] 0 0.5
[2,] 0 0.0
[3,] 0 0.0
[4,] 0 0.0
[5,] 0 0.0
[6,] 1 0.0
> e_vrs$sy
sy1
[1,] 0
[2,] 0
[3,] 0
[4,] 0
[5,] 0
[6,] 0


