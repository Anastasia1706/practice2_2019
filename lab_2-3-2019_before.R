# Esly russkie bukvy ne otobrajautsa: File -> Reopen with encoding... UTF-8

# Используйте UTF-8 как кодировку по умолчанию!
# Установить кодировку в RStudio: Tools -> Global Options -> General, 
#  Default text encoding: UTF-8

# Аналитический пакет R: Занятие 3

# Часть 1: Графические системы R -----------------------------------------------

# загрузка пакетов
library('data.table')          # работаем с объектами "таблица данных"
library('moments')             # коэффициенты асимметрии и эксцесса 
library('lattice')
library('ggplot2')

# Пакет "base" =================================================================

# Пример 1 #####################################################################

# встроенный набор: загрязнённость воздуха в Нью-Ньорке
# справка
?airquality
# копируем в таблицу данных
DT.air <- data.table(airquality)

# простой график разброса
plot(x = DT.air$Ozone, y = DT.air$Wind)              # Рис. 1 

# сколько месяцев в данных?
unique(DT.air$Month)

# цвет по месяцам для легенды графика
mnth.f <- as.factor(unique(DT.air$Month))
# берём палитру на 5 цветов
cls <- palette(rainbow(5))
# результат
cls

# ГРАФИК РАЗБРОСА
# создаём график без осей
plot(x = DT.air$Ozone, 
     y = DT.air$Wind, 
     pch = 21, 
     bg = cls[as.factor(DT.air$Month)],
     axes = F, 
     ylim = c(0, 25), 
     xlim = c(0, 180), 
     xlab = 'Озон в воздухе, частиц на миллиард', 
     ylab = 'Сила ветра, м/час')

# горизонтальная ось 
axis(side = 1, 
     pos = 0, 
     at = seq(0, 150, by = 50),
     labels = seq(0, 150, by = 50))
# вертикальная ось
axis(side = 2, 
     pos = 0, 
     at = seq(0, 20, by = 5),
     labels = c('', seq(5, 20, by = 5)),
     las = 2)
# стрелки на концах осей
arrows(x0 = c(0, 150), y0 = c(20, 0),
       x1 = c(0, 180), y1 = c(25, 0))

# легенда
legend('topright', legend = mnth.f, fill = cls[mnth.f])
# заголовок
mtext(text = 'Разброс значений по месяцам', 
      side = 3, line = 2, font = 2)
mtext(text = '(в легенде указан номер месяца)', 
      side = 3, line = 1, cex = 0.8, font = 3)

# КОРОБЧАТАЯ ДИАГРАММА
# ящики с усами по месяцам
boxplot(DT.air$Ozone ~ as.factor(DT.air$Month),
        xlab = 'Номер месяца', 
        ylab = 'Озон в воздухе, частиц на миллиард')

# ГРАФИК ПЛОТНОСТИ РАСПРЕДЕЛЕНИЯ
d <- density(DT.air$Ozone, na.rm = T)   
plot(d,
     main = 'Плотность распределения показателя \n "Содержание озона в воздухе"',
     ylab = 'Плотность')
# заливка
polygon(d,
        col = rgb(1, 0, 0, alpha = 0.4), 
        border = 'red')

# удаляем временные объекты
rm(cls, d, mnth.f)


# Пакет "lattice" ==============================================================


# Продолжение примера 1 ########################################################

# ГРАФИКИ ПЛОТНОСТИ РАСПРЕДЕЛЕНИЯ ПО МЕСЯЦАМ
densityplot( ~ Ozone | as.factor(Month), data = DT.air,
             main = 'Распределение содержания озона в воздухе по месяцам',
             xlab = 'Озон в воздухе, частиц на миллиард',
             ylab = 'Плотность распределения')


# Пример 2 #####################################################################

# встроенный набор данных: характеристики автомобилей
# справка
?mtcars
# сохраняем данные в таблицу
DT.cars <- data.table(mtcars)

# ГРАФИК РАЗБРОСА
xyplot(qsec ~ mpg, data = DT.cars)                  

# ПОКАЖЕМ ТРЕТЬЮ ПЕРЕМЕННУЮ ЦВЕТОМ (НЕПРЕРЫВНАЯ)
# палитра с градиентом
rbPal <- colorRampPalette(c('cyan', 'navyblue'))
# переводим непрерывную переменную hp в 10 интервалов
hp.cut <- cut(DT.cars$hp, breaks = 10) 
# уникальные и отсортированные интервалы для легенды
hp.intervals <- sort(unique(cut(DT.cars$hp, breaks = 10)))
# создаём вектор цветов для наблюдений
cls <- rbPal(10)[as.numeric(hp.cut)] 

# строим график и задаём легенду вручную
xyplot(qsec ~ mpg,
       data = DT.cars, 
       key = list(text = list(lab = as.character(hp.intervals)),
                  points = list(pch = 21, 
                                col = rbPal(10)[hp.intervals],
                                fill = rbPal(10)[hp.intervals]),
                  columns = 3, 
                  title = 'Интервалы мощности, л.с.', 
                  cex.title = 1),
       fill.color = cls ,
       xlab = 'Миль на галлон топлива',
       ylab = 'Время, за которое проходит 1/4 мили, секунд',
       panel = function(x, y, fill.color,..., subscripts) {
           # задать свой цвет каждому наблюдению
           fill = fill.color [subscripts] 
           # изобразить точки разными цветами
           panel.xyplot(x, y, pch = 19, 
           col=fill)
           }
       )                               

# ПОКАЖЕМ ТРЕТЬЮ ПЕРЕМЕННУЮ ЦВЕТОМ (ФАКТОР)
# новая переменная-фактор: количество цилиндров
DT.cars[, Число.цилиндров := factor(cyl, levels = c(4, 6, 8), 
                                  labels = c('4 цилиндра', 
                                             '6 цилиндров',
                                             '8 цилиндров'))]

# группы обозначены цветом: дискретный показатель. 
xyplot(qsec ~ mpg,
       data = DT.cars, 
       groups = Число.цилиндров,
       auto.key = T, 
       ylab = 'Время, за которое проходит 1/4 мили, секунд',
       xlab = 'Миль на галлон топлива')

# РАЗБРОС С РАЗБИВКОЙ ПО ГРУППАМ
# группы вынесены на отдельные панели графика
xyplot(qsec ~ mpg|Число.цилиндров,
       data = DT.cars,
       ylab = 'Время, за которое проходит 1/4 мили, секунд',
       xlab = 'Миль на галлон топлива')

# то же самое со средними по вертикальной оси (медианы)
xyplot(qsec ~ mpg | Число.цилиндров,
       data = DT.cars, 
       xlab = 'Миль на галлон топлива',
       ylab = 'Время, за которое проходит 1/4 мили, секунд',
       panel = function(x, y, ...) {
           # вызов функции по умолчанию (график разброса)
           panel.xyplot(x, y, ...) 
           # рисуем горизонтальные прямые
           panel.abline(h = median(y), lty = 2)   
       })

# переменная-фактор: тип коробки передач
DT.cars[, Коробка.передач := factor(am, levels = c(0, 1), 
                                    labels = c('автоматическая',
                                               'ручная'))]

# графики разброса с линиями регрессий
xyplot(qsec ~ mpg | Коробка.передач,
       data = DT.cars, 
       ylab = 'Время, за которое проходит 1/4 мили, секунд',
       xlab = 'Миль на галлон топлива',
       main = 'Характеристики в зависимости от типа коробки передач',
       panel = function(x, y, ...) {
           # вызов функции по умолчанию (график разброса)
           panel.xyplot(x, y, ...) 
           # затем накладываем линии регрессии
           panel.lmline(x, y, col = 'red') 
       })

# ГИСТОГРАММЫ
# гистограммы с разбиением по двум категориальным переменным
histogram(  ~ mpg | Число.цилиндров * Коробка.передач,
          data = DT.cars)


# Пакет "ggplot2" ==============================================================


# Пример 3 #####################################################################

# встроенный набор данных: экономия топлива 38 моделями 
#  автомобилей (1999 и 2008)
# справка
?mpg
# сохраняем данные в таблицу
DT.mpg <- data.table(mpg)

# РАЗБРОС
# простой график разброса
qplot(displ, hwy, data = DT.mpg) 

# ДОБАВЛЯЕМ ЦВЕТ
# переменная-фактор: привод автомобиля
DT.mpg[, Тип.привода := factor(drv, levels = c('f', 'r', '4'),
                               labels = c('передний', 'задний',
                                          'полный'))]

# обозначаем цветом привод
qplot(x = displ ,
      y = hwy, 
      data = DT.mpg, 
      color = Тип.привода,
      xlab = 'Объём двигателя, литров',
      ylab = 'Миль на галлон')

# РАЗБРОС СО СГЛАЖИВАНИЕМ
# простая функция qplot()
qplot(x = displ, 
      y = hwy, 
      data = DT.mpg,
      xlab = 'Объём двигателя, литров',
      ylab = 'Миль на галлон', 
      geom = c('point', 'smooth')
      )

# использование явной грамматики, функция ggplot()
# начинаем строить ggplot с объявления исходных данных
gp <- ggplot(data = DT.mpg, aes(x = displ, y = hwy)) 
# объясняем, как изображать данные: график разброса
gp <- gp + geom_point()  
# добавляем фасетки для разных типов привода
gp <- gp + facet_grid(. ~ Тип.привода) 
# добавляем линии регрессии
gp <- gp + geom_smooth(method = 'lm') 
# добавляем подписи осей и заголовок
gp <- gp + xlab('Объём двигателя, литров') 
gp <- gp + ylab('Миль на галлон')
gp <- gp + ggtitle('Зависимость расхода топлива от объёма двигателя')
# выводим график
gp

# КОРОБКИ ПО ГРУППАМ (ЦВЕТ + ОСЬ)
# всё, что зависит от значений данных, заносим в аргумент aes
gp <- ggplot(data = DT.mpg, 
             aes(x = as.factor(year), 
                 y = displ, color = Тип.привода
                 ))
gp <- gp + geom_boxplot() 
gp <- gp + xlab('год')
gp <- gp + ylab('Миль на галлон')
gp


# Часть 2: Заполнение пропусков в данных ---------------------------------------

# загружаем файл с данными по импорту масла в РФ (из прошлой практики)
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# создаём директорию для данных, если она ещё не существует:
if (!file.exists('./data')) {
    dir.create('./data')
}
# создаём файл с логом загрузок, если он ещё не существует:
if (!file.exists('./data/download.log')) {
    file.create('./data/download.log')
}
# загружаем файл, если он ещё не существует,
#  и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
    download.file(fileURL, './data/040510-Imp-RF-comtrade.csv')
    # сделать запись в лог
    write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен', Sys.time()), 
          file = './data/download.log', append = T)
}
# читаем данные из загруженного .csv во фрейм, если он ещё не существует
if (!exists('DT')){
    DT.import <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', 
                                     stringsAsFactors = F))
}
# предварительный просмотр
dim(DT.import)            # размерность таблицы
str(DT.import)            # структура (характеристики столбцов)
DT.import          # удобный просмотр объекта data.table


# Заполнение пропусков средними значениями =====================================


# Пример 4.1 ###################################################################

# сколько NA в каждом из оставшихся столбцов?
na.num <- apply(DT.import, 2, function(x) length(which(is.na(x)))) 
# выводим только положительные и по убыванию
sort(na.num[na.num > 0], decreasing = T)

# графики плотности распределения массы поставки по годам
png('Pic-01.png', width = 500, height = 500)
densityplot(~ Netweight.kg | as.factor(Year), data = DT.import,
            ylim = c(-0.5e-05, 8.5e-05),
            main = 'Распределение массы поставки по годам, Netweight.kg',
            xlab = 'Масса поставки, кг',
            ylab = 'Плотность распределения')
dev.off()

# явное преобразование типа, чтобы избежать проблем 
#  при заполнении пропусков
DT.import[, Netweight.kg :=as.double(Netweight.kg) ]
# считаем медианы и округляем до целого, как исходные данные
DT.import[, round(median(.SD$Netweight.kg, na.rm = T), 0),
          by = Year] 


# сначала копируем все значения
DT.import[, Netweight.kg.median := round(median(.SD$Netweight.kg,
                                                na.rm = T), 0),
          by = Year] 

# затем заменяем пропуски на медианы
DT.import[!is.na(Netweight.kg), Netweight.kg.median := Netweight.kg] 

# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.median]
DT.import[is.na(Netweight.kg), Year, Netweight.kg.median]

# смотрим, что изменилось
png('Pic-02.png', width = 500, height = 500)
densityplot( ~ Netweight.kg.median | as.factor(Year),
            data = DT.import,
            ylim = c(-0.5e-05, 8.5e-05),
            main = 'Распределение массы поставки по годам, Netweight.kg.median',
            xlab = 'Масса поставки, кг',
            ylab = 'Плотность распределения')
dev.off()

# если то же самое сделать по среднему арифметическому
# считаем средние и округляем до целого, как исходные данные
DT.import[, round(mean(.SD$Netweight.kg, na.rm = T), 0), by = Year]

# заменяем пропуски на средние
DT.import[, Netweight.kg.mean := round(mean(.SD$Netweight.kg,
                                            na.rm = T), 0), by = Year]
DT.import[!is.na(Netweight.kg), Netweight.kg.mean := Netweight.kg]

# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.mean]
DT.import[is.na(Netweight.kg), Year, Netweight.kg.mean]

# смотрим, что изменилось
png('Pic-03.png', width = 500, height = 500)
densityplot( ~ Netweight.kg.mean | as.factor(Year), 
            data = DT.import,
            ylim = c(-0.5e-05, 8.5e-05),
            main = 'Распределение массы поставки по годам, Netweight.kg.mean',
            xlab = 'Масса поставки, кг',
            ylab = 'Плотность распределения')
dev.off()

# скошенность и эксцесс для переменных в целом
# ненормальность распределений закономерно усиливается
df.stats <- 
    data.frame(var = c('Netweight.kg', 'Netweight.kg.median', 
                       'Netweight.kg.mean'),
               skew = round(c(skewness(na.omit(DT.import$Netweight.kg)), 
                              skewness(DT.import$Netweight.kg.median),
                              skewness(DT.import$Netweight.kg.mean)), 2),
               kurt = round(c(kurtosis(na.omit(DT.import$Netweight.kg)), 
                              kurtosis(DT.import$Netweight.kg.median),
                              kurtosis(DT.import$Netweight.kg.mean)), 2),
               stringsAsFactors = F)
df.stats


# Заполнение пропусков с помощью модели регрессии ==============================


# Пример 4.2 ###################################################################

# переменные: масса поставки и её стоимость
x <- DT.import$Trade.Value.USD 
y <- DT.import$Netweight.kg  
y[y == 0] <- NA

# оценка регрессии с помощью МНК
fit <- lm(y~x)
# результаты
summary(fit)
# сохраняем R-квадрат
R.sq <- summary(fit)$r.squared     

# график разброса с линией регрессии
# 1. делаем точки прозрачными, чтобы обозначить центр массы
plot(x, 
     y, 
     xlab = 'Стоимость поставки, долл.США', 
     ylab = 'Масса поставки, кг',
     pch = 21, 
     col = rgb(0, 0, 0, alpha = 0.4), 
     bg = rgb(0, 0, 0, alpha = 0.4))
# 2. добавляем прямую регрессии на график
abline(fit, col = rgb(0, 0, 1), lwd = 2)
# 3. добавляем название графика
mtext(paste('Прямая линейная взаимосвязь, R^2=', 
            round(R.sq*100, 1),
            '%', sep = ''), 
      side = 3, line = 1)
# координаты пропущенных y по оси x
NAs <- x[is.na(y)]
# 4. отмечаем каким значениям x соответствуют пропущенные y
points(x = NAs,
       y = rep(0, length(NAs)), 
       col = 'red', pch = '|')

# увеличение участка графика: добавляем xlim, ylim
plot(x, 
     y, 
     xlim = c(0, 5000), 
     ylim = c(0, 5000),
     xlab = 'Стоимость поставки, долл.США', 
     ylab = 'Масса поставки, кг',
     pch = 21, 
     col = rgb(0, 0, 0, alpha = 0.4), 
     bg = rgb(0, 0, 0, alpha = 0.4))
# линия регрессии
abline(fit, col = rgb(0, 0, 1), lwd = 2)
# координаты пропусков по X
points(x = NAs, 
       y = rep(0, length(NAs)), 
       col = 'red', pch = '|')

# пробуем регрессию на логарифмах
fit.log <-  lm(log(y) ~ log(x)) 
# результаты
summary(fit.log)
# сохраняем R-квадрат
R.sq.log <- summary(fit.log)$r.squared  

# график разброса с линией регрессии (логарифмы)
# 1. делаем точки прозрачными, чтобы обозначить центр массы
plot(log(x), 
     log(y), 
     xlab = 'Логарифмы стоимости поставки', 
     ylab = 'Логарифмы массы поставки',
     pch = 21, 
     col = rgb(0, 0, 0, alpha = 0.4), 
     bg = rgb(0, 0, 0, alpha = 0.4))
# 2. добавляем прямую регрессии на график
abline(fit.log, col = rgb(0, 0, 1), lwd = 2)

# 3. добавляем название графика
mtext(paste('Прямая линейная взаимосвязь, R^2=',
            round(R.sq.log*100, 1),
            '%', sep = ''), 
      side = 3, line = 1)

# отмечаем каким значениям x соответствуют пропущенные y
points(x = log(NAs), 
       y = rep(0, length(NAs)), 
       col = 'red', pch = '|')

# новый столбец, в котором будут заполнены пропуски
DT.import[, Netweight.kg.model := Netweight.kg]
# прогноз по модели на логарифмах сохраняем как вектор
y.model.log <- predict(fit.log, newdata = data.frame(x = NAs))
# наносим прогнозы на график
points(log(NAs), 
       y.model.log, 
       pch = '+', cex = 2)
# пересчитываем в исходные единицы измерения y
y.model <- round(exp(y.model.log),0) 
# заполняем пропуски модельными значениями
DT.import[is.na(Netweight.kg.model), Netweight.kg.model :=  y.model]
# смотрим результат
DT.import[, Netweight.kg, Netweight.kg.model]
DT.import[is.na(Netweight.kg), Netweight.kg.model , Trade.Value.USD]

# смотрим, как теперь выглядит распределение Netweight.kg
png('Pic-04.png', width = 500, height = 500)
densityplot( ~ Netweight.kg.model | as.factor(Year), 
            data = DT.import,
            ylim = c(-0.5e-05, 8.5e-05),
            main = 'Распределение массы поставки по годам, Netweight.kg.model',
            xlab = 'Масса поставки, кг',
            ylab = 'Плотность распределения')
dev.off()

# скошенность и эксцесс для переменной в целом
df.stats[nrow(df.stats) + 1, ] <- 
    c('Netweight.kg.model',
      round(skewness(DT.import$Netweight.kg.model), 2),
      round(kurtosis(DT.import$Netweight.kg.model), 2))
df.stats
