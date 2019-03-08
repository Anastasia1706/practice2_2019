library('dplyr')
library('lattice')
library('ggplot2')
library('data.table')

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


# сколько месяцев в данных?
unique(DT.import$Period.Desc)
#фильтруем данные для получения необходимого среза данных
(inform <- data.table(filter(DT.import, startsWith(Period.Desc, "September ")
                             | startsWith(Period.Desc, "October ")
                             | startsWith(Period.Desc, "November ") 
                             | startsWith(Period.Desc, "December "))))
#(data <- filter(DT.import, grepl("September ", Period.Desc)))


#проверим количество пропусков
na.num <- apply(inform, 2, function(x) length(which(is.na(x)))) 
sort(na.num[na.num > 0], decreasing = T) #получили один пропуск



# явное преобразование типа, чтобы избежать проблем 
#  при заполнении пропусков
inform[, Netweight.kg := as.double(Netweight.kg) ]
# считаем медианы и округляем до целого, как исходные данные
inform[, round(median(.SD$Netweight.kg, na.rm = T), 0),
       by = Year]
# сначала копируем все значения
inform[, Netweight.kg.median := round(median(.SD$Netweight.kg,
                                             na.rm = T), 0),
       by = Year] 

# затем заменяем пропуски на медианы
inform[!is.na(Netweight.kg), Netweight.kg.median := Netweight.kg] 

# смотрим результат
inform[, Netweight.kg, Netweight.kg.median]
inform[is.na(Netweight.kg), Year, Netweight.kg.median]

#Разделим страны по группам
unique(DT.import$Reporter)
#страны таможенного союза
customs_union <- c('Armenia',"Belarus", "Kazakhstan", "Kyrgyzstan", "Russian Federation")
#страны СНГ, не входящий в таможенный союз
cis <- c("Azerbaijan")
#остальные страны
other_countries <- c("EU-27","Finland","Georgia","Germany","United States of America",
                     "Estonia","Ukraine","Lithuania","Latvia","Mongolia","New Zealand",
                     "United Arab Emirates","Slovenia","Egypt")
cls <- palette(rainbow(3))# обозначили цвета для графиков
#(inform.customs_union <- filter(inform, Reporter=='Armenia'|Reporter=="Belarus"|
#                                  Reporter=="Kazakhstan"|Reporter=="Kyrgyzstan"|
#                                  Reporter=="Russian Federation"))
#plot(x=inform.customs_union$Year, y=inform.customs_union$Netweight.kg.median)

#сделаем поле Reporter ключевым, чтобы сделать возможной фильтрацию
setkey(inform, Reporter)
#inform.customs_union <- inform[c('Armenia',"Belarus", 
#                                 "Kazakhstan", "Kyrgyzstan", "Russian Federation")]

#формируем таблицы по признаку принадлежности стран к тому или иному союзу
inform.customs_union <- inform[customs_union]
inform.cis <- inform[cis]
inform.other_countries <- inform[other_countries]

#добавим столбец фактор принадлежности к союзу
inform1 <- mutate(inform.customs_union, c_fact='customs_union')
inform2 <- mutate(inform.cis, c_fact='cis')
inform3 <- mutate(inform.other_countries, c_fact='other_countries')

#объединяем таблицы в одну новую
inform_super <- data.table()
inform_super <- full_join(inform1,inform2)
inform_super <- full_join(inform_super, inform3)


year_in <- as.factor(unique(inform_super$c_fact))


#считаем суммарные постаки по годам и союзу
res <- select(inform_super, Netweight.kg.median, c_fact,Year) %>%
  group_by(c_fact, Year) %>%
  mutate(Netweight.kg.total = sum(Netweight.kg.median))
res1 <- na.omit(res)

#график, построенный с помощью пакета base------ 
png(filename = 'Pic-1.png', width = 500, height = 500)
plot(x = res1$Year, y = res1$Netweight.kg.total, type = 'p',
     pch = 21, bg = cls[as.factor(res1$c_fact)],
     axes = F, ylim=c(0, 1000000),
     xlim=c(2010,2018),
     xlab = 'Год продажи',
     ylab = 'Количество проданной продукции')
# легенда
legend('topright', legend = year_in, fill = cls[year_in])

# горизонтальная ось
axis(side = 1, pos = 0, at = seq(2010, 2018, by = 1),
     labels = seq(2010, 2018, by = 1))


dev.off()

#график, построенный с помощью пакета lattice-----
pic2 <- xyplot(Netweight.kg.total~Year, data=res1,
               fill.color = cls[as.factor(res1$c_fact)],
               xlab = "Год продажи",
               ylab = 'Количество проданной продукции',
               panel = function(x,y,fill.color,...,subscipts){
                 fill = fill.color[subscipts]
                 panel.xyplot(x,y,pch = 19,
                              col = fill)})
xyplot(Netweight.kg.total~Year, data=res1)
png(filename = 'Pic-2.png', width = 500, height = 500)
pic2
dev.off()
#график построенный с помощью ggplot2-----------
qplot(Year, Netweight.kg.total, data = res1)
png(filename = 'Pic-3.png', width = 500, height = 500)
qplot(Year, Netweight.kg.total, data = res1, color = as.factor(c_fact),
      xlab = 'Год продажи',
      ylab = 'Количество проданной продукции' )
dev.off()