library(ggplot2)
library(plm)
library(devtools)
#library(devtools)
library(recharts)

setwd("D:/workspace/dea面板回归")
huigui <- read.csv(file = '回归数据.csv', header = T)
pdea <- pdata.frame(huigui, index = c('location', 'year'))

p1 <- ggplot(aes(x = year, y =  energy, color = location), data = huigui) + geom_line()

echartr(huigui, year, energy, location, type = 'line') %>% set_legend(pos = 3)

echartr(huigui, year, diversify1, location, type = 'line') %>% set_legend(pos = 3)

echartr(huigui, year, diversify2, location, type = 'line') %>% set_legend(pos = 3)


#######plot map#######
setwd("D:/workspace/dea面板回归")
library(readxl)
library(recharts)
mapd <- read_excel( '地图数据.xlsx', 1) 
mapd <- mapd[-26,]#去掉西藏
mapd$地区 <- factor(mapd$地区)
#names(mapd) <- c('a', 'b' , 'c' , 'd')
#map1 <- mapd[,c(1,2)]
#echartr(mapd, 地区, 能源投入, type = "map_china")
echartr(mapd, 地区, 能源投入, type="map_china") %>%
  setDataRange(splitNumber=0, valueRange=c(range(mapd$能源投入)[1]-1,range(mapd$能源投入)[2]+1), 
               color=c('red','orange','yellow','limegreen','green')) %>%
  setTitle("中国省级能源消耗图")


echartr(mapd, 地区, 工业增加值, type="map_china") %>%
  setDataRange(splitNumber=0, valueRange=c(range(mapd$工业增加值)[1]-1,range(mapd$工业增加值)[2]+1), 
               color=c('red','orange','yellow','limegreen','green')) %>%
  setTitle("中国省级工业增加值图")

echartr(mapd, 地区, 能源禀赋 , type="map_china") %>%
  setDataRange(splitNumber=0, valueRange=c(range(mapd$能源禀赋 )[1]-1,range(mapd$能源禀赋 )[2]+1), 
               color=c('red','orange','yellow','limegreen','green')) %>%
  setTitle("中国省级能源禀赋图")

echartr(mapd, 地区, 二氧化碳排放量 , type="map_china") %>%
  setDataRange(splitNumber=0, valueRange=c(range(mapd$二氧化碳排放量 )[1]-1,range(mapd$二氧化碳排放量 )[2]+1), 
               color=c('red','orange','yellow','limegreen','green')) %>%
  setTitle("中国省级二氧化碳排放量图")


########corplot
library(ggplot2)
library(corrplot)
huigui <- read.csv(file = '回归数据.csv', header = T)
mycor <- cor(huigui[,-c(1,2,22,23)],method = 'spearman')
corrplot(mycor)

#####mi DEA #####
setwd("D:/workspace/dea面板回归")
library(recharts)
library(dplyr)
mi <- read.csv(file = 'mi.csv')
str(mi)



#东北
d1 <- c('黑龙江省','吉林省','辽宁省')
#华东
d2 <- c('上海市','江苏省','浙江省','安徽省','福建省','江西省','山东省')
#华北
d3 <- c('北京市','天津市','山西省','河北省','内蒙古自治区')
#华中
d4 <- c('河南省','湖北省','湖南省')
#华南
d5 <- c('广东省','广西壮族自治区','海南省')
#西南
d6 <- c('四川省','贵州省','云南省','重庆市')
#西北
d7 <-c('陕西省','甘肃省','青海省','宁夏回族自治区','新疆维吾尔自治区')

m1 <- mi[mi$DMU%in%d1,]
t1 <- m1 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '东北')

m2 <- mi[mi$DMU%in%d2,]
t2 <- m2 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '华东')

m3 <- mi[mi$DMU%in%d3,]
t3 <- m3 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '华北')

m4 <- mi[mi$DMU%in%d4,]
t4 <- m4 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '华中')

m5 <- mi[mi$DMU%in%d5,]
t5 <- m5 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '华南')

m6 <- mi[mi$DMU%in%d6,]
t6 <- m6 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '西南')

m7 <- mi[mi$DMU%in%d7,]
t7 <- m7 %>% group_by(Period) %>% 
  summarise(MI = mean(MI), EC = mean(EC), TC = mean(TC)) %>%
  mutate(DMU = '西北')

mt <- data.frame()
mt <- t1 %>% rbind(t2) %>% rbind(t3) %>% rbind(t4) %>% rbind(t5) %>% 
  rbind(t5) %>% rbind(t7)

write.csv(mt, file = '按地理区域划分汇总mi.csv')

mt <- read.csv(file = '按地理区域划分汇总mi.csv')
echartr(mt[order(mt$Period),], Period, MI, DMU, type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区域Malmquist时间序列图")
#mt$Period <- factor(mt$Period)
#mt$DMU <- as.character(mt$DMU)
echartr(mt[order(mt$DMU),], DMU, MI, t = Period, type = 'column') %>% set_legend(pos = 2) %>% 
  setTitle("2016年各地理区域MI指数")

echartr(mt[order(mt$Period),],DMU,MI,facet = Period,type = 'rose') %>% set_legend(pos = 3) %>% 
  setTitle("2003-2016各地理区域MI饼图")


echartr(mt[order(mt$Period),], Period, TC, DMU, type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区域技术效率时间序列图")

echartr(mt[order(mt$Period),],DMU,TC,facet = Period,type = 'rose') %>% set_legend(pos = 3) %>% 
  setTitle("2003-2016各地理区域技术效率饼图")

echartr(mt[order(mt$Period),], Period, EC, DMU, type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区域规模效率时间序列图")

echartr(mt[order(mt$Period),],DMU,EC,facet = Period,type = 'rose',subtype='radius') %>% set_legend(pos = 3) %>% 
  setTitle("2003-2016各地理区域规模效率饼图")
####windows dea#####
setwd("D:/workspace/dea面板回归")
library(recharts)
library(dplyr)
library(stringr)
window <- read.csv(file = 'windows.csv')
str(window)
window$DMU <- as.character(window$DMU)

window <- window %>% mutate(str_extract(window$DMU,"[0-9]{4}"))
window$location <- str_split(window$DMU , '\\{')[[1]][1]
names(window) <- c('DMU','mean','year','location')
window <- window[,c(2,3,4)]
window$year <- as.numeric(window$year)
window$location <- factor(window$location)
echartr(window[order(window$year),], year,mean,location,type = 'line')



###########potential#####
setwd("D:/workspace/dea面板回归")
library(recharts)
library(dplyr)
mi <- read.csv(file = '碳排放潜力画图数据.csv')

######province####
#东北
d1 <- c('黑龙江省','吉林省','辽宁省')
#华东
d2 <- c('上海市','江苏省','浙江省','安徽省','福建省','江西省','山东省')
#华北
d3 <- c('北京市','天津市','山西省','河北省','内蒙古自治区')
#华中
d4 <- c('河南省','湖北省','湖南省')
#华南
d5 <- c('广东省','广西壮族自治区','海南省')
#西南
d6 <- c('四川省','贵州省','云南省','重庆市')
#西北
d7 <-c('陕西省','甘肃省','青海省','宁夏回族自治区','新疆维吾尔自治区')

m1 <- mi[mi$DMU%in%d1,]
t1 <- m1 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '东北')

m2 <- mi[mi$DMU%in%d2,]
t2 <- m2 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '华东')

m3 <- mi[mi$DMU%in%d3,]
t3 <- m3 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '华北')

m4 <- mi[mi$DMU%in%d4,]
t4 <- m4 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '华中')

m5 <- mi[mi$DMU%in%d5,]
t5 <- m5 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '华南')

m6 <- mi[mi$DMU%in%d6,]
t6 <- m6 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '西南')

m7 <- mi[mi$DMU%in%d7,]
t7 <- m7 %>% group_by(Period) %>% 
  summarise(Potential = mean(碳减排潜力)) %>%
  mutate(DMU = '西北')

mt <- data.frame()
mt <- t1 %>% rbind(t2) %>% rbind(t3) %>% rbind(t4) %>% rbind(t5) %>% 
  rbind(t5) %>% rbind(t7)

#pro <- pot %>% group_by(省份,年份) %>% summarise(potential = mean(碳减排潜力))

write.csv(mt,'按地理划分碳减排潜力.csv')

pot <- read.csv(file = '按地理划分碳减排潜力.csv')
echartr(pot[order(pot$Period),],Period,Potential,DMU,type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区域碳排放潜力时间序列图")


echartr(pot[order(pot$Period),],DMU,Potential,facet = Period,type = 'rose') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区域碳排放潜力时间序列图")


#######energy input
setwd("D:/workspace/dea面板回归")
library(recharts)
library(dplyr)
mi <- read.csv(file = '能源投入.csv')

######province####
#东北
d1 <- c('黑龙江省','吉林省','辽宁省')
#华东
d2 <- c('上海市','江苏省','浙江省','安徽省','福建省','江西省','山东省')
#华北
d3 <- c('北京市','天津市','山西省','河北省','内蒙古自治区')
#华中
d4 <- c('河南省','湖北省','湖南省')
#华南
d5 <- c('广东省','广西壮族自治区','海南省')
#西南
d6 <- c('四川省','贵州省','云南省','重庆市')
#西北
d7 <-c('陕西省','甘肃省','青海省','宁夏回族自治区','新疆维吾尔自治区')

m1 <- mi[mi$DMU%in%d1,]
# t1 <- m1 %>% group_by(Period) %>% 
#   summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2),
#             rationlization = mean(rationlization), advance = mean(advance), diversify1 = mean(diversify1), diversify2 = mean(diverdify2),
#             endowment = mean(endowment), fossil_endowment = mean(fossil_endowment), structure = mean(structure ),
#             deflation = mean(deflation), gov = mean(gov), fdi = mean(fdi), intensity = mean(intensity),
#             employ = mean(employ), environment1 = mean(environment1), environment2 = mean(environment2)，
#             electricity = mean(electricity), gas = mean(gas)) %>%
#   mutate(DMU = '东北')

t1 <- m1 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '东北')

m2 <- mi[mi$DMU%in%d2,]
t2 <- m2 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '华东')

m3 <- mi[mi$DMU%in%d3,]
t3 <- m3 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '华北')

m4 <- mi[mi$DMU%in%d4,]
t4 <- m4 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '华中')

m5 <- mi[mi$DMU%in%d5,]
t5 <- m5 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '华南')

m6 <- mi[mi$DMU%in%d6,]
t6 <- m6 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '西南')

m7 <- mi[mi$DMU%in%d7,]
t7 <- m7 %>% group_by(Period) %>% 
  summarise(energy = mean(energy),labor = mean(labor),capital = mean(capital), gdp = mean(gdp), co2 = mean(co2)) %>%
  mutate(DMU = '西北')

mt <- data.frame()
mt <- t1 %>% rbind(t2) %>% rbind(t3) %>% rbind(t4) %>% rbind(t5) %>% 
  rbind(t5) %>% rbind(t7)

write.csv(mt,'投入项按地区汇总数据.csv')

dd <- read.csv(file = '投入项按地区汇总数据.csv')
echartr(dd[order(dd$Period),],Period,energy,DMU,type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区能源投入时间序列图")

echartr(dd[order(dd$Period),],Period,labor,DMU,type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区城镇劳动力投入时间序列图")

echartr(dd[order(dd$Period),],Period,capital,DMU,type = 'line') %>% set_legend(pos = 3) %>% 
  setTitle("各地理区资本投入时间序列图")

