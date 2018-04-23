library(xlsx)
library(dplyr)
library(stringr)
#setwd("F:/dea面板回归")
setwd("D:/workspace/dea面板回归")
dea_row1 <- read.xlsx2(file = 'dea_raw.xlsx', 1)
dea_row2 <- read.xlsx2(file = 'dea_raw.xlsx', 2)
dea_row3 <- read.xlsx2(file = 'dea_raw.xlsx', 3)
dea_row4 <- read.xlsx2(file = 'dea_raw.xlsx', 4)
dea_row5 <- read.xlsx2(file = 'dea_raw.xlsx', 5)
dea_row6 <- read.xlsx2(file = 'dea_raw.xlsx', 6)
dea_row7 <- read.xlsx2(file = 'dea_raw.xlsx', 7)
dea_row8 <- read.xlsx2(file = 'dea_raw.xlsx', 8)
dea_row9 <- read.xlsx2(file = 'dea_raw.xlsx', 9)
dea_row10 <- read.xlsx2(file = 'dea_raw.xlsx', 10)
dea_row11 <- read.xlsx2(file = 'dea_raw.xlsx', 11)
dea_row12 <- read.xlsx2(file = 'dea_raw.xlsx', 12)
dea_row13 <- read.xlsx2(file = 'dea_raw.xlsx', 13)
dea_row14 <- read.xlsx2(file = 'dea_raw.xlsx', 14)
dea_row15 <- read.xlsx2(file = 'dea_raw.xlsx', 15)
dea_row16 <- read.xlsx2(file = 'dea_raw.xlsx', 16)
dea_row17 <- read.xlsx2(file = 'dea_raw.xlsx', 17)
dea_row18 <- read.xlsx2(file = 'dea_raw.xlsx', 18)
dea_row19 <- read.xlsx2(file = 'dea_raw.xlsx', 19)
dea_row20 <- read.xlsx2(file = 'dea_raw.xlsx', 20)
dea_row21 <- read.xlsx2(file = 'dea_raw.xlsx', 21)
dea_row22 <- read.xlsx2(file = 'dea_raw.xlsx', 22)
dea_row23 <- read.xlsx2(file = 'dea_raw.xlsx', 23)
#dea_row24 <- read.xlsx2(file = 'dea_raw.xlsx', 24)

#for(i in 1:17){
  eval(parse(paste('dea_row', i,sep = ''))) <- read.xlsx2(file = 'dea_raw.xlsx', i)
}

#####preprocess
final_row1 <- data.frame()
for(i in 1 : dim(dea_row1)[1]){
  test_1 <- slice(dea_row1,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','labor')
  rownames(tmp) <- NULL
  final_row1 <- rbind(final_row1 , tmp)
}

final_row2 <- data.frame()
for(i in 1 : dim(dea_row2)[1]){
  test_1 <- slice(dea_row2,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','capital')
  rownames(tmp) <- NULL
  final_row2 <- rbind(final_row2 , tmp)
}

final_row3 <- data.frame()
for(i in 1 : dim(dea_row3)[1]){
  test_1 <- slice(dea_row3,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','energy')
  rownames(tmp) <- NULL
  final_row3 <- rbind(final_row3 , tmp)
}

final_row4 <- data.frame()
for(i in 1 : dim(dea_row4)[1]){
  test_1 <- slice(dea_row4,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','gdp')
  rownames(tmp) <- NULL
  final_row4 <- rbind(final_row4 , tmp)
}

final_row5 <- data.frame()
for(i in 1 : dim(dea_row5)[1]){
  test_1 <- slice(dea_row5,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','co2')
  rownames(tmp) <- NULL
  final_row5 <- rbind(final_row5 , tmp)
}

final_row6 <- data.frame()
for(i in 1 : dim(dea_row6)[1]){
  test_1 <- slice(dea_row6,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','rationlization')
  rownames(tmp) <- NULL
  final_row6 <- rbind(final_row6 , tmp)
}

final_row7 <- data.frame()
for(i in 1 : dim(dea_row7)[1]){
  test_1 <- slice(dea_row7,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','advance')
  rownames(tmp) <- NULL
  final_row7 <- rbind(final_row7 , tmp)
}

final_row8 <- data.frame()
for(i in 1 : dim(dea_row8)[1]){
  test_1 <- slice(dea_row8,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','diversify1')
  rownames(tmp) <- NULL
  final_row8 <- rbind(final_row8 , tmp)
}

final_row9 <- data.frame()
for(i in 1 : dim(dea_row9)[1]){
  test_1 <- slice(dea_row9,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','diversify2')
  rownames(tmp) <- NULL
  final_row9 <- rbind(final_row9 , tmp)
}

final_row10 <- data.frame()
for(i in 1 : dim(dea_row10)[1]){
  test_1 <- slice(dea_row10,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','endowment')
  rownames(tmp) <- NULL
  final_row10 <- rbind(final_row10 , tmp)
}

final_row11 <- data.frame()
for(i in 1 : dim(dea_row11)[1]){
  test_1 <- slice(dea_row11,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','fossil_endowment')
  rownames(tmp) <- NULL
  final_row11 <- rbind(final_row11 , tmp)
}

final_row12 <- data.frame()
for(i in 1 : dim(dea_row12)[1]){
  test_1 <- slice(dea_row12,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','structure')
  rownames(tmp) <- NULL
  final_row12 <- rbind(final_row12 , tmp)
}

final_row13 <- data.frame()
for(i in 1 : dim(dea_row13)[1]){
  test_1 <- slice(dea_row13,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','deflation')
  rownames(tmp) <- NULL
  final_row13 <- rbind(final_row13 , tmp)
}

final_row14 <- data.frame()
for(i in 1 : dim(dea_row14)[1]){
  test_1 <- slice(dea_row14,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','gov')
  rownames(tmp) <- NULL
  final_row14 <- rbind(final_row14 , tmp)
}

final_row15 <- data.frame()
for(i in 1 : dim(dea_row15)[1]){
  test_1 <- slice(dea_row15,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','fdi')
  rownames(tmp) <- NULL
  final_row15 <- rbind(final_row15 , tmp)
}

final_row16 <- data.frame()
for(i in 1 : dim(dea_row16)[1]){
  test_1 <- slice(dea_row16,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','intensity')
  rownames(tmp) <- NULL
  final_row16 <- rbind(final_row16 , tmp)
}

final_row17 <- data.frame()
for(i in 1 : dim(dea_row17)[1]){
  test_1 <- slice(dea_row17,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','employ')
  rownames(tmp) <- NULL
  final_row17 <- rbind(final_row17 , tmp)
}

final_row18 <- data.frame()
for(i in 1 : dim(dea_row18)[1]){
  test_1 <- slice(dea_row18,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','environment1')
  rownames(tmp) <- NULL
  final_row18 <- rbind(final_row18 , tmp)
}

final_row19 <- data.frame()
for(i in 1 : dim(dea_row19)[1]){
  test_1 <- slice(dea_row19,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','environment2')
  rownames(tmp) <- NULL
  final_row19 <- rbind(final_row19 , tmp)
}

final_row20 <- data.frame()
for(i in 1 : dim(dea_row20)[1]){
  test_1 <- slice(dea_row20,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','electricity')
  rownames(tmp) <- NULL
  final_row20 <- rbind(final_row20 , tmp)
}


final_row21 <- data.frame()
for(i in 1 : dim(dea_row21)[1]){
  test_1 <- slice(dea_row21,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','gas')
  rownames(tmp) <- NULL
  final_row21 <- rbind(final_row21 , tmp)
}


final_row22 <- data.frame()
for(i in 1 : dim(dea_row22)[1]){
  test_1 <- slice(dea_row22,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','fossil')
  rownames(tmp) <- NULL
  final_row22 <- rbind(final_row22 , tmp)
}

final_row23 <- data.frame()
for(i in 1 : dim(dea_row23)[1]){
  test_1 <- slice(dea_row23,i)
  test_1_1 <- merge(test_1[1],colnames(test_1)[-1])
  tmp <- cbind(test_1_1 , t(test_1[1,-1]))
  names(tmp) <- c('location','year','coal')
  rownames(tmp) <- NULL
  final_row23 <- rbind(final_row23 , tmp)
}

final <- data.frame()
final <- bind_cols(final_row1 , final_row2) %>% bind_cols(final_row3) %>% 
  bind_cols(final_row4) %>% bind_cols(final_row5) %>% bind_cols(final_row6) %>% 
  bind_cols(final_row7) %>% bind_cols(final_row8) %>% bind_cols(final_row9) %>% 
  bind_cols(final_row10) %>% bind_cols(final_row11) %>% bind_cols(final_row12) %>% 
  bind_cols(final_row13) %>% bind_cols(final_row14) %>% bind_cols(final_row15) %>% 
  bind_cols(final_row16) %>% bind_cols(final_row17) %>% bind_cols(final_row18) %>% 
  bind_cols(final_row19)

final<- final[final$location != '西藏自治区',] 
final <- final %>% bind_cols(final_row20) %>% bind_cols(final_row21) %>% bind_cols(final_row22) %>% bind_cols(final_row23)

final_1 <- final[,c(1,2,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66,69)]
final_1$year <- as.character(final_1$year)
final_1$year <- str_replace(final_1$year,'年','')
final_1$year <- str_replace(final_1$year,'X','')

#final_2 <- final_1[final_1$location != '西藏自治区',] # 去除西藏
if(exists('final.csv')) remove('final.csv')
write.csv(final_1,file = 'final.csv')




#####dependent variable#####
myfactor <- read.csv(file = '因变量.csv', header = T)
dea <- read.csv(file = 'final.csv')
a1 <- group_by(myfactor, dmu)
a2 <- summarise(a1, potential = mean(potential), score = mean(Score),alpha = mean(Alpha))
a2$year <- str_extract(a2$dmu, '[0-9]{4}')
a2$location <- str_sub(a2$dmu, end = -7)
a2 <- a2[,-1]
a2$location <- factor(a2$location)
a2$year <- as.numeric(a2$year)
a2$potential <- (a2$potential *a2$alpha)
dea_1 <- merge(dea, a2, by = c('location', 'year'))

if(exists('回归数据.csv')) remove('回归数据.csv')
write.csv(dea_1, file = '回归数据.csv')

######tobit####

#####projection###
library(censReg)
library(plm)
huigui <- read.csv(file = '回归数据.csv', header = T)

pdea <- pdata.frame(huigui, index = c('location', 'year'))

######potential tobit#####
myformula1 <- I(potential/100) ~ rationlization + advance + diversify1 + diversify2 + 
    endowment + fossil_endowment + structure + deflation + gov + fdi + intensity + employ 
tobitresult1 <- censReg(myformula1 , data = pdea ,method = 'BHHH')
summary(tobitresult1)


myformula2 <- I(potential/100) ~ rationlization + advance + diversify1  + 
  endowment + fossil_endowment + structure + deflation + fdi + intensity + employ +environment1
tobitresult2 <- censReg(myformula2 , data = pdea ,method = 'BHHH')
summary(tobitresult2)

myformula3 <- I(potential/100) ~ rationlization + advance + diversify2  + 
  endowment + fossil_endowment + structure + deflation + fdi + intensity + employ +environment1
tobitresult3 <- censReg(myformula3 , data = pdea ,method = 'BHHH')
summary(tobitresult3)

myformula4 <- I(potential/100) ~ rationlization + advance + diversify1  + 
  endowment + fossil_endowment + structure + deflation + fdi + intensity + employ +environment2
tobitresult4 <- censReg(myformula4 , data = pdea ,method = 'BHHH')
summary(tobitresult4)

myformula5 <- I(potential/100) ~ rationlization + advance + diversify2  + 
  endowment + fossil_endowment + structure + deflation + fdi + intensity + employ +environment2
tobitresult5 <- censReg(myformula5 , data = pdea ,method = 'BHHH')
summary(tobitresult5)

myformula6 <- I(potential/100) ~ rationlization + advance + diversify2  + 
  endowment + fossil_endowment + structure + deflation  + intensity + employ +environment1
tobitresult6 <- censReg(myformula6 , data = pdea ,method = 'BHHH')
summary(tobitresult6)


#########

lm_1 <- censReg(potential ~ rationlization + advance + diversify1 + diversify2 + 
                  endowment + fossil_endowment + structure , data = pdea, method = "BHHH")

lm_2 <- censReg(potential ~ rationlization + advance + diversify1 + diversify2 + 
                  endowment + fossil_endowment + structure + deflation + gov + I(100*intensity) + I(100*employ), data = pdea, method = "BHHH")
lm_4 <- plm(score ~ rationlization + advance + diversify1 + diversify2 + 
              endowment + fossil_endowment + structure + deflation + gov + intensity + employ,data = pdea , model = 'random')

####index

lm_3 <- censReg(score ~ rationlization + advance + diversify1 + diversify2 + 
                  endowment + fossil_endowment + structure + deflation + gov + intensity + employ, data = pdea, left = 0 , right = 1, method = 'BHHH')
