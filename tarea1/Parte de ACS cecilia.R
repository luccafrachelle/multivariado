library(tidyverse)
library(FactoMineR)
library(vcd)
library(ca)
load('./data/bfm.RData')
datos = datos %>% select(-c(17:22 , 28)) %>% filter(!if_any(c(1:23), is.na))
datos = datos %>% filter(! (edadrec2 == 1 & edad > 34))
datos2 <- datos %>% mutate(across(where(is.factor), ~ as.integer(.)))
datos2 = datos2 %>% select(-c(ingresos , edad,niveledu.rec))
datos2 = datos2 %>% rename(ingresos = ingresos.rec , edad = edadrec2)


# 2 cat no deja
# tabl <- table(datos2$ingresos,datos2$sexo) 
# catabl <- ca(tabl, nd=2)
# colnames(tabl)
# rownames(tabl) <- c("A", "B", "C", "D", "E", "F")

# más de 2 cat deja
# tabl <- table(datos2$ingresos,datos2$edad) 
# colnames(tabl)
# rownames(tabl) <- c("A", "B", "C", "D", "E", "F")
# catabl <- ca(tabl)
# plot(catabl)

#Sí es 2, no es 1
1
2
#V8-11, ingreso
datos2$V8_9 <- 2*(datos2$V8-1)+datos2$V9
datos2$V8_10 <- 2*(datos2$V8_9-1)+datos2$V10
datos2$V8_11 <- 2*(datos2$V8_10-1)+datos2$V11
tab_ing <- table(datos2$V8_11,  datos2$ingresos)
tab_ing
colnames(tab_ing) <- c('0-17', '17-24', '24-32', '32-40' , '40+' , 'NS/NC')
rownames(tab_ing) <- c("nada", "P", "C", "CP", "D", "DP", "DC", "DCP", "B", "BP", "BC", "BCP", "BD", "BDP", "BDC", "BDCP")
catab_ing <- ca(tab_ing)
sum(catab_ing$rowinertia)
sum(catab_ing$colinertia)#el mismo
plot(catab_ing)
#60%

#V8_11 y edad
tab_edad <- table(datos2$V8_11,  datos2$edad)
tab_edad
colnames(tab_edad)
rownames(tab_edad) <- c("nada", "P", "C", "CP", "D", "DP", "DC", "DCP", "B", "BP", "BC", "BCP", "BD", "BDP", "BDC", "BDCP")
catab_edad <- ca(tab_edad)
sum(catab_edad$rowinertia)
plot(catab_edad)
#87%

#V8_11 y CPO
tab_CPO <- table(datos2$V8_11,  datos2$CPO)
tab_CPO
colnames(tab_CPO)[-c(1,33)] <- rep(NA, 31)
rownames(tab_CPO) <- c("nada", "P", "C", "CP", "D", "DP", "DC", "DCP", "B", "BP", "BC", "BCP", "BD", "BDP", "BDC", "BDCP")
catab_CPO <- ca(tab_CPO)
sum(catab_CPO$rowinertia)
plot(catab_CPO)
#54%

catab_CPO_ <- ca(tab_CPO, supcol=33)
sum(catab_CPO_$rowinertia)
sum(catab_CPO_$colinertia)#NA porque una cat es suplementaria
plot(catab_CPO_)
#58%

catab_CPO_2 <- ca(tab_CPO, suprow=1, supcol=33)
plot(catab_CPO_2)
#59%

#V8_11 y educación
tab_educ <- table(datos2$V8_11,  datos2$niveledu)
tab_educ
colnames(tab_educ) <- c('BC','BI','CBC','CBI','EP','ET', "EU" ,"PC" ,"PI" ,"SEF")
rownames(tab_educ) <- c("nada", "P", "C", "CP", "D", "DP", "DC", "DCP", "B", "BP", "BC", "BCP", "BD", "BDP", "BDC", "BDCP")
catab_educ <- ca(tab_educ)
sum(catab_educ$rowinertia)
plot(catab_educ)
#57%

catab_educ_ <- ca(tab_educ, supcol=c(5,10))
sum(catab_educ_$rowinertia)
plot(catab_educ_)
#64.5%


#V8_11 y V1_3
datos2$V1_2 <- 2*(datos2$V1-1)+datos2$V2
datos2$V1_3<- 2*(datos2$V1_2-1)+datos2$V3
tab_V1_3 <- table(datos2$V8_11,  datos2$V1_3)
tab_V1_3
colnames(tab_V1_3) <- c('000', '001', '010', '011' , '100' , '101', "110", "111")
rownames(tab_V1_3) <- c("nada", "P", "C", "CP", "D", "DP", "DC", "DCP", "B", "BP", "BC", "BCP", "BD", "BDP", "BDC", "BDCP")
catab_V1_3 <- ca(tab_V1_3)
sum(catab_V1_3$rowinertia)
plot(catab_V1_3)
#48.4%

catab_V1_3_ <- ca(tab_V1_3, suprow=13)
sum(catab_V1_3_$colinertia)
plot(catab_V1_3_)
#50%

catab_V1_3_2 <- ca(tab_V1_3, suprow=c(1, 13))
sum(catab_V1_3_2$colinertia)
plot(catab_V1_3_2)
#48.4%

#V8_11 y V4_7
datos2$V4_5 <- 2*(datos2$V4-1)+datos2$V5
datos2$V4_6<- 2*(datos2$V4_5-1)+datos2$V6
datos2$V4_7<- 2*(datos2$V4_6-1)+datos2$V7
tab_V4_7 <- table(datos2$V8_11,  datos2$V4_7)
tab_V4_7
colnames(tab_V4_7) <- c('0000', '0001', '0010', '0011' , '0100' , '0101', "0110", "0111", 
                        '1000', '1001', '1010', '1011' , '1100' , '1101', "1110", "1111")
rownames(tab_V4_7) <- c("nada", "P", "C", "CP", "D", "DP", "DC", "DCP", "B", "BP", "BC", "BCP", "BD", "BDP", "BDC", "BDCP")
catab_V4_7 <- ca(tab_V4_7)
sum(catab_V4_7$rowinertia)
plot(catab_V4_7)
#44.5%

catab_V4_7_ <- ca(tab_V4_7, supcol=c(2, 15, 16))
sum(catab_V4_7_$rowinertia)
plot(catab_V4_7_)
#51%




#Extras, no tienen que ver con V8_11

#V4_7 y V1_3
tab_1_7 <- table(datos2$V1_3, datos2$V4_7)
colnames(tab_1_7) <- c('0000', '0001', '0010', '0011' , '0100' , '0101', "0110", "0111", 
                           '1000', '1001', '1010', '1011' , '1100' , '1101', "1110", "1111")
rownames(tab_1_7) <- c('000', '001', '010', '011' , '100' , '101', "110", "111")
tab_1_7
catab_1_7 <- ca(tab_1_7)
sum(catab_1_7$rowinertia)
plot(catab_1_7)
#57.5

#CPO y V1_3
tab_1_3_CPO <- table(datos2$CPO, datos2$V1_3)
colnames(tab_1_3_CPO) <- c('000', '001', '010', '011' , '100' , '101', "110", "111")
rownames(tab_1_3_CPO)#[-c(1,33)] <- rep(NA, 31)
tab_1_3_CPO
catab_1_3_CPO <- ca(tab_1_3_CPO)
sum(catab_1_3_CPO$rowinertia)
plot(catab_1_3_CPO)
#45.5%

#CPO y V4_7
tab_4_7_CPO <- table(datos2$CPO, datos2$V4_7)
colnames(tab_4_7_CPO) <- c('0000', '0001', '0010', '0011' , '0100' , '0101', "0110", "0111", 
                          '1000', '1001', '1010', '1011' , '1100' , '1101', "1110", "1111")
rownames(tab_4_7_CPO)#[-c(1,33)] <- rep(NA, 31)
tab_4_7_CPO
catab_4_7_CPO <- ca(tab_4_7_CPO)
sum(catab_4_7_CPO$rowinertia)
plot(catab_4_7_CPO)
#36.5%

#edad y V1_3
tab_1_3_edad <- table(datos2$edad, datos2$V1_3)
colnames(tab_1_3_edad) <- c('000', '001', '010', '011' , '100' , '101', "110", "111")
rownames(tab_1_3_edad)
tab_1_3_edad
catab_1_3_edad <- ca(tab_1_3_edad)
sum(catab_1_3_edad$rowinertia)
plot(catab_1_3_edad)
#87%

#edad y V4_7
tab_4_7_edad <- table(datos2$edad, datos2$V4_7)
colnames(tab_4_7_edad) <- c('0000', '0001', '0010', '0011' , '0100' , '0101', "0110", "0111", 
                           '1000', '1001', '1010', '1011' , '1100' , '1101', "1110", "1111")
rownames(tab_4_7_edad)
tab_4_7_edad
catab_4_7_edad <- ca(tab_4_7_edad)
sum(catab_4_7_edad$rowinertia)
plot(catab_4_7_edad)
#92%


#educación y V1_3
tab_1_3_educ <- table(datos2$niveledu, datos2$V1_3)
colnames(tab_1_3_educ) <- c('000', '001', '010', '011' , '100' , '101', "110", "111")
rownames(tab_1_3_educ) <- c('BC','BI','CBC','CBI','EP','ET', "EU" ,"PC" ,"PI" ,"SEF")
tab_1_3_educ
catab_1_3_educ <- ca(tab_1_3_educ)
sum(catab_1_3_educ$rowinertia)
plot(catab_1_3_educ)
#68%


#educación y V4_7
tab_4_7_educ <- table(datos2$niveledu, datos2$V4_7)
colnames(tab_4_7_educ) <- c('0000', '0001', '0010', '0011' , '0100' , '0101', "0110", "0111", 
                            '1000', '1001', '1010', '1011' , '1100' , '1101', "1110", "1111")
rownames(tab_4_7_educ) <- c('BC','BI','CBC','CBI','EP','ET', "EU" ,"PC" ,"PI" ,"SEF")
tab_4_7_educ
catab_4_7_educ <- ca(tab_4_7_educ)
sum(catab_4_7_educ$rowinertia)
plot(catab_4_7_educ)
#50.4%

#educación y V1_3
tab_1_3_ing <- table(datos2$ingresos, datos2$V1_3)
colnames(tab_1_3_ing) <- c('000', '001', '010', '011' , '100' , '101', "110", "111")
rownames(tab_1_3_ing) <- c('0-17', '17-24', '24-32', '32-40' , '40+' , 'NS/NC')
tab_1_3_ing
catab_1_3_ing <- ca(tab_1_3_ing)
sum(catab_1_3_ing$rowinertia)
plot(catab_1_3_ing)
#82%


#ingresos y V4_7
tab_4_7_ing <- table(datos2$ingresos, datos2$V4_7)
colnames(tab_4_7_ing) <- c('0000', '0001', '0010', '0011' , '0100' , '0101', "0110", "0111", 
                            '1000', '1001', '1010', '1011' , '1100' , '1101', "1110", "1111")
rownames(tab_4_7_ing) <- c('0-17', '17-24', '24-32', '32-40' , '40+' , 'NS/NC')
tab_4_7_ing
catab_4_7_ing <- ca(tab_4_7_ing)
sum(catab_4_7_ing$rowinertia)
plot(catab_4_7_ing)
#58%