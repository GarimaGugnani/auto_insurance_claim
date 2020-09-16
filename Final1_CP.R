#Set directory
setwd("D:/R files/Capstone Project")
#Read file
mb = read.csv("Car insurance data set.csv", na.strings=c("", "NA"))

#Data structure
dim(mb)
names(mb)
str(mb)
str(mb[,c(120:127)])
summary(mb)

#Target variable
mb$ClaimStatus=as.factor(mb$ClaimStatus)
table(mb$ClaimStatus)
prop.table(table(mb$ClaimStatus))

##Changing Variable Type
mb$Renewed=as.factor(mb$Renewed)
mb$CoverageLiability=as.factor(mb$CoverageLiability)
mb$CoveragePD_1=as.factor(mb$CoveragePD_1)
mb$CoverageMP=as.factor(mb$CoverageMP)
mb$CoveragePIP_CDW=as.factor(mb$CoveragePIP_CDW)
mb$CoverageUMBI=as.factor(mb$CoverageUMBI)
mb$CoverageUMPD=as.factor(mb$CoverageUMPD)
mb$Engine_1=as.numeric(mb$Engine_1)
mb$MaritalStatus_1=as.factor(mb$MaritalStatus_1)
mb$MaritalStatus_2=as.factor(mb$MaritalStatus_2)
mb$MaritalStatus_3=as.factor(mb$MaritalStatus_3)
mb$MaritalStatus_4=as.factor(mb$MaritalStatus_4)
mb$MaritalStatus_5=as.factor(mb$MaritalStatus_5)
mb$Sex_1=as.factor(mb$Sex_1)
mb$Sex_2=as.factor(mb$Sex_2)
mb$Sex_3=as.factor(mb$Sex_3)
mb$Sex_4=as.factor(mb$Sex_4)
mb$Sex_5=as.factor(mb$Sex_5)
mb$Surcharge1Unit_1=as.factor(mb$Surcharge1Unit_1)
mb$Surcharge2Unit_1=as.factor(mb$Surcharge2Unit_1)
mb$Surcharge3Unit_1=as.factor(mb$Surcharge3Unit_1)
mb$VehicleInspected_1=as.factor(mb$VehicleInspected_1)
mb$NoLossSigned=as.factor(mb$NoLossSigned)
mb$Type=as.factor(mb$Type)
mb$CancellationType=as.factor(mb$CancellationType)
mb$Year_1=as.factor(mb$Year_1)
mb$GaragedZIP_1=as.factor(mb$GaragedZIP_1)
mb$Zip=as.factor(mb$Zip)

str(mb)
str(mb[,c(120:127)])
summary(mb)

#Exclude variable seems not important
library(dplyr)
data1 = mb %>% select(-"Sr.No", -"DOB1",-"DOB2",-"DOB3",-"DOB4",-"DOB5",-"ExcludedDriverName_01",-"ExcludedDriverName_02",-"ExcludedDriverName_03",
                      -"ExcludedDriverName_04",-"ExcludedDriverName_05",-"ExcludedDriverName_06",-"ExcludedDriverName_07",-"ExcludedDriverName_08",
                      -"ExcludedDriverName_09",-"ExcludedDriverName_10",-"ExcludedDriverName_11",-"ExcludedDriverName_12",-"ExcludedDriverName_13",
                      -"ExcludedDriverName_14",-"ExcludedDriverName_15",-"ExcludedDriverName_16",-"ExcludedDriverName_17",-"ExcludedDriverName_18",
                      -"ExcludedDriverName_19",-"ExcludedDriverName_20")

summary(data1[,c(62,63,65:93)])
data1=data1[,-c(62,63,65:93)]#removed empty violation points

str(data1)
############################################################
library(ggplot2)
attach(data1)

##Binary
p1=data1 %>%
  ggplot(aes(Renewed, fill = Renewed)) +
  geom_bar() +
  theme(legend.position = "none")

p2=data1 %>%
  ggplot(aes(VehicleInspected_1, fill = VehicleInspected_1)) +
  geom_bar() +
  theme(legend.position = "none")

p3=data1 %>%
  ggplot(aes(NoLossSigned, fill = NoLossSigned)) +
  geom_bar() +
  theme(legend.position = "none")

p4=data1 %>%
  ggplot(aes(MaritalStatus_1, fill = MaritalStatus_1)) +
  geom_bar() +
  theme(legend.position = "none")

p5=data1 %>%
  ggplot(aes(Sex_1, fill = Sex_1)) +
  geom_bar() +
  theme(legend.position = "none")

p6=data1 %>%
  ggplot(aes(CoverageUMBI, fill = CoverageUMBI)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6)

###Categorical

p7=data1 %>%
  ggplot(aes(Type, fill = Type)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p8=data1 %>%
  ggplot(aes(CancellationType, fill = CancellationType)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p9=data1 %>%
  ggplot(aes(Surcharge1Unit_1, fill = Surcharge1Unit_1)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p10=data1 %>%
  ggplot(aes(Surcharge2Unit_1, fill = Surcharge2Unit_1)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p11=data1 %>%
  ggplot(aes(CoveragePIP_CDW, fill = CoveragePIP_CDW)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p12=data1 %>%
  ggplot(aes(CoveragePD_1, fill = CoveragePD_1)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p13=data1 %>%
  ggplot(aes(CoverageMP, fill = CoverageMP)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p14=data1 %>%
  ggplot(aes(CoverageLiability, fill = CoverageLiability)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

p15=data1 %>%
  ggplot(aes(CoverageUMPD, fill = CoverageUMPD)) +
  geom_bar() +
  scale_y_log10() +
  theme(legend.position = "none")

grid.arrange(p7, p8, p9, p10, p11, p12, p13, p14, p15)

###Integer
b1=data1 %>%
  mutate(Billing_Term = as.factor(Billing_Term)) %>%
  ggplot(aes(Billing_Term, fill = Billing_Term)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b2=data1 %>%
  mutate(Units = as.factor(Units)) %>%
  ggplot(aes(Units, fill = Units)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b3=data1 %>%
  mutate(Amendment = as.factor(Amendment)) %>%
  ggplot(aes(Amendment, fill = Amendment)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b4=data1 %>%
  mutate(ViolPoints1Driver_1 = as.factor(ViolPoints1Driver_1)) %>%
  ggplot(aes(ViolPoints1Driver_1, fill = ViolPoints1Driver_1)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b5=data1 %>%
  mutate(ViolPoints1Driver_2 = as.factor(ViolPoints1Driver_2)) %>%
  ggplot(aes(ViolPoints1Driver_2, fill = ViolPoints1Driver_2)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b6=data1 %>%
  mutate(ViolPoints1Driver_3 = as.factor(ViolPoints1Driver_3)) %>%
  ggplot(aes(ViolPoints1Driver_3, fill = ViolPoints1Driver_3)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b7=data1 %>%
  mutate(ViolPoints1Driver_4 = as.factor(ViolPoints1Driver_4)) %>%
  ggplot(aes(ViolPoints1Driver_4, fill = ViolPoints1Driver_4)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b8=data1 %>%
  mutate(ViolPoints1Driver_5 = as.factor(ViolPoints1Driver_5)) %>%
  ggplot(aes(ViolPoints1Driver_5, fill = ViolPoints1Driver_5)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b9=data1 %>%
  mutate(ViolPoints2Driver_1= as.factor(ViolPoints2Driver_1)) %>%
  ggplot(aes(ViolPoints2Driver_1, fill = ViolPoints2Driver_1)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b10=data1 %>%
  mutate(ViolPoints2Driver_2= as.factor(ViolPoints2Driver_2)) %>%
  ggplot(aes(ViolPoints2Driver_2, fill = ViolPoints2Driver_2)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b11=data1 %>%
  mutate(ViolPoints2Driver_3= as.factor(ViolPoints2Driver_3)) %>%
  ggplot(aes(ViolPoints2Driver_3, fill = ViolPoints2Driver_3)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b12=data1 %>%
  mutate(ViolPoints3Driver_1= as.factor(ViolPoints3Driver_1)) %>%
  ggplot(aes(ViolPoints3Driver_1, fill = ViolPoints3Driver_1)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b13=data1 %>%
  mutate(Rental_1= as.factor(Rental_1)) %>%
  ggplot(aes(Rental_1, fill = Rental_1)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b14=data1 %>%
  mutate(ClaimFrequency= as.factor(ClaimFrequency)) %>%
  ggplot(aes(ClaimFrequency, fill = ClaimFrequency)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b15=data1 %>%
  mutate(Number_of_Driver= as.factor(Number_of_Driver)) %>%
  ggplot(aes(Number_of_Driver, fill = Number_of_Driver)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

b16=data1 %>%
  mutate(Towing_1= as.factor(Towing_1)) %>%
  ggplot(aes(Towing_1, fill = Towing_1)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")


b17=data1 %>%
  mutate(Units= as.factor(Units)) %>%
  ggplot(aes(Units, fill = Units)) +
  geom_bar(fill="green") +
  theme(legend.position = "none")

grid.arrange(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b12, b13, b14, b15, b16, b17)

#Continuous
c1=data1 %>%
  ggplot(aes(Premium, fill = Premium)) +
  geom_histogram(fill = "blue")+
  theme(legend.position = "none")


c2=data1 %>%
  ggplot(aes(AgeUSdriving_1, fill = AgeUSdriving_1)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")


c3=data1 %>%
  ggplot(aes(AgeUSdriving_2, fill = AgeUSdriving_2)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c4=data1 %>%
  ggplot(aes(AgeUSdriving_3, fill = AgeUSdriving_3)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c5=data1 %>%
  ggplot(aes(AgeUSdriving_4, fill = AgeUSdriving_4)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c6=data1 %>%
  ggplot(aes(AgeUSdriving_5, fill = AgeUSdriving_5)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c7=data1 %>%
  ggplot(aes(Engine_1, fill = Engine_1)) +
  geom_histogram(fill = "blue")+
  theme(legend.position = "none")

c8=data1 %>%
  ggplot(aes(DistanceToWork_1, fill = DistanceToWork_1)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c9=data1 %>%
  ggplot(aes(DistanceToWork_2, fill = DistanceToWork_2)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c10=data1 %>%
  ggplot(aes(DistanceToWork_3, fill = DistanceToWork_3)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c11=data1 %>%
  ggplot(aes(DistanceToWork_4, fill = DistanceToWork_4)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c12=data1 %>%
  ggplot(aes(DistanceToWork_5, fill = DistanceToWork_5)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")

c13=data1 %>%
  ggplot(aes(Total_Distance_To_Work, fill = Total_Distance_To_Work)) +
  geom_histogram(fill = "blue") +
  theme(legend.position = "none")


grid.arrange(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c12, c13)

###Target variable
ggplot(data1 %>% group_by(ClaimStatus) %>% summarise(Count = n())) +   
  geom_bar(aes(ClaimStatus, Count), stat = "identity", fill = "pink")

###Bivariate######################################

d1=ggplot(data1, aes(x=ClaimStatus, y= Premium)) + 
  geom_boxplot()

d2=ggplot(data1, aes(x=ClaimStatus, y= Total_Distance_To_Work)) + 
  geom_boxplot()#scatter

d3=ggplot(data1, aes(x=ClaimStatus, y= AgeUSdriving_1)) + 
  geom_boxplot()

d4=ggplot(data1, aes(x=ClaimStatus, y= DistanceToWork_1)) + 
  geom_boxplot()#scatter

d5=ggplot(data1, aes(x=Renewed, y= Premium)) + 
  geom_boxplot()

d6=ggplot(data1, aes(AgeUSdriving_1, Premium)) +
  geom_bar(stat="identity", na.rm = TRUE) 

d7=ggplot(data1, aes(as.factor(Billing_Term), Premium)) +
  geom_bar(stat="identity", na.rm = TRUE)+
  xlab("Billing Term") + ylab("Premium")

d8=ggplot(data1, aes(as.factor(Renewed), Premium)) +
  geom_bar(stat="identity", na.rm = TRUE)+
  xlab("Renewed") + ylab("Premium")

d9=ggplot(data1, aes(as.factor(ClaimFrequency), Premium)) +
  geom_bar(stat="identity", na.rm = TRUE)+
  xlab("Claim Frequency") + ylab("Premium")


d11=ggplot(data1,  aes(x = as.factor(Billing_Term),  fill = ClaimStatus)) + geom_bar(position = "stack")+
  xlab("Billing Term") + ylab("Claim Status")

d12=ggplot(data1,  aes(x = Renewed,  fill = ClaimStatus)) + geom_bar(position = "stack")

d13=ggplot(data1,  aes(x = as.factor(Amendment),  fill = ClaimStatus)) + geom_bar(position = "stack")

d14=ggplot(data1,  aes(x = Type,  fill = ClaimStatus)) + geom_bar(position = "stack")

d15=ggplot(data1,  aes(x = CancellationType,  fill = ClaimStatus)) + geom_bar(position = "stack")

d16=ggplot(data1,  aes(x = as.factor(ClaimFrequency),  fill = ClaimStatus)) + geom_bar(position = "stack")+
  xlab("Claim Frequency") + ylab("Claim Status")

d17=ggplot(data1,  aes(x = as.factor(Billing_Term),  fill = Renewed)) + geom_bar(position = "stack")+
  xlab("Billing Term") + ylab("Renewed")


grid.arrange(d1, d2, d3, d4, d5, d6, d7, d8, d9)
grid.arrange(d11, d12, d13, d14,d15, d16, d17)
###################################################################################


z=c("/","1.7/4","1.8L/","107/4","108/4","110/4","110/4","113/4","114/4","116/4","119/4","121/4",
    "122/4","132/4","133/4","134/4","135/4","138/4","140/4","144/4","146/4","150/4","151/4","153/4",
    "163/6","165/6","165/6","170/6","170/6","173/6","179/6","180/6","181/6","182/6","183/6","183/5",
    "189/6","191/6","192/6","195/6","2.0/4","2.0L","2.3/4","2.4/4","2.5/4","2.5/6","2.5L","2.7/6",
    "200/6","201/6","204/6","207/6","214/6","219/6","231/6","232/6","239/6","242/6","242/8","244/6",
    "250/8","256/6","256/6","262/6","262/8","281/8","293/8","3.0/6","3.1/6","3.4L/","3.7/6","3.8/6",
    "300/6","300/8","302/8","305/8","307/8","318/8","323/8","326/12","330/8","350/8","351/8","360/6",
    "360/8","366/8","379/8","4.0/6","4.2/6","4.3/6","4.6/8","4.7/8","451/8","5.0L/","5.4/8","5.6/8",
    "6.0/8","6.8/10","61/3","79/4","80/4","89/4","90/4","91/4","97/4","98/4")

library(stringr)
daat1$Engine_1=str_replace(data1$Engine_1, "z","NA")
class(data1$Engine_1)
data1$Engine_1
data1$Engine_1=as.numeric(data1$Engine_1)

data1=data1%>% select(-"DistanceToWork_1",-"DistanceToWork_2",-"DistanceToWork_3",-"DistanceToWork_4",-"DistanceToWork_5")

data1$AgeUSdriving_5[data1$AgeUSdriving_5==0]=NA
summary(data1$AgeUSdriving_1)
data1$AgeUSdriving_4[data1$AgeUSdriving_4==0]=NA
data1$AgeUSdriving_3[data1$AgeUSdriving_3==0]=NA
data1$AgeUSdriving_2[data1$AgeUSdriving_2==0]=NA
data1$AgeUSdriving_1[data1$AgeUSdriving_1==0]=NA


########################################################
####Missing Values#########################

sum(is.na(data1))/prod(dim(data1))
#30% missing values
colSums(is.na(data1))

library(DataExplorer)
plot_missing(data1)
data2=data1[, which(colMeans(!is.na(data1)) > 0.5)]
sum(is.na(data2))/prod(dim(data2))
plot_missing(data2)

data2 = data2 %>% select(-"Occupation_1", -"Relation_1")
colSums(is.na(data2))

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}


data2$Surcharge1Unit_1[which(is.na(data2$Surcharge1Unit_1))]=Mode(data2$Surcharge1Unit_1)
sum(is.na(data2$Surcharge1Unit_1))

data2$Surcharge1Unit_1[which(is.na(data2$Surcharge1Unit_1))]=Mode(data2$Surcharge1Unit_1)
sum(is.na(data2$Surcharge1Unit_1))

data2$Surcharge2Unit_1[which(is.na(data2$Surcharge2Unit_1))]=Mode(data2$Surcharge2Unit_1)
sum(is.na(data2$Surcharge2Unit_1))

data2$Surcharge3Unit_1[which(is.na(data2$Surcharge3Unit_1))]=Mode(data2$Surcharge3Unit_1)
sum(is.na(data2$Surcharge3Unit_1))

data2$CoverageMP[which(is.na(data2$CoverageMP))]=Mode(data2$CoverageMP)
sum(is.na(data2$CoverageMP))

data2$CoveragePD_1[which(is.na(data2$CoveragePD_1))]=Mode(data2$CoveragePD_1)
sum(is.na(data2$CoveragePD_1))

data2$CoveragePIP_CDW[which(is.na(data2$CoveragePIP_CDW))]=Mode(data2$CoveragePIP_CDW)
sum(is.na(data2$CoveragePIP_CDW))

data2$CoverageUMBI[which(is.na(data2$CoverageUMBI))]=Mode(data2$CoverageUMBI)
sum(is.na(data2$CoverageUMBI))

data2$CoverageUMPD[which(is.na(data2$CoverageUMPD))]=Mode(data2$CoverageUMPD)
sum(is.na(data2$CoverageUMPD))

data2$Make_1[which(is.na(data2$Make_1))]=Mode(data2$Make_1)
sum(is.na(data2$Make_1))

data2$Model_1[which(is.na(data2$Model_1))]=Mode(data2$Model_1)
sum(is.na(data2$Model_1))

#numeric missing
data2$Engine_1[is.na(data2$Engine_1)] <- mean(data2$Engine_1, na.rm = TRUE)

plot_missing(data2)

data_p1=data2
summary(data_p1)
#Variable Treatment#########################################################################################################

colnames(data_p1[,sapply(data_p1, is.numeric)])
summary(data_p1[,sapply(data_p1, is.numeric)])
table(data_p1$Towing_1)

data_p1$Amendment=as.factor(data_p1$Amendment)
summary(data_p1$Amendment)
library(forcats)
data_p1$Amendments_Made = fct_collapse(data_p1$Amendment, no = "0", yes = c(1:10))
data_p1 = data_p1 %>% select(-"Amendment")
summary(data_p1$Amendments_Made)

data_p1$Rental_1=as.factor(data_p1$Rental_1)
summary(data_p1$Rental_1)
data_p1$Rental_charge = fct_collapse(data_p1$Rental_1, no = "0", yes = c(1:300))
data_p1 = data_p1 %>% select(-"Rental_1")
summary(data_p1$Rental_charge)

data_p1$Towing_1=as.factor(data_p1$Towing_1)
summary(data_p1$Towing_1)
data_p1$towing_cost= fct_collapse(data_p1$Towing_1, no = "0", yes = c(1:300))
data_p1 = data_p1 %>% select(-"Towing_1")
summary(data_p1$towing_cost)

#################outlier treatment
colnames(data_p1[,sapply(data_p1, is.numeric)])

boxplot(data_p1$Premium,data_p1$AgeUSdriving_1,data_p1$Total_Distance_To_Work,data_p1$Engine_1, names= c("Premium", "AgeUSdriving_1", "Total_Distance_to_Work","Engine")  )


capOutlier <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}


data_p1$Premium=capOutlier(data_p1$Premium)
data_p1$AgeUSdriving_1=capOutlier(data_p1$AgeUSdriving_1)
data_p1$Total_Distance_To_Work=capOutlier(data_p1$Total_Distance_To_Work)

data3=data_p1
################################

########################Numeric and Factors 
num=data3[,sapply(data3, is.numeric)]
summary(num)
fac=data3[,sapply(data3, is.factor)]
summary(fac)
#########################FISHER TEST
colnames(fac)

tab3 = table(fac$Renewed, fac$ClaimStatus)
chisq.test(tab3)
fisher.test(tab3, simulate.p.value = T)##YES


tab5 = table(fac$CoverageLiability, fac$ClaimStatus)
chisq.test(tab5)
fisher.test(tab5, simulate.p.value = T)##YES

tab6 = table(fac$CoverageMP, fac$ClaimStatus)
chisq.test(tab6)
fisher.test(tab6, simulate.p.value = T)#NO

tab7 = table(fac$CoveragePD_1, fac$ClaimStatus)
chisq.test(tab7)
fisher.test(tab7, simulate.p.value = T)##YES

tab8 = table(fac$CoveragePIP_CDW, fac$ClaimStatus)
chisq.test(tab8)
fisher.test(tab8, simulate.p.value = T)#NO

tab9 = table(fac$CoverageUMBI, fac$ClaimStatus)
chisq.test(tab9)
fisher.test(tab9, simulate.p.value = T)##NOOO

tab10= table(fac$CoverageUMPD, fac$ClaimStatus)
chisq.test(tab10)
fisher.test(tab10, simulate.p.value = T)##NO


tab11= table(fac$MaritalStatus_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##YES

tab11= table(fac$Rental_charge, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##YES

tab11= table(fac$Sex_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##YES

tab11= table(fac$Surcharge1Unit_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##NO

tab11= table(fac$Surcharge2Unit_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##NO

tab11= table(fac$Surcharge3Unit_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##YES

tab11= table(fac$VehicleInspected_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#YES

tab11= table(fac$NoLossSigned, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#YES

tab11= table(fac$Type, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#YES

tab11= table(fac$Amendments_Made, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#NO

tab11= table(fac$Zip, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#NO

tab11= table(fac$Year_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#YES


tab11= table(fac$GaragedZIP_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#NO

tab11= table(fac$Make_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)##NO

tab11= table(fac$Model_1, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#YES

tab11= table(fac$towing_cost, fac$ClaimStatus)
chisq.test(tab11)
fisher.test(tab11, simulate.p.value = T)#Yes


fac_1 = fac %>% select(-"CoverageMP",-"CoveragePIP_CDW",-"CoverageUMBI",-"CoverageUMPD",-"Surcharge1Unit_1",-"Surcharge2Unit_1",
                       -"Make_1",-"GaragedZIP_1",-"Zip",-"Amendments_Made")
colnames(fac_1)
summary(fac_1)
fac_1$CoverageLiability[fac_1$CoverageLiability == "None"] = "25/50/25"
fac_1$CoveragePD_1[fac_1$CoveragePD_1 == "1000/1000"] = "None"
summary(fac_1$CoveragePD_1)


fac_1$CoverageLiability=fac_1$CoverageLiability[fac_1$CoverageLiability !="None"]
fac_1$CoverageLiability=droplevels(fac_1$CoverageLiability)
summary(fac_1$CoverageLiability)

fac_1$CoveragePD_1=fac_1$CoveragePD_1[fac_1$CoveragePD_1 !="1000/1000"]
fac_1$CoveragePD_1=droplevels(fac_1$CoveragePD_1)
summary(fac_1$CoveragePD_1)

fac_1 = fac_1 %>% select(-"Year_1", -"Model_1",-"Type")
summary(fac_1)

colnames(num)
num_1 = num %>% select(-"ViolPoints1Driver_2",-"ViolPoints1Driver_5",-"ViolPoints2Driver_1",-"ViolPoints3Driver_1",-"ViolPoints2Driver_3",-"ViolPoints1Driver_1",-"ViolPoints1Driver_3",-"ViolPoints1Driver_4",-"ViolPoints2Driver_2")

############################################
colnames(num_1)
num_1=as.data.frame(scale(num_1))
summary(num_1)

data_1=cbind(fac_1,num_1)
summary(data_1)
data.sc=data_1[,-1]
summary(data.sc)

####################HOt encoding 
library(data.table)
library(mltools)
dummy1=one_hot(as.data.table(data.sc))
summary(dummy1)
str(dummy1)


dummy.scale=dummy1
##########Correlation
mat=cor(dummy.scale)
head(round(mat,2))
library(corrplot)
corrplot(mat,type = "lower",method = "number")
library(psych)
cortest.bartlett(mat, nrow(mat))
KMO(mat)

####################FA
A = eigen(mat)
ev = A$values
ev
##########PCA
Factor=c(1:30)
Scree=data.frame(Factor, ev)
plot(Scree, main="Scree Plot", col="Blue", ylim=c(0,4))
lines(Scree, col="Red")
abline(h=1, col="Green")
Unrotate=principal(dummy.scale,nfactors = 11, rotate="none")
print(Unrotate, digits=2)
Rotate=principal(dummy.scale,nfactors = 11, rotate = "varimax")
print(Rotate, digits = 2)
Rotate$scores

finaldata=cbind(ClaimStatus=data_1$ClaimStatus,Rotate$scores)
finaldata=data.frame(finaldata)
names(finaldata)=c("ClaimStatus","Damage_coverage","Lease_Towing","Surcharge_tax", "Renewal","Vehicle_Details","Loss_Signed","Marital_Status","Gender","Driver_details","Claim_frequency","Liability")
View(finaldata)
#target variable
summary(finaldata$ClaimStatus)
finaldata$ClaimStatus[finaldata$ClaimStatus == 1] = 0
finaldata$ClaimStatus[finaldata$ClaimStatus == 2] = 1
finaldata$ClaimStatus=as.factor(finaldata$ClaimStatus)

library(caTools)
set.seed(123)
spl = sample.split(finaldata$ClaimStatus, SplitRatio = 0.7)
train = subset(finaldata, spl == T)
test = subset(finaldata, spl == F)

dim(train)
dim(test)
prop.table(table(train$ClaimStatus))
table(train$ClaimStatus)
prop.table(table(test$ClaimStatus))

library(DMwR)
smote.train <- SMOTE(ClaimStatus ~., train , perc.over = 300, k = 5, perc.under = 310)
table(smote.train$ClaimStatus)
prop.table(table(smote.train$ClaimStatus))

#######LOGISTIC
library(caret)
model=glm(ClaimStatus~., data=smote.train, family = binomial("logit"), maxit = 100)
summary(model)

model_1=glm(ClaimStatus~Damage_coverage+Surcharge_tax+Renewal+Vehicle_Details+Marital_Status+Gender+Driver_details+Claim_frequency+Liability, data=smote.train, family = binomial("logit"), maxit = 100)
library(car)
vif(model)
summary(model_1)
# Odds Ratio
exp(coef(model))

# Probability
exp(coef(model))/(1+exp(coef(model)))

# Prediction
library(ROCR)
library(pROC)
smote.train_predict=predict(model,newdata=train,type="response")
train_roc=roc(train$ClaimStatus,smote.train_predict)
train_roc$auc
plot(train_roc, colorize=TRUE)
library(pROC)
#Finding best threshold/cut-off
coords(train_roc,"best","threshold")#

pred1= predict(model, data=smote.train, type="response")
y_pred_num = ifelse(pred1>0.26,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = smote.train$ClaimStatus
confusionMatrix(y_pred,y_actual,positive="1")

# Performance metrics (out-of-the-sample)
pred = predict(model, newdata=test, type="response")###ERROR OF 1 level variable
y_pred_num = ifelse(pred>0.26,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = test$ClaimStatus
confusionMatrix(y_pred,y_actual,positive="1")

# ROC plot
library(ROCR)
train.roc <- prediction(pred1, smote.train$ClaimStatus)
plot(performance(train.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for train data")
abline(0, 1, lty = 8, col = "blue")



# AUC
train.auc = performance(train.roc, "auc")
train.area = as.numeric(slot(train.auc, "y.values"))
train.area

# KS
ks.train <- performance(train.roc, "tpr", "fpr")
train.ks <- max(attr(ks.train, "y.values")[[1]] - (attr(ks.train, "x.values")[[1]]))
train.ks

# Gini
train.gini = (2 * train.area) - 1
train.gini

# ROC plot-------TEST
library(ROCR)
test.roc <- prediction(pred, test$ClaimStatus)
plot(performance(test.roc, "tpr", "fpr"), 
     col = "red", main = "ROC Curve for test data")
abline(0, 1, lty = 8, col = "blue")

# AUC
test.auc = performance(test.roc, "auc")
test.area = as.numeric(slot(test.auc, "y.values"))
test.area

# KS
ks.test <- performance(test.roc, "tpr", "fpr")
test.ks <- max(attr(ks.test, "y.values")[[1]] - (attr(ks.test, "x.values")[[1]]))
test.ks

# Gini
test.gini = (2 * test.area) - 1
test.gini



################XGBOOST
library(xgboost)
features_train = as.matrix(smote.train[,-1])
label_train = as.matrix(smote.train[,1])
features_test = as.matrix(test[,-1])



#lets find best fit
tp_xgb<-vector()
lr <- c(0.001, 0.01, 0.1, 0.3, 0.5, 0.7, 1)
md<-c(1,3,5,7,9,15)
nr<-c(2, 50, 100, 1000, 10000)
for (i in nr) {
  
  xgb.fit <- xgboost(
    data = features_train,
    label = label_train,
    eta =0.001,
    max_depth = 5,
    nrounds = i,
    nfold = 5,
    objective = "binary:logistic",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
  )
  
  test$xgb.pred.class <- predict(xgb.fit, features_test)
  
  tp_xgb<-cbind(tp_xgb,sum(test$ClaimStatus==1 & test$xgb.pred.class>=0.5))
  
}


tp_xgb

XGBmodel = xgboost(
  data = features_train,
  label = label_train,
  eta = 0.01,
  max_depth = 15,
  min_child_weight = 7,
  nrounds = 1000,
  nfold = 10,
  objective = "binary:logistic",  # for regression models
  verbose = 0, # silent,
  alpha= 0.5,
  gamma=1,
  early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)


XGBpredtrain=predict(XGBmodel, features_train)
y_pred_num = ifelse(XGBpredtrain >0.5,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
y_actual = smote.train$ClaimStatus
confusionMatrix(y_pred,y_actual,positive="1")


XGBpredTest = predict(XGBmodel, features_test)
y_pred_num = ifelse(XGBpredTest >0.5,1,0)
y_pred = factor(y_pred_num, levels=c(0,1))
confusionMatrix(y_pred,test$ClaimStatus,positive="1")


###################KNN
library(caret)
knn_fit = train(ClaimStatus ~., data = smote.train, method = "knn",
                trControl = trainControl(method = "cv", number = 3),
                tuneLength = 10)

knn_fit$bestTune$k

knn_fit = train(ClaimStatus ~., data = smote.train, method = "knn",
                trControl = trainControl(method = "cv", number = 5),
                tuneLength = 10)

# Performance metrics (out-of-the-sample)
pred = predict(knn_fit, newdata = test[-1], type = "raw")
confusionMatrix(pred,test$ClaimStatus,positive="1")


##################RANDOM Forest
library(randomForest)
library(caret)
library(e1071)
tr_Control=trainControl(method="cv", number=10, search = "grid")
set.seed(1000)
rf_model=train(ClaimStatus~., data=smote.train, method="rf", metric="Accuracy", trControl= tr_Control)
print(rf_model)

#best mtry
set.seed(1000)
tuneGrid=expand.grid(.mtry=c(1:10))
rf_mtry=train(ClaimStatus~., data=smote.train, method="rf", tuneGrid=tuneGrid, trControl=tr_Control, importance= TRUE, nodesize=14, ntree=300)
print(rf_mtry)
best_mtry=rf_mtry$bestTune$mtry
best_mtry

#best maxnodes
store_maxnode=list()
tuneGrid=expand.grid(.mtry=best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1000)
  rf_maxnode <- train(ClaimStatus~.,
                      data = smote.train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = tr_Control,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}

results_mtry=resamples(store_maxnode)
results_mtry
summary(results_mtry)

#try with hgher score
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(20: 30)) {
  set.seed(1234)
  rf_maxnode <- train(ClaimStatus~.,
                      data = smote.train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = tr_Control,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  key <- toString(maxnodes)
  store_maxnode[[key]] <- rf_maxnode
}
results_node <- resamples(store_maxnode)
summary(results_node)

##best mtrees
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(ClaimStatus~.,
                       data = smote.train,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = tr_Control,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 15,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

###final
fit_rf <- train(ClaimStatus~.,
                data=smote.train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = tr_Control,
                importance = TRUE,
                nodesize = 14,
                ntree = 400,
                maxnodes = 20)

predict.train_rf=predict(fit_rf, smote.train)
confusionMatrix(predict.train_rf,smote.train$ClaimStatus,positive="1")

prediction <-predict(fit_rf, test)
confusionMatrix(prediction, test$ClaimStatus, positive = "1")


imp=varImp(fit_rf, scale=FALSE)
imp
plot(imp)









































