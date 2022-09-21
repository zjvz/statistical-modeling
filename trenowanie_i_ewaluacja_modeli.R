library(caTools)
library(corrplot)
library(GGally)
library(scatterplot3d)
library(ggplot2)

#tworzenie modeli sredniej rocznej wielkosci opadow
#przygotowanie danych
getwd()
dane<-read.csv2('weather.csv',sep=';')
dane
str(dane)
dane<-subset(dane,select=-c(id,DWD_ID,STATION.NAME,FEDERAL.STATE,PERIOD))
dane<-na.omit(dane)
dim(dane)

#podzial na zbior uczacy i testowy w sposob losowy
podzial<-sample.split(dane,SplitRatio=4/5) 
trening<-subset(dane,podzial==TRUE)
test<-subset(dane,podzial==FALSE)

#zmienna objasniajaca
y_trening<-trening$MEAN.ANNUAL.RAINFALL
y_test<-test$MEAN.ANNUAL.RAINFALL

x_trening<-subset(trening,select=-c(MEAN.ANNUAL.RAINFALL))
x_test<-subset(test,select=-c(MEAN.ANNUAL.RAINFALL))

corr<-cor(dane)
corr
corrplot(corr)
corrplot(corr,method="number",type="lower")

#model bazowy
model_bazowy<-lm(MEAN.ANNUAL.RAINFALL~1,trening)
model_bazowy$coefficients
mean(dane$MEAN.ANNUAL.RAINFALL)

#blad sredniokwadratowy(RMSE)
RMSE<-sqrt(sum(model_bazowy$residuals^2)/156)

#przewidywanie, blad pomiedzy wartosciami obserwowanymi a przewidywanymi
pre<-predict(model_bazowy,newdata=test)
RMSE2<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre)^2)/48)

#wizualizacja
par(mfrow=c(1,2))
plot(trening$MEAN.ANNUAL.RAINFALL)
abline(model_bazowy)
plot(test$MEAN.ANNUAL.RAINFALL)
abline(model_bazowy)

#model regresji liniowej
model_regresji<-lm(MEAN.ANNUAL.RAINFALL~ALTITUDE,trening)
RMSE3<-sqrt(sum(model_regresji$residuals^2)/156)

pre2<-predict(model_regresji,newdata=test)
RMSE4<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre2)^2)/48)

par(mfrow=c(1,2))
plot(trening$MEAN.ANNUAL.RAINFALL)
abline(model_regresji)
plot(test$MEAN.ANNUAL.RAINFALL)
abline(model_regresji)

#model liniowy z maksymalna uloscia opadow jako zmienna objasniajaca
model_max<-lm(MEAN.ANNUAL.RAINFALL~MAX.RAINFALL,trening)
RMSE5<-sqrt(sum(model_max$residuals^2)/156)

pre3<-predict(model_max,newdata=test)
RMSE6<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre3)^2)/48)

par(mfrow=c(1,2))
plot(trening$MEAN.ANNUAL.RAINFALL)
abline(model_max)
plot(test$MEAN.ANNUAL.RAINFALL)
abline(model_max)

#model regresji wielorakiej
model_wielorakiej<-lm((MEAN.ANNUAL.RAINFALL~MAX.RAINFALL+ALTITUDE),trening)
RMSE7<-sqrt(sum(model_wielorakiej$residuals^2)/156)

pre4<-predict(model_wielorakiej,newdata=test)
RMSE8<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre4)^2)/48)

plot3<-scatterplot3d(test$MEAN.ANNUAL.RAINFALL~test$MAX.RAINFALL+test$ALTITUDE)
plot3$plane3d(model_wielorakiej)
plot33<-scatterplot3d(trening$MEAN.ANNUAL.RAINFALL~trening$MAX.RAINFALL+trening$ALTITUDE)
plot33$plane3d(model_wielorakiej)

#model przy uzyciu wysokosci, logarytmu z wysokosci oraz wysokosci podniesionej do kwadratu
model_log_kwad<-lm((MEAN.ANNUAL.RAINFALL~ALTITUDE+log(ALTITUDE)+ALTITUDE^2),trening)
RMSE9<-sqrt(sum(model_log_kwad$residuals^2)/156)

pre5<-predict(model_log_kwad,newdata=test)
RMSE10<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre4)^2)/48)

#model przy uzyciu max ilosci opadow, logarytmu z max ilosci opadow oraz max ilosci opadow podniesionej do kwadratu
model_max_log_kwad<-lm((MEAN.ANNUAL.RAINFALL~MAX.RAINFALL+log(MAX.RAINFALL)+MAX.RAINFALL^2),trening)
RMSE11<-sqrt(sum(model_max_log_kwad$residuals^2)/156)

pre6<-predict(model_max_log_kwad,newdata=test)
RMSE12<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre4)^2)/48)

#selekcja modeli - forward selection
model_bazowy
zmienne<-as.formula(lm(trening$MEAN.ANNUAL.RAINFALL ~ trening$ALTITUDE +
                         trening$MAX.RAINFALL + trening$MEAN.CLOUD.COVER +
                         trening$MEAN.ANNUAL.AIR.TEMP))
step(model_bazowy,scope=zmienne,direction="forward")

#model wybrany na podstawie powyzszej selekcji 
model_wybrane_f<-lm(MEAN.ANNUAL.RAINFALL~MAX.RAINFALL+MEAN.ANNUAL.AIR.TEMP,trening)
RMSE13<-sqrt(sum(model_wybrane_f$residuals^2)/156)

pre7<-predict(model_wybrane_f,newdata=test)
RMSE14<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre4)^2)/48)

#selekcja modeli - backward selection
model_pocz<-lm(MEAN.ANNUAL.RAINFALL~ALTITUDE+MAX.RAINFALL+MEAN.CLOUD.COVER+MEAN.ANNUAL.AIR.TEMP,trening)
step(model_pocz, direction="backward")

model_wybrane_b<-lm((MEAN.ANNUAL.RAINFALL~MEAN.ANNUAL.AIR.TEMP+MAX.RAINFALL),trening)
RMSE15<-sqrt(sum(model_wybrane_b$residuals^2)/156)

pre7<-predict(model_wybrane_b,newdata=test)
RMSE16<-sqrt(sum((test$MEAN.ANNUAL.RAINFALL-pre4)^2)/48)
  
  #RMSE #bazowy trening
  #RMSE2 #bazowy test
  #RMSE3 
  #RMSE4 #regresja test
  #RMSE5
  #RMSE6 #max test
  #RMSE7
  #RMSE8 #wieloraka test
  #RMSE9
  #RMSE10 #wysokosc log kwadrat test
  #RMSE11
  #RMSE12 #max log kwadrat test
  #RMSE13
  #RMSE14 #forward test
  #RMSE15
  #RMSE16 #backward test

data_test<-data.frame(
  name=c("bazowy","regresja","max","wieloraka", "wysokosc log kwad","max log kwad","forward","backward") ,  
  value=c(RMSE2,RMSE4,RMSE6,RMSE8,RMSE10,RMSE12,RMSE14,RMSE16)
)
data_trening<-data.frame(
  name=c("bazowy","regresja","max","wieloraka","wysokosc log kwad","max log kwad","forward","backward") ,  
  value=c(RMSE,RMSE3,RMSE5,RMSE7,RMSE9,RMSE11,RMSE13,RMSE15)
)
ggplot(data_trening, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")+ggtitle("RMSE-trening")

ggplot(data_test, aes(x=name, y=value)) + 
  geom_bar(stat = "identity")+ggtitle("RMSE-test")
