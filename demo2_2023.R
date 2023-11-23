   
library(foreign)
dat<-read.spss("demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

#Jatketaan vielä yksisuuntaista varianssianalyysiä


install.packages(c("car", "contrast"))

# hajontojen yhtäsuuruustestaus


library(car)
leveneTest(oma_tulo~taltyyt)

# Yksisuuntainen varianssianalyysi, hajonnat ovat yhtäsuuret

malli1<-lm(oma_tulo~taltyyt)
malli_lm<-aov(malli1)
summary (malli_lm)
 
#Tukey HSD-testi

tukey.test <- TukeyHSD(malli_lm)
tukey.test

plot(tukey.test)

#Siirrytään kahden selittäjän malliin

# keskiarvot soluittain

with(dat,tapply(oma_tulo,list(tyotilan,taltyyt),mean)) 

# mediaanit soluittain

with(dat,tapply(oma_tulo,list(tyotilan,taltyyt),median)) 

# keskihajonnat soluittain

with(dat,tapply(oma_tulo,list(tyotilan,taltyyt),sd)) 



#with(dat,tapply(oma_tulo,list(tyotilan,taltyyt),shapiro.test))
 

#Shapiro-Wilk -testit, työssä olevat
 
install.packages("dplyr")
library(dplyr)
raj.ain1 <- select(filter(dat, tyotilan=="tyossa"),c(tyotilan,taltyyt,oma_tulo))    

with(raj.ain1,tapply(oma_tulo,list(taltyyt),shapiro.test))
attach (raj.ain1)

#Laatikko-jana -kuvio, työssä olevat

boxplot(oma_tulo~taltyyt)
detach (raj.ain1)

#Shapiro-Wilk -testit, työttömät (ei erittäin tyytyväiset)

raj.ain2 <- select(filter(dat, tyotilan=="tyoton" & taltyyt!="erittain tyytyvainen"),c(tyotilan,taltyyt,oma_tulo))    

with(raj.ain2,tapply(oma_tulo,list(taltyyt),shapiro.test))

#Laatikko-jana -kuvio, työttömät

raj.ain3 <- select(filter(dat, tyotilan=="tyoton"),c(tyotilan,taltyyt,oma_tulo))    

attach (raj.ain3)

boxplot( oma_tulo~taltyyt)  
detach (raj.ain3)

# hajontojen yhtäsuuruustestaus

attach(dat)
library(car)
leveneTest(oma_tulo~taltyyt*tyotilan)

#Kaksisuuntainen varianssianalyysi

anova(lm(dat$oma_tulo~taltyyt*tyotilan))   

#Nimetään malli

fit1<-lm(oma_tulo ~taltyyt*tyotilan, data=dat) 
summary(fit1)

#Keskiarvojen profiilikuvio

interaction.plot(taltyyt ,tyotilan, oma_tulo,
                  xlab="Tyytyvaisyys", ylab="Omat tulot",
                  trace.label="Tyotilanne", las=1, lwd=2)  

#Suljetaan data

detach (dat)

dat2<-read.spss("demo2_2015r.sav", to.data.frame=TRUE)
attach(dat2)

# keskiarvot soluittain

with(dat2,tapply(hyl,list(tehdas,vuoro),mean))  

#Shapiro-Wilk -testit, tehdas1

tehdas1.dat <- select(filter(dat2, tehdas=="tehdas1"),c(tehdas,vuoro,hyl))    

with(tehdas1.dat,tapply(hyl,list(vuoro),shapiro.test))
attach (tehdas1.dat)

#Laatikko-jana -kuvio, tehdas1

boxplot(hyl~vuoro)
detach (tehdas1.dat)

#Shapiro-Wilk -testit,tehdas2

tehdas2.dat <- select(filter(dat2, tehdas=="tehdas2"),c(tehdas,vuoro,hyl))    

with(tehdas2.dat,tapply(hyl,list(vuoro),shapiro.test)) 

#Laatikko-jana -kuvio,tehdas2
attach (tehdas2.dat)
boxplot(hyl~vuoro)  
detach (tehdas2.dat)

# hajontojen yhtäsuuruustestaus

attach(dat2)
library(car)
leveneTest(hyl~tehdas*vuoro)

#Kaksisuuntainen varianssianalyysi

anova(lm(hyl~tehdas*vuoro))   

#Nimetään malli

fit2<-lm(hyl ~ tehdas *vuoro, data=dat2) 
malli_lm2<-aov(fit2)
summary (malli_lm2)
#keskiarvojen profiilit

interaction.plot(vuoro ,tehdas, hyl,
                  xlab="Vuoros", ylab="Hylatyt",
                  trace.label="Tehdas", las=1, lwd=2)  


#Tukeyn testi yhdysvaikutukselle

tukey.test2<-TukeyHSD(malli_lm2)
tukey.test2
plot(tukey.test2)

 