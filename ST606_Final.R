install.packages("reshape2")
library(tidyverse)
library(fpp3)
library(lubridate)
library(readxl)
library(feasts)
library(bayesforecast)
library(rjags)
library(R2jags)
library(tidybayes)
library(bayesplot)
install.packages("writexl")
library(writexl)
library(reshape2)



###########################

serengeti <- read.table("serengeti_birds_data.txt", header = TRUE) %>%
  as_tibble
#serengeti %>% count(genus) %>% View()

serengeti<-serengeti %>%
  mutate(date = make_date(year, month, day)) 

birds <- serengeti %>%
  group_by(year, IOCEnglishName, family) %>%
  summarise(n = sum(individualCount)) %>%
  ungroup()

#rare species only found in serengeti
rare_bird<-birds %>% filter(IOCEnglishName=="Schalow's Turaco" | IOCEnglishName=="Grey-crested Helmetshrike" | IOCEnglishName=="Rufous-tailed Weaver" | IOCEnglishName=="Red-throated Tit" | IOCEnglishName=="Grey-breasted Spurfowl")
rare_bird_name_tsibble<-as_tsibble(rare_bird,index=year,key=IOCEnglishName)%>% filter(year>1990)
#autoplot(rare_bird_name_tsibble,n)

#most spottable birds in serengeti
common_bird<-birds %>% filter(IOCEnglishName=="Common Ostrich" | IOCEnglishName=="Tawny Eagle" | IOCEnglishName=="Von der Decken's Hornbill" | IOCEnglishName=="White Stork" | IOCEnglishName=="Grey Crowned Crane")
common_bird_name_tsibble<-as_tsibble(common_bird,index=year,key=IOCEnglishName) %>% filter(year>1990)
#autoplot(common_bird_name_tsibble,n)



#The plot shows time series of top 5 rare species only found in serengeti region
rare_bird_name_tsibble %>%
  ggplot(aes(x = year, y = n,color=IOCEnglishName)) +
  geom_line() +
  facet_wrap(~IOCEnglishName, scales = "free_y")

#The plot shows time series of 5 most spottable birds in serengeti region
rare_bird_name_tsibble %>% 
  ggplot(aes(x = year, y = n,color=IOCEnglishName)) +
  geom_line() +
  facet_wrap(~IOCEnglishName, scales = "free_y")

#creating a dataframe with all 10 species and plotting the time series
rare_common_birds<-bind_rows(rare_bird_name_tsibble,common_bird_name_tsibble)
rare_common_birds %>% 
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~IOCEnglishName, scales = "free_y")+ 
  labs(title="Time series plot of abundance of species from 1990-2017",y = "No of birds observed")+
  guides(colour = guide_legend(title = "Bird Species"))

#taking log transform of the counts to stabilize variance
rare_common_birds_log<-rare_common_birds %>% mutate(logn=log(n))
rare_common_birds_log %>% 
  ggplot(aes(x = year, y = logn)) +
  geom_line() +
  facet_wrap(~IOCEnglishName, scales = "free_y") + 
  labs(title="Time series plot of abundance of species from 1990-2017",y = "log(No of birds observed)")+
  guides(colour = guide_legend(title = "Bird Species"))



rare_common_birds %>% 
  ggplot(aes(x = year, y = n)) + theme_bw() +
  geom_point(cex = .8, alpha = .5) +
  facet_wrap(~ IOCEnglishName, scales = "free_y") +
  geom_smooth()



#to fill the gaps in year with NA values
rare_common_birds_log_Exp <- tsibble::fill_gaps(rare_common_birds_log) 

#change the Na to 0 for logn
rare_common_birds_log_Exp<-rare_common_birds_log_Exp %>% 
                        mutate(lognn=ifelse(is.na(rare_common_birds_log_Exp$logn),
                            0,rare_common_birds_log_Exp$logn))

rare_common_birds_log_Exp %>% 
  ggplot(aes(x = year, y = logn,color=IOCEnglishName)) +
  geom_line() +
  facet_wrap(~IOCEnglishName, scales = "free_y") + 
  labs(title="Time series plot of abundance of species from 1990-2017",y = "log(No of birds observed)")+
  guides(colour = guide_legend(title = "Bird Species"))


rare_common_birds_log_Exp %>% 
  ggplot(aes(x = year, y = lognn,color=IOCEnglishName)) +
  geom_line() +
  facet_wrap(~IOCEnglishName, scales = "free_y")


#Exploratory Data Anlaysis

#correlation plot
library(corrplot)


rare_common_birds_log_Short <- subset( rare_common_birds_log, select = -c(3, 5 ) )
cor_sum_pivot<-pivot_wider(as_tibble(rare_common_birds_log_Short),
                           values_from= n, names_from=IOCEnglishName)

cor_sum_pivot %>% view()
cor_sum_pivot[is.na(cor_sum_pivot)] <- 0

GGally::ggpairs(cor_sum_pivot,columns = 2:11) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

cor_sum_pivot.cor <- cor(cor_sum_pivot[,2:11]) #matrix
typeof(cor_sum_pivot.cor)
View(cor_sum_pivot.cor)
library("writexl")
corrplot(cor_sum_pivot.cor,is.corr = FALSE,type="lower",diag=FALSE,order = c("original"),tl.col = "black",tl.cex = 0.7)

source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(cor_sum_pivot)

####################################3

#ARIMA model, non-seasonal

arima_fit1 <- rare_common_birds_log_Exp %>%
  filter(IOCEnglishName == "Schalow's Turaco") %>%
  model(ARIMA(lognn))
arima_fit1
report(arima_fit1)

arima_fit2 <- rare_common_birds_log_Exp %>% 
  model(ARIMA(lognn))
arima_fit2

augment(arima_fit2) %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

report(arima_fit2 %>% filter(IOCEnglishName == "Common Ostrich"))
report(arima_fit2 %>% filter(IOCEnglishName == "Grey-breasted Spurfowl"))
report(arima_fit2 %>% filter(IOCEnglishName == "Grey-crested Helmetshrike"))
report(arima_fit2 %>% filter(IOCEnglishName == "Grey Crowned Crane"))
report(arima_fit2 %>% filter(IOCEnglishName == "Red-throated Tit"))
report(arima_fit2 %>% filter(IOCEnglishName == "Rufous-tailed Weaver"))
report(arima_fit2 %>% filter(IOCEnglishName == "Schalow's Turaco"))
report(arima_fit2 %>% filter(IOCEnglishName == "Tawny Eagle"))
report(arima_fit2 %>% filter(IOCEnglishName == "Von der Decken's Hornbill"))
report(arima_fit2 %>% filter(IOCEnglishName == "White Stork"))
#arima(1,0,0) 

##ACF and PACF plots

rare_common_birds_log_Exp %>%
  filter(IOCEnglishName == "Tawny Eagle") %>%
  ACF(lognn) %>%
  autoplot() +labs(subtitle = "ACF plot for Abundance of Tawny Eagle")

rare_common_birds_log_Exp %>%
  filter(IOCEnglishName == "Tawny Eagle") %>%
  PACF(lognn) %>%
  autoplot()+labs(subtitle = "PACF plot for Abundance of Tawny Eagle")

rare_common_birds_log_Exp %>%
  filter(IOCEnglishName == "White Stork") %>%
  ACF(lognn) %>%
  autoplot()
##only one spike outside the threshold

rare_common_birds_log_Exp %>%
  filter(IOCEnglishName == "Schalow's Turaco") %>%
  PACF(lognn) %>%
  autoplot()

rare_common_birds_log_Exp %>%
  filter(IOCEnglishName == "White Stork") %>%
  model(ARIMA(lognn)) %>%
  forecast(h=10) %>%
  autoplot(rare_common_birds_log_Exp)+labs(subtitle = "Forecasts of White Stork")
##########################################################


#Dynamic regression model
fit_dm <- rare_common_birds_log_Exp %>%
  model(tslm=ARIMA(lognn~trend()+ pdq(0,1,0)))

tidy(fit_dm)
report(fit_dm %>% filter(IOCEnglishName == "Common Ostrich"))
#yt-yt-1=-.0234 + et 
#yt=y0-0.0234t+nt (1+trend for intercept)
#nt=nt-1+et
#??t???NID(3.181)
#there is decline in abundance of 2.34% of common ostrich
report(fit_dm %>% filter(IOCEnglishName == "Grey-breasted Spurfowl"))
report(fit_dm %>% filter(IOCEnglishName == "Grey-crested Helmetshrike"))
report(fit_dm %>% filter(IOCEnglishName == "Grey Crowned Crane"))
report(fit_dm %>% filter(IOCEnglishName == "Red-throated Tit"))
report(fit_dm %>% filter(IOCEnglishName == "Rufous-tailed Weaver"))
report(fit_dm %>% filter(IOCEnglishName == "Schalow's Turaco"))
report(fit_dm %>% filter(IOCEnglishName == "Tawny Eagle"))
report(fit_dm %>% filter(IOCEnglishName == "Von der Decken's Hornbill"))
report(fit_dm %>% filter(IOCEnglishName == "White Stork"))
#fit_dm %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Common Ostrich") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Grey-breasted Spurfowl") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Grey-crested Helmetshrike") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Grey Crowned Crane") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Red-throated Tit") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Rufous-tailed Weaver") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Schalow's Turaco") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Tawny Eagle") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "Von der Decken's Hornbill") %>% gg_tsresiduals()
fit_dm %>% filter(IOCEnglishName == "White Stork") %>% gg_tsresiduals()

#The variance is a bit low after  2010. Model has some significant autocorrelation in the residuals.
#The histogram shows some tails.

fit_dm %>%
  forecast(h = 10) %>%
  autoplot(rare_common_birds_log_Exp)


##########################################################


#VAR using Bayesian model

b1<- rare_common_birds_log %>% filter(IOCEnglishName=="White Stork") %>% fill_gaps #24


b2<-rare_common_birds_log %>% filter(IOCEnglishName=="Schalow's Turaco")%>% fill_gaps #27
b3<- rare_common_birds_log %>% filter(IOCEnglishName=="Red-throated Tit") %>% fill_gaps #23
b4<-rare_common_birds_log %>% filter(IOCEnglishName=="Grey-breasted Spurfowl") %>% fill_gaps #25
b5<- rare_common_birds_log %>% filter(IOCEnglishName=="Rufous-tailed Weaver") %>% fill_gaps #23
b6<-rare_common_birds_log %>% filter(IOCEnglishName=="Grey-crested Helmetshrike") %>% fill_gaps #15
b7<- rare_common_birds_log %>% filter(IOCEnglishName=="Grey Crowned Crane") %>% fill_gaps #25
b8<-rare_common_birds_log %>% filter(IOCEnglishName=="Common Ostrich") %>% fill_gaps #24
b9<- rare_common_birds_log %>% filter(IOCEnglishName=="Tawny Eagle") %>% fill_gaps #26
b10<-rare_common_birds_log %>% filter(IOCEnglishName=="Von der Decken's Hornbill") %>% fill_gaps #24

y1<-matrix(c(c(NA,b1$logn,NA,NA),b2$logn,c(NA,b3$logn,NA,NA,NA),c(NA,b4$logn,NA),
             c(NA,b5$logn,NA,NA,NA),c(NA,b6$logn,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),c(NA,b3$logn,NA),
             c(NA,b8$logn,NA,NA),c(NA,b9$logn),c(NA,b10$logn,NA,NA)),ncol=10,byrow=FALSE)

poismodel1 = "
model{
#likelihood
mu[1,1] = y1[1,1]
mu[1,2] = y1[1,2]
mu[1,3] = y1[1,3]
mu[1,4] = y1[1,4]
mu[1,5] = y1[1,5]
mu[1,6] = y1[1,6]
mu[1,7] = y1[1,7]
mu[1,8] = y1[1,8]
mu[1,9] = y1[1,9]
mu[1,10] = y1[1,10]

for(t in 2:(T)) {

  y1[t,1] ~ dnorm(mu[t,1], tau[1])
  mu[t,1] = beta0[1] + beta1[1]*t + phi11*y1[t-1,1] + phi12*y1[t-1,2] +phi13*y1[t-1,3] + phi14*y1[t-1,4]+phi15*y1[t-1,5]+phi16*y1[t-1,6]+phi17*y1[t-1,7]+phi18*y1[t-1,8]+phi19*y1[t-1,9]+phi1.10*y1[t-1,10]
  
  y1[t,2] ~ dnorm(mu[t,2], tau[2])
  mu[t,2] = beta0[2] + beta1[2]*t + phi21*y1[t-1,1] + phi22*y1[t-1,2] +phi23*y1[t-1,3] + phi24*y1[t-1,4]+phi25*y1[t-1,5]+phi26*y1[t-1,6]+phi27*y1[t-1,7]+phi28*y1[t-1,8]+phi29*y1[t-1,9]+phi2.10*y1[t-1,10]
  
   y1[t,3] ~ dnorm(mu[t,3], tau[3])
  mu[t,3] = beta0[3] + beta1[3]*t + phi31*y1[t-1,1] + phi32*y1[t-1,2] +phi33*y1[t-1,3] + phi34*y1[t-1,4]+phi35*y1[t-1,5]+phi36*y1[t-1,6]+phi37*y1[t-1,7]+phi38*y1[t-1,8]+phi39*y1[t-1,9]+phi3.10*y1[t-1,10]
  
   y1[t,4] ~ dnorm(mu[t,4], tau[4])
  mu[t,4] = beta0[4] + beta1[4]*t + phi41*y1[t-1,1] + phi42*y1[t-1,2] +phi43*y1[t-1,3] + phi44*y1[t-1,4]+phi45*y1[t-1,5]+phi46*y1[t-1,6]+phi47*y1[t-1,7]+phi48*y1[t-1,8]+phi49*y1[t-1,9]+phi4.10*y1[t-1,10]
  
   y1[t,5] ~ dnorm(mu[t,5], tau[5])
  mu[t,5] = beta0[5] + beta1[5]*t + phi51*y1[t-1,1] + phi52*y1[t-1,2] +phi53*y1[t-1,3] + phi54*y1[t-1,4]+phi55*y1[t-1,5]+phi56*y1[t-1,6]+phi57*y1[t-1,7]+phi58*y1[t-1,8]+phi59*y1[t-1,9]+phi5.10*y1[t-1,10]
  
   y1[t,6] ~ dnorm(mu[t,6], tau[6])
  mu[t,6] = beta0[6] + beta1[6]*t + phi61*y1[t-1,1] + phi62*y1[t-1,2] +phi63*y1[t-1,3] + phi64*y1[t-1,4]+phi65*y1[t-1,5]+phi66*y1[t-1,6]+phi67*y1[t-1,7]+phi68*y1[t-1,8]+phi69*y1[t-1,9]+phi6.10*y1[t-1,10]
  
   y1[t,7] ~ dnorm(mu[t,7], tau[7])
  mu[t,7] = beta0[7] + beta1[7]*t + phi71*y1[t-1,1] + phi72*y1[t-1,2] +phi73*y1[t-1,3] + phi74*y1[t-1,4]+phi75*y1[t-1,5]+phi76*y1[t-1,6]+phi77*y1[t-1,7]+phi78*y1[t-1,8]+phi79*y1[t-1,9]+phi7.10*y1[t-1,10]
  
   y1[t,8] ~ dnorm(mu[t,8], tau[8])
  mu[t,8] = beta0[8] + beta1[8]*t + phi81*y1[t-1,1] + phi82*y1[t-1,2] +phi83*y1[t-1,3] + phi84*y1[t-1,4]+phi85*y1[t-1,5]+phi86*y1[t-1,6]+phi87*y1[t-1,7]+phi88*y1[t-1,8]+phi89*y1[t-1,9]+phi8.10*y1[t-1,10]
  
   y1[t,9] ~ dnorm(mu[t,9], tau[9])
  mu[t,9] = beta0[9] + beta1[9]*t + phi91*y1[t-1,1] + phi92*y1[t-1,2] +phi93*y1[t-1,3] + phi94*y1[t-1,4]+phi95*y1[t-1,5]+phi96*y1[t-1,6]+phi97*y1[t-1,7]+phi98*y1[t-1,8]+phi99*y1[t-1,9]+phi9.10*y1[t-1,10]
  
   y1[t,10] ~ dnorm(mu[t,10], tau[10])
  mu[t,10] = beta0[10] + beta1[10]*t + phi101*y1[t-1,1] + phi102*y1[t-1,2] +phi103*y1[t-1,3] + phi104*y1[t-1,4]+phi105*y1[t-1,5]+phi106*y1[t-1,6]+phi107*y1[t-1,7]+phi108*y1[t-1,8]+phi109*y1[t-1,9]+phi10.10*y1[t-1,10]
  
}


# priors

for(i in 1:10) {
    beta0[i] ~ dnorm(0, .001)
    beta1[i] ~ dnorm(0, .001)
    
    tau[i] <- 1/sigma[i]^2
    sigma[i] ~ dt(0, 10^-2, 1)T(0,)
}
  
phi11 ~ dunif(-1,1)
phi13 ~ dunif(-1,1)
phi12 ~ dunif(-1,1)
phi14 ~ dunif(-1,1)
phi15 ~ dunif(-1,1)
phi16 ~ dunif(-1,1)
phi17 ~ dunif(-1,1)
phi18 ~ dunif(-1,1)
phi19 ~ dunif(-1,1)
phi1.10 ~ dunif(-1,1)

phi21 ~ dunif(-1,1)
phi22 ~ dunif(-1,1)
phi23 ~ dunif(-1,1)
phi24 ~ dunif(-1,1)
phi25 ~ dunif(-1,1)
phi26 ~ dunif(-1,1)
phi27 ~ dunif(-1,1)
phi28 ~ dunif(-1,1)
phi29 ~ dunif(-1,1)
phi2.10 ~ dunif(-1,1)

phi31 ~ dunif(-1,1)
phi32 ~ dunif(-1,1)
phi33 ~ dunif(-1,1)
phi34 ~ dunif(-1,1)
phi35 ~ dunif(-1,1)
phi36 ~ dunif(-1,1)
phi37 ~ dunif(-1,1)
phi38 ~ dunif(-1,1)
phi39 ~ dunif(-1,1)
phi3.10 ~ dunif(-1,1)


phi41 ~ dunif(-1,1)
phi42 ~ dunif(-1,1)
phi43 ~ dunif(-1,1)
phi44 ~ dunif(-1,1)
phi45 ~ dunif(-1,1)
phi46 ~ dunif(-1,1)
phi47 ~ dunif(-1,1)
phi48 ~ dunif(-1,1)
phi49 ~ dunif(-1,1)
phi4.10 ~ dunif(-1,1)


phi51 ~ dunif(-1,1)
phi52 ~ dunif(-1,1)
phi53 ~ dunif(-1,1)
phi54 ~ dunif(-1,1)
phi55 ~ dunif(-1,1)
phi56 ~ dunif(-1,1)
phi57 ~ dunif(-1,1)
phi58 ~ dunif(-1,1)
phi59 ~ dunif(-1,1)
phi5.10 ~ dunif(-1,1)

phi61 ~ dunif(-1,1)
phi62 ~ dunif(-1,1)
phi63 ~ dunif(-1,1)
phi64 ~ dunif(-1,1)
phi65 ~ dunif(-1,1)
phi66 ~ dunif(-1,1)
phi67 ~ dunif(-1,1)
phi68 ~ dunif(-1,1)
phi69 ~ dunif(-1,1)
phi6.10 ~ dunif(-1,1)

phi71 ~ dunif(-1,1)
phi72 ~ dunif(-1,1)
phi73 ~ dunif(-1,1)
phi74 ~ dunif(-1,1)
phi75 ~ dunif(-1,1)
phi76 ~ dunif(-1,1)
phi77 ~ dunif(-1,1)
phi78 ~ dunif(-1,1)
phi79 ~ dunif(-1,1)
phi7.10 ~ dunif(-1,1)

phi81 ~ dunif(-1,1)
phi82 ~ dunif(-1,1)
phi83 ~ dunif(-1,1)
phi84 ~ dunif(-1,1)
phi85 ~ dunif(-1,1)
phi86 ~ dunif(-1,1)
phi87 ~ dunif(-1,1)
phi88 ~ dunif(-1,1)
phi89 ~ dunif(-1,1)
phi8.10 ~ dunif(-1,1)

phi91 ~ dunif(-1,1)
phi92 ~ dunif(-1,1)
phi93 ~ dunif(-1,1)
phi94 ~ dunif(-1,1)
phi95 ~ dunif(-1,1)
phi96 ~ dunif(-1,1)
phi97 ~ dunif(-1,1)
phi98 ~ dunif(-1,1)
phi99 ~ dunif(-1,1)
phi9.10 ~ dunif(-1,1)

phi101 ~ dunif(-1,1)
phi102 ~ dunif(-1,1)
phi103 ~ dunif(-1,1)
phi104 ~ dunif(-1,1)
phi105 ~ dunif(-1,1)
phi106 ~ dunif(-1,1)
phi107 ~ dunif(-1,1)
phi108 ~ dunif(-1,1)
phi109 ~ dunif(-1,1)
phi10.10 ~ dunif(-1,1)

}
"


jags.data <- list(y1 = y1[-c(1:10),], 
                  T = 17)


parnames <- c("mu","beta0","beta1","sigma",
              "phi11","phi13","phi12","phi14","phi15","phi16","phi17","phi18","phi19","phi1.10",
              "phi21","phi22","phi23","phi24","phi25","phi26","phi27","phi28","phi29","phi2.10",
              "phi31","phi32","phi33","phi34","phi35","phi36","phi37","phi38","phi39","phi3.10",
              "phi41","phi42","phi43","phi44","phi45","phi46","phi47","phi48","phi49","phi4.10",
              "phi51","phi52","phi53","phi54","phi55","phi56","phi57","phi58","phi59","phi5.10",
              "phi61","phi62","phi63","phi64","phi65","phi66","phi67","phi68","phi69","phi6.10",
              "phi71","phi72","phi73","phi74","phi75","phi76","phi77","phi78","phi79","phi7.10",
              "phi81","phi82","phi83","phi84","phi85","phi86","phi87","phi88","phi89","phi8.10",
              "phi91","phi92","phi93","phi94","phi95","phi96","phi97","phi98","phi99","phi9.10",
              "phi101","phi102","phi103","phi104","phi105","phi106","phi107","phi108","phi109","phi10.10")


## Run the model
mod <- jags(data = jags.data, 
            parameters.to.save=parnames, 
            model.file = textConnection(poismodel1),
            n.iter=100000,
            n.burnin = 10000,
            n.thin = 120)

plot(mod)
p<-print(mod)
View(p)
View(p[["summary"]])
m <- mod$BUGSoutput$sims.matrix
View(m)

reg_summary <- m %>% 
  gather_rvars(mu[1:20,b=1:10]) %>% 
  median_qi(.value)

z<-gather_rvars(m,mu[1:20,1:17]) %>% 
  median_qi(.value)
View(z)

z <- m %>% 
  gather_rvars(mu[,1:10]) %>% 
  median_qi(.value) %>% 
  mutate(         y1 = jags.data$y1)




reg_summary1<- reg_summary %>% mutate(x = reg_summary$`1:20`,
                                      id = reg_summary$`1:10`)
reg_summary2<-reg_summary1 %>% mutate(y1=jags.data$y1[11:27,])
y1[1,1]
reg_summary1$Name <- ifelse(reg_summary1$id==1,'White Stork',
                            ifelse(reg_summary1$id==2,'Schalows Turaco',
                                   ifelse(reg_summary1$id==3,'Red-throated Tit',
                                          ifelse(reg_summary1$id==4,'Grey-breasted Spurfowl',
                                                 ifelse(reg_summary1$id==5,'Rufous-tailed Weaver',
                                                        ifelse(reg_summary1$id==6,'Grey-crested Helmetshrike',
                                                               ifelse(reg_summary1$id==7,'Grey Crowned Crane',
                                                                      ifelse(reg_summary1$id==8,'Common Ostrich',
                                                                             ifelse(reg_summary1$id==9,'Tawny Eagle','Von der Deckens Hornbill')))))))))


5-2006
10-2011
15-2016
reg_summary1$year<-ifelse(reg_summary1$x==1,'2002',
                          ifelse( reg_summary1$x==2,'2003',
                                  ifelse(reg_summary1$x==3,'2004',
                                         ifelse(reg_summary1$x==4,'2005',
                                                ifelse(reg_summary1$x==5,'2006',
                                                       ifelse(reg_summary1$x==6,'2007',
                                                              ifelse(reg_summary1$x==7,'2008',
                                                                     ifelse(reg_summary1$x==8,'2009',
                                                                            ifelse(reg_summary1$x==9,'2010', 
                                                                                   ifelse(reg_summary1$x==10,'2011',
                                   ifelse(reg_summary1$x==11,'2012',
                                          ifelse(reg_summary1$x==12,'2013',
                                                 ifelse(reg_summary1$x==13,'2014',
                                                        ifelse(reg_summary1$x==14,'2015',
                                 ifelse(reg_summary1$x==15,'2016',
                                        ifelse(reg_summary1$x==16,'2017','2018'))))))))))))))))

reg_summary1$year1<-ifelse(reg_summary1$x==5,'2006',
                     ifelse( reg_summary1$x==10,'2011',
                             ifelse(reg_summary1$x==15,'2016',reg_summary1$x)))
View(reg_summary1) 
ggplot(reg_summary1,aes(x = year, y = .value,group=1),alpha = 0.3) +
  geom_line() +geom_point()+
  geom_ribbon(data = reg_summary1,aes(ymin = .lower, ymax = .upper), alpha = 0.4) +facet_wrap(~Name)+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +ylab('Posterior Mean') + xlab('Year')


ggplot() + 
  geom_line(reg_summary1,aes(x = year, y = .value),alpha = 0.3) + 
  geom_point(data=y2, color='red') +
  geom_ribbon(data = reg_summary1,aes(ymin = .lower, ymax = .upper), alpha = 0.4) +facet_wrap(~Name)+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +ylab('Posterior Mean') + xlab('Year')
ggplot(y2)
longData<-melt(y2)
longData<-longData[longData$value!=0,]
ggplot(longData, aes(x = Var2, y = Var1)) + geom_point()
y1[11:27,]
y2<-y1[11:27,1:10]
colnames(y2)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10")
rownames(y2)<-c("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10",
                "y11","y12","y13","y14","y15","y16","y17")
#phi<-m[,192:291]
typeof(y2)
y2<-as.data.frame(y2)
y2[x1:x10,]

phi<-p[["summary"]]
View(phi)
var<-phi[1:20,]
sigma<-phi[292:301,]


phi1_10<-t(phi[192,1])
phi1_9<-t(phi[203:211,1])
phi1<-matrix(c(phi1_9,phi1_10),nrow=1,ncol=10)
phi2<-matrix(c(t(phi[213:221,1]),t(phi[212,1])),nrow=1,ncol=10)

phi3<-matrix(c(t(phi[223:231,1]),t(phi[222,1])),nrow=1,ncol=10)

phi4<-matrix(c(t(phi[233:241,1]),t(phi[232,1])),nrow=1,ncol=10)

phi5<-matrix(c(t(phi[243:251,1]),t(phi[242,1])),nrow=1,ncol=10)

phi6<-matrix(c(t(phi[253:261,1]),t(phi[252,1])),nrow=1,ncol=10)

phi7<-matrix(c(t(phi[263:271,1]),t(phi[262,1])),nrow=1,ncol=10)

phi8<-matrix(c(t(phi[273:281,1]),t(phi[272,1])),nrow=1,ncol=10)

phi9<-matrix(c(t(phi[283:291,1]),t(phi[282,1])),nrow=1,ncol=10)

phi10<-matrix(c(t(phi[194:202,1]),t(phi[193,1])),nrow=1,ncol=10)

phi_matrix<-matrix(c(phi1,phi2,phi3,phi4,phi5,phi6,phi7,phi8,phi9,phi10),nrow=10,ncol=10)
rownames(phi_matrix) <- c(":phi[1]",":phi[2]",":phi[3]",":phi[4]",":phi[5]",":phi[6]",":phi[7]",":phi[8]",":phi[9]",":phi[10]")
colnames(phi_matrix) <- c(":phi[1]",":phi[2]",":phi[3]",":phi[4]",":phi[5]",":phi[6]",":phi[7]",":phi[8]",":phi[9]",":phi[10]")
write_xlsx(as.data.frame(phi_matrix), "D:\\MU\\Sem2\\Summer_Project\\images\\phi_matrix.xlsx")
write_xlsx(as.data.frame(var), "D:\\MU\\Sem2\\Summer_Project\\images\\var.xlsx")
write_xlsx(as.data.frame(sigma), "D:\\MU\\Sem2\\Summer_Project\\images\\var_sigma.xlsx")

#?matrix
library(Matrix)
install.packages("corrplot")
library(corrplot)
devtools::install_github('cran/ggplot2')
#phi_matrix <- matrix(rnorm(100), ncol = 10)

#image(Matrix(phi_matrix),xlab=colnames(phi_matrix),main = "Phi matrix") 
corrplot(phi_matrix,method = "number",is.corr = FALSE,diag=FALSE,order = c("original"))
?corrplot
exp(-3.1)
exp(10)
exp(19)
###############################################################

?citation
citation()
