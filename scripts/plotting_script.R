### Plotting script for Hudgins et al. 2022, A brighter future? Stable and growing sea turtle populations in the Republic of Maldives

##code written by Emma J. Hudgins
#emma.hudgins@carleton.ca
#summary table - green turtles
greens<-read.csv('./data/greens-Table 1.csv')
hawks<-read.csv('./data/hawksbills-Table 1.csv')

# install.packages('ggplot2')
# install.packages('paletteer')
library(ggplot2)
library(paletteer)

greens$Site<-as.factor(greens$Site)
hawks$Site<-as.factor(hawks$Site)
hawks$Site<-factor(hawks$Site, levels=(unique(c(levels(greens$Site), levels(hawks$Site)))))
greens$Site<-factor(greens$Site, levels=levels(hawks$Site))


pop_plot<-ggplot(greens, aes(x=Period,y=Updated, col=Site))+theme_classic()+
  scale_fill_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_line()+
  geom_ribbon(aes(ymin=Updated, ymax=ucl, fill=Site), alpha=0.25, outline.type="lower")+
  geom_ribbon(aes(ymax=Updated, ymin=lcl, fill=Site), alpha=0.25, outline.type="upper")+
  scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  xlab("Time Period")+
  ylab("Estimated population size (number of individuals)")+
  scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))

pop_plot

pop_plot2<-ggplot(greens, aes(x=Period,y=Updated, col=Site))+theme_classic()+scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_line(size=1.5)+
   xlab("Time Period")+
  ylab("Estimated population size (number of individuals)")+  scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))
pop_plot2

greens$percent<-(greens$longterm-1)*100
greens$lcl_percent<-(greens$lcl_longterm-1)*100
greens$ucl_percent<-(greens$ucl_longterm-1)*100

greens_sub<-subset(greens, is.na(percent)==F)
growth_plot<-ggplot(greens_sub, aes(y=percent, x=Site))+
  theme_classic()+
  scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_point(aes(y=percent,col=Site), show.legend=F)+
  geom_pointrange(aes(ymin=percent, ymax=ucl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_pointrange(aes(ymax=percent, ymin=lcl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_line(aes(y=cv*100, x=1:6), col='red', size=1.25)+
  geom_ribbon(aes(ymin=cv*100, ymax=cv_ucl*100, x=1:6), fill="red",alpha=0.25, show.legend = F, outline.type="upper")+
  geom_ribbon(aes(ymax=cv*100, ymin=cv_lcl*100, x=1:6), fill="red", alpha=0.25, show.legend = F, outline.type="lower")+
  geom_hline(aes(yintercept=0), col='red', lty=2)+
  xlab("Site")+
  scale_y_continuous(
    name = "Multiyear population growth rate (% per year)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.01, name="Coefficient of variation (year^2)")
  )
growth_plot

#summary table - hawksbills

pop_plot<-ggplot(hawks, aes(x=Period,y=Updated, col=Site))+theme_classic()+
  scale_fill_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_line()+
  geom_ribbon(aes(ymin=Updated, ymax=ucl, fill=Site), alpha=0.25, outline.type="lower")+
  geom_ribbon(aes(ymax=Updated, ymin=lcl, fill=Site), alpha=0.25, outline.type="upper")+
  scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  xlab("Time Period")+
  ylab("Estimated population size (number of individuals)")+
  scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))

pop_plot

pop_plot2<-ggplot(hawks, aes(x=Period,y=Updated, col=Site))+theme_classic()+scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_line(size=1.5)+
  xlab("Time Period")+
  ylab("Estimated population size (number of individuals)")+  scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))
pop_plot2

hawks$percent<-(hawks$longterm-1)*100
hawks$lcl_percent<-(hawks$lcl_longterm-1)*100
hawks$ucl_percent<-(hawks$ucl_longterm-1)*100

hawks_sub<-subset(hawks, is.na(percent)==F)
growth_plot<-ggplot(hawks_sub, aes(y=percent, x=Site))+
  theme_classic()+
  scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_point(aes(y=percent,col=Site), show.legend=F)+
  geom_pointrange(aes(ymin=percent, ymax=ucl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_pointrange(aes(ymax=percent, ymin=lcl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_line(aes(y=cv*100, x=1:6), col='red', size=1.25)+
  geom_ribbon(aes(ymin=cv*100, ymax=cv_ucl*100, x=1:6), fill="red",alpha=0.25, show.legend = F, outline.type="upper")+
  geom_ribbon(aes(ymax=cv*100, ymin=cv_lcl*100, x=1:6), fill="red", alpha=0.25, show.legend = F, outline.type="lower")+
  geom_hline(aes(yintercept=0), col='red', lty=2)+
  xlab("Site")+
  scale_y_continuous(
    name = "Multiyear population growth rate (% per year)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.01, name="Coefficient of variation (year^2)")
  )
growth_plot



survival<-read.csv('./data/survival.csv')
survival_gr<-subset(survival, Species=="Greens")
survival_gr_plot<-ggplot(survival_gr, aes(y=Survival_annual, x=Site, col=Site))+
  theme_classic()+
  scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_point(aes(y=Survival_annual, x=Site , col=Site), show.legend=F,size=3)+
  geom_linerange(aes(ymin=Survival_annual, ymax=Survival_annual_ucl, col=Site), alpha=0.5, show.legend = F, size=1.25)+
  geom_linerange(aes(ymax=Survival_annual, ymin=Survival_annual_lcl,  col=Site), alpha=0.5, show.legend = F, size=1.25)+
  xlab("Site")+
  scale_y_continuous( name = "Estimated annual survival (% per year)")+
  scale_shape_discrete()
survival_gr_plot


survival_hk<-subset(survival, Species=="Hawksbills")
survival_hk_plot<-ggplot(survival_hk, aes(y=Survival_annual, x=Site, col=Site))+
  theme_classic()+
  scale_colour_paletteer_d("colorBlindness::paletteMartin",drop=F)+
  geom_point(aes(y=Survival_annual, x=Site , col=Site), show.legend=F,size=3)+
  geom_linerange(aes(ymin=Survival_annual, ymax=Survival_annual_ucl, col=Site), alpha=0.5, show.legend = F, size=1.25)+
  geom_linerange(aes(ymax=Survival_annual, ymin=Survival_annual_lcl,  col=Site), alpha=0.5, show.legend = F, size=1.25)+
  xlab("Site")+
  scale_y_continuous( name = "Estimated annual survival (% per year)")+
  scale_shape_discrete()
survival_hk_plot

mean(survival_gr$Survival_annual)
mean(survival_gr$Survival_annual_lcl)
mean(survival_gr$Survival_annual_ucl)

mean(survival_hk$Survival_annual)
mean(survival_hk$Survival_annual_lcl)
mean(survival_hk$Survival_annual_ucl)
greens<-read.csv('data/gr_covid.csv')
greens2<-read.csv('data/greens-Table 1.csv')
unique(greens2$Site)
unique(greens$Site)
greens<-subset(greens, Site%in%c('Kuredu Caves', 'Kuredu Express','Kuredu House Reef', 'Hithadhoo West', 'Hithadhoo Corner', 'Six Senses House Reef'))
greens$det_16<-rowSums(greens[,6:17], na.rm=T)
greens$det_17<-rowSums(greens[,18:29], na.rm=T)
greens$det_18<-rowSums(greens[,30:41], na.rm=T)
greens$det_19<-rowSums(greens[,42:53], na.rm=T)
length(which(rowSums(greens[,6:53],  na.rm=T)==0))
length(which(rowSums(greens[,6:53],  na.rm=T)>0))

length(which(greens$det_16>0))
length(which(greens$det_17>0))
length(which(greens$det_18>0))
length(which(greens$det_19>0))



hk<-read.csv('data/hk_covid.csv')
hk2<-read.csv('data/hawksbills-Table 1.csv')
hk<-subset(hk,Site%in%c( "Dhidhdhoo Dhigurah", "Dhonfanu", "Bodu Hithi House Reef", "Bodu Hithi Turtle Point", "Hithadhoo Corner", "Six Senses House Reef"))
unique(hk$Site)
length(which(rowSums(hk[,6:53],  na.rm=T)==0))
length(which(rowSums(hk[,6:53],  na.rm=T)>0))


hk$det_16<-rowSums(hk[,6:17], na.rm=T)
hk$det_17<-rowSums(hk[,18:29], na.rm=T)
hk$det_18<-rowSums(hk[,30:41], na.rm=T)
hk$det_19<-rowSums(hk[,42:53], na.rm=T)
length(which(hk$det_16>0))
length(which(hk$det_17>0))
length(which(hk$det_18>0))
length(which(hk$det_19>0))

hk_nyr<-as.numeric(hk$det_16>0)+as.numeric(hk$det_17>0)+as.numeric(hk$det_18>0)+as.numeric(hk$det_19>0)
length(which(hk_nyr==1))/325
length(which(hk_nyr==2))/325
length(which(hk_nyr==3))/325
length(which(hk_nyr==4))/325

gr_nyr<-as.numeric(greens$det_16>0)+as.numeric(greens$det_17>0)+as.numeric(greens$det_18>0)+as.numeric(greens$det_19>0)
length(which(gr_nyr==1))/291
length(which(gr_nyr==2))/291
length(which(gr_nyr==3))/291
length(which(gr_nyr==4))/291

greens$det_dry<-rowSums(greens[,c(6:11,18:23,30:35,42:47)], na.rm=T)
nrow(greens)-length(which(greens$det_dry>0))
nrow(greens)-length(which(greens$det_dry==0))

greens$det_dry1<-rowSums(greens[,c(6:11)], na.rm=T)
greens$det_dry2<-rowSums(greens[,c(18:23)], na.rm=T)
greens$det_dry3<-rowSums(greens[,c(30:35)], na.rm=T)
greens$det_dry4<-rowSums(greens[,c(42:47)], na.rm=T)
sd(c(length(which(greens$det_dry1>0))/length(which(greens$det_16>0)),length(which(greens$det_dry2>0))/length(which(greens$det_17>0)),length(which(greens$det_dry3>0))/length(which(greens$det_18>0)),length(which(greens$det_dry4>0))/length(which(greens$det_19>0))))
mean(c(1-length(which(greens$det_dry1>0))/length(which(greens$det_16>0)),1-length(which(greens$det_dry2>0))/length(which(greens$det_17>0)),1-length(which(greens$det_dry3>0))/length(which(greens$det_18>0)),1-length(which(greens$det_dry4>0))/length(which(greens$det_19>0))))





hk$det_dry1<-rowSums(hk[,c(6:11)], na.rm=T)
hk$det_dry2<-rowSums(hk[,c(18:23)], na.rm=T)
hk$det_dry3<-rowSums(hk[,c(30:35)], na.rm=T)
hk$det_dry4<-rowSums(hk[,c(42:47)], na.rm=T)
mean(c(length(which(hk$det_dry1>0))/length(which(hk$det_16>0)),length(which(hk$det_dry2>0))/length(which(hk$det_17>0)),length(which(hk$det_dry3>0))/length(which(hk$det_18>0)),length(which(hk$det_dry4>0))/length(which(hk$det_19>0))))
sd(c(1-length(which(hk$det_dry1>0))/length(which(hk$det_16>0)),1-length(which(hk$det_dry2>0))/length(which(hk$det_17>0)),1-length(which(hk$det_dry3>0))/length(which(hk$det_18>0)),1-length(which(hk$det_dry4>0))/length(which(hk$det_19>0))))


sd(c(length(which(greens$Atoll[which(greens$det_16>0)]=="Lhaviyani"))/length(which(greens$det_16>0)),length(which(greens$Atoll[which(greens$det_17>0)]=="Lhaviyani"))/length(which(greens$det_17>0)),length(which(greens$Atoll[which(greens$det_18>0)]=="Lhaviyani"))/length(which(greens$det_18>0)),length(which(greens$Atoll[which(greens$det_19>0)]=="Lhaviyani"))/length(which(greens$det_19>0))))


sd(c(length(which(greens$Atoll[which(greens$det_16>0)]=="Laamu"))/length(which(greens$det_16>0)),length(which(greens$Atoll[which(greens$det_17>0)]=="Laamu"))/length(which(greens$det_17>0)),length(which(greens$Atoll[which(greens$det_18>0)]=="Laamu"))/length(which(greens$det_18>0)),length(which(greens$Atoll[which(greens$det_19>0)]=="Laamu"))/length(which(greens$det_19>0))))

nrow(hk)-length(which(hk$det_dry>0))
nrow(hk)-length(which(hk$det_dry==0))

table(greens$Atoll)
table(hk$Atoll)
sd(c(length(which(hk$Atoll[which(hk$det_16>0)]=="North Male"))/length(which(hk$det_16>0)),length(which(hk$Atoll[which(hk$det_17>0)]=="North Male"))/length(which(hk$det_17>0)),length(which(hk$Atoll[which(hk$det_18>0)]=="North Male"))/length(which(hk$det_18>0)),length(which(hk$Atoll[which(hk$det_19>0)]=="North Male"))/length(which(hk$det_19>0))))

mean(c(length(which(hk$Atoll[which(hk$det_16>0)]=="Ari"))/length(which(hk$det_16>0)),length(which(hk$Atoll[which(hk$det_17>0)]=="Ari"))/length(which(hk$det_17>0)),length(which(hk$Atoll[which(hk$det_18>0)]=="Ari"))/length(which(hk$det_18>0)),length(which(hk$Atoll[which(hk$det_19>0)]=="Ari"))/length(which(hk$det_19>0))))

sd(c(length(which(hk$Atoll[which(hk$det_16>0)]=="Baa"))/length(which(hk$det_16>0)),length(which(hk$Atoll[which(hk$det_17>0)]=="Baa"))/length(which(hk$det_17>0)),length(which(hk$Atoll[which(hk$det_18>0)]=="Baa"))/length(which(hk$det_18>0)),length(which(hk$Atoll[which(hk$det_19>0)]=="Baa"))/length(which(hk$det_19>0))))


mean(c(length(which(hk$Atoll[which(hk$det_16>0)]=="Laamu"))/length(which(hk$det_16>0)),length(which(hk$Atoll[which(hk$det_17>0)]=="Laamu"))/length(which(hk$det_17>0)),length(which(hk$Atoll[which(hk$det_18>0)]=="Laamu"))/length(which(hk$det_18>0)),length(which(hk$Atoll[which(hk$det_19>0)]=="Laamu"))/length(which(hk$det_19>0))))

sum<-greens%>%group_by(TurtleID)%>%summarize_at('Atoll', n_distinct)
sum<-hk%>%group_by(TurtleID)%>%summarize_at('Atoll', n_distinct)
