### Plotting script for Hudgins et al. 2022, A brighter future? Stable and growing sea turtle populations in the Republic of Maldives

##code written by Emma J. Hudgins
#emma.hudgins@carleton.ca
#summary table - green turtles
greens<-read.csv('greens-Table 1.csv')
# install.packages('ggplot2')
# install.packages('viridis')
library(ggplot2)
library(viridis)

pop_plot<-ggplot(greens, aes(x=Period,y=Updated, col=Site))+theme_classic()+
  scale_color_viridis(discrete=T)+
  geom_line()+
  geom_ribbon(aes(ymin=Updated, ymax=ucl, fill=Site), alpha=0.25, outline.type="lower")+
  geom_ribbon(aes(ymax=Updated, ymin=lcl, fill=Site), alpha=0.25, outline.type="upper")+
  scale_fill_viridis(discrete=T)+
  xlab("Time Period")+
  ylab("Estimated population size")+
  scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))

pop_plot

pop_plot2<-ggplot(greens, aes(x=Period,y=Updated, col=Site))+theme_classic()+scale_color_viridis(discrete=T)+
  geom_line()+
   xlab("Time Period")+
  ylab("Estimated population size")+scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))
pop_plot2
greens$percent<-(greens$longterm-1)*100
greens$lcl_percent<-(greens$lcl_longterm-1)*100
greens$ucl_percent<-(greens$ucl_longterm-1)*100

greens_sub<-subset(greens, is.na(percent)==F)
growth_plot<-ggplot(greens_sub, aes(y=percent, x=Site))+
  theme_classic()+
  scale_color_viridis(discrete=T)+
  geom_point(aes(y=percent,col=Site), show.legend=F)+
  geom_pointrange(aes(ymin=percent, ymax=ucl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_pointrange(aes(ymax=percent, ymin=lcl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_line(aes(y=cv*100, x=1:6), col='red')+
  geom_ribbon(aes(ymin=cv*100, ymax=cv_ucl*100, x=1:6), fill="red",alpha=0.25, show.legend = F, outline.type="upper")+
  geom_ribbon(aes(ymax=cv*100, ymin=cv_lcl*100, x=1:6), fill="red", alpha=0.25, show.legend = F, outline.type="lower")+
  geom_hline(aes(yintercept=0), col='red', lty=2)+
  xlab("Site")+
  scale_y_continuous(
    name = "Multiyear growth rate (%)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.01, name="Coefficient of variation")
  )
growth_plot

#summary table - hawksbilsl
hawks<-read.csv('hawksbills-Table 1.csv')

pop_plot<-ggplot(hawks, aes(x=Period,y=Updated, col=Site))+theme_classic()+
  scale_color_viridis(discrete=T)+
  geom_line()+
  geom_ribbon(aes(ymin=Updated, ymax=ucl, fill=Site), alpha=0.25, outline.type="lower")+
  geom_ribbon(aes(ymax=Updated, ymin=lcl, fill=Site), alpha=0.25, outline.type="upper")+
  scale_fill_viridis(discrete=T)+
  xlab("Time Period")+
  ylab("Estimated population size")+
  scale_x_continuous(breaks=c(1:8),labels=c("May 2016","Nov 2016","May 2017","Nov 2017","May 2018","Nov 2018","May 2019","Nov 2019"))

pop_plot

pop_plot2<-ggplot(hawks, aes(x=Period,y=Updated, col=Site))+theme_classic()+scale_color_viridis(discrete=T)+
  geom_line()+
  xlab("Time Period")+
  ylab("Estimated population size")
pop_plot2
hawks$percent<-(hawks$longterm-1)*100
hawks$lcl_percent<-(hawks$lcl_longterm-1)*100
hawks$ucl_percent<-(hawks$ucl_longterm-1)*100

hawks_sub<-subset(hawks, is.na(percent)==F)
growth_plot<-ggplot(hawks_sub, aes(y=percent, x=Site))+
  theme_classic()+
  scale_color_viridis(discrete=T)+
  geom_point(aes(y=percent,col=Site), show.legend=F)+
  geom_pointrange(aes(ymin=percent, ymax=ucl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_pointrange(aes(ymax=percent, ymin=lcl_percent, col=Site), alpha=0.5, show.legend = F)+
  geom_line(aes(y=cv*100, x=1:6), col='red')+
  geom_ribbon(aes(ymin=cv*100, ymax=cv_ucl*100, x=1:6), fill="red",alpha=0.25, show.legend = F, outline.type="upper")+
  geom_ribbon(aes(ymax=cv*100, ymin=cv_lcl*100, x=1:6), fill="red", alpha=0.25, show.legend = F, outline.type="lower")+
  geom_hline(aes(yintercept=0), col='red', lty=2)+
  xlab("Site")+
  scale_y_continuous(
    name = "Long-term growth rate (%)",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*0.01, name="Coefficient of variation")
  )
growth_plot


survival<-read.csv('survival.csv')
survival_gr<-subset(survival, Species=="hawks")
survival_gr_plot<-ggplot(survival_gr, aes(y=Survival..or.mean., x=Site, col=Site))+
  theme_classic()+
  scale_color_viridis(discrete=T)+
  geom_point(aes(y=Survival..or.mean., x=Site , col=Site), show.legend=F)+
  geom_linerange(aes(ymin=Survival..or.mean., ymax=ucl.survival, col=Site), alpha=0.5, show.legend = F)+
  geom_linerange(aes(ymax=Survival..or.mean., ymin=lcl.survival,  col=Site), alpha=0.5, show.legend = F)+
  xlab("Site")+
  scale_y_continuous( name = "Estimated survival (%)")+
  scale_shape_discrete()
survival_gr_plot


survival_hk<-subset(survival, Species=="Hawksbills")
survival_hk_plot<-ggplot(survival_hk, aes(y=Survival..or.mean., x=Site, col=Site))+
  theme_classic()+
  scale_color_viridis(discrete=T)+
  geom_point(aes(y=Survival..or.mean., x=Site , col=Site), show.legend=F)+
  geom_linerange(aes(ymin=Survival..or.mean., ymax=ucl.survival, col=Site), alpha=0.5, show.legend = F)+
  geom_linerange(aes(ymax=Survival..or.mean., ymin=lcl.survival,  col=Site), alpha=0.5, show.legend = F)+
  xlab("Site")+
  scale_y_continuous( name = "Estimated survival (%)")+
  scale_shape_discrete()
survival_hk_plot
