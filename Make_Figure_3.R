#### This code makes Figure 3 ####
library(tidyverse)
library(readxl)
library(egg)

#### Import dataset ####

All_Peridotite_Experiments<-read_excel("Supplementary Table 2.xlsx")
# if this step isn't working for you, then you can manually import using the "import dataset" function within Rstudio.
# if you do this, make sure to name your dataset "All_Peridotite_Experiments"
Filtered_Peridotite_Experiments<-All_Peridotite_Experiments%>%
  filter(`Melt Fraction`>=1&`Melt Fraction`<=30)
#### Confirm fit ####
Fit_Line_MF<-lm(Filtered_Peridotite_Experiments$`Duration (hrs)`~Filtered_Peridotite_Experiments$`Melt Fraction`)
summary(Fit_Line_MF)
#### Make Figure ####
MainTimeFig<-ggplot(data=All_Peridotite_Experiments,aes(y=`Duration (hrs)`,x=`Melt Fraction`))+
  geom_point(aes(shape=QualitativeWater),size=2)+
  theme_article()+labs(x="Melt Percentage (vol%)", y="Duration (hours)")+
  geom_rect(aes(xmin=1,xmax=30,ymin=0,ymax=350), color="red", fill=NA, size=1)+lims(x=c(0,80))+
  theme(legend.title = element_blank())

InsetTimeFig<-ggplot(data=Filtered_Peridotite_Experiments,aes(y=`Duration (hrs)`,x=`Melt Fraction`))+
  geom_point()+
  geom_smooth(method=lm,formula=y~x, color="tan1", se=TRUE)+
  theme_article()+labs(x="F (vol%)", y="Duration (hours)")+
  lims(y=c(0,350))+theme(axis.title = element_blank())+
  theme(plot.background = element_rect(color = "red", size = 1.5))

TimeFigAll<-MainTimeFig+
  annotation_custom(ggplotGrob(InsetTimeFig), xmin = 32, xmax = 82, 
                                          ymin = 110, ymax = 360)+annotate(geom="text",x=65,y=320,label="Time == -4.4*F + 153.67", size=4, parse=TRUE)+annotate(geom = "text",x=70,y=300,label="R.S.E == 48.77", parse=TRUE)

TimeFigAll
