library(tidyverse)
library(ggpubr)
library(lubridate)
library(readxl)

metadata=read_excel("SiteInformation.xlsx")
temp.data=read.csv("TempProfiles.csv")

nrow(metadata)

world.map=map_data("world")

ggplot() +
  geom_polygon(data=world.map,aes(x=long,y=lat,group=group),size=0.5,fill="white",color="black") +
  geom_hline(yintercept=seq(-80,80,by=20),linetype=3,color="grey75") +
  geom_vline(xintercept=seq(-180,180,by=20),linetype=3,color="grey75") +  
  geom_point(data=metadata,aes(x=Longitude,y=Latitude),
             shape=21,stroke=1,fill="#F95F02",color="black",size=2.5,alpha=0.66) +
  scale_x_continuous(breaks=seq(-80,80,by=20)) +
  scale_y_continuous(breaks=seq(-80,80,by=20)) +
  coord_cartesian(xlim=c(-160,180),ylim=c(-65,70)) +
  theme_bw() +
  theme(axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_line(linetype=3,color="grey75"),
        panel.background=element_rect(fill="#E6F3F7"))
  


temp.summary=temp.data %>%
  group_by(LakeID,LakeName) %>%
  summarize(MinYear=min(year(ymd(Date))),
            MaxYear=max(year(ymd(Date))),
            NYears=length(unique(year(ymd(Date)))),
            MinMonth=min(month(ymd(Date))),
            MaxMonth=max(month(ymd(Date))))

temp.summary.long=temp.summary %>%
  select(LakeID,LakeName,MinYear,MaxYear) %>%
  gather(key="Time",value="Year",MinYear,MaxYear) %>%
  mutate(Time=factor(Time,levels=c("MinYear","MaxYear")))

minmax.years.plot=ggplot() +
  geom_histogram(data=temp.summary.long,aes(Year,fill=Time),
                 position="identity",binwidth=2,color="black",alpha=0.6) +
  scale_x_continuous(breaks=seq(1910,2020,by=20)) +
  scale_y_continuous(breaks=seq(0,100,by=5),expand=expansion(mult=c(0,0.025))) +
  scale_fill_manual(values=c("#1B9E77","#D95F02"),labels=c("Earliest year of data","Latest year of data"),name=NULL) +
  labs(x="Year",y="Number of lakes",title="a") +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),
        axis.title=element_text(size=15,color="black"),
        panel.grid=element_blank(),
        legend.text=element_text(size=15,color="black"),
        legend.title=element_text(size=15,color="black"),
        legend.position=c(0.3,0.885),
        plot.title=element_text(size=15,color="black",face="bold"))


total.years.plot=ggplot() +
  geom_histogram(data=temp.summary,aes(NYears),binwidth=2,fill="grey75",color="black") +
  scale_x_continuous(breaks=seq(0,100,by=10)) +
  scale_y_continuous(breaks=seq(0,20,by=2),expand=expansion(mult=c(0,0.025))) +
  labs(x="Number of years of data",y="Number of lakes",title="b") +
  theme_bw() +
  theme(axis.text=element_text(size=15,color="black"),
        axis.title=element_text(size=15,color="black"),
        panel.grid=element_blank(),
        plot.title=element_text(size=15,color="black",face="bold"))


ggarrange(minmax.years.plot,total.years.plot,
          nrow=1,ncol=2,align="hv")
