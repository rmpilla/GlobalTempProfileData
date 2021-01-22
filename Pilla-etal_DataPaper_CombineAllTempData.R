library(tidyverse)
library(lubridate)
library(readxl)

site.info=read_excel("SiteInformation.xlsx") %>%
  select(siteID,LakeID,LakeName,AlternateLakeName,CountryOfLake,Region)

setwd("~/Temperature Data - CONFIDENTIAL")
temp.files=list.files()
temp.files=temp.files[-which(temp.files=="desktop.ini")]

site.files=as.numeric(unlist(lapply(strsplit(temp.files,"_"),"[",1)))
lakename.files=unlist(lapply(strsplit(unlist(lapply(strsplit(temp.files,"_"),"[",2)),"-"),"[",1))

file.number=c()
for(i in 1:nrow(site.info)){
  site.match=which(site.files==site.info$siteID[i])
  
  if(length(site.match)==1){
    file.number[i]=site.match
  }else{
    full.match=which(site.files==site.info$siteID[i] & lakename.files==site.info$LakeName[i])
    
    if(length(full.match)==1){
      file.number[i]=full.match
    }else{
      approx.match=agrep(pattern=site.info$LakeName[i],x=lakename.files)
      
      if(length(approx.match)==1){
        file.number[i]=approx.match
      }
    }
  }
}


site.info[which(is.na(file.number)),1:5]

file.number[which(is.na(file.number))]=c(which(lakename.files=="Bubble"),
                                         which(lakename.files=="Fracksjon"),
                                         which(lakename.files=="Grand"),
                                         which(lakename.files=="Lake222"),
                                         which(lakename.files=="Lake224"),
                                         which(lakename.files=="Lake239"),
                                         which(lakename.files=="Lake240"),
                                         which(lakename.files=="Lake373"),
                                         which(lakename.files=="Lake442"),
                                         which(lakename.files=="PaksuvuonoNellim"), ##SI=23 Inari
                                         which(lakename.files=="Jordan"),
                                         which(lakename.files=="Nareselka"),
                                         which(lakename.files=="Pynnolanniemi"),
                                         which(lakename.files=="Lugano")[1],
                                         which(lakename.files=="Lugano")[2],
                                         which(lakename.files=="Pengonpohja"), ##SI=23 Nasijarvi
                                         which(lakename.files=="OvreSkarsjon"), 
                                         which(lakename.files=="Linnasaari"),
                                         which(lakename.files=="PaijannePaijatsalo"),
                                         which(lakename.files=="Pesiojarvi"),
                                         which(lakename.files=="PyhajarviSakyla"),
                                         which(lakename.files=="Sparkling"),
                                         which(lakename.files=="StSkarsjon"))


site.info$TempFileNumber=file.number

setwd("~/Temperature Data - CONFIDENTIAL")

data.combine=c()
for(i in 1:nrow(site.info)){
  already.used=which(data.combine$TempFileName==temp.files[site.info$TempFileNumber[i]])
  
  if(length(already.used)==0){
  data=read.csv(temp.files[site.info$TempFileNumber[i]])
  
  data.long=data %>%
    select(-Year,-Day.of.Year,-Selected.Minus.Median) %>%
    gather(key="Depth",value="Temperature",2:ncol(.)) %>%
    mutate(LakeID=site.info$LakeID[i],
           SiteID=site.info$siteID[i],
           LakeName=site.info$LakeName[i],
           TempFileName=temp.files[site.info$TempFileNumber[i]])
  
  data.combine=rbind(data.combine,data.long)
  }else{
    print(i)
  }
}


temp.files[which(is.na(match(temp.files,unique(data.combine$TempFileName))))]



data.final=data.combine %>%
  mutate(Depth_m=as.numeric(unlist(lapply(strsplit(unlist(lapply(strsplit(data.combine$Depth,"_"),"[",2)),"m"),"[",1)))) %>%
  rename(Temperature_degCelsius=Temperature) %>%
  select(SiteID,LakeID,LakeName,Date,Depth_m,Temperature_degCelsius)



setwd("~/MS Files")

write.csv(data.final,file="TTempProfiles.csv",row.names=F)
