list.of.packages <- c("data.table","WDI","reshape2","ggplot2","readr","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}


load("project_data/crs.RData")
crs=subset(crs, FlowName=="ODA Grants" | FlowName=="ODA Loans"|FlowName=="Equity Investment")
crs$usd_commitment_defl[which(crs$Year==2006)]=crs$usd_commitment_defl[which(crs$Year==2006)]*10
crs$social=0
crs$social[which(crs$PurposeCode>16000 & crs$PurposeCode<16020)]=crs$usd_commitment_defl[which(crs$PurposeCode>16000 & crs$PurposeCode<16020)]




crs=crs[which(crs$bi_multi==4 | crs$DonorName %in% DAC_donors),]
crs$decade=NA
crs$decade[which(crs$Year>=1970 & crs$Year<1980)]="1975-79"
crs$decade[which(crs$Year>=1980 & crs$Year<1990)]="1980-9"
crs$decade[which(crs$Year>=1990 & crs$Year<2000)]="1990-99"
crs$decade[which(crs$Year>=2000 & crs$Year<2010)]="2000-09"
crs$decade[which(crs$Year>=2010 & crs$Year<2020)]="2010+"


aid2=data.table(crs)[,.(
  socialaid=sum(social,na.rm=T),
  totalaid=sum(usd_commitment_defl, na.rm=T)
),by=c("decade")]


wd = paste0(prefix,"/git/p20_hum_cap")
setwd(wd)


aid2$socialshare=aid2$social/aid2$totalaid


aid2=aid2[,c("decade","socialshare")]
setnames(aid2,"socialshare","Share of ODA commitments going to social/welfare services")
  
write.csv(aid2,"project_data/human_cap_odashare.csv", row.names=F, na="")


