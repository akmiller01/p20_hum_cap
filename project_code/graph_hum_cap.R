list.of.packages <- c("data.table","WDI","reshape2","ggplot2","readr","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}


# setwd("C:/Users/Zach/Documents/ITEP 2018/Human Capital/CRS")
# # years <- c("1973-94","1995-99","2000-01","2002-03","2004-05",2006:2016)
# # crslist <-list()
# # for(i in 1:length(years)){
# #   nam= paste("CRS",years[i],"data.txt",sep=" ")
# #   print(nam)
# #   crss <- paste("CRS",years[i],sep=".")
# #   crs.temp <- read_delim(nam, "|", escape_double = FALSE, trim_ws = TRUE)
# #   crslist[[crss]]=crs.temp
# # }
# # crs = rbindlist(crslist)
# #
# #
# #
# crs <- rbindlist(lapply(Sys.glob("CRS*.txt"),read.delim, sep = "|"))
# save(crs,file="C:/Users/Zach/Documents/ITEP 2018/Human Capital/CRSraw.RData")
load("C:/Users/Zach/Documents/ITEP 2018/Human Capital/CRSraw.RData")
crs=subset(crs, FlowName=="ODA Grants" | FlowName=="ODA Loans")
# crs$usd_commitment_defl[which(crs$Year<1990)]=crs$usd_commitment_defl[which(crs$Year<1990)]/100
crs$usd_commitment_defl[which(crs$Year==2006)]=crs$usd_commitment_defl[which(crs$Year==2006)]*10
# crs$health=0
# crs$health[which(crs$PurposeCode>=12000 & crs$PurposeCode<14000)]=crs$usd_commitment_defl[which(crs$PurposeCode>=12000 & crs$PurposeCode<14000)]
# crs$educ=0
# crs$educ[which(crs$PurposeCode<12000)]=crs$usd_commitment_defl[which(crs$PurposeCode<12000)]
crs$social=0
crs$social[which(crs$PurposeCode>16000 & crs$PurposeCode<16020)]=crs$usd_commitment_defl[which(crs$PurposeCode>16000 & crs$PurposeCode<16020)]


# DAC_donors=
#   c("Australia",
#     "Austria",
#     "Belgium",
#     "Canada",
#     "Czech Republic",
#     "Denmark",
#     "EU Institutions",
#     "Finland",
#     "France",
#     "Germany",
#     "Greece",
#     "Hungary",
#     "Iceland",
#     "Ireland",
#     "Italy",
#     "Japan",
#     "Italy",
#     "Japan",
#     "Korea",
#     "Luxembourg",
#     "Netherlands",
#     "New Zealand",
#     "Norway",
#     "Poland",
#     "Portugal",
#     "Slovak Republic",
#     "Slovenia",
#     "Spain",
#     "Sweden",
#     "Switzerland",
#     "United Kingdom",
#     "United States")


crs=crs[which(crs$bi_multi==4 | crs$DonorName %in% DAC_donors),]
crs$decade=NA
crs$decade[which(crs$Year>=1970 & crs$Year<1980)]="1975-79"
crs$decade[which(crs$Year>=1980 & crs$Year<1990)]="1980-9"
crs$decade[which(crs$Year>=1990 & crs$Year<2000)]="1990-99"
crs$decade[which(crs$Year>=2000 & crs$Year<2010)]="2000-09"
crs$decade[which(crs$Year>=2010 & crs$Year<2020)]="2010+"
# aid=data.table(crs)[,.(
#   healthaid=sum(health, na.rm=T),
#   educaid=sum(educ, na.rm=T),
#   totalaid=sum(usd_commitment_defl, na.rm=T)
# ),by=c("DonorName","Year")]

aid2=data.table(crs)[,.(
  # healthaid=sum(health, na.rm=T),
  # educaid=sum(educ, na.rm=T),
  socialaid=sum(social,na.rm=T),
  totalaid=sum(usd_commitment_defl, na.rm=T)
),by=c("decade")]


wd = paste0(prefix,"/git/p20_hum_cap")
setwd(wd)

# aid2$humcapshare=(aid2$healthaid+aid2$educaid)/aid2$totalaid
# aid$humcapshare=(aid$healthaid+aid$educaid)/aid$totalaid
aid2$socialshare=aid2$social/aid2$totalaid
# aid$socialshare=aid$social/aid$totalaid
# 
# ggplot(aid2, aes(x=Year, y=healthaid))+geom_bar(stat="identity")
# ggplot(aid2, aes(x=Year, y=educaid))+geom_bar(stat="identity")
# ggplot(aid2, aes(x=Year, y=totalaid))+geom_bar(stat="identity")
# ggplot(aid2, aes(x=Year, y=humcapshare))+geom_bar(stat="identity")
# ggplot(aid2, aes(x=Year, y=socialshare))+geom_bar(stat="identity")
# 
# 
# ggplot(aid[which(aid$DonorName %in% DAC_donors)], aes(x=Year, y=humcapshare))+geom_bar(stat="identity")+facet_wrap(~DonorName)
# 
# ggplot(aid, aes(x=Year, y=(healthaid+educaid)))+geom_bar(stat="identity")+facet_wrap(~DonorName)
# ggplot(aid, aes(x=Year, y=educaid))+geom_bar(stat="identity")+facet_wrap(~DonorName)
# 
# dat=aid[which(aid$Year==2015 & aid$DonorName %in% DAC_donors),]

aid2=aid2[,c("decade","socialshare")]
setnames(aid2,"socialshare","Share of ODA commitments going to social/welfare services")
  
write.csv(aid2,"project_data/human_cap_odashare.csv", row.names=F, na="")


# ggplot(social, aes(x=Year, y=usd_commitment_defl))+geom_bar(stat="identity")
# 
# load("E:/git/p20_hum_cap/project_data/crs_sector_oda.RData")
# setnames(crs,"RecipientCode","RECIPIENT")
# codes = read_csv("project_data/country_codes.csv")
# missing.codes = setdiff(crs$RECIPIENT,codes$RECIPIENT)
# missing.code.names = unique(crs$Recipient[which(crs$RECIPIENT %in% missing.codes)])
# missing.code.names = missing.code.names[order(missing.code.names)]
# 
# crs = merge(crs,codes,by="RECIPIENT")
# crs = subset(crs,!is.na(ISO_A3))
# 
# crs$commitment_value_nominal = crs$commitment_value*1000000
# crs$disbursement_value_nominal = crs$disbursement_value*1000000
# merge.WDI = function(df,indicator,varname,start=1960,end=2018){
#   wdi_tmp = WDI(indicator,country="all",extra=T,start=start,end=end)
#   keep = c("iso3c","year",indicator)
#   wdi_tmp = wdi_tmp[keep]
#   names(wdi_tmp) = c("ISO_A3","Year",varname)
#   df = merge(df,wdi_tmp,by=c("ISO_A3","Year"))
#   return(df)
# }
# 
# keep = c("country","ISO_A3","Year","metaSector","commitment_value_nominal")
# crs = data.frame(crs)
# crs = crs[keep]
# 
# crs.hc=subset(crs, metaSector %in% c(11,12,13,16))
# ggplot(crs.hc, aes(x=country, y=commitment_value_nominal, fill=metaSector, group=metaSector))+geom_bar(stat="identity")
# codes = read_csv("project_data/country_codes.csv")
# 
# crs_melt = melt(crs.hc,id.vars=c("country","ISO_A3","Year","metaSector"))
# crs_wide = dcast(crs_melt,country+ISO_A3+Year~variable+metaSector)
# crs_wide=join(crs_wide,codes,by=c("country","ISO_A3"))
# 
# crs_wide = merge.WDI(crs_wide,"NY.GDP.PCAP.KD","gdp.pc")
# crs_wide = merge.WDI(crs_wide,"SE.XPD.TOTL.GD.ZS","education.expenditure")
# crs_wide = merge.WDI(crs_wide,"SH.XPD.CHEX.GD.ZS","health.expenditure")
# crs_wide = merge.WDI(crs_wide,"NY.GDP.MKTP.CD","gdp_current")
# crs_wide = merge.WDI(crs_wide,"NY.GDP.PCAP.KD.ZG","gdp.pc.growth")
# 
# crs_wide$totaleducationspending=(crs_wide$education.expenditure*crs_wide$gdp)+crs_wide$commitment_value_nominal_11
# crs_wide$totalhealthspending=(crs_wide$health.expenditure*crs_wide$gdp)+crs_wide$commitment_value_nominal_12+crs_wide$commitment_value_nominal_13
# crs_wide$totalhcspending=crs_wide$totaleducationspending+crs_wide$totalhealthspending
# crs_wide$totalhcspendingpct=crs_wide$totalhcspending/crs_wide$gdp
# crs_wide$Year=as.numeric(crs_wide$Year)
# 
# ggplot(subset(crs_wide,!is.na(commitment_value_nominal_11)), aes(x=Year, y=commitment_value_nominal_11,color=sqrt(gdp.pc.growth)))+geom_point()+scale_color_gradient(low="white",high="steelblue")+facet_wrap(~country)
# ggplot(crs_wide, aes(x=Year, y=c(commitment_value_nominal_12+commitment_value_nominal_13)))+ geom_bar(stat="identity")
# 
# 
# WDIdata=WDI(country="all",indicator=c("SE.XPD.TOTL.GD.ZS","SH.XPD.CHEX.GD.ZS"),start=1960, end=2017,extra=T)
# WDIdata$hcinvestments=WDIdata$SE.XPD.TOTL.GD.ZS+WDIdata$SH.XPD.CHEX.GD.ZS
# WDIdata$hcinvestments[which(is.na(WDIdata$SE.XPD.TOTL.GD.ZS)|is.na(WDIdata$SH.XPD.CHEX.GD.ZS))]=NA
# WDIdata2=WDIdata[which(!is.na(WDIdata$SE.XPD.TOTL.GD.ZS)&!is.na(WDIdata$SH.XPD.CHEX.GD.ZS)),]
# ggplot(WDIdata2, aes(x=year, y=hcinvestments))+geom_line()+facet_wrap(~country)
# WDIdata2=WDIdata2[,c("country","year","hcinvestments","SE.XPD.TOTL.GD.ZS","SH.XPD.CHEX.GD.ZS")]
# names(WDIdata2)=c("country","year","Hum.Cap.Share.GDP","Education.Share.GDP","Health.Share.GDP")
# write.csv(WDIdata2,"project_data/human.capital.share.gdp.csv",row.names=F,na="")
# 
