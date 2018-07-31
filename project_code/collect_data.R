list.of.packages <- c("data.table","WDI","reshape2","plm","stargazer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/p20_hum_cap")
setwd(wd)

load("project_data/crs_health_educ_oda.RData")
setnames(crs,"RecipientCode","RECIPIENT")
codes = read_csv("project_data/country_codes.csv")
missing.codes = setdiff(crs$RECIPIENT,codes$RECIPIENT)
missing.code.names = unique(crs$Recipient[which(crs$RECIPIENT %in% missing.codes)])
missing.code.names = missing.code.names[order(missing.code.names)]

crs = merge(crs,codes,by="RECIPIENT")
crs = subset(crs,!is.na(ISO_A3))

crs$commitment_value_nominal = crs$commitment_value*1000000
crs$disbursement_value_nominal = crs$disbursement_value*1000000
merge.WDI = function(df,indicator,varname,start=1960,end=2018){
  wdi_tmp = WDI(indicator,country="all",extra=T,start=start,end=end)
  keep = c("iso3c","year",indicator)
  wdi_tmp = wdi_tmp[keep]
  names(wdi_tmp) = c("ISO_A3","Year",varname)
  df = merge(df,wdi_tmp,by=c("ISO_A3","Year"))
  return(df)
}

keep = c("country","ISO_A3","Year","metaSector","commitment_value_nominal","disbursement_value_nominal")
crs = data.frame(crs)
crs = crs[keep]

crs = merge.WDI(crs,"SP.POP.TOTL","pop")
crs$disbursement_value_per_cap = crs$disbursement_value_nominal/crs$pop
crs$commitment_value_per_cap = crs$commitment_value_nominal/crs$pop
crs$disbursement_value_nominal = NULL
crs$commitment_value_nominal = NULL
crs_melt = melt(crs,id.vars=c("country","ISO_A3","Year","metaSector","pop"))
crs_wide = dcast(crs_melt,country+ISO_A3+Year+pop~variable+metaSector)

crs_wide = merge.WDI(crs_wide,"NY.GDP.PCAP.KD","gdp.pc")

# Fixed effects
fixed = plm(
  gdp.pc~disbursement_value_per_cap_11+disbursement_value_per_cap_12
  ,data=crs_wide
  ,index=c("ISO_A3","Year")
  ,model="within"
)

fixed2 = plm(
  gdp.pc~commitment_value_per_cap_11+disbursement_value_per_cap_11+commitment_value_per_cap_12+disbursement_value_per_cap_12
  ,data=crs_wide
  ,index=c("ISO_A3","Year")
  ,model="within"
)

crs_wide = merge.WDI(crs_wide,"SE.XPD.TOTL.GD.ZS","education.expenditure")
crs_wide = merge.WDI(crs_wide,"SH.XPD.CHEX.GD.ZS","health.expenditure")

fixed3 = plm(
  lag(gdp.pc,5)~commitment_value_per_cap_11+disbursement_value_per_cap_11+education.expenditure+commitment_value_per_cap_12+disbursement_value_per_cap_12+health.expenditure
  ,data=crs_wide
  ,index=c("ISO_A3","Year")
  ,model="within"
)
summary(fixed)
summary(fixed2)
summary(fixed3)

# stargazer(fit,fixed,type="html",
#           dep.var.labels=c("GDP growth (annual %)"),
#           covariate.labels=c("Education ODA Commitments (%/GDP)","Health ODA Commitments (%/GDP)"),
#           out="models.htm"
#           )