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

crs = read_csv("project_data/CRS1_educ_health.csv")
codes = read_csv("project_data/country_codes.csv")
missing.codes = setdiff(crs$RECIPIENT,codes$RECIPIENT)
missing.code.names = unique(crs$Recipient[which(crs$RECIPIENT %in% missing.codes)])
missing.code.names = missing.code.names[order(missing.code.names)]

crs = merge(crs,codes,by="RECIPIENT")
crs = subset(crs,!is.na(ISO_A3))

crs$value_nominal = crs$Value*1000000
merge.WDI = function(df,indicator,varname,start=1990,end=2018){
  wdi_tmp = WDI(indicator,country="all",extra=T,start=start,end=end)
  keep = c("iso3c","year",indicator)
  wdi_tmp = wdi_tmp[keep]
  names(wdi_tmp) = c("ISO_A3","Year",varname)
  df = merge(df,wdi_tmp,by=c("ISO_A3","Year"))
  return(df)
}

keep = c("country","ISO_A3","Year","SECTOR","Flow_type","value_nominal")
setnames(crs,"Flow type","Flow_type")
crs = crs[keep]

crs = merge.WDI(crs,"SP.POP.TOTL","pop")
crs$value_per_cap = crs$value_nominal/crs$pop
crs$value_nominal = NULL
crs_melt = melt(crs,id.vars=c("country","ISO_A3","Year","SECTOR","Flow_type","pop"))
crs_wide = dcast(crs_melt,country+ISO_A3+Year+pop~variable+SECTOR+Flow_type)
names(crs_wide) = make.names(names(crs_wide))

crs_wide = merge.WDI(crs_wide,"SI.POV.DDAY","pov")

# Fixed effects
fixed = plm(
  pov~value_per_cap_110_Commitments+value_per_cap_120_Commitments
  ,data=crs_wide
  ,index=c("ISO_A3","Year")
  ,model="within"
)

fixed2 = plm(
  pov~value_per_cap_110_Commitments+value_per_cap_110_Gross.Disbursements+value_per_cap_120_Commitments+value_per_cap_120_Gross.Disbursements
  ,data=crs_wide
  ,index=c("ISO_A3","Year")
  ,model="within"
)

crs_wide = merge.WDI(crs_wide,"SE.XPD.TOTL.GD.ZS","education.expenditure")
crs_wide = merge.WDI(crs_wide,"SH.XPD.CHEX.GD.ZS","health.expenditure")

fixed3 = plm(
  lag(pov,5)~value_per_cap_110_Commitments+value_per_cap_110_Gross.Disbursements+education.expenditure+value_per_cap_120_Commitments+value_per_cap_120_Gross.Disbursements+health.expenditure
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