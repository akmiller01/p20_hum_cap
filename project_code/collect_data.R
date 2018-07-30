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

indicator = "NY.GDP.MKTP.CD"
gdp_current = WDI(indicator,country="all",extra=T,start=1990,end=2018)
keep = c("iso3c","year",indicator)
gdp_current = gdp_current[keep]
names(gdp_current) = c("ISO_A3","Year","gdp.current")
missing.gdp = setdiff(crs$ISO_A3,gdp_current$ISO_A3)
missing.gdp.names = unique(crs$Recipient[which(crs$ISO_A3 %in% missing.gdp)])
missing.gdp.names = missing.gdp.names[order(missing.gdp.names)]

crs = merge(crs,gdp_current,by=c("ISO_A3","Year"))
crs = subset(crs,!is.na(gdp.current))

crs$value_nominal = crs$Value*1000000
crs$value_per_gdp = crs$value_nominal/crs$gdp.current

keep = c("country","ISO_A3","Year","SECTOR","Flow_type","value_per_gdp")
setnames(crs,"Flow type","Flow_type")
crs_melt = melt(crs[keep],id.vars=c("country","ISO_A3","Year","SECTOR","Flow_type"))
crs_wide = dcast(crs_melt,country+ISO_A3+Year~variable+SECTOR+Flow_type)
names(crs_wide) = make.names(names(crs_wide))

indicator = "NY.GDP.MKTP.KD.ZG"
gdp_growth = WDI(indicator,country="all",extra=T,start=1990,end=2018)
keep = c("iso3c","year",indicator)
gdp_growth = gdp_growth[keep]
names(gdp_growth) = c("ISO_A3","Year","gdp.growth")

crs_wide = merge(crs_wide,gdp_growth,by=c("ISO_A3","Year"))

# OLS
fit = lm(gdp.growth~value_per_gdp_110_Commitments+value_per_gdp_120_Commitments,data=crs_wide)
summary(fit)

# Fixed effects
fixed = plm(
  gdp.growth~value_per_gdp_110_Commitments+value_per_gdp_120_Commitments
  ,data=crs_wide
  ,index=c("ISO_A3","Year")
  ,model="within"
)
summary(fixed)

stargazer(fit,fixed,type="html",
          dep.var.labels=c("GDP growth (annual %)"),
          covariate.labels=c("Education ODA Commitments (%/GDP)","Health ODA Commitments (%/GDP)"),
          out="models.htm"
          )
