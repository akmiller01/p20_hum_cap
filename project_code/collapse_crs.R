list.of.packages <- c("data.table")
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

load("project_data/crs.RData")

crs$metaSector = substr(crs$SectorCode,1,2)

crs = subset(crs,metaSector %in% c("11", "12"))
crs = subset(crs,FlowName %in% c("ODA Grants","ODA Loans"))

crs = data.table(crs)[
  ,.(
    commitment_value=sum(usd_commitment_defl,na.rm=T)
    ,disbursement_value=sum(usd_disbursement_defl,na.rm=T)
    )
  ,by=.(RecipientCode,RecipientName,metaSector,Year)
  ]

save(crs,file="project_data/crs_health_educ_oda.RData")
