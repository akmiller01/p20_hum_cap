list.of.packages <- c("data.table","readr")
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

txts = list.files("project_data/zips/","*.txt",full.names=T)

data.list = list()
data.index = 1

for(txt in txts){
  message(basename(txt))
  tmp = read_delim(txt,"|")
  data.list[[data.index]] = tmp
  data.index = data.index + 1
}

crs = do.call(rbind,data.list)
save(crs,file="project_data/crs.RData")
