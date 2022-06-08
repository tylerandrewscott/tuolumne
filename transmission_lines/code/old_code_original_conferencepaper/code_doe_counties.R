
library(pbapply)
library(data.table)
library(tidyverse)
library(stringr)
library(sf)
library(lwgeom)
library(rgeos)
doe = readRDS('input/agency_nepa/doe/doefiles.rds')
doe$allcountytext = gsub('VVheeler','Wheeler',doe$allcountytext)
doe$allcountytext = gsub('Wahkiahum','Wahkiakum',doe$allcountytext)
doe$allcountytext = gsub('Orielle','Oreille',doe$allcountytext)
doe$allcountytext = gsub('Yakama','Yakima',doe$allcountytext)
doe$doc_type = str_extract(doe$NEPA_ID,'^[A-Z]{1,}')
doe2 = readRDS('input/agency_nepa/doe/doe_ea_eis_record.RDS')
doe2$IN_SAMPLE = grepl('Bonneville|Western Area|WAPA|BPA',doe2$proj_description) + 0
cx = readRDS('input/agency_nepa/doe/rpa_cx_dt.RDS')
cx$NEPA_ID = str_extract(cx$Title,'CX-[0-9]{1,}')
cx = cx[cx$Power_Agency  %in% c('BPA','WAPA'),]

doe$IN_SAMPLE = 0
doe$IN_SAMPLE[doe$NEPA_ID %in% cx$NEPA_ID] <- 1
doe$IN_SAMPLE[doe$NEPA_ID %in% doe2$DOE_ID[doe2$IN_SAMPLE==1]] <- 1



albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"

fips = tigris::fips_codes
fips$FULL = paste(fips$county,fips$state_name,sep = ', ')
fips$FULL2 = paste(fips$county,fips$state,sep = ', ')
fips$FULL3 = paste(gsub(' County$','',fips$county),fips$state,sep = ', ')
fips$FULL4 = paste(gsub(' County$','',fips$county),fips$state,sep = ', ')
fips$CFIPS = paste0(fips$state_code,fips$county_code)

county_text = pbsapply(doe$allcountytext,function(x) str_extract_all(x,paste(paste(fips$FULL,collapse='|'),paste(fips$FULL2,collapse='|'),
                                                                             paste(fips$FULL3,collapse='|'),paste(fips$FULL4,collapse='|'),collapse='|')),cl = 8)
county_text = sapply(seq_along(county_text),function(x) unique(county_text[[x]]))

counties = tigris::counties(class = 'sf')
plc = pblapply(state.abb,function(x) tigris::places(x,class= 'sf',year = '2018'),cl = 10)
plall = do.call(rbind,plc)
plall = st_transform(plall,albersNA)
counties = st_transform(counties,albersNA)
place_in_county = st_within(plall,counties)
place_overs_county = st_overlaps(plall,counties)
place_in_county[sapply(place_in_county,length)==0] <- place_overs_county[sapply(place_in_county,length)==0]
plall$CFIPS = sapply(place_in_county,function(x) counties$GEOID[unlist(x)])

doe$CFIPS = pbsapply(seq_along(county_text),function(x) 
  unique(c(fips$CFIPS[match(county_text[[x]],fips$FULL)] %>% .[!is.na(.)],
  fips$CFIPS[match(county_text[[x]],fips$FULL2)] %>% .[!is.na(.)],
  fips$CFIPS[match(county_text[[x]],fips$FULL3)] %>% .[!is.na(.)],
  fips$CFIPS[match(county_text[[x]],fips$FULL4)] %>% .[!is.na(.)])),cl = 8)


library(tigris)
gaz = fread('input/gazetteers/2018_Gaz_place_national.txt')
gaz$basename = gsub('\\b[a-z]{1,}.*|\\sCDP$|\\(.*$','',gaz$NAME,perl = T)
gaz$basename = str_squish(gaz$basename)
gaz$GEOID = formatC(gaz$GEOID,width=7,flag=0)
gaz$CFIPS = plall$CFIPS[match(gaz$GEOID,plall$GEOID)]
gaz=gaz[!sapply(gaz$CFIPS,length)==0,]
gaz$basefull1 = paste0(gaz$basename,', ',gaz$USPS)
gaz$basefull2 = paste0(gaz$basename,', ', fips_codes$state_name[match(gaz$USPS,fips_codes$state)])

fill = which(sapply(doe$CFIPS,length)==0 & doe$allcountytext!='')
doe$CFIPS[fill] <- pbsapply(fill,function(i){
    #print(i)
      m1 = str_extract_all(doe$allcountytext[[i]],paste(gaz$basefull1,collapse='|'))
      m2 = str_extract_all(doe$allcountytext[[i]],paste(gaz$basefull2,collapse='|'))
      ex = c(unlist(m1),unlist(m2))
      ma = union(match(ex,gaz$basefull1),match(ex,gaz$basefull2))
      ma = ma[!is.na(ma)]
      if(length(ma)>0){unlist(gaz$CFIPS[ma])}else{NULL}
      },cl =10)


unique_counties = as.data.table(table(counties$NAME))[order(N),][N==1,]
uq_county = counties[counties$NAME %in% unique_counties$V1,]
uq_county$CFIPS =counties$GEOID[match(uq_county$NAME,counties$NAME)]
uq_county$full_name = paste0(uq_county$NAME,' County')
replace_index = which(sapply(doe$CFIPS,length)==0)
doe$CFIPS[replace_index] <- sapply(sapply(str_extract_all(doe$allcountytext[replace_index],paste(uq_county$full_name,collapse='|')),unique),function(x) uq_county$CFIPS[match(x,uq_county$full_name)])


test_i = which(sapply(doe$CFIPS,length)==0)
extract_i = lapply(str_extract_all(doe$allcountytext[test_i],'[A-Z][a-z]{1,}\\,\\s[A-Z]{2}'),unique)

matches = pblapply(seq_along(extract_i),function(x) {if(length(extract_i[[x]])>0){
  unique(unlist(sapply(extract_i[[x]],function(y) unlist(gaz$CFIPS[grep(y,gaz$basefull1)]),simplify = F)))}else{character(0)}
},cl = 10)

matches =  lapply(matches,function(x) {if(length(x)>1){NULL}else{x}})
doe$CFIPS[test_i] <- matches



doe$CFIPS[grepl('Walla County|Walla Walla',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Walla County|Walla Walla',doe$allcountytext)],function(x) c(x,'53071'))
doe$CFIPS[grepl('Coos County',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Coos County',doe$allcountytext)],function(x) c(x,'41011'))
doe$CFIPS[grepl('Coos\\W',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Coos\\W',doe$allcountytext)],function(x) c(x,'41011'))
doe$CFIPS[grepl('Oreille',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Oreille',doe$allcountytext)],function(x) c(x,'53051'))
doe$CFIPS[grepl('Lincoln, OR',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Lincoln, OR',doe$allcountytext)],function(x) c(x,'41041'))
doe$CFIPS[grepl('Harbor County, Washington',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Harbor County, Washington',doe$allcountytext)],function(x) c(x,'53027'))
doe$CFIPS[grepl('Yakima',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Yakima',doe$allcountytext)],function(x) c(x,'53077'))
doe$CFIPS[grepl('Juan County, (WA|Washington)',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Juan County, (WA|Washington)',doe$allcountytext)],function(x) c(x,'53055'))
doe$CFIPS[grepl('River County, (OR|Oregon)',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('River County, (OR|Oregon)',doe$allcountytext)],function(x) c(x,'41027'))
doe$CFIPS[grepl('Perce County',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Perce County',doe$allcountytext)],function(x) c(x,'16069'))
doe$CFIPS[grepl('Whitman',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Whitman',doe$allcountytext)],function(x) c(x,'53075'))
doe$CFIPS[grepl('Asotin',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Asotin',doe$allcountytext)],function(x) c(x,'53003'))
doe$CFIPS[grepl('Walla$',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Walla$',doe$allcountytext)],function(x) c(x,'53071'))
doe$CFIPS[grepl('Idaho Counties$',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Idaho Counties$',doe$allcountytext)],function(x) c(x,'16049'))
doe$CFIPS[grepl('Cowlitz',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Cowlitz',doe$allcountytext)],function(x) c(x,'53015'))
doe$CFIPS[grepl('Lodge County',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Lodge County',doe$allcountytext)],function(x) c(x,'30023'))
doe$CFIPS[grepl('Gilliam',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Gilliam',doe$allcountytext)],function(x) c(x,'41021'))
doe$CFIPS[grepl('Deschutes',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Deschutes',doe$allcountytext)],function(x) c(x,'41017'))
doe$CFIPS[grepl('Harney',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Harney',doe$allcountytext)],function(x) c(x,'41025'))
doe$CFIPS[grepl('Chelan',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Chelan',doe$allcountytext)],function(x) c(x,'53007'))
doe$CFIPS[grepl('Clackamas',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Clackamas',doe$allcountytext)],function(x) c(x,'41005'))
doe$CFIPS[grepl('Umatilla',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Umatilla',doe$allcountytext)],function(x) c(x,'41059'))
doe$CFIPS[grepl('Okanogan',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Okanogan',doe$allcountytext)],function(x) c(x,'53047'))
doe$CFIPS[grepl('Grant and Benton',doe$allcountytext)&grepl('Washington',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Grant and Benton',doe$allcountytext)&grepl('Washington',doe$allcountytext)],function(x) c(x,'53005','53025'))
doe$CFIPS[grepl('King',doe$allcountytext)&grepl('Washington',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('King',doe$allcountytext)&grepl('Washington',doe$allcountytext)],function(x) c(x,'53033'))

doe$CFIPS[grepl('Dalles',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('Dalles',doe$allcountytext)],function(x) c(x,'41065'))
doe$CFIPS[grepl('River, Oregon',doe$allcountytext)] <- sapply(doe$CFIPS[grepl('River, Oregon',doe$allcountytext)],function(x) c(x,'41027'))

saveRDS(doe,'input/agency_nepa/doe/doefiles_cfips.rds')

