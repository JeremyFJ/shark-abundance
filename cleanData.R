#' Clean sharkPulse data
#'
#' clead sharkPulse data sourched with the `getSharkPulse()` function and sanitize species names, correct for mispelling errors and remove records on land. It also write sql and csv files with records to be further validated. These will be found in teh gitHub sp-database repo. It also adds the associated FAO sector
#' @param dat sharkPulse data from getSharkPulse()
#' @param repopath is the directory path of the Github repo receiving the erroneus records to be corrected
#' @export 
cleanData = function(dat, repopath = "~/SharkPulse/GitHub/sp-database"){

dat = dat[!is.na(dat$species_name),] # remove records with missing species names


dat = sanitizeTaxonomy(dat)
dat = subset(dat, !species_name %in% c("","-","Not enough visual info","Other species"))


# now we need to validate the names
con = connectPelagic(dbuser="spr",dbpass="spr_pass")
tx = selectData(con, "select * from taxonomy3")

dat = merge(dat, tx, by = "species_name", all.x = T)

dat$order = with(dat,ifelse(order=="",order_name, order))
dat$family = with(dat,ifelse(family=="",family_name, family))
dat$genus = with(dat,ifelse(genus=="",genus_name, genus))
#moving family, genus and falimy names in their dedicated space



wrongs = dat$species_name[is.na(dat$taxonid)]


# #source("sanitizeNames.R") # needs rfishbase 
# dat$species_name2 = sanitizeNames(dat$species_name, tx) # this takes between 3 and 4 minutes to run
# # check 
# # unique(subset(dat, species_name!=species_name2, select = c(species_name, species_name2)))

# tofix = unique(subset(dat, dat$species_name!=dat$species_name2, select = c(table, species_name, species_name2))) # need to fix at the database source
# # # -- write.csv(tofix, "../data/toFixDatabase.csv", row.names = F) # i can write the sql statements  but need to remove some records

# sql_commands = with(tofix,ifelse(table %in% c("flickr"),paste("update ",table," set species_name_1 = '",species_name2,"' where LOWER(TRIM(species_name_1)) = '",tolower(species_name),"';", sep = "")
#      ,paste("update ",table," set species_name = '",species_name2,"' where LOWER(TRIM(species_name)) = '",tolower(species_name),"';", sep = "")))
# writeLines(sql_commands, paste(repopath,"/sql/update_commands.sql", sep = ""))

# # remove duplicates
# dat$species_name = dat$species_name2
# dupls <- dat[duplicated(dat),] # 238 duplicates

# write.csv(dupls, paste(repopath,"/data/duplicates.csv", sep=""), row.names = F) # produces duplicate file to fix in database

dat = unique(dat) # they are all unique and there should not be duplicates because we have a primary key


# remove points inlands

# working on removeInslands
#source("../../../SharkPulse/GitHub/sp-Flickr/filtergeo.R")
buffer = -100 # I have included 100 km to be sure to remove records actually onteh coast but from region with a very complex coastline
dat = removeInlands3(dat, buffer_km=buffer) # there are 5452 inlands
inlands = subset(dat, inland==TRUE)
write.csv(inlands, paste(repopath,"/data/checkInlands_buffer",abs(buffer),"km.csv",sep=""), row.names = F)


dat = subset(dat, inland==FALSE)

dat

}