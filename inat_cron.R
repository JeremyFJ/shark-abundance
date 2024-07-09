library(DBI)
library(RPostgreSQL)
# library("spocc")
require("sharkPulseR")
require(dplyr)
require(rinat)
setwd("/var/www/html/sharkPulse/sp-iNat/s/")
source("/var/www/html/sharkPulse/sp-iNat/s/get_inat_obs2.R")

date = Sys.Date()
file_path = "/home/spr/sp_images/www/iNaturalist_"
con = connectPelagic(dbuser="spr",dbpass="****")
tx = selectData(con, "select valid from taxonomy3 where superorder = 'Selachimorpha'")
latest_rec = selectData(con, "select max(datetime) from inat")
latest_rec = substr(latest_rec, 1, 10)
sharks = unique(unlist(tx)) # 535 sharks

# Initialize an empty dataframe to store results
res <- data.frame()

# Loop through the list of scientific names
for (i in 1:length(sharks)) {
    print(paste(sharks[i], i))
    
    # Try to retrieve observations, handle errors
    test <- tryCatch(
        {
            get_inat_obs2(start_date = latest_rec, end_date = date, 
                                taxon_name = sharks[i], maxresults=10000)
        },
        error = function(e) {
            message("Error occurred:", conditionMessage(e))
            Sys.sleep(3)  # Pause for 10 seconds
            NULL
        }
    )
    
    # If the result is not NULL, bind it to the result dataframe
    if (!is.null(test)) {
        res <- rbind(res, test)
    tryCatch ({
        for (img in 1:nrow(test)){
            download.file(test$image_url[img], paste0(file_path, test$id[img], ".jpg"), mode = "wb")
            }
        },
        error = function(e){
            message("Could not download image:", conditionMessage(e))
        })
    }
}

save.image(file = paste0(date, "_inat.RData"))
inat = unique(res) # these were some duplicates from the harvesting process

# Get distinct 'id' values from the 'inat' table
query <- "SELECT DISTINCT id FROM inat"
distinct_ids <- dbGetQuery(con, query)$id

# Remove duplicates from the 'inat_new' data frame based on 'id'
inat_new <- inat[!inat$id %in% distinct_ids, ]
inat_new$sound_url = NULL
inat_new$img_name = paste0("iNaturalist_", as.character(inat_new$id), ".jpg")
dbWriteTable(con, "inat", inat_new, append = TRUE, row.names=FALSE)

write.csv(inat_new, paste0(date, "_inat.csv"), row.names=FALSE)
