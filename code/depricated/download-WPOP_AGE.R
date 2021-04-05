# Download preparation script for WorldPop Age Structure data

# The idea of this script is to create a txt file with URLs to the needed
# files. Once this file is written to disk, the aria2 command line tool 
# is used to download the files in parallel.


# create a list object with the levels of the files
ls = list(
genders = c("f","m"), # levels of gender
age_classes = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80), # levels of age classes
years = 2000:2020 # levels of years
)
ls = expand.grid(ls) # epand the list
ls$gender = as.character(ls$gender) # change factor to charachter

urls = do.call(rbind, lapply(1:nrow(ls), function(i){ # lapply to get complete urls
  
  file_name = paste("global", paste(ls[i,], collapse  = "_"), "1km.tif", sep = "_") # concat the complete file name
  file_path = paste("ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020", # path on the remote server
                    ls$years[i], "0_Mosaicked/global_mosaic_1km", 
                    file_name,sep = "/")
  file_path # return
}))

writeLines(urls, "data-raw/wpop_age-urls.txt") # write to file
