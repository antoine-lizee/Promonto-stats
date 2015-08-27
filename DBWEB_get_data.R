### Set WD
setwd('/media/FD/RMISC/DaphStats/')

# Get the data 
source('./preproc.R')

TABLES <- list(atcd = atcd2[-(1:4)], tech=tech2[-(1:5)], post=post2[-(1:7)]) #2 to get the modified names
tablenames <- names(TABLES)

dir.create('./DATA/', showWarnings=F)
save.image('./DATA/DBs.RData', safe = TRUE)

## Update the data
source('./WEB_launchQA.R')

## Copy the files
unlink('/media/FD/WEB/TEST2/current/DATA', recursive = TRUE)
file.copy('./DATA', '/media/FD/WEB/TEST2/current/', recursive = TRUE, copy.mode = TRUE)

