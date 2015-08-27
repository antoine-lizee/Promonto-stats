
STR = T

# source('./DBWEB_get_data.R')

dir.create('../DATA/export/', showWarnings=F)
dir.create('../DATA/export/images/', showWarnings=F)

### Get the TABLE file and the other tables from the database.
load(file='../DATA/DBs.RData')

v_str <- c()

## Create the graphs
for (s_table in tablenames) {
  cat(paste('####### TREATING :',s_table, '\n'))
  table <- data.frame(lapply(TABLES[[s_table]], as.vector), stringsAsFactors=TRUE, check.names=F)
  num <- 0
  ## go through the columns
  for (s_col in colnames(table) ){
    num <- num + 1
    i_str <- iconv(paste0(s_table, ' --- ', s_col),to="ASCII//TRANSLIT")
    v_str <- c(v_str, i_str)
    png(paste0('../DATA/export/images/',i_str,'.png'), width = 850, height = 600 )
    cat(paste("treating column", s_col,"\n"))
    plot_col(table[s_col], s_table, num)
    dev.off()
  }
  cat("DONE\n")     
}

## Ouptut the parsed structure
if(STR){
sink('../DATA/export/images_index.txt')
cat(paste0(v_str, "\n", collapse=""))
sink()
}