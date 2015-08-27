rm(list=ls())


### IMPORTING -------------------

##METHOD 1

atcd0 <- read.csv('./ATCD.cur.csv', skip = 1)
tech0 <- read.csv('./TECHNIQUE.cur.csv', skip = 1)
post0 <- read.csv('./POSTOP.cur.csv', skip = 1)
atcd0 <- atcd0[order(atcd0$N.DOSSIER),]
tech0 <- tech0[order(tech0$N.DOSSIER),]
post0 <- post0[order(post0$N.DOSSIER),]

## METHOD 2
require(XLConnect)
wb <- loadWorkbook('./curr.xlsx')
sheet_names <- c('atcd','tech','post')
dumb <- mapply(function (sheet_id, sheet_name) {
                  assign( sheet_name, readWorksheet(wb, sheet = sheet_id, startRow = 2) , envir=.GlobalEnv) },
               1:3,
               sheet_names )

atcd <- atcd[order(atcd$N.DOSSIER),]
tech <- tech[order(tech$N.DOSSIER),]
post <- post[order(post$N.DOSSIER),]
#Remove useless columns
col_atcd <- c(5, 8, 12, 14, 16:20, 36:39, 45, 47:49,51:58, 60:61, 63:64, 66:67, 69:77)
col_tech <- c(1, 11:12, 14:17, 22:23, 26:27, 29:36)
col_post <- c(25:27, 30, 34:42, 49:50, 52:53, 55:56, 58:59, 62:71)
atcd[col_atcd] <- list(NULL)
tech[col_tech] <- list(NULL)
post[col_post] <- list(NULL)

##Quickly compare the methods
# mapply(function(x,y) summary(x==y), atcd, atcd0)
summary(names(atcd)==names(atcd0))
summary(class(atcd)==class(atcd0))
b <- names(atcd)!=names(atcd0)
rbind(names(atcd)[b], names(atcd0)[b])

##Create some new variables
pop <- tech[c('NOM', 'PRENOM',  "N.DOSSIER", "fix.inf.proth.post", "fix.sup.protpost")]
pop$pop1 <- factor(pop$fix.inf.proth.post, level=c("VAGIN", "ELEVATEURS"))
pop$pop2 <- factor(pop$fix.sup.protpost, level=c("PROMONTOIRE", "PERITOINE", "US"))
levels(pop$pop2) <- c("PROMO", "PERI+US", "PERI+US")

## Check for duplicates #OK
# atcd[rep(which(duplicated(atcd$N.DOSSIER)), each=2)+-1:0, ]
# tech[rep(which(duplicated(tech$N.DOSSIER)), each=2)+-1:0, ]
# post[rep(which(duplicated(post$N.DOSSIER)), each=2)+-1:0, ]

fulltable0 <- merge(x=atcd,y=tech,by='N.DOSSIER', all.x = F, all.y=F)
fulltable <- merge(x=fulltable0, y= post, by='N.DOSSIER', all = F)

##Check Data
source('/media/FD/BIOSCREEN/R/DB_function.R')
# oldpar <- par(ask=T)
# quick_analysis_table(atcd)
# par(oldpar)


## CLEANING ----------------------
# detach('package:Hmisc')
require(car)
require(reshape)
num <- list()
num[['atcd']] <- c(12, 18, 21, 22, 23, 26, 31, 32, 34, 35, 36, 37)
num[['tech']] <- c(6,7)
# atcd[num_atcd] <- lapply(atcd[num_atcd], function(x) as.numeric(as.character(x)))
# tech[num_tech] <- tech(atcd[num_tech], function(x) as.numeric(as.character(x)))

#O=o, n=N, " "=NA=NP = NR = ? =
# + transformation in factors for the rest
check_factors <- function(x) {
  car::recode(x, "'o'='O'; 'n'='N'; 'N '='N'; 'NP'=NA; 'NR'=NA; '?'=NA; ' '=NA; 'NA'=NA")
}
for (name in sheet_names) {
  df <- get(name)
  df <- data.frame(lapply(df,as.vector))
  suppressWarnings(df[num[[name]]] <- lapply(df[num[[name]]], function(x) as.numeric(as.character(x))) )
  b <- sapply(df,is.factor)
  df[b] <- lapply(df[b], check_factors)
  assign(name, df)
} 
# Some pinpoints
atcd[[29]] <- car::recode(atcd[[29]], "'0'='O'")
tech[[8]] <- car::recode(tech[[8]], "'PF '='PF'") # beware of Hmisc masking "recode"

# transform_df <- function(name, fun, env = .GlobalEnv) {
#   df <- get(name, envir=env)
#   
#   assign(name, df, envir=env)
# }


rbind(pop$NOM, as.character(atcd$NOM), as.character(tech$NOM), as.character(post$NOM.))

## ASSIGNING TYPE ------
DESC <- list()
faclevels <- c('label', 'O-N', 'fac', 'disc', 'num', 'date')
DESC[['atcd']] <- factor(c(1,1,1,1,5,
                           1,4,2,1,2,
                           2,5,1,1,1,
                           4,4,4,4,2,
                           4,4,4,4,4,
                           4,2,2,2,2,
                           5,5,2,5,5,
                           5,5,1,1), levels=1:6, labels = faclevels)
names(DESC[['atcd']]) <- names(atcd)
DESC[['tech']] <- factor(c(1,1,5,6,6,
                           5,4,3,2,2,
                           3,3,3,2,2,
                           2,2), levels=1:6, labels = faclevels)
names(DESC[['tech']]) <- names(tech)
DESC[['post']] <- factor(c(1,1,1,5,6,
                           6,6,6,4,4,
                           4,2,2,2,2,
                           4,4,5,2,4,
                           4,4,2,2,2,
                           2,2,2,2,2,
                           2,2,2,2,2,
                           5,1,1,5,2,
                           1,1,1,1,1,
                           1,1,1,1), levels=1:6, labels = faclevels)
names(DESC[['post']]) <- names(post)

TABLES2 <- list(atcd2 = atcd, tech2=tech, post2=post)


for (table_name in names(TABLES2)) {
  names(TABLES2[[table_name]]) <- mapply(paste, names(TABLES2[[table_name]]), paste0('[', DESC[[substr(table_name, 1, nchar(table_name)-1)]], ']'))
}
dumb <- mapply(function(table, name) assign(name,table,envir=.GlobalEnv), TABLES2, names(TABLES2))




