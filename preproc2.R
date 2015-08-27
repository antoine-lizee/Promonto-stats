## Analysis for the promonto Paper (Daphne Lizee)
# Importation and cleaning of the data.
#
# Copyright Antoine Lizee @ UCSF the 08/12/2014 antoine.lizee@ucsf.com

# rm(list=ls())
if (!exists("debug.b"))
  debug.b <- F

### IMPORTING -------------------

##METHOD 1
if (method1 <- F) {
  atcd0 <- read.csv('./ATCD.cur.csv', skip = 1)
  tech0 <- read.csv('./TECHNIQUE.cur.csv', skip = 1)
  post0 <- read.csv('./POSTOP.cur.csv', skip = 1)
  atcd0 <- atcd0[order(atcd0$N.DOSSIER),]
  tech0 <- tech0[order(tech0$N.DOSSIER),]
  post0 <- post0[order(post0$N.DOSSIER),]
}
## METHOD 2
require(XLConnect)
wb <- loadWorkbook('/media/FD/Dropbox/promonto/Datas prothese post + explications  .xlsx')
# wb <- loadWorkbook('./Updated data//Datas prothese post + explications   (4).xlsx')
sheet_names <- c('atcd', 'tech', 'post', 'complic')
suppressWarnings(dumb <- mapply(function (sheet_id, sheet_name) {
  assign( sheet_name, readWorksheet(wb, sheet = sheet_id, startRow = 2) , envir=.GlobalEnv) },
  1:3,
  sheet_names ))

atcd <- atcd[order(atcd$N.DOSSIER),]
tech <- tech[order(tech$N.DOSSIER),]
post <- post[order(post$N.DOSSIER),]

## Check for duplicates #OK
# atcd[rep(which(duplicated(atcd$N.DOSSIER)), each=2)+-1:0, ]
# tech[rep(which(duplicated(tech$N.DOSSIER)), each=2)+-1:0, ]
# post[rep(which(duplicated(post$N.DOSSIER)), each=2)+-1:0, ]

## Create some big merged table
fulltable0 <- merge(x=atcd,y=tech,by='N.DOSSIER', all.x = F, all.y=F)
fulltable <- merge(x=fulltable0, y= post, by='N.DOSSIER', all = F)



## ASSIGNING TYPE ------

getType <- function(SS) {
  if (all(is.na(SS)))
    return(NA)
  if (is.factor(SS))
    SS <- as.character(SS)
  SS <- SS[!is.na(SS)]  ######################################## !
  n_SS <- length(SS)
  n_char <- length(unique(as.character(SS)))
  n_num <- length(unique(suppressWarnings(na.omit(as.numeric(SS)))))
  n_na_num <- sum( !is.na(SS) & is.na(suppressWarnings(as.numeric(SS))) ) # First condition useless, see above
  return( if (n_num > 15) 'NUM'
          else if (n_num/n_SS > 0.7) 'NUM'
          else if (n_char <= 2) 'FAC2'
          else if (n_na_num > 0.1*n_SS) { if (n_char > 5) 'LABEL' else 'FAC'}
          else 'DISC')
}

getTypeFull <- function(TABLE) {
  nc <- length (TABLE)
  types <- c()
  for (ic in 1:nc) {
    namei <- names(TABLE)[ic]
    if (grepl( 'date', namei, ignore.case=T)) {
      typei <- 'DATE'
    } else {
      typei <- getType(TABLE[[ic]])
    }
    types[ic] <- typei
  }
  names(types) <- names(TABLE)
  return(types)
}

TYPES <- list( atcd = getTypeFull(atcd),
               tech = getTypeFull(tech),
               post = getTypeFull(post))

## CLEANING ----------------------
# detach('package:Hmisc')
require(car)
require(reshape)

check_factors <- function(x) {
  car::recode(x, "'0'='O'; 'o'='O'; 'n'='N'; 'N '='N'; 'NP'=NA; 'NR'=NA; '?'=NA; ' '=NA; 'NA'=NA")
}
for (sheetname in sheet_names) {
  df <- get(sheetname)
  df <- data.frame(lapply(df,as.vector))
  b_num <- TYPES[[sheetname]] %in% c('NUM', 'DISC')
  b_fac <- TYPES[[sheetname]] %in% c('FAC', 'FAC2')
  suppressWarnings(df[b_num] <- lapply(df[b_num], function(x) as.numeric(as.character(x))) )
  df[b_fac] <- lapply(df[b_fac], check_factors)
  assign(paste0(sheetname, "Untreated"), get(sheetname))
  assign(sheetname, df)
} 
# Some pinpoints
atcd[[42]] <- car::recode(atcd[[42]], "'0'='O'")
tech[[9]] <- car::recode(tech[[9]], "'PF '='PF'") # beware of Hmisc masking "recode"
post[[31]] <- car::recode(post[[31]], "'P'='O'") 
post[[32]] <- car::recode(post[[32]], "'P'='O'") 
post[[33]] <- car::recode(post[[33]], "'P'='O'") 

# redo the typing
TYPES <- list( atcd = getTypeFull(atcd),
               tech = getTypeFull(tech),
               post = getTypeFull(post))

# transform_df <- function(name, fun, env = .GlobalEnv) {
#   df <- get(name, envir=env)
#   
#   assign(name, df, envir=env)
# }

# Create lsit object for the tables
TABLES2 <- list(atcd2 = atcd, tech2=tech, post2=post)

## Put the types in the names
for (table_name in names(TABLES2)) {
  names(TABLES2[[table_name]]) <- mapply(paste, names(TABLES2[[table_name]]), paste0('[', TYPES[[substr(table_name, 1, nchar(table_name)-1)]], ']'))
}
dumb <- mapply(function(table, name) assign(name,table,envir=.GlobalEnv), TABLES2, names(TABLES2))


## CREATE ANALYSIS DATA -------------


# Importing data ----------------------------------------------------------

# create letter -> number LUT
LUT <- 1:(26*4)
names(LUT) <- paste0(rep(c("", 'A', 'B', 'C'), each=26), LETTERS)

ADcol <- list(ATCD=c('F', #: age 
                     'G', #: ATCD 
                     'I', #: nb AVB
                     'J', #: cesar oui/non
                     'K', #: poids du plus gros BB: je t'ai mis les cases des dames n'ayant pas eu d'enfant en gris : non applicable (et non NR)
                     'M', #: ATCD hysterectomie
                     'O', #: ATCD chir statique
                     'W', #: IMC ##TOBECOMPUTED
                     'Y', #: cystocele
                     'Z', #: hysteroptose/cystoptose
                     'AA', #: rectocele
                     'AF', #: TVL 
                     'AG', #: POPQ ant
                     'AH', #: POPQ median
                     'AI', #: POPQ post
                     'AN', #: IUE clinique
                     'AO', #: IU masquée 
                     'AP', #: IU mixte  
                     #CCG pré op:
                     'AY', #: cysto
                     'AZ', #: Hysteroptose /Colpoptose
                     'BA', #: Rectocele
                     'BC', # 2e phase : Enterocele
                     #                      'BF', # 2e phase : Rectocele in action
                     'BJ', #Fond vaginal in action BJ (2eme phase)
                     'BG', #Diff fond vaginal  (3eme)
                     'BP', #Rectocele in action BP (2eme phase)
                     'BR'), #Perinee descendant BR (2eme phase)
              #                      'BG', #: differentiel fond vaginal
              #                      'BJ', #: différentiel col vesical
              #                      'BM', #: differentiel fond vesical
              #                      'BP'), #: différentiel rectocele
              TECH = c('I', #: PF ou (PF + HT ou subTNC ou subTC)
                       'G', #: Durée opératoire
                       'J', #: geste urinaire
                       'R', #: type prothese
                       'H') #: recul opératoire)
)

AD <- merge(atcd[c(4, LUT[ADcol$ATCD])], tech[c(4, LUT[ADcol$TECH])], 'N.DOSSIER')

TYPES$AD <- c(TYPES$atcd[c(4, LUT[ADcol$ATCD])], TYPES$tech[LUT[ADcol$TECH]])
namesAD <- c(names(atcd)[c(4, LUT[ADcol$ATCD])], names(tech)[LUT[ADcol$TECH]])
summary(namesAD == names(AD))

RESULTScol <- list(POSTOP=c('H', #Recul ##TOBECOMPUTED
                            'K', #BW posterieur: K
                            'T', # 2e phase POPQ ant
                            'U', # 2e phase POPQ med
                            'V', #: POP Q postérieur :V 
                            'R', #: TVL: colonne R
                            'AS', #:   hysteroptose: AS
                            'AT', #:   rectocele AT Rectocele CCG: critère secondaire
                            'AY', #: différentiel fond vaginal AY
                            'BH', #: différentiel rectocele BH
                            'AR', #: Cystocele (3eme)
                            'BI', #: Enterocele (3eme)
                            #                            'AE', #: " êtes vous satisfaite ? " : AE
                            #                             'AF', #: si c'etait a refaire, le referiez vous ?  : AF
                            #                             'AG', #: le recommanderiez vous à une copine? : AG
                            'M', #: constipation apparue/ accrue : M
                            'N', #: dyschésie/ constip terminale: N
                            'O'), #: manoeuvres : O
                   TECH = c('U', #:complications per opératoires : U
                            'X', #:complications post opératoires: X
                            'Y') #:reprise chirurgicale pour complication: Y
)

RESULTS <- merge(post[c(4, LUT[RESULTScol$POSTOP])], tech[c(4, LUT[RESULTScol$TECH])], 'N.DOSSIER')

TYPES$RESULTS <- c(TYPES$post[c(4, LUT[RESULTScol$POSTOP])], TYPES$tech[LUT[RESULTScol$TECH]])
namesRESULTS <- c(names(post)[c(4, LUT[RESULTScol$POSTOP])], names(tech)[LUT[RESULTScol$TECH]])
summary(namesRESULTS == names(RESULTS))



# Compute analysis variables ----------------------------------------------

AD$IMC <- atcd$POIDS / atcd$TAILLE^2 * 100^2
TYPES$AD["IMC"] <- "NUM"

RESULTS <- data.frame(
  RESULTS[1:6],
  POPQ.TOUS = with(post, POPQ.ant.PO < 2& 
                     POPQ.méd.PO < 2 & 
                     POPQ.POST.PO < 2 ),
  RESULTS[7:8],
  SATISFAITE.SUMMARY = with(post, SATISFAITE.O.N == "O" & 
                              Si.A.refaire.OK.O.N == "O" & 
                              Si.A.refaire.OK.O.N== "O" ),
  RESULTS[9:length(RESULTS)]
)
TYPES$RESULTS[c("POPQ.TOUS","SATISFAITE.SUMMARY")] <- "FAC2"

dateChir <- as.Date(postUntreated$Date.chir)
dateCs <- as.Date(postUntreated$DATE.cs)
recul <- round(as.numeric(dateCs - dateChir) / 30.4368, digits = 1)
#Wanna see?
# data.frame(dateChir, dateCs, n.dossier = post$N.DOSSIER, Recul = recul)
#Compute Recul variable
RESULTS$Recul. <-  recul
#Change type
TYPES$RESULTS["Recul."] <- "NUM"

if (debug.b) {
  with(post, data.frame(SATISFAITE.O.N,
                        Si.A.refaire.OK.O.N ,
                        Si.A.refaire.OK.O.N, 
                        QUESTIONSPATIENT = SATISFAITE.O.N == "O" & 
                          Si.A.refaire.OK.O.N == "O" & 
                          Si.A.refaire.OK.O.N == "O" ))
  with(post, data.frame(POPQ.ant.PO,
                        POPQ.méd.PO,
                        POPQ.POST.PO,
                        POPQ.TOUS = POPQ.ant.PO < 2& 
                          POPQ.méd.PO < 2 & 
                          POPQ.POST.PO < 2 ))
}



# Create classification variable ------------------------------------------

pop <- tech[c('NOM', 'PRENOM',  "N.DOSSIER", "fix.inf.proth.post", "fix.sup.protpost")]
pop$pop1 <- factor(pop$fix.inf.proth.post, level=c("VAGIN", "ELEVATEURS"))
pop$pop2 <- factor(pop$fix.sup.protpost, level=c("PROMONTOIRE", "PERITOINE", "US"))
levels(pop$pop2) <- c("PROMO", "PERI+US", "PERI+US")

# Check Name Consistency
# rbind(pop$NOM, as.character(atcd$NOM), as.character(tech$NOM), as.character(post$NOM.))

fullAD <- merge(AD, pop, by = 'N.DOSSIER')
fullRESULTS <- merge(RESULTS, pop, by = 'N.DOSSIER')

stopifnot(all(diff(
  sapply(c(TABLES2, list(AD=AD), list(fullAD=fullAD), list(fullRESULTS=fullRESULTS)), nrow)
)) == 0)


# Save Data ---------------------------------------------------------------
dir.create("/media/FD/Dropbox/promonto/Pre-Processed/", showWarnings = F)
wb <- XLConnect::loadWorkbook(create = T, '/media/FD/Dropbox/promonto/Pre-Processed/Pre-processed_data.xlsx')
createSheet(wb, name = "AD")
createSheet(wb, name = "RESULTS")
XLConnect::writeWorksheet(object = wb, data = AD, sheet = "AD")
XLConnect::writeWorksheet(object = wb, data = RESULTS, sheet = "RESULTS")
XLConnect::saveWorkbook(wb)

save(file = '/media/FD/Dropbox/promonto/Pre-Processed/Pre-processed_data.RData', AD, RESULTS, TYPES, pop)

source('/media/FD/BIOSCREEN/R/DB_function.R')

pdf('/media/FD/Dropbox/promonto/Pre-Processed/AnalyseDemographique_QuickGraphs.pdf', width = 10, height = 7)
quick_analysis_table(AD)
dev.off()
pdf('/media/FD/Dropbox/promonto/Pre-Processed/Resultats_QuickGraphs.pdf', width = 10, height = 7)
quick_analysis_table(RESULTS)
dev.off()




