##############################################################################
## Read data
##############################################################################
data_oslo <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Oslo_20161212/Oslo_20170616.xlsx", 
                       sheetName = "Komplett_data", stringsAsFactors = FALSE, encoding = "UTF-8")

data_stockholm_MR <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Stockholm_20161212/T_MR_data.xlsx",
                               sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
data_stockholm_standard <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Stockholm_20161212/T_Standard_Biopsi.xlsx",
                                     sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
data_stockholm_target <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Stockholm_20161212/T_Target_Biopsi.xlsx",
                                   sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")

data_tonsberg_MR <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Tonsberg_20161212/T_MR_data.xlsx",
                              sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
data_tonsberg_standard <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Tonsberg_20161212/T_Standard_Biopsi.xlsx",
                                    sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
data_tonsberg_target <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Tonsberg_20161212/T_Target_Biopsi.xlsx",
                                  sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")

s3m_pred <- read.table("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/output.txt", header = TRUE, stringsAsFactors = FALSE)

##############################################################################
## Remove one duplicated in S3M predictions.
##############################################################################
s3m_dup <- duplicated(s3m_pred$studieid)
s3m_pred <- s3m_pred[!s3m_dup, ]

##############################################################################
## Clean Oslo
##############################################################################
data_oslo <- select(data_oslo, -starts_with("NA.")) ## Drop columns with only NA.
data_oslo <- select(data_oslo, -matches("STHLM.3.study.ID.1")) ## Duplicated variable.
data_oslo <- filter(data_oslo, !is.na(STHLM.3.study.ID))
data_oslo$birth_date <- as.Date(paste0("19", data_oslo$Født.År, "-07-01")) ## Assume all in oslo born in the middle of the year.
data_oslo[is.na(data_oslo$MR.date) | data_oslo$MR.date < as.Date("2015-01-01"), "MR.date"] <- as.Date("2017-01-01") ## A few with obviously wrong dates of MRI.
data_oslo$age <- data_oslo$Alder
data_oslo$Mottagning_standard <- NA
data_oslo$Mottagning_target <- NA
data_oslo$Pnr <- NA

##############################################################################
## Clean Stockholm Tonsberg
##############################################################################
data_tonsberg_standard <- select(data_tonsberg_standard, -starts_with("Remit")) ## Drop columns with only NA.
data_tonsberg_target <- select(data_tonsberg_target, -starts_with("Remit")) ## Drop columns with only NA.
data_stockholm_standard <- select(data_stockholm_standard, -starts_with("Remit")) ## Drop columns with only NA.
data_stockholm_target <- select(data_stockholm_target, -starts_with("Remit")) ## Drop columns with only NA.
if(external_age_tonsberg){
  data_tonsberg_age <- read.xlsx("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/Tonsberg_20161212/Age_NO_T_2_Peter.xlsx",
                                 sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
  data_tonsberg_age <- data_tonsberg_age[substr(data_tonsberg_age$pnr,  1, 3) == "SIV", ]
  data_tonsberg_age$Studie_id <- as.numeric(substr(data_tonsberg_age$pnr,  4, 6))
  data_tonsberg_age$Pnr <- data_tonsberg_age$PNR
  data_tonsberg_age <- data_tonsberg_age[, c("Studie_id", "Pnr")]
  data_tonsberg_MR$Pnr <- NULL
  data_tonsberg_MR <- left_join(data_tonsberg_MR, data_tonsberg_age, by = "Studie_id")
}

##############################################################################
## Change obviously WRONG study ID
##############################################################################
fun_STO_change_id <- function(x){
  ## A function to correct typos in the variable Studie_id in the three sthlm data sets.
  ## x: A stockholm data.
  EXPR <- x$Studie_id < 6808100000 | x$Studie_id > 6808200000
  wrong_id <- x[EXPR, ]$Studie_id
  if (length(wrong_id) > 0){
  right_id <- paste0("6808100", substr(wrong_id, nchar(wrong_id) - 2, nchar(wrong_id)))
  x[EXPR, ]$Studie_id <- right_id
  x$Studie_id <- as.numeric(x$Studie_id)
  }
  return(x)
}

data_stockholm_MR <- fun_STO_change_id(data_stockholm_MR)
data_stockholm_standard <- fun_STO_change_id(data_stockholm_standard)
data_stockholm_target <- fun_STO_change_id(data_stockholm_target)

## Study ID for Oslo is OK and for Tonsberg there is one that belong to STHLM (probably an example ID)
data_tonsberg_MR <- filter(data_tonsberg_MR, Studie_id < 10000)

##############################################################################
## Join Stockholm data and combine dublicated columns to one column.
## Except Biopsy_Date (unique to standard and target) it seems that the rest
## of the duplicated columns are common to all three data sets.
##############################################################################
## MR and standard:
data_stockholm <- full_join(data_stockholm_MR, data_stockholm_standard, by = "Studie_id")
tmp_old <- names(select(data_stockholm, contains(".x")))
tmp_new <- substr(tmp_old, 1, nchar(tmp_old) - 2)
setnames(data_stockholm, old = tmp_old, new = tmp_new)
for (i in tmp_new){
  data_stockholm[[i]] <- ifelse(!is.na(data_stockholm[[i]]), data_stockholm[[i]], data_stockholm[[paste0(i, ".y")]])
}
data_stockholm <- select(data_stockholm, -ends_with(".y"))
rm(tmp_old, tmp_new)

## ..and target:
data_stockholm <- full_join(data_stockholm, data_stockholm_target, by = "Studie_id")
colnames(data_stockholm)[which(names(data_stockholm) == "Biopsy_Date.x")] <- "Biopsy_Date_standard"
colnames(data_stockholm)[which(names(data_stockholm) == "Biopsy_Date.y")] <- "Biopsy_Date_target"
colnames(data_stockholm)[which(names(data_stockholm) == "Mottagning.x")] <- "Mottagning_standard"
colnames(data_stockholm)[which(names(data_stockholm) == "Mottagning.y")] <- "Mottagning_target"
tmp_old <- names(select(data_stockholm, contains(".x")))
tmp_new <- substr(tmp_old, 1, nchar(tmp_old) - 2)
setnames(data_stockholm, old = tmp_old, new = tmp_new)
for (i in tmp_new){
  data_stockholm[[i]] <- ifelse(!is.na(data_stockholm[[i]]), data_stockholm[[i]], data_stockholm[[paste0(i, ".y")]])
}
data_stockholm <- select(data_stockholm, -ends_with(".y"))
rm(tmp_old, tmp_new)

##############################################################################
## Join Tonsberg data and combine dublicated columns to one column.
## Except Biopsy_Date (unique to standard and target) it seems that the rest
## of the duplicated columns are common to all three data sets.
##############################################################################
## MR and standard:
data_tonsberg <- full_join(data_tonsberg_MR, data_tonsberg_standard, by = "Studie_id")
tmp_old <- names(select(data_tonsberg, contains(".x")))
tmp_new <- substr(tmp_old, 1, nchar(tmp_old) - 2)
setnames(data_tonsberg, old = tmp_old, new = tmp_new)
for (i in tmp_new){
  data_tonsberg[[i]] <- ifelse(!is.na(data_tonsberg[[i]]), data_tonsberg[[i]], data_tonsberg[[paste0(i, ".y")]])
}
data_tonsberg <- select(data_tonsberg, -ends_with(".y"))
rm(tmp_old, tmp_new)

## ..and target:
data_tonsberg <- full_join(data_tonsberg, data_tonsberg_target, by = "Studie_id")
colnames(data_tonsberg)[which(names(data_tonsberg) == "Biopsy_Date.x")] <- "Biopsy_Date_standard"
colnames(data_tonsberg)[which(names(data_tonsberg) == "Biopsy_Date.y")] <- "Biopsy_Date_target"
colnames(data_tonsberg)[which(names(data_tonsberg) == "Mottagning.x")] <- "Mottagning_standard"
colnames(data_tonsberg)[which(names(data_tonsberg) == "Mottagning.y")] <- "Mottagning_target"
tmp_old <- names(select(data_tonsberg, contains(".x")))
tmp_new <- substr(tmp_old, 1, nchar(tmp_old) - 2)
setnames(data_tonsberg, old = tmp_old, new = tmp_new)
for (i in tmp_new){
  data_tonsberg[[i]] <- ifelse(!is.na(data_tonsberg[[i]]), data_tonsberg[[i]], data_tonsberg[[paste0(i, ".y")]])
}
data_tonsberg <- select(data_tonsberg, -ends_with(".y"))
rm(tmp_old, tmp_new)

##############################################################################
## Create indicator variables to know which data set (of MR, standard and target)
## each subject is in.
##############################################################################
data_stockholm$in_MR <- as.numeric(data_stockholm$Studie_id %in% data_stockholm_MR$Studie_id) 
data_stockholm$in_standard <- as.numeric(data_stockholm$Studie_id %in% data_stockholm_standard$Studie_id) 
data_stockholm$in_target <- as.numeric(data_stockholm$Studie_id %in% data_stockholm_target$Studie_id) 

data_tonsberg$in_MR <- as.numeric(data_tonsberg$Studie_id %in% data_tonsberg_MR$Studie_id) 
data_tonsberg$in_standard <- as.numeric(data_tonsberg$Studie_id %in% data_tonsberg_standard$Studie_id) 
data_tonsberg$in_target <- as.numeric(data_tonsberg$Studie_id %in% data_tonsberg_target$Studie_id) 

##############################################################################
## Idicator for Site Stockholm and Tonsberg before combining.
##############################################################################
data_stockholm$Site <- "Stockholm"
data_tonsberg$Site <- "Tonsberg"

##############################################################################
## Row bind Stockholm and Tonsberg.
##############################################################################
data_tonsberg$Biopsy_Date_standard <- as.Date(data_tonsberg$Biopsy_Date_standard, "%d.%m.%Y")
data_tonsberg$Biopsy_Date_target <- as.Date(data_tonsberg$Biopsy_Date_target, "%d.%m.%Y")
data_tonsberg$MR_date <- as.Date(data_tonsberg$MR_date, "%d.%m.%Y")

data_sto_ton <- rbind(data_stockholm, data_tonsberg)

##############################################################################
## Remove the raw data sets.
##############################################################################
rm(data_stockholm, data_stockholm_MR, data_stockholm_standard, data_stockholm_target,
   data_tonsberg, data_tonsberg_MR, data_tonsberg_standard, data_tonsberg_target, i, fun_STO_change_id)

##############################################################################
## Create Age Sto_ton
##############################################################################
## Add 19 in Pnr if it does not exist (but Pnr exist).
data_sto_ton$Pnr <- ifelse(data_sto_ton$Pnr == "", NA, data_sto_ton$Pnr)
data_sto_ton$Pnr <- ifelse(substr(data_sto_ton$Pnr, 1, 2) != "19", paste0("19", data_sto_ton$Pnr), data_sto_ton$Pnr)
data_sto_ton$Pnr <- ifelse(nchar(data_sto_ton$Pnr) == 12,
                           paste0(substr(data_sto_ton$Pnr, 1, 8), "-", substr(data_sto_ton$Pnr, 9, 12)), 
                           data_sto_ton$Pnr)

data_sto_ton$birth_date <- as.Date(substr(data_sto_ton$Pnr, 1, 8), "%Y%m%d")
data_sto_ton$age <- as.numeric(data_sto_ton$MR_date - data_sto_ton$birth_date) / 365.24

##############################################################################
## Create volume for both Sto_ton and Oslo.
##############################################################################
## STO_TON
cols <- names(select(data_sto_ton, contains("Largest_diam")))
data_sto_ton[cols] <- lapply(data_sto_ton[cols], as.numeric)
data_sto_ton[cols] <- lapply(data_sto_ton[cols], function(x) ifelse(x == 99999, NA, x))
data_sto_ton$Tumor_volume_T1 <- with(data_sto_ton, (T1_Largest_diam_W * T1_Largest_diam_D * T1_Largest_diam_H) * 4 * pi / (3 * 2^3)) ## W*D*H*pi/6
data_sto_ton$Tumor_volume_T2 <- with(data_sto_ton, (T2_Largest_diam_W * T2_Largest_diam_D * T2_Largest_diam_H) * 4 * pi / (3 * 2^3))
data_sto_ton$Tumor_volume_T3 <- with(data_sto_ton, (T3_Largest_diam_W * T3_Largest_diam_D * T3_Largest_diam_H) * 4 * pi / (3 * 2^3))

## OSLO
# cols <- names(select(data_oslo, contains(".diameter.")))
data_oslo$Tumor_volume_T1 <- with(data_oslo, (Tumor1.diameter.x * Tumor1.diameter.y * Tumor.1.diameter.z) * 4 * pi / (3 * 2^3))
data_oslo$Tumor_volume_T2 <- with(data_oslo, (Tumor.2.diameter.x * Tumor.2.diameter.y * Tumor.2.diameter.z) * 4 * pi / (3 * 2^3))
data_oslo$Tumor_volume_T3 <- with(data_oslo, (Tumor.3.diameter.x * Tumor.3.diameter.y * Tumor3.diameter.z) * 4 * pi / (3 * 2^3))

## Drop original tumor diameter variables in Sto_ton and Oslo
data_sto_ton <- select(data_sto_ton, -contains("Largest_diam"))
data_oslo <- select(data_oslo, -contains(".diameter."))

##############################################################################
## Palp.
##############################################################################
data_sto_ton$Palpasjon.overall <- NA
data_sto_ton[data_sto_ton$Palpationsfynd_T1 == TRUE & !is.na(data_sto_ton$Palpationsfynd_T1), ]$Palpasjon.overall <- "T1"
data_sto_ton[data_sto_ton$Palpationsfynd_T2 == TRUE & !is.na(data_sto_ton$Palpationsfynd_T2), ]$Palpasjon.overall <- "T2"
data_sto_ton[data_sto_ton$Palpationsfynd_T3 == TRUE & !is.na(data_sto_ton$Palpationsfynd_T3), ]$Palpasjon.overall <- "T3"
# data_sto_ton[data_sto_ton$Palpationsfynd_T4 == TRUE & !is.na(data_sto_ton$Palpationsfynd_T4), ]$Palpasjon.overall <- "T4"

##############################################################################
## Create Biopsy length
##############################################################################
## Systematic Oslo
vars <- c("A1.Biopsy.length", "A2.Biopsy.length", "A3.Biopsy.length", 
          "A4.Biopsy.length", "B1.Biopsy.length", "B2.Biopsy.length", "B3.Biopsy.length", 
          "B4.Biopsy.length", "C1.Biopsy.length", "C2.Biopsy.length")
data_oslo$Length_biopsy_standard <- rowSums(data_oslo[vars], na.rm = TRUE)

## Target Oslo
vars <- c("Tumor1a.Biopsy.length", "Tumor1b.Biopsy.length", "Tumor1c.Biopsy.length", 
          "Tumor.2a.Biopy.length", "Tumor2b.Biopsy.length", "Tumor2c.biopsy.length", 
          "Tumor3a.Biopsy.length", "Tumor3b.Biopsy.length")
data_oslo$Length_biopsy_target <- rowSums(data_oslo[vars], na.rm = TRUE)

## Systematic Sto_ton
vars <- c("A1_Biopsy_length", "A2_Biopsy_length", "A3_Biopsy_length", 
          "A4_Biopsy_length", "B1_Biopsy_length", "B2_Biopsy_length", "B3_Biopsy_length", 
          "B4_Biopsy_length", "C1_Biopsy_length", "C2_Biopsy_length", "C3_Biopsy_length", 
          "C4_Biopsy_length")
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 999999, NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 99999, NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 1299999.0, NA, x))
data_sto_ton$Length_biopsy_standard <- rowSums(data_sto_ton[vars], na.rm = TRUE)

## Target Sto_ton
vars <- c("A1d_Biopsy_length", "A2d_Biopsy_length", 
          "A3d_Biopsy_length", "A4d_Biopsy_length", "B1d_Biopsy_length", 
          "B2d_Biopsy_length", "B3d_Biopsy_length", "B4d_Biopsy_length", 
          "C1d_Biopsy_length", "C2d_Biopsy_length", "C3d_Biopsy_length", 
          "C4d_Biopsy_length", "A1v_Biopsy_length", "A2v_Biopsy_length", 
          "A3v_Biopsy_length", "A4v_Biopsy_length", "B1v_Biopsy_length", 
          "B2v_Biopsy_length", "B3v_Biopsy_length", "B4v_Biopsy_length", 
          "C1v_Biopsy_length", "C2v_Biopsy_length", "C3v_Biopsy_length", 
          "C4v_Biopsy_length")
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 999999, NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 99999, NA, x))
data_sto_ton$Length_biopsy_target <- rowSums(data_sto_ton[vars], na.rm = TRUE)

##############################################################################
## Create Tumor length
##############################################################################
## Systematic Oslo
vars <- c("A1.Tumor.length", "A2.Tumor.length", "A3.Tumor.length", "A4.Tumor.length", 
          "B1.Tumor.length", "B2.Tumor.length", "B3.Tumor.length", "B4.Tumor.length", 
          "C1.Tumor.length", "C2.Tumor.length") 
data_oslo$Length_tumor_standard <- rowSums(data_oslo[vars], na.rm = TRUE)

## Target Oslo
vars <- c("Tumor1a.Cancer.length", "Tumor1b.Cancer.length", "Tumor.1c.Cancer.length", 
          "Tumor2a.Cancerlength", "Tumor2b.Cancerlength", "Tumor.2c.Cancer.length", 
          "Tumor3a.Cancerlength", "Tumor3b.Cancerlenth", "Tumor3C.Cancerlength") 
data_oslo$Length_tumor_target <- rowSums(data_oslo[vars], na.rm = TRUE)

## Systematic Sto_ton
vars <- c("A1_Tumor_length", "A2_Tumor_length", "A3_Tumor_length", "A4_Tumor_length", 
          "B1_Tumor_length", "B2_Tumor_length", "B3_Tumor_length", "B4_Tumor_length", 
          "C1_Tumor_length", "C2_Tumor_length", "C3_Tumor_length", "C4_Tumor_length")
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 999999, NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 99999, NA, x))
# data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 1299999.0, NA, x))
data_sto_ton$Length_tumor_standard <- rowSums(data_sto_ton[vars], na.rm = TRUE)

## Target Sto_ton
vars <- c("A1d_Tumor_length", "A2d_Tumor_length", "A3d_Tumor_length", "A4d_Tumor_length", 
          "B1d_Tumor_length", "B2d_Tumor_length", "B3d_Tumor_length", "B4d_Tumor_length", 
          "C1d_Tumor_length", "C2d_Tumor_length", "C3d_Tumor_length", "C4d_Tumor_length", 
          "A1v_Tumor_length", "A2v_Tumor_length", "A3v_Tumor_length", "A4v_Tumor_length", 
          "B1v_Tumor_length", "B2v_Tumor_length", "B3v_Tumor_length", "B4v_Tumor_length", 
          "C1v_Tumor_length", "C2v_Tumor_length", "C3v_Tumor_length", "C4v_Tumor_length")
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 999999, NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 99999, NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == 9999, NA, x))
data_sto_ton$Length_tumor_target <- rowSums(data_sto_ton[vars], na.rm = TRUE)

##############################################################################
## Create Gleason - Use the highest value. 9 means 4+5 and 9.5 means 5+4 (we think..)
##############################################################################
## Systematic Oslo
vars <- c("A1.Gleason", "A2.Gleason",
          "A3.Gleason", "A4.Gleason", 
          "B1.Gleason", "B2.Gleason", 
          "B3.Gleason", "B4.Gleason", 
          "C1.Gleason",  "C2.Gleason") 
data_oslo$Gleason_standard <- apply(data_oslo[vars], 1, function(x) max(x, na.rm = TRUE))
data_oslo$Gleason_standard <- ifelse(data_oslo$Gleason_standard == -Inf, NA, data_oslo$Gleason_standard)
#data_oslo$Gleason_standard <- ifelse(data_oslo$Gleason_standard
                                     
## Target Oslo
vars <- c("X1a.Gleason", "X1b.Gleason", 
          "X1c.Gleason", "X2a.Gleason", 
          "X2b.Gleason", "X2c.Gleason",
          "X3a.Gleason", "X3b.Gleason", 
          "X3c.Gleason")
data_oslo$Gleason_target <- apply(data_oslo[vars], 1, function(x) max(x, na.rm = TRUE))
data_oslo$Gleason_target <- ifelse(data_oslo$Gleason_target == -Inf, NA, data_oslo$Gleason_target)

## Change Over_all_Gleason and Over_all_gleason by removing "=x" e.g. "3+4=7" to "3+4" in data_sto_ton.
data_sto_ton$Over_all_Gleason <- gsub("=.*", "", data_sto_ton$Over_all_Gleason)
data_sto_ton$Over_all_gleason <- gsub("=.*", "", data_sto_ton$Over_all_gleason)

## Systematic Sto_ton
vars <- c("A1_Gleason", "A2_Gleason", 
          "A3_Gleason", "A4_Gleason", 
          "B1_Gleason", "B2_Gleason", 
          "B3_Gleason", "B4_Gleason", 
          "C1_Gleason", "C2_Gleason", 
          "C3_Gleason", "C4_Gleason",
          "Over_all_gleason")
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "999999", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "99999", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "9999", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "3+3", "6", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "3+4", "7", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "4+3", "7.5", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "3+5", "8", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "4+4", "8", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "5+3", "8.5", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "4+5", "9", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "5+4", "9.5", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "5+5", "10", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], as.numeric)
data_sto_ton$Gleason_standard <- apply(data_sto_ton[vars], 1, function(x) max(x, na.rm = TRUE))
data_sto_ton$Gleason_standard <- ifelse(data_sto_ton$Gleason_standard == -Inf, NA, data_sto_ton$Gleason_standard)
data_sto_ton$Gleason_standard <- ifelse(!is.na(data_sto_ton$Over_all_gleason), data_sto_ton$Over_all_gleason, data_sto_ton$Gleason_standard)
                                          
## Target Sto_ton
vars <- c("A1d_Gleason",
          "A2d_Gleason", "A3d_Gleason", 
          "A4d_Gleason", "B1d_Gleason", 
          "B2d_Gleason", "B3d_Gleason", 
          "B4d_Gleason", "C1d_Gleason", 
          "C2d_Gleason", "C3d_Gleason", 
          "C4d_Gleason", "A1v_Gleason", 
          "A2v_Gleason", "A3v_Gleason", 
          "A4v_Gleason", "B1v_Gleason", 
          "B2v_Gleason", "B3v_Gleason", 
          "B4v_Gleason", "C1v_Gleason",  
          "C2v_Gleason", "C3v_Gleason", 
          "C4v_Gleason", "Over_all_Gleason")
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "999999", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "99999", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "9999", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "", NA, x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "3+3", "6", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "3+4", "7", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "4+3", "7.5", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "3+5", "8", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "4+4", "8", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "5+3", "8.5", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "4+5", "9", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "5+4", "9.5", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], function(x) ifelse(x == "5+5", "10", x))
data_sto_ton[vars] <- lapply(data_sto_ton[vars], as.numeric)
data_sto_ton$Gleason_target <- apply(data_sto_ton[vars], 1, function(x) max(x, na.rm = TRUE))
data_sto_ton$Gleason_target <- ifelse(data_sto_ton$Gleason_target == -Inf, NA, data_sto_ton$Gleason_target)
data_sto_ton$Gleason_target <- ifelse(!is.na(data_sto_ton$Over_all_Gleason), data_sto_ton$Over_all_Gleason, data_sto_ton$Gleason_target)

##############################################################################
## Create av logical variable if Standard BX was NOT taken, should correspond to
## Oslos "Standart BX not taken /registered" but based on biopsy length >0.
##############################################################################
data_sto_ton$standard_bx_nottaken <- !(data_sto_ton$Length_biopsy_standard > 0)

##############################################################################
## Variables to keep track of which raw data set is used for each subject. 
##############################################################################
data_oslo$in_standard <- as.numeric(data_oslo$Standart.BX.not.taken..registered == FALSE)
data_oslo$in_target <- as.numeric(data_oslo$No.Target.Biopsy == FALSE)
data_oslo$in_MR <- as.numeric(!(is.na(data_oslo$MR.date)))
data_oslo$Site <- "Oslo"

##############################################################################
## Link column names in data_sto_ton to those in data_oslo.
##############################################################################
var <- c("Studie_id", "STHLM.3.study.ID",
         "Pnr", "Pnr",
         "age", "age",
         "MR_date", "MR.date",
         "Mottagning_standard", "Mottagning_standard",
         "Mottagning_target", "Mottagning_target",
         "PSA", "PSA",
         "standard_bx_nottaken", "Standart.BX.not.taken..registered",
         "Diffuse_changes", "Diffuse.changes",
         "Tumor1_zone", "Tumor1.zone", 
         "Tumor1_EPE", "Tumor1.EPE",
         "Tumor1_SVI", "Tumor1.SVI", 
         "Tumor1_T2w", "Tumor1.T2w", 
         "Tumor1_DWI", "Tumor1.DWI",
         "Tumor1_PIRAD", "Tumor1.PIRAD",
         "Tumor2_zone", "Tumor2.Zone", 
         "Tumor2_EPE", "Tumor2.EPE", 
         "Tumor2_SVI", "Tumor2.SVI", 
         "Tumor2_T2w", "Tumor2.T2w", 
         "Tumor2_DWI", "Tumor2.DWI",
         "Tumor2_PIRAD", "Tumor2.PIRAD", 
         "Tumor3_zone", "Tumor3.Zone",
         "Tumor3_EPE", "Tumor3.EPE",
         "Tumor3_SVI", "Tumor3.SVI",
         "Tumor3_T2w", "Tumor3.T2w", 
         "Tumor3_DWI", "Tumor3.DWI", 
         "Tumor3_PIRAD", "Tumor3.PIRAD",
         "A1_Tumor_length", "A1.Tumor.length",
         "A2_Tumor_length", "A2.Tumor.length",
         "A3_Tumor_length", "A3.Tumor.length",
         "A4_Tumor_length", "A4.Tumor.length",
         "B1_Tumor_length", "B1.Tumor.length",
         "B2_Tumor_length", "B2.Tumor.length",
         "B3_Tumor_length", "B3.Tumor.length",
         "B4_Tumor_length", "B4.Tumor.length",
         "C1_Tumor_length", "C1.Tumor.length", 
         "C2_Tumor_length", "C2.Tumor.length",
         "Biopsy_Date_standard", "Biopsy.Date", 
         "Tidigare_biopsi", "Tidligere.Biopsi", 
         "A1_Biopsy_length", "A1.Biopsy.length", 
         "A2_Biopsy_length", "A2.Biopsy.length", 
         "A3_Biopsy_length", "A3.Biopsy.length", 
         "A4_Biopsy_length", "A4.Biopsy.length", 
         "B1_Biopsy_length", "B1.Biopsy.length", 
         "B2_Biopsy_length", "B2.Biopsy.length", 
         "B3_Biopsy_length", "B3.Biopsy.length", 
         "B4_Biopsy_length", "B4.Biopsy.length", 
         "C1_Biopsy_length", "C1.Biopsy.length", 
         "C2_Biopsy_length", "C2.Biopsy.length", 
         "Biopsy_Date_target", "Biopsi.date",
         "Radiologist", "Radiologist", 
         "Tumor_volume_T1", "Tumor_volume_T1",
         "Tumor_volume_T2", "Tumor_volume_T2",
         "Tumor_volume_T3", "Tumor_volume_T3",
         "Prostate_volume", "Prostate.volume", 
         "Diffuse_changes", "Diffuse.changes",
         "Palpasjon.overall", "Palpasjon.overall",
         "Tumor1._dom", "Tumor.1..dom",
         "Tumor1._sub1", "Tumor.1..sub1",
         "Tumor1._sub2", "Tumor.1..sub2",
         "Tumor2._dom", "Tumor.2..dom",
         "Tumor2._sub1", "Tumor.2..sub1",
         "Tumor_2._sub2", "Tumor.2..sub2",
         "Tumor_3._dom", "Tumor.3..dom",
         "Tumor_3._sub1", "Tumor.3..sub1",
         "Tumor_3._sub2", "Tumor.3..sub2", 
         "Length_biopsy_standard", "Length_biopsy_standard",
         "Length_biopsy_target", "Length_biopsy_target",
         "Length_tumor_standard", "Length_tumor_standard",
         "Length_tumor_target", "Length_tumor_target",
         "Gleason_standard", "Gleason_standard",
         "Gleason_target", "Gleason_target", 
         "in_standard", "in_standard",
         "in_target", "in_target",
         "in_MR", "in_MR", 
         "Site", "Site")

##############################################################################
## Create data with MATCHED column names between oslo and sto_ton, and also 
## non-matched names.
##############################################################################
b <- data.frame(matrix(var, ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)

##############################################################################
## Rbind Oslo and Sto_ton
##############################################################################
data_oslo_inner <- data_oslo[b[[2]]]
names(data_oslo_inner) <- b[[1]]
data_sto_ton_inner <- data_sto_ton[b[[1]]]

data <- rbind(data_sto_ton_inner, data_oslo_inner)
dat <- select(data, Studie_id, MR_date)
data <- select(data, -MR_date)
data[] <- lapply(data, function(x) ifelse(x == 99999, NA, x))
data[] <- lapply(data, function(x) ifelse(x == "NA", NA, x))
data[] <- lapply(data, function(x) ifelse(x == "", NA, x))
data[] <- lapply(data, function(x) ifelse(x == "-", NA, x))
data <- inner_join(data, dat, by = "Studie_id")

data$PSA <- gsub(",", ".", data$PSA)
data$PSA <- as.numeric(data$PSA)
data[is.na(data$Tidigare_biopsi), ]$Tidigare_biopsi <- FALSE
data[is.na(data$Tumor1_PIRAD), "Tumor1_PIRAD"] <- 2 ## Set PIRADS NA to 2 
data[is.na(data$Tumor2_PIRAD), "Tumor2_PIRAD"] <- 2 ## Set PIRADS NA to 2 
data[is.na(data$Tumor3_PIRAD), "Tumor3_PIRAD"] <- 2 ## Set PIRADS NA to 2 

##############################################################################
## Change Gleason_standard. Separate NA due to missing from NA as in negative 
## biopsy. 
## Change Gleason_target. If Diffuse_changes then there are no targeted biopsies 
## but instead systematic. 
##############################################################################
data$Gleason_standard <- ifelse((is.na(data$Gleason_standard) & !(data$standard_bx_nottaken)), 0, data$Gleason_standard)
data$Gleason_target <- ifelse(is.na(data$Diffuse_changes) | data$Diffuse_changes == FALSE, data$Gleason_target, data$Gleason_standard)
data$Gleason_target <- ifelse(is.na(data$Gleason_target) & data$Length_biopsy_target > 0, 0, data$Gleason_target)
data$done_standard <- ifelse(is.na(data$Gleason_standard), 0, 1)
data$done_target <- ifelse(is.na(data$Gleason_target), 0, 1)
data$Gleason_max <- pmax(data$Gleason_target, data$Gleason_standard, na.rm = T)

##############################################################################
## Add predictions from S3M
##############################################################################
s3m_pred <- s3m_pred[, c("studieid", "S3M")]

## Make a corresponding studieid in data. i.e Pnr from Swe and Studie_id from Nor.
data$studieid <- data$Studie_id
data[data$Site == "Stockholm", ]$studieid <- paste0(substr(data[data$Site == "Stockholm", ]$Pnr, 3, 8), substr(data[data$Site == "Stockholm", ]$Pnr, 10, 13)) 

## Correct error
data[data$Studie_id == "6808100143", ]$Prostate_volume <- 46

## Attach values for S3M.
data <- left_join(data, s3m_pred, by = "studieid")

##############################################################################
## Save ALL data as xlsx and RData 
##############################################################################
if (write_output){
  write.xlsx(data, "P:/STHLM3MR/STHLM3MR_Admin/Data/Analysdata/Analysdata_all.xlsx", row.names = FALSE, showNA = FALSE)
  save(data, file = "P:/STHLM3MR/STHLM3MR_Admin/Data/Analysdata/Analysdata_all.RData")
}

## Final definition of Cohort is "men with S3M, MR and standard Bx".
data <- filter(data, !is.na(S3M) & !is.na(Gleason_standard) & in_MR == 1)

##############################################################################
## Save COMPLETE data as xlsx and RData 
##############################################################################
if (write_output){
write.xlsx(data, "P:/STHLM3MR/STHLM3MR_Admin/Data/Analysdata/Analysdata.xlsx", row.names = FALSE, showNA = FALSE)
save(data, file = "P:/STHLM3MR/STHLM3MR_Admin/Data/Analysdata/Analysdata.RData")
}