##############################################################################
## Old tables. No longer part of main study, not even in appendix. According  
## to Tobias 2017-10-05.
##############################################################################


##############################################################################
## Table S1, only on full study. (google docs).
## Idea: Use only subjects who had a systematic biopsy. Calculate sensetivity
## etc on subjects positiv according to column names in the table (for GS7+).
##############################################################################
t2 <- function(data, dig){
  dig_str <- paste0("%.", dig, "f")
  x <- data
  x$Gleason_target <- ifelse(is.na(x$Gleason_target), 0, x$Gleason_target) ## Set to 0 to remove NA.
  x$Gleason_max <- pmax(x$Gleason_target, x$Gleason_standard, na.rm = T)
  ## Positive (GS7+) by criteria 
  x$pos_s3m_tar <- x$S3M >= 0.1 & x$Gleason_target >= 7
  x$pos_s3m_sys <- x$S3M >= 0.1 & x$Gleason_standard >= 7
  x$pos_s3m_tar_sys <- x$S3M >= 0.1 & x$Gleason_max >= 7
  x$pos_tar <- x$Gleason_target >= 7
  x$pos_sys <- x$Gleason_standard >= 7
  x$pos_tar_sys <- x$Gleason_max >= 7 
  ## Positive (GS3.4) by criteria 
  x$pos_s3m_tar_34 <- x$S3M >= 0.1 & x$Gleason_target == 7
  x$pos_s3m_sys_34 <- x$S3M >= 0.1 & x$Gleason_standard == 7
  x$pos_s3m_tar_sys_34 <- x$S3M >= 0.1 & x$Gleason_max == 7
  x$pos_tar_34 <- x$Gleason_target == 7
  x$pos_sys_34 <- x$Gleason_standard == 7
  x$pos_tar_sys_34 <- x$Gleason_max == 7
  ## Positive (GS4.3) by criteria 
  x$pos_s3m_tar_43 <- x$S3M >= 0.1 & x$Gleason_target == 7.5
  x$pos_s3m_sys_43 <- x$S3M >= 0.1 & x$Gleason_standard == 7.5
  x$pos_s3m_tar_sys_43 <- x$S3M >= 0.1 & x$Gleason_max == 7.5
  x$pos_tar_43 <- x$Gleason_target == 7.5
  x$pos_sys_43 <- x$Gleason_standard == 7.5
  x$pos_tar_sys_43 <- x$Gleason_max == 7.5
  ## Positive (GS4.4p) by criteria 
  x$pos_s3m_tar_44 <- x$S3M >= 0.1 & x$Gleason_target >= 8
  x$pos_s3m_sys_44 <- x$S3M >= 0.1 & x$Gleason_standard >= 8
  x$pos_s3m_tar_sys_44 <- x$S3M >= 0.1 & x$Gleason_max  >= 8
  x$pos_tar_44 <- x$Gleason_target >= 8
  x$pos_sys_44 <- x$Gleason_standard >= 8
  x$pos_tar_sys_44 <- x$Gleason_max >=8
  ## Positive (GS6) by criteria 
  x$pos_s3m_tar_6 <- x$S3M >= 0.1 & x$Gleason_target == 6
  x$pos_s3m_sys_6 <- x$S3M >= 0.1 & x$Gleason_standard == 6
  x$pos_s3m_tar_sys_6 <- x$S3M >= 0.1 & x$Gleason_max == 6
  x$pos_tar_6 <- x$Gleason_target == 6
  x$pos_sys_6 <- x$Gleason_standard == 6
  x$pos_tar_sys_6 <- x$Gleason_max == 6
  ## Biopsies by criteria
  x$n_s3m_tar <- x$S3M >= 0.1 & x$done_target == 1
  x$n_s3m_sys <- x$S3M >= 0.1 
  x$n_s3m_tar_sys <- x$S3M >= 0.1 
  x$n_tar <- x$done_target == 1
  x$n_sys <- TRUE
  x$n_tar_sys <- TRUE
  ## List variable names
  var_list_pos <- c("pos_s3m_tar", "pos_s3m_sys", "pos_s3m_tar_sys", "pos_tar", "pos_sys", "pos_tar_sys")
  var_list_pos_6 <- c("pos_s3m_tar_6", "pos_s3m_sys_6", "pos_s3m_tar_sys_6", "pos_tar_6", "pos_sys_6", "pos_tar_sys_6")
  var_list_pos_34 <- c("pos_s3m_tar_34", "pos_s3m_sys_34", "pos_s3m_tar_sys_34", "pos_tar_34", "pos_sys_34", "pos_tar_sys_34")
  var_list_pos_43 <- c("pos_s3m_tar_43", "pos_s3m_sys_43", "pos_s3m_tar_sys_43", "pos_tar_43", "pos_sys_43", "pos_tar_sys_43")
  var_list_pos_44 <- c("pos_s3m_tar_44", "pos_s3m_sys_44", "pos_s3m_tar_sys_44", "pos_tar_44", "pos_sys_44", "pos_tar_sys_44")
  var_list_n <- c("n_s3m_tar", "n_s3m_sys", "n_s3m_tar_sys", "n_tar", "n_sys", "n_tar_sys")
  ## Rows in table
  sens <- lapply(var_list_pos, function(a) sprintf(dig_str, sum(x[[a]] & x[["pos_tar_sys"]]) / sum(x[["pos_tar_sys"]])))
  #spec <- lapply(var_list, function(a) sprintf(dig_str, sum(!(x[[a]]) & !(x[["pos_tar_sys"]])) / sum(!(x[["pos_tar_sys"]])))) 
  n <-  sapply(var_list_n, function(a) as.character(sum(x[[a]])))
  n_34 <-  sapply(var_list_pos_34, function(a) as.character(sum(x[[a]])))
  n_43<-  sapply(var_list_pos_43, function(a) as.character(sum(x[[a]])))
  n_44 <-  sapply(var_list_pos_44, function(a) as.character(sum(x[[a]])))
  n_6 <-  sapply(var_list_pos_6, function(a) as.character(sum(x[[a]])))
  
  df <- data.frame(sens, stringsAsFactors = FALSE)
  names(df) <- var_list_pos
  df <- rbind(df, n, n_6, n_34, n_43, n_44)
  measure <- c("sensitivity", "n", "n_6", "n_3.4", "n_4.3", "n_4.4p")
  df <- cbind(measure, df)
  
  return(df)
}

tabS1 <- t2(data, 2)
tabS1_s <- t2(filter(data, Site == "Stockholm"), 2)
tabS1_t <- t2(filter(data, Site == "Tonsberg"), 2)
tabS1_o <- t2(filter(data, Site == "Oslo"), 2)

##############################################################################
## Table S2.
##############################################################################
tS2 <- function(data, dig){
  ## Function to calculate the columns in table 3.
  ## data: data.
  ## dig: significant digits.
  dig_str <- paste0("%.", dig, "f")
  str_perc <- function(a, b) paste0(a, " (",  sprintf(dig_str, a/b*100), "%)")
  x <- data
  pirad <- sapply(2:5, function(a) sum(x$Tumor1_PIRAD == a))
  pirad <- sapply(pirad, function(z) str_perc(z, sum(pirad)))
  x$lesion <- (x$Tumor1_PIRAD > 2) + (x$Tumor2_PIRAD > 2) + (x$Tumor3_PIRAD > 2) 
  lesion_mri <- paste0(sprintf(dig_str, mean(x$lesion, na.rm = TRUE)), " (", sprintf(dig_str, sd(x$lesion, na.rm = TRUE)), ")")
  lesion_vol <- paste0(sprintf(dig_str, mean(x$Tumor_volume_T1/1000, na.rm = TRUE)), " (", sprintf(dig_str, sd(x$Tumor_volume_T1/1000, na.rm = TRUE)), ")")
  return(c(pirad, lesion_mri, lesion_vol))
}

tS2_df <- function(data, dig){
  all <- tS2(data = data, dig = dig)
  s3m_p <- tS2(data = filter(data, S3M >= 0.1), dig = dig)
  s3m_n <- tS2(data = filter(data, S3M < 0.1), dig = dig)
  df <- data.frame(all, s3m_p, s3m_n)
  data <- filter(data, Tidigare_biopsi == FALSE)
  all <- tS2(data = data, dig = dig)
  s3m_p <- tS2(data = filter(data, S3M >= 0.1), dig = dig)
  s3m_n <- tS2(data = filter(data, S3M < 0.1), dig = dig)
  df_neg <- data.frame(all, s3m_p, s3m_n)
  return(cbind(df, df_neg))
}

tab_S2 <- tS2_df(data, 1)
tab_S2_s <- tS2_df(filter(data, Site == "Stockholm"), 1)
tab_S2_t <- tS2_df(filter(data, Site == "Tonsberg"), 1)
tab_S2_o <- tS2_df(filter(data, Site == "Oslo"), 1)

##############################################################################
## Table S3.
## XTAB PIRAD and GS by SITE
##############################################################################

t5a <- function(data) {
  x <- data
  x$Tumor1_PIRAD <- ifelse(x$Tumor1_PIRAD == 1, 2, x$Tumor1_PIRAD)
  x$Gleason_target_tbl <- ifelse(x$Gleason_target < 6, 0, x$Gleason_target)
  x$Gleason_target_tbl <- ifelse(x$Gleason_target_tbl >= 8, 8, x$Gleason_target_tbl)
  x$Gleason_standard_tbl <- ifelse(x$Gleason_standard < 6, 0, x$Gleason_standard)
  x$Gleason_standard_tbl <- ifelse(x$Gleason_standard_tbl >= 8, 8, x$Gleason_standard_tbl)
  x <- filter(x, !is.na(Gleason_standard_tbl))
  ## For target all NA is 0 by definition of the study protocol. (ref. Tobias).
  x$Gleason_target_tbl <- ifelse(is.na(x$Gleason_target_tbl), 0, x$Gleason_target_tbl)
  x$Gleason_max <- ifelse(x$Gleason_target_tbl > x$Gleason_standard_tbl, x$Gleason_target_tbl, x$Gleason_standard_tbl)
  x <- addmargins(xtabs(~Tumor1_PIRAD + Gleason_max, x))
  x <- as.data.frame.matrix(x)
  return(x)
}

t5b <- function(data) {
  x <- data
  x$Tumor1_PIRAD <- ifelse(x$Tumor1_PIRAD == 1, 2, x$Tumor1_PIRAD)
  x$Gleason_target_tbl <- ifelse(x$Gleason_target < 6, 0, x$Gleason_target)
  x$Gleason_target_tbl <- ifelse(x$Gleason_target_tbl >= 8, 8, x$Gleason_target_tbl)
  x <- filter(x, !is.na(Gleason_standard))
  ## For target all NA is 0 by definition of the study protocol. (ref. Tobias).
  x$Gleason_target_tbl <- ifelse(is.na(x$Gleason_target_tbl), 0, x$Gleason_target_tbl)
  x <- addmargins(xtabs(~Tumor1_PIRAD + Gleason_target_tbl, x))
  x <- as.data.frame.matrix(x)
  return(x)
}

tab5a <- t5a(data)
tab5a_s <- t5a(filter(data, Site == "Stockholm"))
tab5a_t <- t5a(filter(data, Site == "Tonsberg"))
tab5a_o <- t5a(filter(data, Site == "Oslo"))

tab5b <- t5b(data)
tab5b_s <- t5b(filter(data, Site == "Stockholm"))
tab5b_t <- t5b(filter(data, Site == "Tonsberg"))
tab5b_o <- t5b(filter(data, Site == "Oslo"))


##############################################################################
## Output the table (then copypaste the whole block into the formated version. 
## Use only text paste.)
##############################################################################

write.xlsx(tabS1, paste0(folder, "tableS1.xlsx"))
write.xlsx(tabS1_s, paste0(folder, "tableS1_by_site.xlsx"), sheetName = "Stockholm")
write.xlsx(tabS1_t, paste0(folder, "tableS1_by_site.xlsx"), sheetName = "Tonsberg", append = TRUE)
write.xlsx(tabS1_o, paste0(folder, "tableS1_by_site.xlsx"), sheetName = "Oslo", append = TRUE)

write.xlsx(tab_S2, paste0(folder, "table_S2.xlsx"))
write.xlsx(tab_S2_s, paste0(folder, "table_S2_by_site.xlsx"), sheetName = "Stockholm")
write.xlsx(tab_S2_t, paste0(folder, "table_S2_by_site.xlsx"), sheetName = "Tonsberg", append = TRUE)
write.xlsx(tab_S2_o, paste0(folder, "table_S2_by_site.xlsx"), sheetName = "Oslo", append = TRUE)

write.xlsx(tab5a, paste0(folder, "table_S3a.xlsx"))
write.xlsx(tab5a_s, paste0(folder, "table_S3a_by_site.xlsx"), sheetName = "Stockholm")
write.xlsx(tab5a_t, paste0(folder, "table_S3a_by_site.xlsx"), sheetName = "Tonsberg", append = TRUE)
write.xlsx(tab5a_o, paste0(folder, "table_S3a_by_site.xlsx"), sheetName = "Oslo", append = TRUE)

write.xlsx(tab5b, paste0(folder, "table_S3b.xlsx"))
write.xlsx(tab5b_s, paste0(folder, "table_S3b_by_site.xlsx"), sheetName = "Stockholm")
write.xlsx(tab5b_t, paste0(folder, "table_S3b_by_site.xlsx"), sheetName = "Tonsberg", append = TRUE)
write.xlsx(tab5b_o, paste0(folder, "table_S3b_by_site.xlsx"), sheetName = "Oslo", append = TRUE)