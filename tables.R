## Tables (see Google docs).
load("P:/STHLM3MR/STHLM3MR_Admin/Data/Analysdata/Analysdata.RData")
set.seed(1)

##############################################################################
## Table 1.
##############################################################################
t1 <- function(data, dig){
  ## Function to calculate the columns in table 1.
  ## data: the (filtered) data for each column.
  dig_str <- paste0("%.", dig, "f")
  dig_str2 <- "%.2f"
  str_perc <- function(a, b) paste0(a, " (",  sprintf(dig_str, a/b*100), "%)")
  x <- filter(data, !is.na(Gleason_standard))
  
  ## FLOWCHART
  n <- dim(x)[1]
  biop_naive <- paste0(round(sum(x$Tidigare_biopsi == FALSE) / n * 100), "%")
  miss_mr <- sum(x$in_MR == 0)
  miss_tar <- sum(x$done_target == 0) # This was changed 06/01/17 since "Diffuse_changes" == TRUE also should be in_target = 1.
  miss_sta <- sum(x$done_standard == 0)
  miss_bio <- sum(is.na(x$S3M))
  no_miss <- with(x, sum((in_MR + done_standard + done_target) == 3 & !is.na(S3M)))
  s3m_mean_median <- paste0(sprintf(dig_str2, mean(x$S3M, na.rm = T)), " / ", 
                            sprintf(dig_str2, median(x$S3M, na.rm = T)))
  s3m_lt10 <- paste0(sum(x$S3M < 0.1, na.rm = T), " (", 
                     round(mean(x$S3M < 0.1, na.rm = T)*100), "%)")
  x$Gleason_standard_tbl <- ifelse(x$Gleason_standard >= 8, 8, x$Gleason_standard)
  x$Gleason_target_tbl <- ifelse(x$Gleason_target >= 8, 8, x$Gleason_target)
  x$Gleason_max <- pmax(x$Gleason_target_tbl, x$Gleason_standard_tbl, na.rm = T)
  n_gs <- dim(x)[1]
  GS_n <- numeric(length = 4)
  GS_n[1] <- sum(x$Gleason_max == 6, na.rm = T)
  GS_n[2] <- sum(x$Gleason_max == 7, na.rm = T)
  GS_n[3] <- sum(x$Gleason_max == 7.5, na.rm = T)
  GS_n[4] <- sum(x$Gleason_max == 8, na.rm = T)
  
  perc <- sprintf(dig_str, GS_n*100 / n_gs)
  GS <- paste0(GS_n, " (", sprintf(dig_str, GS_n*100 / n_gs), "%)")
  
  ## DEMOGRAPHIC
  age <- paste0(sprintf(dig_str, mean(x$age, na.rm = TRUE)), " (", sprintf(dig_str, sd(x$age, na.rm = TRUE)), ")")
  psa <- paste0(sprintf(dig_str, median(x$PSA, na.rm = T)), 
                " (", sprintf(dig_str, IQR(x$PSA, na.rm = T)), ")")
  miss_psa <- sum(is.na(x$PSA))
  vol <- paste0(sprintf(dig_str, median(x$Prostate_volume, na.rm = T)), 
                " (", sprintf(dig_str, IQR(x$Prostate_volume, na.rm = T)), ")")
  miss_vol <- sum(is.na(x$Prostate_volume))
  x$Tumor1_PIRAD <- ifelse(x$Tumor1_PIRAD == 1, 2, x$Tumor1_PIRAD)
  pirad2 <- sum(x$Tumor1_PIRAD == 2, na.rm = TRUE)
  pirad3 <- sum(x$Tumor1_PIRAD == 3, na.rm = TRUE)
  pirad4 <- sum(x$Tumor1_PIRAD == 4, na.rm = TRUE)
  pirad5 <- sum(x$Tumor1_PIRAD == 5, na.rm = TRUE)
  pirad <- c(pirad2, pirad3, pirad4, pirad5)
  pirad <- sapply(pirad, function(z) str_perc(z, sum(pirad)))
  T1 <- sum(x$Palpasjon.overall == "T1", na.rm = TRUE)
  T2 <- sum(x$Palpasjon.overall == "T2", na.rm = TRUE)
  T3 <- sum(x$Palpasjon.overall == "T3", na.rm = TRUE)
  T4 <- sum(x$Palpasjon.overall == "T4", na.rm = TRUE)
  T1 <- str_perc(T1, sum(!is.na(x$Palpasjon.overall)))
  T2 <- str_perc(T2, sum(!is.na(x$Palpasjon.overall)))
  T3 <- str_perc(T3, sum(!is.na(x$Palpasjon.overall)))
  T4 <- str_perc(T4, sum(!is.na(x$Palpasjon.overall)))
  miss_palp <- sum(is.na(data$Palpasjon.overall))
  x$lesion <- (x$Tumor1_PIRAD > 2) + (x$Tumor2_PIRAD > 2) + (x$Tumor3_PIRAD > 2) 
  lesion_mri <- paste0(sprintf(dig_str, mean(x$lesion, na.rm = TRUE)), " (", sprintf(dig_str, sd(x$lesion, na.rm = TRUE)), ")")
  
  return(c("", n, biop_naive, miss_mr, miss_tar, miss_sta, miss_bio, no_miss,
           s3m_mean_median, s3m_lt10, GS,
           "", age, psa, miss_psa, vol, miss_vol, "", pirad, "", T1,
           T2, T3, T4, miss_palp, lesion_mri))
  
}

## Create Table 1.
tab1 <- as.data.frame(sapply(c("Stockholm", "Oslo", "Tonsberg"), function(x){
  t1(filter(data, Site == x), 1)
}))
tab1$Total <- t1(data, 1)

## Include columns with and without any previous negative biopsy.
data_naive <- filter(data, Tidigare_biopsi == FALSE)
data_notnaive <- filter(data, Tidigare_biopsi == TRUE)

tab <- as.data.frame(sapply(c("Stockholm", "Oslo", "Tonsberg"), function(x){
  t1(filter(data_naive, Site == x), 1)
}))
tab$Total <- t1(data_naive, 1)
tab1 <- cbind(tab1, tab)

tab <- as.data.frame(sapply(c("Stockholm", "Oslo", "Tonsberg"), function(x){
  t1(filter(data_notnaive, Site == x), 1)
}))
tab$Total <- t1(data_notnaive, 1)
tab1 <- cbind(tab1, tab)

##############################################################################
## Tabel 2:
## Comparison TBx/SBx using alternative definitions of significant cancer, 
## reported for entire study and biopsy-naive/previ biopsies
##############################################################################
set.seed(1)

t2 <- function(data, num, n=1000){
  x <- data
  den <- dim(x)[1]
  
  if (num == "t"){
    num7p <- sum(x$Gleason_target >= 7, na.rm = T)
    num6 <- sum(x$Gleason_target == 6, na.rm = T)
    num_ca1 <- sum(x$Gleason_target >= 7.5 | x$Length_tumor_target >= 6, na.rm = T)
    num_ca2 <- sum(x$Gleason_target >= 7 | x$Length_tumor_target >= 4, na.rm = T)
    num_ca3 <- sum(x$Gleason_target == 7.5, na.rm = T)
    num_ca4 <- sum(x$Gleason_target == 8, na.rm = T)
  } else {
    num7p <- sum(x$Gleason_max >= 7, na.rm = T)
    num6 <- sum(x$Gleason_max == 6, na.rm = T)
    num_ca1 <- sum(x$Gleason_max >= 7.5 | x$Length_tumor_target >= 6, na.rm = T)
    num_ca2 <- sum(x$Gleason_max >= 7 | x$Length_tumor_target >= 4, na.rm = T)
    num_ca3 <- sum(x$Gleason_max == 7.5, na.rm = T)
    num_ca4 <- sum(x$Gleason_max == 8, na.rm = T)
  }
  
  den7p <- sum(x$Gleason_standard >= 7)
  den6 <- sum(x$Gleason_standard == 6)
  den_ca1 <- sum(x$Gleason_standard >= 7.5 | x$Length_tumor_standard >= 6)
  den_ca2 <- sum(x$Gleason_standard >= 7 | x$Length_tumor_standard >= 4)
  den_ca3 <- sum(x$Gleason_standard == 7.5)
  den_ca4 <- sum(x$Gleason_standard == 8)
  RR7p <- num7p / den7p
  RR6 <- num6 / den6
  RR_ca1 <- num_ca1 / den_ca1
  RR_ca2 <- num_ca2 / den_ca2
  RR_ca3 <- num_ca3 / den_ca3
  RR_ca4 <- num_ca4 / den_ca4
  
  M1 <- matrix(c(num7p, den7p, RR7p,
                 num6, den6, RR6,
                 num_ca1, den_ca1, RR_ca1,
                 num_ca2, den_ca2, RR_ca2,
                 num_ca3, den_ca3, RR_ca3,
                 num_ca4, den_ca4, RR_ca4), nrow = 6, ncol = 3, byrow = TRUE)
  M2 <- M1[, 1:2] / den 
  return(list(n = M1, perc = M2))
}

dig <- 2
spr_str <- paste0("%.", dig, "f")

## Bootstrap p
p <- matrix(NA, nrow = 6, ncol = n)
data_compl <- data
size <- dim(data_compl)[1]
for (i in 1:n){
  d <- sample(1:size, size = size, replace = TRUE)
  d <- data_compl[d, ]
  p[, i] <- t2(d, "t")$n[, 3] 
}

pval <- apply(p, 1, function(x) quantile(x, c(.025,.975)))

t2_M <- t2(data = data, "t")
tab2 <- paste0(t2_M$n[, 1:2], " (", sprintf(spr_str, t2_M$perc), ")")
tab2 <- data.frame(Tx = tab2[1:6], Sx = tab2[7:12])
tab2$RR_Tx_vs_Sx <- paste0(sprintf(spr_str, t2_M$n[, 3]),
                           ", (",
                           sprintf(spr_str, t(pval)[, 1]),
                           ", ",
                           sprintf(spr_str, t(pval)[, 2]),
                           ")")
tab2_first <- tab2

## Bootstrap p
p <- matrix(NA, nrow = 6, ncol = n)
data_compl <- data
size <- dim(data_compl)[1]
for (i in 1:n){
  d <- sample(1:size, size = size, replace = TRUE)
  d <- data_compl[d, ]
  p[, i] <- t2(d, "ts")$n[, 3] 
}

pval <- apply(p, 1, function(x) quantile(x, c(.025,.975)))

t2_M <- t2(data = data, "ts")
tab2 <- paste0(t2_M$n[, 1:2], " (", sprintf(spr_str, t2_M$perc), ")")
tab2 <- data.frame(Tx = tab2[1:6], Sx = tab2[7:12])
tab2$RR_Tx_vs_Sx <- paste0(sprintf(spr_str, t2_M$n[, 3]),
                           ", (",
                           sprintf(spr_str, t(pval)[, 1]),
                           ", ",
                           sprintf(spr_str, t(pval)[, 2]),
                           ")")

tab2 <- cbind(tab2, tab2_first)
tab2$definition <- c("GS7", "GS6", "GS7.5 or 6mm", "GS7 or 4mm", "GS7.5", "GS8")
tab2 <- tab2[c(2, 1, 5, 6, 4, 3), c(7, 2, 1, 3, 4, 6)]
names(tab2) <- c("definition", "Sx", "TxSx", "RS_TxSx_vs_Sx", "Tx", "RS_Tx_vs_Sx")

##############################################################################
## Tabel 3: Comparisons of different strategies to detect prostate 
## cancer in X men coming for prostate cancer workup. 
## New table summer 2017 (google docs)
##############################################################################

t3 <- function(data, numerator, denominator){
  x <- data
  if (denominator == "current"){ ##Denominator is "current".
    if (numerator == "target") {
      num <- sum(x$done_target == 1)
      num7p <- sum(x$Gleason_target >= 7, na.rm = T)
      num6 <- sum(x$Gleason_target == 6, na.rm = T)
    }
    if (numerator == "biomarker") {
      num <- sum(x$S3M > 0.1)
      num7p <- sum(x$S3M > 0.1 & x$Gleason_standard >= 7)
      num6 <- sum(x$S3M > 0.1 & x$Gleason_standard == 6)
    }
    if (numerator == "combined") {
      num <- sum(x$S3M > 0.1 & x$done_target == 1)
      num7p <- sum(x$S3M > 0.1 & x$Gleason_target >= 7, na.rm = T)
      num6 <- sum(x$S3M > 0.1 & x$Gleason_target == 6, na.rm = T)
    }
    if (numerator == "tsb") { #tsb=target, systematic and biomarker
      num <- sum(x$S3M > 0.1)
      num7p <- sum(x$S3M > 0.1 & x$Gleason_max >= 7, na.rm = T)
      num6 <- sum(x$S3M > 0.1 & x$Gleason_max == 6, na.rm = T)
    } 
    den <- dim(x)[1]
    den7p <- sum(x$Gleason_standard >= 7)
    den6 <- sum(x$Gleason_standard == 6)
    perc <- num / den * 100
    RR7p <- ifelse(den7p == 0, num7p / (den7p + 1) , num7p / den7p)
    RR6 <- ifelse(den6 == 0, num6 / (den6 + 1) , num6 / den6)
  }
  if (denominator == "ts"){ ##Denominator is "T and S" new revision EU.
    if (numerator == "target") {
      num <- sum(x$done_target == 1)
      num7p <- sum(x$Gleason_target >= 7, na.rm = T)
      num6 <- sum(x$Gleason_target == 6, na.rm = T)
    }
    if (numerator == "current") {
      num <- dim(x)[1]
      num7p <- sum(x$Gleason_standard >= 7)
      num6 <- sum(x$Gleason_standard == 6)
    }
    if (numerator == "tb") { #tsb=target, systematic and biomarker
      num <- sum(x$S3M > 0.1 & x$done_target == 1)
      num7p <- sum(x$S3M > 0.1 & x$Gleason_target >= 7, na.rm = T)
      num6 <- sum(x$S3M > 0.1 & x$Gleason_target == 6, na.rm = T)
    }
    if (numerator == "tsb") { #tsb=target, systematic and biomarker
      num <- sum(x$S3M > 0.1)
      num7p <- sum(x$S3M > 0.1 & x$Gleason_max >= 7, na.rm = T)
      num6 <- sum(x$S3M > 0.1 & x$Gleason_max == 6, na.rm = T)
    } 
    den <- dim(x)[1]
    den7p <- sum(x$Gleason_max >= 7, na.rm = T)
    den6 <- sum(x$Gleason_max == 6, na.rm = T)
    perc <- num / den * 100
    RR7p <- ifelse(den7p == 0, num7p / (den7p + 1) , num7p / den7p)
    RR6 <- ifelse(den6 == 0, num6 / (den6 + 1) , num6 / den6)
  }
  if (denominator == "target") {
    den <- sum(x$done_target == 1)
    den7p <- sum(x$Gleason_target >= 7, na.rm = T)
    den6 <- sum(x$Gleason_target == 6, na.rm = T)
    num <- sum(x$S3M > 0.1 & x$done_target == 1)
    num7p  <- sum(x$S3M > 0.1 & x$Gleason_target >= 7, na.rm = T)
    num6 <- sum(x$S3M > 0.1 & x$Gleason_target == 6, na.rm = T)
    perc <- num / den * 100
    RR7p <- ifelse(den7p == 0, num7p / (den7p + 1) , num7p / den7p)
    RR6 <- ifelse(den6 == 0, num6 / (den6 + 1) , num6 / den6)
  }
  if (denominator == "biomarker") {
    den <- sum(x$S3M > 0.1)
    den7p <- sum(x$S3M > 0.1 & x$Gleason_standard >= 7)
    den6 <- sum(x$S3M > 0.1 & x$Gleason_standard == 6)
    num <- sum(x$S3M > 0.1 & x$done_target == 1)
    num7p  <- sum(x$S3M > 0.1 & x$Gleason_target >= 7, na.rm = T)
    num6 <- sum(x$S3M > 0.1 & x$Gleason_target == 6, na.rm = T)
    perc <- num / den * 100
    RR7p <- ifelse(den7p == 0, num7p / (den7p + 1) , num7p / den7p)
    RR6 <- ifelse(den6 == 0, num6 / (den6 + 1) , num6 / den6)
  }
  return(c(num, den, perc, RR7p, RR6))
}

fun_boot <- function() {
  ## Bootstrap 95CI
  r0 <- matrix(NA, nrow = n, ncol = 5)
  r1 <- matrix(NA, nrow = n, ncol = 5)
  r2 <- matrix(NA, nrow = n, ncol = 5)
  r3 <- matrix(NA, nrow = n, ncol = 5)
  r4 <- matrix(NA, nrow = n, ncol = 5)
  r5 <- matrix(NA, nrow = n, ncol = 5)
  r6 <- matrix(NA, nrow = n, ncol = 5)
  r7 <- matrix(NA, nrow = n, ncol = 5)
  r8 <- matrix(NA, nrow = n, ncol = 5)
  r9 <- matrix(NA, nrow = n, ncol = 5)
  
  size <- dim(data)[1]
  for (i in 1:n){
    d <- sample(1:size, size = size, replace = TRUE)
    d <- data[d, ]
    r0[i, ] <- t3(data = d, numerator = "tb", denominator = "ts")
    r1[i, ] <- t3(data = d, numerator = "target", denominator = "current")
    r2[i, ] <- t3(data = d, numerator = "biomarker", denominator = "current")
    r3[i, ] <- t3(data = d, numerator = "combined", denominator = "current")
    r4[i, ] <- t3(data = d, numerator = "tsb", denominator = "current")
    r5[i, ] <- t3(data = d, numerator = "target", denominator = "ts")
    r6[i, ] <- t3(data = d, numerator = "current", denominator = "ts")
    r7[i, ] <- t3(data = d, numerator = "tsb", denominator = "ts")
    r8[i, ] <- t3(data = d, numerator = "target", denominator = "target")
    r9[i, ] <- t3(data = d, numerator = "biomarker", denominator = "biomarker")
  }
  return(list(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5, r6 = r6, r7 = r7, r8 = r8, r9 = r9, r0 = r0))
}

t3_to_str <- function(r, point){
  dig <- 2
  spr_str <- paste0("%.", dig, "f")
  fun1 <- function(x, perc) sprintf(spr_str, quantile(x, perc)) 
  row <- apply(r[, 4:5], 2, function(x) paste0("(", 
                                               fun1(x = x, perc = 0.025), 
                                               ", ",
                                               fun1(x = x, perc = 0.975), 
                                               ")"))
  row45 <- paste(sprintf(spr_str, point[4:5]), row, sep = " ")
  dig <- 0
  spr_str <- paste0("%.", dig, "f")
  fun1 <- function(x, perc) sprintf(spr_str, quantile(x, perc)) 
  row <- paste0("(", 
                fun1(x = r[, 3], perc = 0.025), 
                ", ",
                fun1(x = r[, 3], perc = 0.975), 
                ")")
  row3 <- paste(sprintf(spr_str, point[3]), row, sep = " ")
  row12 <- paste0(point[1], " / ", point[2])
  c(row12, row3, row45)
}

## Generate table
gen_t3 <- function(data){
  tb_ts <- t3(data = data, numerator = "tb", denominator = "ts")
  tar_cur <- t3(data = data, numerator = "target", denominator = "current")
  bio_cur <- t3(data = data, numerator = "biomarker", denominator = "current")
  com_cur <- t3(data = data, numerator = "combined", denominator = "current")
  tsb_cur <- t3(data = data, numerator = "tsb", denominator = "current")
  tar_ts <- t3(data = data, numerator = "target", denominator = "ts")
  cur_ts <- t3(data = data, numerator = "current", denominator = "ts")
  tsb_ts <- t3(data = data, numerator = "tsb", denominator = "ts")
  com_tar <- t3(data = data, numerator = "target", denominator = "target")
  com_bio <- t3(data = data, numerator = "biomarker", denominator = "biomarker")
  
  r <- fun_boot()
  
  tab3 <- rbind(c("", "", "", ""),
                t3_to_str(r = r[[1]], point = tar_cur),
                t3_to_str(r = r[[2]], point = bio_cur),
                t3_to_str(r = r[[3]], point = com_cur),
                t3_to_str(r = r[[4]], point = tsb_cur),
                t3_to_str(r = r[[5]], point = tar_ts),
                t3_to_str(r = r[[6]], point = cur_ts),
                t3_to_str(r = r[[7]], point = tsb_ts),
                t3_to_str(r = r[[8]], point = com_tar),
                t3_to_str(r = r[[9]], point = com_bio),
                t3_to_str(r = r[[10]], point = tb_ts))
  tab3 <- as.data.frame(tab3)
  names(tab3) <- c("n", "perc", "GS7p", "GS6")
  return(tab3)
}

t3_all <- gen_t3(data = data)
t3_sto <- gen_t3(data = filter(data, Site == "Stockholm"))
t3_osl <- gen_t3(data = filter(data, Site == "Oslo"))
t3_ton <- gen_t3(data = filter(data, Site == "Tonsberg"))

##############################################################################
## Output the table (then copypaste the whole block into the formated version. 
## Use only text paste.)
##############################################################################
write.xlsx(tab1, paste0(folder, "table1.xlsx"))

write.xlsx(tab2, paste0(folder, "table3.xlsx"), row.names = FALSE)

write.xlsx(t3_all, paste0(folder, "table2.xlsx"), row.names = FALSE)
write.xlsx(t3_sto, paste0(folder, "table2_by_site.xlsx"), sheetName = "Stockholm")
write.xlsx(t3_ton, paste0(folder, "table2_by_site.xlsx"), sheetName = "Tonsberg", append = TRUE)
write.xlsx(t3_osl, paste0(folder, "table2_by_site.xlsx"), sheetName = "Oslo", append = TRUE)
