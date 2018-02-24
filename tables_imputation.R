##############################################################################
## Redo Table 2 with imputation on men who should have had systematic biopsy
## but did not.
##############################################################################
imputed <- read.table("P:/STHLM3MR/STHLM3MR_Admin/Data/Rawdata/imputation.txt",
                       header = TRUE,
                       stringsAsFactors = FALSE)
load(file = "P:/STHLM3MR/STHLM3MR_Admin/Data/Analysdata/Analysdata_all.RData")
imputed = imputed[!duplicated(imputed$studieid), ]

imputed = inner_join(data, imputed, by="studieid")

miss_s = imputed %>% filter(is.na(Gleason_standard)) 
nomiss = imputed %>% filter(!is.na(Gleason_standard)) 

##############################################################################
## Simulate n=1000 GS on systematic biopsies where missing (n=137).
##############################################################################
set.seed(1)
n=1000
n_ind = dim(miss_s)[1]
miss_s_mat <- matrix(NA, nrow = n_ind, ncol = n)
for (i in 1:n_ind){
  miss_s_mat[i, ] <- with(miss_s[i, ],
                          sample(c(0.0, 6.0, 7.0),
                                 n,
                                 prob = c(prob.benign, prob.gs6, prob.gs7andMore),
                                 replace = TRUE))
}

##############################################################################
## Generate the table.
##############################################################################
fun_boot <- function() {
  ## Bootstrap 95CI
  r1 <- matrix(NA, nrow = n, ncol = 5)
  r2 <- matrix(NA, nrow = n, ncol = 5)
  r3 <- matrix(NA, nrow = n, ncol = 5)
  r4 <- matrix(NA, nrow = n, ncol = 5)
  r5 <- matrix(NA, nrow = n, ncol = 5)
  r6 <- matrix(NA, nrow = n, ncol = 5)
  r7 <- matrix(NA, nrow = n, ncol = 5)
  r8 <- matrix(NA, nrow = n, ncol = 5)
  r9 <- matrix(NA, nrow = n, ncol = 5)
  
  size <- dim(rbind(nomiss, miss_s))[1]
  for (i in 1:n){
    miss_s$Gleason_standard <- miss_s_mat[, i]
    tmp <- rbind(nomiss, miss_s)
    d <- sample(1:size, size = size, replace = TRUE)
    d <- tmp[d, ]
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
  return(list(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5, r6 = r6, r7 = r7, r8 = r8, r9 = r9))
}

t3_to_str <- function(r){
  dig <- 2
  spr_str <- paste0("%.", dig, "f")
  fun1 <- function(x, perc) sprintf(spr_str, quantile(x, perc)) 
  row45 <- apply(r[, 4:5], 2, function(x) paste0(fun1(x = x, perc = 0.5),
                                               " (", 
                                               fun1(x = x, perc = 0.025), 
                                               ", ",
                                               fun1(x = x, perc = 0.975), 
                                               ")"))
  dig <- 0
  spr_str <- paste0("%.", dig, "f")
  fun1 <- function(x, perc) sprintf(spr_str, quantile(x, perc))
  row3 <- paste0(fun1(x = r[, 3], perc = 0.5),
                " (", 
                fun1(x = r[, 3], perc = 0.025), 
                ", ",
                fun1(x = r[, 3], perc = 0.975), 
                ")")
  row12 <- paste0(fun1(x = r[, 1], perc = 0.5), " / ", fun1(x = r[, 2], perc = 0.5))
  c(row12, row3, row45)
}

r <- fun_boot()

tab3 <- rbind(c("", "", "", ""),
              t3_to_str(r = r[[1]]),
              t3_to_str(r = r[[2]]),
              t3_to_str(r = r[[3]]),
              t3_to_str(r = r[[4]]),
              t3_to_str(r = r[[5]]),
              t3_to_str(r = r[[6]]),
              t3_to_str(r = r[[7]]),
              t3_to_str(r = r[[8]]),
              t3_to_str(r = r[[9]]))

tab3 <- as.data.frame(tab3)
names(tab3) <- c("n", "perc", "GS7p", "GS6")

write.xlsx(tab3, paste0(folder, "table2_imputed.xlsx"), row.names = FALSE)
