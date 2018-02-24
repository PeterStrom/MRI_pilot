## Figure 1

gg_data <- data
gg_data$Gleason_target <- ifelse(is.na(gg_data$Gleason_target), 0, gg_data$Gleason_target)
gg_data$Gleason_max <- pmax(gg_data$Gleason_target, gg_data$Gleason_standard, na.rm = T)
# S3M TAR
gg_data$x_s3m_tar <- NA
gg_data$x_s3m_tar[gg_data$done_target == 1 & gg_data$S3M >= .1] <- "Benign"
gg_data$x_s3m_tar[gg_data$Gleason_target == 6 & gg_data$S3M >= .1] <- "GG1"
gg_data$x_s3m_tar[gg_data$Gleason_target == 7 & gg_data$S3M >= .1] <- "GG2"
gg_data$x_s3m_tar[gg_data$Gleason_target == 7.5 & gg_data$S3M >= .1] <- "GG3"
gg_data$x_s3m_tar[gg_data$Gleason_target >= 8 & gg_data$S3M >= .1] <- "GG4"
# S3M SYS
gg_data$x_s3m_sys <- NA
gg_data$x_s3m_sys[gg_data$S3M >= .1] <- "Benign"
gg_data$x_s3m_sys[gg_data$Gleason_standard == 6 & gg_data$S3M >= .1] <- "GG1"
gg_data$x_s3m_sys[gg_data$Gleason_standard == 7 & gg_data$S3M >= .1] <- "GG2"
gg_data$x_s3m_sys[gg_data$Gleason_standard == 7.5 & gg_data$S3M >= .1] <- "GG3"
gg_data$x_s3m_sys[gg_data$Gleason_standard >= 8 & gg_data$S3M >= .1] <- "GG4"
# S3M BOTH
gg_data$x_s3m_both <- NA
gg_data$x_s3m_both[gg_data$S3M >= .1] <- "Benign"
gg_data$x_s3m_both[gg_data$Gleason_max == 6 & gg_data$S3M >= .1] <- "GG1"
gg_data$x_s3m_both[gg_data$Gleason_max == 7 & gg_data$S3M >= .1] <- "GG2"
gg_data$x_s3m_both[gg_data$Gleason_max == 7.5 & gg_data$S3M >= .1] <- "GG3"
gg_data$x_s3m_both[gg_data$Gleason_max >= 8 & gg_data$S3M >= .1] <- "GG4"
# TAR
gg_data$x_tar <- NA
gg_data$x_tar[gg_data$done_target == 1] <- "Benign"
gg_data$x_tar[gg_data$Gleason_target == 6] <- "GG1"
gg_data$x_tar[gg_data$Gleason_target == 7] <- "GG2"
gg_data$x_tar[gg_data$Gleason_target == 7.5] <- "GG3"
gg_data$x_tar[gg_data$Gleason_target >= 8] <- "GG4"
# SYS
gg_data$x_sys <- "Benign"
gg_data$x_sys[gg_data$Gleason_standard == 6] <- "GG1"
gg_data$x_sys[gg_data$Gleason_standard == 7] <- "GG2"
gg_data$x_sys[gg_data$Gleason_standard == 7.5] <- "GG3"
gg_data$x_sys[gg_data$Gleason_standard >= 8] <- "GG4"
# BOTH
gg_data$x_both <- "Benign"
gg_data$x_both[gg_data$Gleason_max == 6] <- "GG1"
gg_data$x_both[gg_data$Gleason_max == 7] <- "GG2"
gg_data$x_both[gg_data$Gleason_max == 7.5] <- "GG3"
gg_data$x_both[gg_data$Gleason_max >= 8] <- "GG4"

gg_data1 <- gather(gg_data, key = Strategies, value = Biopsies, starts_with("x_"), na.rm = TRUE)
gg_data2 <- group_by(gg_data1, Strategies, Biopsies) %>% summarise(n = n())
gg_data2$Biopsies <- factor(gg_data2$Biopsies, 
                            levels = c("Benign", "GG1", "GG2", "GG3", "GG4"),
                            labels = c("Benign", "GG1", "GG2", "GG3", "GG4"))
gg_data2$Strategies <- factor(gg_data2$Strategies, 
                              levels = c("x_s3m_tar", "x_s3m_sys", "x_s3m_both", 
                                         "x_tar", "x_sys", "x_both"),
                              labels = c("S3M\nTarget", "S3M\nSystematic", "S3M\nTarget\nSystematic",
                                         "Target", "Systematic", "Target\nSystematic"))

png(paste0(folder, "Figure1.png"))
ggplot(gg_data2, aes(x = Strategies, y = n, fill = Biopsies, label = n)) +
  geom_bar(stat = "identity") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))
dev.off()

write.xlsx(as.data.frame(gg_data2), paste0(folder, "Figure1.xlsx"), row.names = FALSE)
