training_atalakito <- function(df_eredeti, df_spontan) {
  
  df_eredeti[(nrow(df_eredeti)+1),] <- as.vector(c("spontan", t(df_spontan)))
  colnames(df_eredeti) <- c("Frame", paste0("ROI", 1:(ncol(df_eredeti)-1)))
  df_eredeti <- as.data.frame(t(df_eredeti))
  colnames(df_eredeti) <- df_eredeti[1,]
  df_eredeti <- df_eredeti[-1, ] 
  
  sornev <- rownames(df_eredeti)
  oszlopnev <- colnames(df_eredeti)
  
  df_eredeti <- lapply(df_eredeti, as.numeric)
  df_eredeti$spontan <- as.factor(df_eredeti$spontan)
  df_eredeti <- as.data.frame(df_eredeti)
  
  colnames(df_eredeti) <- oszlopnev
  rownames(df_eredeti) <- sornev
  
  colnames(df_eredeti) <- paste("t_", oszlopnev, sep = "")
  rownames(df_eredeti) <- sornev
  colnames(df_eredeti)[ncol(df_eredeti)] <- "spontan"
  assign('training',df_eredeti,envir=.GlobalEnv)
  
}
