diff_max <- function(df_teljes) {
  columns = colnames(df_teljes) 
  diff_df = data.frame(matrix(nrow = 1, ncol = length(columns))) 
  colnames(diff_df) = columns
  
  for(i in 1:ncol(diff_df)) {       
    diff_df[1,i] <- which.max(diff(df_teljes[,i]))
  }
  
  diff_df <- t(diff_df[,-1])
  hist(diff_df)
  diff_df <- as.data.frame(diff_df)
  colnames(diff_df) <- "date"
  diff_df$cell <- rownames(diff_df)
  
  beadas <- which.max(table(diff_df$date))
  diff_df$same <- NA
  diff_df$difference <- NA
  
  for (i in 1:nrow(diff_df)) {
    if (diff_df[i,1] == beadas) {
      diff_df[i,3] <- TRUE
      diff_df[i,4] <- diff_df[i,1] - beadas
    } else {
      diff_df[i,3] <- FALSE
      diff_df[i,4] <- diff_df[i,1] - beadas
    }
  }
  
  return(beadas)
  return(diff_df$same)
}
