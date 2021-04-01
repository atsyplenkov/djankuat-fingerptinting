# Test multicollinearity
is_multicol <- function(data){    
  d <- abs(cor(data, method = "spearman"))     
  d[lower.tri(d)] <- 0    
  diag(d) <- 0    
  index <- which((1-d) < 1e-10, arr.ind = T)    
  if (length(index) == 0){      
    cat('There is no collinearity in the data')
    return(data)
  } else {
    collinears <- (rownames(d)[index])
    data <- data[, !(names(data) %in% collinears)]
    cat("Attention!",
        rownames(d)[index],
        "variables are collinear and were removed")
    return(data)
  }      
}
