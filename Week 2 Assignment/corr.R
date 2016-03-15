corr <- function(directory, threshold = 0){
    files <- list.files(directory)
    comp <- complete(directory)
    test <- comp[, 2] >= threshold
    if(sum(test) > 0) {
        id  <- which(test)
        dfs <- list()
        r   <- vector()
        for(j in id){
            dfs[[j]] <- read.csv(files[j], header = TRUE)
            r[j]<- cor(dfs[[j]][, 2], dfs[[j]][, 3], use = "pairwise.complete.obs")
        }
        r <- unlist(r)
        r <- r[which(!is.na(r))]
    } else {
        r   <- vector() # r should be a vector w/ length == 0
    }
    return(r)
}