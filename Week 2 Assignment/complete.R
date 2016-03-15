complete <- function(directory, id = 1:332){
    # locate the file:
    files <- list.files(directory)
    # read the files indicated by id
    dfs <- list()
    comp <- list()
    for(i in id){
        dfs[[i]] <- read.csv(files[i], header = TRUE)
        # exclude NA cases
        comp[[i]] <- sum(!is.na(dfs[[i]][, 2]) & !is.na(dfs[[i]][, 3]))
    }
    # output new dfs
    nobs <- unlist(comp)
    new_df <- as.data.frame(cbind(id, nobs))
    return(new_df)
}