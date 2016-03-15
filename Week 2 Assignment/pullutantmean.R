pollutantmean <- function(directory, pollutant, id = 1:332){
    # locate the file:
    files <- list.files(directory)
    # read the files indicated by id
    dfs <- list()
    for(i in id){
        dfs[[i]]<- read.csv(files[i], header = TRUE)
    }
    # find the mean
    big_df <- do.call(rbind, dfs)
    mean <- mean(big_df[, eval(pollutant)], na.rm = TRUE)
    return(mean)
}