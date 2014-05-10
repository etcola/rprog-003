corr <- function(directory, threshold=0) {
    corr_vect <- numeric()
    for (i in 1:332) {
        filepath <- paste(directory, "/", coverseindex(i), ".csv", sep="")
        file <- read.table(filepath, sep=",", header=TRUE)
        is_complete <- !is.na(file$sulfate) & !is.na(file$nitrate)
        if (length(file[is_complete, "ID"]) > threshold) {
            corr_vect <- c(corr_vect, 
                           cor(file[is_complete, "sulfate"], 
                               file[is_complete, "nitrate"]))
        }
    }
    corr_vect
}

# function that converses integer id into string index
coverseindex <- function(index) {
    if (index != as.integer(index)) {
        str_index <- paste("index_error_", index, sep="")
    } else if (index >= 1 & index < 10) {
        str_index <- paste("00", index, sep="")
    } else if (index >= 10 & index < 100) {
        str_index <- paste("0", index, sep="")
    } else  {
        str_index <- as.character(index)
    }
}