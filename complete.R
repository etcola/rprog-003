complete <- function(directory, id = 1:332) {
    df <- data.frame(id=integer(), nobs=integer())
    for (i in id) {
        filepath <- paste(directory, "/", coverseindex(i), ".csv", sep="")
        # exclude invalid IDs that can't be found in the directory
        if (!file.exists(filepath)) {
            print(paste("Can't find the file:", filepath))
            print(paste("Exclude ID", i))
            next
        }
        file <- read.table(filepath, sep=",", header=TRUE)
        is_complete <- !is.na(file$sulfate) & !is.na(file$nitrate)
        df <- rbind(df, c(i, length(file[is_complete, "ID"])))
  }
  
  colnames(df) <- c("id", "nobs")
  df
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