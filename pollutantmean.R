pollutantmean <- function(directory, pollutant, id = 1:332) {
	# check pollutant
	if (pollutant !="sulfate" && pollutant != "nitrate") {
		print("The pollutant can either be sulfate or nitrate.")
		return()
	}

	# loop over id and read non-NA data into data
	pollutant_vect <- c() # initialize the vector
	for (i in id) {
		filepath <- paste(directory, "/", coverseindex(i), ".csv", sep="")
		
		# exclude invalid IDs that can't be found in the directory
		if (!file.exists(filepath)) {
			print(paste("Can't find the file:", filepath))
			print(paste("Exclude ID", i))
			next
		}
		
		file <- read.table(filepath, sep=",", header=TRUE)
		p_col <- file[, pollutant] # pullutant vector with NAs
		# append non NA values into pollutant vector
		pollutant_vect <- c(pollutant_vect, p_col[!is.na(p_col)])
	}
	
	mean(pollutant_vect)
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