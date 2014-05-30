best <- function(state, type) {
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    states <- unique(outcome$State)
    hospital <- "Hospital.Name"
    if (!state %in% states) {
        stop("invalid state")
    }
    if (!type %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    cnames <- grep("^Hospital.30.Day.Death", names(outcome), value=TRUE)
    cname <- grep(sub(" ", ".", type), cnames, ignore.case=TRUE, value=TRUE)
    temp <- outcome[outcome$State == state, c(hospital, cname)]
    temp[order(as.numeric(temp[, cname])), ][1, hospital]
}
