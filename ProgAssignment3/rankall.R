rankall <- function(type, num="best") {
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    states <- sort(unique(outcome$State))
    hospital <- "Hospital.Name"
    if (!type %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    cnames <- grep("^Hospital.30.Day.Death", names(outcome), value=TRUE)
    cname <- grep(sub(" ", ".", type), cnames, ignore.case=TRUE, value=TRUE)
    df <- data.frame()
    for (state in states) {
        temp <- outcome[outcome$State == state, c(hospital, cname)]
        if (num == "best") {
            df <- rbind(df, data.frame(temp[order(as.numeric(temp[, cname])), ][1, hospital], state))
        } else if (num == "worst") {
            df <- rbind(df, data.frame(temp[order(as.numeric(temp[, cname]), decreasing=TRUE), ][1, hospital], state))
        } else {
            df <- rbind(df, data.frame(temp[order(as.numeric(temp[, cname])), ][as.numeric(num), hospital], state))
        }    
    }
    colnames(df) <- c("hospital", "state")
    rownames(df) <- states
    df
}
