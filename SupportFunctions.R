# this function extracts the bare essentials about nodes from the raw files. 
## it expects inputs a df with relevant fields and date indicating when that information wa created 
extract_essentials <- function(data, date){ 
    if("Governance..Domain" %in% colnames(data)) {
        tmp <- data.frame( Title = data$Title,
                           Leader= data$Proposer..Leader.s.,
                           Members = data$Proposed.Development.Group,
                           Status = data$Status,
                           MainDomain = data$Governance..Domain,
                           MainTheme = data$Governance..Theme,
                           Date = date)
                           }
    else { 
        tmp <- data.frame( Title = data$Title,
                           Leader= data$Proposer..Leader.s.,
                           Members = data$Proposed.Development.Group,
                           Status = data$Status,
                           MainDomain = NA,
                           MainTheme = NA,
                           Date = date)
                           }
    return(tmp)
    }
# function for merging colum valuevar (as string) from df sourcedat to df dat, via joint variable matchvar (as string)
## intended to be used when there are alos nas in sourcedat's valuevar column
merge_a_column <- function(dat, sourcedat, matchvar, valuevar){
    tmpdat <- sourcedat[!is.na(sourcedat[,valuevar]),]
    matchorder <- match( dat[, matchvar], tmpdat[, matchvar] )
     dat[, valuevar] <- tmpdat[, valuevar][matchorder]
    return(dat)
}

### Wrapper for Isolate Participants and Isolate.Leaders
identify_node_members <- function(Nodename, Leader, Members, Date){
    Leader <- Isolate.Leaders(as.character(Leader))
    Participants <- Isolate.Participants(Members)
    out <- cbind(ShortTitle = Nodename, Members = c(Leader, Participants), Date=Date)
    return(out)
}


Isolate.Participants <- function(AString){
                                          # assume x a concatenated string of names
                                          x <- AString
                                          if (AString != ""){
                                          #x <- CPCNodesNames$Participants[14]
                                          x <- gsub(x, pattern=";#[0-9]+", replacement="")

                                          x <- gsub(x, pattern=" +,", replacement=",")
                                          x <- gsub(x, pattern="\t", replacement="", fixed=TRUE)

                                          #x <- gsub(x, pattern="[a-z]+;#|[a-z]+ ;#", replacement="\\.;;")
                                          #x <- gsub(x, pattern="[a-z]+$|[a-z]+ $", replacement="\\.")
                                          x <- gsub(x, pattern=" , ", replacement=", ")
                                          x <- gsub(x, pattern=" $", replacement="")
                                          participants.list <- strsplit(x, split=";#")[[1]]
                                          return(  participants.list)
                                          #return(data.frame(Leader = FALSE, Participant = TRUE, Researcher =  participants.list))
                                          }
                                          #else{participants.list <- ""}
                                         }

Isolate.Leaders <- function(AString){
                                     #assume x a concatenated string of names
                                     if (AString != ""){
                                     x <- AString
                                     #x <- CPCNodesNames$Leader[14]
                                     x <- gsub(x, pattern=";#[0-9]+", replacement="")
                                     x <- gsub(x, pattern=" +,", replacement=",")
                                     x <- gsub(x, pattern="\t", replacement="", fixed=TRUE)
                                     #x <- gsub(x, pattern="[a-z]+;#|[a-z]+ ;#", replacement="\\.;;")
                                     #x <- gsub(x, pattern="[a-z]+$|[a-z]+ $", replacement="\\.")
                                     x <- gsub(x, pattern=" , ", replacement=", ")
                                     x <- gsub(x, pattern=" $", replacement="")
                                     Leader.list <- strsplit(x, split=";#")[[1]]
                                     return(Leader.list)
                                     #return(data.frame(Leader = TRUE, Participant = FALSE, Researcher = Leader.list))
                                     }
                                     #else{Leader.list <- ""}
                                     }
