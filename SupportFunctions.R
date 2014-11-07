# this function extracts the bare essentials about nodes from the raw files. 
## it expects inputs a df with relevant fields and date indicating when that information wa created 
extract_essentials <- function(data, date){ 
        data.frame(Title = data$Title,
                   Leader= data$Proposer..Leader.s.,
                   Members = data$Proposed.Development.Group,
                   Status = data$Status,
                   Date = date)
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
                                          return(data.frame(Leader = FALSE, Participant = TRUE, Researcher =  participants.list))
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
                                     return(data.frame(Leader = TRUE, Participant = FALSE, Researcher = Leader.list))
                                     }
                                     #else{Leader.list <- ""}
                                     }
