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

trim <- function (x) gsub("^\\s+|\\s+$", "", x)



make_short_name <- function(AFullName){a <- regexpr("[a-z], [A-Z]", AFullName)
                                        Last <- substr(AFullName, 1, a )
                                        Initial <- substr(AFullName, (a+3), (a+3))
                                        return(paste(Last, Initial, sep=" "))}


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

#### strings for extraction author/affiliation information from Authors.with.affiliations field

# Extract authors' names for each publication  from a list like  "SurnameA, XA. AffiliationA1, CountryA1, AffiliationA2, CountryA2;" or
                                                              #  "SurnameB, XB. AffiliationB1, CountryB1, AffiliationB2, CountryB2"
Isolate.Name <- function(x){
                            if (mode(x) != "character") stop("Input is not a Character String")
                            if (nchar(x) == 0) stop("Affiliation is empty")
                            # name is at the beginning and ends with an "., "
                            tmpEnd <- regexpr("[A-Z]\\., ", x)[1] + 1 # includes the period behind the name
                            #Extract the name
                            Indiv.Author.Name <- substr(x, start=1, stop=tmpEnd)
                            # Remove Comma after Surname
                           # Indiv.Author.Name <- gsub(Indiv.Author.Name, pattern=",", replacement="")
                            # Should there be a middle innitial, this will be cut, b/c such information is not available in the CPC data dump
                            return(Indiv.Author.Name)
                            }


# To be Applied to the column that contains "SurnameA, XA. AffiliationA1, CountryA1, AffiliationA2, CountryA2; SurnameB, XB. AffiliationB1, CountryB1, AffiliationB2, CountryB2"
Isolate.Affiliation <- function(x){
                                   if (mode(x) != "character") stop("Input is not a Character String")
                                   if (nchar(x) == 0) stop("No affiliation provided")
                                  # cut after name at the beginning and ending with "., "
                                  tmpEnd <- regexpr("[A-Z]\\., ", x)[1] + 1 # includes the period behind the name
                                  #Extract the name
                                  #Syd.Author.Name <- substr(x=SydneyAutors[1], start=1, stop=tmpEnd)
                                  All.Author.Affiliations <- substr(x, start=tmpEnd + 3, stop=nchar(x))
                                  return(All.Author.Affiliations)
                                  }


### bring all the author and affiliation information together in one long data frame  #####
Make_Long_Author_DF <- function(widedat, index){
                            
                            tmprow <- widedat[index,]
                            tmpAuthAfil  <- strsplit(tmprow$Authors.with.affiliations, split="; " )[[1]]
                            tmpAuthors   <- unlist(lapply(tmpAuthAfil, Isolate.Name))
                            tmpAffiliations <- unlist(lapply(tmpAuthAfil,Isolate.Affiliation ))

                            SimplAuth <- unlist(lapply(tmpAuthors, function(x){Endcut <- regexpr(x, pattern=",[[:space:]][[:upper:]].")
                                                                                 return(paste(substr(x, 1, (Endcut - 1)), substr(x, (Endcut + 2), (Endcut + 2)),
                                                                                              sep=" " )) }
                                                ))
                            output   <- data.frame(Author=tmpAuthors, 
                                                  Affiliation=tmpAffiliations, 
                                                  SimplAuth=SimplAuth,  
                                                  #Title = tmprow$Title, 
                                                  Year = tmprow$Year, 
                                                  ArtID=tmprow$Article.ID)
                            return(output)                                
}

# Match.Name <- function(x){
#                            if (mode(x) != "character") stop("Input is not a Character String")
#                            if (nchar(x) == 0) stop("No name provided")
#                           }



##### string manipulation for keywords

Isolate.Keywords <- function(AK, IK){
                                     AKeywords.list <- strsplit(AK, split="; " )[[1]]
                                     IKeywords.list <- strsplit(IK, split="; " )[[1]]
                                     return(  c(AKeywords.list, IKeywords.list)  )
                                    }

# very simple stemming algorithm- essentially changes all words to lower case and cuts off most terminal s's
SimpleStem <- function(x){
                           tmpString <- x
                           if (tmpString != "")
                                 {
                                 tmpString <- tolower(tmpString)
                                 if ( !grepl(tmpString, pattern="ss$") && !grepl(tmpString, pattern="is$") && grepl(tmpString, pattern="s$")){ tmpString <- sub(tmpString, pattern="s$", replacement="")}
                                 if ( grepl(tmpString, pattern="^the ")){ tmpString <- sub(tmpString, pattern="^the ", replacement="")}
                                 }

                           return(tmpString)
                          }


