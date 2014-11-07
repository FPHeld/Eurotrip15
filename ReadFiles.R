library(xlsx)
library(dplyr)


source(file="SupportFunctions.R")

sourcefilesNodes <- list.files(path="./rawdata/rawNodeData")
sourcefilesPeople <- "2014 08 25 CPC Contacts.xlsx"
sourcefilesCitations <- list.files(path="./rawdata/dat_CitationData")

##### Read CPC Node data from dumps: store info in ShortNodeInfo ##########

RawNodeInfo <- list(NULL)

for (i in 1:length( sourcefilesNodes )){
    RawNodeInfo[[i]] <- read.xlsx(file=paste0("./rawdata/rawNodeData/", sourcefilesNodes[i]), 
                                  sheetIndex=1, as.data.frame=TRUE, header=TRUE)
}
names(RawNodeInfo) <- sourcefilesNodes

ShortNodeInfo <- rbind(
                        extract_essentials(RawNodeInfo[[1]], as.Date("130701", "%y%m%d")),
                        extract_essentials(RawNodeInfo[[2]], as.Date("130816", "%y%m%d")),
                        extract_essentials(RawNodeInfo[[3]], as.Date("140220", "%y%m%d")),
                        extract_essentials(RawNodeInfo[[4]], as.Date("140429", "%y%m%d")),
                        extract_essentials(RawNodeInfo[[5]], as.Date("140825", "%y%m%d"))
                      )    

ShortNodeInfo <- ShortNodeInfo[ShortNodeInfo$Status == "Active" & !is.na(ShortNodeInfo$Status) & 
                                   nchar(as.character(ShortNodeInfo$Leader)) > 0 &
                                   nchar(as.character(ShortNodeInfo$Members)) > 0 , ]

# sapply(ShortNodeInfo$Members, FUN=function(x)(nchar(as.character(x))))

ShortNodeInfo$ShortTitle <- tolower(substr(ShortNodeInfo$Title, start=1, stop=20))

##### Extract main info about CPC Nodes:  "Title"      "ShortTitle" "MainDomain" "MainTheme" ####

MinimalNodeInfo <- unique(select(ShortNodeInfo, Title, ShortTitle))


MinimalNodeInfo <- merge_a_column(dat=MinimalNodeInfo, sourcedat=ShortNodeInfo, matchvar="Title", valuevar="MainDomain")
MinimalNodeInfo <- merge_a_column(dat=MinimalNodeInfo, sourcedat=ShortNodeInfo, matchvar="Title", valuevar="MainTheme")

dir.create("./workingData")
write.csv(MinimalNodeInfo, file="./workingdata/Attributes_ProjectNodes_UTF8.csv", fileEncoding="UTF-8")

# matching seems to work
### MinimalNodeInfo[,2:4]
###select(ShortNodeInfo, Title, MainDomain, MainTheme)

#### Extract main info about node membership: MembershipInfo $ "ShortTitle" "Members"    "Date"    ####

# extract membership information from ShortNodeInfo
MembershipInfo <- data.frame(ShortTitle=character(), Members=character(), Date=numeric())

for (i in 1:nrow(ShortNodeInfo)){
    MembershipInfo <- rbind( MembershipInfo, 
                             identify_node_members(Nodename=ShortNodeInfo$ShortTitle[i], 
                                                   Leader=ShortNodeInfo$Leader[i], 
                                                   Members=ShortNodeInfo$Members[i],
                                                   Date = ShortNodeInfo$Date[i]
                                                   )
                            )
    }
        ### stringsasfactors strikes again
        MembershipInfo$ShortTitle <- as.character(MembershipInfo$ShortTitle)
        #some quick cleanup
        tmpResearcher <- trim(MembershipInfo$Member) #remove trailing spaces front and back
        #Dirty Quickfixes for names that could not be cleaned properly
        tmpResearcher[which(grepl("^.+Singh, Maria.*$",tmpResearcher))] <- "Fiatarone-Singh, Maria"
        tmpResearcher[which(tmpResearcher == "Cerneaz, Nick Cerneaz")] <- "Cerneaz, Nick"
        tmpResearcher[which(grepl("^.+Loughlin, Kate$",tmpResearcher))] <- "O'Loughlin, Kate"
        tmpResearcher[which(tmpResearcher == "Oâ€™Loughlin, Kate")]
        tmpResearcher[which(tmpResearcher == "Kunic, Zdenka")] <-  "Kuncic, Zdenka"
        tmpResearcher[which(tmpResearcher =="Morris, Jonathon")] <-  "Morris, Jonathan" 
        tmpResearcher[which(tmpResearcher =="Reddy, Parusa")] <- "Reddy, Prasuna"
        tmpResearcher[which(tmpResearcher =="Sudoyu, Herawati")] <- "Sudoyo, Herawati"  
        tmpResearcher[which(tmpResearcher =="Febina, Clarissa Asha")] <- "Febinia, Clarissa Asha"

        MembershipInfo$Members <- tmpResearcher
        
       # sort(unique(MembershipInfo$ShortTitle))  ## did the preconception study change its name at some point?




write.csv(MembershipInfo, file="./workingdata/Relations_NodeMembership_UTF8.csv", fileEncoding="UTF-8")

###### Extract main info about CPC affiliated researchers: CPCMemberInfo_Short $ "First.Name" "Email.Address" "Faculty.Affiliation" ... #####
                                                            # "ID"  "Last.Name" "Salutation" "Full.Name""MatchName"...
    
CPCMembers_raw  <- read.xlsx(  file=paste0("./rawdata/", sourcefilesPeople), 
                                  sheetIndex=1, as.data.frame=TRUE, header=TRUE  )
CPCMemberInfo_Short <- select(CPCMembers_raw, First.Name, Email.Address, Faculty.Affiliation, ID, Last.Name, Salutation) 

#trim spaces leading and trailing
#trim <- function (x) gsub("^\\s+|\\s+$", "", x)

CPCMemberInfo_Short$First.Name <- trim(CPCMemberInfo_Short$First.Name)
CPCMemberInfo_Short$Last.Name <- trim(CPCMemberInfo_Short$Last.Name)
CPCMemberInfo_Short$Email.Address <- trim(CPCMemberInfo_Short$Email.Address)
CPCMemberInfo_Short$Faculty.Affiliation <- trim(CPCMemberInfo_Short$Faculty.Affiliation)
CPCMemberInfo_Short$Salutation  <- trim(CPCMemberInfo_Short$Salutation)

CPCMemberInfo_Short$Full.Name <- paste(CPCMemberInfo_Short$Last.Name, CPCMemberInfo_Short$First.Name, sep=", ")

CPCMemberInfo_Short <- CPCMemberInfo_Short[!duplicated(CPCMemberInfo_Short$Full.Name),]

#  sort(MembershipInfo$Members[!MembershipInfo$Members %in% CPCMemberInfo_Short$Full.Name])  # noone left, everyone matches

CPCMemberInfo_Short$MatchName <- make_short_name(CPCMemberInfo_Short$Full.Name)
write.csv(CPCMemberInfo_Short, ".\\workingdata\\Attributes_Members_UTF8.csv", fileEncoding="UTF-8")
    
    

##### Extract Citation Relationships from Citation Data #### 


sourcefilesCitations


#CountryNames<- read.csv("CountryNamesEN.csv", as.is=TRUE, header = FALSE)$V1

CPC_Member_Attributes <- read.csv(".\\WorkingFiles\\Attributes_CPCMembers_UTF8.csv",  fileEncoding="UTF-8")

Raw.File.Names <- dir(".\\Data\\dat_CitationData")


Cite_Data <- data.frame()

for ( i in sourcefilesCitations ){tmp <- read.csv(paste(".\\rawdata\\dat_CitationData\\", i, sep=""), as.is=TRUE, header = TRUE, encoding="UTF-8")  
                            #concatenate file name and path
                            #str(imported.data)
                            names(tmp)[1] <- "Authors"
                            tmp <- cbind(tmp, DataSource = i)
                            print(i)
                            Cite_Data <- rbind(Cite_Data, tmp)
                            }

#Cite_Data$Authors.with.affiliations[30000]

nrow(Cite_Data)
colnames(Cite_Data)
Cite_Data <- Cite_Data[!duplicated(Cite_Data[, -21]), ]   # removes duplicates that may be downloaded in more than one dataset - should yield 17431 rows for 2011-13
nrow(Cite_Data)

Cite_Data <- mutate(Cite_Data, Article.ID = 1:nrow(Cite_Data))     # Append a unique document identifier, in the absence of DOI/ISBN
dim(Cite_Data)
colnames(Cite_Data)
names(Cite_Data)


Cite_Data <- Cite_Data[-15601,] #Trouble with: Ho?bye, C., Department of Endocrinology, Metabolism, and Diabetology, Karolinska University Hospital, SE-17176 Stockholm, Sweden;

#filter(AllData, Article.ID == 1)
#AllData[15601,]
#
#dplyr::filter(AllData, Article.ID==15602)


# Define Scopus specific procedures to extract the data in individual columns
# split and merge the two keyword columns

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


Match.Name <- function(x){
                           if (mode(x) != "character") stop("Input is not a Character String")
                           if (nchar(x) == 0) stop("No name provided")
                          }




CPC_Names <-     as.character(unique(CPC_Member_Attributes$SimplAuth[ CPC_Member_Attributes$USYD]))
CPC_Externals <- as.character(unique(CPC_Member_Attributes$SimplAuth[!CPC_Member_Attributes$USYD]))
CPC_Externals <- substr(CPC_Externals, 6, nchar(CPC_Externals))
CPC_New <- c("Gallagher R", "James D", "Raubenheimer D") 
CPC_Externals <- c(CPC_Externals, CPC_New)

######## Workhorse, creating a long long list of Researchers x Articles x Keywords   ######################



# Run in separate batches of 1000 or 2000 entries each (algorithm slows down)

outfile <- file(description = ".\\WorkingFiles\\PublicationData_UTF8.csv", open = "w", blocking = TRUE, encoding = "UTF-8")
#open(outfile, open = "a", blocking = TRUE)
for (i in 1:nrow(Cite_Data)){
#  for (i in 1:10){
    # AUTHORS AND AFFILIATIONS
                            # i <- 81
                            tmprow <- Cite_Data[i,]
                            # tmprow <- Cite_Data[2509,] 

                            tmpAuthAfil  <- strsplit(tmprow$Authors.with.affiliations, split="; " )[[1]]
                            tmpAuthors   <- unlist(lapply(tmpAuthAfil, Isolate.Name))
                            tmpAffiliations <- unlist(lapply(tmpAuthAfil,Isolate.Affiliation ))

                            SimplAuth <- unlist(lapply(tmpAuthors, function(x){Endcut <- regexpr(x, pattern=",[[:space:]][[:upper:]].")
                                                return(paste(substr(x, 1, (Endcut - 1)), substr(x, (Endcut + 2), (Endcut + 2)), sep=" " )) }
                                                ))

                            # Identify the Uni of Sydney Authors
                            USydRows <- unlist(lapply( tmpAffiliations, function(x){grepl( x, pattern="University of Sydney")}))
                            # Check if those authors with an affiliation to USyd actually match CPC participants
                            MatchCPC <- SimplAuth %in% CPC_Names
                            
                            MatchCPCExt <- SimplAuth %in% CPC_Externals
                            # If USydRows and MatchCPC are both true, this suggestss that we have identified the right person (would exclude david Raubenheimer tho)
                            
                            # add prefit "ext_" to those author names that are not USyd and not a CPC external member
                            prefix <- rep("", length(SimplAuth) )
                            prefix[!USydRows] <- "x_"
                            prefix[!USydRows & MatchCPCExt] <- "CPCx_"
                            SimplAuth <- paste(prefix, SimplAuth, sep="")
                            
                            CPC_Pub <- sum(USydRows & MatchCPC) + sum(!USydRows & MatchCPCExt) > 0
                            #if (CPC_Pub) print(tmprow)
                            
                            # KEYWORDS
                      #     tmpKeys <- Isolate.Keywords(tmprow$Author.Keywords, tmprow$Index.Keywords)
                      #     tmpSimplKeys <- unlist(lapply(tmpKeys, SimpleStem))
                      #     if (length(unique(tmpSimplKeys)) < length(tmpSimplKeys)){
                      #                                                              tmpKeys <- tmpKeys[!duplicated(tmpSimplKeys)]
                      #                                                              tmpSimplKeys <- tmpSimplKeys[!duplicated(tmpSimplKeys)]
                      #                                                              }
                      #     length(tmpKeys) == 0
                            
                            
                           # The following section combines all the relevant infromation for each row of data
                      #        twoKeys <- data.frame(Key=tmpKeys, SimplKey=tmpSimplKeys)
                      #        if (nrow(twoKeys) == 0){twoKeys <- data.frame(Key=NA, SimplKey=NA)}
                            twoAA   <- data.frame(Author=tmpAuthors, Affiliation=tmpAffiliations, SimplAuth=SimplAuth, 
                                                  SydAfil=USydRows, MatchCPC=MatchCPC, MatchCPCExt=MatchCPCExt)
                            OtherPublDetails <- data.frame(CPC_Pub=CPC_Pub, Title = tmprow$Title, Year = tmprow$Year, ArtID=tmprow$Article.ID)
                           
                           # INFO on CPC Involvement - extends the twoAA df, by adding information of the Authors' CPC node involvement
                         #  New_Entries <- data.frame()
#                           for (j in 1:nrow(twoAA)){
#                                                    tmpentry <- twoAA[j,]
#                                                    if (USydRows[j]){
#                                                                     CPCParticipant = FALSE
#                                                                     ProjectNode = ""
#                                                                     tmpName <- tmpentry$Author
#                                                                     NodeIndexes <- unlist( lapply ( CPC.Projects$MatchName, FUN=function(z){ regexpr(z, tmpName) > 0 } ) )
#                                                                     if (sum(NodeIndexes) > 0) {CPCParticipant = TRUE}
#                                                                     extdRow <- merge(cbind(tmpentry, CPCParticipant), data.frame(ProjectNode = CPC.Projects$Title[NodeIndexes]))
#                                                                     }
#                                                                     else
#                                                                     {
#                                                                      CPCParticipant = FALSE
#                                                                      ProjectNode = ""
#                                                                      extdRow <- cbind(tmpentry, CPCParticipant, data.frame(ProjectNode = ProjectNode))
#                                                                     }
#                                                    New_Entries <- rbind(New_Entries, extdRow)
#                                                    }

                                step1 <- merge(twoAA, OtherPublDetails)
                                #step2 <- merge(step1, twoKeys)
                                     ###LongList <- rbind(LongList, step2)

                            
                            #### writing only step1 to output makes this list a lot shorter, as it ignores the many many keywords.
if (i == 1) {write.table(step1, file = outfile, append = FALSE, row.names = FALSE, fileEncoding = "UTF-8",  sep = ",",
                         quote = c(1,2,3,8), qmethod = "double", eol = "\n", na = "NA", dec = ".", col.names = TRUE)
                         } else
            {write.table(step1, file = outfile, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8",  sep = ",",
                         quote = c(1,2,3,8), qmethod = "double", eol = "\n", na = "NA", dec = ".", col.names = FALSE)
                        }
#if (i == 1) {write.table(step2, file = outfile, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8",  sep = ",",
#                         quote = c(1,2,3,6,9,10), qmethod = "double", eol = "\n", na = "NA", dec = ".", col.names = TRUE)
#                         } else
#            {write.table(step2, file = outfile, append = TRUE, row.names = FALSE, fileEncoding = "UTF-8",  sep = ",",
#                         quote = c(1,2,3,6,9,10), qmethod = "double", eol = "\n", na = "NA", dec = ".", col.names = FALSE)
 #                        }
# Do nothing if none of the matching authors have a USyd affiliation
print(i)
}
close(outfile)








