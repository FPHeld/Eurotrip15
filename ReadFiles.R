library(xlsx)
library(dplyr)


source(file="SupportFunctions.R")

sourcefilesNodes <- list.files(path="./rawdata/rawNodeData")
sourcefilesPeople <- "2014 08 25 CPC Contacts.xlsx"
sourcefilesCitations <- list.files(path="./rawdata/dat_CitationData", recursive=TRUE ,  include.dirs = TRUE, pattern="*.csv")
sourcefilesCitations <- sourcefilesCitations[-c(13, 26, 31)]  #  ignore "2013_1.csv" 

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

###### Extract main info about CPC Nodes:  "Title"      "ShortTitle" "MainDomain" "MainTheme" ####

MinimalNodeInfo <- unique(select(ShortNodeInfo, Title, ShortTitle))


MinimalNodeInfo <- merge_a_column(dat=MinimalNodeInfo, sourcedat=ShortNodeInfo, matchvar="Title", valuevar="MainDomain")
MinimalNodeInfo <- merge_a_column(dat=MinimalNodeInfo, sourcedat=ShortNodeInfo, matchvar="Title", valuevar="MainTheme")

dir.create("./workingData")
write.csv(MinimalNodeInfo, file="./workingdata/Attributes_ProjectNodes_UTF8.csv", fileEncoding="UTF-8")

# matching seems to work
### MinimalNodeInfo[,2:4]
###select(ShortNodeInfo, Title, MainDomain, MainTheme)

###### Extract main info about node membership: MembershipInfo $ "ShortTitle" "Members"    "Date"    ####

# extract membership information from ShortNodeInfo
MembershipInfo <- data.frame(ShortTitle=character(), Members=character(), Date=character(), stringsAsFactors = FALSE)

for (i in 1:nrow(ShortNodeInfo)){
    MembershipInfo <- rbind( MembershipInfo, 
                             identify_node_members(Nodename=ShortNodeInfo$ShortTitle[i], 
                                                   Leader=ShortNodeInfo$Leader[i], 
                                                   Members=ShortNodeInfo$Members[i],
                                                   Date = as.character(ShortNodeInfo$Date[i])
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
    
    

###### Extract Citation Relationships from Citation Data #### 

Cite_Data <- data.frame()

for ( i in sourcefilesCitations ){
                            tmp <- read.csv(paste(".\\rawdata\\dat_CitationData\\", i, sep=""), as.is=TRUE, header = TRUE, encoding="UTF-8")  
                                tmp_short <- select(tmp, Title, Year, Authors.with.affiliations, Author.Keywords, Index.Keywords)
                                tmp_short <- cbind(tmp_short, DataSource = i)
                            print(i)
                                Cite_Data <- rbind(Cite_Data, tmp_short)
                            }


Cite_Data <- Cite_Data[!duplicated(select(Cite_Data, -DataSource)), ]   # removes duplicates that may be downloaded in more than one dataset - should yield 17431 rows for 2011-13
Cite_Data <- mutate(Cite_Data, Article.ID = 1:nrow(Cite_Data))     # Append a unique document identifier, in the absence of DOI/ISBN

# write to external file b/c this df is very long and it is not known how long it will be at  the end

outfile <- file(description = ".\\workingData\\PublicationData_UTF8.csv", open = "w", blocking = TRUE, encoding = "UTF-8")
write.table(Make_Long_Author_DF(Cite_Data, index=1),
            file = outfile, append = FALSE, row.names = FALSE, fileEncoding = "UTF-8",  sep = ",",
            quote = c(1,2,3,4), qmethod = "double", eol = "\n", na = "NA", dec = ".", col.names = TRUE)
            

for (i in 2:nrow(Cite_Data)){
                            write.table(Make_Long_Author_DF(Cite_Data, index=i), 
                                        file = outfile, append = TRUE, 
                                        row.names = FALSE, fileEncoding = "UTF-8",  sep = ",",
                                        quote = c(1,2,3,4), qmethod = "double", eol = "\n", na = "NA", dec = ".", col.names = FALSE)
                                        print(i)
                                        }
close(outfile)

Cite_Data_Long <- read.csv(".\\workingData\\PublicationData_UTF8.csv", fileEncoding = "UTF-8")

CPCMemberInfo_Short <- read.csv(".\\workingdata\\Attributes_Members_UTF8.csv", fileEncoding="UTF-8", stringsAsFactors=FALSE)




# identify people with the name that matches CPC members
select_matchCPCautors <- Cite_Data_Long$SimplAuth %in% CPCMemberInfo_Short$MatchName
### identify usyd columns
select_USydRows <- grepl(Cite_Data_Long$Affiliation, pattern="University of Sydney")

#table(select_USydRows, select_matchCPCautors)

CPC_New <- c("Gallagher R", "James D", "Raubenheimer D") 
select_newBiggies <- Cite_Data_Long$SimplAuth %in% CPC_New
 # Cite_Data_Long[select_newBiggies & !select_USydRows,]
# select all interesting, CPC related publications

select_allCPC_members <- (select_newBiggies & !select_USydRows) | (select_USydRows & select_matchCPCautors)
select_allCPC_ArtIDs <- Cite_Data_Long$ArtID[select_allCPC_members]
select_allCPC_Coauthors <- Cite_Data_Long$ArtID %in% select_allCPC_ArtIDs

##### enrich author properties

Cite_Data_Long$USyd <- select_USydRows
Cite_Data_Long$CPC  <- select_allCPC_members

#### author properties

Cite_Data_CPCPlus <- Cite_Data_Long[select_allCPC_Coauthors,]

Attributes_Coauthors <- unique(select(Cite_Data_CPCPlus, Author, SimplAuth, USyd, CPC)) # careful, SimplAuth is too simple
write.csv(Attributes_Coauthors, ".\\workingdata\\Attributes_Coauthors_UTF8.csv", fileEncoding="UTF-8")

Relationships_Coauthors <- select(Cite_Data_CPCPlus, Author, ArtID, Year)
write.csv(Relationships_Coauthors, file="./workingdata/Relationships_Coauthors_UTF8.csv", fileEncoding="UTF-8")

