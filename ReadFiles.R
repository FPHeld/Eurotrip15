library(xlsx)
library(dplyr)


source(file="SupportFunctions.R")

sourcefilesNodes <- list.files(path="./rawdata/rawNodeData")
sourcefilesPeople <- "2014 08 25 CPC Contacts.xlsx"
sourcefilesCitations <- list.files(path="./rawdata/dat_CitationData")


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

MinimalNodeInfo <- unique(select(ShortNodeInfo, Title, ShortTitle))


MinimalNodeInfo <- merge_a_column(dat=MinimalNodeInfo, sourcedat=ShortNodeInfo, matchvar="Title", valuevar="MainDomain")
MinimalNodeInfo <- merge_a_column(dat=MinimalNodeInfo, sourcedat=ShortNodeInfo, matchvar="Title", valuevar="MainTheme")



# matching seems to work
### MinimalNodeInfo[,2:4]
###select(ShortNodeInfo, Title, MainDomain, MainTheme)

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
        tmpResearcher <- unlist(lapply(MembershipInfo$Member, gsub, pattern=" $", replacement="")) #remove trailing spaces
        #Dirty Quickfixes for names that could not be cleaned properly
        tmpResearcher[which(tmpResearcher == "Fiatarone-Singh, Maria Fiatarone-Singh")] <- "Fiatarone-Singh, Maria"
        MembershipInfo$Member <- tmpResearcher
        
        sort(unique(MembershipInfo$ShortTitle))  ## did the preconception study change its name at some point?
    
    






CPC.df$Researcher <- tmpResearcher


#Shorten first name to innitial
MatchName <- unlist(lapply(CPC.df$Researcher, function(x){a <- regexpr("[a-z], [A-Z]", x)
                                                               Last <- substr(x, 1, a )
                                                                Initial <- substr(x, (a+3), (a+3))
                                                                return(paste(Last, Initial, sep=" "))}
                                                                ))
CPC.df <- transform(CPC.df,  MatchName=MatchName)



CPCMember_details <- read.csv(file=".\\Data\\2014 08 25 CPC Contacts list for Fabian Held.csv", as.is=TRUE, header=TRUE)
CPCMember_details$SimplAuth <- paste(gsub(CPCMember_details$Last.Name, pattern=" $", replacement=""),
                                     substring(CPCMember_details$First.Name, 1,1),
                                     sep=" ")

CPC_df_xtd <- merge(CPC.df, select(CPCMember_details, SimplAuth, Faculty.Affiliation) , by.x="MatchName", by.y="SimplAuth")

prefix <- rep("", nrow(CPC_df_xtd) )
prefix[CPC_df_xtd$Faculty.Affiliation == "External to University of Sydney"] <- "CPCx_"
CPC_df_xtd$MatchName <- paste(prefix, CPC_df_xtd$MatchName, sep="")



Links <- select(CPC_df_xtd, MatchName, Title_short)
Links <- Links[!duplicated(Links),]

write.csv(Links, ".\\WorkingFiles\\Links_Participants_x_CPCNodes_UTF8.csv", fileEncoding="UTF-8")
write.csv(CPC_df_xtd, ".\\WorkingFiles\\CPCNodeParticipants_UTF8.csv", fileEncoding="UTF-8")


##### isolate project node properties

names(CPC.df)
# "Leader"        "Participant"   "Researcher"    "Title"         "Title_short"   "Main_Domain"   "Main_Theme"    "Status"        "Activity_Type"
# "Project_Index" "Dom1_Pop"      "Dom2_Bio"      "Dom3_SnE"      "Dom4_Sol"      "Theme1_Nut"    "Theme2_PAEEE"  "Theme3_ATSI"   "Theme4_PGE"   
# "Theme5_CSM"    "MatchName"  

Project_Nodes_Attributes <- transform(CPC.df, Type="Project")
Project_Nodes_Attributes <- transform(Project_Nodes_Attributes, USYD=TRUE)
Project_Nodes_Attributes <- transform(Project_Nodes_Attributes, Faculty="CPC")
Project_Nodes_Attributes <- transform(Project_Nodes_Attributes, CPCMember=TRUE)


Project_Nodes_Attributes <- select(Project_Nodes_Attributes, Title_short, Type, USYD, Faculty, CPCMember, 
                                                             Main_Domain, Main_Theme, 
                                                             Dom1_Pop, Dom2_Bio, Dom3_SnE, Dom4_Sol, 
                                                             Theme1_Nut, Theme2_PAEEE, Theme3_ATSI, Theme4_PGE, Theme5_CSM,
                                                             Dom_Biology, Dom_Populations, Dom_SocEnv, Dom_Solutions, 
                                                             Theme_ATSI, Theme_ComplexSystems, Theme_Nutrition, Theme_PAEE, Theme_Gov)

CPC_Nodes_Attributes <- Project_Nodes_Attributes[!duplicated(Project_Nodes_Attributes),]
write.csv(CPC_Nodes_Attributes, ".\\WorkingFiles\\Attributes_CPCNodes_UTF8.csv", fileEncoding="UTF-8")
    
    
