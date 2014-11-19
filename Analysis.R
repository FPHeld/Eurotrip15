library(igraph)

Relations_NodeMembership <- read.csv(file="./workingdata/Relations_NodeMembership_UTF8.csv", 
                                     fileEncoding="UTF-8", stringsAsFactors=FALSE, row.names=1)
dim(Relations_NodeMembership)   #  1412    3
names(Relations_NodeMembership) # "ShortTitle" "Members"    "Date" 


Relations_Coauthorship <-   read.csv(file="./workingdata/Relationships_Coauthors_UTF8.csv", 
                                     fileEncoding="UTF-8", stringsAsFactors=FALSE, row.names=1)
dim(Relations_Coauthorship)   #   38773     3
names(Relations_Coauthorship) # "Author" "ArtID"  "Year"


Attributes_ProjectNodes <-  read.csv(file="./workingdata/Attributes_ProjectNodes_UTF8.csv", 
                                     fileEncoding="UTF-8", stringsAsFactors=FALSE, row.names=1)
dim(Attributes_ProjectNodes)   #  46  4
names(Attributes_ProjectNodes) # ""Title"      "ShortTitle" "MainDomain" "MainTheme" 


Attributes_Members <-       read.csv(file="./workingdata/Attributes_Members_UTF8.csv",      
                                     fileEncoding="UTF-8", stringsAsFactors=FALSE, row.names=1)
dim(Attributes_Members)   #  402   8
names(Attributes_Members) # "First.Name" "Email.Address" "Faculty.Affiliation" "ID" "Last.Name" "Salutation" "Full.Name" "MatchName"


Attributes_Coauthors <-     read.csv(file="./workingdata/Attributes_Coauthors_UTF8.csv",   
                                     fileEncoding="UTF-8", stringsAsFactors=FALSE, row.names=1)
dim(Attributes_Coauthors)   #  16507     4
names(Attributes_Coauthors) #   "Author"    "SimplAuth" "USyd"      "CPC"


# turn Authorship links into coauthorship links

 
graph(select(Relations_Coauthorship, ShortTitle, Members), 



#### there are a few monster publications of 2000+ authors 370 seems to be a good stopping level.
Cocite_freq <- table(as.factor(Pubs_CPC$ArtID)) 
quantile(Cocite_freq, probs=seq(0.90,1, by=0.001)) # the 95% quantile lies at 2207, 99% at 3035 and the maximum is 3125 authors in one paper
# need to cut off at least at 99%, possibly lower
graph.incidence
