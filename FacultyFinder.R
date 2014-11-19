Cite_Data_Long <- read.csv(".\\workingData\\PublicationData_UTF8.csv", fileEncoding = "UTF-8", stringsAsFactors=FALSE)
CPCMemberInfo_Short <- read.csv(".\\workingdata\\Attributes_Members_UTF8.csv", fileEncoding="UTF-8", stringsAsFactors=FALSE)
USyd_Faculties <- read.csv(".\\rawdata\\USydFaculties.csv", header=TRUE, sep = ",", stringsAsFactors=FALSE , strip.white=TRUE)

#Countries <- as.vector(read.csv(".\\rawdata\\CountryNamesEN.csv", header=FALSE, sep = ",", stringsAsFactors=FALSE , strip.white=TRUE)$V1)

select_USydRows <- grepl(Cite_Data_Long$Affiliation, pattern="University of Sydney")   # let's assume Scopus gets this right if it is under the uni sydney publications
Cite_Data_Long_Sydney <- Cite_Data_Long[select_USydRows,]

Cite_Data_Long_Sydney$Affiliation <- gsub(pattern='\\(', replacement="", Cite_Data_Long_Sydney$Affiliation)
Cite_Data_Long_Sydney$Affiliation <- gsub(pattern='\\)', replacement="", Cite_Data_Long_Sydney$Affiliation)



# Count_Uni_Sydney_Entries <- function(input_vec, ... ){
#     vec_affil <- strsplit(input_vec, split=", ")[[1]]
#     return(sum(grepl(vec_affil, ...)))
# }

# countUSyd <- sapply(Cite_Data_Long_Sydney$Affiliation, FUN=Count_Uni_Sydney_Entries, pattern= "University of Sydney")
# hist(countUSyd)

find_exact_match <- function(input_string, comparison=USyd_Faculties){
                                vec_affil <- strsplit(input_string, split=", " )[[1]]
                                    #grepl(vec_affil, pattern= "University of Sydney")
                                matches <- character()
                                if (length(vec_affil) > 1) {
                                                            # find all that match at all
                                                            matches_search <- amatch(vec_affil, table=comparison$searchterm, method=c("lcs"), maxDist=5)
                                                            # reduce the list of NAs and numbers to only numbers
                                                            matches_search <- matches_search[!is.na(matches_search)]
                                                            # if anything is found matching, take these matches and look up the corresponding fillterms
                                                            if (length(matches_search) > 0) matches <- unique(comparison$fillterm[matches_search])
                                                            } else matches <- vec_affil
                                out <- NA
                                # if any fillterms are found, put these into the out vatiable and return
                                if (length(matches) > 0 ) out <- paste(matches,  collapse="; " )
                                return( out )
                        }
Cite_Data_Long_Sydney$Affiliation[92]

find_exact_match( input_string=Cite_Data_Long_Sydney$Affiliation[92], comparison=USyd_Faculties)

find_within_Affil_string <- function(input_string, comparison=USyd_Faculties){
                                    # this function checks if any of the comparisons match the input string 
                                    matches <- character()
                                    matches_search <- sapply(comparison$searchterm, FUN=function(x) grepl(pattern=x ,input_string) )
                                            
                                    if (any(matches_search)) matches <- unique(comparison$fillterm[matches_search])
                                   
                                    out <- NA
                                    if ( length(matches) > 0 ) out <- paste(matches, collapse="; ")
                                    return( out )
}
 


#find_exact_match(input_string=Affil_string, comparison=USyd_Faculties)

Cite_Data_Long_Sydney$WhatMatched <- NA

# first run looking at very good matches that can be found via amatch
for (i in 1:nrow(Cite_Data_Long_Sydney) ){
        tmp <- find_exact_match(input_string=Cite_Data_Long_Sydney$Affiliation[i], comparison=USyd_Faculties) 
        Cite_Data_Long_Sydney$WhatMatched[i]  <- tmp
print(i)
}

nomatches <- which(is.na(Cite_Data_Long_Sydney$WhatMatched))


for (i in nomatches){
Affil_string <- Cite_Data_Long_Sydney$Affiliation[i]
            tmp <- find_within_Affil_string(input_string=Affil_string, comparison=USyd_Faculties)
            Cite_Data_Long_Sydney$WhatMatched[i]  <- tmp
print(i)
}

still_nomatches <- which(is.na(Cite_Data_Long_Sydney$WhatMatched))



##### attempt at machine learning classification

library(tm)

Cite_Data_Long_Sydney


CleanCorpus <- function(corpus){
                                corpus.tmp <- tm_map(corpus, removePunctuation)
                                corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
                                #corpus.tmp <- tm_map(corpus.tmp, tolower)
                                corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("english"))
                                return(corpus.tmp)
                                }



corpus_raw <- Corpus(VectorSource(tolower(Cite_Data_Long_Sydney$Affiliation)))
corpus_clean <- CleanCorpus(corpus_raw)
tdm <- DocumentTermMatrix(corpus_clean, control = list(minWordLength = 3))
inspect(tdm)

tdm_df <- as.data.frame(data.matrix(tdm), stringsAsFactors = FALSE)
tdm_df <- cbind(tdm_df, Faculty=Cite_Data_Long_Sydney$WhatMatched)

idx.NA <- is.na(tdm_df$Faculty)
idx.multi <- grep(tdm_df$Faculty, pattern="; ")


idx.train <- 1:nrow(tdm_df)                         #this is every individual row index 1:66297
idx.train <- idx.train[!idx.NA]                     # 66297 TRUE FALSES
idx.train <- idx.train[idx.train %in% idx.multi]   # more 66297 TRUE FALSES - 15328 have multiple matches


idxidx.test <- sort(sample(1:length(idx.train), size=floor( 0.7*length(idx.train)) ) )  # 

idx.test  <- idx.train[  idxidx.test]
idx.train <- idx.train[- idxidx.test]



#### need to reduce the features?

tdm_df$Faculty[idx.train] ## somethingi s malaligned here =(

library(e1071)
svm.model <- svm(Faculty ~ ., data=tdm_df[idx.train,], kernel="linear")  # cost 1 or 1000
svm.pred <- predict(svm.model, tdm_df[idx.test, -ncol(tdm_df)], type="class")
table(pred = svm.pred, true = tdm_df[idx.test, ncol(tdm_df)], )

library(nnet)
nnet.model <- nnet(Faculty ~ ., data=tdm_df, subset=idx.train, size=3)


library(glmnet)
Outcome <- tdm_df$Faculty
### doesnt work, cant be bothered finding out why
glmnet.model <- glmnet(y=Outcome[idx.train], x=tdm[idx.train,] , family="multinomial")
