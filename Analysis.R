library(xlsx)


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

length(RawNodeInfo)



names(CPC_data) <- c("Title", 
                          "Title_short", 
                          "Leader", 
                          "Participants",
                          "Domain", 
                          "Theme", 
                          "Dom_Biology", 
                          "Dom_Populations", 
                          "Dom_SocEnv", 
                          "Dom_Solutions", 
                          "Theme_ATSI", 
                          "Theme_ComplexSystems", 
                          "Theme_Nutrition", 
                          "Theme_PAEE",
                          "Theme_Gov",
                          "Main_Domain", 
                          "Main_Theme",
                          "Status",  
                          "Activity_Type" )


CPC_data$Title_short <- c(
 "Human-animal Interactions",                                                                                                       
 "Nutrition, Human Health and Natural Resources",                                                                                            
 "Nepean: Coordinated research/education/clinical services",                                                                     
 "Promoting Stair Use",                                                                                                     
 "One Welfare",                                                                                                                              
 "Move to New Building study",                                                                                                              
 "Influence of microbial metabolites on intestinal health",                                                                                 
 "IMELDA",                                                                    
 "e-Health and m-Health network",                                                                                                           
 "Implementation Science",                                                                                                                   
 "Preconception, pregnancy and childhood cohort study",
 "Wireless wellbeing and personalised health",                                                                                     
 "Community Academic Partnerships (CAP)",
 "Smart Food Production Systems",
 "Global Food and Nutrition Security",                                                                                                       
 "Businesses, Markets and the Social Context of Health",                                                                                    
 "Work and health",                                                                                                                         
 "Health informatics and health analytics",                                                                                                 
 "Science of Learning Science",                                                                                                              
 "Schizophrenia: cardiometabolic and other medical comorbidity",                                                                             
 "Fibrosis and Wound Healing",                                                                                                               
 "Positive Computing in Health Systems",                                                                                                     
 "Regional Governance and Leadership",                             
 "Remote / Indigenous Communities, community led innovation",                                                                
 "Aligning Child and Adolescent Mental Health Services",                   
 "Health and Economics",                                                      
 "Population Analysis of Human Diet and Nutrition",                                                                                          
 "Human Food Chain",                                                                                                                         
 "Gut Microbiome",                                                                                                                           
 "Developmental Origin of Disease",                                                                                                         
 "Health Literacy Chronic Disease Network",                                                                                                  
 "Aboriginal Macronutrition",                                                                                                                
 "Translational Gerontology",                                                                                                                
 "PLANET Sydney Network",                                                                                                                   
 "Identification of microbiome control of weight loss",                                             
 "Dietary fats as drivers of obesity-related inflammation",                                                                                  
 "Translational Cardiac Imaging",                                                                                                            
 "Life Lab",                                                                                                                                 
 "Integrative Systems Lab",                                                                                                           
 "E-health in gaming and Avatars" 
    )



CPC.df <- data.frame()
for  (i in 1:nrow(CPC_data)) {
                               tmprow <- CPC_data[i,]
                               #tmprow <- CPCNodesNames[7,]
                               Leaders <- Isolate.Leaders(tmprow$Leader)
                               Patricipants <- Isolate.Participants(tmprow$Participants)
                               datf <- data.frame()
                                   if (!is.null(Leaders)) {datf <- Leaders}
                                   if (!is.null(Patricipants)) {datf <- rbind(datf, Patricipants)}
                                   if (nrow(datf) > 0 ){# datf <- cbind(datf, Project.Index = tmprow$Project.Index, Title =tmprow$New_Title, Status=tmprow$Status)
                                                         datf <- cbind(datf, select(tmprow, -Leader, -Participants, -Domain, -Theme))
                                                         CPC.df <- rbind(CPC.df, datf)
                                                         }
                                  }

tmpResearcher <- unlist(lapply(CPC.df$Researcher, gsub, pattern=" $", replacement="")) #remove trailing spaces

