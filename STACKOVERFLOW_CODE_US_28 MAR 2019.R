###### IMPORT COMMON DATAFILES ######

TV<- FT09_SMGFT_YTDDetailTV[,c(1:28,44)]
Radio<-FT09_SMGFT_YTDDetailRadio[,c(1:28,44)]
OnlineVideo <- FT09_SMGFT_YTDDetailOnlineVideo[,c(1:28,45)]
OnlineDisplay <- FT09_SMGFT_YTDDetailOnlineDisplay[,c(1:28,45)]
Print<-FT09_SMGFT_YTDDetailPrint[,c(1:28,44)]
Mobile<-FT09_SMGFT_YTDDetailMobile[,c(1:28,45)]

Combined_Data<- rbind(TV,Radio,OnlineVideo,OnlineDisplay,Print,Mobile)
Combined_Data$FY<-"FY20"

##Remove all the entries for June (Month 6 of Previous Year)
#Combined_Data<-subset(Combined_Data, MonthNum != '6')

##Media Week Zero Clean-up on the year data
##Subset the Media Week Zero entries
FY19_Media_Week_Zero<-subset(Combined_Data, Media.Week == '0')
FY19_Data_wo_MWZero<-subset(Combined_Data, Media.Week != '0')

##Reclassify all media week zeros into media week 52 along with the dependent variables
FY19_Media_Week_Zero$Quarter<-'Q4'
FY19_Media_Week_Zero$MonthNum<-12
FY19_Media_Week_Zero$Year<-2019
FY19_Media_Week_Zero$Media.Week<-52

##Append the Reclassified media weeks into the Prefinal without media week zeros dataset
Combined_Data<-rbind(FY19_Data_wo_MWZero,FY19_Media_Week_Zero)

##Reclassify Office 365 from Business to Consumer Entries
O365_records<-subset(Combined_Data, Product.Campaign == "Office 365" )
Records_wo_O365<-subset(Combined_Data, Product.Campaign != "Office 365" )
O365_records$BusVsCons<-"Consumer"
Combined_Data<-rbind(O365_records,Records_wo_O365)

Consumer_Entries<-subset(Combined_Data,BusVsCons == "Consumer")
GP_Consumer_Entries<-Consumer_Entries
Business_Entries<-subset(Combined_Data,BusVsCons == "Business" & Category != 'Home & Garden: Appliances'& Category != 'Home & Garden: Home Improvement')

##Executing the Business Path
##SUBSETTING ONLY THE SEVEN SHORTLISTED BRANDS & SPLITTING THE BUSINESS ENTRIES INTO FOCUS BRANDS AND OTHERS
Focus_Seven<-subset(Business_Entries, Advertiser == 'Microsoft' | Advertiser == 'Amazon'| Advertiser == 'Salesforce'| Advertiser == 'Google'| Advertiser == 'IBM'| Advertiser == 'Dropbox'| Advertiser == 'Slack Technologies')
###USING %IN% TO REMOVE ONE DATAFRAME FROM ANOTHER
BP_Others<-Business_Entries[!(Business_Entries$Advertiser %in% Focus_Seven$Advertiser),]

##DUPLICATIONS FOR MAIN BRANDS WITHIN FOCUS SEVEN DF
###CANNOT CLASSIFY BEFORE DUPLICATING
FS_Coop1<-subset(Focus_Seven,Co.op.Partner.1 == 'Microsoft' | Co.op.Partner.1 == 'Amazon'| Co.op.Partner.1 == 'Salesforce'| Co.op.Partner.1 == 'Google'| Co.op.Partner.1 == 'IBM'| Co.op.Partner.1 == 'Dropbox'| Co.op.Partner.1 == 'Slack Technologies' )
FS_Coop2<-subset(Focus_Seven,Co.op.Partnet.2 == 'Microsoft' | Co.op.Partnet.2 == 'Amazon'| Co.op.Partnet.2 == 'Salesforce'| Co.op.Partnet.2 == 'Google'| Co.op.Partnet.2 == 'IBM'| Co.op.Partnet.2 == 'Dropbox'| Co.op.Partnet.2 == 'Slack Technologies' )
FS_Coop3<-subset(Focus_Seven,Co.op.Partner.3 == 'Microsoft' | Co.op.Partner.3 == 'Amazon'| Co.op.Partner.3 == 'Salesforce'| Co.op.Partner.3 == 'Google'| Co.op.Partner.3 == 'IBM'| Co.op.Partner.3 == 'Dropbox'| Co.op.Partner.3 == 'Slack Technologies' )

FS_Coop1$Classification<-FS_Coop1$Co.op.Partner.1
FS_Coop1$Classification<-paste(FS_Coop1$Classification,'Copartner')

FS_Coop2$Classification<-FS_Coop2$Co.op.Partnet.2
FS_Coop2$Classification<-paste(FS_Coop2$Classification,'Copartner')

FS_Coop3$Classification<-FS_Coop3$Co.op.Partner.3
FS_Coop3$Classification<-paste(FS_Coop3$Classification,'Copartner')

FS_Coops<-rbind(FS_Coop1, FS_Coop2, FS_Coop3)

##CLASSIFICATION OWN
###MOVING ADVERTISER INTO CLASSIFICATION FOR OWN SPENDS
Focus_Seven$Classification<-Focus_Seven$Advertiser
###USING PASTE FUNCTION TO APPEND THE ADVERTISER WITH TERM 'OWN'
Focus_Seven$Classification<-paste(Focus_Seven$Classification,'Own')

##COPARTNER SPENDS CLASSIFICATION FROM OTHERS DF
BP_Coop1<-subset(BP_Others,Co.op.Partner.1 == 'Microsoft' | Co.op.Partner.1 == 'Amazon'| Co.op.Partner.1 == 'Salesforce'| Co.op.Partner.1 == 'Google'| Co.op.Partner.1 == 'IBM'| Co.op.Partner.1 == 'Dropbox'| Co.op.Partner.1 == 'Slack Technologies' )
BP_Coop2<-subset(BP_Others,Co.op.Partnet.2 == 'Microsoft' | Co.op.Partnet.2 == 'Amazon'| Co.op.Partnet.2 == 'Salesforce'| Co.op.Partnet.2 == 'Google'| Co.op.Partnet.2 == 'IBM'| Co.op.Partnet.2 == 'Dropbox'| Co.op.Partnet.2 == 'Slack Technologies' )
BP_Coop3<-subset(BP_Others,Co.op.Partner.3 == 'Microsoft' | Co.op.Partner.3 == 'Amazon'| Co.op.Partner.3 == 'Salesforce'| Co.op.Partner.3 == 'Google'| Co.op.Partner.3 == 'IBM'| Co.op.Partner.3 == 'Dropbox'| Co.op.Partner.3 == 'Slack Technologies' )

BP_Coop1$Classification<-BP_Coop1$Co.op.Partner.1
BP_Coop1$Classification<-paste(BP_Coop1$Classification,'Copartner')

BP_Coop2$Classification<-BP_Coop2$Co.op.Partnet.2
BP_Coop2$Classification<-paste(BP_Coop2$Classification,'Copartner')

BP_Coop3$Classification<-BP_Coop3$Co.op.Partner.3
BP_Coop3$Classification<-paste(BP_Coop3$Classification,'Copartner')

BP_Coops<-rbind(BP_Coop1, BP_Coop2, BP_Coop3)

##REMOVING COOPS FROM OTHERS IN A THREE STEP BASIS
BP_O1<-BP_Others[!(BP_Others$Co.op.Partner.1 %in% BP_Coop1$Co.op.Partner.1),]
BP_O2<-BP_O1[!(BP_O1$Co.op.Partnet.2 %in% BP_Coop2$Co.op.Partnet.2),]
BP_O3<-BP_O2[!(BP_O2$Co.op.Partner.3 %in% BP_Coop3$Co.op.Partner.3),]
BP_O3$Classification<-'Third Party / Others'

##DEFINING VARIABLE OCT
Focus_Seven$OCT<-'Own'
FS_Coops$OCT<-'Copartner'
BP_Coops$OCT<-'Copartner'
BP_O3$OCT<-'Third Party / Others'

##BINDING OWN, COPARTNERS AND OTHERS 
Business_Path<-rbind(Focus_Seven, FS_Coops, BP_Coops, BP_O3)

##UP CLASSIFICATION USING TEXT SUB() REMOVING THE WORDS AFTER THE FIRST WORD
Business_Path$Up_Classification <- sub(' .*$', '', Business_Path$Classification, perl=TRUE)

##Defining Business Categories in Business Path
Business_Path$Business_Categories[Business_Path$Category == 'Network Attached Storage']<-'Peripherals / Accessories'
Business_Path$Business_Categories[Business_Path$Category == 'Network Operating Systems']<-'OS'
Business_Path$Business_Categories[Business_Path$Category == 'Networked Systems']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Notebook - Intel/AMD']<-'PC'
Business_Path$Business_Categories[Business_Path$Category == 'On-line Services']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Notebook - Other Non Intel/AMD']<-'PC'
Business_Path$Business_Categories[Business_Path$Category == 'Online Shopping']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Operating Systems']<-'OS'
Business_Path$Business_Categories[Business_Path$Category == 'Other']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Other Digital Cameras Systems']<-'Peripherals / Accessories'
Business_Path$Business_Categories[Business_Path$Category == 'Other Internet Related']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Other Networking Equipment']<-'Peripherals / Accessories'
Business_Path$Business_Categories[Business_Path$Category == 'Other Services']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Other Software']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Other Storage Devices']<-'Peripherals / Accessories'
Business_Path$Business_Categories[Business_Path$Category == 'Other Systems']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Project Management']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Recruitment']<-'Business Consulting'
Business_Path$Business_Categories[Business_Path$Category == 'Search Engine/Portal']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Security Services']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Security Software & Services']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Servers - High End']<-'Peripherals / Accessories'
Business_Path$Business_Categories[Business_Path$Category == 'Smartphone']<-'Phone'
Business_Path$Business_Categories[Business_Path$Category == 'Social Networking Sites']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Software Advertisers Image']<-'Image'
Business_Path$Business_Categories[Business_Path$Category == 'Software Suite']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Software Suite - Business']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Sponsorship']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Sweepstakes & Contests']<-'Others'
Business_Path$Business_Categories[Business_Path$Category == 'Switch']<-'Peripherals / Accessories'
Business_Path$Business_Categories[Business_Path$Category == 'Tablet PC']<-'Tablet'
Business_Path$Business_Categories[Business_Path$Category == 'Training/Business Opportunities']<-'Business Consulting'
Business_Path$Business_Categories[Business_Path$Category == 'Ultrabook']<-'PC'
Business_Path$Business_Categories[Business_Path$Category == 'Videoconferencing Pe']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Voice- Business']<-'Business Apps & Services'
Business_Path$Business_Categories[Business_Path$Category == 'Web Hosting/Development']<-'Business Apps & Services'
Business_Path$Business_Categories[is.na(Business_Path$Business_Categories)]<-"Others"

Business_Path$Product_Series<-Business_Path$Product.Campaign

#Rename 'Third' to 'Third Party / Others' at Up-Classification due to truncation which has happened after using sub function
BP_Third<-subset(Business_Path, Up_Classification == "Third")
BP_Non_Third<-subset(Business_Path, Up_Classification != "Third")
BP_Third$Up_Classification<-'Third Party / Others'
Business_Path<-rbind(BP_Non_Third, BP_Third)
Business_Path$Product_Series<-Business_Path$Product.Campaign
Business_Path$Path<-'Business'

#
##Executing the Gaming Path
###Creating a new column called classification as the 31st column with NA entries###
GP_Consumer_Entries$Classification<-NA

##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
GP_Playstation_Rows1<-grep("Playstation",GP_Consumer_Entries$Product.Campaign)
GP_Playstation_Rows2<-grep("PlayStation",GP_Consumer_Entries$Product.Campaign)
GP_Playstation_Rows3<-grep("PS4",GP_Consumer_Entries$Product.Campaign)
GP_Playstation_Records<-GP_Consumer_Entries[c(GP_Playstation_Rows1,GP_Playstation_Rows2,GP_Playstation_Rows3),]
GP_Playstation_Records$Classification<-'Sony Console Related'

###Defining Dataset without Playstation_Records##
##Provision for any other exclusive entries in future##
GP_Data1<-GP_Consumer_Entries[-c(GP_Playstation_Rows1,GP_Playstation_Rows2),]

###Defining the Gaming/Software Games/Toys & Games subset###
GP_Data2<-subset(GP_Data1,c(Category == 'Gaming' | Category == 'Software-Games'|Category == 'Toys & Games'))

##Pulling all the XBox Live/XBox Live Gold entries from Gaming$OnlineGames separately and classifying as Console spends ###
GP_XBox_Live_Gold_Rows<-grep("Xbox Live Gold",GP_Data2$Product.Campaign)
GP_XBox_Live_Rows<-grep("Xbox Live",GP_Data2$Product.Campaign)
GP_XBox_Live_Records<-GP_Data2[c(GP_XBox_Live_Gold_Rows,GP_XBox_Live_Rows),]
GP_XBox_Live_Records$Classification<-'Microsoft Console Related'

##Pulling all the XBox Live/XBox Live Gold entries from Gaming$OnlineGames separately and classifying as Console spends ###
GP_Handheld_Rows1<-grep("Game Player - Handheld", GP_Data2$Subcategory)
GP_Handheld_Rows2<-grep("Handheld Game Cartridges", GP_Data2$Subcategory)
GP_Handheld_Records<-GP_Data2[c(GP_Handheld_Rows1,GP_Handheld_Rows2),]

##Data without the XboxLive and Handheld records
GP_Data<-GP_Data2[-c(GP_XBox_Live_Gold_Rows,GP_XBox_Live_Rows, GP_Handheld_Rows1,GP_Handheld_Rows2),]

###Segregating Advertisers into MSN & Others
##MSN (Microsoft/Sony/Nintendo) Advertisers
GP_Microsoft_Adv_row <-grep('Microsoft',GP_Data$Advertiser)
GP_Microsoft_adv <- GP_Data[GP_Microsoft_Adv_row,]
GP_Sony_Adv_row <-grep('Sony', GP_Data$Advertiser)
GP_Sony_adv <- GP_Data[GP_Sony_Adv_row,]
GP_Nintendo_Adv_row <-grep('Nintendo',GP_Data$Advertiser)
GP_Nintendo_adv <- GP_Data[GP_Nintendo_Adv_row,]

##Other Advertisers
GP_Other_Advertisers <- GP_Data[-c(GP_Microsoft_Adv_row,GP_Sony_Adv_row,GP_Nintendo_Adv_row),]
GP_Other_Advertisers<-GP_Other_Advertisers[,1:30]

##Classifications using MSN Advertisers
###Consoles Related Classification
GP_Microsoft_Console_row <- grep('Video Game Console/Accessories',GP_Microsoft_adv$Subcategory)
GP_Microsoft_adv$Classification[GP_Microsoft_Console_row]<-'Microsoft Console Related'
GP_Nintendo_Console_row <- grep('Video Game Console/Accessories',GP_Nintendo_adv$Subcategory)
GP_Nintendo_adv$Classification[GP_Nintendo_Console_row]<-'Nintendo Console Related'
GP_Sony_Console_row <- grep('Video Game Console/Accessories',GP_Sony_adv$Subcategory)
GP_Sony_adv$Classification[GP_Sony_Console_row]<-'Sony Console Related'

###Games Classification
###MSN Advertisers are the classified own spends for all the three brands now
GP_Microsoft_adv$Classification[GP_Microsoft_adv$Subcategory !='Video Game Console/Accessories']<-"Microsoft Own Games"
GP_Sony_adv$Classification[GP_Sony_adv$Subcategory !='Video Game Console/Accessories']<-"Sony Own Games"
GP_Nintendo_adv$Classification[GP_Nintendo_adv$Subcategory !='Video Game Console/Accessories']<-"Nintendo Own Games"
GP_MSN_Advertisers<-rbind(GP_Microsoft_adv,GP_Nintendo_adv,GP_Sony_adv)

##CoPartner and Third Party Classification
##Microsoft Copartner
GP_M1 <- grepl('Microsoft',GP_Other_Advertisers$Co.op.Partner.1)
GP_M2 <- grepl('Microsoft',GP_Other_Advertisers$Co.op.Partnet.2)
GP_M3 <- grepl('Microsoft',GP_Other_Advertisers$Co.op.Partner.3)
##Sony Copartner
GP_S1 <- grepl('Sony',GP_Other_Advertisers$Co.op.Partner.1)
GP_S2 <- grepl('Sony',GP_Other_Advertisers$Co.op.Partnet.2)
GP_S3 <- grepl('Sony',GP_Other_Advertisers$Co.op.Partner.3)
##Nintendo Copartner
GP_N1 <- grepl('Nintendo',GP_Other_Advertisers$Co.op.Partner.1)
GP_N2 <- grepl('Nintendo',GP_Other_Advertisers$Co.op.Partnet.2)
GP_N3 <- grepl('Nintendo',GP_Other_Advertisers$Co.op.Partner.3)
GP_B1 <- is.na(GP_Other_Advertisers$Co.op.Partner.1)
GP_B2 <- is.na(GP_Other_Advertisers$Co.op.Partnet.2)
GP_B3 <- is.na(GP_Other_Advertisers$Co.op.Partner.3)
##Copartner and Third Party Classifications
### using nested if-else
Classification <- ifelse(((GP_M1 == T|GP_M2 == T|GP_M3 == T)&(GP_S1==F & GP_S2==F & GP_S3== F & GP_N1== F & GP_N2== F & GP_N3 == F)),"Microsoft Games Copartner",
                         ifelse(((GP_S1 == T|GP_S2 == T|GP_S3 == T)&(GP_M1== F & GP_M2== F & GP_M3==F & GP_N1== F & GP_N2== F & GP_N3 == F)),"Sony Games Copartner",
                                ifelse(((GP_N1 == T|GP_N2 == T|GP_N3 == T)&(GP_M1== F & GP_M2== F & GP_M3==F & GP_S1== F & GP_S2== F & GP_S3 == F)),"Nintendo Games Copartner","Third Party / Others")
                         )
)
GP_Copartners_TP <- cbind(GP_Other_Advertisers,Classification)
GP_MSN_Advertisers$OCT<-'Own'
GP_Playstation_Records$OCT<-'Own'
GP_XBox_Live_Records$OCT<-'Own'
GP_Copartners<-subset(GP_Copartners_TP, Classification != 'Third Party / Others')
GP_ThirdParty<-subset(GP_Copartners_TP, Classification == 'Third Party / Others')
GP_Copartners$OCT<-'Copartner'
GP_ThirdParty$OCT<-'Third Party / Others'

##Combining all the classified data and the PlayStation Records##
GP_Prefinal_Data<-rbind(GP_MSN_Advertisers, GP_Copartners, GP_ThirdParty, GP_Playstation_Records, GP_XBox_Live_Records)

##Broader Classification bucketing by brand##
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Microsoft Console Related']<-'Microsoft'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Microsoft Own Games']<-'Microsoft'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Microsoft Games Copartner']<-'Microsoft'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Sony Console Related']<-'Sony'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Sony Own Games']<-'Sony'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Sony Games Copartner']<-'Sony'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Nintendo Console Related']<-'Nintendo'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Nintendo Own Games']<-'Nintendo'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Nintendo Games Copartner']<-'Nintendo'
GP_Prefinal_Data$Up_Classification[GP_Prefinal_Data$Classification == 'Third Party / Others']<-'Third Party / Others'

GP_Prefinal_Data$Business_Categories<-'Games'

GP_Prefinal_Data$Product_Series[GP_Prefinal_Data$Up_Classification == 'Microsoft']<-'Xbox'
GP_Prefinal_Data$Product_Series[GP_Prefinal_Data$Up_Classification == 'Sony']<-'PlayStation'
GP_Prefinal_Data$Product_Series[GP_Prefinal_Data$Up_Classification == 'Nintendo']<-'Wii / Switch'
GP_Prefinal_Data$Product_Series[GP_Prefinal_Data$Up_Classification == 'Third Party / Others']<-'Third Party Games'

#Renaming 'Gaming' category as 'Services and Accessories' (suggestion from Mike on 25th June 2019)
GP_CatGaming<-subset(GP_Prefinal_Data, Category == 'Gaming')
GP_CatOthers<-subset(GP_Prefinal_Data, Category != 'Gaming')
GP_CatGaming$Category<-'Services and Accessories'
GP_Prefinal_Data<-rbind(GP_CatGaming, GP_CatOthers)
GP_Prefinal_Data$Path<-'Gaming'

#Bringing the Services into Classification (26th Sep 2019 per meeting with Mike in Redmond)
GP_Prefinal_Data$Classification[GP_Prefinal_Data$Product.Campaign == 'XBOX Game Pass']<-'Microsoft Services'
GP_Prefinal_Data$Classification[GP_Prefinal_Data$Product.Campaign == 'XBOX Game Pass Ultimate']<-'Microsoft Services'
GP_Prefinal_Data$Classification[GP_Prefinal_Data$Product.Campaign == 'Playstation Now']<-'Sony Services'
GP_Prefinal_Data$Classification[GP_Prefinal_Data$Product.Campaign == 'PlayStation Vue']<-'Sony Services'
GP_Prefinal_Data$Classification[GP_Prefinal_Data$Product.Campaign == 'PlayStation Plus']<-'Sony Services'
GP_Prefinal_Data$Classification[GP_Prefinal_Data$Product.Campaign == 'PlayStation Plus online gaming']<-'Sony Services'

#Google Stadia Entries (Remove from TP and change Classi and Up Classi to Google)
GP_Goog<-subset(GP_Prefinal_Data, Advertiser == 'Google')
GP_Not_Goog<-GP_Prefinal_Data[!GP_Prefinal_Data$Advertiser %in% GP_Goog$Advertiser,]
GP_Goog$Classification<-'Google'
GP_Goog$Up_Classification<-'Google'
GP_Prefinal_Data<-rbind(GP_Goog, GP_Not_Goog)

Gaming_Path<-GP_Prefinal_Data
#

##Executing the OCC Path
##Pulling all the Playstation entries initially itself without any category / subcategory specifics or limits###
##Deleting the Playstation mention records from the larger dataset itself##
Playstation_Rows1<-grep("Playstation",Consumer_Entries$Product.Campaign)
Playstation_Rows2<-grep("PlayStation",Consumer_Entries$Product.Campaign)
Playstation_Records<-Consumer_Entries[c(Playstation_Rows1,Playstation_Rows2),]

###Defining Dataset without Playstation_Records##
Data1<-Consumer_Entries[-c(Playstation_Rows1,Playstation_Rows2),]

###Reclassify all Surface entries at Subcategory level from Tablet PC to Desktops & Notebooks###
Surface_Rows_Reclassification<-grep('Surface', Data1$Product.Campaign)
Data1$Subcategory[Surface_Rows_Reclassification]<-'Desktops & Laptops'

###Reclassify the selected records entries of Amazon Records which are all under Retail Category ###
Amazon_records<-subset(Data1,Advertiser == "Amazon")
Data1_minus_Amazon_records<-subset(Data1, Advertiser !="Amazon")
Amazon_Echo_Reclassification<-grep('Echo',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Echo_Reclassification]<-'Consumer Electronics'
Amazon_records$Subcategory[Amazon_Echo_Reclassification]<-'Other Consumer Electronics'
Amazon_Alexa_Reclassification<-grep('Amazon Alexa',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Alexa_Reclassification]<-'Software'
Amazon_records$Subcategory[Amazon_Alexa_Reclassification]<-'Cloud/Virtualization Services'
Amazon_Coin_Reclassification<-grep('Amazon Coin',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Coin_Reclassification]<-'Credit Cards'
Amazon_records$Subcategory[Amazon_Coin_Reclassification]<-'Digital Payment Systems'
Amazon_Appstore_Reclassification<-grep('Amazon Appstore',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Appstore_Reclassification]<-'Software'
Amazon_records$Subcategory[Amazon_Coin_Reclassification]<-'Multiple Software'
Amazon_Dash_Reclassification<-grep('Amazon Dash',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Dash_Reclassification]<-'Consumer Electronics'
Amazon_records$Subcategory[Amazon_Dash_Reclassification]<-'Other Consumer Electronics'
Amazon_dynamodb_Reclassification<-grep('Amazon DynamoDB',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_dynamodb_Reclassification]<-'Software'
Amazon_records$Subcategory[Amazon_dynamodb_Reclassification]<-'Cloud/Virtualization Services'
Amazon_Fire_Tablet_Reclassification<-grep('Amazon Fire Tablet',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Fire_Tablet_Reclassification]<-'Computers'
Amazon_records$Subcategory[Amazon_Fire_Tablet_Reclassification]<-'Tablet PC'
Amazon_Music_Reclassification<-grep('Amazon Music Unlimited',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Music_Reclassification]<-'Internet Content Providers'
Amazon_records$Subcategory[Amazon_Music_Reclassification]<-'Music Download'
Amazon_Payments_Reclassification<-grep('Amazon Payments',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Payments_Reclassification]<-'Credit Cards'
Amazon_records$Subcategory[Amazon_Payments_Reclassification]<-'Digital Payment Systems'
Amazon_Cellphones_Reclassification<-grep('Cell Phones',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_Cellphones_Reclassification]<-'Wireless Communications'
Amazon_records$Subcategory[Amazon_Cellphones_Reclassification]<-'Smartphone'
Amazon_TV_Reclassification<-grep('TV',Amazon_records$Product.Campaign)
Amazon_records$Category[Amazon_TV_Reclassification]<-'Consumer Electronics'
Amazon_records$Subcategory[Amazon_TV_Reclassification]<-'Multiple TV Types'

##levels(factor(Amazon_records$Subcategory))
##Bringing back the Amazon Records and Data1_minus_Amazon_Records back to Data1##
Data1<-rbind(Amazon_records, Data1_minus_Amazon_records)

#SUbsetting only the required categories
Category_InternetRelatedServices<-subset(Data1,Category == "Internet Related Services")
Category_Associations<-subset(Data1,Category == "Associations")
Category_Computers<-subset(Data1,Category == "Computers")
Category_CreditCards<-subset(Data1,Category == "Credit Cards")
Category_GeneralComputer<-subset(Data1,Category == "General Computer")
Category_InternetContentProviders<-subset(Data1,Category == "Internet Content Providers")
Category_Software<-subset(Data1,Category == "Software")
Category_SoftwareOperatingSystems<-subset(Data1,Category == "Software-Operating Systems")
Category_SubscriptionVideoServices<-subset(Data1,Category == "Subscription Video Services")
Category_DepartmentStores<-subset(Data1,Category == "Department Stores")
Category_PeripheralsOther<-subset(Data1,Category == "Peripherals (Other)")
Category_ComputerRelatedServices<-subset(Data1,Category == "Computer Related Services")
Category_ConsumerElectronics<-subset(Data1,Category == "Consumer Electronics")
Category_Image<-subset(Data1,Category == "Image")
Category_WirelessCommunications<-subset(Data1,Category == "Wireless Communications")
Category_Retail<-subset(Data1,Category == "Retail")
Category_Education<-subset(Data1,Category == "Education")

#rbinding all the required categories into one dataframe
Data<-rbind(Category_InternetRelatedServices,Category_Associations,Category_Computers,Category_CreditCards,Category_GeneralComputer,Category_InternetContentProviders,Category_Software,Category_SoftwareOperatingSystems,Category_SubscriptionVideoServices,Category_DepartmentStores,Category_PeripheralsOther,Category_ComputerRelatedServices,Category_ConsumerElectronics,Category_Image,Category_WirelessCommunications,Category_Retail,Category_Education)

##Classification Starts
##Segregating Advertisers into MAGSA & Others
##MAGSA (Microsoft/Apple/Google/Samsung & Amazon) Advertisers
Microsoft_Adv_row <-grep('Microsoft',Data$Advertiser)
Microsoft_adv <- Data[Microsoft_Adv_row,]
Apple_Adv_row <-grep('Apple',Data$Advertiser)
Apple_adv <- Data[Apple_Adv_row,]
Google_Adv_row <-grep('Google',Data$Advertiser)
Google_adv <- Data[Google_Adv_row,]
Samsung_Adv_row <-grep('Samsung',Data$Advertiser)
Samsung_adv <- Data[Samsung_Adv_row,]
Amazon_Adv_row <-grep('Amazon',Data$Advertiser)
Amazon_adv <- Data[Amazon_Adv_row,]

##Creating a new column Classification and moving all the own spends
Microsoft_Own_Spends<-cbind(Data[Microsoft_Adv_row,],'Microsoft Own')
colnames(Microsoft_Own_Spends)[31] <- 'Classification'
Apple_Own_Spends<-cbind(Data[Apple_Adv_row,],'Apple Own')
colnames(Apple_Own_Spends)[31] <- 'Classification'
Google_Own_Spends<-cbind(Data[Google_Adv_row,],'Google Own')
colnames(Google_Own_Spends)[31] <- 'Classification'
Samsung_Own_Spends<-cbind(Data[Samsung_Adv_row,],'Samsung Own')
colnames(Samsung_Own_Spends)[31] <- 'Classification'
Amazon_Own_Spends<-cbind(Data[Amazon_Adv_row,],'Amazon Own')
colnames(Amazon_Own_Spends)[31] <- 'Classification'

##Combining all the own spends
Own_Spends<-rbind(Microsoft_Own_Spends,Apple_Own_Spends,Google_Own_Spends,Samsung_Own_Spends,Amazon_Own_Spends)
Own_Spends$OCT<-'Own'

##Own Spends
MAGSA_Advertisers<-Data[c(Microsoft_Adv_row,Apple_Adv_row,Google_Adv_row,Samsung_Adv_row,Amazon_Adv_row),]

#Other Advertisers
Other_Advertisers<-Data[-c(Microsoft_Adv_row,Apple_Adv_row,Google_Adv_row,Samsung_Adv_row,Amazon_Adv_row),]

#CoPartners and Third Party Classification#
#CoPartners#
##Microsoft Copartner
M1 <- grepl('Microsoft',Other_Advertisers$Co.op.Partner.1)
M2 <- grepl('Microsoft',Other_Advertisers$Co.op.Partnet.2)
M3 <- grepl('Microsoft',Other_Advertisers$Co.op.Partner.3)
##Apple Copartner
A1 <- grepl('Apple',Other_Advertisers$Co.op.Partner.1)
A2 <- grepl('Apple',Other_Advertisers$Co.op.Partnet.2)
A3 <- grepl('Apple',Other_Advertisers$Co.op.Partner.3)
##Google Copartner
G1 <- grepl('Google',Other_Advertisers$Co.op.Partner.1)
G2 <- grepl('Google',Other_Advertisers$Co.op.Partnet.2)
G3 <- grepl('Google',Other_Advertisers$Co.op.Partner.3)
##Samsung Copartner
S1 <- grepl('Samsung',Other_Advertisers$Co.op.Partner.1)
S2 <- grepl('Samsung',Other_Advertisers$Co.op.Partnet.2)
S3 <- grepl('Samsung',Other_Advertisers$Co.op.Partner.3)
##Amazon Copartner
Amz1 <- grepl('Amazon',Other_Advertisers$Co.op.Partner.1)
Amz2 <- grepl('Amazon',Other_Advertisers$Co.op.Partnet.2)
Amz3 <- grepl('Amazon',Other_Advertisers$Co.op.Partner.3)
#Classification#
Classification<-ifelse(((((M1 == T|M2 == T|M3 == T)&(A1==F & A2==F & A3== F & G1== F & G2== F & G3 == F & S1 == F & S2 == F & S3 == F  & Amz1 == F & Amz2 == F & Amz3 == F )))),"Microsoft Copartner",
                       ifelse(((((A1 == T|A2 == T|A3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & S1 == F & S2 == F & S3 == F & Amz1 == F & Amz2 == F & Amz3 == F)))),"Apple Copartner",
                              ifelse(((((S1 == T|S2 == T|S3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & A1 == F & A2 == F & A3 == F & Amz1 == F & Amz2 == F & Amz3 == F)))),"Samsung Copartner",
                                     ifelse(((((Amz1 == T|Amz2 == T|Amz3 == T)&(M1==F & M2==F & M3== F & G1== F & G2== F & G3 == F & A1 == F & A2 == F & A3 == F & S1 == F & S2 == F & S3 == F)))),"Amazon Copartner",
                                            ifelse(((((G1 == T|G2 == T|G3 == T)&(M1==F & M2==F & M3== F & A1== F & A2== F & A3 == F & S1 == F & S2 == F & S3 == F& Amz1 == F & Amz2 == F & Amz3 == F )))),"Google Copartner","Third Party / Others")
                                     ))))
Copartners_ThirdParty <- cbind(Other_Advertisers,Classification)

##Split CoPartners and ThirdParty for Simplicity Sake
ThirdParty_Spends<-subset(Copartners_ThirdParty,Classification == 'Third Party / Others')
CoPartner_Spends<-subset(Copartners_ThirdParty,Classification != 'Third Party / Others')
CoPartner_Spends$OCT<-'Copartner'

####Duplicating the two main brand entry spends in ThirdParty
####(i.e. eg. ATT has spend for $30k and has co-partner with Apple & Samsung, it should count as $30k for Apple C0-Partner, and $30k for Samsung Co-partner)
###By CoPartner 1,2,3 separately###
Apple_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Apple Inc.')
Apple_Coop1$Classification<-'Apple Copartner'
Samsung_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Samsung')
Samsung_Coop1$Classification<-'Samsung Copartner'
Microsoft_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Microsoft')
Microsoft_Coop1$Classification<-'Microsoft Copartner'
Google_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Google')
Google_Coop1$Classification<-'Google Copartner'
Amazon_Coop1<-subset(ThirdParty_Spends, Co.op.Partner.1 == 'Amazon')
Amazon_Coop1$Classification<-'Amazon Copartner'
Dup_Coop1_Records<-rbind(Apple_Coop1,Samsung_Coop1,Microsoft_Coop1,Google_Coop1,Amazon_Coop1)
Apple_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Apple Inc.')
Apple_Coop2$Classification<-'Apple Copartner'
Samsung_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Samsung')
Samsung_Coop2$Classification<-'Samsung Copartner'
Microsoft_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Microsoft')
Microsoft_Coop2$Classification<-'Microsoft Copartner'
Google_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Google')
Google_Coop2$Classification<-'Google Copartner'
Amazon_Coop2<-subset(ThirdParty_Spends, Co.op.Partnet.2 == 'Amazon')
Amazon_Coop2$Classification<-'Amazon Copartner'
Dup_Coop2_Records<-rbind(Apple_Coop2,Samsung_Coop2,Microsoft_Coop2,Google_Coop2,Amazon_Coop2)
Apple_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Apple Inc.')
Apple_Coop3$Classification<-'Apple Copartner'
Samsung_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Samsung')
Samsung_Coop3$Classification<-'Samsung Copartner'
Microsoft_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Microsoft')
Microsoft_Coop3$Classification<-'Microsoft Copartner'
Google_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Google')
Google_Coop3$Classification<-'Google Copartner'
Amazon_Coop3<-subset(ThirdParty_Spends, Co.op.Partner.3 == 'Amazon')
Amazon_Coop3$Classification<-'Amazon Copartner'
Dup_Coop3_Records<-rbind(Apple_Coop3,Samsung_Coop3,Microsoft_Coop3,Google_Coop3,Amazon_Coop3)

##All duplications classified and combined##
Dup_3levels_Records<-rbind(Dup_Coop1_Records,Dup_Coop2_Records, Dup_Coop3_Records)
Dup_3levels_Records$OCT<-'Copartner'

###Remove all the Dup records without redundancy from the Third Party entries###
Apple_rows1_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partner.1)
Microsoft_rows1_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partner.1)
Samsung_rows1_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partner.1)
Google_rows1_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partner.1)
Amazon_rows1_Remove<-grep('Amazon',ThirdParty_Spends$Co.op.Partner.1)
Apple_rows2_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partnet.2)
Microsoft_rows2_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partnet.2)
Samsung_rows2_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partnet.2)
Google_rows2_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partnet.2)
Amazon_rows2_Remove<-grep('Amazon',ThirdParty_Spends$Co.op.Partnet.2)
Apple_rows3_Remove<-grep('Apple Inc.',ThirdParty_Spends$Co.op.Partner.3)
Microsoft_rows3_Remove<-grep('Microsoft',ThirdParty_Spends$Co.op.Partner.3)
Samsung_rows3_Remove<-grep('Samsung',ThirdParty_Spends$Co.op.Partner.3)
Google_rows3_Remove<-grep('Google',ThirdParty_Spends$Co.op.Partner.3)
Amazon_rows3_Remove<-grep('Amazon',ThirdParty_Spends$Co.op.Partner.3)
Coops_To_Remove<-c(Apple_rows1_Remove,Microsoft_rows1_Remove,Samsung_rows1_Remove,Google_rows1_Remove,Apple_rows2_Remove,Microsoft_rows2_Remove,Samsung_rows2_Remove,Google_rows2_Remove,Apple_rows3_Remove,Microsoft_rows3_Remove,Samsung_rows3_Remove,Google_rows3_Remove,Amazon_rows3_Remove,Amazon_rows2_Remove,Amazon_rows1_Remove)
ThirdParty_Pure_Spends<-ThirdParty_Spends[-Coops_To_Remove,]
ThirdParty_Pure_Spends$OCT<-'Third Party / Others'
Prefinal_Data<-rbind(Own_Spends,CoPartner_Spends, Dup_3levels_Records,ThirdParty_Pure_Spends)

##PC/Tab/Phone Column creation using the subcategory entries
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Smartphone"]<-'Phone'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Tablet PC"]<-'Tablet'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Notebook - Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Ultrabook"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Notebooks"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Notebooks & Servers"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Notebook - Other Non Intel/AMD"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Macintosh"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktops & Laptops"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Desktop - Other (Non Intel/AMD)"]<-'PC'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Operating Systems"]<-'OS'

##These below two on category level should be just before wearables
Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Peripherals (Other)"]<-'Peripherals / Accessories'
Prefinal_Data$Business_Categories[Prefinal_Data$Category == "Consumer Electronics"]<-'Consumer Electronics'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Electronic & Appliance Stores"]<-'Consumer Electronics'

###
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Wearable Electronics"]<-'Wearables'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Internet Broadcast/Streaming Media"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Education/Training Software"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Search Engine/Portal"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "On-line Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "E-Mail Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Online Shopping"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Music Download"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Automobile Content Sites"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Mobile Apps"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Software Suite"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Document Management"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Other Software"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Multiple Software"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Cloud/Virtualization Services"]<-'Cloud'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Other Services"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Digital Payment Systems"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "IPTV Service"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Computer Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "High Speed Internet"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Social Networking Sites"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Online Entertainment"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Ringtones and Other Downloads"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Digital Media Downloads"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Music & Video Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Sporting Good Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Supermarkets"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Toy Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Bookstores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Shoe Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Specialty Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Clothing Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Home Furnishings"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Pet Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Bedding & Bath Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Auto Parts Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Office Supply Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Department Stores"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Wireless Phone Accessories"]<-'Peripherals / Accessories'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Internet Related Services Image"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Food & Beverage"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[Prefinal_Data$Subcategory == "Appliances"]<-'Consumer Apps & Services'
Prefinal_Data$Business_Categories[is.na(Prefinal_Data$Business_Categories)]<-"Others"

##Bucketing the form factor series for SP and Tablets (Galaxy Series, Lumia Series, IPhone series etc.)
##Create a new column called Product_Series with 'NA' in it
Prefinal_Data$Product_Series<-NA

###~PC
Surface_Series_Rows<-grep("Surface",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Surface_Series_Rows]<-'Surface Series'
Alienware_Series_Rows<-grep("Alienware",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Alienware_Series_Rows]<-'Dell Alienware Series'
Aspire_Series_Rows<-grep("Aspire",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Aspire_Series_Rows]<-'Acer Aspire Series'
Chromebook_Series_Rows<-grep("Chromebook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Chromebook_Series_Rows]<-'Google Chromebook Series'
Chromebox_Series_Rows<-grep("Chromebox",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Chromebox_Series_Rows]<-'Google Chromebook Series'
Elite_Series_Rows<-grep("Elite",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Elite_Series_Rows]<-'HP Elite Series'
Pavilion_Series_Rows<-grep("Pavilion",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Pavilion_Series_Rows]<-'HP Pavilion Series'
Spectre_Series_Rows<-grep("Spectre",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Spectre_Series_Rows]<-'HP Spectre Series'
Ideapad_Series_Rows<-grep("Ideapad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Ideapad_Series_Rows]<-'Lenovo Ideapad Series'
IdeaPad_Series_Rows<-grep("IdeaPad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[IdeaPad_Series_Rows]<-'Lenovo Ideapad Series'
Inspiron_Series_Rows<-grep("Inspiron",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Inspiron_Series_Rows]<-'Dell Inspiron Series'
Latitude_Series_Rows<-grep("Latitude",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Latitude_Series_Rows]<-'Dell Latitude Series'
MacBook_Series_Rows<-grep("Mac",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[MacBook_Series_Rows]<-'Apple MacBook Series'
Precision_Series_Rows<-grep("Precision",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Precision_Series_Rows]<-'Dell Precision Series'
Predator_Series_Rows<-grep("Predator",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Predator_Series_Rows]<-'Acer Predator Series'
Thinkpad_Series_Rows<-grep("Thinkpad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Thinkpad_Series_Rows]<-'Lenovo Thinkpad Series'
ZenBook_Series_Rows<-grep("ZenBook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[ZenBook_Series_Rows]<-'Asus ZenBook Series'
Zenbook_Series_Rows<-grep("Zenbook",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Zenbook_Series_Rows]<-'Asus ZenBook Series'
XPS_Series_Rows<-grep("XPS",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[XPS_Series_Rows]<-'Dell XPS Series'
Envy_Series_Rows<-grep("Envy",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Envy_Series_Rows]<-'HP Envy Series'

##~Tablet
iPad_Series_Rows<-grep("iPad",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[iPad_Series_Rows]<-'iPad Series'
Galaxy_Series_Rows<-grep("Galaxy",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Galaxy_Series_Rows]<-'Galaxy Series'
Fire_Series_Rows<-grep("Fire",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Fire_Series_Rows]<-'Fire Series'
Pixel_Series_Rows<-grep("Pixel",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Pixel_Series_Rows]<-'Pixel Series'
Yoga_Series_Rows<-grep("Yoga",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Yoga_Series_Rows]<-'Yoga Series'

##~Phone
iPhone_Series_Rows<-grep("iPhone",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[iPhone_Series_Rows]<-'iPhone Series'
Iphone_Series_Rows<-grep("Iphone",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Iphone_Series_Rows]<-'iPhone Series'
Lumia_Series_Rows<-grep("Lumia",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Lumia_Series_Rows]<-'Lumia Series'
Nexus_Series_Rows<-grep("Nexus",Prefinal_Data$Product.Campaign)
Prefinal_Data$Product_Series[Nexus_Series_Rows]<-'Nexus Series'
Prefinal_Data$Product_Series[is.na(Prefinal_Data$Product_Series)]<-'Others'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Own']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Microsoft Copartner']<-'Microsoft'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Apple Own']<-'Apple'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Apple Copartner']<-'Apple'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Google Own']<-'Google'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Google Copartner']<-'Google'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Own']<-'Amazon'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Amazon Copartner']<-'Amazon'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Samsung Own']<-'Samsung'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Samsung Copartner']<-'Samsung'
Prefinal_Data$Up_Classification[Prefinal_Data$Classification == 'Third Party / Others']<-'Third Party / Others'

#Prefinal_Data$Up_Classification[is.na(Prefinal_Data$Up_Classification)]<-'Others'
OCC_Path<-Prefinal_Data
OCC_Path$Path<-'Consumer'

##Combining all paths
OCC_Path<-OCC_Path[,c(1:32,35,33:34,36)]
Final_Data<-rbind(Business_Path,Gaming_Path,OCC_Path)
Final_Data<-Final_Data[,c(1:31,33,32,34:36)]

##Adding Date column using the Media Week and Year Column variables with monday as start day of the week
Final_Data$Date_WeekStarting<-as.Date(paste("1", Final_Data$Media.Week, Final_Data$Year, sep = "-"), format = "%w-%W-%Y")
Final_Data$Date_WeekEnding<-as.Date(paste("1", Final_Data$Media.Week, Final_Data$Year, sep = "-"), format = "%w-%W-%Y")+6

##Media Level by RAAD definitions (09/24/2018): TV and Online Video as Video, Mobile and Online Display as Digital Non-Video and Radio and Print as OOH
Video<-subset(Final_Data, Media.1. == 'Television' | Media.1. == 'Online Video' )
Digital_Non_Video<-subset(Final_Data, Media.1. == 'Mobile' | Media.1. == 'Online Display' )
OOH<-subset(Final_Data, Media.1. == 'Print' | Media.1. == 'Radio' )
Digital_Non_Video$Media<-'Digital Non Video'
Video$Media<-'Video'
OOH$Media<-'OOH'
Final_Data<-rbind(Digital_Non_Video,OOH,Video)
Final_Data$Geo<-'US'

##Write the Final Data file for further analysis##
setwd('D:/WORK/R RELATED/STACKOVERFLOW/OUTPUTS/US/WEEKLY')
write.csv(Final_Data, file = paste("Stackoverflow_Weekly_Report_Output",Sys.Date(),"csv",sep = '.'))

##End of Code##





























































