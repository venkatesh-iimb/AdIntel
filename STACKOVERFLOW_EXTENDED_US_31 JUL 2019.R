##Redefining Categories and Sub-Categories - July 2019 ## Ben and Keala

##Recode / Refresh InHouse entries
#First step get all the Y and N codes into one subset (This is only to handle the NA entries which otherwise is getting into character(0))
YandN<-subset(Final_Data, In.House == 'Y' | In.House == 'N')
###USING %IN% TO REMOVE ONE DATAFRAME FROM ANOTHER
NotYandN<-Final_Data[!(Final_Data$In.House %in% YandN$In.House),]
NotYandN$In.House<-'N'

#Brand Subsets
In_Microsoft<-subset(NotYandN, Up_Classification == 'Microsoft')
In_Amazon<-subset(NotYandN, Up_Classification == 'Amazon')
In_Google<-subset(NotYandN, Up_Classification == 'Google')
In_MAG<-rbind(In_Microsoft, In_Amazon,In_Google)
In_Others<-NotYandN[!(NotYandN$Up_Classification %in% In_MAG$Up_Classification),]

#Brand CodeBlock for recoding the In.House Entries (Single variable used In_Brand rows for efficiency)
In_brand_Rows<-grep("MSN", In_Microsoft$MediaOutlet)
In_Microsoft$In.House[In_brand_Rows]<-'Y'
In_brand_Rows<-grep("bing", In_Microsoft$MediaOutlet)
In_Microsoft$In.House[In_brand_Rows]<-'Y'
In_brand_Rows<-grep("LinkedIn", In_Microsoft$MediaOutlet)
In_Microsoft$In.House[In_brand_Rows]<-'Y'
In_brand_Rows<-grep("google", In_Google$MediaOutlet)
In_Google$In.House[In_brand_Rows]<-'Y'
In_brand_Rows<-grep("Amazon", In_Amazon$MediaOutlet)
In_Amazon$In.House[In_brand_Rows]<-'Y'

NotYandN<-rbind(In_Amazon, In_Microsoft, In_Google, In_Others)
Final_Data<-rbind(YandN, NotYandN)

##Instructions, split the Final data back into paths
GP<-subset(Final_Data, Path == 'Gaming')
BP<-subset(Final_Data, Path == 'Business')
CP<-subset(Final_Data, Path == 'Consumer')

##Defining Business Categories & Sub-Categories in Gaming Path
#Categories

#Recode the Notebook entries from GP to CP
GP$Path[GP$Subcategory == 'Notebook - Intel/AMD']<-'Consumer'

GP$New_Category[GP$Category == 'Toys & Games']<-'Retail'
GP$New_Category[GP$Category == 'Software-Games']<-'Gaming'
GP$New_Category[GP$Category == 'Services and Accessories']<-'Gaming'
GP$New_Category[GP$Category == 'Fast Food (QSR)']<-'N/A'
GP$New_Category[GP$Category == 'Internet Content Providers']<-'Tech Services'
GP$New_Category[GP$Category == 'Subscription Video Services']<-'Tech Services'

#Subcategories
GP$New_Subcategory[GP$Subcategory == 'Video Game Console/Accessories']<-'Console/Hardware'
GP$New_Subcategory[GP$Subcategory == 'Image- Gaming']<-'Console/Hardware'
GP$New_Subcategory[GP$Subcategory == 'Gaming Publishing & Content']<-'Console/Hardware'
GP$New_Subcategory[GP$Subcategory == 'Other']<-'Toys & Games'
GP$New_Subcategory[GP$Subcategory == 'Other Gaming-Related Accessories']<-'Others'
GP$New_Subcategory[GP$Subcategory == 'Gaming Subscription & Download']<-'Subscriptions'
GP$New_Subcategory[GP$Subcategory == 'online games']<-'Others'
GP$New_Subcategory[GP$Subcategory == 'Online Games']<-'Others'
GP$New_Subcategory[GP$Subcategory == 'Action']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Sports']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Role Playing']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Driving']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Strategy']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Adventure']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Simulation']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Miscellaneous Games']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Bundles']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Family Entertainment']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Desktops & Laptops']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Games']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Mobile Gaming Apps']<-'Titles'
GP$New_Subcategory[GP$Subcategory == 'Online Shopping']<-'Others'
GP$New_Subcategory[GP$Category == 'Fast Food (QSR)']<-'N/A'
GP$New_Subcategory[GP$Subcategory == 'IPTV Service']<-'Video/Streaming'

##Category clean up for remaining records by pushing the old categories itself since initial overriding doesnt work
GP$New_Category[is.na(GP$New_Category)]<-'zero'
GP_C_zero<-subset(GP, New_Category == 'zero')
GP_C_Not_zero<-subset(GP, New_Category != 'zero')
GP_C_zero$New_Category<-GP_C_zero$Category
GP<-rbind(GP_C_zero, GP_C_Not_zero)

##Subcategory clean up for remaining records by pushing the old categories itself since initial overriding doesnt work
GP$New_Subcategory[is.na(GP$New_Subcategory)]<-'zero'
GP_SC_zero<-subset(GP, New_Subcategory == 'zero')
GP_SC_Not_zero<-subset(GP, New_Subcategory != 'zero')
GP_SC_zero$New_Subcategory<-GP_SC_zero$Subcategory
GP<-rbind(GP_SC_zero, GP_SC_Not_zero)

##Defining Business Categories & Sub-Categories in Business Path

#Image
BP$New_Category[BP$Subcategory == 'Image']<-'Image'
BP$New_Subcategory[BP$Subcategory == 'Image']<-'Others'
#Computers
BP$New_Subcategory[BP$Subcategory == 'Notebook - Intel/AMD']<-'Laptop'
BP$New_Subcategory[BP$Subcategory == 'Servers - High End']<-'Servers'
BP$New_Subcategory[BP$Subcategory == 'Servers - PC']<-'Servers'
BP$New_Subcategory[BP$Subcategory == 'Tablet PC']<-'Tablet'
BP$New_Subcategory[BP$Subcategory == 'Ultrabook']<-'Laptop'
BP$New_Subcategory[BP$Subcategory == 'Desktop - Other (Non Intel/AMD)']<-'Desktop'
BP$New_Subcategory[BP$Subcategory == 'Desktops & Laptops']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Desktops & Notebooks & Servers']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'MultiTech Prods w PC']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Main Frames']<-'Servers'
BP$New_Subcategory[BP$Subcategory == 'Desktop - Macintosh']<-'Desktop'
BP$New_Subcategory[BP$Subcategory == 'Other Systems']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Desktop - Intel/AMD']<-'Desktop'
#Consumer Electronics
BP$New_Subcategory[BP$Subcategory == 'Other Consumer Electronics']<-'Others'
BP$New_Category[BP$Subcategory == 'Electronics']<-'Consumer Electronics'
BP$New_Subcategory[BP$Subcategory == 'Electronics']<-'Others'
#General Computer
BP$New_Subcategory[BP$Subcategory == 'Corporate Image - Computers']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Sponsorship']<-'Others'
BP$New_Category[BP$Subcategory == 'Corporate Image - Computers']<-'Image'
BP$New_Category[BP$Subcategory == 'Sponsorship']<-'Events and Summits'
#Software Operating Systems
BP$New_Category[BP$Category == 'Software-Operating Systems']<-'Operating Systems'
BP$New_Subcategory[BP$Category == 'Software-Operating Systems']<-'Others'
#Wireless Communications
BP$New_Subcategory[BP$Category == 'Wireless Communications']<-'Others'
#Storage Devices
BP$New_Subcategory[BP$Category == 'Storage Devices']<-'Storage Devices'
BP$New_Category[BP$Category == 'Storage Devices']<-'Computer components and hardware devices'
#Software
BP$New_Category[BP$Category == 'Software']<-'Tech Services'
BP$New_Subcategory[BP$Category == 'Software']<-'Productivity/Business Apps & Software'
BP$New_Category[BP$Subcategory == 'Finance/Insurance Software']<-'N/A'
BP$New_Subcategory[BP$Subcategory == 'Finance/Insurance Software']<-'N/A'
BP$New_Subcategory[BP$Subcategory == 'Mobile Apps']<-'Others'
BP$New_Category[BP$Subcategory == 'Mobile Apps']<-'Voice Assistance/Home Hubs'
BP$New_Subcategory[BP$Subcategory == 'Manufacturing/Distrib/SCM Software']<-'Others'
BP$New_Category[BP$Subcategory == 'Manufacturing/Distrib/SCM Software']<-'AI and Cloud'
BP$New_Subcategory[BP$Subcategory == 'Cloud/virtualization services']<-'Others'
BP$New_Category[BP$Subcategory == 'Cloud/virtualization services']<-'AI and Cloud'
#Image
BP$New_Subcategory[BP$Category == 'Image']<-'Others'
#Peripherals (Other)
BP$New_Category[BP$Category == 'Peripherals (other)']<-'Computer components and hardware devices'
BP$New_Subcategory[BP$Category == 'Peripherals (other)']<-'Computer Component'
BP$New_Category[BP$Category == 'Peripherals (Other)']<-'Computer components and hardware devices'
BP$New_Subcategory[BP$Category == 'Peripherals (Other)']<-'Computer Component'
BP$New_Subcategory[BP$Subcategory == 'Multiple Peripherals-Input Devices']<-'Others'
#Networking
BP$New_Category[BP$Subcategory == 'Networking Advertisers Image']<-'Image'
BP$New_Subcategory[BP$Subcategory == 'Networking Advertisers Image']<-'Others'
BP$New_Category[BP$Category == 'Networking']<-'Computer components and hardware devices'
#Misc Products
BP$New_Category[BP$Category == 'Miscellaneous Products']<-'Consumer Electronics'
BP$New_Subcategory[BP$Category == 'Miscellaneous Products']<-'Others'
#Internet Content Providers
BP$New_Category[BP$Category == 'Internet Content Providers']<-'Tech Services'
BP$New_Subcategory[BP$Subcategory == 'Online Shopping']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Social Networking Sites']<-'Productivity/Business Apps & Software'
#Internet Related Services
BP$New_Category[BP$Category == 'Internet Related Services']<-'Tech Services'
BP$New_Subcategory[BP$Subcategory == 'Search Engine/Portal']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'High Speed Internet']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'Application Service Provider']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'E-mail Services']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'E-Mail Services']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'Other Internet Related']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'Web Hosting/Development']<-'Internet, Apps & Subscriptions'
BP$New_Subcategory[BP$Subcategory == 'On-line Services']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Internet Marketing I']<-'Others'
BP$New_Subcategory[BP$Subcategory == 'Internet Broadcast/streaming media']<-'Video/Streaming'
#Home & Garden: Major Appliances
BP$New_Category[BP$Category == 'Home & Garden: Major Appliances']<-'Retail'
BP$Subcategory[BP$Category == 'Home & Garden: Major Appliances']<-'Home & Garden: Major Appliances'
#Computer Related Services
BP$New_Category[BP$Category == 'Computer Related Services']<-'Tech Services'
BP$New_Subcategory[BP$Category == 'Computer Related Services']<-'Productivity/Business Apps & Software'
BP$New_Category[BP$Subcategory == 'Cloud/Virtualization Services']<-'AI and Cloud'
BP$New_Subcategory[BP$Subcategory == 'Cloud/Virtualization Services']<-'Others'
BP$New_Category[BP$Subcategory == 'Other Services']<-'AI and Cloud'
BP$New_Subcategory[BP$Subcategory == 'Other Services']<-'Others'
#Retail
BP$New_Category[BP$Subcategory == 'Electronic & Appliance Stores']<-'Multiple Categories'
BP$New_Subcategory[BP$Subcategory == 'Electronic & Appliance Stores']<-'Others'


##Category clean up for remaining records by pushing the old categories itself since initial overriding doesnt work
BP$New_Category[is.na(BP$New_Category)]<-'zero'
BP_C_zero<-subset(BP, New_Category == 'zero')
BP_C_Not_zero<-subset(BP, New_Category != 'zero')
BP_C_zero$New_Category<-BP_C_zero$Category
BP<-rbind(BP_C_zero, BP_C_Not_zero)

##Subcategory clean up for remaining records by pushing the old categories itself since initial overriding doesnt work
BP$New_Subcategory[is.na(BP$New_Subcategory)]<-'zero'
BP_SC_zero<-subset(BP, New_Subcategory == 'zero')
BP_SC_Not_zero<-subset(BP, New_Subcategory != 'zero')
BP_SC_zero$New_Subcategory<-BP_SC_zero$Subcategory
BP<-rbind(BP_SC_zero, BP_SC_Not_zero)

##Defining Business Categories & Sub-Categories in Consumer Path

#Computer Related Services
CP$New_Category[CP$Category == 'Computer Related Services']<-'Tech Services'
CP$New_Subcategory[CP$Category == 'Computer Related Services']<-'Productivity/Business Apps & Software'
CP$New_Category[CP$Product.Campaign == 'eBook']<-'Consumer Electronics'
CP$New_Subcategory[CP$Product.Campaign == 'eBook']<-'Others'
CP$New_Category[CP$Subcategory == 'Cloud/Virtualization Services']<-'AI and Cloud'
CP$New_Subcategory[CP$Subcategory == 'Cloud/Virtualization Services']<-'Others'
CP$New_Category[CP$Subcategory == 'Other Services']<-'AI and Cloud'
CP$New_Subcategory[CP$Subcategory == 'Other Services']<-'Others'
#Computers
CP$New_Subcategory[CP$Subcategory == 'Notebook - Intel/AMD']<-'Laptop'
CP$New_Subcategory[CP$Subcategory == 'Desktops & Laptops']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'MultiTech Prods w PC']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Tablet PC']<-'Tablet'
CP$New_Subcategory[CP$Subcategory == 'Notebook - Macintosh']<-'Laptop'
CP$New_Subcategory[CP$Subcategory == 'Multi Consumer Prods w PC']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Desktop - Intel/AMD']<-'Desktop'
CP$New_Subcategory[CP$Subcategory == 'Ultrabook']<-'Laptop'
CP$New_Subcategory[CP$Subcategory == 'Desktops & Notebooks & Servers']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Desktop - Other (Non Intel/AMD)']<-'Desktop'
CP$New_Subcategory[CP$Subcategory == 'Desktop - Macintosh']<-'Desktop'
CP$New_Subcategory[CP$Subcategory == 'Notebook - Other Non Intel/AMD']<-'Laptop'
#Software
CP$New_Category[CP$Category == 'Software']<-'Tech Services'
CP$New_Subcategory[CP$Category == 'Software']<-'Productivity/Business Apps & Software'
CP$New_Subcategory[CP$Subcategory == 'Cloud/virtualization services']<-'Others'
CP$New_Category[CP$Subcategory == 'Cloud/virtualization services']<-'AI and Cloud'
CP$New_Subcategory[CP$Subcategory == 'Manufacturing/Distrib/SCM Software']<-'Others'
CP$New_Category[CP$Subcategory == 'Manufacturing/Distrib/SCM Software']<-'AI and Cloud'
CP$New_Subcategory[CP$Subcategory == 'Mobile Apps']<-'Others'
CP$New_Category[CP$Subcategory == 'Mobile Apps']<-'Voice Assistance/Home Hubs'
CP$New_Category[CP$Subcategory == 'Finance/Insurance Software']<-'N/A'
CP$New_Subcategory[CP$Subcategory == 'Finance/Insurance Software']<-'N/A'
#Software Operating Systems
CP$New_Category[CP$Category == 'Software-Operating Systems']<-'Operating Systems'
CP$New_Subcategory[CP$Category == 'Software-Operating Systems']<-'Others'
#Subscription Video Services
CP$New_Category[CP$Category == 'Subscription Video Services']<-'Tech Services'
CP$New_Subcategory[CP$Category == 'Subscription Video Services']<-'Video/Streaming'
#Image
CP$New_Subcategory[CP$Category == 'Image']<-'Others'
#Internet Content Providers
CP$New_Category[CP$Category == 'Internet Content Providers']<-'Tech Services'
CP$New_Subcategory[CP$Subcategory == 'Music download']<-'Video/Streaming'
CP$New_Subcategory[CP$Subcategory == 'Social Networking Sites']<-'Productivity/Business Apps & Software'
CP$New_Subcategory[CP$Subcategory == 'Online shopping']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Computer Company: other content']<-'Video/Streaming'
#Peripherals (Other)
CP$New_Category[CP$Category == 'Peripherals (other)']<-'Computer components and hardware devices'
CP$New_Subcategory[CP$Category == 'Peripherals (other)']<-'Computer Component'
CP$New_Category[CP$Category == 'Peripherals (Other)']<-'Computer components and hardware devices'
CP$New_Subcategory[CP$Category == 'Peripherals (Other)']<-'Computer Component'
CP$New_Subcategory[CP$Subcategory == 'Multiple Peripherals-Input Devices']<-'Others'
#Internet Related Services
CP$New_Category[CP$Category == 'Internet Related Services']<-'Tech Services'
CP$New_Subcategory[CP$Subcategory == 'Search Engine/Portal']<-'Internet, Apps & Subscriptions'
CP$New_Subcategory[CP$Subcategory == 'On-line Services']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Internet Broadcast/streaming media']<-'Video/Streaming'
CP$New_Subcategory[CP$Subcategory == 'High Speed Internet']<-'Internet, Apps & Subscriptions'
CP$New_Subcategory[CP$Subcategory == 'E-mail Services']<-'Internet, Apps & Subscriptions'
CP$New_Subcategory[CP$Subcategory == 'E-Mail Services']<-'Internet, Apps & Subscriptions'
CP$New_Subcategory[CP$Subcategory == 'Internet Marketing I']<-'Others'
#Consumer Electronics
CP$New_Subcategory[CP$Category == 'Consumer Electronics']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Audio headsets']<-'Audio headsets'
CP$New_Category[CP$Subcategory == 'Electronics']<-'Consumer Electronics'
CP$New_Subcategory[CP$Subcategory == 'Electronics']<-'Others'
#General Computer
CP$New_Subcategory[CP$Subcategory == 'Corporate Image - Computers']<-'Others'
CP$New_Subcategory[CP$Subcategory == 'Sponsorship']<-'Others'
CP$New_Category[CP$Subcategory == 'Corporate Image - Computers']<-'Image'
CP$New_Category[CP$Subcategory == 'Sponsorship']<-'Events and Summits'
#Credit Cards
CP$New_Category[CP$Category == 'Credit Cards']<-'Retail'
CP$New_Subcategory[CP$Category == 'Credit Cards']<-'Credit Cards'
#Department Stores
CP$New_Category[CP$Category == 'Department Stores']<-'Retail'
CP$New_Subcategory[CP$Category == 'Department Stores']<-'Department Stores'
#Wireless Communications
CP$New_Subcategory[CP$Category == 'Wireless Communications']<-'Others'
CP$New_Category[CP$Subcategory == 'Image']<-'Image'
CP$New_Subcategory[CP$Subcategory == 'Image']<-'Others'
#Retail
CP$New_Category[CP$Subcategory == 'Electronic & Appliance Stores']<-'Multiple Categories'
CP$New_Subcategory[CP$Subcategory == 'Electronic & Appliance Stores']<-'Others'

##Category clean up for remaining records by pushing the old categories itself since initial overriding doesnt work
CP$New_Category[is.na(CP$New_Category)]<-'zero'
CP_C_zero<-subset(CP, New_Category == 'zero')
CP_C_Not_zero<-subset(CP, New_Category != 'zero')
CP_C_zero$New_Category<-CP_C_zero$Category
CP<-rbind(CP_C_zero, CP_C_Not_zero)

##Subcategory clean up for remaining records by pushing the old categories itself since initial overriding doesnt work
CP$New_Subcategory[is.na(CP$New_Subcategory)]<-'zero'
CP_SC_zero<-subset(CP, New_Subcategory == 'zero')
CP_SC_Not_zero<-subset(CP, New_Subcategory != 'zero')
CP_SC_zero$New_Subcategory<-CP_SC_zero$Subcategory
CP<-rbind(CP_SC_zero, CP_SC_Not_zero)

#Appending the three paths
Final_Data<-rbind(GP, BP, CP)
Final_Data$New_Subcategory[Final_Data$New_Subcategory == 'Other']<-'Others'

##Digital and Non Digital Filters (D -> OV, OD and Mobile, ND ->Radio, Print and TV)
Final_Data$Media4[Final_Data$Media.1. == 'Television']<-'Non Digital'
Final_Data$Media4[Final_Data$Media.1. == 'Radio']<-'Non Digital'
Final_Data$Media4[Final_Data$Media.1. == 'Print']<-'Non Digital'
Final_Data$Media4[Final_Data$Media.1. == 'Online Video']<-'Digital'
Final_Data$Media4[Final_Data$Media.1. == 'Online Display']<-'Digital'
Final_Data$Media4[Final_Data$Media.1. == 'Mobile']<-'Digital'

##Write the Final Data file for further analysis##
setwd('D:/WORK/R RELATED/STACKOVERFLOW/OUTPUTS/US/WEEKLY')
write.csv(Final_Data, file = paste("Stackoverflow_Weekly_Report_Output",Sys.Date(),"csv",sep = '.'))

##End of Code##
