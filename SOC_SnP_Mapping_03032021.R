# SnP Mapping 03032021
# Step 1: SnP Data from SQL with BU level filters remove - ALTS / CTAS/ Global Derivatives / IRAS / NTAM / VR, retain remaining
# Step 2: SnP Data filtered with the latest month
# Step 3: Check the exceptions where the Name is present as a Reporting Manager but not present as an individual record at Position Number
# Step 4: After creating individual records for the exceptions, Anthony Cooney's record to report to Toby Glaysher (13002)
# Documentation : https://www.rdocumentation.org/packages/hR/versions/0.2.50/vignettes/hR.Rmd

#rbind the main table with the exceptions table to complete the list 
Book<-rbind(SnP_Feb_2021, MR_Feb_2021)

#Few Initial Changes (*Dynamic) to accomodate the Span of Control
Book$REPORTS_TO_NAME[Book$POSITION_NBR == 26363 ]<-'Sandeep Kumar Singh Thakur'
Book$REPORTS_TO_POSN[Book$POSITION_NBR == 26363 ]<- 47561

#hR library has the hierarchy function which can give the Org levels structure
library(hR)
df<-hierarchy(Book$POSITION_NBR, Book$REPORTS_TO_POSN, format = 'wide')

#Rename Employee column to POSITION_NBR to match the main df
colnames(df)[colnames(df)=='Employee']<-'POSITION_NBR'

Names.df<-Book[,23:24]

#Use Match() to get the Supervisors names after doing a org level details by Position numbers
df$Supv1.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv1, Names.df$REPORTS_TO_POSN)]
df$Supv2.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv2, Names.df$REPORTS_TO_POSN)]
df$Supv3.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv3, Names.df$REPORTS_TO_POSN)]
df$Supv4.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv4, Names.df$REPORTS_TO_POSN)]
df$Supv5.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv5, Names.df$REPORTS_TO_POSN)]
df$Supv6.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv6, Names.df$REPORTS_TO_POSN)]
df$Supv7.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv7, Names.df$REPORTS_TO_POSN)]
df$Supv8.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv8, Names.df$REPORTS_TO_POSN)]
df$Supv9.Name <- Names.df$REPORTS_TO_NAME[match(df$Supv9, Names.df$REPORTS_TO_POSN)]

#Do an outer join
Book<-merge(x = Book, y = df, by = "POSITION_NBR", all = TRUE)

#Hierarchy function to get the direct and indirect reports directly, also check Rough sheet row numbers 1076 to 1080 for the other possible functions in the hierarchy family
hStats = hierarchyStats(Book$POSITION_NBR,Book$REPORTS_TO_POSN)
Reports.df <- hStats$spanOfControlTable

colnames(Reports.df)[colnames(Reports.df)=='Employee']<-'POSITION_NBR'
Book<-merge(x = Book, y = Reports.df, by = "POSITION_NBR", all.x = TRUE)
Book$IndirectReports<-Book$spanOfControl - Book$directReports

#Mapping the Function variable using the Org levels created
Book$Function <- Book$PRACTICE
Book$Function[Book$Supv3.Name == 'Bangalore Loganath Kamalarajan']<-'DO'
Book$Function[Book$Supv3.Name == 'Vaishnavi Narendra Kudige']<-'EBS'
Book$Function[Book$Supv4.Name == 'Vikas Mangla']<-'Fund Accounting'
Book$Function[Book$Supv4.Name == 'Sachin Venkatadri']<-'Fund Accounting'
Book$Function[Book$Supv4.Name == 'Dharmendra Ramsinh Chohan']<-'Transfer Agency'
Book$Function[Book$Supv4.Name == 'Rag Ranjan']<-'NTHFS'
Book$Function[Book$Supv4.Name == 'Muthukrishnan Sridhar']<-'TA Matrix'
Book$Function[Book$Supv4.Name == 'Muzna Abdul Gafoor Memon']<-'GFS Control'
Book$Function[Book$Supv4.Name == 'Sandeep Kumar Singh Thakur']<-'GFS Training'
Book$Function[Book$Supv4.Name == 'Pravin Shetty']<-'FACP'
Book$Function[Book$Supv4.Name == 'Javed Gulam Shaikh']<-'FACP'

Book$Function[Book$DEPARTMENT == 4910 | Book$DEPARTMENT == 4837 | Book$DEPARTMENT == 4840 | Book$DEPARTMENT == 7741 | Book$DEPARTMENT == 8807]<-'PE & REIF'


#Additional Mapping by Name (Dynamic section - Manual)
Book$Function[Book$POSITION_NBR == 49459 ]<-'FACP'
Book$Function[Book$POSITION_NBR == 41698 ]<-'FACP'
Book$Function[Book$POSITION_NBR == 40492 ]<-'FACP'
Book$Function[Book$POSITION_NBR == 35602 ]<-'FACP'
Book$Function[Book$POSITION_NBR == 26351 ]<-'FACP'
Book$Function[Book$POSITION_NBR == 26363 ]<-'GFS Training'

Book$Function[Book$NAME == 'Muzna Abdul Gafoor Memon'] <- 'GFS Control'
Book$Function[Book$NAME == 'Malini Varier'] <- 'GFS Control'
Book$Function[Book$NAME == 'Sandeep Kumar Singh Thakur'] <- 'GFS Training'

#Replace na by blank in the derived columns

Book$Supv1[is.na(Book$Supv1)]<-""
Book$Supv2[is.na(Book$Supv2)]<-""
Book$Supv3[is.na(Book$Supv3)]<-""
Book$Supv4[is.na(Book$Supv4)]<-""
Book$Supv5[is.na(Book$Supv5)]<-""
Book$Supv6[is.na(Book$Supv6)]<-""
Book$Supv7[is.na(Book$Supv7)]<-""
Book$Supv8[is.na(Book$Supv8)]<-""
Book$Supv9[is.na(Book$Supv9)]<-""

Book$Supv1.Name[is.na(Book$Supv1.Name)]<-""
Book$Supv2.Name[is.na(Book$Supv2.Name)]<-""
Book$Supv3.Name[is.na(Book$Supv3.Name)]<-""
Book$Supv4.Name[is.na(Book$Supv4.Name)]<-""
Book$Supv5.Name[is.na(Book$Supv5.Name)]<-""
Book$Supv6.Name[is.na(Book$Supv6.Name)]<-""
Book$Supv7.Name[is.na(Book$Supv7.Name)]<-""
Book$Supv8.Name[is.na(Book$Supv8.Name)]<-""
Book$Supv9.Name[is.na(Book$Supv9.Name)]<-""

Book$directReports[is.na(Book$directReports)]<-""
Book$spanOfControl[is.na(Book$spanOfControl)]<-""
Book$IndirectReports[is.na(Book$IndirectReports)]<-""

#Use Match() to get the PositionTitle names from tblOTAttendance table mapping the EmpID
Book$PositionTitle <- tblOTAttendance$PositionTitle[match(Book$POSITION_NBR, tblOTAttendance$Position.Number)]


#Ideal Span Y/N based on Rahul Kapil's inputs
Book$Ideal.Span[Book$PositionTitle == 'Team Lead' & Book$directReports == 5 ]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Team Lead' & Book$directReports == 8 ]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Team Lead' & Book$directReports <5 ]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Team Lead' & Book$directReports == 6 ]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Team Lead' & Book$directReports == 7 ]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Team Lead' & Book$directReports >8 ]<- 'No'

Book$Ideal.Span[Book$PositionTitle == 'Consultant' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Div Manager' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Head of Operations' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Manager' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Ops Manager' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Section Manager' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Specialist' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'
Book$Ideal.Span[Book$PositionTitle == 'Sr Consult' & (Book$directReports == 3 | Book$directReports == 4| Book$directReports == 5)]<- 'Yes'

Book$Ideal.Span[Book$PositionTitle == 'Consultant' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Div Manager' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Head of Operations' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Manager' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Ops Manager' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Section Manager' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Specialist' & (Book$directReports <3 | Book$directReports >5)]<- 'No'
Book$Ideal.Span[Book$PositionTitle == 'Sr Consult' & (Book$directReports <3 | Book$directReports >5)]<- 'No'

#Exception handling when the Position Title is Manager (This to be only SM / OM), manual intervention
#Logic (M3+VP=DM; M3+2VP=OM; M3+Officer=SM and M4+VP=DM)
Book$PositionTitle[Book$PositionTitle == 'Manager']<-"NA" 
Book$PositionTitle[Book$PositionTitle == 'Manager' & (Book$JOB_LVL == "M3" & Book$OFFICIAL_TITLE == "VP")]<- 'Div Manager'
Book$PositionTitle[Book$PositionTitle == 'Manager' & (Book$JOB_LVL == "M3" & Book$OFFICIAL_TITLE == "2nd VP")]<- 'Ops Manager'
Book$PositionTitle[Book$PositionTitle == 'Manager' & (Book$JOB_LVL == "M3" & Book$OFFICIAL_TITLE == "Officer")]<- 'Section Manager'
Book$PositionTitle[Book$PositionTitle == 'Manager' & (Book$JOB_LVL == "M4" & Book$OFFICIAL_TITLE == "VP")]<- 'Div Manager'

##Write the Final Data file for further analysis##
setwd('//IDIDFSP01/Home$/vs405/My Documents/WORK/1 PROJECTS/Datafiles/SnP/SOC/FEB GFS SOC/OUTPUT FILES')
write.csv(Book, file = paste("SoC",Sys.Date(),"csv",sep = '.'))

##End of Code##












