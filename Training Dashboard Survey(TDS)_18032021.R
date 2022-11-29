##Automation of Training Dashboard Survey - 18th March 2021
##Datafile used - Praveen S - L1 Survey Report for ILT

##Get the Date to extract the Month and Year into separate columns

library(lubridate)
TDS$Date<- lubridate::dmy(TDS$Scheduled.Class.End.Date)
TDS$Year<-format(as.Date(TDS$Date),"%Y")
TDS$Month<-format(as.Date(TDS$Date),"%m")
TDS$Day<-format(as.Date(TDS$Date),"%d")

##Map the Teams by the name of the instructor
TDS$Team[TDS$Instructor.Full.Name == "Bhavna Hariharan"]<-"CVR"
TDS$Team[TDS$Instructor.Full.Name == "Divya P"]<-"HFS"
TDS$Team[TDS$Instructor.Full.Name == "Genevieve Fernandez"]<-"DO"
TDS$Team[TDS$Instructor.Full.Name == "Pawan Kumar"]<-"Derivatives"
TDS$Team[TDS$Instructor.Full.Name == "Pranshi Gupta"]<-"IOO"
TDS$Team[TDS$Instructor.Full.Name == "Prasanna C"]<-"FACP"
TDS$Team[TDS$Instructor.Full.Name == "Radha Gattani"]<-"FA"
TDS$Team[TDS$Instructor.Full.Name == "Soumya C V"]<-"FA"
TDS$Team[TDS$Instructor.Full.Name == "Sruthi Sathyan"]<-"HFS"
TDS$Team[TDS$Instructor.Full.Name == "Sagar Hashilkar"]<-"FACP"
TDS$Team[TDS$Instructor.Full.Name == "Poonam Jain"]<-"FA"
TDS$Team[TDS$Instructor.Full.Name == "Maria Linda Xavier"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Linda Xavier"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Mary Anthony"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Mary Tressa Anthony"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Shwetha S K"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Sunitha S"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Ahmed Faraaz S"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Rukmini Lohith"]<-"PE/REIF"
TDS$Team[TDS$Instructor.Full.Name == "Anisha Sayyed"]<-"FA"
TDS$Team[TDS$Instructor.Full.Name == "Nirmala Naik"]<-"HFS"
TDS$Team[TDS$Instructor.Full.Name == "Nirmala Mangesh Naik"]<-"HFS"
TDS$Team[TDS$Instructor.Full.Name == "Meghana Hosmath"]<-"FA"
TDS$Team[TDS$Instructor.Full.Name == "Poonam Gawande"]<-"HFS"
TDS$Team[TDS$Instructor.Full.Name == "Poonam Yogesh Gawande"]<-"HFS"
TDS$Team[TDS$Instructor.Full.Name == "Amit Pandita"]<-"IOO"
TDS$Team[TDS$Instructor.Full.Name == "Jayadevan Meppalli"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Poonam Patnaik"]<-"TA"
TDS$Team[TDS$Instructor.Full.Name == "Varghese Thomas"]<-"IRAS"
TDS$Team[TDS$Instructor.Full.Name == "Soumita Bhattacharya"]<-"PE/REIF"
TDS$Team[TDS$Instructor.Full.Name == "Shrenik Surana"]<-"FA"
TDS$Team[TDS$Instructor.Full.Name == "Sangeetha C"]<-"PE/REIF"
TDS$Team[TDS$Instructor.Full.Name == "Vaibhav Jain"]<-"PE/REIF"
TDS$Team[TDS$Instructor.Full.Name == "Sruthi V"]<-"HFS"

##Replace the Avg Rating (10 point scale) which is blank with 2*Summary rating
TDS$Average.rating.for.scale.type.questions<-as.numeric(TDS$Average.rating.for.scale.type.questions)
TDS$Average.rating.for.scale.type.questions[is.na(TDS$Average.rating.for.scale.type.questions)]<-"A"
TDS$Average.rating.for.scale.type.questions[TDS$Average.rating.for.scale.type.questions == "A"]<-TDS$Summary.Score.Rating*2

##Bring back nulls for OEs for the above replacements
TDS$Average.rating.for.scale.type.questions[TDS$Question.Text == "The least valuable part of this training was ."]<-""
TDS$Average.rating.for.scale.type.questions[TDS$Question.Text == "The least valuable part of this training was."]<-""
TDS$Average.rating.for.scale.type.questions[TDS$Question.Text == "The most valuable part of this training was..."]<-""
TDS$Average.rating.for.scale.type.questions[TDS$Question.Text == "The most valuable part of this training was."]<-""

##Creating Themes and Sub-Themes
##Themes
TDS$Theme[TDS$Question.Text == "The least valuable part of this training was ."]<-"OE"
TDS$Theme[TDS$Question.Text == "The least valuable part of this training was."]<-"OE"
TDS$Theme[TDS$Question.Text == "The most valuable part of this training was..."]<-"OE"
TDS$Theme[TDS$Question.Text == "The most valuable part of this training was."]<-"OE"

TDS$Theme[TDS$Question.Text == "I would recommend this course to another partner."]<-"Active Recommendation"
TDS$Theme[TDS$Question.Text == "I would recommend this course to my colleagues"]<-"Active Recommendation"

TDS$Theme[TDS$Question.Text == "My instructor effectively presented the information."]<-"Trainer"
TDS$Theme[TDS$Question.Text == "My instructor had command of the subject matter."]<-"Trainer"
TDS$Theme[TDS$Question.Text == "My instructor encouraged active participation."]<-"Trainer"

TDS$Theme[TDS$Question.Text == "For virtual training only (such as WebEx or Webinar): I was able to access and interact in the virtual training session without encountering technical issues."]<-"Technology"
TDS$Theme[TDS$Question.Text == "I was able to launch and complete the course without technical issues"]<-"Technology"

TDS$Theme[TDS$Question.Text == "I was given ample opportunity to practice the skills I learned."]<-"Partner Learning"
TDS$Theme[TDS$Question.Text == "I will be able to apply what I have learned to my work."]<-"Partner Learning"
TDS$Theme[TDS$Question.Text == "I found the images and graphics in the course supported my learning"]<-"Partner Learning"
TDS$Theme[TDS$Question.Text == "I will be able to apply the training to my current role and/or my future career"]<-"Partner Learning"
TDS$Theme[TDS$Question.Text == "The course included appropriate interactions to support my learning"]<-"Partner Learning"

TDS$Theme[TDS$Question.Text == "I found the course materials useful, easy to navigate and logically sequenced."]<-"Course Related"
TDS$Theme[TDS$Question.Text == "I was comfortable with the pace and duration of the program."]<-"Course Related"
TDS$Theme[TDS$Question.Text == "This training has increased my understanding of the topics covered."]<-"Course Related"
TDS$Theme[TDS$Question.Text == "Any additional supporting content was of a high quality"]<-"Course Related"
TDS$Theme[TDS$Question.Text == "Course instructions and navigation were straightforward"]<-"Course Related"
TDS$Theme[TDS$Question.Text == "The course was logically structured"]<-"Course Related"

##Sub-Themes
TDS$Sub.Theme[TDS$Question.Text == "The least valuable part of this training was ."]<-"OE"
TDS$Sub.Theme[TDS$Question.Text == "The least valuable part of this training was."]<-"OE"
TDS$Sub.Theme[TDS$Question.Text == "The most valuable part of this training was..."]<-"OE"
TDS$Sub.Theme[TDS$Question.Text == "The most valuable part of this training was."]<-"OE"

TDS$Sub.Theme[TDS$Question.Text == "I would recommend this course to another partner."]<-"Active Recommendation"
TDS$Sub.Theme[TDS$Question.Text == "I would recommend this course to my colleagues"]<-"Active Recommendation"

TDS$Sub.Theme[TDS$Question.Text == "My instructor effectively presented the information."]<-"Effective Presentation"
TDS$Sub.Theme[TDS$Question.Text == "My instructor had command of the subject matter."]<-"SME"

TDS$Sub.Theme[TDS$Question.Text == "For virtual training only (such as WebEx or Webinar): I was able to access and interact in the virtual training session without encountering technical issues."]<-"Technology"
TDS$Sub.Theme[TDS$Question.Text == "I was able to launch and complete the course without technical issues"]<-"Technology"

TDS$Sub.Theme[TDS$Question.Text == "My instructor encouraged active participation."]<-"Engagement"
TDS$Sub.Theme[TDS$Question.Text == "I was given ample opportunity to practice the skills I learned."]<-"Engagement"
TDS$Sub.Theme[TDS$Question.Text == "I will be able to apply what I have learned to my work."]<-"Learn & Apply"
TDS$Sub.Theme[TDS$Question.Text == "I found the images and graphics in the course supported my learning"]<-"Learn & Apply"
TDS$Sub.Theme[TDS$Question.Text == "I will be able to apply the training to my current role and/or my future career"]<-"Learn & Apply"
TDS$Sub.Theme[TDS$Question.Text == "The course included appropriate interactions to support my learning"]<-"Engagement"

TDS$Sub.Theme[TDS$Question.Text == "I found the course materials useful, easy to navigate and logically sequenced."]<-"Course Instructions, Navigation & Structure"
TDS$Sub.Theme[TDS$Question.Text == "I was comfortable with the pace and duration of the program."]<-"Course Duration"
TDS$Sub.Theme[TDS$Question.Text == "This training has increased my understanding of the topics covered."]<-"Content Relevance"
TDS$Sub.Theme[TDS$Question.Text == "Any additional supporting content was of a high quality"]<-"Content Relevance"
TDS$Sub.Theme[TDS$Question.Text == "Course instructions and navigation were straightforward"]<-"Course Instructions, Navigation & Structure"
TDS$Sub.Theme[TDS$Question.Text == "The course was logically structured"]<-"Course Instructions, Navigation & Structure"

##Split OE and Not OE, to treat OE separately
OE<-subset(TDS, Theme == "OE")
Not.OE<-subset(TDS,Theme != "OE")

##OE Data Processing
##Replace na, NA, Nill, NILL, NULL with N/A
OE$Question.Response<-tolower(OE$Question.Response)
OE$Sub.Theme[OE$Question.Response == "NA" | OE$Question.Response == "no issues" | OE$Question.Response == "nothing" | OE$Question.Response == "......" | OE$Question.Response == "......" | OE$Question.Response == "...." | OE$Question.Response == "..." | OE$Question.Response == ".." | OE$Question.Response == "," | OE$Question.Response == "**" | OE$Question.Response == "*" | OE$Question.Response == "-" | OE$Question.Response == "." | OE$Question.Response == "NA" |OE$Question.Response == "none" | OE$Question.Response == "none." | OE$Question.Response == "None." | OE$Question.Response == "no comments" |OE$Question.Response == "n/a" | OE$Question.Response == "na" | OE$Question.Response == "nil" | OE$Question.Response == "n/a" | OE$Question.Response == "n\\a" | OE$Question.Response == "n/a." | OE$Question.Response == "n\a" | OE$Question.Response == ""]<-"N/A"
OE$Sub.Theme[is.na(OE$Question.Response)]<-"N/A"

OE$Sub.Theme[grep("nothing",OE$Question.Response, fixed = TRUE)]<-"Nothing"
OE$Sub.Theme[grep("good course",OE$Question.Response, fixed = TRUE)]<-"Everything Important"
OE$Sub.Theme[grep("trainer",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("instructor",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("instructors",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("presenter",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("explanation",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("explained",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("examples",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("example",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("tips",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("illustrations",OE$Question.Response, fixed = TRUE)]<-"Trainer"
OE$Sub.Theme[grep("everything",OE$Question.Response, fixed = TRUE)]<-"Everything Important"
OE$Sub.Theme[grep("every thing",OE$Question.Response, fixed = TRUE)]<-"Everything Important"
OE$Sub.Theme[grep("all",OE$Question.Response, fixed = TRUE)]<-"Everything Important"
OE$Sub.Theme[grep("content",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("material",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("information",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("informative",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("slides",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("presentation",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("concept",OE$Question.Response, fixed = TRUE)]<-"Concept"
OE$Sub.Theme[grep("overview",OE$Question.Response, fixed = TRUE)]<-"Overview / Refresher Course"
OE$Sub.Theme[grep("refresher",OE$Question.Response, fixed = TRUE)]<-"Overview / Refresher Course"
OE$Sub.Theme[grep("activities",OE$Question.Response, fixed = TRUE)]<-"Activities / Participation"
OE$Sub.Theme[grep("participation",OE$Question.Response, fixed = TRUE)]<-"Activities / Participation"
OE$Sub.Theme[grep("exercises",OE$Question.Response, fixed = TRUE)]<-"Activities / Participation"
OE$Sub.Theme[grep("assessments",OE$Question.Response, fixed = TRUE)]<-"Activities / Participation"
OE$Sub.Theme[grep("evaluations",OE$Question.Response, fixed = TRUE)]<-"Activities / Participation"
OE$Sub.Theme[grep("interactive",OE$Question.Response, fixed = TRUE)]<-"Interactivity"
OE$Sub.Theme[grep("insightful",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("interesting",OE$Question.Response, fixed = TRUE)]<-"Content"
OE$Sub.Theme[grep("webex",OE$Question.Response, fixed = TRUE)]<-"Technology"
OE$Sub.Theme[grep("technical issues",OE$Question.Response, fixed = TRUE)]<-"Technology"
OE$Sub.Theme[grep("glitches",OE$Question.Response, fixed = TRUE)]<-"Technology"
OE$Sub.Theme[grep("disconnection",OE$Question.Response, fixed = TRUE)]<-"Technology"
OE$Sub.Theme[grep("it issues",OE$Question.Response, fixed = TRUE)]<-"Technology"

##Remaining into Everything for most valuable and Nothing for least valuable (use the subset form)
OE$Sub.Theme[OE$Sub.Theme == "OE" & OE$Question.Text == "The most valuable part of this training was..."]<- "Everything Important"
OE$Sub.Theme[OE$Sub.Theme == "OE" & OE$Question.Text == "The most valuable part of this training was."]<- "Everything Important"
OE$Sub.Theme[OE$Sub.Theme == "OE" & OE$Question.Text == "The least valuable part of this training was ."]<- "Nothing"
OE$Sub.Theme[OE$Sub.Theme == "OE" & OE$Question.Text == "The least valuable part of this training was."]<- "Nothing"

TDS<-rbind(OE, Not.OE)

##Region based on Trainer names and Teams
TDS$Region<-"India"
TDS$Region[is.na(TDS$Team)]<-"International"

##Create a column called measure for NPS (Promoters, Passives and Detractors)
TDS$Measure<-"NA"

##Subset Active Recommendation questions

AR<-subset(TDS, TDS$Question.Text == "I would recommend this course to another partner." | TDS$Question.Text == "I would recommend this course to my colleagues")
Not.AR<-TDS[!(TDS$Question.Text %in% AR$Question.Text),]

AR$Measure[AR$Average.rating.for.scale.type.questions <= 6.49]<-"Detractors"
AR$Measure[AR$Average.rating.for.scale.type.questions >= 6.5]<-"Passives"
AR$Measure[AR$Average.rating.for.scale.type.questions >= 8.5]<-"Promoters"
AR$Measure[AR$Average.rating.for.scale.type.questions == 9]<-"Promoters"
AR$Measure[AR$Average.rating.for.scale.type.questions == 10]<-"Promoters"

Not.AR$Measure<-"NA"

TDS<-rbind(AR, Not.AR)

##Measure - Trainer Score, using the three questions related to trainer

TDS$Measure[TDS$Question.Text == "My instructor effectively presented the information." | TDS$Question.Text == "My instructor had command of the subject matter." | TDS$Question.Text == "My instructor encouraged active participation."]<-"Trainer Score"

##Write the Final Data file for further analysis##
setwd('F:/My Documents/WORK/1 PROJECTS/Training Dashboard_Sathya_16022021/OUTPUT FILES')
write.csv(TDS, file = paste("TDS",Sys.Date(),"csv",sep = '.'))

##End of Code##
