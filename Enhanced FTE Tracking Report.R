## Enhanced FTTR ##
## Venky_EFTR<-subset(Venky_EFTR, PRACTICE == "Depository Operations" | PRACTICE == "EBS" | PRACTICE == "Fund Accounting" | PRACTICE == "GFS Control & Training" | PRACTICE == "GFS Management" | PRACTICE == "IOO" | PRACTICE == "NTHFS" | PRACTICE == "Transfer Agency")
Dec <- subset (Venky_EFTR, DATE == '12/31/2020')
Nov <- subset (Venky_EFTR, DATE == '11/30/2020')

## Dont use this NP <- setdiff(Dec$POSITION_NBR, Nov$POSITION_NBR)

library(dplyr)    
New_Positions<-anti_join(Dec, Nov, by = c("POSITION_NBR" = "POSITION_NBR"))

## Remove New Positions from Dec / Latest file
Dec_Minus_New_Positions<-Dec[!(Dec$POSITION_NBR %in% New_Positions$POSITION_NBR),]

Eliminations<-anti_join(Nov, Dec, by = c("POSITION_NBR" = "POSITION_NBR"))

## Remove Eliminations from Nov / Previous month file
Nov_Minus_Eliminations<-Nov[!(Nov$POSITION_NBR %in% Eliminations$POSITION_NBR),]

## Raw data for Movements##
Movements_df<-rbind(Dec_Minus_New_Positions, Nov_Minus_Eliminations)
Movements_df$Conc<-paste(Movements_df$DEPARTMENT,Movements_df$POSITION_NBR)

Movements_Dec<-subset (Movements_df, DATE == '12/31/2020')
Movements_Nov<-subset (Movements_df, DATE == '11/30/2020')

Movements<-anti_join(Movements_Dec, Movements_Nov, by = c("Conc" = "Conc"))
Non_Movements<-Movements_Dec[!(Movements_Dec$POSITION_NBR %in% Movements$POSITION_NBR),]

Non_Movements<-Non_Movements[,1:56]
Non_Movements$EMN<-'Nothing'

Movements<-Movements[,1:56]
Movements$EMN<-'Move In'

Movements_Dec<-rbind(Movements, Non_Movements)

Movements_Nov<-Movements_Nov[,1:56]
Movements_Nov$EMN<-'Nothing'

library(plyr)
dfmo<-match_df(Movements_Nov, Movements, on = 'POSITION_NBR')

dfmo$EMN<-'Move Out'
dfnot<-Movements_Nov[!(Movements_Nov$POSITION_NBR %in% dfmo$POSITION_NBR),]
Movements_Nov<-rbind(dfmo, dfnot)

Movements_df<-rbind(Movements_Nov, Movements_Dec)

New_Positions$EMN<-'New Position'
Eliminations$EMN<-'Elimination'

EFTR<-rbind(New_Positions, Eliminations, Movements_df)

##New Hiring Calculation

NH_Dec <- subset (EFTR, DATE == '12/31/2020')
NH_Nov <- subset (EFTR, DATE == '11/30/2020')

New_Hiring<-anti_join(NH_Dec, NH_Nov, by = c("EMPLID" = "EMPLID"))

NH_Dec_Minus_New_Hiring<-NH_Dec[!(NH_Dec$EMPLID %in% New_Hiring$EMPLID),]
New_Hiring$EMN<-'New Hiring'

EFTR<-rbind(New_Hiring, NH_Nov, NH_Dec_Minus_New_Hiring)


##Attrition Calculation

Att_Dec <- subset (EFTR, DATE == '12/31/2020')
Att_Nov <- subset (EFTR, DATE == '11/30/2020')

Attrition<-anti_join(Att_Nov, Att_Dec, by = c("EMPLID" = "EMPLID"))
Att_Nov_Minus_Attrition<-Att_Nov[!(Att_Nov$EMPLID %in% Attrition$EMPLID),]

Attrition$Attrition<-"Yes"
Att_Nov_Minus_Attrition$Attrition<-"No"
Att_Nov<-rbind(Att_Nov_Minus_Attrition, Attrition)

Att_Dec$Attrition<-"No"

EFTR<-rbind(Att_Nov, Att_Dec)

write.csv(EFTR, 'Test.csv')









