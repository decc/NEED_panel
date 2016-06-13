setwd("/home/decc/Documents/need/Panel")
data = read.csv("need_public_use_file_2014.csv")
prices = read.csv("regional_prices_for_panel.csv")
require(plm)
attach(data)

#recode IMD vaiables into 1
data["IMD"] <- ifelse(is.na(IMD_ENG), IMD_WALES, IMD_ENG)

#drop unrequired variables
data = subset(data, select=-c(IMD_ENG, IMD_WALES, Econs2005, Econs2005Valid, Econs2006,
                              Econs2006Valid, Econs2007, Econs2007Valid,
                              Econs2008, Econs2008Valid, Econs2009,
                              Econs2009Valid, Econs2010, Econs2010Valid,
                              Econs2011, Econs2011Valid, Econs2012,
                              Econs2012Valid, E7Flag2012))

#drop rows with invalid gas consumption in any of the years 
data["Gas_valid"] <- ifelse((Gcons2005Valid != "V" |
                     Gcons2006Valid != "V" |
                     Gcons2007Valid != "V" |
                     Gcons2008Valid != "V" |
                     Gcons2009Valid != "V" |
                     Gcons2010Valid != "V" |
                     Gcons2011Valid != "V" |
                     Gcons2012Valid != "V")
                  ,0, 1)

data = subset(data, Gas_valid==1)

###Creating the panel 
#NB the same code is run, with year specific edits, for each year 2005-12
#(there must be a quicker way of doing this!)

#_2005__________________________________________________________________________##
#create a copy the data that only includes consumption for the required year
data_05 = subset(data, select=-c(Gcons2005Valid, Gcons2006, Gcons2006Valid, Gcons2007,Gcons2007Valid,
                                 Gcons2008, Gcons2008Valid, Gcons2009, Gcons2009Valid,
                                 Gcons2010, Gcons2010Valid, Gcons2011, Gcons2011Valid,
                                 Gcons2012, Gcons2012Valid))

#create CWI variable equal to 1 if the CWI_year implies it has already been installed
#and repeat for other energy efficiency installations
data_05$CWI <- 0
data_05$CWI[data_05$CWI_YEAR<2005] <- 1



#create a panel variable 'Year'
data_05["Year"] <-2005

## Rename Gcons2005 to Gas_consump

colnames(data_05)[colnames(data_05)=="Gcons2005"] <- "Gas_cons"
#need to add in the prices data

##calculate average consumption by region
AvCon_05 = aggregate(data_05$Gas_cons, list(data_05$REGION), mean)

data_05 <- merge(data_05, AvCon_05, by.x="REGION", by.y="Group.1")

#add average bill data
prices2005 <-subset(prices, select=c(Region, X2005))
data_05 <- merge(data_05, prices2005, by.x="REGION", by.y="Region")
data_05$unitPrice = (data_05$X2005/data_05$x)*100
#drop bill price variable
data_05 = subset(data_05, select=-c(X2005))

#End of repeated section
#_______________________________________________________________________________##

#Test of the repeated sectin - seems t work.  Add is sections for all other
#EE measures before duplication again
#_2006__________________________________________________________________________##
#create a copy the data that only includes consumption for the required year
data_06 = subset(data, select=-c(Gcons2005, Gcons2005Valid, Gcons2006Valid, Gcons2007,Gcons2007Valid,
                                 Gcons2008, Gcons2008Valid, Gcons2009, Gcons2009Valid,
                                 Gcons2010, Gcons2010Valid, Gcons2011, Gcons2011Valid,
                                 Gcons2012, Gcons2012Valid))

#create CWI variable equal to 1 if the CWI_year implies it has already been installed
#and repeat for other energy efficiency installations
data_06$CWI <- 0
data_06$CWI[data_06$CWI_YEAR<2006] <- 1



#create a panel variable 'Year'
data_06["Year"] <-2006

## Rename Gcons2005 to Gas_consump

colnames(data_06)[colnames(data_06)=="Gcons2006"] <- "Gas_cons"
#need to add in the prices data

##calculate average consumption by region
AvCon_06 = aggregate(data_06$Gas_cons, list(data_06$REGION), mean)
data_06 <- merge(data_06, AvCon_06, by.x="REGION", by.y="Group.1")

#add average bill data
prices2006 <-subset(prices, select=c(Region, X2006))
data_06 <- merge(data_06, prices2006, by.x="REGION", by.y="Region")
data_06$unitPrice = (data_06$X2006/data_06$x)*100
#drop bill price variable
data_06 = subset(data_06, select=-c(X2006))

#End of repeated section
#_______________________________________________________________________________##


#_2007__________________________________________________________________________##

#create a copy the data that only includes consumption for the required year

data_07 = subset(data, select=-c(Gcons2005, Gcons2005Valid,
                                Gcons2006, Gcons2006Valid,
                                Gcons2007Valid,Gcons2008, Gcons2008Valid,
                                Gcons2009, Gcons2009Valid,Gcons2010, Gcons2010Valid,
                                Gcons2011, Gcons2011Valid,Gcons2012, Gcons2012Valid))

#create CWI variable equal to 1 if the CWI_year implies it has already been installed

#and repeat for other energy efficiency installations

data_07$CWI <- 0
data_07$CWI[data_07$CWI_YEAR<2007] <- 1

#create a panel variable 'Year'
data_07["Year"] <-2007
## Rename Gcons2007 to Gas_consump
colnames(data_07)[colnames(data_07)=="Gcons2007"] <- "Gas_cons"
#need to add in the prices data


##calculate average consumption by region

AvCon_07 = aggregate(data_07$Gas_cons, list(data_07$REGION), mean)

data_07 <- merge(data_07, AvCon_07, by.x="REGION", by.y="Group.1")

#add average bill data
prices2007 <-subset(prices, select=c(Region, X2007))
data_07 <- merge(data_07, prices2007, by.x="REGION", by.y="Region")
data_07$unitPrice = (data_07$X2007/data_07$x)*100

#drop bill price variable
data_07 = subset(data_07, select=-c(X2007))

#End of repeated section

#_______________________________________________________________________________##





#_2008__________________________________________________________________________##

#create a copy the data that only includes consumption for the required year

data_08 = subset(data, select=-c(Gcons2005, Gcons2005Valid,Gcons2006, Gcons2006Valid,
                                 Gcons2007, Gcons2007Valid,Gcons2008Valid,Gcons2009,
                                 Gcons2009Valid,Gcons2010, Gcons2010Valid,Gcons2011, Gcons2011Valid,
                                 Gcons2012, Gcons2012Valid))



#create CWI variable equal to 1 if the CWI_year implies it has already been installed

#and repeat for other energy efficiency installations

data_08$CWI <- 0

data_08$CWI[data_08$CWI_YEAR<2008] <- 1





#create a panel variable 'Year'

data_08["Year"] <-2008
## Rename Gcons2008 to Gas_consump
colnames(data_08)[colnames(data_08)=="Gcons2008"] <- "Gas_cons"
#need to add in the prices data
##calculate average consumption by region
AvCon_08 = aggregate(data_08$Gas_cons, list(data_08$REGION), mean)
data_08 <- merge(data_08, AvCon_08, by.x="REGION", by.y="Group.1")

#add average bill data
prices2008 <-subset(prices, select=c(Region, X2008))
data_08 <- merge(data_08, prices2008, by.x="REGION", by.y="Region")
data_08$unitPrice = (data_08$X2008/data_08$x)*100

#drop bill price variable

data_08 = subset(data_08, select=-c(X2008))
#End of repeated section

#_______________________________________________________________________________##






#_2009__________________________________________________________________________##

#create a copy the data that only includes consumption for the required year

data_09 = subset(data, select=-c(Gcons2005, Gcons2005Valid,Gcons2006, Gcons2006Valid,
                                 Gcons2007, Gcons2007Valid,Gcons2008, Gcons2008Valid,
                                 Gcons2009Valid,Gcons2010, Gcons2010Valid,Gcons2011,
                                 Gcons2011Valid,Gcons2012, Gcons2012Valid))


#create CWI variable equal to 1 if the CWI_year implies it has already been installed

#and repeat for other energy efficiency installations

data_09$CWI <- 0

data_09$CWI[data_09$CWI_YEAR<2009] <- 1




#create a panel variable 'Year'
data_09["Year"] <-2009



## Rename Gcons2009 to Gas_consump
colnames(data_09)[colnames(data_09)=="Gcons2009"] <- "Gas_cons"

#need to add in the prices data
##calculate average consumption by region
AvCon_09 = aggregate(data_09$Gas_cons, list(data_09$REGION), mean)
data_09 <- merge(data_09, AvCon_09, by.x="REGION", by.y="Group.1")

#add average bill data

prices2009 <-subset(prices, select=c(Region, X2009))
data_09 <- merge(data_09, prices2009, by.x="REGION", by.y="Region")
data_09$unitPrice = (data_09$X2009/data_09$x)*100

#drop bill price variable
data_09 = subset(data_09, select=-c(X2009))
#End of repeated section

#_______________________________________________________________________________##


#_2010__________________________________________________________________________##

#create a copy the data that only includes consumption for the required year

data_10 = subset(data, select=-c(Gcons2005, Gcons2005Valid,Gcons2006, Gcons2006Valid,
                                 Gcons2007, Gcons2007Valid,Gcons2008, Gcons2008Valid,Gcons2009,
                                 Gcons2009Valid, Gcons2010Valid,Gcons2011, Gcons2011Valid,
                                 Gcons2012, Gcons2012Valid))



#create CWI variable equal to 1 if the CWI_year implies it has already been installed

#and repeat for other energy efficiency installations

data_10$CWI <- 0
data_10$CWI[data_10$CWI_YEAR<2010] <- 1

#create a panel variable 'Year'
data_10["Year"] <-2010
## Rename Gcons2010 to Gas_consump
colnames(data_10)[colnames(data_10)=="Gcons2010"] <- "Gas_cons"
#need to add in the prices data
##calculate average consumption by region
AvCon_10 = aggregate(data_10$Gas_cons, list(data_10$REGION), mean)
data_10 <- merge(data_10, AvCon_10, by.x="REGION", by.y="Group.1")

#add average bill data
prices2010 <-subset(prices, select=c(Region, X2010))
data_10 <- merge(data_10, prices2010, by.x="REGION", by.y="Region")
data_10$unitPrice = (data_10$X2010/data_10$x)*100
#drop bill price variable

data_10 = subset(data_10, select=-c(X2010))

#End of repeated section

#_______________________________________________________________________________##







#_2011__________________________________________________________________________##

#create a copy the data that only includes consumption for the required year

data_11 = subset(data, select=-c(Gcons2005, Gcons2005Valid,Gcons2006, Gcons2006Valid,
				 Gcons2007, Gcons2007Valid,Gcons2008, Gcons2008Valid,
				 Gcons2009, Gcons2009Valid,Gcons2010, Gcons2010Valid,
				 Gcons2011Valid,
				 Gcons2012, Gcons2012Valid))



#create CWI variable equal to 1 if the CWI_year implies it has already been installed

#and repeat for other energy efficiency installations

data_11$CWI <- 0

data_11$CWI[data_11$CWI_YEAR<2011] <- 1




#create a panel variable 'Year'

data_11["Year"] <-2011



## Rename Gcons2011 to Gas_consump



colnames(data_11)[colnames(data_11)=="Gcons2011"] <- "Gas_cons"

#need to add in the prices data



##calculate average consumption by region

AvCon_11 = aggregate(data_11$Gas_cons, list(data_11$REGION), mean)

data_11 <- merge(data_11, AvCon_11, by.x="REGION", by.y="Group.1")



#add average bill data

prices2011 <-subset(prices, select=c(Region, X2011))

data_11 <- merge(data_11, prices2011, by.x="REGION", by.y="Region")

data_11$unitPrice = (data_11$X2011/data_11$x)*100

#drop bill price variable

data_11 = subset(data_11, select=-c(X2011))



#End of repeated section

#_______________________________________________________________________________##





#_2012__________________________________________________________________________##

#create a copy the data that only includes consumption for the required year

data_12 = subset(data, select=-c(Gcons2005, Gcons2005Valid,Gcons2006, Gcons2006Valid,
				 Gcons2007, Gcons2007Valid,Gcons2008, Gcons2008Valid,
				Gcons2009, Gcons2009Valid,Gcons2010, Gcons2010Valid,
				Gcons2011, Gcons2011Valid,
					Gcons2012Valid))



#create CWI variable equal to 1 if the CWI_year implies it has already been installed

#and repeat for other energy efficiency installations

data_12$CWI <- 0

data_12$CWI[data_12$CWI_YEAR<2012] <- 1

#create a panel variable 'Year'

data_12["Year"] <-2012

## Rename Gcons2012 to Gas_consump

colnames(data_12)[colnames(data_12)=="Gcons2012"] <- "Gas_cons"

#need to add in the prices data
##calculate average consumption by region

AvCon_12 = aggregate(data_12$Gas_cons, list(data_12$REGION), mean)
data_12 <- merge(data_12, AvCon_12, by.x="REGION", by.y="Group.1")

#add average bill data

prices2012 <-subset(prices, select=c(Region, X2012))

data_12 <- merge(data_12, prices2012, by.x="REGION", by.y="Region")

data_12$unitPrice = (data_12$X2012/data_12$x)*100

#drop bill price variable

data_12 = subset(data_12, select=-c(X2012))



#End of repeated section

#_______________________________________________________________________________##



##########Bind all years data into one dataset (05-12)
PanelAllYears <- rbind(data_05, data_06, data_07, data_08, data_09, data_10, data_11, data_12)

######## remove 0s from Gas_cons and unitPrice (required as logs are taken in the next step)
PanelAllYears_No0<-PanelAllYears[!(PanelAllYears$Gas_cons==0 | PanelAllYears$unitPrice==0),]

#########Running the panel analysis
#logs are taken of consumption and price to allow the coefficient to be interpretted
#as an elasticity

reg1.re <- plm(log(Gas_cons) ~ MAIN_HEAT_FUEL + PROP_AGE + PROP_TYPE + FLOOR_AREA_BAND +
                 EE_BAND  + WALL_CONS + CWI + IMD + log(unitPrice), 
               data=PanelAllYears_No0, index = "Year", model = "random", na.action = "na.omit")
summary(reg1.re)

#####Subset data based on IMD

Panel_IMD_1 = subset(PanelAllYears_No0, IMD==1)
Panel_IMD_2 = subset(PanelAllYears_No0, IMD==2)
Panel_IMD_3 = subset(PanelAllYears_No0, IMD==3)
Panel_IMD_4 = subset(PanelAllYears_No0, IMD==4)
Panel_IMD_5 = subset(PanelAllYears_No0, IMD==5)


#######re-run panel regression on all subsets of IMD classification
regIMD1.re <- plm(log(Gas_cons) ~ MAIN_HEAT_FUEL + PROP_AGE + PROP_TYPE + FLOOR_AREA_BAND +
                 EE_BAND  + WALL_CONS + CWI + log(unitPrice), 
               data=Panel_IMD_1, index = "Year", model = "random", na.action = "na.omit")
summary(regIMD1.re)

regIMD2.re <- plm(log(Gas_cons) ~ MAIN_HEAT_FUEL + PROP_AGE + PROP_TYPE + FLOOR_AREA_BAND +
                    EE_BAND  + WALL_CONS + CWI + log(unitPrice), 
                  data=Panel_IMD_2, index = "Year", model = "random", na.action = "na.omit")
summary(regIMD2.re)

regIMD3.re <- plm(log(Gas_cons) ~ MAIN_HEAT_FUEL + PROP_AGE + PROP_TYPE + FLOOR_AREA_BAND +
                    EE_BAND  + WALL_CONS + CWI + log(unitPrice), 
                  data=Panel_IMD_3, index = "Year", model = "random", na.action = "na.omit")
summary(regIMD3.re)

regIMD4.re <- plm(log(Gas_cons) ~ MAIN_HEAT_FUEL + PROP_AGE + PROP_TYPE + FLOOR_AREA_BAND +
                    EE_BAND  + WALL_CONS + CWI + log(unitPrice), 
                  data=Panel_IMD_4, index = "Year", model = "random", na.action = "na.omit")
summary(regIMD4.re)

regIMD5.re <- plm(log(Gas_cons) ~ MAIN_HEAT_FUEL + PROP_AGE + PROP_TYPE + FLOOR_AREA_BAND +
                    EE_BAND  + WALL_CONS + CWI + log(unitPrice), 
                  data=Panel_IMD_5, index = "Year", model = "random", na.action = "na.omit")
summary(regIMD5.re)

