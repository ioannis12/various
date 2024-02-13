#Set Libraries
#library(plyr)
library(dplyr)
library(googlesheets4)
source("SupportFunctions.R")

df <- range_speedread('1xFO2G6CIRPSEtGvSaTjg406yR6JuZS8v4RpwDJaYJ0c', 
                      sheet = 'NCAAB', na = c("NA", "missing"))
#Global Default values
StartDate = min(as.Date(df$Date))
EndDate = max(as.Date(df$Date))
initialMu = 25
initialSigma = initialMu / 3
Beta = 13
Tau = initialSigma / 100
DrawProbability = 0
DrawMargin = InverseCumulative((DrawProbability+1)/2) * sqrt(2) * Beta

#initialize empty table for fight predictions
CompletePredictions = data.frame(matrix(vector(), 0, 15,
              dimnames=list(c(), c("Event", "Organization", "Date", "FightCount", "Fighter1", "Fighter2", "Result1",
                                   "Result2", "mu1","sigma1","mu2","sigma2", "FightPrediction", "NewMu", "NewSigma"))),
                stringsAsFactors=F)

#Generate Current Ratings
CurrentRatings <- data.frame(unique(df$Team1))
names(CurrentRatings)[1]="Name"
CurrentRatings$mu <- initialMu
CurrentRatings$sigma <- initialSigma

#Set analysis date
today <- StartDate

while (today < EndDate) {

#Subset data for this date
  TodaysGames <- df[df$Date == today,]

#TodaysFights <- ddply(TodaysFights, .(Fighter1), function(X) data.frame(X, FightCount=1:nrow(X)))
#TodaysFights$FightCount <- ave(TodaysFights$Date, TodaysFights$Fighter1,  FUN = seq_along)


  if (nrow(TodaysGames)==0) {
    today <- today+1
    next
  }

#Incremental counter for multiple fighters
TodaysFights <- (TodaysFights %>% group_by(Fighter1) %>% mutate(FightCount = 1:n()))

dates <- unique(as.Date(df$Date))
#Loop for fight counter
for (i in 1:length(dates)) {
  print(dates[i])}
  
  #Split by counter
  PartialTodaysGames <- TodaysGames[TodaysGames$Date ==dates[i],]
  
  #Get current fighter ratings
  TodaysFighterRatings <- CurrentRatings[CurrentRatings$Name %in% PartialTodaysFights$Fighter1 | CurrentRatings$Name %in% PartialTodaysFights$Fighter2,]
  PartialTodaysFights <- merge(PartialTodaysFights, TodaysFighterRatings, by.x=c("Fighter1"), by.y=c("Name"))
  names(PartialTodaysFights)[9]="mu1"
  names(PartialTodaysFights)[10]="sigma1"
  PartialTodaysFights <- merge(PartialTodaysFights,TodaysFighterRatings, by.x=c("Fighter2"), by.y=c("Name"))
  names(PartialTodaysFights)[11]="mu2"
  names(PartialTodaysFights)[12]="sigma2"
  
  #Calculate result prediction
  PartialTodaysFights$FightPrediction <- sapply(1:nrow(PartialTodaysFights), function(n) predictOutcome(PartialTodaysFights$mu1[n], PartialTodaysFights$mu2[n], PartialTodaysFights$sigma1[n], PartialTodaysFights$sigma2[n]))
  
  #Update ratings
  PartialTodaysFights$NewMu <- sapply(1:nrow(PartialTodaysFights), function(n) {
    GetNewMu(PartialTodaysFights$mu1[n], 
             PartialTodaysFights$mu2[n], 
             PartialTodaysFights$sigma1[n], 
             PartialTodaysFights$sigma2[n], 
             PartialTodaysFights$Result1[n])})
  PartialTodaysFights$NewSigma <- sapply(1:nrow(PartialTodaysFights), function(n) {
    GetNewSigma(PartialTodaysFights$mu1[n], 
             PartialTodaysFights$mu2[n], 
             PartialTodaysFights$sigma1[n], 
             PartialTodaysFights$sigma2[n], 
             PartialTodaysFights$Result1[n])})
  
  #Store predictions
  CompletePredictions <- rbind(CompletePredictions, PartialTodaysFights)
  
  #Update Ratings
  UpdatedRatings <- PartialTodaysFights[,c("Fighter1","NewMu","NewSigma")]
  names(UpdatedRatings)[1]="Name"
  names(UpdatedRatings)[2]="mu"
  names(UpdatedRatings)[3]="sigma"
  CurrentRatings <- CurrentRatings[!(CurrentRatings$Name %in% UpdatedRatings$Name),]
  CurrentRatings <- rbind(CurrentRatings, UpdatedRatings)
}
today <- today+1
print(today)
}

write.csv(CompletePredictions, file = "CompletePredictions.csv")

