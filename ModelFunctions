#functions to take daily forecasted volumes and handled times to workload
#hours, staff hours required, and FTE required
fteHrsPerDay = 7.75
fteHrsPerWeek = 38.75
#Create & Populate Year from Date
require(zoo)
#zoo must be loaded, date must be defined
yearFromDate = function(xDate){
  year = format(xDate, "%Y")
  return(year)
}
#return month from formatted date
monthFromDate = function(xDate){
  month = format(xDate, "%b")
  return(month)
}
#return day of week(DOW) from date
dow = function(xDate){
  x = weekdays(as.Date(xDate))
  return(x)
}
#return the date of Monday from the current week with Monday as the first
#day of the week.
weekof = function(xDate){
  x = xDate
  y = 0
  while(dow(x-y)!="Monday"){
    y = y+1;
  }
  return(x-y)
}
#calculate workload hours from call volume and AHT in seconds
wkldHrs = function(cv, aht){
  x = cv*aht/60/60
  return(x)
}
#calculate staff hours required
staffHrsReq = function(wkldHrs, RSF){
  x = wkldHrs*RSF
  return(x)
}
#Calculate FTE Required
fteReq = function(staffHrsReq){
  x = staffHrsReq/fteHrsPerDay
  return(x)
}
#Calculate Presence Factor
presenceFactor = function(yourTime, holiday, otherPaid, fteHrsPerMonth){
  x = 1 - ((yourTime/(fteHrsPerWeek*2)) + (holiday/fteHrsPerMonth) + (otherPaid/fteHrsPerMonth))
  return(x)
}
#calculate Scheduled Rate
scheduledRate = function(presenceFactor, breaks, aux7, meeting, train, coach,
                         assistance, specProj, corpEvent, sysIssue,fteHrsPerMonth){
  a = breaks/(fteHrsPerDay*60)*presenceFactor
  b = aux7/(fteHrsPerDay*60)*presenceFactor
  c = meeting/fteHrsPerMonth*presenceFactor
  d = train/fteHrsPerMonth*presenceFactor
  e = coach/fteHrsPerMonth*presenceFactor
  f = assistance/fteHrsPerMonth*presenceFactor
  g = specProj/fteHrsPerMonth*presenceFactor
  h = corpEvent/fteHrsPerMonth*presenceFactor
  i = sysIssue/fteHrsPerMonth*presenceFactor
  x = presenceFactor - (a+b+c+d+e+f+g+h+i)
  return(x)
}
#Calculate Manned Percentage
mannedPct = function(scheduledRate, nonConf){
  x = scheduledRate - (nonConf/(fteHrsPerDay*60))
  return(x)
}
#Calculate Availability
availability = function(occupancy, mannedPct){
  x = (1-occupancy)*mannedPct
  return(x)
}
#Calculate Random Factors
randFactors = function(nonConf, availability){
  x = (nonConf/(fteHrsPerDay*60))+availability
  return(x)
}
#Calculate Design Factor
designFactor = function(scheduledRate, randFactors){
  x = scheduledRate - randFactors
  return(x)
}
#Calculate Total non-Available
totalNonAvail = function(designFactor){
  x = 1 - designFactor
  return(x)
}
nonAvail = totalNonAvail(df)
#Calculate Rostered Staff Factor(RSF)
RSF = function(designFactor){
  x = 1/designFactor
  return(x)
}
