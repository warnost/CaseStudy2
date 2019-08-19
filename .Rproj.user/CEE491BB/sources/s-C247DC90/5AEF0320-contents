library(ggthemes)
plotNumVars <- function(df){
  df %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_density() +
    geom_histogram() + 
    theme_fivethirtyeight()
}

plotAllNumeric(casedata)


ggplot(casedata,aes(BusinessTravel)) + geom_bar(aes(y = (..count..)/sum(..count..), fill = Attrition), position = "dodge")

x<-casedata %>% group_by(BusinessTravel) %>% summarise(count = n(), percentTotal = paste0(round(n()/dim(casedata)[1]*100,2),"%"))
kable(x)

df %>% group_by(x) %>% summarise( Count = n(), Proportion = n()/dim(df)[1]) %>% kable() %>% kable_styling(full_width = FALSE)

casedata %>% group_by_at("Attrition") %>% 
  summarise(Count = n(), Proportion = n()/dim(casedata)[1]) %>% 
  kable() %>% kable_styling(full_width = FALSE)


library(GGally);library(ggplot2)
ggpairs(data = casedata, 
              mapping = aes(color = Attrition),
              columns = c("HourlyRate","DailyRate","MonthlyRate","MonthlyIncome"))

ggpairs(data = casedata, 
        mapping = aes(color = Attrition),
        columns = c("YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager","MonthlyIncome"))




age.t <- t.test(casedata$Age~casedata$Attrition)
income.t <- t.test(casedata$MonthlyIncome~casedata$Attrition)
distance.t <- t.test(casedata$DistanceFromHome~casedata$Attrition)
numcomp.t <- t.test(casedata$NumCompaniesWorked~casedata$Attrition)
salhike.t <- t.test(casedata$PercentSalaryHike~casedata$Attrition)
workyears.t <- t.test(casedata$TotalWorkingYears~casedata$Attrition)
yearscomp.t <- t.test(casedata$YearsAtCompany~casedata$Attrition)
yearsrole.t <- t.test(casedata$YearsInCurrentRole~casedata$Attrition)
yearsprom.t <- t.test(casedata$YearsSinceLastPromotion~casedata$Attrition)
yearsmang.t <- t.test(casedata$YearsWithCurrManager~casedata$Attrition)

testnames <- c("Age","Monthly Income","Distance From Home","Companies Worked For",
               "Salary Hike","Total Work Years","Years at Company","Years in Role","Years Since Promotion","Years With Manager")
testpval <- c(age.t$p.value,
              income.t$p.value,
              distance.t$p.value,
              numcomp.t$p.value,
              salhike.t$p.value,
              workyears.t$p.value,
              yearscomp.t$p.value,
              yearsrole.t$p.value,
              yearsprom.t$p.value,
              yearsmang.t$p.value)

ttestout <- as.data.frame(cbind(testnames,testpval))
ttestout$testpval <- round(as.numeric(levels(ttestout$testpval)),5) 
names(ttestout) <- c("Variable","P-Value")
ttestout %>% kable()
