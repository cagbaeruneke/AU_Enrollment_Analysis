#####################################################################################
## DESCRIPTION: Join data for project analysis 
#####################################################################################

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

#-Read in data-
lead <- read_csv("./../data/lead.csv")
spend <- read_csv("./../data/spend.csv")
opportunity <- read_csv("./../data/opportunity.csv")

edLevel.c <- read_csv("./../data/ed_level.csv")
state.c <- read_csv("./../data/states.csv")

#-Show columns in the data-
names(lead)
names(spend)

#-SELECT a subset of variables-
df1 <- lead %>% 
  select(email, leadsource, state, status, lead.cat.brd)

#-DROP a subset of variables-
df2 <- df1 %>% 
  select(-status)

#-CREATE a new variable-
df3 <- df2 %>% 
  mutate(web_lead = ifelse(lead.cat.brd=="Web", 1, 0))

#-FILTER to a subset of rows-
df4 <- df3 %>% 
  filter(state == "Virginia")

#-Create SUMMARY METRICS BY GROUP-
df5 <- df3 %>% 
  group_by(state) %>% 
  summarise(total_web = sum(web_lead, na.rm = TRUE)) %>% 
  arrange(desc(total_web))

print(df5)

#-JOIN DATA SOURCES--
merged <- lead %>% 
  select(email, leadsource, state, lead.cat.brd) %>% 
  mutate(web_lead = ifelse(lead.cat.brd=="Web", 1, 0)) %>% 
  left_join(spend)

rm(df1, df2, df3, df4, df5, merged)
##--------------------------------------------------------------##
## PREPARE PROJECT DATA ----------------------------------------##-----------------------------
##--------------------------------------------------------------##

#NOTE <- FIGURE OUT HOW TO SHOW THE FLAW HERE
## MERGE DATA  -------------------------------------------------##
full_data <- lead %>% 
  left_join(spend) %>% 
  left_join(opportunity)

test <- full_data %>% 
  select(name, application_last_login__c)

rm(test)

## MERGE DATA  -------------------------------------------------##
full_data <- lead %>% 
  left_join(spend) %>% 
  left_join(opportunity, by=c("convertedopportunityid" = "id"))

test <- full_data %>% 
  select(name, application_last_login__c)

rm(test)

## Keep a subset of variables ----------------------------------##
full_data <- full_data %>% 
  select(id, email, gender__c, lead.cat.brd, lead.cat.nrw, leadsource, cost, 
         createddate, application_start_date__c, application_submit_date__c, 
         application_complete_date__c, admitted_date__c, enrolled_date__c,
         state, domestic_international__c, ga_browser__c, industry, 
         prospect_score__c, stated_gpa_range__c, age__c, cumulative_graduate_gpa__c, 
         cumulative_undergraduate_gpa__c, gre_quantitative_score__c, 
         gre_verbal_score__c, status, level_of_education__c)


##--------------------------------------------------------------##
## Create New Variables ----------------------------------------##
##--------------------------------------------------------------##

#-Binary (0/1) for major funnel stages---------------------
full_data <- full_data %>% 
  mutate(app.start = ifelse(is.na(application_start_date__c),0,1),
         app.submit = ifelse(is.na(application_submit_date__c),0,1),
         app.complete = ifelse(is.na(application_complete_date__c),0,1),
         app.admit = ifelse(is.na(admitted_date__c),0,1),
         app.enroll = ifelse(is.na(enrolled_date__c),0,1))

#-Days between create date and major stage date------------
#---(standardize date formats)
full_data <- full_data %>% 
  mutate(create.date=as.Date(createddate),
         start.date = as.Date(application_start_date__c),
         submit.date = as.Date(application_submit_date__c),
         complete.date = as.Date(application_complete_date__c),
         admit.date = as.Date(admitted_date__c),
         enroll.date = as.Date(enrolled_date__c)) %>% 
  #--(create Intervals)
  mutate(days.to.start = difftime(start.date, create.date, units=c("days")),
         days.to.submit = difftime(submit.date, create.date, units=c("days")),
         days.to.complete = difftime(complete.date, create.date, units=c("days")),
         days.to.admit = difftime(admit.date, create.date, units=c("days")),
         days.to.enroll = difftime(enroll.date, create.date, units=c("days"))) %>%
  #--(drop old date variables)
  select(-createddate, -application_start_date__c, -application_submit_date__c, -application_complete_date__c, 
         -admitted_date__c, -enrolled_date__c) 

##-Rename Variables ---------------------------------------
full_data <- full_data %>% 
  rename(lead.status = status,
         domest.intl = domestic_international__c,
         lead.cost = cost,
         ed.level = level_of_education__c,
         prospect.score = prospect_score__c,
         undergrad.gpa = cumulative_undergraduate_gpa__c,
         grad.gpa = cumulative_graduate_gpa__c,
         age = age__c,
         browser.type = ga_browser__c,
         gender = gender__c,
         stated.gpa = stated_gpa_range__c,
         gre.quant = gre_quantitative_score__c,
         gre.verbal = gre_verbal_score__c)

##--------------------------------------------------------------##
## Clean-Up state and ed.level ---------------------------------##
##--------------------------------------------------------------##
full_data <- full_data %>% 
  left_join(edLevel.c) %>% 
  left_join(state.c)


##--------------------------------------------------------------##
## Write CSV ---------------------------------------------------##
##--------------------------------------------------------------##
write_csv(full_data, "~/Desktop/ITEC660/full_data.csv", na="")
