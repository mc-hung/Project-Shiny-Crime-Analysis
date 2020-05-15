library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(usmap)
library(wordcloud)
library(RColorBrewer)
library(shinyjs)
library(cowplot)
library(zoo)
library(plotly)
library(corrplot)

path <- file.path("www","data","crimedata.csv", fsep="/")
#Import Data / Clean Data
dataset <- read.csv(path,fileEncoding="latin1")
#keep only columns needed
dataset1 <- dataset[,c(1,2,5,6,7,8,9,10,11,12,13,14,15,17,18,25,27,28,29,30,31,
                       32,34,35,36,37,38,39,48,66,67,69,73,74,75,78,97,98,130,131,
                       132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147)]
#change to all numeric
for (i in c(41:56)){
  dataset1[,i] = data.frame(apply(dataset1[i], 2, as.numeric))}
dataset1[,"OtherPerCap"] <- as.numeric(dataset1[,"OtherPerCap"])

#change column name
names(dataset1)[1] <- "community"

#four regions
Northeast <- c("ME", "MA", "RI", "CT", "NH", "NY", "PA", "NJ")
South <- c("WV", "DC", "DE", "MD", "VA", "KY", "TN", "NC", "SC", "GA", "AL", "MS", "AR", "LA", "FL","TX","OK")
Midwest <- c("OH", "IN", "MI", "IL", "MO", "WI", "MN", "IA", "KS", "NE", "SD", "ND")
West <- c("CO", "WY", "MT", "ID", "WA", "OR", "UT", "NV", "CA", "AZ","NM", "AK", "HI")
#add region
dataset1 <- mutate(dataset1, Region =
                     ifelse(state %in% Northeast,"Northeast",
                            ifelse(state %in% South,"South",
                                   ifelse(state %in% Midwest,"Midwest", "West"))))
#add unique community
dataset1 <- mutate(dataset1, communityInState = paste(dataset1$community, "-", dataset1$state))

#missing value all to 0
dataset1[is.na(dataset1)] <- 0
dataset1[dataset1 == "?"] <- 0

##### ADD ALL SUBSET HERE, NAME IT AS SHOW IN APP!!! #####

############################# JX Subsets #############################
#education level tidy
sub_edu <- dataset1[,c("community", "PctLess9thGrade","PctNotHSGrad","PctBSorMore", "population", "state", "Region")]
names(sub_edu)[2] <- "<=9thGrade"
names(sub_edu)[3] <- "<=HighSchool"
names(sub_edu)[4] <- ">=BS"
sub_edu <- sub_edu %>%
  gather("<=9thGrade","<=HighSchool",">=BS",key="EducationLevel", value="PercentageOfPeople")

sub_edu <- mutate(sub_edu, NumOfPeople = PercentageOfPeople*population/100 )

edu_sub_1 <- 
  sub_edu %>%
  group_by(Region, EducationLevel) %>%
  summarize(population=sum(population),PopofGroup=sum(NumOfPeople))
edu_sub_1 <- mutate(edu_sub_1,percentage=PopofGroup/population)

#Population Aggregate
popbyregion <- aggregate(population ~ Region, dataset1, FUN=sum)
popbystates <- aggregate(population ~ state, dataset1, FUN=sum)

#Age group tidy
age_sub <- dataset1[,c("community","agePct12t21", "agePct12t29", "agePct16t24", "agePct65up","population","Region")]
names(age_sub)[2] <- "age12to21"
names(age_sub)[3] <- "age12to29"
names(age_sub)[4] <- "age16to24"
names(age_sub)[5] <- "age65up"
age_sub <- age_sub %>%
  gather("age12to21", "age12to29","age16to24","age65up",key="agegroup", value="percentage")
age_sub <- mutate(age_sub, PopofGroup = population * percentage / 100)
age_sub$PopofGroup <- round(age_sub$PopofGroup,0)

age_sub_1 <- 
  age_sub %>%
  group_by(Region, agegroup) %>%
  summarize(population=sum(population),PopofGroup=sum(PopofGroup))
age_sub_1 <- mutate(age_sub_1,percentage=PopofGroup/population)

#Employment status tidy
emp_sub <- dataset1[,c("community","Region", "PctUnemployed", "PctEmploy","state")]
names(emp_sub)[3] <- "Unemployed"
names(emp_sub)[4] <- "Employed"
emp_sub <- emp_sub %>%
  gather("Unemployed", "Employed",key="EmpCon", value="percentage")

#Language tidy
lan_sub <- dataset1[,c("community","Region", "PctSpeakEnglOnly", "PctNotSpeakEnglWell","state")]
names(lan_sub)[3] <- "EnglishOnly"
names(lan_sub)[4] <- "NotEnglish"
lan_sub <- lan_sub %>%
  gather("EnglishOnly", "NotEnglish",key="Language", value="percentage")

#Race tidy
race_sub <- dataset1[,c("community","Region", "racepctblack", "racePctWhite","racePctAsian","racePctHisp","state")]
names(race_sub)[3] <- "Black"
names(race_sub)[4] <- "White"
names(race_sub)[5] <- "Asian"
names(race_sub)[6] <- "Hispanic"
race_sub <- race_sub %>%
  gather("Black", "White","Asian","Hispanic",key="Race", value="percentage")

################## MH subset ##################

#all crimes Region
murder_state <- aggregate(murders ~ state, dataset1, FUN=sum)
rape_state <- aggregate(rapes ~ state, dataset1, FUN=sum)
assault_state <- aggregate(assaults ~ state, dataset1, FUN=sum)
burglary_state <- aggregate(burglaries ~ state, dataset1, FUN=sum)
larceny_state <- aggregate(larcenies ~ state, dataset1, FUN=sum)
autoTheft_state <- aggregate(autoTheft ~ state, dataset1, FUN=sum)
arson_state <- aggregate(arsons ~ state, dataset1, FUN=sum)
vio_state  <- aggregate(ViolentCrimesPerPop ~ state, dataset1, FUN=sum)
nonvio_state <- aggregate(nonViolPerPop ~ state, dataset1, FUN=sum)

#relational data - FULL_JOIN
crime_state <- full_join(murder_state, rape_state, by="state")
crime_state <- full_join(crime_state, assault_state, by="state")
crime_state <- full_join(crime_state, burglary_state, by="state")
crime_state <- full_join(crime_state, larceny_state, by="state")
crime_state <- full_join(crime_state, autoTheft_state, by="state")
crime_state <- full_join(crime_state, arson_state, by="state")
crime_state <- full_join(crime_state, vio_state, by="state")
crime_state <- full_join(crime_state, nonvio_state, by="state")

crime_state[9] <- crime_state[9] * 100000
crime_state[10] <- crime_state[10] * 100000
names(crime_state)[9] <- "Violent"
names(crime_state)[10] <- "NonViolent"

crime_state <- mutate(crime_state, Region =
                        ifelse(state %in% Northeast,"Northeast",
                               ifelse(state %in% South,"South",
                                      ifelse(state %in% Midwest,"Midwest", "West"))))
crime_state <- crime_state[,c(11,1,2,3,4,5,6,7,8,9,10)]

################# ZZ Subset ##########################3

factorconvert <- function(f){as.numeric(levels(f))[f]}
# dataset2 <- dataset1[,c("murders","rapes","robberies",
#                         "assaults","burglaries","larcenies","autoTheft","arsons",
#                         "ViolentCrimesPerPop","communityInState","Region")]
dataset2 <- dataset1[,c("murders","rapes","robberies",
                        "assaults","burglaries","larcenies","autoTheft","arsons",
                        "ViolentCrimesPerPop","Region")]
word1 <- aggregate(. ~ Region, dataset2,FUN=sum)
dataset3 <- dataset1[,c("murders","rapes","robberies",
                        "assaults","burglaries","larcenies","autoTheft","arsons",
                        "ViolentCrimesPerPop","state")]
word2 <- aggregate(. ~ state, dataset3,FUN=sum)

################## XL Subset ############################
#Median Income: tidy crime 
med_sub <- dataset1[,c("community","Region", "medIncome", "arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies", "population")]

med_sub <- med_sub %>%
  gather("arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies",key="Crime", value="Cases")

#Number In Shelters: tidy crime
num_sub <- dataset1[,c("community","Region", "NumInShelters","arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies", "population")]

num_sub <- num_sub %>%
  gather("arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies",key="Crime", value="Cases")

#Poverty: tidy crime
pov_sub <- dataset1[,c("community","Region", "PctPopUnderPov", "arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies", "population")]

pov_sub <- pov_sub %>%
  gather("arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies",key="Crime", value="Cases")

pov_sub <- mutate(pov_sub, NumberOfPeople = population * PctPopUnderPov / 100)
pov_sub$NumberOfPeople <- round(pov_sub$NumberOfPeople,0)

#PctPersOwnOccup: tidy crime
ooc_sub <- dataset1[,c("community","Region", "PctPersOwnOccup", "arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies", "population")]

ooc_sub <- ooc_sub %>%
  gather("arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies",key="Crime", value="Cases")


#NumStreet: tidy crime
nus_sub <- dataset1[,c("community","Region", "NumStreet", "arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies", "population")]

nus_sub <- nus_sub %>%
  gather("arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies",key="Crime", value="Cases")


#PctPersDenseHous: tidy crime
dens_sub <- dataset1[,c("community","Region", "PctPersDenseHous", "arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies", "population")]

dens_sub <- dens_sub %>%
  gather("arsons","assaults","autoTheft","burglaries","murders","larcenies","rapes", "robberies",key="Crime", value="Cases")

dens_sub <- mutate(dens_sub, NumberOfPeople = population * PctPersDenseHous / 100)
dens_sub$NumberOfPeople <- round(dens_sub$NumberOfPeople,0)

################# Correlation Subset #####################
race_cri <- dataset1[,c(6:9,39,41,43,45,47,49,51,53,55,56)]
age_cri <- dataset1[,c(10:13,39,41,43,45,47,49,51,53,55,56)]
emp_cri <- dataset1[,c(27,28,39,41,43,45,47,49,51,53,55,56)]
edu_cri <- dataset1[,c(24:26,39,41,43,45,47,49,51,53,55,56)]

################## Data Cleaning for Linear Regression ################

#clean data
dataset_a <- dataset[,-c(1:5)]
dataset_a[dataset_a == "?"] <- NA

#find columns with NA

dataset4 <- dataset_a[,c("NumUnderPov","PctLess9thGrade","PctUnemployed","NumInShelters",
                        "PctBornSameState","rapesPerPop","robbbPerPop","assaultPerPop",
                        "ViolentCrimesPerPop")] 

for (i in 1:length(dataset4)){
  dataset4[,i] = data.frame(apply(dataset4[i], 2, as.numeric))}

na.aggregate(dataset4)
# replace NA with mean
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
replace(dataset4, TRUE, lapply(dataset4, NA2mean))
dataset4[] <- lapply(dataset4, NA2mean)
# 80-20 model, normalized data
set.seed(11)
train_index <- sample(1:nrow(dataset4), 0.8*nrow(dataset4))
normalize <- function(x){
  return((x-min(x))/(max(x) - min(x)))
}
d4_norm <- as.data.frame(lapply(dataset4, normalize))
train_lm <- d4_norm[train_index,]
test_lm <- d4_norm[-train_index,]

lm_fit <- lm(ViolentCrimesPerPop ~ ., data = train_lm)
sm <- summary(lm_fit)
sse <- sum(sm$residuals^2)
mse <- mean(sm$residuals^2)

# write new csv
df.path1 <-file.path("www/data/table1.cvs")
df.tb1 <- data.frame(NumUnderPov=as.numeric(),PctLess9thGrade=as.numeric(),
                     PctUnemployed=as.numeric(),NumInShelters=as.numeric(),
                     PctBornSameState=as.numeric(),rapesPerPop=as.numeric(),
                     robbbPerPop=as.numeric(),assaultPerPop=as.numeric())
write.csv(df.tb1,df.path1)

df.path2 <-file.path("www/data/table2.cvs")
df.tb2 <- data.frame(ViolentCrimesPerPop=as.numeric())
write.csv(df.tb2,df.path2)

################## ADD ALL Functions HERE #########################

################### JX Functions Start ##########################
ageplot <- function(df) {
  if (df=="TotalNumbyAge"){
    ggplot(age_sub_1,aes(x=Region,y=PopofGroup,fill=agegroup))+
      geom_bar(stat = "identity",position=position_dodge())+
      scale_fill_brewer(palette="Paired")+
      ggtitle("Number of People by Age Group")
  }
  else{
    ggplot(age_sub_1,aes(x=Region,y=percentage,fill=agegroup))+
      geom_bar(stat = "identity",position=position_dodge())+
      scale_fill_brewer(palette="Paired")+
      ggtitle("Percentage of People by Age Group")
  }
}

eduplot <- function(df) {
  if (df=="TotalNumberByEdu"){
    ggplot(edu_sub_1,aes(x=Region,y=PopofGroup,fill=EducationLevel))+
      geom_bar(stat = "identity",position=position_dodge())+
      scale_fill_brewer(palette="Paired")+
      ggtitle("Number of People by Education Level")
  }
  else if(df=="EducationLevelByStates"){
    sub_edu %>%
      group_by(EducationLevel, state) %>%
      summarize(sumx = sum(NumOfPeople)) %>%
      ggplot(aes(x = state, y = sumx, fill = EducationLevel)) +
      scale_fill_brewer(palette="Paired")+
      geom_col() +
      labs(title = "State vs Number Of People By Educational Level",
           x = "State", y = "Number of People") + 
      theme(axis.text.x = element_text(angle=75, vjust = 0.7, size = 7 ))
  }
  else{
    ggplot(edu_sub_1,aes(x=Region,y=percentage,fill=EducationLevel))+
      geom_bar(stat = "identity",position=position_dodge())+
      scale_fill_brewer(palette="Paired")+
      ggtitle("Percentage of People By Education Level")
  }
}

download <- function(df){
  if(df=="Age Group"){age_sub}
  else if(df=="Population"){popbystates}
  else if(df=="Education Level"){sub_edu}
  else if(df=="Employement Status"){emp_sub}
  else if(df=="Race"){race_sub}
  else{lan_sub}
}

################### MH Function #########################
#Function to graph Crime Map
crime_map <- function(df,region, crime) {
  
  if (identical(region, c(West, South, Northeast, Midwest))){
    plot_usmap(data=crime_state, values = crime, color = "red", labels = TRUE)+
      scale_fill_continuous(low = "white", high = "red")+
      theme(legend.position = "right")
  } else if (length(region) == 0){
    plot_usmap(regions = "states", labels = TRUE)+
      labs(title = "No region is selected", subtitle ="All states are showed with blank")
  }else {
    plot_usmap(
      data = crime_state, values = crime, include = region, color = "red", labels=TRUE) + 
      scale_fill_continuous(
        low = "white", high = "red", name = "Number of murders", label = scales::comma) + 
      theme(legend.position = "right")
  }
}

stLt <- function(st){
  reg <- c()
  for (i in length(st)){
    if ("Midwest" %in% st){
      reg <- c(reg, Midwest)
    } else reg
    if ("Northeast" %in% st){
      reg <- c(reg, Northeast)
    } else reg
    if ("West" %in% st){
      reg <- c(reg, West)
    } else reg
    if ("South" %in% st){
      reg <- c(reg, South)
    } else reg
  }
  return(reg)
}

########################  ZZ  Function ####################
graphselect <- function(df=word1,gtype,crime){
  
  if(gtype == "bar"){
    # Basic barplot
    ggplot(word1, aes(x=Region,y=word1[,crime], fill=Region)) +
      geom_bar(stat="identity")+
      geom_text(aes(label=word1[,crime]), vjust=1.6, color="white", size=3.5)+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = "Region", y = "Crime Total")
  }
  else if(gtype == "line"){
    # Basic lineplot
    ggplot(word1, aes(x=Region,y=word1[,crime], group=1)) +
      geom_line(color="#b05454")+
      geom_point()+
      labs(x = "Region", y = "Crime Total")
  }
  else{df=word2
  wordcloud(words = word2$state,freq = word2[,crime],scale = c(7,1),
            min.freq = 1,max.words = 3500,random.order = FALSE,
            rot.per = 0.35,colors = brewer.pal(8,"Dark2"))
  
  }
}

dataselect <- function(df,gtype){
  
  if(gtype == "bar"){
    # Basic barplot
    df=word1
  }
  else if(gtype == "line"){
    # Basic lineplot
    df=word1
  }
  else{df=word2
  df=word1
  
  }
}

################## APP START HERE ############################
#Define UI
ui <- fluidPage(themeSelector(),
  
#Navbar structur for UI
  navbarPage(strong("Crime Analysis in US"),
             
################## JX Page #######################

    navbarMenu("Geographical", icon = icon("globe-americas"),
           tabPanel("People", fluid = TRUE, icon = icon("fas fa-users"),
                    titlePanel("People in the U.S."),
                    #sidebar layout with input / output 
                    sidebarLayout(
                      #sidebarPanel(the LEFT)
                      sidebarPanel(
                        conditionalPanel(
                          condition = "input.tabselected==1",
                          headerPanel("Population Distribution"),
                          selectInput(
                            inputId = "poplist1",label = "Select Highest Color",
                            choices = c("red","sandybrown","olivedrab","tomato","violet","slateblue","sienna","yellow"),
                            selectize = FALSE), #select1.1 ends here
                          selectInput(
                            inputId = "poplist2",label = "Select Lowest Color",
                            choices = c("snow","thistle1","seashell","white","linen","mintcream","ivory"),
                            selectize = FALSE), #select1.2 ends here
                          helpText("The four regions:",
                                   br(),
                                   " Northeast: ME, MA, RI, CT, NH, NY, PA, NJ",
                                   br(),
                                   " South: WV, DC, DE, MD, VA, KY, TN, NC, SC, GA, AL, MS, AR, LA, FL,TX,OK",
                                   br(),
                                   " Midwest: OH, IN, MI, IL, MO, WI, MN, IA, KS, NE, SD, ND",
                                   br(),
                                   " West: CO, WY, MT, ID, WA, OR, UT, NV, CA, AZ,NM, AK, HI"),
                          helpText("There is no data for MT, NE, HI states !"),
                          actionButton(
                            inputId = "clear",label = "Reset Data",icon = icon("refresh"),
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.tabselected==2",
                          headerPanel("Age Group Distribution"),
                          selectInput(
                            inputId = "poplist3",label = "Select Age Group Variables",
                            choices = c( "TotalNumbyAge","percentage"),
                          ), #select2.1 ends here
                          helpText("There are four age groups:",
                                   br(),
                                   "1. Age between 12 and 21",
                                   br(),
                                   "2. Age between 12 and 29",
                                   br(),
                                   "3. Age between 16 and 24",
                                   br(),
                                   "4. Age up to 65"),
                        ),

                        conditionalPanel(
                          condition = "input.tabselected==3",
                          headerPanel("Education Level Distribution"),
                          selectInput(
                            inputId = "poplist5",label = "Select Different Education Levels",
                            choices = c("TotalNumberByEdu","PercentageByEdu","EducationLevelByStates"),
                            selectize = FALSE), #select3.1 ends here
                          helpText("There are three education levels:",
                                   br(),
                                   "1. Less 9th grade",
                                   br(),
                                   "2. Not high school grade",
                                   br(),
                                   "3. Bachelor's degree or more"),
                          actionButton(
                            inputId = "reset_edu",label = "Reset Data",icon = icon("refresh"),
                          )
                        ),
                        conditionalPanel(
                          condition = "input.tabselected==4",
                          headerPanel("Employment Status"),
                          img(src="https://www.pageuppeople.com/wp-content/uploads/2019/01/Top-60-Employee-Engagement-image43-1024x510.png",
                              height="150px"),
                          selectInput(
                            inputId = "poplist7",label = "Select Employment Status",
                            choices = c(choose = "List of data frame...",
                                        "Employed","Unemployed"),
                            selectize = FALSE), #select4.1 ends here
                          helpText("There are two employement status:",
                                   br(),
                                   "1. Percentage of Employed",
                                   br(),
                                   "2. Percentage of Unemployed"),
                          helpText("There is no data for MT, NE, HI states !"),
                        ),
                        conditionalPanel(
                          condition = "input.tabselected==5",
                          headerPanel("English Speaking Status"),
                          img(src="https://s3-ap-southeast-1.amazonaws.com/images.humanresourcesonline.net/wp-content/uploads/2018/07/Aditi-Jul-2018-communication-languages-istock.jpg",
                              height="150px"),
                          br(),
                          selectInput(
                            inputId = "poplist9",label = "Select Languages Status",
                            choices = c(choose = "List of data frame...",
                                        "EnglishOnly","NotEnglish"),
                            selectize = FALSE), #select5.1 ends here
                          helpText("There are two English speaking status:",
                                   br(),
                                   "1. Speak English Only",
                                   br(),
                                   "2. Not Speak English Well"),
                          helpText("There is no data for MT, NE, HI states !"),
                        ),
                        conditionalPanel(
                          condition = "input.tabselected==6",
                          headerPanel("Race Distribution"),
                          img(src="https://www.statnews.com/wp-content/uploads/2016/02/RaceAndGenetics_APStock5654561-1600x900.jpg",
                              height="150px"),
                          selectInput(
                            width = "100%",
                            inputId = "poplist11",label = "Select Races",
                            choices = c(choose = "race",
                                        "Black","White","Asian","Hispanic"),
                            selectize = FALSE), #select6.1 ends here
                          helpText("There are four races:",
                                   br(),
                                   "1. Black Americans",
                                   br(),
                                   "2. White Americans",
                                   br(),
                                   "3. Asian Americans",
                                   br(),
                                   "4. Hispanic Americans"),
                          helpText("There is no data for MT, NE, HI states !"),
                        ),
                        conditionalPanel(
                          condition = "input.tabselected==7",
                          headerPanel("Data Download"),
                          selectInput(
                            width = "100%",
                            inputId = "data_download",label = "Select Dataset",
                            choices = c("Population","Age Group","Education Level","Employment Status","Language","Race"),
                            selectize = FALSE)),#select7.1 ends here
                        
                        width = 4
                        
                      ),#end of sidebarPanel
                      
                      #mainPanel(the RIGHT)
                      mainPanel(
                        tabsetPanel(
                          tabPanel(
                            title = strong("Population"),value = 1, icon = icon("fas fa-angle-right"),
                            column(6,offset=9,img(src="https://image.freepik.com/free-vector/business-people-organization-office-freelance-job-character_40876-1291.jpg",
                                                  height="150px")),
                            withSpinner(plotOutput(outputId = "popmap"
                            )), 
                            withSpinner(tableOutput("poptable"))),
                          
                          tabPanel(
                            title = strong("Age Group"), value = 2,icon = icon("fas fa-angle-right"),
                            column(6,offset=8,img(src="https://inspirationalperspective.com/wp-content/uploads/2018/01/Coming-of-Age.jpg",
                                                  height="160px")),
                            withSpinner(plotOutput("age"))
                          ),
                          tabPanel(
                            title = strong("Education level"), value = 3,icon = icon("fas fa-angle-right"),
                            column(6,offset=8,img(src="https://static01.nyt.com/images/2018/04/08/opinion/sunday/08levy/08levy-superJumbo.jpg?quality=90&auto=webp",
                                                  height="160px")),
                            withSpinner(plotOutput("edu"))
                          ),
                          tabPanel(
                            title = strong("Employment Status"),value = 4,icon = icon("fas fa-angle-right"),
                            withSpinner(plotOutput("empmap")),
                            plotOutput("emptotal",width = "70%", height = "350px")
                          ),
                          tabPanel(
                            title = strong("Language"),value = 5,icon = icon("fas fa-angle-right"),
                            withSpinner(plotOutput("lanmap")),
                            plotOutput("lantotal",width = "70%", height = "350px")
                          ),
                          tabPanel(
                            title = strong("Race"), value = 6,icon = icon("fas fa-angle-right"),
                            withSpinner(plotOutput("racemap")),
                            textOutput("racetype")
                          ),
                          tabPanel(
                            title = strong("Data Download"), value = 7,icon = icon("fas fa-angle-right"),
                            downloadButton("down",label = "Download"),
                            DT::dataTableOutput("Dataselected")
                          ),
                          id="tabselected"
                        )
                      )#end of mainPanel
                    ),#end of sidebarLayout
           ),#end of 1st tabPanel
                        
######################### JX Page End Here ##################################                      
######################### MH Page Start Here ############################                       
                        
                        tabPanel("Crime", fluid = TRUE, icon = icon("flushed"),
                                 titlePanel("Crime in the Region(s)"),
                                 sidebarLayout(
                                   #sidebarPanel(the LEFT)
                                   sidebarPanel(width = 4,
                                     
                                     #selection
                                     h3("",
                                        img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcRudu3YOEykOCRZwfENHKZEPIr3in9oT6cpFX2fXOft1kwJ-1NZ&usqp=CAU",
                                            height = 150, width = 250)),
                                     checkboxGroupInput(inputId = "Region",
                                                        label = "Select Region(s)",
                                                        choices = c("West", "Midwest",
                                                                    "South", "Northeast"),
                                                        selected = c("West", "Midwest",
                                                                     "South", "Northeast")),
                                     selectInput(inputId = "crime",
                                                 label = "Select Crime Type",
                                                 choices = c("Select", "murders",
                                                             "rapes", "assaults", "burglaries",
                                                             "larcenies", "autoTheft", "arsons",
                                                             "Violent", "NonViolent"),
                                                 width = "100%"),
                                     sliderInput(
                                       inputId = "bins",
                                       label = "Number of bins:",
                                       min = 1,
                                       max = 50,
                                       value = 30
                                     ),
                                     
                                     helpText("Region List:"),
                                     helpText("West: CO, WY, MT, ID, WA, OR, UT, 
                                              NV, CA, AZ, NM, AK, HI"),
                                     helpText("Midwest:OH, IN, MI, IL, MO, WI, MN, IA, KS, NE, SD, ND"),
                                     helpText("Northeast: ME, MA, RI, CT, NH, NY, PA, NJ"),
                                     helpText("South: WV, DC, DE, MD, VA, KY, TN, NC, SC, 
                                                        GA, AL, MS, AR, LA, FL, TX, OK"),
                                     plotlyOutput("PlotUS", height = 230, width = "100%"),
                                     actionButton(
                                       inputId = "reset1",
                                       label = "Reset/Clear",
                                       icon = icon("refresh"),
                                       width = "100%"
                                     )
                                   ),#end of sidebarPanel
                                   #mainPanel(the RIGHT)
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel(strong("Map & Summary"),icon = icon("fas fa-angle-right"), fluidRow(
                                         column(8,
                                                plotOutput("Map_crime",
                                                          width = "100%",
                                                          height = "500px"),
                                                textOutput("aaa")),#end of column1
                                         column(4,
                                                textOutput("ccc"),
                                                verbatimTextOutput("bbb"))#end of column2
                                       )#end of fluidRow
                                                
                                       ),#end of tab1
                                       
                                       tabPanel(strong("Histogram"), icon = icon("fas fa-angle-right"), fluid = TRUE,
                                                plotOutput("histo")),
                                       
                                       tabPanel(strong("Data"), icon = icon("fas fa-angle-right"), fluid = TRUE,
                                                downloadButton("download_crime_reg",
                                                               label = "Download Data"),
                                                DT::dataTableOutput("dataSet")
                                                
                                       )#end of tab2
                                     )
                                     
                                   ))#end of mainPanel
                        )#end of 2nd tabPanel
             
            ),#end of first navbarMenu
######################### MH Page End Here ##################################
######################### ZX Page Start Here #################################
            navbarMenu("Analytical", icon = icon("chart-bar"),
                       
                       tabPanel("Region & Crime", fluid = TRUE, icon = icon("fort-awesome-alt"),
                                titlePanel(strong("Region & Crime")),
                                sidebarLayout(
                                  
                                  sidebarPanel(
                                    conditionalPanel(
                                      condition = "input.tabselected2==1",
                                      headerPanel("Graph Analysis"),
                                      
                                      selectInput(inputId = "crimeselect",
                                                  label = "Select Crime Type",
                                                  choices = c(choose = "select",
                                                              "murders","rapes","robberies",
                                                              "assaults","burglaries",
                                                              "larcenies","autoTheft",
                                                              "arsons",
                                                              "ViolentCrimesPerPop"),
                                                  width = "220px"),
                                      selectInput(inputId = "chartselect",
                                                  label = "Select graph type",
                                                  choices = c("wordcloud","bar","line"),
                                                  width = "220px"),
                                      h3("",
                                         img(src = "https://cdn.clipart.email/4e2ea75ff9e31681b67ad5a1b792c330_graph-clipart-transparent_1920-1080.png",
                                             width = "100%", height = 150))
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected2==2",
                                      headerPanel("Dataset Check"),
                                      
                                      selectInput(inputId = "crimeselect2",
                                                  label = "Select crime type",
                                                  choices = c("murders","rapes","robberies",
                                                              "assaults","burglaries",
                                                              "larcenies","autoTheft",
                                                              "arsons",
                                                              "ViolentCrimesPerPop"),
                                                  width = "220px"),
                                      selectInput(inputId = "raemselect",
                                                  label = "Select race age education or employment",
                                                  choices = c("PctLess9thGrade","PctNotHSGrad",
                                                              "PctBSorMore","PctPopUnderPov",
                                                              "PctEmploy","PctUnemployed",
                                                              "racepctblack","racePctWhite",
                                                              "racePctAsian","racePctHisp",
                                                              "agePct12t21","agePct12t29",
                                                              "agePct16t24","agePct65up"),
                                                  width = "220px"),
                                      h3("",
                                         img(src ="img/ZZ5.png",
                                             width = "100%", height = 200))
                                      
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected2==3",
                                      headerPanel("Full Dataset"),
                                      h3("",img(src="https://styles.redditmedia.com/t5_2r97t/styles/communityIcon_ri05w19k4zh01.png",
                                                height=200))
                                      
                                    )
                                  ),
                                  
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel(tags$b("Graph Analysis"),value = 1 , icon = icon("fas fa-angle-right"),
                                               plotOutput("plot")),
                                      tabPanel(tags$b("Dataset Check"),value = 2, icon = icon("fas fa-angle-right"),
                                               plotOutput("plota", brush = "user_brush"),
                                               dataTableOutput("table")),
                                      tabPanel(tags$b("Full Dataset"),value = 3, icon = icon("fas fa-angle-right"),
                                               # tags$blockquote("R is Great", cite = "R Programmer"),
                                               downloadButton("download_dataset1",
                                                              label = "Download Data"),
                                               dataTableOutput("table2")),
                                      id="tabselected2"
                                    )
                                  )
                      )), #end of tabPanel1

######################### ZZ Page End Here #################################
######################### XL Page Start Here ###################################
                       
                       #second choice under this icon
                       tabPanel("People & Crime", fluid = TRUE, icon = icon("user-secret"),
                                
                                #sidebar layout with input / output
                                titlePanel("Crimes by Sociological Factors"),                          
                                sidebarLayout(
                                  #sidebarPanel(the LEFT)
                                  sidebarPanel(
                                    conditionalPanel(
                                      condition = "input.tabselected3==1",
                                      headerPanel("Density House"),
                                      selectInput(
                                        inputId = "pop1",label = "View crime type",
                                        choices = c(" ","arsons","assaults","autoTheft","burglaries",
                                                    "murders","larcenies","rapes", "robberies"),
                                      ), #select1.1 ends here
                                      verbatimTextOutput("info1"),
                                      actionButton(
                                        inputId = "reset",label = "Reset Data",icon = icon("refresh"),
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected3==2",
                                      headerPanel("Median income"),
                                      selectInput(
                                        inputId = "pop2",label = "View crime type",
                                        choices = c(" ","arsons","assaults","autoTheft","burglaries",
                                                    "murders","larcenies","rapes", "robberies"),
                                      ), #select2.1 ends here
                                      verbatimTextOutput("info2"),
                                      actionButton(
                                        inputId = "reset2",label = "Reset Data",icon = icon("refresh"),
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected3==3",
                                      headerPanel("Number in shelters"),
                                      selectInput(
                                        inputId = "pop3",label = "View crime type",
                                        choices = c(" ","arsons","assaults","autoTheft","burglaries",
                                                    "murders","larcenies","rapes", "robberies"),
                                      ), #select3.1 ends here
                                      verbatimTextOutput("info3"),
                                      actionButton(
                                        inputId = "reset3",label = "Reset Data",icon = icon("refresh"),
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected3==4",
                                      headerPanel("Number Street"),
                                      selectInput(
                                        inputId = "pop4",label = "View crime type",
                                        choices = c(" ","arsons","assaults","autoTheft","burglaries",
                                                    "murders","larcenies","rapes", "robberies"),
                                      ), #select4.1 ends here
                                      verbatimTextOutput("info4"),
                                      actionButton(
                                        inputId = "reset4",label = "Reset Data",icon = icon("refresh"),
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected3==5",
                                      headerPanel("Own Occupy"),
                                      selectInput(
                                        inputId = "pop5",label = "View crime type",
                                        choices = c(" ","arsons","assaults","autoTheft","burglaries",
                                                    "murders","larcenies","rapes", "robberies"),
                                      ), #select5.1 ends here
                                      verbatimTextOutput("info5"),
                                      actionButton(
                                        inputId = "reset5",label = "Reset Data",icon = icon("refresh"),
                                      )
                                    ),
                                    conditionalPanel(
                                      condition = "input.tabselected3==6",
                                      headerPanel("Poverty"),
                                      selectInput(
                                        inputId = "pop6",label = "View crime type",
                                        choices = c(" ","arsons","assaults","autoTheft","burglaries",
                                                    "murders","larcenies","rapes", "robberies"),
                                      ), #select6.1 ends here
                                      verbatimTextOutput("info6"),
                                      actionButton(
                                        inputId = "reset6",label = "Reset Data",icon = icon("refresh"),
                                      )
                                    ),
                                    
                                    width = 3
                                    
                                  ),#end of sidebarPanel
                                  
                                  #mainPanel(the RIGHT)
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel(
                                        title = strong("Density House"),value = 1, icon = icon("fas fa-angle-right"),
                                        plotOutput("deScatter", hover = hoverOpts(id = "plot_hover1")),
                                        DT::dataTableOutput("dataSet1")
                                      ),
                                      tabPanel(
                                        title = strong("Median Income"), value = 2, icon = icon("fas fa-angle-right"),
                                        plotOutput("medInScatter", hover = hoverOpts(id = "plot_hover2")),
                                        DT::dataTableOutput("dataSet2")
                                        
                                      ),
                                      tabPanel(
                                        title = strong("Number in shelters"), value = 3, icon = icon("fas fa-angle-right"),
                                        plotOutput("numInScatter", hover = hoverOpts(id = "plot_hover3")),
                                        DT::dataTableOutput("dataSet3")
                                      ),
                                      tabPanel(
                                        title = strong("Number Street"),value = 4, icon = icon("fas fa-angle-right"),
                                        plotOutput("nuScatter", hover = hoverOpts(id = "plot_hover4")),
                                        DT::dataTableOutput("dataSet4")
                                        
                                      ),
                                      tabPanel(
                                        title = strong("Own Occupy"),value = 5, icon = icon("fas fa-angle-right"),
                                        plotOutput("ocScatter", hover = hoverOpts(id = "plot_hover5")),
                                        DT::dataTableOutput("dataSet5")
                                      ),
                                      tabPanel(
                                        title = strong("Poverty"), value = 6, icon = icon("fas fa-angle-right"),
                                        plotOutput("povScatter", hover = hoverOpts(id = "plot_hover6")),
                                        DT::dataTableOutput("dataSet6")
                                        
                                      ),
                                      
                                      id="tabselected3"
                                    )
                                  )#end of mainPanel
                                ),#end of sidebarLayout
                                
                                
                                #can add more under this icon
                       ),#end of tabPanel2  

######################### XL Page End Here ###################################
######################### Correlation (MH) Start ########################################

                       tabPanel("Correlation", fluid = TRUE, icon = icon("braille"),
                                titlePanel("Correlation Matrix"),
                                sidebarLayout(
                                  sidebarPanel(width = 3,
                                    selectInput(inputId = "Corr",
                                                label = "Relationshiop between Crimes &",
                                                choices = c("Select", "Age Group",
                                                            "Education Level", 
                                                            "Race", "Employment"),
                                                width = "100%")
                                  ),
                                  mainPanel(width = 9,fluidRow(
                                    column(6,
                                           h4(strong("The Correlation Matrix Plot")),
                                           plotOutput("Corr_Plot", width = "100%")
                                           ),#end of column1
                                    column(6,
                                           h4(strong("The Heatmap")),
                                           plotOutput("Heat_Plot", width = "100%")
                                           )#end of column2
                                  )#end of fluidRow
                                  )#end of mainPanel
                                )#end of sidebarLayout
                                #can add more under this icon
                       )#end of tabPanel3
            ),#end of 1st navbarMenu

######################### Machine Learning (MH) Start ####################################
                        
                        #first choice under this icon
                        tabPanel("Machine Learning Model", fluid = TRUE,
                                 icon = icon("lightbulb"),
                                 titlePanel("Multi-Linear Regression Model Prediction"),
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                fluidRow(column(6,
                                                                numericInput(
                                                                  width = 100,
                                                                  inputId = "colA",
                                                                  label = "Number\nUnder\nPoverty",
                                                                  value = NA,
                                                                  min = 0),
                                                                numericInput(
                                                                  width = 100,
                                                                  inputId = "colB",
                                                                  label = "Percentage\nLess9thGrade",
                                                                  value = NA,
                                                                  min = 0,
                                                                  max = 100),
                                                                numericInput(
                                                                  width = 100,
                                                                  inputId = "colC",
                                                                  label = "Percentage\nUnemployed",
                                                                  value = NA,
                                                                  min = 0,
                                                                  max = 100),
                                                                numericInput(
                                                                  width = 100,
                                                                  inputId = "colD",
                                                                  label = "Number\nInShelters",
                                                                  value = NA,
                                                                  min = 0)
                                                ),
                                                column(6,
                                                       numericInput(
                                                         width = 100,
                                                         inputId = "colI",
                                                         label = "Percentage\nBornSameState",
                                                         value = NA,
                                                         min = 0,
                                                         max = 100),
                                                       numericInput(
                                                         width = 100,
                                                         inputId = "colJ",
                                                         label = "Rapes\nPer100k",
                                                         value = NA,
                                                         min = 0),
                                                       numericInput(
                                                         width = 100,
                                                         inputId = "colK",
                                                         label = "Robbery\nPer100k",
                                                         value = NA,
                                                         min = 0),
                                                       numericInput(
                                                         width = 100,
                                                         inputId = "colL",
                                                         label = "Assault\nPer100k",
                                                         value = NA,
                                                         min = 0)
                                                )),
                                                actionButton(
                                                  inputId = "submitt",
                                                  label = "Submit/Update",
                                                  icon = icon("database"),
                                                  width = "100%"),
                                                actionButton(
                                                  inputId="recall",
                                                  label="recall",
                                                  icon=icon("refresh"),
                                                  width="100%")
                                   ), #end siderbarPanel
                                   mainPanel(
                                     h3(textOutput("Tablename")),
                                     DT::dataTableOutput("DTSet"),
                                     h3(textOutput("PT")),
                                     DT::dataTableOutput("Prediction")
                                     
                                   )#end mainPanel
                                 )
                        ), #end of tabPanel
             
######################## Information Tab (MH) Start #########################

                        #About the Project
                        tabPanel("Project Info", fluid = TRUE, icon = icon("r-project"),
                                 h1(strong("About this project")),
                                 h3(strong("Introduction")),
                                 h4(p("The main purpose of this project is to 
                                   visualize and explore the relationship between 
                                   different types of crime and any of different 
                                   variables. The dataset is based on Communities 
                                   and Crime Data Set from UCI machine learning 
                                   repository and is combined with socio-economic 
                                   data from 1990 US Census, law enforcement data 
                                   from the 1990 US LEMAS survey, and crime data from 
                                   the 1995 FBI UCR. The combined data set is from Analyzing 
                                   UCI Crime and Communities Dataset(by Kavitha from Kaggle).")),
                                 br(),
                                 br(),
                                 h3(strong("Method")),
                                 h4(p("There are total of 147 attributes and 2200+ rows in the 
                                   dataset. After the data cleaning, the number of attributes 
                                   is reduced to 55 attributes, and all the missing/NA values 
                                   are change to 0 because there are only few of them. If all 
                                   the missing/NA values are omitted, some of the states will 
                                   be removed from the dataset. There are several data processing 
                                   methods are used, includes tidy data, data transformation, 
                                   data wrangle and relational data. Several functions are set 
                                   for visualization. Visualization for this project includes 
                                   Map plot, Bar plot, Scatter plot, Word Cloud, and Correlation 
                                   for both crime and other type of variables.")),
                                 br(),
                                 br(),
                                 h3(strong("Source")),
                                 
                                 h4(p("Communities and Crime Data Set, UCI machine learning repository",
                                      a("This is the Link", 
                                        href = "https://archive.ics.uci.edu/ml/datasets/Communities+and+Crime"))),
                                 h4(p("Analyzing UCI Crime and Communities Dataset, Kaggle, Author: Kavitha",
                                      a("This is the Link",
                                        href = "https://www.kaggle.com/kkanda/analyzing-uci-crime-and-communities-dataset/data")))
                                 ),#end of tabPanel
                        #second choice - Team
                        tabPanel("Team Info", fluid = TRUE, icon = icon("thumbs-up"),
                                 fluidRow(
                                   column(6,
                                          h3(img(src = "https://staticx.ibncollege.com/wcsstore/ExtendedSitesCatalogAssetStore/992_100_10_29713/images/LARGEIMAGE_1517589.jpg",
                                             height = 180, style="display: block; margin-left: auto; margin-right: auto;")),
                                          
                                          h4(strong("Team Members:")),
                                          h4(p("Jiamei Xu, Mei-Chun Hung, Xinye Li, ZhiCheng Zhu")),
                                          br(),
                                          h4(strong("Course:")), 
                                          h4(p("IE6600 Computation and Visualization for Analytics")),
                                          br(),
                                          h4(strong("Instructor:")), 
                                          h4(p("Zhenyuan Lu")),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          br(),
                                          h5("Image from:",
                                             a("NEU Image",
                                               href = "https://staticx.ibncollege.com/wcsstore/ExtendedSitesCatalogAssetStore/992_100_10_29713/images/LARGEIMAGE_1517589.jpg"),
                                             a("Shiny Image",
                                               href = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png"),
                                             a("RStudio Image",
                                               href = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png"),
                                             a("ThankYou Image",
                                               href = "https://www.littleoverlodge.co.uk/wp-content/uploads/2018/12/hotel-derby-thank-you-2018-600x400.jpg"))
                                          ),#end of column1
                                   column(6,
                                          h2("Built with",
                                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "50px"),
                                             "by",
                                             img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "50px"),
                                             "."),
                                          h3(img(src = "https://www.littleoverlodge.co.uk/wp-content/uploads/2018/12/hotel-derby-thank-you-2018-600x400.jpg",
                                                 height = 200, width = "100%")),
                                          h4(p("April, 2020")))#end of column2
                                 ))#end of tabPanel
  )#end of navbarPage

)#end of ui fluidPage


#Define server
server <- function(input, output, session) {
  
  values <- reactiveValues(
    tbl = NULL,
    reg = NULL,
    cri = NULL,
    #bins = NULL,
    area = NULL,
    df.plot = NULL,
    graph = NULL,
    Region = NULL,
    col1 = NULL,
    col2 = NULL,
    pop_1 = NULL,
    pop_2 = NULL,
    pop_3 = NULL,
    pop_4 = NULL,
    pop_5 = NULL,
    pop_6 = NULL,
    dataInput = NULL,
    df1 = NULL
  )
################### Jiamei Xu Server Start HERE ##############################
  
  ##Population tab
  observeEvent(input$poplist1,{
    values$col1 <- input$poplist1
  })
  observeEvent(input$poplist2,{
    values$col2 <- input$poplist2
    
  output$popmap <- renderPlot(
    plot_usmap(data=popbystates, values = "population", color = "black", labels = TRUE)+
      scale_fill_continuous(low = values$col2, high = values$col1, name = "Population")+
      theme(legend.position = "right"))
  })

  output$poptable <- renderTable(popbyregion)
  
  ##Age tab
  output$age <- renderPlot(
    ageplot(input$poplist3)
  )
  
  ##Education Tab
  output$edu <- renderPlot(
    eduplot(input$poplist5)
  )
  
  ##Employ tab
  output$empmap <- renderPlot(
    plot_usmap(data=filter(emp_sub,EmpCon==input$poplist7),
               values = "percentage", color = "black", labels = TRUE)+
      scale_fill_continuous(low = "white", high = "red", name = "Percentage")+
      theme(legend.position = "right")
  )
  output$emptotal <- renderPlot(
    ggplot(emp_sub, aes(x = Region, y = percentage, fill = EmpCon)) +
      geom_boxplot(alpha=0.7) +
      scale_y_continuous(name = "Percentage") +
      ggtitle("Boxplot of Employ Condition In US Different Regions")
  )
  
  ##Language Tab
  output$lanmap <- renderPlot(
    plot_usmap(data=filter(lan_sub,Language==input$poplist9),
               values = "percentage", color = "black", labels = TRUE)+
      scale_fill_continuous(low = "white", high = "red", name = "Percentage of Language")+
      theme(legend.position = "right")
  )
  output$lantotal <- renderPlot(
    ggplot(lan_sub, aes(x = Region, y = percentage, fill = Language)) +
      geom_boxplot(alpha=0.7) +
      scale_y_continuous(name = "Percentage") +
      ggtitle("Boxplot of English Speaking Status In US Different Regions")
  )
  
  ##Race Tab  
  output$racemap <- renderPlot(
    plot_usmap(data=filter(race_sub,Race==input$poplist11), 
               values = "percentage", color = "black", labels = TRUE)+
      scale_fill_continuous(low = "white", high = "red",name="Percentage")+
      theme(legend.position = "right")
  )
  
  output$racetype <- renderText(
    paste("This map shows the percentage of",input$poplist11,"Americans in US."))
  
  output$popnum <- renderText(
    "The total test population in US is 99801791.")
  
  ##Download Data Tab
  
  output$down <- downloadHandler(
    filename = "FILENAME.csv",
    content = function(file){
      write.csv(download(input$data_download), file, row.names = FALSE)
    }
  )
  output$Dataselected <- DT::renderDT({
    download(input$data_download)
    
  })#end of output$dataset
  
  ##Reset Button
  observeEvent(input$clear, {
    values$col1 <- "Grey"
    values$col2 <- "White"
  })
  
################### Mei-Chun Hung server start ##########################
  observeEvent(input$Region, {
    values$reg <- stLt(input$Region)
    values$tbl <- crime_state %>% 
      filter(state %in% values$reg)
    tb <- crime_state %>%
      filter(state %in% values$reg)  
    
    output$download_crime_reg <- downloadHandler(
      filename = function(){
        paste("data-", values$reg, ".csv", sep="")
      },
      content = function(file){
        write.csv(tb, file, row.names = FALSE)
      })#end of output$download_crime_reg
  })
  
  observeEvent(input$crime,{
    values$reg <- stLt(input$Region)
    #values$tbl <- crime_state %>% 
      #filter(state %in% values$reg)
    values$cri <- input$crime
    
    require(tidyverse)
    require(plotly)
    output$PlotUS <- renderPlotly({
      if (values$cri != c("Select")){
        df1 <- as.data.frame(crime_state)
        df1$hover <- with(df1, paste(state, "<br>", values$cri, df1[,values$cri]))
        bound <- list(color = toRGB("Black"), width = 2)
        opt <- list(scope = 'usa',
                  projection = list(type = 'albers usa'),
                  showlakes = TRUE,
                  lakecolor = toRGB("White"))
      
      
        plot_geo(df1, locationmode = 'USA-states') %>% 
          add_trace(z= ~df1[,values$cri], text = ~hover, locations = ~state,
                                 color = ~df1[,values$cri], colors = "Blues")%>% 
          colorbar(title = "Total") %>% 
          layout(title = "Number of crime", geo = opt)} 
    })
    
    output$Map_crime <- renderPlot({
      if (values$cri != c("Select")){
        crime_map(values$tbl, values$reg, values$cri)} 
    })#end of output$Map_crime
    
    output$ccc <- renderText({
      paste("Summary:")
    })#end of renderText
    
    output$bbb <- renderPrint({
      if (values$cri != c("Select")){
      summary(crime_state[crime_state$state %in% values$reg,][values$cri])}
    })#end of renderPrint
    
    output$histo <- renderPlot({
      bins <- input$bins
      if (values$cri != c("Select")){
        ggplot(values$tbl, aes_string(x=values$cri))+
          geom_histogram(bins=bins, color="darkblue", fill="lightblue")+
          labs(title = "Histogram of Crime count by States")}
    })

    
    output$dataSet <- DT::renderDataTable({
      crime_state %>%
        filter(state %in% values$reg)    
      },
      extensions = c('Scroller', 'FixedColumns'),
      options = list(
        deferRender = TRUE,
        scrollX = TRUE,
        scrollY = 200,
        scroller = TRUE,
        dom = 'Bfrtip',
        fixedColumns = TRUE))#end of output$dataset
  })

  output$aaa <- renderText({
    c("Special Note: MT, NE & HI have no data")
  })#end of renderText
  
  observeEvent(input$reset1, {
    values$tbl <- NULL
    values$reg <- NULL
    #values$cri <- "Select"
    output$dataSet <- NULL
  })#end of observeEvent reset1

################## ZhiCheng Zhu Server Start #######################
  
  observeEvent(input$crimeselect, {
    if(input$crimeselect != c("select")){

      output$plot <- renderPlot({
        crime <- input$crimeselect
        gtype <- input$chartselect
        graphselect(word1,gtype,crime)
        #output$table <- DT::renderDataTable(word1)
      })}
  })# observeEvent end here
  
  observe({
    
    output$plota <- renderPlot({
      
      ggplot(dataset1, aes_string(input$raemselect,input$crimeselect2,
                                  shape="Region", color="Region")) + 
        geom_point() +
        ylim(0, 100)
    })
    diam <- reactive({
      
      user_brush <- input$user_brush
      sel <- brushedPoints(dataset1, user_brush)
      return(sel)
      
    })
    
    output$download_dataset1 <- downloadHandler(
      filename = function(){
        paste("fulldataset.csv")
      },
      content = function(file){
        write.csv(dataset1, file, row.names = FALSE)
      })
    
    output$table <- DT::renderDataTable(
      DT::datatable(diam())
      )
    output$table2 <- DT::renderDataTable(
      dataset2
      )
  })
  
################## Xinye Li Server Start ###########################
  
  observeEvent(input$pop1, {
    values$pop_1 <- input$pop1
    if(input$pop1 != " "){
      
      output$deScatter <- renderPlot(
        ggplot(data=filter(dens_sub,Crime==values$pop_1),
               aes(x = log(PctPersDenseHous), y = log(Cases))) +
          geom_point(color= "tomato") + 
          labs(x = "log(PctPersDenseHous)", y = "Cases"))#end of renderPlot
      
      output$info1  <- renderText({
        
        xy1 <- function(a){
          if(is.null(a)) return("NULL\n")
          paste0("x:", round(a$x, 4), " y:", round(a$y,4))
        }
        paste(xy1(input$plot_hover1))
        })
      }
    }
  )
  
  observeEvent(input$pop2, {
    values$pop_2 <- input$pop2
    if(input$pop2 != " "){
      
      output$medInScatter <- renderPlot(
        ggplot(data=filter(med_sub,Crime==values$pop_2),
               aes(x = log(medIncome), y = log(Cases))) +
          geom_point(color= "violetred3") + 
          labs(x = "log(medIncome)", y = "Cases"))
      
      output$info2  <- renderText({
        
        xy2 <- function(b){
          if(is.null(b)) return("NULL\n")
          paste0("x:", round(b$x, 4), " y:", round(b$y,4))
        }
        paste(xy2(input$plot_hover2))
      })
      
      }
  })
  
  observeEvent(input$pop3, {
    values$pop_3 <- input$pop3
    if(input$pop3 != " "){
      output$numInScatter <- renderPlot(
        ggplot(data=filter(num_sub,Crime==values$pop_3),
               aes(x = log(NumInShelters), y = log(Cases))) +
          geom_point(color= "steelblue") + 
          labs(x = "log(NumInShelters)", y = "Cases"))
      
      output$info3  <- renderText({
        
        xy3 <- function(c){
          if(is.null(c)) return("NULL\n")
          paste0("x:", round(c$x, 4), " y:", round(c$y,4))
        }
        paste(xy3(input$plot_hover3))
      })
      
      }
  })
  
  observeEvent(input$pop4, {  
    values$pop_4 <- input$pop4
    if(input$pop4 != " "){
      output$nuScatter <- renderPlot(
        ggplot(data=filter(nus_sub,Crime==values$pop_4),
               aes(x = log(NumStreet), y = log(Cases))) +
          geom_point(color= "darkseagreen") + 
          labs(x = "log(NumStreet)", y = "Cases"))
      
      output$info4  <- renderText({
        
        xy4 <- function(d){
          if(is.null(d)) return("NULL\n")
          paste0("x:", round(d$x, 4), " y:", round(d$y,4))
        }
        paste(xy4(input$plot_hover4))
      })
      
      }
  })
  
  observeEvent(input$pop5, {
    values$pop_5 <- input$pop5
    if(input$pop5 != " "){
      output$ocScatter <- renderPlot(
        ggplot(data=filter(ooc_sub,Crime==values$pop_5),
               aes(x = log(PctPersOwnOccup), y = log(Cases))) +
          geom_point(color= "burlywood4") + 
          labs(x = "log(PctPersOwnOccup)", y = "Cases"))
      
      output$info5 <- renderText({
        
        xy5 <- function(f){
          if(is.null(f)) return("NULL\n")
          paste0("x:", round(f$x, 4), " y:", round(f$y,4))
        }
        paste(xy5(input$plot_hover5))
      })
      
      }
  })
  
  observeEvent(input$pop6, {  
    values$pop_6 <- input$pop6
    if(input$pop6 != " "){
      output$povScatter <- renderPlot(
        ggplot(data=filter(pov_sub,Crime==values$pop_6),
               aes(x = log(NumberOfPeople), y = log(Cases))) +
          geom_point(color= "darkmagenta") + 
          labs(x = "log(NumberOfPeople)", y = "Cases"))
      
      output$info6  <- renderText({
        
        xy6 <- function(g){
          if(is.null(g)) return("NULL\n")
          paste0("x:", round(g$x, 4), " y:", round(g$y,4))
        }
        paste(xy6(input$plot_hover6))
      })
      
      }
  })
  
  output$dataSet1 <- DT::renderDT({
    dens_sub %>%
      filter(Crime==values$pop_1)
  })
  
  output$dataSet2 <- DT::renderDT({
    med_sub %>%
      filter(Crime==values$pop_2)
  })
  
  output$dataSet3 <- DT::renderDT({
    num_sub %>%
      filter(Crime==values$pop_3)
  })
  
  output$dataSet4 <- DT::renderDT({
    nus_sub %>%
      filter(Crime==values$pop_4)
  })
  
  output$dataSet5 <- DT::renderDT({
    ooc_sub %>%
      filter(Crime==values$pop_5)
  })
  
  output$dataSet6 <- DT::renderDT({
    pov_sub %>%
      filter(Crime==values$pop_6)
  })
  
  observeEvent(input$reset, {
    values$pop_1 <- " "})
  
  observeEvent(input$reset2, {
    values$pop_2 <- " "})
  
  observeEvent(input$reset3, {
    values$pop_3 <- " "})
  
  observeEvent(input$reset4, {
    values$pop_4 <- " "})
  
  observeEvent(input$reset5, {
    values$pop_5 <- " "})
  
  observeEvent(input$reset6, {
    values$pop_6 <- " "})
  
################## Correlation Server (MH) Start  ############################
  
  output$Corr_Plot <- renderPlot({
    
    rc.cor <- cor(race_cri)
    ac.cor <- cor(age_cri)
    emc.cor <- cor(emp_cri)
    edc.cor <- cor(edu_cri)
    
    if (input$Corr == "Age Group"){
      corrplot(ac.cor)
    } else if (input$Corr == "Race"){
      corrplot(rc.cor)
    } else if (input$Corr == "Education Level"){
      corrplot(edc.cor)
    } else if (input$Corr == "Employment"){
      corrplot(emc.cor)
    } else paste("Please select group")
    
  })#end of renderPlot
  
  output$Heat_Plot <- renderPlot({
    
    rc.cor <- cor(race_cri)
    ac.cor <- cor(age_cri)
    emc.cor <- cor(emp_cri)
    edc.cor <- cor(edu_cri)
    
    if (input$Corr == "Age Group"){
      palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(20)
      heatmap(x = ac.cor, col = palette, symm = TRUE)
    } else if (input$Corr == "Race"){
      palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(20)
      heatmap(x = rc.cor, col = palette, symm = TRUE)
    } else if (input$Corr == "Education Level"){
      palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(20)
      heatmap(x = edc.cor, col = palette, symm = TRUE)
    } else if (input$Corr == "Employment"){
      palette <- colorRampPalette(brewer.pal(9, "YlOrRd"))(20)
      heatmap(x = emc.cor, col = palette, symm = TRUE)
    } else print("Please select")
    
  })#end of renderPlot
  
################## ML Server (MH) Start ###############################
  
  observeEvent(input$submitt,{
    values$dataInput <- data.frame(NumUnderPov=input$colA,
                                   PctLess9thGrade=input$colB,
                                   PctUnemployed=input$colC,
                                   NumInShelters=input$colD,
                                   PctBornSameState=input$colI,
                                   rapesPerPop=input$colJ,
                                   robbbPerPop=input$colK,
                                   assaultPerPop=input$colL) #df end
    values$df1 <- as.data.frame(read.csv(df.path1)[-1])
    if(!"TRUE"%in%is.na(values$dataInput)){
      values$new1 <- na.omit(unique(rbind(values$df1,values$dataInput)))
      write.csv(values$new1, df.path1)
    
    pdc_tb <- read.csv("www/data/table1.cvs")
    #newrow <- pdc_tb[dim(pdc_tb)[1],]
    
    y_h <- predict(lm_fit, pdc_tb)
    values$yhat <- data.frame(ViolentCrimesPerPop=y_h)
    
    #print(values$yhat)
    
    values$df2 <- as.data.frame(read.csv(df.path2)[-1])
      values$new2 <- na.omit(unique(rbind(values$yhat)))
      print(values$new2)
      write.csv(values$new2, df.path2)
      }
    
  })#end observeEvent
  
  output$Tablename <- renderText(
    paste("Inserted Data")
    )
  
  output$DTSet <- DT::renderDataTable(
    values$new1
    )
  
  output$PT <- renderText(
    paste("Predict Number of Violent Crime per 100K People")
    )
  
  output$Prediction <- DT::renderDataTable(
    values$new2
    )
  
  observeEvent(input$recall, {
      if (dim(values$new1)[1] > 1) {
    
        values$new1<- values$new1[-dim(values$new1)[1],]
        write.csv(values$new1, df.path1)
    
        pdc_tb <- read.csv("www/data/table1.cvs")
        y_h <- predict(lm_fit, pdc_tb)
        values$yhat <- data.frame(ViolentCrimesPerPop=y_h)
        values$df2 <- as.data.frame(read.csv(df.path2)[-1])
        values$new2 <- na.omit(unique(rbind(values$yhat)))
        #print(values$new2)
        write.csv(values$new2, df.path2)}
  })#end of observeEvent
  
}#end of server

#Run the Application

shinyApp(ui=ui, server=server)





