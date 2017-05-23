library(shiny)
library("shinythemes")
library("ggplot2")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
ui <- fluidPage(
  theme=shinytheme("darkly"),
  titlePanel("Bike Sharing Demand"),
  navbarPage("Bike Sharing",
      tabPanel("Regression Analysis",
               sidebarLayout(
                 sidebarPanel(
                   dateInput("date","Date:",value=Sys.Date()-10,format="mm/dd/yyyy"),
                   sliderInput("hour","Hour:",min=0,max=12,value=c(2,8),pre="$"),
                   radioButtons("r1","Season:",choices=c("Srping","Summer","Fall","Winter"),selected="Fall"),
                   radioButtons("r2","Weather:",choices=c("Clear","Misty","Light snow/Rain","Thunderstorm"),selected="Misty"),
                   radioButtons("r3","Holiday:",choices=c("Yes","No"),selected="No"),
                   radioButtons("r4","Working Day:",choices=c("Yes","No"),selected="Yes"),
                   numericInput("temp","Temperature:",12.67,min=0,max=50),
                   numericInput("atemp","ATemp:",10.89,min=1,max=50),
                   numericInput("humidity","Humidity:",80,min=1,max=120),
                   numericInput("windspeed","Windspeed:",10.34,min=0,max=40)
                   
                 ),
                 mainPanel("Various features from the training data are taken into account using which linear regression analysis is performed for the input data. Due to this the estimated count and accuracy plot will be taking some time to load.. Please wait for a minute or two to see the results.",
                   #"Based on the training data, we generated two models that predict number of registered and casual users based on the input values and the resulting count of the bike sharing users for different ntrees (decision trees in randomForest) is displayed below:",
                   textOutput("txt"),
                   br(),
                   tableOutput("tbl"),
                   br(),
                   textOutput("txt1"),
                   br(),
                   textOutput("txt2"),
                   br(),
                   plotOutput("coolplot"),
                   br(),
                   textOutput("txt3"),
                   br(),
                   plotOutput("coolplot1"),
                   br(),
                   textOutput("txt4"),
                   br(),
                   plotOutput("coolplot2")
                   
                 )
               )
          ),
            tabPanel("Regression Plots",
                     mainPanel(
                              textOutput("txt7"),
                              br(),
                              plotOutput("plt1"),
                              br(),
                              textOutput("txt8"),
                              br(),
                              plotOutput("plt2")
                      )
            )
        
  )
  
  )
server <- function(input,output) {
  
 # s <- reactiveValues(ntree=data.frame(),count=data.frame())
  
  output$txt <- renderPrint ({
    print("We generated two models that predict number of registered and casual users based on the input values and the training data. The resulting count of the bike sharing users for different ntrees (decision trees in randomForest) is displayed below:")
  })
  
  prediction <- reactive({
    test<-data.frame(input$date,input$hour[1],input$r1,input$r3,input$r4,input$r2,input$temp,input$atemp,input$humidity,input$windspeed, stringsAsFactors=FALSE)
  
    train1 <- read.csv("train.csv")
    train1$hour <- substr(train1$datetime,12,13)
    train1$hour <- as.factor(train1$hour)
    train1$datetime <- substr(train1$datetime,1,10)
    train <- data.frame(train1$datetime,train1$hour,train1$season,train1$holiday,train1$workingday,train1$weather,train1$temp,train1$atemp,train1$humidity,train1$windspeed,train1$casual,train1$registered,train1$count)
    
    
    colnames(train) <- c("date","hour","season","holiday","workingday","weather","temp","atemp","humidity","windspeed","casual","registered","count")
    
    colnames(test) <- c("date","hour","season","holiday","workingday","weather","temp","atemp","humidity","windspeed")

    #test$season <- as.factor(test$season)        
    test$season[test$season=="Spring"] <- 1
    test$season[test$season=="Summer"] <- 2
    test$season[test$season=="Fall"] <- 3
    test$season[test$season=="Winter"] <- 4

    
    test$holiday[test$holiday=="Yes"] <- 0
    test$holiday[test$holiday=="No"] <- 1
    #test$holiday <- as.factor(test$holiday)
    
    test$workingday[test$workingday=="Yes"] <- 0
    test$workingday[test$workingday=="No"] <- 1
    #test$workingday <- as.factor(test$workingday)
    
    test$weather[test$weather=="Clear"] <- 1
    test$weather[test$weather=="Misty"] <- 2
    test$weather[test$weather=="Light snow/Rain"] <- 3
    test$weather[test$weather=="Thunderstorm"] <- 4
    #test$weather <- as.factor(test$weather)
    
    
    test$date <- as.factor(test$date)
    test$hour <- as.factor(test$hour)
    
    test$season <- as.integer(test$season)
    test$holiday <- as.integer(test$holiday)
    test$workingday <- as.integer(test$workingday)
    test$weather <- as.integer(test$weather)
    
    #print(train)
    #print(test)
    
    test$registered <- as.integer(0)
    test$casual <- as.integer(0)
    test$count <- as.integer(0)
    
    #str(train)
    #str(test)
    
    data <- rbind(train,test)
    #print(data)
    
    data$season=as.factor(data$season)
    data$weather=as.factor(data$weather)
    data$holiday=as.factor(data$holiday)
    data$workingday=as.factor(data$workingday)
    
    date <- data$date
    days <- weekdays(as.Date(date))
    data$day <- days
    
    #print(data)
    train <- data[as.integer(substr(data$date,9,10))<20,]
    #print("Train")
    #print(train)
    test <- data[as.integer(substr(data$date,9,10))>19,]
    #print("Test")
    #print(test)
    
    data$year <- substr(data$date,1,4)
    data$year <- as.factor(data$year)
    train <- data[as.integer(substr(data$date,9,10))<20,]
    test <- data[as.integer(substr(data$date,9,10))>19,]
    
    data$hour <- as.integer(data$hour)
    data$day_part <- 0
    train=data[as.integer(substr(data$date,9,10))<20,]
    test=data[as.integer(substr(data$date,9,10))>19,]
    
    data=rbind(train,test)
    data$dp_reg=0
    data$dp_reg[data$hour<8]=1
    data$dp_reg[data$hour>=22]=2
    data$dp_reg[data$hour>9 & data$hour<18]=3
    data$dp_reg[data$hour==8]=4
    data$dp_reg[data$hour==9]=5
    data$dp_reg[data$hour==20 | data$hour==21]=6
    data$dp_reg[data$hour==19 | data$hour==18]=7
    
    data$dp_cas=0
    data$dp_cas[data$hour<=8]=1
    data$dp_cas[data$hour==9]=2
    data$dp_cas[data$hour>=10 & data$hour<=19]=3
    data$dp_cas[data$hour>19]=4
    
    data$temp_reg=0
    data$temp_reg[data$temp<13]=1
    data$temp_reg[data$temp>=13 & data$temp<23]=2
    data$temp_reg[data$temp>=23 & data$temp<30]=3
    data$temp_reg[data$temp>=30]=4
    
    
    
    data$temp_cas=0
    data$temp_cas[data$temp<15]=1
    data$temp_cas[data$temp>=15 & data$temp<23]=2
    data$temp_cas[data$temp>=23 & data$temp<30]=3
    data$temp_cas[data$temp>=30]=4
    
    
    
    data$year_part[data$year=='2011']=1
    data$year_part[data$year=='2011' & data$month>3]=2
    data$year_part[data$year=='2011' & data$month>6]=3
    data$year_part[data$year=='2011' & data$month>9]=4
    data$year_part[data$year=='2012']=5
    data$year_part[data$year=='2012' & data$month>3]=6
    data$year_part[data$year=='2012' & data$month>6]=7
    data$year_part[data$year=='2012' & data$month>9]=8
    
    data$day_type=0
    
    data$day_type[data$holiday==0 & data$workingday==0]="weekend"
    data$day_type[data$holiday==1]="holiday"
    data$day_type[data$holiday==0 & data$workingday==1]="working day"
    
    
    train=data[as.integer(substr(data$date,9,10))<20,]
    test=data[as.integer(substr(data$date,9,10))>19,]
    
    
    data=rbind(train,test)
    data$month=substr(data$date,6,7)
    
    data$month=as.integer(data$month)
    
    data$weekend=0
    data$weekend[data$day=="Sunday" | data$day=="Saturday"]=1
    
    data$season=as.factor(data$season)
    data$holiday=as.factor(data$holiday)
    data$workingday=as.factor(data$workingday)
    data$weather=as.factor(data$weather)
    data$hour=as.factor(data$hour)
    data$month=as.factor(data$month)
    data$day_part=as.factor(data$dp_cas)
    data$day_type=as.factor(data$dp_reg)
    data$day=as.factor(data$day)
    data$temp_cas=as.factor(data$temp_cas)
    data$temp_reg=as.factor(data$temp_reg)
    
    train=data[as.integer(substr(data$date,9,10))<20,]
    test=data[as.integer(substr(data$date,9,10))>19,]
    
    train$reg1=train$registered+1
    train$cas1=train$casual+1
    train$logcas=log(train$cas1)
    train$logreg=log(train$reg1)
    test$logreg=0
    test$logcas=0
    
    #print("Entering randomForest:")
    
    library("randomForest")
    
    set.seed(415)
    fit1 <- randomForest(logreg ~ hour +workingday, data=train,importance=TRUE, ntree=100)
    pred1=predict(fit1,test)
    test$logreg=pred1
    
    set.seed(415)
    fit2 <- randomForest(logcas ~ hour +workingday, data=train,importance=TRUE, ntree=100)
    pred2=predict(fit2,test)
    test$logcas=pred2
    
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s1<-data.frame(ntree=100,registered=test$registered,casual=test$casual,count=test$count)
    #print("Estimated bikes to be shared:")
    #print(s$count)
    
    set.seed(415)
    fit3 <- randomForest(logreg ~ hour +workingday, data=train,importance=TRUE, ntree=150)
    pred3=predict(fit3,test)
    test$logreg=pred3
    
    set.seed(415)
    fit4 <- randomForest(logcas ~ hour +workingday, data=train,importance=TRUE, ntree=150)
    pred4=predict(fit4,test)
    test$logcas=pred4
    
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s2<-data.frame(ntree=150,registered=test$registered,casual=test$casual,count=test$count)
    #print("Estimated bikes to be shared for 150 ntrees:")
    #print(s$count)
    
    
    set.seed(415)
    fit5 <- randomForest(logreg ~ hour +workingday, data=train,importance=TRUE, ntree=200)
    pred5=predict(fit5,test)
    test$logreg=pred5
    
    set.seed(415)
    fit6 <- randomForest(logcas ~ hour +workingday, data=train,importance=TRUE, ntree=200)
    pred6=predict(fit6,test)
    test$logcas=pred6
    
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s3<-data.frame(ntree=200,registered=test$registered,casual=test$casual,count=test$count)
    #print("Estimated bikes to be shared for 200 ntrees:")
    #print(s$count)

    set.seed(415)
    fit7 <- randomForest(logreg ~ hour +workingday, data=train,importance=TRUE, ntree=250)
    pred7=predict(fit7,test)
    test$logreg=pred7
    
    set.seed(415)
    fit8 <- randomForest(logcas ~ hour +workingday, data=train,importance=TRUE, ntree=250)
    pred8=predict(fit8,test)
    test$logcas=pred8
    
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s4<-data.frame(ntree=250,registered=test$registered,casual=test$casual,count=test$count)
    #print("Estimated bikes to be shared for 250 ntrees:")
    #print(s$count)
    
    set.seed(415)
    fit9 <- randomForest(logreg ~ hour +workingday, data=train,importance=TRUE, ntree=300)
    pred9=predict(fit9,test)
    test$logreg=pred9
    
    set.seed(415)
    fit10 <- randomForest(logcas ~ hour +workingday, data=train,importance=TRUE, ntree=300)
    pred10=predict(fit10,test)
    test$logcas=pred10
    
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s5<-data.frame(ntree=300,registered=test$registered,casual=test$casual,count=test$count)
    #print("Estimated bikes to be shared for 300 ntrees:")
    #print(s$count)
    #print(data)
    
    set.seed(415)
    fit11 <- randomForest(logreg ~ hour +workingday, data=train,importance=TRUE, ntree=350)
    pred11=predict(fit11,test)
    test$logreg=pred11
    
    set.seed(415)
    fit12 <- randomForest(logcas ~ hour +workingday, data=train,importance=TRUE, ntree=350)
    pred12=predict(fit12,test)
    test$logcas=pred12
    
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s6<-data.frame(ntree=350,registered=test$registered,casual=test$casual,count=test$count)
    #print("Estimated bikes to be shared for 350 ntrees:")
    #print(s$count)
    
    s <- rbind(s1,s2,s3,s4,s5,s6)
    
    return(s)
    #print(s)
    
    })
  
    output$tbl <- renderTable ({
      prediction()}, striped=TRUE, hover=TRUE, spacing='m', align='c', width=50
      
      
      
    )
    
    
    plt <- reactive({
      
      #reading the data files
      t1=read.csv("train.csv")
      
      train <- t1[as.integer(substr(t1$datetime,9,10))<13,]
      test <- t1[as.integer(substr(t1$datetime,9,10))>12,]
      
      #print(train)
      #print(test)
      sample <- data.frame(test$registered,test$casual,test$count)
      colnames(sample) <- c("registered2","casual2","count2")
      #print(sample)
      test$registered=0
      test$casual=0
      test$count=0
      data=rbind(train,test)
      
      
      #factoring some variables from numeric
      data$season=as.factor(data$season)
      data$weather=as.factor(data$weather)
      data$holiday=as.factor(data$holiday)
      data$workingday=as.factor(data$workingday)
            #extracting hour from the datetime variable
      data$hour=substr(data$datetime,12,13)
      data$hour=as.factor(data$hour)
      
      train=data[as.integer(substr(data$datetime,9,10))<13,]
      test=data[as.integer(substr(data$datetime,9,10))>12,]
      
      
      
      data$hour=as.integer(data$hour)
     
      data$day_part=0
      
      
      
      train=data[as.integer(substr(data$datetime,9,10))<13,]
      test=data[as.integer(substr(data$datetime,9,10))>12,]
      #data=rbind(train,test)
      data$dp_reg=0
      data$dp_reg[data$hour<8]=1
      data$dp_reg[data$hour>=22]=2
      data$dp_reg[data$hour>9 & data$hour<18]=3
      data$dp_reg[data$hour==8]=4
      data$dp_reg[data$hour==9]=5
      data$dp_reg[data$hour==20 | data$hour==21]=6
      data$dp_reg[data$hour==19 | data$hour==18]=7
      
      data$dp_cas=0
      data$dp_cas[data$hour<=8]=1
      data$dp_cas[data$hour==9]=2
      data$dp_cas[data$hour>=10 & data$hour<=19]=3
      data$dp_cas[data$hour>19]=4
      
      
      data$temp_reg=0
      data$temp_reg[data$temp<13]=1
      data$temp_reg[data$temp>=13 & data$temp<23]=2
      data$temp_reg[data$temp>=23 & data$temp<30]=3
      data$temp_reg[data$temp>=30]=4
      
      
      
      data$temp_cas=0
      data$temp_cas[data$temp<15]=1
      data$temp_cas[data$temp>=15 & data$temp<23]=2
      data$temp_cas[data$temp>=23 & data$temp<30]=3
      data$temp_cas[data$temp>=30]=4
      

      data$season=as.factor(data$season)
      data$holiday=as.factor(data$holiday)
      data$workingday=as.factor(data$workingday)
      data$weather=as.factor(data$weather)
      data$hour=as.factor(data$hour)
      data$temp_cas=as.factor(data$temp_cas)
      data$temp_reg=as.factor(data$temp_reg)
    
      train=data[as.integer(substr(data$datetime,9,10))<13,]
      test=data[as.integer(substr(data$datetime,9,10))>12,]
    
      
      #log transformation for some skewed variables
      train$reg1=train$registered+1
      train$cas1=train$casual+1
      train$logcas=log(train$cas1)
      train$logreg=log(train$reg1)
      #print(test)
      test$logreg <- 0
      test$logcas <- 0
      
      #final model building using random forest
      set.seed(415)
      fit1 <- randomForest(logreg ~ hour +workingday+temp_reg, data=train,importance=TRUE, ntree=100)
      pred1=predict(fit1,test)
      test$logreg=pred1
  
      set.seed(415)
      fit2 <- randomForest(logcas ~ hour + workingday + temp_reg, data=train,importance=TRUE, ntree=100)
      pred2=predict(fit2,test)
      test$logcas=pred2
      

      test$registered=exp(test$logreg)-1
      test$casual=exp(test$logcas)-1
      test$count=test$casual+test$registered

      s2 <- data.frame(sample,test)
      s2$rmsreg <- (log(s2$registered+1)-log(s2$registered2+1))^2
      rmsreg1 <- sqrt(mean(s2$rmsreg))
      s2$rmscas <- (log(s2$casual+1)-log(s2$casual2+1))^2
      rmscas1 <- sqrt(mean(s2$rmscas))
      s2$rmscount <- (log(s2$count+1)-log(s2$count2+1))^2
      rmscount1 <- sqrt(mean(s2$rmscount))

      set.seed(415)
      fit3 <- randomForest(logreg ~ hour +workingday+temp_reg, data=train,importance=TRUE, ntree=150)
      pred3=predict(fit3,test)
      test$logreg=pred3
      
      set.seed(415)
      fit4 <- randomForest(logcas ~ hour + workingday + temp_reg, data=train,importance=TRUE, ntree=150)
      pred4=predict(fit4,test)
      test$logcas=pred4
      
      
      test$registered=exp(test$logreg)-1
      test$casual=exp(test$logcas)-1
      test$count=test$casual+test$registered
      
      s2 <- data.frame(sample,test)
      s2$rmsreg <- (log(s2$registered+1)-log(s2$registered2+1))^2
      rmsreg2 <- sqrt(mean(s2$rmsreg))
      s2$rmscas <- (log(s2$casual+1)-log(s2$casual2+1))^2
      rmscas2 <- sqrt(mean(s2$rmscas))
      s2$rmscount <- (log(s2$count+1)-log(s2$count2+1))^2
      rmscount2 <- sqrt(mean(s2$rmscount))
      
      set.seed(415)
      fit5 <- randomForest(logreg ~ hour +workingday+temp_reg, data=train,importance=TRUE, ntree=200)
      pred5=predict(fit5,test)
      test$logreg=pred5
      
      set.seed(415)
      fit6 <- randomForest(logcas ~ hour + workingday + temp_reg, data=train,importance=TRUE, ntree=200)
      pred6=predict(fit6,test)
      test$logcas=pred6
      
      
      test$registered=exp(test$logreg)-1
      test$casual=exp(test$logcas)-1
      test$count=test$casual+test$registered
      
      s2 <- data.frame(sample,test)
      s2$rmsreg <- (log(s2$registered+1)-log(s2$registered2+1))^2
      rmsreg3 <- sqrt(mean(s2$rmsreg))
      s2$rmscas <- (log(s2$casual+1)-log(s2$casual2+1))^2
      rmscas3 <- sqrt(mean(s2$rmscas))
      s2$rmscount <- (log(s2$count+1)-log(s2$count2+1))^2
      rmscount3 <- sqrt(mean(s2$rmscount))
      
      set.seed(415)
      fit7 <- randomForest(logreg ~ hour +workingday+temp_reg, data=train,importance=TRUE, ntree=250)
      pred7=predict(fit7,test)
      test$logreg=pred7
      
      set.seed(415)
      fit8 <- randomForest(logcas ~ hour + workingday + temp_reg, data=train,importance=TRUE, ntree=250)
      pred8=predict(fit8,test)
      test$logcas=pred8
      
      
      test$registered=exp(test$logreg)-1
      test$casual=exp(test$logcas)-1
      test$count=test$casual+test$registered
      
      s2 <- data.frame(sample,test)
      s2$rmsreg <- (log(s2$registered+1)-log(s2$registered2+1))^2
      rmsreg4 <- sqrt(mean(s2$rmsreg))
      s2$rmscas <- (log(s2$casual+1)-log(s2$casual2+1))^2
      rmscas4 <- sqrt(mean(s2$rmscas))
      s2$rmscount <- (log(s2$count+1)-log(s2$count2+1))^2
      rmscount4 <- sqrt(mean(s2$rmscount))
      
      set.seed(415)
      fit9 <- randomForest(logreg ~ hour +workingday+temp_reg, data=train,importance=TRUE, ntree=300)
      pred9=predict(fit9,test)
      test$logreg=pred9
      
      set.seed(415)
      fit10 <- randomForest(logcas ~ hour + workingday + temp_reg, data=train,importance=TRUE, ntree=300)
      pred10=predict(fit10,test)
      test$logcas=pred10
      
      
      test$registered=exp(test$logreg)-1
      test$casual=exp(test$logcas)-1
      test$count=test$casual+test$registered
      
      s2 <- data.frame(sample,test)
      s2$rmsreg <- (log(s2$registered+1)-log(s2$registered2+1))^2
      rmsreg5 <- sqrt(mean(s2$rmsreg))
      s2$rmscas <- (log(s2$casual+1)-log(s2$casual2+1))^2
      rmscas5 <- sqrt(mean(s2$rmscas))
      s2$rmscount <- (log(s2$count+1)-log(s2$count2+1))^2
      rmscount5 <- sqrt(mean(s2$rmscount))
      
      set.seed(415)
      fit11 <- randomForest(logreg ~ hour +workingday+temp_reg, data=train,importance=TRUE, ntree=350)
      pred11=predict(fit11,test)
      test$logreg=pred11
      
      set.seed(415)
      fit12 <- randomForest(logcas ~ hour + workingday + temp_reg, data=train,importance=TRUE, ntree=350)
      pred12=predict(fit12,test)
      test$logcas=pred12
      
      
      test$registered=exp(test$logreg)-1
      test$casual=exp(test$logcas)-1
      test$count=test$casual+test$registered
      
      s2 <- data.frame(sample,test)
      s2$rmsreg <- (log(s2$registered+1)-log(s2$registered2+1))^2
      rmsreg6 <- sqrt(mean(s2$rmsreg))
      s2$rmscas <- (log(s2$casual+1)-log(s2$casual2+1))^2
      rmscas6 <- sqrt(mean(s2$rmscas))
      s2$rmscount <- (log(s2$count+1)-log(s2$count2+1))^2
      rmscount6 <- sqrt(mean(s2$rmscount))
      
      sd1 <- data.frame(100,rmsreg1,rmscas1,rmscount1)
      sd2 <- data.frame(150,rmsreg2,rmscas2,rmscount2)
      sd3 <- data.frame(200,rmsreg3,rmscas3,rmscount3)
      sd4 <- data.frame(250,rmsreg4,rmscas4,rmscount4)
      sd5 <- data.frame(300,rmsreg5,rmscas5,rmscount5)
      sd6 <- data.frame(350,rmsreg6,rmscas6,rmscount6)
      colnames(sd1) <- c("Ntree","Reg","Cas","Count")
      colnames(sd2) <- c("Ntree","Reg","Cas","Count")
      colnames(sd3) <- c("Ntree","Reg","Cas","Count")
      colnames(sd4) <- c("Ntree","Reg","Cas","Count")
      colnames(sd5) <- c("Ntree","Reg","Cas","Count")
      colnames(sd6) <- c("Ntree","Reg","Cas","Count")
      sd <- rbind(sd1,sd2,sd3,sd4,sd5,sd6)
      return(sd)
      
    })
    
    output$txt1 <- renderPrint({
      print("Logarithmic root mean squared error has been calculated by considering actual and predicted values of Registered users, Casual users and Count of the total users. The results are displayed in various plots as below:")
    })
    
    output$txt2 <- renderPrint({
      print("Accuracy of Registered Users: ")
    })
    
    output$txt3 <- renderPrint({
      print("Accuracy of Casual Users: ")
    })
    
    output$txt4 <- renderPrint({
      print("Accuracy of Total Count:")
    })
    
    
    
    output$coolplot <- renderPlot ({
      sd <- plt()
      
      ggplot(sd,aes(Ntree,Cas)) + geom_step(colour="red")

    })
    
    output$coolplot1 <- renderPlot ({
      sd <- plt()
      ggplot(sd,aes(Ntree,Reg)) + geom_line(colour="blue")
      
    })
  
    output$coolplot2 <- renderPlot ({
      sd <- plt()
      ggplot(sd,aes(Ntree,Count)) + geom_path(colour="green")
    })
    
    output$txt7 <- renderPrint ({
      print("Regression analysis of total count of the users for train data:")
    })
    
    output$plt1 <- renderPlot({
      train <- read.csv("train.csv")
      train <- train[as.integer(substr(train$datetime,9,10))<13,]
      ggplot(train, aes(y=train$casual, x=train$atemp, color=train$count)) + geom_point() + geom_abline()
    })
    
    output$txt8 <- renderPrint ({
      print("Regression analysis of count of bike sharing users for test data:")
    })
    
    output$plt2 <- renderPlot ({
      train <- read.csv("train.csv")
      test <- train[as.integer(substr(train$datetime,9,10))>12,]
      ggplot(test,aes(y=test$casual, x=test$atemp, color=test$count)) + geom_point() + geom_abline()
    })
    
  }
shinyApp(ui=ui,server=server)
