if(!require('shiny')) {
  install.packages('shiny')
  library('shiny')
}
if(!require('shinydashboard')) {
  install.packages('shinydashboard')
  library('shinydashboard')
}
if(!require('reactable')) {
   install.packages('reactable')
   library('reactable')
}
if(!require('corrplot')) {
  install.packages('corrplot')
  library('corrplot')
}
if(!require('reshape')) {
  install.packages('reshape')
  library('reshape')
}
if(!require('plotrix')) {
  install.packages('plotrix')
  library('plotrix')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}
if(!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('tidyr')) {
  install.packages('tidyr')
  library('tidyr')
}
#loading data
load("../data/data_2009.Rdata")
load("../data/data_2010.Rdata")
load("../data/data_2011.Rdata")
load("../data/data_2012.Rdata")
load("../data/data_2013.Rdata")
load("../data/data_2014.Rdata")
load("../data/data_2015.Rdata")
load("../data/data_2016.Rdata")
load("../data/data_2017.Rdata")
load("../data/data_2018.Rdata")
load("../data/data_2019.Rdata")
load("../data/data_2020.Rdata")
load("../data/data_2021.Rdata")
dat_1 <- read.csv("../data/season-0910_csv.csv")
dat_2 <- read.csv("../data/season-1011_csv.csv")
dat_3 <- read.csv("../data/season-1112_csv.csv")
dat_4 <- read.csv("../data/season-1213_csv.csv")
dat_5 <- read.csv("../data/season-1314_csv.csv")
dat_6 <- read.csv("../data/season-1415_csv.csv")
dat_7 <- read.csv("../data/season-1516_csv.csv")
dat_8 <- read.csv("../data/season-1617_csv.csv")
dat_9 <- read.csv("../data/season-1718_csv.csv")
dat_10 <- read.csv("../data/season-1819_csv.csv")
dat_11 <-read.csv("../data/season-1920_csv.csv")
dat_12 <-read.csv("../data/season-2021_csv.csv")
dat_13 <- read.csv("../data/season-2122_csv.csv")

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "EPL Data Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Points Table", tabName = "pointstable" , icon = icon("table")),
      menuItem("Match wise Analysis", tabName = "matchwise", icon = icon("futbol")),
      menuItem("Season wise Analysis", tabName = "seasonwise", icon = icon("th")),
      menuItem("Prediction", tabName = "prediction", icon = icon("coins"))
      
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              h1(strong("Analysing Premier League Data (2009-2022)"),align="center"),br(),
              h2("“ It’s difficult to make predictions in the 
                 Premier League, as unpredictable things can
                 happen, and I know that well: I won the title 
                 in a crazy way and lost one unexpectedly ” — Joe Hart ",align="center"),br(),
              h3("EPL is considered as one of the top football league in the world.The
                  teams, players, and fans across the globe make it a huge event.
                 But the thing which makes it truly fascinating is the sheer unpredictability."),
              h3("Through this shiny app, we have tried to analyse EPL data for 13 seasons.
                 The dataset has been taken from football-data-co.uk and espn.")
              ),
      tabItem(tabName = "matchwise",
              fluidRow(
                box(
                  title = "Year", width = 4,height = 150,
                  selectInput("dataset", "Select a year",
                              c("2009_2010" , "2010_2011" , "2011_2012" , "2012_2013" ,
                                "2013_2014" ,"2014_2015" , "2015_2016" , "2016_2017" ,
                                "2017_2018" ,"2018_2019" ,"2019_2020","2020_2021","2021_2022" ))
                ),
                box(
                  title = "Country",width = 4,height = 150,
                  selectInput(inputId = "variable", label = "Select a club",
                              choices = NULL)
                  
                ),
                box(
                  title = "Home/Away",width = 4,height=150,
                  radioButtons(inputId = "hoaw", label = "Home/Away",
                               choices = c("HomeTeam","AwayTeam"))
                ),
                box(
                  plotOutput(outputId = "distPlot",height=600),width=12,collapsible = TRUE,
                  solidHeader = TRUE
                ),
                box(
                  plotOutput(outputId = "distPlot_1",height=600),width=12,collapsible = TRUE,
                  solidHeader = TRUE
                ),
                box(
                  plotOutput(outputId = "distPlot_5",height=600),width=12,collapsible = TRUE,
                  solidHeader = TRUE
                )
                
                
              )),
      tabItem(tabName = "pointstable",
              fluidRow(
                box(
                  title = "Year", width = 12,height = 150,
                  selectInput("dataset4" , "Select a year",
                              c("2009" , "2010" , "2011" , "2012" ,
                                "2013" ,"2014" , "2015" , "2016" ,
                                "2017" ,"2018" ,"2019","2020","2021" ))),
                box(reactableOutput("table"),width=12)
                
              )),
      tabItem(tabName = "seasonwise",
              fluidRow(
                box(
                  title = "Year", width = 12,height = 150,
                  selectInput("dataset2" , "Select a year",
                              c("2009" , "2010" , "2011" , "2012" ,
                                "2013" ,"2014" , "2015" , "2016" ,
                                "2017" ,"2018" ,"2019","2020","2021" ))),
                box(
                  plotOutput(outputId = "distPlot_2",height=600),width=12,collapsible = TRUE,
                  solidHeader = TRUE
                ),
                box(
                  plotOutput(outputId = "distPlot_3",height=600),width=12,collapsible = TRUE,
                  solidHeader = TRUE
                )
              )
      ),
              

    tabItem(tabName = "prediction",
            fluidRow(
              box(
                title = "Year", width = 12,height = 150,
                selectInput("dataset3", "Select a year",
                            c("2009_2010" , "2010_2011" , "2011_2012" , "2012_2013" ,
                              "2013_2014" ,"2014_2015" , "2015_2016" , "2016_2017" ,
                              "2017_2018" ,"2018_2019" ,"2019_2020","2020_2021","2021_2022" ))
              ),
              box(
                plotOutput(outputId = "distPlot_4",height=600),width=12,collapsible = TRUE,
                solidHeader = TRUE
              )
            )
            )
  
  
)
)
)
server <- function(input, output) { 
  datasetInput4 <- reactive({
    switch(input$dataset4,
           "2009" = data_2009,
           "2010" = data_2010,
           "2011" = data_2011,
           "2012" = data_2012,
           "2013" = data_2013,
           "2014" = data_2014,
           "2015" = data_2015,
           "2016" = data_2016,
           "2017" = data_2017,
           "2018" = data_2018,
           "2019" = data_2019,
           "2020" = data_2020,
           "2021" = data_2021)
  })
  output$table <- renderReactable({
    dat <- datasetInput4()
    reactable(dat,defaultPageSize = 20)
    })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "2009_2010" = dat_1,
           "2010_2011" = dat_2,
           "2011_2012" = dat_3,
           "2012_2013" = dat_4,
           "2013_2014" = dat_5,
           "2014_2015" = dat_6,
           "2015_2016" = dat_7,
           "2016_2017" = dat_8,
           "2017_2018" = dat_9,
           "2018_2019" = dat_10,
           "2019_2020" = dat_11,
           "2020_2021" = dat_12,
           "2021_2022" = dat_13)
  })
  observeEvent(datasetInput(), {
    choices <- unique(datasetInput()$HomeTeam)
    updateSelectInput(inputId = "variable", choices = choices)})
  
  output$distPlot <- renderPlot({
    attach(datasetInput())
    
    if(input$hoaw == "HomeTeam"){
      team_data <- datasetInput()[which(HomeTeam == input$variable),]
      team_data <- team_data[order(team_data[,'AwayTeam']), ]
      attach(team_data)
      a <- length(team_data$HomeTeam)
      mat_chelsea <- cbind(team_data$AwayTeam,matrix( 0 , nrow = 19 , ncol = 3))
      
    }
    else{
      team_data <- datasetInput()[which(AwayTeam == input$variable),]
      team_data <- team_data[order(team_data[,'HomeTeam']), ]
      attach(team_data)
      a <- length(team_data$AwayTeam)
      mat_chelsea <- cbind(team_data$HomeTeam,matrix( 0 , nrow = 19 , ncol = 3))
    }
    for( i in 1:a )
    {
      if( FTHG[i] > FTAG[i])
      {
        mat_chelsea[i,2] = 1
      }else if( FTHG[i] == FTAG[i]){
        mat_chelsea[i,3] = 1
      }else{
        mat_chelsea[i,4] = 1
      }
    }
    total <- c("total" ,sum(as.numeric(mat_chelsea[,2]))
               ,sum(as.numeric(mat_chelsea[,3]))
               ,sum(as.numeric(mat_chelsea[,4])))
    
    mat_chelsea <- rbind(mat_chelsea , total)
    #   
    #   
    mat_chelsea <- as.data.frame(mat_chelsea)
    #   # Barplot
    #   
    #   
    #   
    #   
    x  = mat_chelsea[,1][1:19]
    y1 =   mat_chelsea[,2][1:19]
    y2 =   mat_chelsea[,3][1:19]
    y3 = mat_chelsea[,4][1:19]
    #   
    to_plot <- data.frame(x=x,win=y1,draw = y2,loose = y3)
    melted<-melt(to_plot, id="x")
    variable <- c("draw","loose","win"   )
    print(ggplot(melted,aes(x=x,y=value,fill=variable), xlab = "team") +
            labs(y = "Number of Matches", x = "TEAM",title = paste("Analysis of ",input$variable,sep="")) +
            coord_flip() +
            geom_bar(stat="identity",position = "dodge", alpha=1))+
      scale_fill_manual(values=c("#003C30", "#35978F","#C7EAE5" ) ,name = "Result of the match",
                        labels = c("Win", "Draw", "Lose")) + theme(plot.title = element_text(hjust = 0.5,face = "bold"))
    #   ######################################################
    #   
    #   
  })
  output$distPlot_1 <- renderPlot({
    #   
    #   
    #   
    #   
    if(input$hoaw == "HomeTeam"){
      team <- datasetInput()[which(HomeTeam == input$variable),]
      #A_chelsea <- A_chelsea[order(A_chelsea[, 'HomeTeam']), ]
      b = team %>%
        group_by(HomeTeam) %>%
        summarise_if(is.numeric, mean) %>%
        mutate(AY = -AY) %>%
        select(HomeTeam, HY, AY) %>%
        pivot_longer(., -HomeTeam) %>%
        ggplot() +
        geom_col(aes(x=HomeTeam, y=value, fill=name )) +
        scale_fill_manual(values=c("#C7EAE5","#003C30" ))+
        theme(axis.text.x = element_text(angle = 70, vjust = 0.4))}
    else{
      team <- datasetInput()[which(AwayTeam == input$variable),]
      #A_chelsea <- A_chelsea[order(A_chelsea[, 'AwayTeam']), ]
      b = team %>%
        group_by(AwayTeam) %>%
        summarise_if(is.numeric, mean) %>%
        mutate(HY = -HY) %>%
        select(AwayTeam, AY, HY) %>%
        pivot_longer(., -AwayTeam) %>%
        ggplot() +
        geom_col(aes(x=AwayTeam, y=value, fill=name )) +
        scale_fill_manual(values=c("#C7EAE5","#003C30" )) + 
        theme(axis.text.x = element_text(angle = 70, vjust = 0.4))
    }
    #   
    print(b +labs( title="Yellow Cards",y = 'NUMBERS OF YELLOW CARDS', x = 'OPPONENT TEAMS'))
    #   
    #   
  })
  output$distPlot_5 <- renderPlot({
    if(input$hoaw == "HomeTeam"){
      team <- datasetInput()[which(HomeTeam == input$variable),]
      b = team %>% 
        group_by(HomeTeam) %>% 
        summarise_if(is.numeric, mean) %>% 
        mutate(AR = -AR) %>% 
        select(HomeTeam, HR, AR) %>% 
        pivot_longer(., -HomeTeam) %>% 
        ggplot() +
        geom_col(aes(x=HomeTeam, y=value, fill=name )) +
        scale_fill_manual(values=c("#C7EAE5","#003C30" )) +
        theme(axis.text.x = element_text(angle = 70, vjust = 0.4))}
    else{
      team <- datasetInput()[which(AwayTeam == input$variable),]
      b = team %>% 
        group_by(AwayTeam) %>% 
        summarise_if(is.numeric, mean) %>% 
        mutate(HR= -HR) %>% 
        select(AwayTeam, AR, HR) %>% 
        pivot_longer(., -AwayTeam) %>% 
        ggplot() +
        geom_col(aes(x=AwayTeam, y=value, fill=name )) +
        scale_fill_manual(values=c("#C7EAE5","#003C30" )) +
        theme(axis.text.x = element_text(angle = 70, vjust = 0.4))
    }
    
    print(b +labs(title = "Red Cards", y = 'NUMBERS OF RED CARDS', x = 'OPPONENT TEAMS'))
    
    
  })
  
  datasetInput2 <- reactive({
    switch(input$dataset2,
           "2009" = data_2009,
           "2010" = data_2010,
           "2011" = data_2011,
           "2012" = data_2012,
           "2013" = data_2013,
           "2014" = data_2014,
           "2015" = data_2015,
           "2016" = data_2016,
           "2017" = data_2017,
           "2018" = data_2018,
           "2019" = data_2019,
           "2020" = data_2020,
           "2021" = data_2021)
  })
  output$distPlot_2 <- renderPlot({
    
    dat = datasetInput2()
    
    stands = subset(dat,select = c(1,P))
    names(stands)[1]="team"
    ggplot(data = stands, aes(x = P, y = reorder(team , P))) +
      geom_col( position = "dodge") +
      geom_col(aes(fill = P), show.legend = FALSE) +
      geom_col(data = stands[1:4,], fill = "#01665E") +
      scale_fill_continuous(low = "#8C510A", high = "#35978F") +
      geom_label(aes(label=P)) +
      labs(title = paste("Premier League ",input$dataset2,sep=""),
           x = "Total Points",
           y = "Teams") +
      theme(legend.position = "top", 
            plot.title = element_text(hjust = 0.5,face="bold"),
            plot.subtitle = element_text(hjust = 0.5))
  })
  output$distPlot_3 <- renderPlot({ 
    dat = datasetInput2()
    
    fadata <- subset(dat,select = c(1,F,A))
    ratio_sc <- fadata$F/fadata$A
    fadata[4] <- ratio_sc
    colnames(fadata)[4]="ratio_sc"
    colnames(fadata)[1]="team"
    fadata %>%
      arrange(desc(ratio_sc)) %>%
      mutate(team = factor(team, team)) %>%
      ggplot(aes(x=A, y=F, size=ratio_sc, color=team)) +
      geom_point(alpha=0.5) + scale_size(range = c(.1, 20)) +
      guides(size= "none",color=guide_legend(title="Teams")) + 
      ylab("Total Goals Scored") +
      xlab("Total Goals Conceded") + labs(title="Goals scored/Goals conceded")+
      theme( plot.title = element_text(hjust = 0.5,face="bold"))
    
    
  })
  datasetInput3 <- reactive({
    switch(input$dataset3,
           "2009_2010" = dat_1,
           "2010_2011" = dat_2,
           "2011_2012" = dat_3,
           "2012_2013" = dat_4,
           "2013_2014" = dat_5,
           "2014_2015" = dat_6,
           "2015_2016" = dat_7,
           "2016_2017" = dat_8,
           "2017_2018" = dat_9,
           "2018_2019" = dat_10,
           "2019_2020" = dat_11,
           "2020_2021" = dat_12,
           "2021_2022" = dat_13)
  })
  output$distPlot_4 <- renderPlot({
    dat <- datasetInput3()
    #FOR B365
    b365 <- subset(dat,select = c(FTR,B365A,B365H))
    b365 <- b365 %>% slice(-(which(b365$FTR == "D")))
    b365[4] <- 0
    colnames(b365)[4] = "predicted_result"
    for(i in 1:nrow(b365)){
      if(b365$B365A[i] > b365$B365H[i]){
        b365$predicted_result[i] = "H"
      }
      else{
        b365$predicted_result[i] = "A"
      }
    }
    right_b365 = sum(b365$FTR == b365$predicted_result)
    wrong_b365 = nrow(b365) - right_b365
    
    #FOR BW
    bw <- dat %>% filter(!is.na(BWA)) %>% filter(!is.na(BWH)) %>% select(FTR,BWA,BWH)
    bw <- bw %>% slice(-(which(bw$FTR == "D")))
    bw[4] <- 0
    colnames(bw)[4] = "predicted_result"
    for(i in 1:nrow(bw)){
      if(bw$BWA[i] > bw$BWH[i]){
        bw$predicted_result[i] = "H"
      }
      else{
        bw$predicted_result[i] = "A"
      }
    }
    right_bw = sum(bw$FTR == bw$predicted_result)
    wrong_bw = nrow(bw) - right_bw
    
    #FOR WH
    wh <- subset(dat,select = c(FTR,WHA,WHH))
    wh <- wh %>% slice(-(which(wh$FTR == "D")))
    wh[4] <- 0
    colnames(wh)[4] = "predicted_result"
    for(i in 1:nrow(wh)){
      if(wh$WHA[i] > wh$WHH[i]){
        wh$predicted_result[i] = "H"
      }
      else{
        wh$predicted_result[i] = "A"
      }
    }
    right_wh = sum(wh$FTR == wh$predicted_result)
    wrong_wh = nrow(wh) - right_wh
    
    
    #FOR IW
    iw <- dat %>% filter(!is.na(IWA)) %>% filter(!is.na(IWH)) %>% select(FTR,IWA,IWH)
    iw <- iw %>% slice(-(which(iw$FTR == "D")))
    iw[4] <- 0
    colnames(iw)[4] = "predicted_result"
    for(i in 1:nrow(iw)){
      if(iw$IWA[i] > iw$IWH[i]){
        iw$predicted_result[i] = "H"
      }
      else{
        iw$predicted_result[i] = "A"
      }
    }
    right_iw = sum(iw$FTR == iw$predicted_result)
    wrong_iw = nrow(iw) - right_iw
    
    #FOR VC
    vc <- subset(dat,select = c(FTR,VCA,VCH))
    vc <- vc %>% slice(-(which(vc$FTR == "D")))
    vc[4] <- 0
    colnames(vc)[4] = "predicted_result"
    for(i in 1:nrow(vc)){
      if(vc$VCA[i] > vc$VCH[i]){
        vc$predicted_result[i] = "H"
      }
      else{
        vc$predicted_result[i] = "A"
      }
    }
    right_vc = sum(vc$FTR == vc$predicted_result)
    wrong_vc = nrow(vc) - right_vc
    betplat <- c(rep("BET365" , 2) , rep("BET&WIN" , 2) , rep("William Hill" , 2) , rep("INTERWETTEN" , 2) ,
                 rep("VC Bet" , 2))
    condition <- rep(c("Right" , "Wrong" ) , 5)
    value = c(right_b365,wrong_b365,right_bw,wrong_bw,right_wh,wrong_wh,right_iw,wrong_iw,right_vc,wrong_vc)
    data <- data.frame(betplat,condition,value)
    ggplot(data, aes(fill=condition, y=value, x=betplat)) + 
      geom_bar(position="fill", stat="identity") + 
      scale_fill_manual(values = c("#01665E", "#C7EAE5"))+
      xlab("Betting Websites") +
      ylab("Count") +
      labs(fill='Prediction' , title="Predictions made by different betting websites")+
      theme( plot.title = element_text(hjust = 0.5,face="bold"))
  })
  
  }

shinyApp(ui, server)