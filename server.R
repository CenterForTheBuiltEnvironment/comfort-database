library(shiny)
library(ggplot2)
library(scales)
library(ordinal)
library(grid)
library(shinyapps)
library(shinyIncubator)
require(rdrop2)
require(boxr)

#data!

# outputDir <- "https://www.dropbox.com/s/fwmzxc82e9a0f93/AD2_viz.tool_CSV_Final_Completed_2017.csv?dl=0"
# filesInfo <- drop_dir(outputDir)
# # filePaths <- filesInfo$path
# db1 <- lapply(filesInfo, drop_read_csv, stringsAsFactors = FALSE)

db1<-read.csv("https://www.dropbox.com/s/fwmzxc82e9a0f93/AD2_viz.tool_CSV_Final_Completed_2017.csv?dl=1",stringsAsFactors=F,na.strings=c("NA",""))

# https://app.box.com/file/285295149392
# db2 <- box_read(285295149392)
# db1<-readr::read_csv("https://www.dropbox.com/s/fwmzxc82e9a0f93/AD2_viz.tool_CSV_Final_Completed_2017.csv?dl=0")
# db1 <- repmis::source_DropboxData("AD2_viz.tool_CSV_Final_Completed_2017.csv","fwmzxc82e9a0f93",header=T)
# db1<-read.csv("AD2_viz.tool_CSV_Final_Completed_2017.csv",stringsAsFactors=F,na.strings=c("NA",""))


#personal control columns
# ctrvarlist<-list("Window" = "PCEC1","External door" = "PCEC2","Internal door" = "PCEC3","Thermostat" = "PCEC4",
#                  "Curtains/blinds" = "PCEC5","Local heater" = "PCEC6","Local fan" = "PCEC7")
# ctrvars<-c("PCEC1","PCEC2","PCEC3","PCEC4","PCEC5","PCEC6","PCEC7")
#keep only useful columns
# columns<-c("database","city","TOP","TAAV","dayav_ta","prev_ta","cooling.strategy","ash15_level","tsa_ash_level","comf_ash_level","mci_level","season","country","climate","PMV","ASH","TSA","TSA_ord","acc15","acc2","acc_comf",ctrvars,"VELAV","RH","AGE","SEX","INSUL","MET","TOP_F","TAAV_F","dayav_ta_F","prev_ta_F","VELAV_FPM","TRAV","TRAV_F","BLTYPE")
columns<-c("database","city","TOP","TAAV","dayav_ta","prev_ta","cooling.strategy","ash15_level","tsa_ash_level","comf_ash_level","mci_level","season","country","climate","PMV","ASH","TSA","TSA_ord","acc15","acc2","acc_comf","PCEC1","PCEC2","PCEC3","PCEC4","PCEC5","PCEC6","PCEC7","VELAV","RH","AGE","SEX","INSUL","MET","TOP_F","TAAV_F","dayav_ta_F","prev_ta_F","VELAV_FPM","TRAV","TRAV_F","BLTYPE")

db1<-db1[,columns]



#order the factors for the probits
db1$ash15_level<-factor(db1$ash15_level,levels=c("cold","ok","hot"),ordered=T)
db1$comf_ash_level<-factor(db1$comf_ash_level,levels=c("cold","ok","hot"),ordered=T)
db1$tsa_ash_level<-factor(db1$tsa_ash_level,levels=c("cold","ok","hot"),ordered=T)
db1$mci_level<-factor(db1$mci_level,levels=c("cold","ok","hot"),ordered=T)

#pretty names for axes and such
labelDict<-data.frame(key=c("TSA","acc15","acc2","TAAV","dayav_ta","TOP","TRAV","PMV","ASH","acc_comf","acc_pref","city","cooling.strategy","ash15_probit","tsa_ash_probit","comf_ash_probit","mci_probit","COMF","MCI","N","prev_ta","TOP_F","TAAV_F","dayav_ta_F","prev_ta_F","TRAV_F","VELAV","VELAV_FPM","season","BLTYPE","MET","INSUL","RH","PCEC1","PCEC2","PCEC3","PCEC4","PCEC5","PCEC6","PCEC7"),
                      label=c("Acceptability","Sensation +-1.5","Sensation +-2","Indoor air temperature (°C)","Mean daily outdoor temperature (°C)","Operative temperature (°C)","Indoor radiant temperature (°C)","PMV","Thermal sensation","Thermal comfort","Thermal preference","Location","Ventilation type","Sensation +-1.5","Acceptability","Comfort","Preference","Comfort","Preference","N","Prevailing mean outdoor temperature (°C)",
                              "Operative temperature (°F)","Indoor air temperature (°F)","Mean daily outdoor temperature (°F)","Prevailing mean outdoor temperature (°F)","Indoor radiant temperature (°F)","Air Speed (m/s)","Air Speed (fpm)","Season","Building type","Metabolic rate (MET)","Clothing insulation (clo)","Relative humidity (%)","Window","External door","Internal door","Thermostat","Curtain","Heater","Fan"))
getLabel<-function(x){as.character(labelDict[labelDict$key==x,"label"])}

#function for squaring the labels of the size legend
square_format <- function() {
  function(x) format(x^2) 
}

#automatically add new lines for long strings
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

#colors for 2d temperature bin plots--0%,79%,80%,90%+ satisfied
unlow<-"#FF4040"
unhigh<-"#FFDEDE"
ac80<-"#A2CD5A"
ac90<-"#458B00"
#colors for the probit sample sets
colorDict<-data.frame(id=1:3,color=c("#7CA0FF","#B27D71","#8AB24F"))
getColor<-function(x){as.character(colorDict[colorDict$id==x,"color"])}
A<-"#7CA0FF"
B<-"#B27D71"
C<-"#8AB24F"


theme_set(theme_bw(14))


#make a probit curve given a sample set
probitCurve<-function(data,id,probitList,input,output){
  #show "calculating" instead of an error message
  validate(need(nrow(data)>0,"Calculating...")) 
  new1<-data.frame(x=seq(min(data[,input$xaxis]),max(data[,input$xaxis]),length.out=1000))
  colnames(new1)<-input$xaxis
  
  #fit the probit
  fit<-clm(formula(paste(input[[paste0("metric",id)]],"~",input$xaxis)),data=data,link="probit")
  #find the predicted values for the appropriate x axis
  new1$value<-1-predict(fit,newdata=new1,type="prob")[[1]][,"ok"]
  new1$variable<-paste("Sample set",id)
  probitList$new<-rbind(probitList$new,new1)
  
  #label for the legend and line color
  probitList$legendLab<-c(probitList$legendLab,paste("Sample set",id,"\nN =",format(nrow(data),big.mark=",")))
  probitList$colors<-c(probitList$colors,getColor(id))
  
  #calculate the confidence interval if requested
  if(input$showCI){
    pred<-predict(fit,newdata=new1,type="prob",interval=T)
    ci1<-data.frame(x=c(new1[,input$xaxis],rev(new1[,input$xaxis])),
                    y=c(1-pred[[2]][,"ok"],1-rev(pred[[3]][,"ok"])))
    ci1$variable<-paste("Sample set",id)
    probitList$ci<-rbind(probitList$ci,ci1)
  }
  return(probitList)
}







shinyServer(function(input, output, session) {

  # Filter data for Scatter 1
  filter1<-reactive({
    
    current<-db1
    current[(current$database %in% input$dataset1 &!is.na(current[,input$metric1]) & !is.na(current[,input$xaxis])),]
 
  })
  
  
  #possible ventilation type
  output$type1 <- renderUI( {
    avail<-sort(unique(filter1()[,"cooling.strategy"]))
    checkboxGroupInput("type1","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation type
  filtertype1<-reactive({
    current<-filter1()
    current[current$cooling.strategy %in% input$type1,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE1 <- renderUI( {
    avail<-sort(unique(filtertype1()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE1","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE1<-reactive({
    current<-filtertype1()
    current[current$BLTYPE %in% input$BLTYPE1,]
  })
  
  
  #possible seasons
  output$seasons1 <- renderUI( {
    avail<-sort(unique(filterBLTYPE1()[,"season"]))
    checkboxGroupInput("seasons1","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason1<-reactive({
    current<-filterBLTYPE1()
    if (input$season_check1){
      current[current$season %in% input$seasons1,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates1 <- renderUI( {
    avail<-sort(unique(filterSeason1()[,"climate"]))
    checkboxGroupInput("climates1","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate1<-reactive({
    current<-filterSeason1()
    if (input$climate_check1){
      current[current$climate %in% input$climates1,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries1 <- renderUI( {
    avail<-sort(unique(filterClimate1()[,"country"]))
    checkboxGroupInput("countries1","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry1<-reactive({
    current<-filterClimate1()
    if (input$country_check1){
      current[current$country %in% input$countries1,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities1 <- renderUI( {
    avail<-sort(unique(filterCountry1()[,"city"]))
    checkboxGroupInput("cities1","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity1<-reactive({
    current<-filterCountry1()
    if (input$city_check1){
      current[current$city %in% input$cities1,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond1<-reactive({
    current<-filterCity1()
    if(input$prev1){current<-current[(current$prev_ta >= input$outRange1[1] & current$prev_ta <= input$outRange1[2]) & !is.na(current$prev_ta),]}
    if(input$taav1){current<-current[(current$TAAV >= input$inRange1[1] & current$TAAV <= input$inRange1[2]) & !is.na(current$TAAV),]}
    if(input$trav1){current<-current[(current$TRAV >= input$radRange1[1] & current$TRAV <= input$radRange1[2]) & !is.na(current$TRAV),]}
    if(input$top1){current<-current[(current$TOP >= input$optRange1[1] & current$TOP <= input$optRange1[2]) & !is.na(current$TOP),]}
    if(input$rh1){current<-current[(current$RH >= input$RH1[1] & current$RH <= input$RH1[2]) & !is.na(current$RH),]}
    if(input$velfilter1){current<-current[(current$VELAV >= input$vel1[1] & current$VELAV <= input$vel1[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl1<- reactive({
    current <- filterCond1()
    if(input$winfilter1){current<-current[current$PCEC1 %in% input$win1 & !is.na(current$PCEC1),]}
    if(input$extfilter1){current<-current[current$PCEC2 %in% input$ext1 & !is.na(current$PCEC2),]}
    if(input$intfilter1){current<-current[current$PCEC3 %in% input$int1 & !is.na(current$PCEC3),]}
    if(input$thermofilter1){current<-current[current$PCEC4 %in% input$thermo1 & !is.na(current$PCEC4),]}
    if(input$curfilter1){current<-current[current$PCEC5 %in% input$cur1 & !is.na(current$PCEC5),]}
    if(input$heatfilter1){current<-current[current$PCEC6 %in% input$heat1 & !is.na(current$PCEC6),]}
    if(input$fanfilter1){current<-current[current$PCEC7 %in% input$fan1 & !is.na(current$PCEC7),]}
    current
  })
  
  
#   #possible controls
#   output$controls1 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond1()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist1","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl1<-reactive({
#     current<-filterCond1()
#     if(input$pc1){
#       if(input$andor1=="and"){
#         if(length(input$pclist1)>1){
#           a<-apply(current[,input$pclist1],1,all)
#         } else if(length(input$pclist1==1)){
#           a<-current[,input$pclist1]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor1=="no"){
#         if(length(input$pclist1)>1){
#           a<-apply(current[,input$pclist1],1,any)
#         } else if(length(input$pclist1==1)){
#           a<-current[,input$pclist1]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor1=="or"){
#         if(length(input$pclist1)>1){
#           a<-apply(current[,input$pclist1],1,any)
#         } else if(length(input$pclist1==1)){
#           a<-current[,input$pclist1]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
  
  
  #filter misc
  filterMisc1<-reactive({
    current<-filterControl1()
    if(input$sexfilter1){
      current<-current[current$SEX %in% input$sex1 & !is.na(current$SEX),]}
    if(input$agefilter1){current<-current[(current$AGE >= input$age1[1] & current$AGE <= input$age1[2]) & !is.na(current$AGE),]}
    if(input$clofilter1){current<-current[(current$INSUL >= input$clo1[1] & current$INSUL <= input$clo1[2]) & !is.na(current$INSUL),]}
    if(input$metfilter1){current<-current[(current$MET >= input$met1[1] & current$MET <= input$met1[2]) & !is.na(current$MET),]}
    current
  })
  
  
  
  
  # Filter data for Scatter 2
  filter2<-reactive({
    
    current<-db1
    current[(current$database %in% input$dataset2 &!is.na(current[,input$metric2]) & !is.na(current[,input$xaxis])),]

    
  })
  
  
  #possible ventilation type
  output$type2 <- renderUI( {
    avail<-sort(unique(filter2()[,"cooling.strategy"]))
    checkboxGroupInput("type2","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation type
  filtertype2<-reactive({
    current<-filter2()
    current[current$cooling.strategy %in% input$type2,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE2 <- renderUI( {
    avail<-sort(unique(filtertype2()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE2","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE2<-reactive({
    current<-filtertype2()
    current[current$BLTYPE %in% input$BLTYPE2,]
  })
  
  
  #possible seasons
  output$seasons2 <- renderUI( {
    avail<-sort(unique(filterBLTYPE2()[,"season"]))
    checkboxGroupInput("seasons2","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason2<-reactive({
    current<-filterBLTYPE2()
    if (input$season_check2){
      current[current$season %in% input$seasons2,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates2 <- renderUI( {
    avail<-sort(unique(filterSeason2()[,"climate"]))
    checkboxGroupInput("climates2","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate2<-reactive({
    current<-filterSeason2()
    if (input$climate_check2){
      current[current$climate %in% input$climates2,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries2 <- renderUI( {
    avail<-sort(unique(filterClimate2()[,"country"]))
    checkboxGroupInput("countries2","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry2<-reactive({
    current<-filterClimate2()
    if (input$country_check2){
      current[current$country %in% input$countries2,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities2 <- renderUI( {
    avail<-sort(unique(filterCountry2()[,"city"]))
    checkboxGroupInput("cities2","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity2<-reactive({
    current<-filterCountry2()
    if (input$city_check2){
      current[current$city %in% input$cities2,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond2<-reactive({
    current<-filterCity2()
    if(input$prev2){current<-current[(current$prev_ta >= input$outRange2[1] & current$prev_ta <= input$outRange2[2]) & !is.na(current$prev_ta),]}
    if(input$taav2){current<-current[(current$TAAV >= input$inRange2[1] & current$TAAV <= input$inRange2[2]) & !is.na(current$TAAV),]}
    if(input$trav2){current<-current[(current$TRAV >= input$radRange2[1] & current$TRAV <= input$radRange2[2]) & !is.na(current$TRAV),]}
    if(input$top2){current<-current[(current$TOP >= input$optRange2[1] & current$TOP <= input$optRange2[2]) & !is.na(current$TOP),]}
    if(input$rh2){current<-current[(current$RH >= input$RH2[1] & current$RH <= input$RH2[2]) & !is.na(current$RH),]}
    if(input$velfilter2){current<-current[(current$VELAV >= input$vel2[1] & current$VELAV <= input$vel2[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl2<- reactive({
    current <- filterCond2()
    if(input$winfilter2){current<-current[current$PCEC1 %in% input$win2 & !is.na(current$PCEC1),]}
    if(input$extfilter2){current<-current[current$PCEC2 %in% input$ext2 & !is.na(current$PCEC2),]}
    if(input$intfilter2){current<-current[current$PCEC3 %in% input$int2 & !is.na(current$PCEC3),]}
    if(input$thermofilter2){current<-current[current$PCEC4 %in% input$thermo2 & !is.na(current$PCEC4),]}
    if(input$curfilter2){current<-current[current$PCEC5 %in% input$cur2 & !is.na(current$PCEC5),]}
    if(input$heatfilter2){current<-current[current$PCEC6 %in% input$heat2 & !is.na(current$PCEC6),]}
    if(input$fanfilter2){current<-current[current$PCEC7 %in% input$fan2 & !is.na(current$PCEC7),]}
    current
  })
  
  
  
#   #possible controls
#   output$controls2 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond2()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist2","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl2<-reactive({
#     current<-filterCond2()
#     if(input$pc2){
#       if(input$andor2=="and"){
#         if(length(input$pclist2)>1){
#           a<-apply(current[,input$pclist2],1,all)
#         } else if(length(input$pclist2==1)){
#           a<-current[,input$pclist2]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor2=="no"){
#         if(length(input$pclist2)>1){
#           a<-apply(current[,input$pclist2],1,any)
#         } else if(length(input$pclist2==1)){
#           a<-current[,input$pclist2]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor2=="or"){
#         if(length(input$pclist2)>1){
#           a<-apply(current[,input$pclist2],1,any)
#         } else if(length(input$pclist2==1)){
#           a<-current[,input$pclist2]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
#   
  
  #filter misc
  filterMisc2<-reactive({
    current<-filterControl2()
    if(input$sexfilter2){
      current<-current[current$SEX %in% input$sex2 & !is.na(current$SEX),]}
    if(input$agefilter2){current<-current[(current$AGE >= input$age2[1] & current$AGE <= input$age2[2]) & !is.na(current$AGE),]}
    if(input$clofilter2){current<-current[(current$INSUL >= input$clo2[1] & current$INSUL <= input$clo2[2]) & !is.na(current$INSUL),]}
    if(input$metfilter2){current<-current[(current$MET >= input$met2[1] & current$MET <= input$met2[2]) & !is.na(current$MET),]}
    current
  })
  
  
  
  # Filter data for Scatter for 3
  filter3<-reactive({
    
    current<-db1
    current[(current$database %in% input$dataset3 &!is.na(current[,input$metric3]) & !is.na(current[,input$xaxis])),]
  })
  
  
  #possible ventilation type
  output$type3 <- renderUI( {
    avail<-sort(unique(filter3()[,"cooling.strategy"]))
    checkboxGroupInput("type3","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation type
  filtertype3<-reactive({
    current<-filter3()
    current[current$cooling.strategy %in% input$type3,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE3 <- renderUI( {
    avail<-sort(unique(filtertype3()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE3","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE3<-reactive({
    current<-filtertype3()
    current[current$BLTYPE %in% input$BLTYPE3,]
  })
  
  
  #possible seasons
  output$seasons3 <- renderUI( {
    avail<-sort(unique(filterBLTYPE3()[,"season"]))
    checkboxGroupInput("seasons3","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason3<-reactive({
    current<-filterBLTYPE3()
    if (input$season_check3){
      current[current$season %in% input$seasons3,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates3 <- renderUI( {
    avail<-sort(unique(filterSeason3()[,"climate"]))
    checkboxGroupInput("climates3","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate3<-reactive({
    current<-filterSeason3()
    if (input$climate_check3){
      current[current$climate %in% input$climates3,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries3 <- renderUI( {
    avail<-sort(unique(filterClimate3()[,"country"]))
    checkboxGroupInput("countries3","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry3<-reactive({
    current<-filterClimate3()
    if (input$country_check3){
      current[current$country %in% input$countries3,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities3 <- renderUI( {
    avail<-sort(unique(filterCountry3()[,"city"]))
    #    selectInput("cities3","Cities:",choices=avail,selected=avail,multiple=T,selectize=F)
    checkboxGroupInput("cities3","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity3<-reactive({
    current<-filterCountry3()
    if (input$city_check3){
      current[current$city %in% input$cities3,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond3<-reactive({
    current<-filterCity3()
    if(input$prev3){current<-current[(current$prev_ta >= input$outRange3[1] & current$prev_ta <= input$outRange3[2]) & !is.na(current$prev_ta),]}
    if(input$taav3){current<-current[(current$TAAV >= input$inRange3[1] & current$TAAV <= input$inRange3[2]) & !is.na(current$TAAV),]}
    if(input$trav3){current<-current[(current$TRAV >= input$radRange3[1] & current$TRAV <= input$radRange3[2]) & !is.na(current$TRAV),]}
    if(input$top3){current<-current[(current$TOP >= input$optRange3[1] & current$TOP <= input$optRange3[2]) & !is.na(current$TOP),]}
    if(input$rh3){current<-current[(current$RH >= input$RH3[1] & current$RH <= input$RH3[2]) & !is.na(current$RH),]}
    if(input$velfilter3){current<-current[(current$VELAV >= input$vel3[1] & current$VELAV <= input$vel3[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl3<- reactive({
    current <- filterCond3()
    if(input$winfilter3){current<-current[current$PCEC1 %in% input$win3 & !is.na(current$PCEC1),]}
    if(input$extfilter3){current<-current[current$PCEC2 %in% input$ext3 & !is.na(current$PCEC2),]}
    if(input$intfilter3){current<-current[current$PCEC3 %in% input$int3 & !is.na(current$PCEC3),]}
    if(input$thermofilter3){current<-current[current$PCEC4 %in% input$thermo3 & !is.na(current$PCEC4),]}
    if(input$curfilter3){current<-current[current$PCEC5 %in% input$cur3 & !is.na(current$PCEC5),]}
    if(input$heatfilter3){current<-current[current$PCEC6 %in% input$heat3 & !is.na(current$PCEC6),]}
    if(input$fanfilter3){current<-current[current$PCEC7 %in% input$fan3 & !is.na(current$PCEC7),]}
    current
  })
  
  
#   #possible controls
#   output$controls3 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond3()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist3","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl3<-reactive({
#     current<-filterCond3()
#     if(input$pc3){
#       if(input$andor3=="and"){
#         if(length(input$pclist3)>1){
#           a<-apply(current[,input$pclist3],1,all)
#         } else if(length(input$pclist3==1)){
#           a<-current[,input$pclist3]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor3=="no"){
#         if(length(input$pclist3)>1){
#           a<-apply(current[,input$pclist3],1,any)
#         } else if(length(input$pclist3==1)){
#           a<-current[,input$pclist3]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor3=="or"){
#         if(length(input$pclist3)>1){
#           a<-apply(current[,input$pclist3],1,any)
#         } else if(length(input$pclist3==1)){
#           a<-current[,input$pclist3]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
#   
  
  #filter misc
  filterMisc3<-reactive({
    current<-filterControl3()
    if(input$sexfilter3){
      current<-current[current$SEX %in% input$sex3 & !is.na(current$SEX),]}
    
    #if(input[[paste0("agefilter",id)]]){current<-current[(current$AGE >= input[[paste0("age",id)]][1] & current$AGE <= input[[paste0("age",id)]][2]) & !is.na(current$AGE),]}
    
    if(input$agefilter3){current<-current[(current$AGE >= input$age3[1] & current$AGE <= input$age3[2]) & !is.na(current$AGE),]}
    
    
    if(input$clofilter3){current<-current[(current$INSUL >= input$clo3[1] & current$INSUL <= input$clo3[2]) & !is.na(current$INSUL),]}
    if(input$metfilter3){current<-current[(current$MET >= input$met3[1] & current$MET <= input$met3[2]) & !is.na(current$MET),]}
    current
  })  
  
  
  #filterMisc1()
  
 probit_toplot <- reactive({ 
     

    
    #initialize some stuff
    probitList<-list(colors=c(),legendLab=c(),ci=data.frame(),new=data.frame())
    
    #fill in the list based on the sample sets
    if(input$compare1){
       probitList<-probitCurve(filterMisc1(),"1",probitList,input,output)
    }
    if(input$compare2){
      probitList<-probitCurve(filterMisc2(),"2",probitList,input,output)
    }
    if(input$compare3){
      probitList<-probitCurve(filterMisc3(),"3",probitList,input,output)
    }
    probitList
    
  })
  
  #make a beautiful plot
  probit<-reactive({
    probitList<-probit_toplot()
    if(nrow(probitList$new)>0){
      #axis labels
      axisLab<-getLabel(input$xaxis)
      
      #set up the plot
      p<-ggplot(probitList$new,aes_string(x=input$xaxis,y="value",color="variable"))+#geom_line()+
        scale_y_continuous(name="Percentage dissatisfied",limits=c(0,1),labels=percent_format(),expand=c(0,0))+
        xlab(axisLab)+
        scale_color_manual(name="",labels=probitList$legendLab,values=probitList$colors)+
        guides(colour = guide_legend(override.aes = list(size=5)))+
        theme(legend.key = element_rect(colour=NA), legend.key.height=unit(1.25,"in"))+
        ggtitle("Probit plot for percentage dissatisfied")
      
      #show confidence interval if requested
      if(input$showCI){
        p<-p+geom_polygon(data=probitList$ci,aes(x=x,y=y,fill=variable),alpha=0.25,color=NA)+
          scale_fill_manual(guide=F,values=probitList$colors)
      }
      #add the PMV-PPD curve if the x axis is PMV
      if(input$xaxis=="PMV"){
        p<-p+stat_function(fun=function(y){1-.95*exp(-.03353*y^4-0.2179*y^2)},geom="line",color="black")
      }
      
      #plot the curves
      p<-p+geom_line()
      
      #set limits
      if(input$xaxis=="PMV" | input$xaxis=="ASH"){
        p<-p+coord_cartesian(xlim=c(-3,3))+
          scale_x_continuous(breaks=-3:3,expand=c(0,0))
      } else {
        p<-p+coord_cartesian(xlim=input$xRange)+
          scale_x_continuous(breaks=seq(0,50,2),expand=c(0,0))
      }   
    }
  })
  
  
  
  #send the plot to be seen!
  output$pmvPlot <- renderPlot({
    print(probit())
  })
  
# -----------------------------------------------------------------------------------------
  
  #colors for the descriptive plots
  fillColors<-reactive({
    colors<-c()
    if(input$compare1){
      colors<-c(colors,A)
    }
    if(input$compare2){
      colors<-c(colors,B)
    }
    if(input$compare3){
      colors<-c(colors,C)
    }
    colors
  })
  

  

      histData<-reactive({  

    data<-data.frame()
    
    columns <- c("season","country","BLTYPE","cooling.strategy","city","climate","TOP","TAAV","prev_ta","RH","VELAV","TOP_F","TAAV_F","prev_ta_F","VELAV_FPM","AGE","SEX","MET","INSUL","ASH","PMV")
    
    
    if(input$compare1){
      d<-filterMisc1()[,columns] 
      #hide the error message
      validate(need(nrow(d)>0,""))
      d$set<-"Set 1"
      data<-rbind(data,d)
      colors<-c(colors,A)
    }

    if(input$compare2){
      d<-filterMisc2()[,columns] 
      validate(need(nrow(d)>0,""))
      d$set<-"Set 2"
      data<-rbind(data,d)
      colors<-c(colors,B)
    }

    if(input$compare3){
      d<-filterMisc3()[,columns] 
      validate(need(nrow(d)>0,""))
      d$set<-"Set 3"
      data<-rbind(data,d)
      colors<-c(colors,C)
    }
    
     data

  })
  

      
  # }) 
   
  # histogram
  cityPlot<-reactive({
    
    
     data<-histData()
    if(input$xstat=="country"){
      kkk<-"country"
    }
    if(input$xstat=="cooling.strategy"){
      kkk<-"cooling.strategy"
    }
    if(input$xstat=="BLTYPE"){
      kkk<-"BLTYPE"
    }
    if(input$xstat=="city"){
      kkk<-"city"
    }
    if(input$xstat=="climate"){
      kkk<-"climate"
    }
    if(input$xstat=="season"){
      kkk<-"season"
    }
    if(input$xstat=="SEX"){
      kkk<-"SEX"
    }
    colors<-fillColors()
    
    ggplot(data, aes_string(kkk, fill="set"))+geom_bar()+ 
      coord_flip()+
      scale_x_discrete(name="")+
      scale_fill_manual(values=colors,guide=F)+
      scale_y_continuous(name="Sample count",expand=c(0,0))+
      ggtitle("Bar chart for filtered samples statistics")
  })
  
  #send the city histogram to be shown
  output$cityPlotted<-renderPlot({
    print(cityPlot())
  })
  
  
  # ------------------------------------------------------------------------------------------------------------
  # Boxplot 1 & 2
  
  bp1 <- reactive({
     data<-histData()
    colors<-fillColors()
    if(input$units == "Metric"){
      if(input$boxplot1 == "TOP"){
        var <- "TOP"
        lab <- "Operative temperature (°C)"
      }
      if(input$boxplot1 == "OutTemp"){
        var <- "prev_ta"
        lab <- "Prevailing mean outdoor temperature (°C)"
      }
      if(input$boxplot1 == "AirTemp"){
        var <- "TAAV"
        lab <- "Indoor Air temperature (°C)"
      }
      if(input$boxplot1 == "RH"){
        var <- "RH"
        lab <- "Relative humidity (%)"
      }
      if(input$boxplot1 == "AirSpeed"){
        var <- "VELAV"
        lab <- "Air Speed (m/s)"
      }
      if(input$boxplot1 == "MET"){
        var <- "MET"
        lab <- "Metabolic rate (met)"
      }
      if(input$boxplot1 == "INSUL"){
        var <- "INSUL"
        lab <- "Clothing insulation (clo)"
      }
      if(input$boxplot1 == "AGE"){
        var <- "AGE"
        lab <- "Age"
      }
      if(input$boxplot1 == "TSV"){
        var <- "ASH"
        lab <- "ASHRAE thermal sensation vote"
      }
      if(input$boxplot1 == "PMV"){
        var <- "PMV"
        lab <- "Predict mean vote"
      }
    } else{
      if(input$boxplot1 == "TOP"){
        var <- "TOP_F"
        lab <- "Operative temperature (°F)"
      }
      if(input$boxplot1 == "OutTemp"){
        var <- "prev_ta_F"
        lab <- "Prevailing mean outdoor temperature (°F)"
      }
      if(input$boxplot1 == "AirTemp"){
        var <- "TAAV_F"
        lab <- "Indoor Air temperature (°F)"
      }
      if(input$boxplot1 == "RH"){
        var <- "RH"
        lab <- "Relative humidity (%)"
      }
      if(input$boxplot1 == "AirSpeed"){
        var <- "VELAV_FPM"
        lab <- "Air Speed (fpm)"
      }
      if(input$boxplot1 == "MET"){
        var <- "MET"
        lab <- "Metabolic rate (met)"
      }
      if(input$boxplot1 == "INSUL"){
        var <- "INSUL"
        lab <- "Clothing insulation (clo)"
      }
      if(input$boxplot1 == "AGE"){
        var <- "AGE"
        lab <- "Age"
      }
      if(input$boxplot1 == "TSV"){
        var <- "ASH"
        lab <- "ASHRAE thermal sensation vote"
      }
      if(input$boxplot1 == "PMV"){
        var <- "PMV"
        lab <- "Predict mean vote"
      }
    }
    
    
    ggplot(data,aes_string(x="set",y=var,color="set"))+geom_boxplot()+
      coord_flip()+
      scale_color_manual(values=c("Set 1"="#7CA0FF", "Set 2"="#B27D71", "Set 3"="#8AB24F"),name="",guide=F)+
      scale_x_discrete("set")+
      scale_y_continuous(name=lab,expand=c(0,0))+
      theme(axis.title.y = element_blank(), axis.title.x = element_text(size = 15),axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15))+
      ggtitle("Boxplot for filtered samples statistics")
  })
  
  bp2 <- reactive({
     data<-histData()
    colors<-fillColors()
    if(input$units == "Metric"){
      if(input$boxplot2 == "TOP"){
        var <- "TOP"
        lab <- "Operative temperature (°C)"
      }
      if(input$boxplot2 == "OutTemp"){
        var <- "prev_ta"
        lab <- "Prevailing mean outdoor temperature (°C)"
      }
      if(input$boxplot2 == "AirTemp"){
        var <- "TAAV"
        lab <- "Indoor Air temperature (°C)"
      }
      if(input$boxplot2 == "RH"){
        var <- "RH"
        lab <- "Relative humidity (%)"
      }
      if(input$boxplot2 == "AirSpeed"){
        var <- "VELAV"
        lab <- "Air Speed (m/s)"
      }
      if(input$boxplot2 == "MET"){
        var <- "MET"
        lab <- "Metabolic rate (met)"
      }
      if(input$boxplot2 == "INSUL"){
        var <- "INSUL"
        lab <- "Clothing insulation (clo)"
      }
      if(input$boxplot2 == "AGE"){
        var <- "AGE"
        lab <- "Age"
      }
      if(input$boxplot2 == "TSV"){
        var <- "ASH"
        lab <- "ASHRAE thermal sensation vote"
      }
      if(input$boxplot2 == "PMV"){
        var <- "PMV"
        lab <- "Predict mean vote"
      }
    } else{
      if(input$boxplot2 == "TOP"){
        var <- "TOP_F"
        lab <- "Operative temperature (°F)"
      }
      if(input$boxplot2 == "OutTemp"){
        var <- "prev_ta_F"
        lab <- "Prevailing mean outdoor temperature (°F)"
      }
      if(input$boxplot2 == "AirTemp"){
        var <- "TAAV_F"
        lab <- "Indoor Air temperature (°F)"
      }
      if(input$boxplot2 == "RH"){
        var <- "RH"
        lab <- "Relative humidity (%)"
      }
      if(input$boxplot2 == "AirSpeed"){
        var <- "VELAV_FPM"
        lab <- "Air Speed (fpm)"
      }
      if(input$boxplot2 == "MET"){
        var <- "MET"
        lab <- "Metabolic rate (met)"
      }
      if(input$boxplot2 == "INSUL"){
        var <- "INSUL"
        lab <- "Clothing insulation (clo)"
      }
      if(input$boxplot2 == "AGE"){
        var <- "AGE"
        lab <- "Age"
      }
      if(input$boxplot2 == "TSV"){
        var <- "ASH"
        lab <- "ASHRAE thermal sensation vote"
      }
      if(input$boxplot2 == "PMV"){
        var <- "PMV"
        lab <- "Predict mean vote"
      }
    }
    
    ggplot(data,aes_string(x="set",y=var,color="set"))+geom_boxplot()+
      coord_flip()+
      scale_color_manual(values=c("Set 1"="#7CA0FF", "Set 2"="#B27D71", "Set 3"="#8AB24F"),name="",guide=F)+
      scale_x_discrete("set")+
      scale_y_continuous(name=lab,expand=c(0,0))+
      theme(axis.title.y = element_blank(), axis.title.x = element_text(size = 15),axis.text.y = element_text(size = 15), axis.text.x = element_text(size = 15))
  })
  
  output$boxplot1 <- renderPlot({
    print(bp1())
  })
  
  output$boxplot2 <- renderPlot({
    print(bp2())
  })

  
  
  
  #make the download button work
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$xaxis, '.pdf', sep='') },
    content = function(file) {
      pdf(file,height=input$height,width=input$width)
      print(probit())
      print(cityPlot())
      print(bp1())
      print(bp2())
      
      dev.off()
    })
  

  
  
  
  
  # --------------------------------------------------------------------------------------------------------------
  

  ############################################ heatmap Adaptive model  
  

  #############first 2d bins
  #filter for the first group
  #it's really ugly to do it individually for both groups
  #but I can't make it work with the filterGroup function
  filter_bins1<-reactive({

    current<-db1

    
    
    #correct units
    if(input$units_bins=="Metric"){
      x<-input$xaxis_bins
      y<-input$yaxis_bins
    } else {
      x<-paste0(input$xaxis_bins,"_F")
      y<-paste0(input$yaxis_bins,"_F")
    }
    current[(current$database %in% input$dataset_bins1 & !is.na(current[,x]) & !is.na(current[,y]) & !is.na(current[,input$satmet])),]
  })
  
  
  #possible ventilation type
  output$type_bins1 <- renderUI( {
    avail<-sort(unique(filter_bins1()[,"cooling.strategy"]))
    checkboxGroupInput("type_bins1","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation type
  filtertype_bins1<-reactive({
    current<-filter_bins1()
    current[current$cooling.strategy %in% input$type_bins1,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE_bins1 <- renderUI( {
    avail<-sort(unique(filtertype_bins1()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE_bins1","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE_bins1<-reactive({
    current<-filtertype_bins1()
    current[current$BLTYPE %in% input$BLTYPE_bins1,]
  })
  
  
  #possible seasons
  output$seasons_bins1 <- renderUI( {
    avail<-sort(unique(filterBLTYPE_bins1()[,"season"]))
    checkboxGroupInput("seasons_bins1","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason_bins1<-reactive({
    current<-filterBLTYPE_bins1()
    if (input$season_check_bins1){
      current[current$season %in% input$seasons_bins1,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates_bins1 <- renderUI( {
    avail<-sort(unique(filterSeason_bins1()[,"climate"]))
    #selectInput("climates_bins1","Climates:",choices=avail,selected=avail,multiple=T,selectize=F)
    checkboxGroupInput("climates_bins1","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate_bins1<-reactive({
    current<-filterSeason_bins1()
    if (input$climate_check_bins1){
      current[current$climate %in% input$climates_bins1,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries_bins1 <- renderUI( {
    avail<-sort(unique(filterClimate_bins1()[,"country"]))
    #selectInput("countries_bins1","Countries:",choices=avail,selected=avail,multiple=T,selectize=F)
    checkboxGroupInput("countries_bins1","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry_bins1<-reactive({
    current<-filterClimate_bins1()
    if (input$country_check_bins1){
      current[current$country %in% input$countries_bins1,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities_bins1 <- renderUI( {
    avail<-sort(unique(filterCountry_bins1()[,"city"]))
    #    selectInput("cities_bins1","Cities:",choices=avail,selected=avail,multiple=T,selectize=F)
    checkboxGroupInput("cities_bins1","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity_bins1<-reactive({
    current<-filterCountry_bins1()
    if (input$city_check_bins1){
      current[current$city %in% input$cities_bins1,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond_bins1<-reactive({
    current<-filterCity_bins1()
    if(input$rh_bins1){current<-current[(current$RH >= input$RH_bins1[1] & current$RH <= input$RH_bins1[2]) & !is.na(current$RH),]}
    if(input$velfilter_bins1){current<-current[(current$VELAV >= input$vel_bins1[1] & current$VELAV <= input$vel_bins1[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl_bins1<- reactive({
    current <- filterCond_bins1()
    if(input$winfilter_bins1){current<-current[current$PCEC1 %in% input$win_bins1 & !is.na(current$PCEC1),]}
    if(input$extfilter_bins1){current<-current[current$PCEC2 %in% input$ext_bins1 & !is.na(current$PCEC2),]}
    if(input$intfilter_bins1){current<-current[current$PCEC3 %in% input$int_bins1 & !is.na(current$PCEC3),]}
    if(input$thermofilter_bins1){current<-current[current$PCEC4 %in% input$thermo_bins1 & !is.na(current$PCEC4),]}
    if(input$curfilter_bins1){current<-current[current$PCEC5 %in% input$cur_bins1 & !is.na(current$PCEC5),]}
    if(input$heatfilter_bins1){current<-current[current$PCEC6 %in% input$heat_bins1 & !is.na(current$PCEC6),]}
    if(input$fanfilter_bins1){current<-current[current$PCEC7 %in% input$fan_bins1 & !is.na(current$PCEC7),]}
    current
  })
  
#   #possible controls
#   output$controls_bins1 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond_bins1()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist_bins1","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl_bins1<-reactive({
#     current<-filterCond_bins1()
#     if(input$pc_bins1){
#       if(input$andor_bins1=="and"){
#         if(length(input$pclist_bins1)>1){
#           a<-apply(current[,input$pclist_bins1],1,all)
#         } else if(length(input$pclist_bins1==1)){
#           a<-current[,input$pclist_bins1]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor_bins1=="no"){
#         if(length(input$pclist_bins1)>1){
#           a<-apply(current[,input$pclist_bins1],1,any)
#         } else if(length(input$pclist_bins1==1)){
#           a<-current[,input$pclist_bins1]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor_bins1=="or"){
#         if(length(input$pclist_bins1)>1){
#           a<-apply(current[,input$pclist_bins1],1,any)
#         } else if(length(input$pclist_bins1==1)){
#           a<-current[,input$pclist_bins1]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
#   
  
  #filter misc
  filterMisc_bins1<-reactive({
    current<-filterControl_bins1()
    if(input$sexfilter_bins1){
      current<-current[current$SEX %in% input$sex_bins1 & !is.na(current$SEX),]}
    
    #if(input[[paste0("agefilter",id)]]){current<-current[(current$AGE >= input[[paste0("age",id)]][1] & current$AGE <= input[[paste0("age",id)]][2]) & !is.na(current$AGE),]}
    
    if(input$agefilter_bins1){current<-current[(current$AGE >= input$age_bins1[1] & current$AGE <= input$age_bins1[2]) & !is.na(current$AGE),]}
    
    
    if(input$clofilter_bins1){current<-current[(current$INSUL >= input$clo_bins1[1] & current$INSUL <= input$clo_bins1[2]) & !is.na(current$INSUL),]}
    if(input$metfilter_bins1){current<-current[(current$MET >= input$met_bins1[1] & current$MET <= input$met_bins1[2]) & !is.na(current$MET),]}
    current
  })
  
  
  
  #get the data ready for plotting
  bins_toplot<-reactive({
    #get the data, remove rows that don't have the required axes and metric
    full<-filterMisc_bins1()
    validate(need(nrow(full)>0, 'Calculating...'))
    if(input$units_bins=="Metric"){
      x<-input$xaxis_bins
      y<-input$yaxis_bins
    } else {
      x<-paste0(input$xaxis_bins,"_F")
      y<-paste0(input$yaxis_bins,"_F")
    }
    
    #bin axes
    full[,x]<-round(full[,x]/input$binwidth)*input$binwidth
    full[,y]<-round(full[,y]/input$binwidth)*input$binwidth
    
    #aggregate to find % satisfied (mean) and N
    f<-formula(paste(input$satmet," ~", y, "+", x))
    b1<-aggregate(f,data = full,FUN=mean)
    b2<-aggregate(f,data=full, FUN=length)
    aggregated<-cbind(b1,b2[,3])
    colnames(aggregated)[c(3,4)]<-c("mean","N")
    aggregated
  })
  
  #make the plot
  bins<-reactive({
    aggregated<-bins_toplot()
    #set the scale for the point size
    totalN<-sum(aggregated$N)
    if(input$compare_bins){
      maxN<-sqrt(max(aggregated$N,bins2_toplot()$N))
    } else{
      maxN<-sqrt(max(aggregated$N))
    }
    
    #get the units right
    if(input$units_bins=="Metric"){
      x<-input$xaxis_bins
      y<-input$yaxis_bins
      outRange<-input$outRange_bins
      inRange<-input$inRange_bins
    } else {
      x<-paste0(input$xaxis_bins,"_F")
      y<-paste0(input$yaxis_bins,"_F")
      outRange<-input$outRange_bins_F
      inRange<-input$inRange_bins_F
    }
    
    
    #basic plot setup
    p<-ggplot(aggregated,aes_string(x=x,y=y))+
      xlab(getLabel(x))+
      ylab(getLabel(y))+
      #Inf makes sure that the annotation will be at the corner of the graph regardless of the limits
      annotate(geom="text",x=Inf,y=-Inf,label=paste("N =",format(totalN,big.mark=",")),size=3.5,vjust=-1, hjust=1.2)+
      coord_cartesian(xlim=outRange,ylim=inRange)
    
    #add the gray background if you're comparing
    if(input$compare_bins){
      p<-p+geom_tile(data=bins2_toplot(),aes_string(x=x,y=y),fill="gray90",width=input$binwidth)
    }
    
    #add variable sized squares
    if(input$sizeN){
      p<-p+geom_point(aes(color=mean,size=sqrt(N)),shape=15)+
        scale_color_gradientn(colours=c(unlow,unhigh,ac80,ac90),values=c(0,.79,.8,1),labels=percent_format(),breaks=c(0,.5,.8,1),name="Satisfaction",limits=c(0,1))+
        scale_size_area(labels=square_format(),name="N",limits=c(1,maxN))
    } 
    #one sized squares
    else{
      p<-p+geom_tile(aes(fill=mean),width=input$binwidth)+
        scale_fill_gradientn(colours=c(unlow,unhigh,ac80,ac90),values=c(0,.79,.8,1),labels=percent_format(),breaks=c(0,.5,.8,1),name="Satisfaction",limits=c(0,1))
    }
    
    #overlay the ASHRAE 55 comfort zone
    if(input$ashrae){
      if(input$units_bins=="Metric"){
        p<-p+annotate(geom="segment",y=23.4,yend=30.53,x=10,xend=33,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=18.4,yend=25.53,x=10,xend=33,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=24.4,yend=31.53,x=10,xend=33,size=.5,color="black")+
          annotate(geom="segment",y=17.4,yend=24.53,x=10,xend=33,size=.5,color="black")
      } else {
        p<-p+annotate(geom="segment",y=74,yend=87,x=50,xend=91,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=65,yend=78,x=50,xend=91,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=76,yend=89,x=50,xend=91,size=.5,color="black")+
          annotate(geom="segment",y=63,yend=76,x=50,xend=91,size=.5,color="black")        
      }
    }
    
    #overlay EN 15251 comfort zone
    if(input$en){
      if(input$units_bins=="Metric"){
        p<-p+annotate(geom="segment",y=24.1,yend=30.7,x=10,xend=30,size=.5,linetype="dotted",color="black")+
          annotate(geom="segment",y=25.1,yend=31.7,x=10,xend=30,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=26.1,yend=32.7,x=10,xend=30,size=.5,color="black")+
          annotate(geom="segment",y=21.75,yend=26.7,x=15,xend=30,linetype="dotted",size=.5,color="black")+
          annotate(geom="segment",y=20.75,yend=25.7,x=15,xend=30,linetype="dashed",size=.5,color="black")+
          annotate(geom="segment",y=19.75,yend=24.7,x=15,xend=30,size=.5,color="black")
      } else {
        p<-p+annotate(geom="segment",y=75,yend=87,x=50,xend=86,size=.5,linetype="dotted",color="black")+
          annotate(geom="segment",y=77,yend=89,x=50,xend=86,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=80,yend=91,x=50,xend=86,size=.5,color="black")+
          annotate(geom="segment",y=71,yend=80,x=59,xend=86,linetype="dotted",size=.5,color="black")+
          annotate(geom="segment",y=69,yend=78,x=59,xend=86,linetype="dashed",size=.5,color="black")+
          annotate(geom="segment",y=67,yend=76,x=59,xend=86,size=.5,color="black")
      }
    }
    
    p<-p
  })
  
  
  #send the plot to be seen!
  output$heatPlot <- renderPlot({
    print(bins())
  })
  
  ####second heatmap
  #filter for the second group
  filter_bins2<-reactive({
    if(input$satmet=="calc"){
      current<-calculated()
    } else {
      current<-db1
    }
    if(input$units_bins=="Metric"){
      x<-input$xaxis_bins
      y<-input$yaxis_bins
    } else {
      x<-paste0(input$xaxis_bins,"_F")
      y<-paste0(input$yaxis_bins,"_F")
    }
    current[(current$database %in% input$dataset_bins2 & !is.na(current[,x]) & !is.na(current[,y]) & !is.na(current[,input$satmet])),]
  })
  
  
  #possible ventilation type
  output$type_bins2 <- renderUI( {
    avail<-sort(unique(filter_bins2()[,"cooling.strategy"]))
    checkboxGroupInput("type_bins2","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation type
  filtertype_bins2<-reactive({
    current<-filter_bins2()
    current[current$cooling.strategy %in% input$type_bins2,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE_bins2 <- renderUI( {
    avail<-sort(unique(filtertype_bins2()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE_bins2","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE_bins2<-reactive({
    current<-filtertype_bins2()
    current[current$BLTYPE %in% input$BLTYPE_bins2,]
  })

  
  #possible seasons
  output$seasons_bins2 <- renderUI( {
    avail<-sort(unique(filterBLTYPE_bins2()[,"season"]))
    checkboxGroupInput("seasons_bins2","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason_bins2<-reactive({
    current<-filterBLTYPE_bins2()
    if (input$season_check_bins2){
      current[current$season %in% input$seasons_bins2,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates_bins2 <- renderUI( {
    avail<-sort(unique(filterSeason_bins2()[,"climate"]))
    checkboxGroupInput("climates_bins2","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate_bins2<-reactive({
    current<-filterSeason_bins2()
    if (input$climate_check_bins2){
      current[current$climate %in% input$climates_bins2,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries_bins2 <- renderUI( {
    avail<-sort(unique(filterClimate_bins2()[,"country"]))
    checkboxGroupInput("countries_bins2","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry_bins2<-reactive({
    current<-filterClimate_bins2()
    if (input$country_check_bins2){
      current[current$country %in% input$countries_bins2,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities_bins2 <- renderUI( {
    avail<-sort(unique(filterCountry_bins2()[,"city"]))
    #    selectInput("cities_bins1","Cities:",choices=avail,selected=avail,multiple=T,selectize=F)
    checkboxGroupInput("cities_bins2","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity_bins2<-reactive({
    current<-filterCountry_bins2()
    if (input$city_check_bins2){
      current[current$city %in% input$cities_bins2,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond_bins2<-reactive({
    current<-filterCity_bins2()
    if(input$rh_bins2){current<-current[(current$RH >= input$RH_bins2[1] & current$RH <= input$RH_bins2[2]) & !is.na(current$RH),]}
    if(input$velfilter_bins2){current<-current[(current$VELAV >= input$vel_bins2[1] & current$VELAV <= input$vel_bins2[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl_bins2<- reactive({
    current <- filterCond_bins2()
    if(input$winfilter_bins2){current<-current[current$PCEC1 %in% input$win_bins2 & !is.na(current$PCEC1),]}
    if(input$extfilter_bins2){current<-current[current$PCEC2 %in% input$ext_bins2 & !is.na(current$PCEC2),]}
    if(input$intfilter_bins2){current<-current[current$PCEC3 %in% input$int_bins2 & !is.na(current$PCEC3),]}
    if(input$thermofilter_bins2){current<-current[current$PCEC4 %in% input$thermo_bins2 & !is.na(current$PCEC4),]}
    if(input$curfilter_bins2){current<-current[current$PCEC5 %in% input$cur_bins2 & !is.na(current$PCEC5),]}
    if(input$heatfilter_bins2){current<-current[current$PCEC6 %in% input$heat_bins2 & !is.na(current$PCEC6),]}
    if(input$fanfilter_bins2){current<-current[current$PCEC7 %in% input$fan_bins2 & !is.na(current$PCEC7),]}
    current
  })
  
  
  
#   #possible controls
#   output$controls_bins2 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond_bins2()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist_bins2","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl_bins2<-reactive({
#     current<-filterCond_bins2()
#     if(input$pc_bins2){
#       if(input$andor_bins2=="and"){
#         if(length(input$pclist_bins2)>1){
#           a<-apply(current[,input$pclist_bins2],1,all)
#         } else if(length(input$pclist_bins2==1)){
#           a<-current[,input$pclist_bins2]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor_bins2=="no"){
#         if(length(input$pclist_bins2)>1){
#           a<-apply(current[,input$pclist_bins2],1,any)
#         } else if(length(input$pclist_bins2==1)){
#           a<-current[,input$pclist_bins2]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor_bins2=="or"){
#         if(length(input$pclist_bins2)>1){
#           a<-apply(current[,input$pclist_bins2],1,any)
#         } else if(length(input$pclist_bins2==1)){
#           a<-current[,input$pclist_bins2]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
  
  #filter misc
  filterMisc_bins2<-reactive({
    current<-filterControl_bins2()
    if(input$sexfilter_bins2){
      current<-current[current$SEX %in% input$sex_bins2 & !is.na(current$SEX),]}
    if(input$agefilter_bins2){current<-current[(current$AGE >= input$age_bins2[1] & current$AGE <= input$age_bins2[2]) & !is.na(current$AGE),]}
    if(input$clofilter_bins2){current<-current[(current$INSUL >= input$clo_bins2[1] & current$INSUL <= input$clo_bins2[2]) & !is.na(current$INSUL),]}
    if(input$metfilter_bins2){current<-current[(current$MET >= input$met_bins2[1] & current$MET <= input$met_bins2[2]) & !is.na(current$MET),]}
    current
  })
  
  #get the data ready for plotting
  bins2_toplot<-reactive({
    
    #get the data, remove rows that don't have the required axes and metric
    full<-filterMisc_bins2()
    validate(need(nrow(full)>0, 'Calculating...'))
    
    #appropriate units
    if(input$units_bins=="Metric"){
      x<-input$xaxis_bins
      y<-input$yaxis_bins
    } else {
      x<-paste0(input$xaxis_bins,"_F")
      y<-paste0(input$yaxis_bins,"_F")
    }
    
    
    
    #bin axes
    full[,x]<-round(full[,x]/input$binwidth)*input$binwidth
    full[,y]<-round(full[,y]/input$binwidth)*input$binwidth
    
    #aggregate to find % satisfied (mean) and N
    f<-formula(paste(input$satmet," ~", y, "+", x))
    b1<-aggregate(f,data = full,FUN=mean)
    b2<-aggregate(f,data=full, FUN=length)
    aggregated2<-cbind(b1,b2[,3])
    colnames(aggregated2)[c(3,4)]<-c("mean","N")
    aggregated2
  })
  
  bins2<-reactive({
    aggregated2<-bins2_toplot()
    totalN<-sum(aggregated2$N)
    
    #appropriate units
    if(input$units_bins=="Metric"){
      x<-input$xaxis_bins
      y<-input$yaxis_bins
      outRange<-input$outRange_bins
      inRange<-input$inRange_bins
    } else {
      x<-paste0(input$xaxis_bins,"_F")
      y<-paste0(input$yaxis_bins,"_F")
      outRange<-input$outRange_bins_F
      inRange<-input$inRange_bins_F
    }
    
    #scale for the square size
    if(input$compare_bins){
      maxN<-sqrt(max(aggregated2$N,bins_toplot()$N))
    } else{
      maxN<-sqrt(max(aggregated2$N))
    }
    
    #basic plot setup
    p<-ggplot(aggregated2,aes_string(x=x,y=y))+
      xlab(getLabel(x))+
      ylab(getLabel(y))+
      geom_tile(data=bins_toplot(),aes_string(x=x,y=y),fill="gray90",width=input$binwidth)+
      annotate(geom="text",x=Inf,y=-Inf,label=paste("N =",format(totalN,big.mark=",")),size=3.5,vjust=-1, hjust=1.2)+
      coord_cartesian(xlim=outRange,ylim=inRange)
    
    if(input$sizeN){
      p<-p+geom_point(aes(color=mean,size=sqrt(N)),shape=15)+
        scale_color_gradientn(colours=c(unlow,unhigh,ac80,ac90),values=c(0,.79,.8,1),labels=percent_format(),breaks=c(0,.5,.8,1),name="Satisfaction",limits=c(0,1))+
        scale_size_area(labels=square_format(),name="N",limits=c(1,maxN))
    } else{
      p<-p+geom_tile(aes(fill=mean),width=input$binwidth)+
        scale_fill_gradientn(colours=c(unlow,unhigh,ac80,ac90),values=c(0,.79,.8,1),labels=percent_format(),breaks=c(0,.5,.8,1),name="Satisfaction",limits=c(0,1))
    }
    
    
    #overlay the ASHRAE 55 comfort zone
    if(input$ashrae){
      if(input$units_bins=="Metric"){
        p<-p+annotate(geom="segment",y=23.4,yend=30.53,x=10,xend=33,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=18.4,yend=25.53,x=10,xend=33,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=24.4,yend=31.53,x=10,xend=33,size=.5,color="black")+
          annotate(geom="segment",y=17.4,yend=24.53,x=10,xend=33,size=.5,color="black")
      } else {
        p<-p+annotate(geom="segment",y=74,yend=87,x=50,xend=91,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=65,yend=78,x=50,xend=91,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=76,yend=89,x=50,xend=91,size=.5,color="black")+
          annotate(geom="segment",y=63,yend=76,x=50,xend=91,size=.5,color="black")        
      }
    }
    
    #overlay EN 15251 comfort zone
    if(input$en){
      if(input$units_bins=="Metric"){
        p<-p+annotate(geom="segment",y=24.1,yend=30.7,x=10,xend=30,size=.5,linetype="dotted",color="black")+
          annotate(geom="segment",y=25.1,yend=31.7,x=10,xend=30,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=26.1,yend=32.7,x=10,xend=30,size=.5,color="black")+
          annotate(geom="segment",y=21.75,yend=26.7,x=15,xend=30,linetype="dotted",size=.5,color="black")+
          annotate(geom="segment",y=20.75,yend=25.7,x=15,xend=30,linetype="dashed",size=.5,color="black")+
          annotate(geom="segment",y=19.75,yend=24.7,x=15,xend=30,size=.5,color="black")
      } else {
        p<-p+annotate(geom="segment",y=75,yend=87,x=50,xend=86,size=.5,linetype="dotted",color="black")+
          annotate(geom="segment",y=77,yend=89,x=50,xend=86,size=.5,linetype="dashed",color="black")+
          annotate(geom="segment",y=80,yend=91,x=50,xend=86,size=.5,color="black")+
          annotate(geom="segment",y=71,yend=80,x=59,xend=86,linetype="dotted",size=.5,color="black")+
          annotate(geom="segment",y=69,yend=78,x=59,xend=86,linetype="dashed",size=.5,color="black")+
          annotate(geom="segment",y=67,yend=76,x=59,xend=86,size=.5,color="black")
      }
    }
    
    p<-p
    
  })
  
  
  #send the second plot to be seen!
  output$heatPlot2 <- renderPlot({
    print(bins2())
  })
  
  #make the download button work
  output$downloadPlot_bins <- downloadHandler(
    filename = function() { paste(input$xaxis_bins, '.pdf', sep='') },
    content = function(file) {
      pdf(file,height=4,width=5.7)
      print(bins())
      if(input$compare_bins){
        print(bins2())
      }
      dev.off()
    })
  
  
  
  #}
  
  
  
  # -------------------------------------------------------------------------------------------------------------------------------
  # Scatter dataset
  
  
  # Filter data for Scatter 1
  filter_S1<-reactive({
    
    current<-db1
    current[(current$database %in% input$dataset_S1 & !is.na(current[,input$satmet_S])),]

  })
  
  #possible ventilation strategy
  output$type_S1 <- renderUI( {
    avail<-sort(unique(filter_S1()[,"cooling.strategy"]))
    checkboxGroupInput("type_S1","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation strategy
  filtertype_S1<-reactive({
    current<-filter_S1()
    current[current$cooling.strategy %in% input$type_S1,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE_S1 <- renderUI( {
    avail<-sort(unique(filtertype_S1()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE_S1","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE_S1<-reactive({
    current<-filtertype_S1()
    current[current$BLTYPE %in% input$BLTYPE_S1,]
  })
  
  
  #possible seasons
  output$seasons_S1 <- renderUI( {
    avail<-sort(unique(filterBLTYPE_S1()[,"season"]))
    checkboxGroupInput("seasons_S1","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason_S1<-reactive({
    current<-filterBLTYPE_S1()
    if (input$season_check_S1){
      current[current$season %in% input$seasons_S1,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates_S1 <- renderUI( {
    avail<-sort(unique(filterSeason_S1()[,"climate"]))
    checkboxGroupInput("climates_S1","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate_S1<-reactive({
    current<-filterSeason_S1()
    if (input$climate_check_S1){
      current[current$climate %in% input$climates_S1,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries_S1 <- renderUI( {
    avail<-sort(unique(filterClimate_S1()[,"country"]))
    checkboxGroupInput("countries_S1","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry_S1<-reactive({
    current<-filterClimate_S1()
    if (input$country_check_S1){
      current[current$country %in% input$countries_S1,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities_S1 <- renderUI( {
    avail<-sort(unique(filterCountry_S1()[,"city"]))
    checkboxGroupInput("cities_S1","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity_S1<-reactive({
    current<-filterCountry_S1()
    if (input$city_check_S1){
      current[current$city %in% input$cities_S1,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond_S1<-reactive({
    current<-filterCity_S1()
    if(input$prev_S1){current<-current[(current$prev_ta >= input$outRange_S1[1] & current$prev_ta <= input$outRange_S1[2]) & !is.na(current$prev_ta),]}
    if(input$taav_S1){current<-current[(current$TAAV >= input$inRange_S1[1] & current$TAAV <= input$inRange_S1[2]) & !is.na(current$TAAV),]}
    if(input$trav_S1){current<-current[(current$TRAV >= input$radRange_S1[1] & current$TRAV <= input$radRange_S1[2]) & !is.na(current$TRAV),]}
    if(input$top_S1){current<-current[(current$TOP >= input$optRange_S1[1] & current$TOP <= input$optRange_S1[2]) & !is.na(current$TOP),]}
    if(input$rh_S1){current<-current[(current$RH >= input$RH_S1[1] & current$RH <= input$RH_S1[2]) & !is.na(current$RH),]}
    if(input$velfilter_S1){current<-current[(current$VELAV >= input$vel_S1[1] & current$VELAV <= input$vel_S1[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl_S1<- reactive({
    current <- filterCond_S1()
    if(input$winfilter_S1){current<-current[current$PCEC1 %in% input$win_S1 & !is.na(current$PCEC1),]}
    if(input$extfilter_S1){current<-current[current$PCEC2 %in% input$ext_S1 & !is.na(current$PCEC2),]}
    if(input$intfilter_S1){current<-current[current$PCEC3 %in% input$int_S1 & !is.na(current$PCEC3),]}
    if(input$thermofilter_S1){current<-current[current$PCEC4 %in% input$thermo_S1 & !is.na(current$PCEC4),]}
    if(input$curfilter_S1){current<-current[current$PCEC5 %in% input$cur_S1 & !is.na(current$PCEC5),]}
    if(input$heatfilter_S1){current<-current[current$PCEC6 %in% input$heat_S1 & !is.na(current$PCEC6),]}
    if(input$fanfilter_S1){current<-current[current$PCEC7 %in% input$fan_S1 & !is.na(current$PCEC7),]}
    current
  })
  
  
#   #possible controls
#   output$controls_S1 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond_S1()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist_S1","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl_S1<-reactive({
#     current<-filterCond_S1()
#     if(input$pc_S1){
#       if(input$andor_S1=="and"){
#         if(length(input$pclist_S1)>1){
#           a<-apply(current[,input$pclist_S1],1,all)
#         } else if(length(input$pclist_S1==1)){
#           a<-current[,input$pclist_S1]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor_S1=="no"){
#         if(length(input$pclist_S1)>1){
#           a<-apply(current[,input$pclist_S1],1,any)
#         } else if(length(input$pclist_S1==1)){
#           a<-current[,input$pclist_S1]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor_S1=="or"){
#         if(length(input$pclist_S1)>1){
#           a<-apply(current[,input$pclist_S1],1,any)
#         } else if(length(input$pclist_S1==1)){
#           a<-current[,input$pclist_S1]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
#   
  
  #filter misc
  filterMisc_S1<-reactive({
    current<-filterControl_S1()
    if(input$sexfilter_S1){
      current<-current[current$SEX %in% input$sex_S1 & !is.na(current$SEX),]}
   
    if(input$agefilter_S1){current<-current[(current$AGE >= input$age_S1[1] & current$AGE <= input$age_S1[2]) & !is.na(current$AGE),]}
    
    
    if(input$clofilter_S1){current<-current[(current$INSUL >= input$clo_S1[1] & current$INSUL <= input$clo_S1[2]) & !is.na(current$INSUL),]}
    if(input$metfilter_S1){current<-current[(current$MET >= input$met_S1[1] & current$MET <= input$met_S1[2]) & !is.na(current$MET),]}
    current
  })
  
  
  
  
  # Filter data for Scatter 2
  filter_S2<-reactive({
    
    current<-db1

    current[(current$database %in% input$dataset_S2 & !is.na(current[,input$satmet_S])),]
    
  })
  
  #possible ventilation strategy
  output$type_S2 <- renderUI( {
    avail<-sort(unique(filter_S2()[,"cooling.strategy"]))
    checkboxGroupInput("type_S2","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation strategy
  filtertype_S2<-reactive({
    current<-filter_S2()
    current[current$cooling.strategy %in% input$type_S2,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE_S2 <- renderUI( {
    avail<-sort(unique(filtertype_S2()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE_S2","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE_S2<-reactive({
    current<-filtertype_S2()
    current[current$BLTYPE %in% input$BLTYPE_S2,]
  })
  
  
  #possible seasons
  output$seasons_S2 <- renderUI( {
    avail<-sort(unique(filterBLTYPE_S2()[,"season"]))
    checkboxGroupInput("seasons_S2","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason_S2<-reactive({
    current<-filterBLTYPE_S2()
    if (input$season_check_S2){
      current[current$season %in% input$seasons_S2,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates_S2 <- renderUI( {
    avail<-sort(unique(filterSeason_S2()[,"climate"]))
    checkboxGroupInput("climates_S2","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate_S2<-reactive({
    current<-filterSeason_S2()
    if (input$climate_check_S2){
      current[current$climate %in% input$climates_S2,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries_S2 <- renderUI( {
    avail<-sort(unique(filterClimate_S2()[,"country"]))
    checkboxGroupInput("countries_S2","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry_S2<-reactive({
    current<-filterClimate_S2()
    if (input$country_check_S2){
      current[current$country %in% input$countries_S2,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities_S2 <- renderUI( {
    avail<-sort(unique(filterCountry_S2()[,"city"]))
    checkboxGroupInput("cities_S2","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity_S2<-reactive({
    current<-filterCountry_S2()
    if (input$city_check_S2){
      current[current$city %in% input$cities_S2,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond_S2<-reactive({
    current<-filterCity_S2()
    if(input$prev_S2){current<-current[(current$prev_ta >= input$outRange_S2[1] & current$prev_ta <= input$outRange_S2[2]) & !is.na(current$prev_ta),]}
    if(input$taav_S2){current<-current[(current$TAAV >= input$inRange_S2[1] & current$TAAV <= input$inRange_S2[2]) & !is.na(current$TAAV),]}
    if(input$trav_S2){current<-current[(current$TRAV >= input$radRange_S2[1] & current$TRAV <= input$radRange_S2[2]) & !is.na(current$TRAV),]}
    if(input$top_S2){current<-current[(current$TOP >= input$optRange_S2[1] & current$TOP <= input$optRange_S2[2]) & !is.na(current$TOP),]}
    if(input$rh_S2){current<-current[(current$RH >= input$RH_S2[1] & current$RH <= input$RH_S2[2]) & !is.na(current$RH),]}
    if(input$velfilter_S2){current<-current[(current$VELAV >= input$vel_S2[1] & current$VELAV <= input$vel_S2[2]) & !is.na(current$VELAV),]}
    current
  })
  
  
  #filter human factor
  filterControl_S2<- reactive({
    current <- filterCond_S2()
    if(input$winfilter_S2){current<-current[current$PCEC1 %in% input$win_S2 & !is.na(current$PCEC1),]}
    if(input$extfilter_S2){current<-current[current$PCEC2 %in% input$ext_S2 & !is.na(current$PCEC2),]}
    if(input$intfilter_S2){current<-current[current$PCEC3 %in% input$int_S2 & !is.na(current$PCEC3),]}
    if(input$thermofilter_S2){current<-current[current$PCEC4 %in% input$thermo_S2 & !is.na(current$PCEC4),]}
    if(input$curfilter_S2){current<-current[current$PCEC5 %in% input$cur_S2 & !is.na(current$PCEC5),]}
    if(input$heatfilter_S2){current<-current[current$PCEC6 %in% input$heat_S2 & !is.na(current$PCEC6),]}
    if(input$fanfilter_S2){current<-current[current$PCEC7 %in% input$fan_S2 & !is.na(current$PCEC7),]}
    current
  })
  
#   #possible controls
#   output$controls_S2 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond_S2()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist_S2","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl_S2<-reactive({
#     current<-filterCond_S2()
#     if(input$pc_S2){
#       if(input$andor_S2=="and"){
#         if(length(input$pclist_S2)>1){
#           a<-apply(current[,input$pclist_S2],1,all)
#         } else if(length(input$pclist_S2==1)){
#           a<-current[,input$pclist_S2]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor_S2=="no"){
#         if(length(input$pclist_S2)>1){
#           a<-apply(current[,input$pclist_S2],1,any)
#         } else if(length(input$pclist_S2==1)){
#           a<-current[,input$pclist_S2]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor_S2=="or"){
#         if(length(input$pclist_S2)>1){
#           a<-apply(current[,input$pclist_S2],1,any)
#         } else if(length(input$pclist_S2==1)){
#           a<-current[,input$pclist_S2]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
  
  
  #filter misc
  filterMisc_S2<-reactive({
    current<-filterControl_S2()
    if(input$sexfilter_S2){
      current<-current[current$SEX %in% input$sex_S2 & !is.na(current$SEX),]}
    if(input$agefilter_S2){current<-current[(current$AGE >= input$age_S2[1] & current$AGE <= input$age_S2[2]) & !is.na(current$AGE),]}
    if(input$clofilter_S2){current<-current[(current$INSUL >= input$clo_S2[1] & current$INSUL <= input$clo_S2[2]) & !is.na(current$INSUL),]}
    if(input$metfilter_S2){current<-current[(current$MET >= input$met_S2[1] & current$MET <= input$met_S2[2]) & !is.na(current$MET),]}
    current
  })
  
  
  
  # Filter data for Scatter for S3
  filter_S3<-reactive({
    
    current<-db1
    current[(current$database %in% input$dataset_S3 & !is.na(current[,input$satmet_S])),]
    
  })
  
  #possible ventilation strategy
  output$type_S3 <- renderUI( {
    avail<-sort(unique(filter_S3()[,"cooling.strategy"]))
    checkboxGroupInput("type_S3","Conditioning types:",choices=avail,selected=avail)
  })
  
  #filter ventilation strategy
  filtertype_S3<-reactive({
    current<-filter_S3()
    current[current$cooling.strategy %in% input$type_S3,]
  })
  
  
  #possible BLTYPE
  output$BLTYPE_S3 <- renderUI( {
    avail<-sort(unique(filtertype_S3()[,"BLTYPE"]))
    checkboxGroupInput("BLTYPE_S3","Building types:",choices=avail,selected=avail)
  })
  
  #filter BLTYPE
  filterBLTYPE_S3<-reactive({
    current<-filtertype_S3()
    current[current$BLTYPE %in% input$BLTYPE_S3,]
  })
  
  
  #possible seasons
  output$seasons_S3 <- renderUI( {
    avail<-sort(unique(filterBLTYPE_S3()[,"season"]))
    checkboxGroupInput("seasons_S3","Seasons:",choices=avail,selected=avail)
  })
  
  #filter season
  filterSeason_S3<-reactive({
    current<-filterBLTYPE_S3()
    if (input$season_check_S3){
      current[current$season %in% input$seasons_S3,]
    } else {
      current
    }
  })
  
  #possible climates
  output$climates_S3 <- renderUI( {
    avail<-sort(unique(filterSeason_S3()[,"climate"]))
    checkboxGroupInput("climates_S3","Climates:",choices=avail, selected=avail)
  })
  
  #filter climate
  filterClimate_S3<-reactive({
    current<-filterSeason_S3()
    if (input$climate_check_S3){
      current[current$climate %in% input$climates_S3,]
    } else {
      current
    }
  })
  
  #possible countries
  output$countries_S3 <- renderUI( {
    avail<-sort(unique(filterClimate_S3()[,"country"]))
    checkboxGroupInput("countries_S3","Countries:",choices=avail, selected=avail)
  })
  
  #filter country
  filterCountry_S3<-reactive({
    current<-filterClimate_S3()
    if (input$country_check_S3){
      current[current$country %in% input$countries_S3,]
    } else {
      current
    }
  })
  
  
  #possible cities
  output$cities_S3 <- renderUI( {
    avail<-sort(unique(filterCountry_S3()[,"city"]))
    checkboxGroupInput("cities_S3","Cities:",choices=avail, selected=avail)
  })
  
  
  #filter cities
  filterCity_S3<-reactive({
    current<-filterCountry_S3()
    if (input$city_check_S3){
      current[current$city %in% input$cities_S3,]
    } else {
      current
    }
  })
  
  
  
  #filter measurement conditions
  filterCond_S3<-reactive({
    current<-filterCity_S3()
    if(input$prev_S3){current<-current[(current$prev_ta >= input$outRange_S3[1] & current$prev_ta <= input$outRange_S3[2]) & !is.na(current$prev_ta),]}
    if(input$taav_S3){current<-current[(current$TAAV >= input$inRange_S3[1] & current$TAAV <= input$inRange_S3[2]) & !is.na(current$TAAV),]}
    if(input$trav_S3){current<-current[(current$TRAV >= input$radRange_S3[1] & current$TRAV <= input$radRange_S3[2]) & !is.na(current$TRAV),]}
    if(input$top_S3){current<-current[(current$TOP >= input$optRange_S3[1] & current$TOP <= input$optRange_S3[2]) & !is.na(current$TOP),]}
    if(input$rh_S3){current<-current[(current$RH >= input$RH_S3[1] & current$RH <= input$RH_S3[2]) & !is.na(current$RH),]}
    if(input$velfilter_S3){current<-current[(current$VELAV >= input$vel_S3[1] & current$VELAV <= input$vel_S3[2]) & !is.na(current$VELAV),]}
    current
  })
  
  #filter human factor
  filterControl_S3<- reactive({
    current <- filterCond_S3()
    if(input$winfilter_S3){current<-current[current$PCEC1 %in% input$win_S3 & !is.na(current$PCEC1),]}
    if(input$extfilter_S3){current<-current[current$PCEC2 %in% input$ext_S3 & !is.na(current$PCEC2),]}
    if(input$intfilter_S3){current<-current[current$PCEC3 %in% input$int_S3 & !is.na(current$PCEC3),]}
    if(input$thermofilter_S3){current<-current[current$PCEC4 %in% input$thermo_S3 & !is.na(current$PCEC4),]}
    if(input$curfilter_S3){current<-current[current$PCEC5 %in% input$cur_S3 & !is.na(current$PCEC5),]}
    if(input$heatfilter_S3){current<-current[current$PCEC6 %in% input$heat_S3 & !is.na(current$PCEC6),]}
    if(input$fanfilter_S3){current<-current[current$PCEC7 %in% input$fan_S3 & !is.na(current$PCEC7),]}
    current
  })
  
  
#   #possible controls
#   output$controls_S3 <- renderUI( {
#     availControls<-ctrvarlist[apply(filterCond_S3()[,ctrvars],2,FUN=function(x)max(x,na.rm=T)>0)]
#     if(length(availControls>=1)){
#       checkboxGroupInput("pclist_S3","",choices=availControls,selected=availControls)}
#   })
#   
#   #filter controls
#   filterControl_S3<-reactive({
#     current<-filterCond_S3()
#     if(input$pc_S3){
#       if(input$andor_S3=="and"){
#         if(length(input$pclist_S3)>1){
#           a<-apply(current[,input$pclist_S3],1,all)
#         } else if(length(input$pclist_S3==1)){
#           a<-current[,input$pclist_S3]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       } else if (input$andor_S3=="no"){
#         if(length(input$pclist_S3)>1){
#           a<-apply(current[,input$pclist_S3],1,any)
#         } else if(length(input$pclist_S3==1)){
#           a<-current[,input$pclist_S3]
#         } else {
#           a<-F
#         }
#         current[!is.na(a) & !a,]
#       } else if (input$andor_S3=="or"){
#         if(length(input$pclist_S3)>1){
#           a<-apply(current[,input$pclist_S3],1,any)
#         } else if(length(input$pclist_S3==1)){
#           a<-current[,input$pclist_S3]
#         } else {
#           a<-T
#         }
#         current[!is.na(a) & a,]
#       }
#     } else{
#       current 
#     }
#   })
  
  
  #filter misc
  filterMisc_S3<-reactive({
    current<-filterControl_S3()
    if(input$sexfilter_S3){
      current<-current[current$SEX %in% input$sex_S3 & !is.na(current$SEX),]}
    if(input$agefilter_S3){current<-current[(current$AGE >= input$age_S3[1] & current$AGE <= input$age_S3[2]) & !is.na(current$AGE),]}
    if(input$clofilter_S3){current<-current[(current$INSUL >= input$clo_S3[1] & current$INSUL <= input$clo_S3[2]) & !is.na(current$INSUL),]}
    if(input$metfilter_S3){current<-current[(current$MET >= input$met_S3[1] & current$MET <= input$met_S3[2]) & !is.na(current$MET),]}
    current
  })
  
  
  
  
  # --------------------------------------------------------------------------------------------------------
  
  #Combine data for the elevate plots
  
  combineData<-reactive({
    
    data<-data.frame()
    
    columns <- c("TOP","TAAV","prev_ta","RH","VELAV","TOP_F","TAAV_F","prev_ta_F","VELAV_FPM","dayav_ta","dayav_ta_F","TRAV","TRAV_F","AGE","SEX","MET","INSUL","ASH","PMV","TSA","acc15","acc2","comf_ash_level","mci_level")
    if(input$compare_S1){
      d<-filterMisc_S1()[,columns]
      #hide the error message
      validate(need(nrow(d)>0,""))
      d$set<-"Set 1"
      data<-rbind(data,d)
      colors<-c(colors,A)
    }
    if(input$compare_S2){
      d<-filterMisc_S2()[,columns]
      validate(need(nrow(d)>0,""))
      d$set<-"Set 2"
      data<-rbind(data,d)
      colors<-c(colors,B)
    }
    if(input$compare_S3){
      d<-filterMisc_S3()[,columns]
      validate(need(nrow(d)>0,""))
      d$set<-"Set 3"
      data<-rbind(data,d)
      colors<-c(colors,C)
    }
    
    data
  })
  
  
  
  # make the elevPlot ------------------------------------------------------------------------------------------------------------------
  
  elevated <- reactive({
    
    elevdata<-combineData()
    validate(need(nrow(elevdata)>0, 'Calculating...'))
    
    
    if(input$units_S=="Metric"){
      y<-"VELAV"
      x<-input$xaxis_elev
      tempRange <- input$tempRange_S
    } else {
      y<-"VELAV_FPM"
      x<-paste0(input$xaxis_elev,"_F")
      tempRange <- input$tempRange_S_F
    }
    
    elevdata[(!is.na(elevdata[,x]) & !is.na(elevdata[,y])),]
    
    
    myfunM_C10_1 <- function(x){
      0.000000045069358 * exp(0.7225864 * x)
    }

    myfunM_C10_2 <- function(x){
      0.000000003922312 * exp(0.70106119 * x)
    }
    
    myfunM_C05_1 <- function(x){
      0.00000002882763 * exp(0.639261202929839 * x)
    }
    
    myfunM_C05_2 <- function(x){
      0.000000004923363 * exp(0.626809214972869 * x)
    }
    
    
    
    
    
    
    myfunI_C10_1 <- function(x){
      0.000000000023404 * exp(0.401421466676995 * x)
    }
    
    myfunI_C10_2 <- function(x){
      0.000000000002985 * exp(0.38947843890395 * x)
    }
    
    myfunI_C05_1 <- function(x){
      0.000000000065817 * exp(0.355145112738878 * x)
    }
    
    myfunI_C05_2 <- function(x){
      0.000000000014026 * exp(0.348227341651531 * x)
    }
    
    
    p<-ggplot(elevdata,aes_string(x=x,y=y,color="set"))+geom_point(alpha =0.4,size = 2)+
      scale_color_manual(values=c("Set 1"="#7CA0FF", "Set 2"="#B27D71", "Set 3"="#8AB24F"))+
      xlab(getLabel(x))+
      ylab(getLabel(y))+
      coord_cartesian(xlim=tempRange)+
      ggtitle("Elevated velocity model plot")
    
    
    #overlay the comfort zone clo0.5 and clo1.0
    if(input$elev10){
      if(input$units_S=="Metric"){
        p <- p + stat_function(fun=myfunM_C10_1, xlim=c(20.25,24.92), geom="line", linetype="dashed",color="black", size = 1) +
          stat_function(fun=myfunM_C10_2, xlim=c(24.42,29.17), geom="line", linetype="dashed",color="black", size = 1)+
          annotate(geom="segment",y=0.1,yend=0.1,x=20.25,xend=24.42,size=1,color="black", linetype="dashed")+
          annotate("text",x=23, y=2.5, size=5, color="black", label="1.0 clo")
        
      } else {
        p <- p + stat_function(fun=myfunI_C10_1, xlim=c(68.45,76.92), geom="line", linetype="dashed",color="black", size = 1) +
          stat_function(fun=myfunI_C10_2, xlim=c(76,84.57), geom="line", linetype="dashed",color="black", size = 1)+
          annotate(geom="segment",y=19.68,yend=19.68,x=68.45,xend=76,size=1,color="black", linetype="dashed")+
          annotate("text",x=72, y=450, size=5, color="black", label="1.0 clo")     
      }
    }
    
    
    
    
    if(input$elev05){
      if(input$units_S=="Metric"){
        p <- p + stat_function(fun=myfunM_C05_1, xlim=c(23.68,28.87), geom="line",color="darkblue", size = 1) +
          stat_function(fun=myfunM_C05_2, xlim=c(27,32.275), geom="line",color="darkblue", size = 1)+
          annotate(geom="segment",y=0.1,yend=0.1,x=23.68,xend=27,size=1,color="darkblue")+
          annotate("text",x=34, y=2.5 ,color="darkblue", size=5, label="0.5 clo")
      } else {
        p <- p + stat_function(fun=myfunI_C05_1, xlim=c(74.5,84.03), geom="line",color="darkblue", size = 1) +
          stat_function(fun=myfunI_C05_2, xlim=c(80.4,90.15), geom="line",color="darkblue", size = 1)+
          annotate(geom="segment",y=19.68,yend=19.68,x=74.5,xend=80.4,size=1,color="darkblue")+
          annotate("text",x=93, y=450, color="darkblue", size=5, label="0.5 clo") 
      }
    }
    
    
    
    
    
    p <- p
    
  })
  
  #send the plot to be seen!
  output$elevPlot <- renderPlot({
    print(elevated())
  })
  
  
  
  scatter_1 <- reactive({
    
    sca_1_data<-combineData()
    validate(need(nrow(sca_1_data)>0, 'Calculating...'))
    
    if(input$units_S == "Metric"){
      if(input$yaxis_sca_1 == "TOP"){
        y1 <- "TOP"
        y1lab <- "Operative temperature (°C)"
      }
      if(input$yaxis_sca_1 == "OutTemp"){
        y1 <- "prev_ta"
        y1lab <- "Prevailing mean outdoor temperature (°C)"
      }
      if(input$yaxis_sca_1 == "AirTemp"){
        y1 <- "TAAV"
        y1lab <- "Indoor Air temperature (°C)"
      }
      if(input$yaxis_sca_1 == "RH"){
        y1 <- "RH"
        y1lab <- "Relative humidity (%)"
      }
      if(input$yaxis_sca_1 == "AirSpeed"){
        y1 <- "VELAV"
        y1lab <- "Air Speed (m/s)"
      }
      if(input$yaxis_sca_1 == "MET"){
        y1 <- "MET"
        y1lab <- "Metabolic rate (met)"
      }
      if(input$yaxis_sca_1 == "INSUL"){
        y1 <- "INSUL"
        y1lab <- "Clothing insulation (clo)"
      }
      if(input$yaxis_sca_1 == "TSV"){
        y1 <- "ASH"
        y1lab <- "ASHRAE thermal sensation vote"
      }
      if(input$yaxis_sca_1 == "PMV"){
        y1 <- "PMV"
        y1lab <- "Predict mean vote"
      }
    } else{
      if(input$yaxis_sca_1 == "TOP"){
        y1 <- "TOP_F"
        y1lab <- "Operative temperature (°F)"
      }
      if(input$yaxis_sca_1 == "OutTemp"){
        y1 <- "prev_ta_F"
        y1lab <- "Prevailing mean outdoor temperature (°F)"
      }
      if(input$yaxis_sca_1 == "AirTemp"){
        y1 <- "TAAV_F"
        y1lab <- "Indoor Air temperature (°F)"
      }
      if(input$yaxis_sca_1 == "RH"){
        y1 <- "RH"
        y1lab <- "Relative humidity (%)"
      }
      if(input$yaxis_sca_1 == "AirSpeed"){
        y1 <- "VELAV_FPM"
        y1lab <- "Air Speed (fpm)"
      }
      if(input$yaxis_sca_1 == "MET"){
        y1 <- "MET"
        y1lab <- "Metabolic rate (met)"
      }
      if(input$yaxis_sca_1 == "INSUL"){
        y1 <- "INSUL"
        y1lab <- "Clothing insulation (clo)"
      }
      if(input$yaxis_sca_1 == "TSV"){
        y1 <- "ASH"
        y1lab <- "ASHRAE thermal sensation vote"
      }
      if(input$yaxis_sca_1 == "PMV"){
        y1 <- "PMV"
        y1lab <- "Predict mean vote"
      }
    }
    
    
    if(input$units_S == "Metric"){
      if(input$xaxis_sca_1 == "TOP"){
        x1 <- "TOP"
        x1lab <- "Operative temperature (°C)"
      }
      if(input$xaxis_sca_1 == "OutTemp"){
        x1 <- "prev_ta"
        x1lab <- "Prevailing mean outdoor temperature (°C)"
      }
      if(input$xaxis_sca_1 == "AirTemp"){
        x1 <- "TAAV"
        x1lab <- "Indoor Air temperature (°C)"
      }
      if(input$xaxis_sca_1 == "RH"){
        x1 <- "RH"
        x1lab <- "Relative humidity (%)"
      }
      if(input$xaxis_sca_1 == "AirSpeed"){
        x1 <- "VELAV"
        x1lab <- "Air Speed (m/s)"
      }
      if(input$xaxis_sca_1 == "MET"){
        x1 <- "MET"
        x1lab <- "Metabolic rate (met)"
      }
      if(input$xaxis_sca_1 == "INSUL"){
        x1 <- "INSUL"
        x1lab <- "Clothing insulation (clo)"
      }
      if(input$xaxis_sca_1 == "TSV"){
        x1 <- "ASH"
        x1lab <- "ASHRAE thermal sensation vote"
      }
      if(input$xaxis_sca_1 == "PMV"){
        x1 <- "PMV"
        x1lab <- "Predict mean vote"
      }
    } else{
      if(input$xaxis_sca_1 == "TOP"){
        x1 <- "TOP_F"
        x1lab <- "Operative temperature (°F)"
      }
      if(input$xaxis_sca_1 == "OutTemp"){
        x1 <- "prev_ta_F"
        x1lab <- "Prevailing mean outdoor temperature (°F)"
      }
      if(input$xaxis_sca_1 == "AirTemp"){
        x1 <- "TAAV_F"
        x1lab <- "Indoor Air temperature (°F)"
      }
      if(input$xaxis_sca_1 == "RH"){
        x1 <- "RH"
        x1lab <- "Relative humidity (%)"
      }
      if(input$xaxis_sca_1 == "AirSpeed"){
        x1 <- "VELAV_FPM"
        x1lab <- "Air Speed (fpm)"
      }
      if(input$xaxis_sca_1 == "MET"){
        x1 <- "MET"
        x1lab <- "Metabolic rate (met)"
      }
      if(input$xaxis_sca_1 == "INSUL"){
        x1 <- "INSUL"
        x1lab <- "Clothing insulation (clo)"
      }
      if(input$xaxis_sca_1 == "TSV"){
        x1 <- "ASH"
        x1lab <- "ASHRAE thermal sensation vote"
      }
      if(input$xaxis_sca_1 == "PMV"){
        x1 <- "PMV"
        x1lab <- "Predict mean vote"
      }
    }
    
    p <- ggplot(sca_1_data,aes_string(x=x1,y=y1,color="set"))+geom_point(alpha =0.2,size = 1.2)+
      scale_color_manual(values=c("Set 1"="#7CA0FF", "Set 2"="#B27D71", "Set 3"="#8AB24F"))+
      xlab(getLabel(x1))+
      ylab(getLabel(y1))+
      ggtitle("Scatter plot-1")
    
    
    if(input$sca_1_linear){
      p <- p + geom_smooth(method="loess",alpha=0.3, size=0.75)
      
    }
    p
    
  })
  
  #send the plot to be seen!
  output$scatterPlot_1 <- renderPlot({
    print(scatter_1())
  })
  
  
  scatter_2 <- reactive({
    
    sca_2_data<-combineData()
    validate(need(nrow(sca_2_data)>0, 'Calculating...'))
    
    if(input$units_S == "Metric"){
      if(input$yaxis_sca_2 == "TOP"){
        y2 <- "TOP"
        y2lab <- "Operative temperature (°C)"
      }
      if(input$yaxis_sca_2 == "OutTemp"){
        y2 <- "prev_ta"
        y2lab <- "Prevailing mean outdoor temperature (°C)"
      }
      if(input$yaxis_sca_2 == "AirTemp"){
        y2 <- "TAAV"
        y2lab <- "Indoor Air temperature (°C)"
      }
      if(input$yaxis_sca_2 == "RH"){
        y2 <- "RH"
        y2lab <- "Relative humidity (%)"
      }
      if(input$yaxis_sca_2 == "AirSpeed"){
        y2 <- "VELAV"
        y2lab <- "Air Speed (m/s)"
      }
      if(input$yaxis_sca_2 == "MET"){
        y2 <- "MET"
        y2lab <- "Metabolic rate (met)"
      }
      if(input$yaxis_sca_2 == "INSUL"){
        y2 <- "INSUL"
        y2lab <- "Clothing insulation (clo)"
      }
      if(input$yaxis_sca_2 == "TSV"){
        y2 <- "ASH"
        y2lab <- "ASHRAE thermal sensation vote"
      }
      if(input$yaxis_sca_2 == "PMV"){
        y2 <- "PMV"
        y2lab <- "Predict mean vote"
      }
    } else{
      if(input$yaxis_sca_2 == "TOP"){
        y2 <- "TOP_F"
        y2lab <- "Operative temperature (°F)"
      }
      if(input$yaxis_sca_2 == "OutTemp"){
        y2 <- "prev_ta_F"
        y2lab <- "Prevailing mean outdoor temperature (°F)"
      }
      if(input$yaxis_sca_2 == "AirTemp"){
        y2 <- "TAAV_F"
        y2lab <- "Indoor Air temperature (°F)"
      }
      if(input$yaxis_sca_2 == "RH"){
        y2 <- "RH"
        y2lab <- "Relative humidity (%)"
      }
      if(input$yaxis_sca_2 == "AirSpeed"){
        y2 <- "VELAV_FPM"
        y2lab <- "Air Speed (fpm)"
      }
      if(input$yaxis_sca_2 == "MET"){
        y2 <- "MET"
        y2lab <- "Metabolic rate (met)"
      }
      if(input$yaxis_sca_2 == "INSUL"){
        y2 <- "INSUL"
        y2lab <- "Clothing insulation (clo)"
      }
      if(input$yaxis_sca_2 == "TSV"){
        y2 <- "ASH"
        y2lab <- "ASHRAE thermal sensation vote"
      }
      if(input$yaxis_sca_2 == "PMV"){
        y2 <- "PMV"
        y2lab <- "Predict mean vote"
      }
    }
    
    
    if(input$units_S == "Metric"){
      if(input$xaxis_sca_2 == "TOP"){
        x2 <- "TOP"
        x2lab <- "Operative temperature (°C)"
      }
      if(input$xaxis_sca_2 == "OutTemp"){
        x2 <- "prev_ta"
        x2lab <- "Prevailing mean outdoor temperature (°C)"
      }
      if(input$xaxis_sca_2 == "AirTemp"){
        x2 <- "TAAV"
        x2lab <- "Indoor Air temperature (°C)"
      }
      if(input$xaxis_sca_2 == "RH"){
        x2 <- "RH"
        x2lab <- "Relative humidity (%)"
      }
      if(input$xaxis_sca_2 == "AirSpeed"){
        x2 <- "VELAV"
        x2lab <- "Air Speed (m/s)"
      }
      if(input$xaxis_sca_2 == "MET"){
        x2 <- "MET"
        x2lab <- "Metabolic rate (met)"
      }
      if(input$xaxis_sca_2 == "INSUL"){
        x2 <- "INSUL"
        x2lab <- "Clothing insulation (clo)"
      }
      if(input$xaxis_sca_2 == "TSV"){
        x2 <- "ASH"
        x2lab <- "ASHRAE thermal sensation vote"
      }
      if(input$xaxis_sca_2 == "PMV"){
        x2 <- "PMV"
        x2lab <- "Predict mean vote"
      }
    } else{
      if(input$xaxis_sca_2 == "TOP"){
        x2 <- "TOP_F"
        x2lab <- "Operative temperature (°F)"
      }
      if(input$xaxis_sca_2 == "OutTemp"){
        x2 <- "prev_ta_F"
        x2lab <- "Prevailing mean outdoor temperature (°F)"
      }
      if(input$xaxis_sca_2 == "AirTemp"){
        x2 <- "TAAV_F"
        x2lab <- "Indoor Air temperature (°F)"
      }
      if(input$xaxis_sca_2 == "RH"){
        x2 <- "RH"
        x2lab <- "Relative humidity (%)"
      }
      if(input$xaxis_sca_2 == "AirSpeed"){
        x2 <- "VELAV_FPM"
        x2lab <- "Air Speed (fpm)"
      }
      if(input$xaxis_sca_2 == "MET"){
        x2 <- "MET"
        x2lab <- "Metabolic rate (met)"
      }
      if(input$xaxis_sca_2 == "INSUL"){
        x2 <- "INSUL"
        x2lab <- "Clothing insulation (clo)"
      }
      if(input$xaxis_sca_2 == "TSV"){
        x2 <- "ASH"
        x2lab <- "ASHRAE thermal sensation vote"
      }
      if(input$xaxis_sca_2 == "PMV"){
        x2 <- "PMV"
        x2lab <- "Predict mean vote"
      }
    }
    
    p<- ggplot(sca_2_data,aes_string(x=x2,y=y2,color="set"))+geom_point(alpha =0.3,size = 1.2)+
      scale_color_manual(values=c("Set 1"="#7CA0FF", "Set 2"="#B27D71", "Set 3"="#8AB24F"))+
      xlab(getLabel(x2))+
      ylab(getLabel(y2))+
      ggtitle("Scatter plot-2")
    
    
    if(input$sca_2_linear){
      p <- p + stat_smooth(method="loess",alpha=0.3, size=0.75)
    }
    
    p
  })
  
  #send the plot to be seen!
  output$scatterPlot_2 <- renderPlot({
    print(scatter_2())
  })
  
  #make the download button work
  output$downloadPlot_S <- downloadHandler(
    filename = function() { paste('Scatter.pdf', sep='') },
    content = function(file) {
      pdf(file,height=4,width=5.7)
      print(elevated())
      print(scatter_1())
      print(scatter_2())
      dev.off()
    })
  
})