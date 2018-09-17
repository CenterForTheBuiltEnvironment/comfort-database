require(shiny)
#library(shinyapps)
require(shinyIncubator)



#function to create the interface for filtering the sample sets
filterOptions<-function(id,data_select,filterTemp,filterAcc){
  
  
  tabsetPanel(
    # position="left", # the "position" function is deprecated"
              
              tabPanel(id=paste0("bldg",id),title="building",
                       
                          wellPanel(
                                checkboxGroupInput(paste0("dataset",id), "Database:",
                                                     list("Database 1" = "Database 1",
                                                          "Database 2" = "Database 2"),
                                                     selected=data_select)),    

                       
                       if(filterTemp){
                         if(filterAcc){sliderInput
                           wellPanel(
                             selectInput(paste0("metric",id), "Satisfaction metric:",
                                         list("Acceptability" = "tsa_ash_level", 
                                              # "Sensation +-1.5" = "ash15_level", 
                                              "Comfort" = "comf_ash_level"),
                                              # "Preference" = "mci_level"),
                                         selectize=F, selected="tsa_ash_level")
                           )
                         }
                       },

                       wellPanel(
                         uiOutput(paste0("type",id))),
                       
                       wellPanel(
                         uiOutput(paste0("BLTYPE",id))
                       )
              ),
              
              tabPanel(id=paste0("geog",id),title="geography",
                       wellPanel(
                         checkboxInput(paste0("season_check",id),"Filter by seasons"),
                         conditionalPanel(condition = paste0("input.season_check",id),
                                          uiOutput(paste0("seasons",id))),
                         checkboxInput(paste0("climate_check",id),"Filter by climates"),
                         conditionalPanel(condition = paste0("input.climate_check",id),
                                          uiOutput(paste0("climates",id))),
                         checkboxInput(paste0("country_check",id),"Filter by countries"),
                         conditionalPanel(condition = paste0("input.country_check",id),
                                          uiOutput(paste0("countries",id))),
                         checkboxInput(paste0("city_check",id),"Filter by cities"),
                         conditionalPanel(condition = paste0("input.city_check",id),
                                          uiOutput(paste0("cities",id)))
                       )
              ),
              
              
              tabPanel(id=paste0("conditions",id),title="conditions",
                       
                       #only include the temperature filters for the probits since they're controlled as axis limits for the 2d Adaptive model
                       if(filterTemp){
                         wellPanel(
                           
                           checkboxInput(paste0("prev",id),"Filter by Monthly mean outdoor temperature"),
                           conditionalPanel(condition=paste0("input.prev",id, "& input.units", substring(id,0,nchar(id)-1),"=='Metric'"),
                                            sliderInput(paste0("outRange",id), label="", 
                                                        min = -25, max = 50, value = c(-25,50), step= 1, post="°C")),
                           conditionalPanel(condition=paste0("input.prev",id,"& input.units", substring(id,0,nchar(id)-1),"=='Imperial'"),
                                            sliderInput(paste0("outRange_F",id), label="", 
                                                        min = -13, max = 104, value = c(-13,104), step= 1, post="°C")),
                           
                           checkboxInput(paste0("taav",id),"Filter by indoor temperature"),
                           conditionalPanel(condition=paste0("input.taav",id, "& input.units", substring(id,0,nchar(id)-1),"=='Metric'"),
                                            sliderInput(paste0("inRange",id), label="", 
                                                        min = -25, max = 50, value = c(-25,50), step= 1, post="°C")),
                           conditionalPanel(condition=paste0("input.taav",id,"& input.units", substring(id,0,nchar(id)-1),"=='Imperial'"),
                                            sliderInput(paste0("inRange_F",id), label="", 
                                                        min = -13, max = 104, value = c(-13,104), step= 1, post="°F")),
                           
                           checkboxInput(paste0("trav",id),"Filter by radiant temperature"),
                           conditionalPanel(condition=paste0("input.trav",id, "& input.units", substring(id,0,nchar(id)-1),"=='Metric'"),
                                            sliderInput(paste0("radRange",id), label="", 
                                                        min = -25, max = 50, value = c(-25,50), step= 1, post="°C")),
                           conditionalPanel(condition=paste0("input.trav",id,"& input.units", substring(id,0,nchar(id)-1),"=='Imperial'"),
                                            sliderInput(paste0("radRange_F",id), label="", 
                                                        min = -13, max = 104, value = c(-13,104), step= 1, post="°F")),
                           
                           
                           checkboxInput(paste0("top",id),"Filter by operative temperature"),
                           conditionalPanel(condition=paste0("input.top",id, "& input.units", substring(id,0,nchar(id)-1),"=='Metric'"),
                                            sliderInput(paste0("optRange",id), label="", 
                                                        min = 0, max = 50, value = c(0,50), step= 1, post="°C")),
                           conditionalPanel(condition=paste0("input.top",id,"& input.units", substring(id,0,nchar(id)-1),"=='Imperial'"),
                                            sliderInput(paste0("optRange_F",id), label="", 
                                                        min = 32, max = 104, value = c(32,104), step= 1, post="°F"))
                           
                         )
                       },
                       wellPanel(
                         checkboxInput(paste0("rh",id),"Filter by relative humidity"),
                         conditionalPanel(condition=paste0("input.rh",id),
                                          sliderInput(paste0("RH",id), "", 
                                                      min = 0, max = 100, value = c(0,100), step= 1, round = TRUE, post="%")),
                         checkboxInput(paste0("velfilter",id),"Filter by air speed"),
                         conditionalPanel(condition=paste0("input.velfilter",id, "& input.units", substring(id,0,nchar(id)-1),"=='Metric'"),
                                          sliderInput(paste0("vel",id), "", 
                                                      min = 0, max = 6, value = c(0,2), step= 0.1,post="m/s")),
                         conditionalPanel(condition=paste0("input.velfilter",id, "& input.units", substring(id,0,nchar(id)-1),"=='Imperial'"),
                                          sliderInput(paste0("vel_fpm",id), "", 
                                                      min = 0, max = 1180, value = c(0,400), step= 10,post="fpm")))
              ),
              
              
              tabPanel(id=paste0("other",id),title="human factors",

                       wellPanel(
                         checkboxInput(paste0("curfilter",id),"Filter by blind (curtain) control"),
                         conditionalPanel(condition=paste0("input.curfilter",id),
                                          checkboxGroupInput(paste0("cur",id),label="",choices=list("Open" = "0","Close" = "1"),selected=c("0","1"))),
                         checkboxInput(paste0("fanfilter",id),"Filter by fan control"),
                         conditionalPanel(condition=paste0("input.fanfilter",id),
                                          checkboxGroupInput(paste0("fan",id),label="",choices=list("Off" = "0","On" = "1"),selected=c("0","1"))),
                         checkboxInput(paste0("winfilter",id),"Filter by window control"),
                         conditionalPanel(condition=paste0("input.winfilter",id),
                                          checkboxGroupInput(paste0("win",id),label="",choices=list("Open" = "0","Close" = "1"),selected=c("0","1"))),
                         checkboxInput(paste0("doorfilter",id),"Filter by door control"),
                         conditionalPanel(condition=paste0("input.doorfilter",id),
                                          checkboxGroupInput(paste0("door",id),label="",choices=list("Open" = "0","Close" = "1"),selected=c("0","1"))),
                         checkboxInput(paste0("heatfilter",id),"Filter by heater control"),
                         conditionalPanel(condition=paste0("input.heatfilter",id),
                                          checkboxGroupInput(paste0("heat",id),label="",choices=list("Off" = "0","On" = "1"),selected=c("0","1")))
                       ),
                       
                       wellPanel(
                         checkboxInput(paste0("sexfilter",id),"Filter by sex"),
                         conditionalPanel(condition=paste0("input.sexfilter",id),
                                          #checkboxGroupInput(paste0("sex",id),label="",choices=list("Female" = "1","Male" = "0"),selected=c("1","0"))),
                                          checkboxGroupInput(paste0("sex",id),label="",choices=list("Female" = "Female","Male" = "Male"),selected=c("Female","Male"))),
                         checkboxInput(paste0("agefilter",id),"Filter by age"),
                         conditionalPanel(condition=paste0("input.agefilter",id),
                                          sliderInput(paste0("age",id), "", 
                                                      min = 10, max = 100, value = c(20,90), step= 1)),
                         checkboxInput(paste0("clofilter",id),"Filter by clothing insulation"),
                         conditionalPanel(condition=paste0("input.clofilter",id),
                                          sliderInput(paste0("clo",id), "", 
                                                      min = 0.2, max = 2.3, value = c(0.2,2.3), step= 0.1)),
                         checkboxInput(paste0("metfilter",id),"Filter by metabolic rate"),
                         conditionalPanel(condition=paste0("input.metfilter",id),
                                          sliderInput(paste0("met",id), "", 
                                                      min = 0.8, max = 4.5, value = c(0.8,4.5), step= 0.1))
                       )
                       
              )
  )
}

#probitSample<-function(id,color,data_select){
probitSample<-function(id,color,data_select,active=T){
  column(12,
         withTags(p(checkboxInput(paste0("compare",id),paste("Sample set",id),value=active),style=paste0("background-color:",color))),
         conditionalPanel(condition=paste0("input.compare",id),
                          filterOptions(id,data_select,TRUE,TRUE)
         )
  )
  
}

shinyUI(
  
  navbarPage("ASHRAE Global Thermal Comfort Database II Visualization",
             
             tabPanel("Satisfaction", fluidPage(
               fluidRow(
                 column(2,
                        
                        selectInput("xaxis", "Model selection (X-axis):",
                                    list("Thermal sensation" = "ASH",
                                         "PMV" = "PMV"),
                                    selectize=F)
                 ),
                 column(2,
                        checkboxInput("showCI","Show 95% confidence intervals",TRUE),
                        checkboxInput("descriptive","Show descriptive plots",value=TRUE)
                        
                 ),
                 
                 column(1,
                        radioButtons("units","",choices=c("Metric","Imperial"))),
                 
                 column(3,
                        selectInput("xstat","Select the parameter for bar chart",
                                    list("Country" = "country",
                                         "Ventilation type" = "cooling.strategy",
                                         "Building type" = "BLTYPE",
                                         "City"="city",
                                         "Climate"="climate",
                                         "Season"="season",
                                         "SEX"="SEX"),
                                    selectize=F)
                 ),
                 
                 
#                  column(2,
#                         div("Press 4 times to initiate the program", style = "color:red"),
#                         actionButton("go","Run")
#                         ),
                 
                 
                 column(4,
                        downloadButton('downloadPlot', 'Download plot')
                 )),
               hr(),
               
               fluidRow(
                 column(4,
                        plotOutput("pmvPlot")),
                 withTags(column(4,
                                 conditionalPanel(condition="input.descriptive",
                                                  plotOutput("cityPlotted")
                                 )
                                 #style="overflow-y:scroll;height=400")),
                 )),
                 withTags(column(3,
                                 conditionalPanel(condition="input.descriptive",
                                                  plotOutput("boxplot1",height=200),
                                                  plotOutput("boxplot2",height=200)
                                 ))),
                 withTags(column(1,
                                 conditionalPanel(condition="input.descriptive",
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  #br(),                                                       
                                                  selectInput("boxplot1","boxplot1 x-axis",
                                                              list("Op. Temp"="TOP",
                                                                   "Air Temp"="AirTemp",
                                                                   "Outdoor Temp"="OutTemp",
                                                                   "Relative humidity"="RH",
                                                                   "Air Speed"="AirSpeed",
                                                                   "Metabolic rate"="MET",
                                                                   "Clothing insulation"="INSUL",
                                                                   "AGE"="AGE",
                                                                   "TSV"="TSV",
                                                                   "PMV"="PMV"), selectize=F, selected="TOP"),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  br(),
                                                  selectInput("boxplot2","boxplot2 x-axis",
                                                              list("Op. Temp"="TOP",
                                                                   "Air Temp"="AirTemp",
                                                                   "Outdoor Temp"="OutTemp",
                                                                   "Relative humidity"="RH",
                                                                   "Air Speed"="AirSpeed",
                                                                   "Metabolic rate"="MET",
                                                                   "Clothing insulation"="INSUL",
                                                                   "AGE"="AGE",
                                                                   "TSV"="TSV",
                                                                   "PMV"="PMV"), selectize=F, selected="AirTemp"))))
               ),
fluidRow(

HTML('<hr style="border-color: black;">')

  ),

# HTML('<hr style="border-color: black;">'),

               fluidRow(                 
                 
                 column(4,
                        probitSample("1","rgba(124,160,255,.5)","Database 2")
                 ),
                 
                 column(4,
                        probitSample("2","rgba(178,125,113,.5)","Database 1")
                 ),
                 
                 column(4,
                        probitSample("3","rgba(138,178,93,.5)","Database 2",F)
                 )
               )
             )
             ),
             
             tabPanel("Adaptive model",fluidPage(
               fluidRow(
                 
                 column(3,
                        selectInput(inputId="satmet", label="Satisfaction metric:",
                                    choices=list(#"All data" = "ASH",
                                                 "Acceptability (all range)" = "TSA",
                                                 "Acceptability (TSV+-1.5)" = "acc15",
                                                 "Acceptability (TSV+-2)" = "acc2", 
                                                 "General thermal comfort" = "acc_comf"),selectize=F),
                        numericInput("binwidth", "Width of temperature bins:",1),   
                        
                        checkboxInput("sizeN","Vary size based on N",value=T),
                        radioButtons("units_bins","",choices=c("Metric","Imperial"))
                        
                 ),
                 column(3,
                        selectInput("xaxis_bins", "Outdoor temperature:",
                                    list("Monthly mean" = "prev_ta"
                                         # "Mean daily" = "dayav_ta"),selectize=F),
                                          ),selectize=F),
                        
                        conditionalPanel(condition="input.units_bins=='Metric'",
                                         sliderInput("outRange_bins", "Range of outdoor temperature:", 
                                                     min = -25, max = 50, value = c(-25,40), step= 1, post="°C")),
                        
                        conditionalPanel(condition="input.units_bins=='Imperial'",
                                         sliderInput("outRange_bins_F", "Range of outdoor temperature:", 
                                                     min = -13, max = 122, value = c(-13,104), step= 1, post="°F")),
                        
                        checkboxInput("ashrae","Show ASHRAE 55 comfort zone",value=F),
                        checkboxInput("en","Show EN 15251 comfort zone", value=F)
                 ),
                 column(3,
                        selectInput("yaxis_bins", "Indoor temperature:",
                                    list("Operative" = "TOP",
                                         "Air" = "TAAV",
                                         "Radiant" = "TRAV"),selectize=F),
                        conditionalPanel(condition="input.units_bins=='Metric'",
                                         
                                         sliderInput("inRange_bins", "Range of indoor temperature:", 
                                                     min = 0, max = 50, value = c(6,46), step= 1, post="°C")
                        ),
                        
                        conditionalPanel(condition="input.units_bins=='Imperial'",
                                         
                                         sliderInput("inRange_bins_F", "Range of indoor temperature:", 
                                                     min = 32, max = 122, value = c(42,115), step= 1, post="°F")
                        )),
                 
                 
                 column(3,
                        downloadButton('downloadPlot_bins', 'Download plot(s)')
                 )
               ),
               hr(),

               
               
               fluidRow(
                 column(6,
                        plotOutput("heatPlot")
                 ),
                 column(6,
                        conditionalPanel(condition="input.compare_bins",
                                         plotOutput("heatPlot2")
                        )
                 )
               ),
               
               HTML('<hr style="border-color: black;">'),
               
               fluidRow(
                 column(4,
                        br(),
                        filterOptions("_bins1","Database 2",F,F)
                 ),
                 column(2,
                        br()
                 ),
                 column(4,
                        checkboxInput("compare_bins","Compare",value=F),
                        conditionalPanel(condition="input.compare_bins",
                                         filterOptions("_bins2","Database 2",F,F))                     
                 ),
                 column(2)
               )
             )
             
             ),
             
             #-------------------------------------------------Scatter graph ----------------------------------------------------------------
             
             tabPanel("Scatter",fluidPage(
               fluidRow(
                 
                 column(2,
                        selectInput(inputId="satmet_S", label="Satisfaction metric:",
                                    choices=list("All data" = "ASH", 
                                                 "Acceptability" = "TSA",
                                                 "Acceptability (TSV+-1.5)" = "acc15", 
                                                 # "Preference" = "mci_level", 
                                                 "Comfort" = "comf_ash_level",
                                                 "PMV"="PMV"),selectize=F),
                        
                        radioButtons("units_S","",choices=c("Metric","Imperial"))
                        
                 ),
                 
                 column(3,
                        selectInput("xaxis_elev", "Selected x-axis temperature:",
                                    list("Operative" = "TOP",
                                         "Air" = "TAAV",
                                         "Monthly mean outdoor" = "prev_ta",
                                         "Radiant" = "TRAV"),selectize=F),
                        
                        conditionalPanel(condition="input.units_S=='Metric'",
                                         sliderInput("tempRange_S", "Range of temperature in x-axis:", 
                                                     min = -25, max = 50, value = c(10,40), step= 1, post="°C")),
                        
                        conditionalPanel(condition="input.units_S=='Imperial'",
                                         sliderInput("tempRange_S_F", "Range of temperature in x-axis:", 
                                                     min = -13, max = 122, value = c(45,110), step= 1, post="°F")),
                        
                        conditionalPanel(condition="input.xaxis_elev == 'TOP'",
                                         checkboxInput("elev05","Show elevated velocity comfort zone (0.5clo)", value=F),
                                         checkboxInput("elev10","Show elevated velocity comfort zone (1.0clo)", value=F))
                        
                 ),
                 
                 column(3,
                        selectInput("yaxis_sca_1", "Selected y-axis for plot-1:",
                                    list("Operative Temp."="TOP",
                                         "Air Temp."="AirTemp",
                                         "Outdoor Temp."="OutTemp",
                                         "Relative humidity"="RH",
                                         "Air Speed"="AirSpeed",
                                         "Metabolic rate"="MET",
                                         "Clothing insulation"="INSUL",
                                         "TSV"="TSV",
                                         "PMV"="PMV"), selectize=F, selected="PMV"),
                        selectInput("xaxis_sca_1", "Selected x-axis for plot-1:",
                                    list("Operative Temp."="TOP",
                                         "Air Temp."="AirTemp",
                                         "Outdoor Temp."="OutTemp",
                                         "Relative humidity"="RH",
                                         "Air Speed"="AirSpeed",
                                         "Metabolic rate"="MET",
                                         "Clothing insulation"="INSUL",
                                         "TSV"="TSV",
                                         "PMV"="PMV"), selectize=F, selected="AirTemp"),
                        checkboxInput("sca_1_linear","Show regression line in plot-1", value=F),
                        p("(It may take a while in loading regression line "),
                        p("for large dataset)")
                 ),
                 
                 column(2,
                        selectInput("yaxis_sca_2", "Selected y-axis for plot-2:",
                                    list("Operative Temp"="TOP",
                                         "Air Temp"="AirTemp",
                                         "Outdoor Temp"="OutTemp",
                                         "Relative humidity"="RH",
                                         "Air Speed"="AirSpeed",
                                         "Metabolic rate"="MET",
                                         "Clothing insulation"="INSUL",
                                         "TSV"="TSV",
                                         "PMV"="PMV"), selectize=F, selected="TSV"),
                        selectInput("xaxis_sca_2", "Selected x-axis for plot-2:",
                                    list("Operative Temp."="TOP",
                                         "Air Temp."="AirTemp",
                                         "Outdoor Temp."="OutTemp",
                                         "Relative humidity"="RH",
                                         "Air Speed"="AirSpeed",
                                         "Metabolic rate"="MET",
                                         "Clothing insulation"="INSUL",
                                         "TSV"="TSV",
                                         "PMV"="PMV"), selectize=F, selected="AirTemp"),
                        checkboxInput("sca_2_linear","Show regression line in plot-2", value=F),
                        p("(It may take a while in loading regression line"),
                        p("for large dataset)")
                 ),                      
                 
                 column(2,
                        downloadButton('downloadPlot_S', 'Download plot(s)')
                 )
               ),
               hr(),

               
               fluidRow(
                 column(4,
                        plotOutput("elevPlot")
                 ),
                 column(4,
                        plotOutput("scatterPlot_1")
                 ),
                 column(4,
                        plotOutput("scatterPlot_2")
                 )
               ),
               
               HTML('<hr style="border-color: black;">'),
               
               fluidRow(
                 column(4,
                        checkboxInput("compare_S1","Sample set 1",value=TRUE),
                        conditionalPanel(condition="input.compare_S1",
                                         filterOptions("_S1","Database 1",T,F))
                 ),
                 column(4,
                        checkboxInput("compare_S2","Sample set 2",value=F),
                        conditionalPanel(condition="input.compare_S2",
                                         filterOptions("_S2","Database 2",T,F))   
                 ),
                 column(4,
                        checkboxInput("compare_S3","Sample set 3",value=F),
                        conditionalPanel(condition="input.compare_S3",
                                         filterOptions("_S3","Database 2",T,F)) 
                 )
               )
             )
             
             ),
             
             # ------------------------------------------------------------------------------------------------------------------
             
             
             
tabPanel("About",fluidPage(
  fluidRow(
    h4(""),
    h4("Data description"),
    p("The data shown in this tool is a compilation of many individual thermal comfort field studies in which building occupants were surveyed about their thermal comfort while simultaneous physical measurements were taken.
      The data is contributed by multiple independent studies, not all information are available for each observation.
      The database used in this tool is named 'ASHRAE Global Thermal Comfort Database II', which is a combination of ASHRAE Comfort Database I and Database II, and it is a public-domain resource."),
    p("The ASHRAE Comfort Database I was built up from raw data files generated by researchers in the 1990s.
      This approach allowed a variety of quality controls to be applied and enhanced the consistency and integrity of the entire database. 
      The ASHRAE Comfort Database I comprised over 21,000 rows of paired subjective comfort votes and objective instrumental measurements of concurrent indoor climatic parameters (de Dear et al, 1998)."),
    p("Recognizing the value of open-source research databases, in 2014, project of the ASHRAE Comfort Database II was launched that took on the challenge of systematically collecting and harmonizing the raw data collected in comfort field studies since 1997 (when the ASHRAE database I was built) from around the world into an expanded, quality-assured database.
      More than thirty-five thermal comfort research groups from around the world generously made their raw data freely available to the project and in total contributed with more than 81,000 rows of paired subjective comfort votes and objective instrumental measurements of thermal comfort parameters."),
    br(),
    
    h4("Classification of the data"),
    div(HTML("<ol>
             <li>Database data</li>
             <ul>
             <li>Database 1</li>
             <li>Database 2</li>
             </ul>
             <li>Building level data</li>
             <ul>
             <li>Building locations: Country (N>23), City (N>65)</li>
             <li>Climate:            Season (Spring, Summer, Autumn, Winter), Climatic zone </li>
             <li>Conditioning types: Air-conditioned, Naturally ventilated, Mixed mode, Mechanical ventilated</li>
             <li>Building types:     Multifamily housing, Office, Classroom, Senior center, Others </li>
             </ul>
             <li>Survey information</li>
             <ul>
             <li>Thermal sensation: How do you feel right now? (-3: cold to +3: hot)</li>
             <li>Thermal acceptability: Right now, is the temperature around you acceptable? (yes or no)</li>
             <li>Thermal comfort: Right now, do you feel thermally comfortable? (yes or no)</li>
             <li>Thermal preference: Right now, would you prefer to be cooler, the same, or warmer?</li>
             <li>Environment control status to blind/curtain, fan, window, door and heater</li>
             <li>Clothing insulation</li>
             <li>Metabolic rate (Activity status)</li>
             <li>Demographic (Sex and Age)</li>
             </ul>
             <li>Physical measurements</li>
             <ul>
             <li>Indoor temperature: Air, Radiant, Operative</li>
             <li>Outdoor temperature: Monthly mean</li>
             <li>Indoor relative humidity</li>
             <li>Indoor air speed</li>
             </ul>
             </ol>")),
    br(),                 
    
    h4("Detail tool descriptions"),
    div(HTML("<ol>
             <li>Introduction</li>
             <p> Large sets of thermal comfort field data have been analysed in detail to inform generalized thermal comfort standards, but there is specific information that might be relevant to particular projects that is not easily accessed by practitioners. This interactive tool allows people to explore the data and look at the subsets due to individual interest regarding the difference between database I and II, space conditioning type, building type, geographical locations, human characteristics, etc. The tool is divided into three pages, including:</p>
             <ol type=i>
             <li>“Satisfaction” tab:</li>
             <p> To display the subset statistic data and to show the probit curves of dissatisfaction percentage against thermal sensation and PMV scale. </p>
             <li>“Adaptive model” tab:</li>
             <p> To perform the satisfaction percentages with combinations of indoor and outdoor temperature for comparison with adaptive comfort standards. </p>
             <li>“Scatter” tab:</li>
             <p> To evaluate the subset data by scatter plots for any possible correlations and to display the air speed against different kinds of temperature for comparison with the elevated velocity comfort zone in ASHRAE Standard 55. </p>
             </ol>
             <li>Subset parameters</li>
             <p>The choices of subset a database can be classified into 4 categories: building, geography, conditions and human factors. The “building” tab controls the major classification of the dataset including the choice of database, satisfaction metric, conditioning types and building types. The “geography“ tab describes the locations and climatic characteristics for the selected data including seasons, climates, countries, and cities. The “conditions“ tab helps to bound the physical parameter of selected data in desire ranges, such as monthly mean outdoor, indoor, radiant and operative temperature, indoor relative humidity and indoor air speed. Lastly, the “human factors“ tab clarifies the characteristics of subjects during the survey, including sex, age, clothing insulation and metabolic rate. Besides, the possibility of environmental controls, if provided in corresponding study, are considered, such as blind (curtain), fan, window, door and heater.</p>
             <li>Satisfaction tab</li>
             <p>ASHRAE 55 defines thermal comfort as the “condition of mind that expresses satisfaction with the thermal environment and is assessed by subjective evaluation“. Since most field studies don't ask directly about satisfaction with the thermal environment, researchers look to questions about thermal sensation, acceptability and comfort to assess satisfaction. The “Satisfaction“ tab explores the relation between sensation and the other satisfaction metrics using multinomial probits. The probit plot displays curves of dissatisfaction percentages against the subject's thermal sensation vote and PMV based on the satisfaction metrics of acceptability and comfort. In addition, the statistical data of selected subset for observed factors and recorded parameters is respectively summarized in a bar chart and two boxplots.</p>
             <li>Adaptive model tab</li>
             <p>Adaptive comfort standards in ASHRAE 55 and EN 15251 relate comfortable indoor temperatures to outdoor temperature. The “Adaptive model“ tab looks at the data in this framework by binning thermal comfort votes according to the indoor and outdoor temperature conditions under which they were given. Then the percentage of satisfied votes is calculated within each two-dimensional bin. An accumulation of bins with at least 80% satisfaction delineates the comfort zone. </p>
             <p>For example, the image below shows that the bin with an outdoor and indoor temperature of 20°C has 100 acceptability votes of which 80 are “acceptable”, so the (20°, 20°) bin is colored green to mark it as having 80% satisfaction. Similarly, there are 50 votes in the bin of 20°C outdoor and 30°C indoor temperature, and 10 of them are “acceptable,” so the (20°, 30°) bin is colored red to mark it as having 20% satisfaction.</p>      
             <img src='binningexplanation.png', width=500.8, height=352, margin-bottom:100px, alt=Explanation of binning method,style='margin-left: 100px; margin-top: 100px; margin-bottom:100px'>
             
             <li>Scatter tab</li>
             <p>Elevated air speed comfort zone in ASHRAE 55 is adopted when the average air speed exceeding 0.2m/s (40 fpm), subject's metabolic rates between 1 and 2 met and clothing insulation between 0 and 1.5 clo. It is permissible to determine the operative temperature range by linear interpolation between the limits found in corresponding comfort zones. The “Scatter“ tab considers the data in this aspect and creates scatter plot of raw data regarding two comfort zones criteria (for clothing insulation = 0.5 and 1 clo) at 1.1 met. Furthermore, two extra scatter plots, with changeable x-axis and y-axis for different variables, are provided to observe any possible relationship between parameters.  </p>
             </ol>")),
    
    br(),
    
    h4("References"),
    p("Andamon M. M. (2006) Thermal comfort and building energy consumption in the Philippine context. PLEA 2006-The 23rd Conference on Passive and Low Energy Architecture."),
    p("Bae C., Lee H., Chun C. (2017) Predicting indoor thermal sensation for the elderly in welfare centres in Korea using local skin temperatures. Indoor and Built Environment 26, 1155-1167."),
    p("Bouden C., and Ghrab N. (2005) An adaptive thermal comfort model for the Tunisian context: a field study results. Energy and Buildings 37 (9), 952-963."),
    p("Brager, G. S., Paliaga G., and De Dear R. (2004) Operable windows, personal control and occupant comfort. ASHRAE Transactions, 110, 17-37."),
    p("Candido C. M., de Dear R., Lamberts R., Bittencourt L. S. (2010) Air movement acceptability limits and thermal comfort in Brazil hot humid climate zone. Building and Environment, 45(1), 222-229."),
    p("Cao B., Zhu Y., Ouyang Q., Zhou X., Huang L. (2011) Field study of human thermal comfort and thermal adaptability during the summer and winter in Beijing. Energy and Buildings. 43(5), 1051-1056."),
    p("Cao B., Luo M., Li M., Zhu Y. (2016) Too cold or too warm. A winter thermal comfort study in different climate zones in China, Energy and Buildings 133, 469-477."),
    p("De Dear, R. (1998). A global database of thermal comfort field experiments. ASHRAE Transactions, 104, 1141-1152."),
    p("De Dear, R. and G. S. Brager (1998) Developing an adaptive model of thermal comfort and preference. ASHRAE Transactions, 104, 145-167."),
    p("De Vecchi, R. et al. (2012) Thermal history and its influence on occupants thermal acceptability and cooling preferences in warm-humid climates- a new desire for comfort? Proceedings of 7th Windsor Conference: The changing context of comfort in an unpredictable world. Cumberland Lodge, Windsor, UK, 12-15 April 2012. London: Network for Comfort and Energy Use in Buildings, http://nceub.org.uk."),
    p("De Vecchi R., Candido C., de Dear R., Lamberts R. (2017) Thermal comfort in office buildings: Findings from a field study in mixed-mode and fully-air conditioning environments under humid subtropical conditions, Building and Environment 123, 672-683."),
    p("Deuble M.P. and de Dear R. (2012) Mixed-mode buildings: A double standard in occupants' comfort expectations, Building and Environment 54, 53-60."),
    p("Djamila H., Chu C. M. and Kumaresan S. (2013) Field study of thermal comfort in residential buildings in the equatorial hot-humid climate of Malaysia, Building and Environment 62, 133-142."),
    p("Energy Research and Development Division. Natural ventilation for energy saving in California commercial buildings.  Study of the naturally ventilated office with ceiling fans and operable windows. Energy Research and Development Division INTERIM/FINAL PROJECT REPORT."),
    p("Foldvary V., Beko G., Langer S., Arrhenius K., Petras D. (2017) Effect of energy renovation on indoor air quality in multifamily residential buildings in Slovakia, Building and Environment 122, 363-372."),
    p("Hawighorst, M., Schweiker, M. and Wagner, A. (2016) Thermo-specific self-efficacy (specSE) in relation to perceived comfort and control. Building and Environment, 102, 193-206."),
    p("Heidari S. and Sharples S. (2002) A comparative analysis of short-term and long-term thermal comfort surveys in Iran. Energy and Buildings 34 (6), 607-614."),
    p("Honnekeri A., Braiger G., Dhaka S., Mathur J. (2014) Comfort and adaptation in mixed-mode buildings in a hot-dry climate. Proceedings of 8th Windsor Conference: Counting the Cost of Comfort in a Changing World. Cumberland Lodge, Windsor, UK, 10-13 April 2014. London: Network for Comfort and Energy Use in Buildings, http://nceub.org.uk."),
    p("Honnekeri A. Pigman M. C., Zhang, H., Arens E., Fountain M., Zhai Y. (2014) Dutton S. Use of adaptive actions and thermal comfort in a naturally ventilated office, Proceedings of the 13th International Conference Indoor Air (2014) Hong Kong."),
    p("Indraganti M., Ooka R., Rijal H. B., Braiger G. (2014) Adaptive model of thermal comfort for offices in hot and humid climates of India. Building and Environment 74, 39-736."),
    p("Jin L., Meng Q., Zhao L., Chen L.(2013) Indoor Environment and Thermal Comfort in Rural Houses in East Guangdong of China. Journal of Civil, Architectural and Environmental Engineering. 35(2), 105-112."),
    p("Kim, H. (2012) Methodology for rating a building's overall performance based on the ASHRAE/CIBSE/USGBC Performance Measurement Protocols for Commercial Buildings. Ph.D. dissertation, Department of Architecture, Texas A&M University."),
    p("Konis K. (2013) Evaluating daylighting effectiveness and occupant visual comfort in a side-lit open-plan office building in San Francisco, California. Building and Environment 59 (1), 662-667."),
    p("Kwok, A. G. and Chun C. (2003) Thermal comfort in Japanese schools. Solar Energy 74(3), 245-252."),
    p("Kwon S. H., Chun C., Kwak R.Y. (2011) Relationship between quality of building maintenance management services for indoor environmental quality and occupant satisfaction, Building and Environment 46(11), 2179-2185."),
    p("Langevin J, Gurian PL and Wen J.  (2015) Tracking the human-building interaction: A longitudinal field study of occ upant behavior in air-conditioned offices.  Journal of Environmental Psychology, 42, 94-115."),
    p("Liu, Y., Yan, H., et al. (2013) Residential thermal environment in cold climates at high altitudes and building energy use implications. Energy and Buildings 62, 139-145."),
    p("Loveday D. L., Webb L. H., Verma P., Cook M. J., Rawal R., Vadodaria K., Cropper P., Brager G., Zhang H., Foldvary  V., Arens E., Babich F., Cobb R., Ariffin R., Kaam S., Toledo L. The Role of Air Motion for Providing Thermal Comfort in Residential / Mixed Mode Buildings: Multi-partner Global Innovation Initiative (GII) Project. Proceedings of 9th Windsor Conference: Making Comfort Relevant. Cumberland Lodge, Windsor, UK, 7-10 April 2016. Network for Comfort and Energy Use in Buildings, http://nceub.org.uk."),
    p("Luo M., Zhou X., Zhu Y., Zhang D., Cao, B. (2016) Exploring the dynamic 1 process of human thermal adaptation: A study in teaching building, Energy and Buildings 127, 425-432."),
    p("Manu S., Shukla Y., Rawaj R., de Dear R., Thomas L. (2014) Developing an India Model for Adaptive (Thermal) Comfort. IMAC. Final Report. "),
    p("McCartney, K. J. and F. Nicol (2002) Developing an adaptive control algorithm for Europe. Energy and buildings, 34(6), 623-635."),
    p("Nakamura Y., Yokoyama S., Tsuzuki K., Miyamato S., Ishii A.,  Tsutsumi J., Okamato T. (2008) Method for Simultaneous Measurement of the Occupied Environment Temperature in Various Areas for Grasp of Adaptation to Cilinate in Daily Life. Journal of Human and Living Environment, 15, 5-14."),
    p("Nicol, F. and M. Humphreys (2010) Derivation of the adaptive equations for thermal comfort in free-running buildings in European standard EN15251. Building and Environment, 45, 11-17."),
    p("Oluwafemi K. A. and Ademabowo M. A. (2010) Indoor Thermal Comfort for Residential Buildings in Hot-Dry Climate of Nigeria. The Proceedings of Conference: Adapting to Change: New Thinking on Comfort Cumberland Lodge, Windsor, UK, 9-11 April 2010. London: Network for Comfort and Energy Use in Buildings. Available on: http://nceub.org.uk/"),
    p("Oseland N.A. (1998) Acceptable Temperature Ranges in Naturally Ventilated and Air-Conditioned Offices. United States: American Society of Heating, Refrigerating and Air-Conditioning Engineers, Inc., Atlanta, GA (United States)."),
    p("Petersen S. H. (2012) Indoor Climate Survey at Espergaerde Gymnasium. Master thesis. Technical University of Denmark. Department of Civil Engineering. Denmark."),
    p("Pigman, M. (2014) The impact of cooling strategy and personal control on thermal comfort. MS Thesis."),
    p("Pigman, M., H. Zhang, A. Honnekeri, E. Arens, and G. S. Brager. (2014). Visualizing the results of thermal comfort field studies: Putting publicly accessible data in the hands of practitioners. Proceedings of the 8th Windsor Conference, London, April."),
    p("Pustayova H. (2013) Evaluation of energy performance and thermal comfort in the dwelling buildings in process of refurbishment. Doctoral thesis. Slovak University of Technology in Bratislava. Slovakia."),
    p("Romero R. A., Bojorque G., Corral M., Gallegos R. (2013) Energy and the occupants thermal perception of low-income dwellings in hot-dry climate: Mexicali, Mexico. Renewable Energy 49, 267-270."),
    p("Sekhar S.C., Tham K.W., Cheong K.W. (2003) Indoor air quality and energy performance of air conditioned office buildings in Singapore. Indoor Air 13, 315-331."),
    p("Singh M. K., Mahapatra S., Atreya S. K. (2010) Thermal performance study and evaluation of comfort temperatures in vernacular buildings of North-East India, Building and Environment, 45 (2), 320-329."),
    p("Singh M. K., Mahapatra S., Teller J. (2014) Relation between indoor thermal environment and renovation in Liege residential buildings. Thermal Science 18(3), 889-902."),
    p("Stoops J. L. (2001) The Physical Environment and Occupant Thermal Perceptions in Office Buildings An Evaluation of Sampled Data from Five European Countries, Doctoral thesis, Chalmers University of Technology."),
    p("Tartarini F., Cooper P.,Fleming R. (2018) Thermal perceptions, preferences and adaptive behaviours of occupants of nursing homes, Building and Environment 132, 57-69."),
    p("Tanabe S., Iwahashi Y., Tsushima S., Nishihara N. (2013) Thermal comfort and productivity in offices under mandatory electricity savings after the Great East Japan earthquake, Architectural Science Review 56(1), 4-13."),
    p("Teli, D., Jentsch, M. F., James, P. A. (2012) Naturally ventilated classrooms: An assessment of existing comfort models for predicting the thermal sensation and preference of primary school children. Energy and Buildings 53, 166-182."),
    p("Wagner A., Moosmann C., Gropp Th., Gossauer E., Leonhart R. (2007) Thermal Comfort and Workspace  occupant Satisfaction - Results of Field Studies in German Low Energy Office Buildings. Journal of Energy and Buildings, Volume 39 (7), 758-769."),
    p("Wang Z. (2006) A field study of the thermal comfort in residential buildings in Harbin, Building and Environment 41, 1034-1039"),
    p("Wang Z., Zhang L., Zhao J., He Y. (2011) Thermal responses to different residential environment in Harbin, Building and Environment 46(11), 2170-2178."),
    p("Xavier A. A. (2000) Prediction of thermal comfort in indoor environments with sedentary activities - physical theory combined with field study, Doctoral thesis (2000), Federal University of Santa Caterina."),
    p("Zangheri P., Pagliano L., Armani R., Santamouris M., Freire A., Alexandre J. L., Nicol F. (2010). Thermal compliance in existing buildings. Deliverable number: CC WP5 D5.1. Report of the European project: EIE-07-190."),
    p("Zangheri P., Pagliano R., Armani R. (2011) How the comfort requirements can be used to assess and design low energy buildings: testing the EN 15251 comfort evaluation procedure in 4 buildings. ECEEE Summer Study. Energy Efficiency first: The foundation of low-carbon society, 1569-1679."),
    p("Zhang Y., Chen H., Meng Q. (2013) Thermal comfort in buildings with split air-conditioners in hot-humid area of China. Building and Environment. 64, 213-224."),
    p("Zhang Y., Wang J., Chen H., Zhang J., Meng Q. (2010) Thermal comfort in naturally ventilated buildings in hot-humid area of China. Building and Environment. 45(11), 2562-2570."),
    
    
    br(),
    
    h4("Acknowledgement"),
    p("This tool is originally established by", a("Margaret Pigman",href="https://www.linkedin.com/in/margaret-pigman-044a0024/"), "in 2014 for Database I, thereafter modified by ", a("Toby Cheung",href="https://www.researchgate.net/profile/Chin_To_Cheung"), "in 2016 implementing the survey of Database II and new features. 
      Development of Database II is mainly contributed by the Centre for Built Environment (CBE) team, including ", a("Edward Arens",href="http://www.cbe.berkeley.edu/aboutus/staff-edward.htm"), ", ", a("Hui Zhang",href="http://www.cbe.berkeley.edu/aboutus/staff-hui.htm"), ", ", a("Gail Brager",href="https://www.cbe.berkeley.edu/aboutus/staff-gail.htm"), ", 
      ", a("Stefano Schiavon",href="https://www.cbe.berkeley.edu/aboutus/staff-stefano.htm"), ",", a("Veronika Foldvary",href="https://scholar.google.com/citations?user=xdHLdncAAAAJ&hl=en"), ",", a("Maohui Luo",href="https://www.researchgate.net/profile/Maohui_Luo"),",", a("Ariel Li",href="https://www.linkedin.com/in/ariel-peixian-li-4585ba77/")," and ", a("Soazig Kaam",href="https://scholar.google.com/citations?user=yvuCjNUAAAAJ&hl=en"), ",
      assisted by the research team in University of Sydney represented by ", a("Richard de Dear",href="http://sydney.edu.au/architecture/about/people/profiles/richard.dedear.php"), " and ", a("Thomas Parkinson",href="https://www.researchgate.net/profile/Thomas_Parkinson2"), ", and by research team in Yonsei University Korea, presented by ", a("Chungyoon Chun",href="http://web.yonsei.ac.kr/hbelabenglish/Professor.htm"), ".
      The ASHRAE Comfort Database Project II referred in this description is supported by an American Society of Heating, Refrigerating and Air Conditioning Engineers Grant-In-Aid, 
      British Council and UK Government under the Global Innovation Initiative project scheme, Korea National Science Foundation and the Center for the Built Environment, University of California at Berkeley,
      Additional support is provided by the Republic of Singapore's National Research Foundation through a grant to the Berkeley Education Alliance for Research in Singapore (BEARS) for the Sinapore-Berkeley Building Efficiency and Sustainability in the Tropics (SinBerBEST) Program.
      The project is preformed within the framework of the International Energy Agency Energy in Buildings and Communities programme (IEA-EBC) Annex 69 'Strategy and PRactice of Adaptive Thermal Comfort in Low Energy Buildings.'
      The authors also thank ", a("Michael Humphreys",href="https://www.researchgate.net/profile/Michael_Humphreys2"), " for his continuous scientific support; students who help to organize and format the database including Tina Lee (UC Berkeley), Youngjoo Son (Yonsei University), Sijie Liu and Xiuyuan Du (The University of Sydney);
      and ", a("Tyler Hoyt",href="https://cbe.berkeley.edu/aboutus/staff-tyler.htm"), " (UC Berkeley) for the initial suggestion to build the Comfort Database.
      "),
    br(),
    p("We are also thankful to the data contributors, the list is presented here: "),
    p("Akande Oluwafemi, Alison Kwok, Andreas Wagner, Anoop Honnekeri, Antonio Augusto Xavier, Bin Cao, Chandra Sekhar, Chiheb Bouden, Christina Candido, Chungyoon Chun, Cornelia Moosmann, 
      David Cheong, Dennis Loveday, Despoina Teli, Dusan Petras, Federico Tartarini, Fergus Nicol, Francesco Babich, Gail Brager, Gwelen Paliaga, 
      Hana Pustayova, Harimi Djamila, Holger Wallbaum, Hyojin Kim, Jared Langevin, Joon Ho Choi, Jorn Toftum, Jungsoo Kim, Kathryn McCartney, Kazuyo Tsuzuki, Kwokwai Tham, Kyle Konis, 
      Linda Toledo, Liu Yang, Lorenzo Pagliano, Lynda Webb, Madhavi Indraganti, Malcolm Cook, Manoj Singh, Maohui Luo, Marcel Schweiker, Maren Hawighorst, Max Deuble, Michael Adebamowo, Michael Humphreys, Mary Myla Andamon, 
      Nigel Oseland, Paolo Zangheri, Paul Cropper, Quan Jin, Rajan Rawal, Ramona Romero, Renata De Vecchi, Roberto Lamberts, Ryozo Ooka, Peixian Li,
      Salvatore Carlucci, Sanyogita Manu, Shahin Heidari, Shin-ichi Tanabe, Stefano Schiavon, Stine Pedersens, Soazig Kaam, Veronika Foldvary, Xiang Zhou, 
      Yasuto Nakamura, Yingxin Zhu, Yongchao Zhai, Yufeng Zhang, Zhaojun Wang.
      "),
    
    br(),                 
    
    
    p("Contact Hui Zhang (zhanghui@berkeley.edu) if you have data to contribute."),
    br(),
    withTags(p("This tool was built with the", a("statistical language R", href="http://www.R-project.org/",target="_blank"), "using the", a("ggplot2 package",href="http://ggplot2.org/",target="_blank"), "for visualization, the", 
               a("oridinal package",href="http://cran.r-project.org/web/packages/ordinal/index.html",target="_blank"), "for probit analysis, and the", a("shiny package", href="http://shiny.rstudio.com/",target="_blank"), "as the interface between R and html.",style="font-size:12px"))
    
    )
  
    )
    )
    )

    )