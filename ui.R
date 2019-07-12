library(shiny)
library(plotly)
library(ggplot2)
library(shinyBS)
library(magic)
library(shinyjs)
source("HelperFunctionsJH.R")
options(shiny.sanitize.errors = FALSE)

#, local=TRUE
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #hides momentary flashing red errors while working 
  tags$style(type="text/css",
             
             ".shiny-output-error { visibility: hidden; }",
             
             ".shiny-output-error:before { visibility: hidden; }"
             
  ),
  
  useShinyjs(),
  # Application title
  titlePanel("The Shiny CRT Calculator: Power and Sample size for Cluster Randomised Trials"),
  
  # Trial design
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId="choice2",
                   label = "Trial Design", 
                   choiceNames = c("Parallel", "Parallel with baseline measure", "Two-period cross-over", "Stepped-wedge" , "Multiple-period cross-over", "Upload own design"),
                   choiceValues = c("Parallel", "Before and After", "Cross-over", "Stepped-wedge" , "Multi cross-over", "Upload matrix")),
      bsTooltip("choice2","A parallel design randomly allocates clusters to one of two treatment arms; a parallel design with baseline measures includes an additional period of observation in which all clusters are observed in the control condition; a two-period cross-over trial randomly allocates clusters to one of two sequences (control followed by intervention, or intervention followed by control); a multi-period cross over trial has multiple cross-overs between two treatment conditions; a longitudinal parallel design randomly allocates clusters to one of two arms, and takes repeated measures over time.","bottom"),
      helpText("A non standard design can be accommodated by uploading the design as a CSV file (click on the up-load option for more details)."),
      conditionalPanel(
        condition = "input.choice2=='Upload matrix'",
        fileInput("file1", "Upload a design matrix:", accept=c('text/plain', '.csv')),
        helpText("The file must be a comma separated .csv file consisting of 0s (control), 1s (treatment) and blank (missing observation), with a column for each time period and a row for each treatment sequence. Do not include row or column names.")
        ),
      
  #Sampling structure 
        radioButtons(inputId="choice3",
                     label = "Sampling structure", 
                     choiceValues = c("Cross-sectional", "Cohort"),
                     choiceNames = c("Cross-sectional sample", "Cohort")
                     ),
        helpText("In a cross-sectional design at each measurement occasion a different sample of participants is measured. In a cohort design, participants are repeatidly measured at each measurement occasion. "),
  
  #Correlation structure 
    radioButtons(inputId="choice_corr",
                 label = "Cluster correlation structure", 
                 choiceNames = c("Exchangable", "Two-period decay", "Discrete time decay"),
                 choiceValues = c("exch", "clusbytime", "dtd")),
    helpText("Two-period decay allows for a correlation within the same measurement period to differ to that from a different measurement period. The discrete time decay allows for correlations to decay with each measurement period."), 
  
  #Differential clustering   
  conditionalPanel(
       condition = "input.choice2=='Parallel'",
       conditionalPanel(
         condition = "input.choice_corr=='exch'",
       radioButtons(inputId="choice5",
                    label = "Differential clustering", 
                    choices = c("False", "True")),
       helpText("This is a two arm individually randomised trial with clustering in one arm only.  The option to allow for variation in cluster sizes is not available for this trial design."),
       bsTooltip("choice5","Differential clustering occurs in trials where the clustering might be in one arm only; or where the intervenion induces different types of clusters.","right")
     )),
     
  #Setting plot set-up
      radioButtons(inputId="choice4",
                   label = "Plot set-up",
                   choiceNames = c("Cluster size vs. Power", "Number of clusters vs. Power", "Number of clusters vs. Cluster size"),
                   choiceValues = c("Determine cluster size", "Determine number of clusters", "Trade-off for fixed power")),
 
      
   #Determine number of clusters
      conditionalPanel(
        condition = "input.choice4=='Determine number of clusters'",
        
        conditionalPanel(
          condition = "input.choice2=='Parallel' ||  input.choice2=='Before and After'" ,
          sliderInput("plotrange2a",
                    "X-axis range: Number of clusters (per arm)",
                    min = 0,
                    max = 500,
                    value = c(0,100)),
          bsTooltip("plotrange2a","The plot to the right hand side will be ploted over this range. You can set this range to reflect the plausible numebr of cluster  in your study. ","right"),
        conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is number of clusters under treatment arm."))
          ),
        conditionalPanel(
          condition = "input.choice2== 'Cross-over' || input.choice2=='Stepped-wedge' || input.choice2=='Multi cross-over' || input.choice2=='Upload matrix' ",
          sliderInput("plotrange2b",
                      "X-axis range: Number of clusters (per sequence)",
                      min = 0,
                      max = 500,
                      value = c(0,50)),
          bsTooltip("plotrange2b","The plot to the right hand side will be ploted over this range.  You can set this range to reflect the plausible number of cluster in your study.","right"),
       helpText("The number of clusters per sequence will often be 1 when the user has uploaded their own design matrix")
           ),
       
        conditionalPanel(
          #condition = "input.choice2=='Parallel'" ,
          condition = "input.choice_corr=='exch' & input.choice2=='Parallel'" ,
          numericInput("m", "Cluster size", 100),
          bsTooltip("m","tmp","right"),
          conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is cluster size under treatment arm."))
        ),
        conditionalPanel(
          condition = " input.choice2=='Parallel' && input.choice_corr!='exch' || input.choice2=='Before and After'||  input.choice2=='Cross-over'|| input.choice2=='Stepped-wedge'|| input.choice2=='Multi cross-over' || input.choice2=='Upload matrix'", 
          numericInput("mb", "Cluster size (per period)", 50),
          bsTooltip("mb","tmp","right")
        )),  
      
      
     ##### Differential clustering control arm options
    conditionalPanel(
      condition="input.choice4!='Trade-off for fixed power'",
     conditionalPanel(
     condition="input.choice5=='True'",
     conditionalPanel(
       condition = "input.choice2== 'Parallel'",
       numericInput("k_control", "Number of clusters control arm ", 50),
       helpText("For trials with differential clustering: if no clustering under the control arm this will be total number of individuals in control arm.")
     ),
      conditionalPanel(
       condition = "input.choice2== 'Cross-over' || input.choice2=='Stepped-wedge' || input.choice2=='Multi cross-over' || input.choice2=='Upload matrix' ",
       numericInput("kb_control", "Number of clusters (per sequence) under control condition ", 50),
       helpText("For trials with differential clustering: if no clustering under the control condition this will be total number of individuals in control arm.")
     ),
     conditionalPanel(
       condition = "input.choice2=='Parallel'" ,
       numericInput("m_control", "Cluster size control arm ", 1),
       helpText("For trials with differential clustering: this will be 1 if no clustering under control arm.")
     ),
     conditionalPanel(
       condition = "input.choice2!='Parallel'",
       numericInput("mb_control", "Cluster size (per period) under control condition", 1),
       helpText("For trials with differential clustering: this will be 1 if no clustering under control arm.")
     )  
    )),
     
  
     ##Varying cluster sizes
     conditionalPanel(
    ##July 2017 edit to correct mistake that allows user to select varying cluster sizes for a wider range of designs
    # condition= "input.choice2!='Upload matrix' & input.choice_corr=='exch'", 
      condition= "input.choice2!='Upload matrix' | input.choice_corr!='dtd'", 
      radioButtons(inputId="varyingclustersizes",
                   label = "Allowance for varying cluster sizes", 
                   choices = c("No", "Yes")),
      bsTooltip("varyingclustersizes","Uses a convervative inflation to the design effect. Option not available when user uploads own design. For trials with differntial clustering this represents varying cluster size in the treatment arm.","right"),
      conditionalPanel(
      condition = "input.varyingclustersizes=='Yes'",
      numericInput("cv", "Coefficient of variation of cluster sizes", 
                  min = 0, 
                  max = 4, 
                  value = 0,
                  step= 0.01),
      helpText("Ratio of standard deviation of cluster sizes to mean of cluster sizes.")
    )
    ),
    
    
    ##Trade-off for fixed power without differential clustering
      conditionalPanel(
        condition = "input.choice5=='False'",
        conditionalPanel(
        condition = "input.choice4=='Trade-off for fixed power'",
        numericInput("power",
                     "Power",
                     min = 0,
                     max = 1,
                     value = 0.90,
                     step=0.005
        ),
        conditionalPanel(
          condition = "input.choice_corr=='exch' " ,
          sliderInput("plotrange3a",
                      "X-axis range: Cluster size",
                      min = 0,
                      max = 1000,
                      value = c(0,100)),
          bsTooltip("plotrange3a","The plot to the right hand side will be ploted over this range.  You can set this range to reflect the plausible cluster sizes in your study.","right")
        ),
        conditionalPanel(
          condition = "input.choice_corr!='exch' " ,
          sliderInput("plotrange3b",
                      "X-axis range: Cluster size (per period)",
                      min = 0,
                      max = 1000,
                      value = c(0,50)),
          bsTooltip("plotrange3b","The plot to the right hand side will be ploted over this range. You can set this range to reflect the plausible cluster sizes in your study. ","right")
        )  
       )),
      
    ##Trade-off for fixed power with differential clustering
    conditionalPanel(
      condition="input.choice5=='True'",
      conditionalPanel(
        condition = "input.choice4=='Trade-off for fixed power' ",
        numericInput("power_d",
                     "Power",
                     min = 0,
                     max = 1,
                     value = 0.90,
                     step=0.005
        ),
        conditionalPanel(
          condition = "input.choice2=='Parallel' " ,
          sliderInput("plotrange3a_d",
                      "X-axis range: Number of clusters  treatment arm",
                      min = 0,
                      max = 1000,
                      value = c(100,200)),
          bsTooltip("plotrange3a","The plot to the right hand side will be ploted over this range.  You can set this range to reflect the plausible cluster sizes in your study.","right")
        ),
        conditionalPanel(
          condition = "input.choice2!='Parallel' ",
          sliderInput("plotrange3b_d",
                      "X-axis range: Number of clusters treatment condition",
                      min = 0,
                      max = 1000,
                      value = c(0,50))
       ),
       conditionalPanel(
         condition = "input.choice2=='Parallel'" ,
         numericInput("ma_d", "Cluster size treatment arm", 20),
         numericInput("ma_control_d", "Cluster size control arm",1),
         helpText("For trials with differential clustering: this will be 1 if no clustering under control arm.")
       ),
       conditionalPanel(
         condition = "input.choice2!='Parallel'",
         numericInput("mb_d", "Cluster size (per period) treatment condition", 50),
         numericInput("mb_control_d", "Cluster size (per period) control condition",1)
       )
        
      )),
      
    ####Longitudinal options
      conditionalPanel(
        condition = "input.choice2 == 'Stepped-wedge'",
        numericInput("w", "Number of sequences (i.e. steps)", 3)
      ),
    conditionalPanel(
      condition =  c("input.choice2=='Multi cross-over' ||input.choice2=='Parallel' & input.choice_corr=='dtd'  "),
      numericInput("T", "Number of periods", 2)
    ),
      
    ####Determing cluster size
      conditionalPanel(
        condition = "input.choice4=='Determine cluster size' ",
        conditionalPanel(
          condition = "input.choice2=='Parallel' ||  input.choice2=='Before and After'" ,
          numericInput("k", "Number of clusters (per arm) ", 100),
          bsTooltip("k","The number of cluster per arm.", "right"),
          conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is number of clusters in the treatment arm."))
        ),
        conditionalPanel(
          condition = "input.choice2== 'Cross-over' || input.choice2=='Stepped-wedge' || input.choice2=='Multi cross-over' || input.choice2=='Upload matrix'",
          numericInput("kb", "Number of clusters (per sequence) ", 50),
          bsTooltip("kb","The number of clusters per sequence.", "right"),
          helpText("The number of clusters per sequence will often be 1 when the user has uploaded their own design matrix.")
        ), 
        conditionalPanel(
          condition = "input.choice_corr=='exch' " ,
          sliderInput("plotrangea",
                                "X-axis range: Cluster size",
                                min = 0,
                                max = 5000,
                                value = c(0,100)),
          bsTooltip("plotrangea","The plot to the right hand side will be ploted over this range of cluster sizes. You can set this range to reflect the plausible cluster sizes in your study.","right"),
          conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is cluster size under treatment arm."))
          ), 
        conditionalPanel(
          condition = "input.choice_corr!='exch' " ,
          sliderInput("plotrangeb",
                    "X-axis range: Cluster size (per period)",
                    min = 0,
                    max = 5000,
                    value = c(0,50)),
        bsTooltip("plotrangeb","The plot to the right hand side will be ploted over this range of cluster sizes. You can set this range to reflect the plausible cluster sizes in your study.","right")
      )         
    ),
  
      
  ###correlations
    conditionalPanel(
      condition = "input.choice_corr=='exch'",
      numericInput("icc",
                  "Intra-cluster correlation (ICC)",
                  min = 0,
                  max = 1,
                  value = 0.02,
                  step=0.005
                  ),
      bsTooltip("icc","The Intra-Cluster Correlation (ICC) is the correlation between two observations in the same cluster.","right"),
      conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is correlation under treatment arm.")),
      numericInput("icc_l",
                   "ICC lower extreme",
                   min = 0,
                   max = 1,
                   value = 0.01,
                   step=0.005
      ),
      bsTooltip("icc_l","The lower extreme value for the ICC should represent the lowest expected value for the ICC.","right"),
      conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is correlation under treatment arm.")),
      numericInput("icc_u",
                   "ICC upper extreme",
                   min = 0,
                   max = 1,
                   value = 0.05,
                   step=0.005
      ),
      bsTooltip("icc_u","The upper extreme value for the ICC should represent the highest expected value for the ICC.","right"),
      conditionalPanel(condition="input.choice5=='True'", helpText("For trials with differential clustering: this is correlation under treatment arm."))
      ),
    

    conditionalPanel(
      condition = "input.choice_corr=='clusbytime' || input.choice_corr=='dtd'",
      numericInput("wp_icc",
                   "Within-period ICC",
                   min = 0,
                   max = 1,
                   value = 0.02,
                   step=0.005
      ),
      bsTooltip("wp_icc","The within-period ICC (WP-ICC) is the correlation between two observations in the same cluster and time period.","right"),
      numericInput("wp_icc_l",
                   "Within-period ICC lower extreme",
                   min = 0,
                   max = 1,
                   value = 0.01,
                   step=0.005
      ),
      bsTooltip("wp_icc_l","The lower extreme value for the WP-ICC should represent the lowest expected value for the WP-ICC.","right"),
      numericInput("wp_icc_u",
                   "Within-period ICC upper extreme",
                   min = 0,
                   max = 1,
                   value = 0.05,
                   step=0.005
      ),
      bsTooltip("wp_icc_u","The upper extreme value for the WP-ICC should represent the highest expected value for the WP-ICC.","right")
    ),  
    
    
      conditionalPanel(
      condition = "input.choice_corr=='dtd' || input.choice_corr=='clusbytime'",
      sliderInput("cac",
                  "Cluster auto-correlation (CAC)",
                  min = 0,
                  max = 1,
                  value = 0.8,
                  step=0.001),
      helpText("The Cluster Auto Correlation (CAC) is the correlation between two population means from the same cluster at different times. The lower and upper bounds for the CAC values plotted on the curve are 80% and 120% (or 1) of the base case value inputted here.")
      ),

  #### Additional ICCs for differential clustering 
      conditionalPanel(
      condition = "input.choice5=='True'",
      conditionalPanel(
      condition = "input.choice2=='Parallel'",
      numericInput("icc_control",
               "ICC under control condition",
               min = 0,
               max = 1,
               value = 0.0,
               step=0.005
      ),
      helpText("For trials with differential clustering: this will be 0 if no clustering under control arm.")
      ),

      conditionalPanel(
      condition = "input.choice2!='Parallel'",
      numericInput("wp_icc_control",
               "Within period ICC under control condition",
               min = 0,
               max = 1,
               value = 0.02,
               step=0.005
      ),
      helpText("For trials with differential clustering: this will be 0 if no clustering under control arm.")
       )),
  ###Individual level correlations
      conditionalPanel(
        condition = "input.choice2 != 'Parallel' & input.choice3 == 'Cohort'",
        sliderInput("iac",
                    "Individual auto-correlation (IAC)",
                    min = 0,
                    max = 1,
                    value = 0.8),
      bsTooltip("iac","The Individual Auto Correlation (IAC) is the correlation between observations from the same individual from same cluster at different times.","right")
      ),
      
  ###Outcomes
      radioButtons(inputId="choice",
                   label = "Outcome type", 
                   choiceNames = c("Continuous", "Binary", "Count"),
                   choiceValues = c("Continuous", "Binomial", "Count")),
      
      conditionalPanel(
        condition = "input.choice == 'Continuous'",
        numericInput("meandiff", "Mean Difference", 0.11),
        numericInput("sd", "Standard Deviation", 1)
        ),
      
      conditionalPanel(
        condition = "input.choice == 'Count'",
        numericInput("r0", "Count under control", 10, min=0, max=10000, step=0.01),
        numericInput("r1", "Count under intervention", 15,min=0, max=10000, step=0.01),
        numericInput("thi", "Over dispersion", 1,min=1, max=10000, step=0.01),
        helpText("The over dispersion is an inflation factor for the variance of the Poisson distribution (i.e. sample variance / sample mean).")
      ),
      
      
      conditionalPanel(
        condition = "input.choice == 'Binomial'",
        numericInput("p0", "Proportion under control", 0.1, min=0, max=1, step=0.01),
        numericInput("p1", "Proportion under intervention", 0.15,min=0, max=1, step=0.01)
      ),

    ###Differential ICC in treatment condition 
    conditionalPanel(
    condition = "input.choice2 == 'Upload matrix' ",
      radioButtons(inputId="treatment_variation",
             label = "Variation in treatment effect across clusters", 
             choices = c("False"=0, "True"=1), selected=0, inline=TRUE),
      conditionalPanel(
            condition = "input.treatment_variation == 1",
            numericInput("icc_treat", "Treatment condition SD", value = 0, min = 0, max = NA))),

    ###Significance level 
     numericInput("alpha", "Significance level", 0.05, min = 0, max = 1, step = 0.01), 
     
     ####T-distribution
      conditionalPanel(
      condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
      radioButtons(inputId="choice6",
               label = "Normal approximation", 
               choiceNames = c("Normal approximation", "T-distribution"),
                choiceValues = c("True", "False")
          ),
     helpText("The t-distrubution option should be selected if small sample corrections are intended to be used at the analysis stage. Degrees of freedom used are number of cluster-periods minus number clusters minus number periods.")
          ),
    
    ###Buttons
    conditionalPanel(
    condition = "input.choice2=='Upload matrix'",
    actionButton("button1","Create curve")),
    conditionalPanel(
    condition = "input.choice2=='Upload matrix'",
    downloadButton("downloadCurveDESMAT", "Download plot data")),
    conditionalPanel(
    condition = "input.choice2!='Upload matrix' & input.corr!='dtd' & input.corr!='ctd'",
    downloadButton("downloadCurve", "Download plot data")),
    conditionalPanel(
    condition = "input.choice2!='Upload matrix' & input.corr=='dtd' || input.choice2!='Upload matrix' & input.corr=='ctd'", 
    downloadButton("downloadCurveDECAY", "Download plot data"))

),

#Plot of power vs cluster size
#By cluster size 
    mainPanel(
      style="position:fixed;margin-left:32vw;",
      conditionalPanel(
      condition = "input.choice4=='Determine cluster size'  && input.choice2=='Parallel' && input.choice_corr=='exch' ||input.choice4=='Determine cluster size'  && input.choice5=='True' ", 
      tabsetPanel(
      tabPanel("Power", fixedPanel(h1(plotlyOutput("PowerByClusterSize")),
      h6("Curve shows the increase in power as the cluster size increases (for a fixed number of clusters). Hover cursor over curve to see actual power values"), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary and count outcomes."),
      #Input parameters 
      h4("Parameters"),
      conditionalPanel(
        condition = "input.choice=='Continuous'",
        h6(textOutput("continuousA"))),
      conditionalPanel(
        condition = "input.choice=='Binomial'",
        h6(textOutput("binaryA"))),
      conditionalPanel(
        condition = "input.choice=='Count'",
        h6(textOutput("countA"))),
      h6(textOutput("alphaA")),
      conditionalPanel(
        condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
        h6(textOutput("tdistA"))),
      conditionalPanel(
        condition = "input.varyingclustersizes=='Yes'",
        h6(textOutput("cvA"))),
      conditionalPanel(
        condition = "input.choice2=='Parallel' ||  input.choice2=='Before and After'" ,
        h6(textOutput("kA"))),
      conditionalPanel(
        condition = "input.choice_corr=='exch'",
        h6(textOutput("iccA"))),
      conditionalPanel(
        condition = "input.choice5=='True'",
          h6(textOutput("icc_controlA"))),
      conditionalPanel(
        condition = "input.choice_corr!='exch'",
        h6(textOutput("wp_iccA"))),
      conditionalPanel(
        condition = "input.choice3=='Cohort'",
        h6(textOutput("iacA")))
      )),
      tabPanel("Precision", fixedPanel(h1(plotlyOutput("PrecisionByClusterSize")),h6("Curve shows the increase in precision as the cluster size increases (for a fixed number of clusters). Hover cursor over curve to see actual precision values"), h6("Precision is defined as 1/variance of the treatment effect. When the precision no longer increases the confidence intervals around the estimated effect size will no longer decrease."), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary outcomes."))),
      tabPanel("Design", h4(tableOutput("DesignmatrixDisp")), h6("Design matrix (displayed with one cluster per treatment sequence only)")),
      tabPanel("References and Contacts", 
      h3("References and Contacts"),
      p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA . Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback"),
      p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                  stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                 Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                 27350420."),
      p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                 alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                 10.1136/bmj.h2925. PubMed PMID: 26055828."),
      h2("Acknowedgements"),
      h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
      ))),
      
#By cluster size per period
      conditionalPanel(
        condition = "input.choice4=='Determine cluster size'  && input.choice2=='Parallel' && input.choice_corr!='exch' || input.choice4=='Determine cluster size'  && input.choice2=='Before and After'|| input.choice4=='Determine cluster size'  && input.choice2=='Cross-over'|| input.choice4=='Determine cluster size'  && input.choice2=='Stepped-wedge'|| input.choice4=='Determine cluster size'  && input.choice2=='Multi cross-over' || input.choice4=='Determine cluster size'  && input.choice2=='Upload matrix'", 
        #condition = "input.choice4=='Determine cluster size' && input.choice2=='Upload matrix' || input.choice4=='Determine cluster size' && input.choice2=='Cross-over' || input.choice4=='Determine cluster size' && input.choice2=='Stepped-wedge' || input.choice4=='Determine cluster size' && input.choice2=='Multi cross-over'",
        tabsetPanel(
          tabPanel("Power", 
          fixedPanel(h1(plotlyOutput("PowerByClusterSize_perperiod")),
          h6("Curve shows the increase in power as the cluster-period size increases (for a fixed number of clusters). Hover cursor over curve to see actual power values"), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary and count outcomes"),
          h4("Parameters:"),
          conditionalPanel(
            condition = "input.choice=='Continuous'",
            h6(textOutput("continuousB"))),
          conditionalPanel(
            condition = "input.choice=='Binomial'",
            h6(textOutput("binaryB"))),
          conditionalPanel(
            condition = "input.choice=='Count'",
            h6(textOutput("countB"))),
          h6(textOutput("alphaB")),
          conditionalPanel(
            condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
            h6(textOutput("tdistB"))),
          conditionalPanel(
            condition = "input.varyingclustersizes=='Yes'",
            h6(textOutput("cvB"))),
          conditionalPanel(
            condition = "input.choice2!='Before and After' & input.choice2!='Parallel'",
            h6(textOutput("kbB"))),
          conditionalPanel(
            condition = "input.choice2=='Before and After' || input.choice2=='Parallel'",
            h6(textOutput("kB"))),
          conditionalPanel(
            condition = "input.choice2=='Stepped-wedge'",
          h6(textOutput("wB"))),
          conditionalPanel(
            condition = "input.choice2=='Multi cross-over'",
          h6(textOutput("tB"))),
          conditionalPanel(
            condition = "input.choice_corr=='exch'",
            h6(textOutput("iccB"))),
          conditionalPanel(
            condition = "input.choice_corr!='exch'",
            h6(textOutput("wp_iccB"))),
          conditionalPanel(
            condition = "input.choice3=='Cohort'",
            h6(textOutput("iacB")))
        )),
          tabPanel("Precision", fixedPanel(h1(plotlyOutput("PrecisionByClusterSize_perperiod")),h6("Curve shows the increase in precision as the cluster-period size increases (for a fixed number of clusters). Hover cursor over curve to see actual precision values"),  h6("Precision is defined as 1/variance of the treatment effect. When the precision no longer increases the confidence intervals around the estimated effect size will no longer decrease."), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary outcomes"))),
          tabPanel("Design matrix", h4(tableOutput("DesignmatrixDispNP")), h6("Design matrix (displayed with one cluster per treatment sequence only)")),
          tabPanel("References and Contacts", 
                   h3("References and Contacts"),
                   p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA. Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback"),
                   p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                     stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                     Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                     27350420."),
                   p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                     alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                     10.1136/bmj.h2925. PubMed PMID: 26055828."),
                   h2("Acknowedgements"),
                   h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
                   ))),
      
      
   
     ####Trade-off plot parallel under exhangeable correlation 
     conditionalPanel(
       condition = "input.choice4=='Trade-off for fixed power' && input.choice2=='Parallel' && input.choice_corr=='exch' && input.choice5=='False'",
       tabsetPanel(
         tabPanel("Trade off", fixedPanel(h1(plotlyOutput("TradeoffPlot_parallel")),
         h6("Curve shows the reduction in number of clusters under control (and consequently total sample size) as the number of clusters in the treatment arm increases (for a fixed power). Hover cursor over curve to see actual power values"), h6("Warning: if curve fails to display sensible values try increasing number of clusters in treatment arm"),
         h4("Parameters"),
         h6(textOutput("powerC")),
         conditionalPanel(
           condition = "input.choice=='Continuous'",
           h6(textOutput("continuousC"))),
         conditionalPanel(
           condition = "input.choice=='Binomial'",
           h6(textOutput("binaryC"))),
         conditionalPanel(
           condition = "input.choice=='Count'",
           h6(textOutput("countC"))),
         h6(textOutput("alphaC")),
         conditionalPanel(
           condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
           h6(textOutput("tdistC"))),
         conditionalPanel(
           condition = "input.varyingclustersizes=='Yes'",
           h6(textOutput("cvC"))),
         conditionalPanel(
           condition = "input.choice_corr=='exch'",
           h6(textOutput("iccC"))),
         conditionalPanel(
           condition = "input.choice_corr!='exch'",
           h6(textOutput("wp_iccC"))),
         conditionalPanel(
           condition = "input.choice3=='Cohort'",
           h6(textOutput("iacC")))
         )),
         tabPanel("Design matrix", fixedPanel(h4(tableOutput("DesignmatrixDisp2")),h6("Design matrix (displayed with one cluster per treatment sequence only)"))),
         tabPanel("References and Contacts", 
                  h3("References and Contacts"),
                  p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA. Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback"),
                  p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                    stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                    Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                    27350420."),
                  p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                    alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                    10.1136/bmj.h2925. PubMed PMID: 26055828."),
                  h2("Acknowedgements"),
                  h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
                  ))),

     #Trade off plot under differential clustering 
     conditionalPanel(
       condition = "input.choice4=='Trade-off for fixed power' && input.choice2=='Parallel' && input.choice5=='True'",
       tabsetPanel(
         tabPanel("Trade off", fixedPanel(h1(plotlyOutput("TradeoffPlot_parallel_d")),
         h6("Curve shows the reduction in number of clusters under control (and consequently total sample size) as the number of clusters in the treatment arm increases (for a fixed power). Hover cursor over curve to see actual power values"), h6("Warning: if curve fails to display sensible values try increasing number of clusters in treatment arm"),
         h4("Parameters"),
         h6(textOutput("powerD")),
         conditionalPanel(
           condition = "input.choice=='Continuous'",
           h6(textOutput("continuousD"))),
         conditionalPanel(
           condition = "input.choice=='Binomial'",
           h6(textOutput("binaryD"))),
         conditionalPanel(
           condition = "input.choice=='Count'",
           h6(textOutput("countD"))),
         h6(textOutput("alphaD")),
         conditionalPanel(
           condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
           h6(textOutput("tdistD"))),
         conditionalPanel(
           condition = "input.varyingclustersizes=='Yes'",
           h6(textOutput("cvD"))),
         conditionalPanel(
           condition = "input.choice_corr=='exch'",
           h6(textOutput("iccD"))),
           h6(textOutput("icc_controlD")),
         conditionalPanel(
           condition = "input.choice3=='Cohort'",
           h6(textOutput("iacD")))
         )),
         ##KH this is now a duplicate reference to the design matrix and as it doesnt make much sense have texted out
         #tabPanel("Design matrix", h4(tableOutput("DesignmatrixDisp2")),h6("Design matrix (displayed with one cluster per treatment sequence only)")),
         tabPanel("References and Contacts", 
                  h3("References and Contacts"),
                  p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA. Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback"),
                  p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                    stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                    Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                    27350420."),
                  p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                    alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                    10.1136/bmj.h2925. PubMed PMID: 26055828."),
                  h2("Acknowedgements"),
                  h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
                  ))),
     
       
      conditionalPanel(
        condition = "input.choice4=='Trade-off for fixed power'  && input.choice2=='Parallel' && input.choice_corr!='exch' || input.choice4=='Trade-off for fixed power'  && input.choice2=='Before and After'|| input.choice4=='Trade-off for fixed power'  && input.choice2=='Cross-over'|| input.choice4=='Trade-off for fixed power'  && input.choice2=='Stepped-wedge'|| input.choice4=='Trade-off for fixed power'  && input.choice2=='Multi cross-over' || input.choice4=='Trade-off for fixed power'  && input.choice2=='Upload matrix'", 
        tabsetPanel(
          tabPanel("Trade off",fixedPanel(h1(plotlyOutput("TradeoffPlot_other")),
          h6("Curve shows the reduction in number of clusters required as cluster-period size increases (for a fixed power). Hover cursor over curve to see actual power values"), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary and count outcomes"),
          h4("Parameters"),
          h6(textOutput("powerE")),
          conditionalPanel(
            condition = "input.choice=='Continuous'",
            h6(textOutput("continuousE"))),
          conditionalPanel(
            condition = "input.choice=='Binomial'",
            h6(textOutput("binaryE"))),
          conditionalPanel(
            condition = "input.choice=='Count'",
            h6(textOutput("countE"))),
          h6(textOutput("alphaE")),
          conditionalPanel(
            condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
            h6(textOutput("tdistE"))),
          conditionalPanel(
            condition = "input.varyingclustersizes=='Yes'",
            h6(textOutput("cvE"))),
          conditionalPanel(
            condition = "input.choice2=='Stepped-wedge'",
            h6(textOutput("wE"))),
          conditionalPanel(
            condition = "input.choice2=='Multi cross-over'",
            h6(textOutput("tE"))),
          conditionalPanel(
            condition = "input.choice_corr=='exch'",
            h6(textOutput("iccE"))),
          conditionalPanel(
            condition = "input.choice_corr!='exch'",
            h6(textOutput("wp_iccE"))),
          conditionalPanel(
            condition = "input.choice3=='Cohort'",
            h6(textOutput("iacE")))
          )),
          tabPanel("Design matrix", h4(tableOutput("DesignmatrixDispNP2")),h6("Design matrix (displayed with one cluster per treatment sequence only)")),
          tabPanel("References and Contacts", 
                   h3("References and Contacts"),
                   p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA. Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback"),
                   p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                     stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                     Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                     27350420."),
                   p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                     alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                     10.1136/bmj.h2925. PubMed PMID: 26055828."),
                   h2("Acknowedgements"),
                   h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
                   ))),
      
#Power by number of clusters
#By number of clusters per arm
      conditionalPanel(
        #condition = "input.choice4=='Determine number of clusters' &&  input.choice_corr=='exch' ",
        condition = "input.choice4=='Determine number of clusters' &&  input.choice2=='Parallel' || input.choice4=='Determine number of clusters' &&  input.choice2=='Before and After' ",
        tabsetPanel(
          tabPanel("Power",fixedPanel(h1(plotlyOutput("PowerByNoClusters_byarm")),
          h6("Curve shows the increase in power as the number of clusters increases (for a fixed cluster size). Hover cursor over curve to see actual power values"), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary and count outcomes"),
          h4("Parameters"),
          conditionalPanel(
            condition = "input.choice=='Continuous'",
            h6(textOutput("continuousF"))),
          conditionalPanel(
            condition = "input.choice=='Binomial'",
            h6(textOutput("binaryF"))),
          conditionalPanel(
            condition = "input.choice=='Count'",
            h6(textOutput("countF"))),
          h6(textOutput("alphaF")),
          conditionalPanel(
            condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
            h6(textOutput("tdistF"))),
          conditionalPanel(
            condition = "input.varyingclustersizes=='Yes'",
            h6(textOutput("cvF"))),
          conditionalPanel(
            condition = "input.choice2=='Parallel' & input.choice_corr=='exch'",
            h6(textOutput("mF"))),
          conditionalPanel(
            condition = " input.choice2=='Parallel' && input.choice_corr!='exch' || input.choice2=='Before and After'||  input.choice2=='Cross-over'|| input.choice2=='Stepped-wedge'|| input.choice2=='Multi cross-over' || input.choice2=='Upload matrix'", 
            h6(textOutput("mbF"))),
          conditionalPanel(
            condition = "input.choice_corr=='exch'",
            h6(textOutput("iccF"))),
          conditionalPanel(
            condition = "input.choice5=='True'",
          h6(textOutput("icc_controlF"))),
          conditionalPanel(
            condition = "input.choice_corr!='exch'",
            h6(textOutput("wp_iccF"))),
          conditionalPanel(
            condition = "input.choice3=='Cohort'",
            h6(textOutput("iacF")))
          )),
          tabPanel("Precision", fixedPanel(h1(plotlyOutput("PrecisionByNoClusters_byarm")), h6("Curve shows the increase in power as the number of clusters increases (for a fixed cluster size). Hover cursor over curve to see actual precision values"), h6("Precision is defined as 1/variance of the treatment effect. When the precision no longer increases the confidence intervals around the estimated effect size will no longer decrease."), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary outcomes"))),
          tabPanel("Design matrix", h4(tableOutput("DesignmatrixDisp3")),h6("Design matrix (displayed with one cluster per treatment sequence only)")),
          tabPanel("References and Contacts",
          h4("References and Contacts"), 
          p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA. Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback. The function follows methodology pubished elsewhere:"),
          p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                  stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                 Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                 27350420."),
          p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                 alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                 10.1136/bmj.h2925. PubMed PMID: 26055828."),
          h2("Acknowedgements"),
          h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
          ))),
     
#By number of clusters per sequence 
    conditionalPanel(
      #condition = "input.choice4=='Determine number of clusters' && input.choice_corr!='exch'",
      condition = "input.choice4=='Determine number of clusters' && input.choice2 =='Cross-over' ||  input.choice4=='Determine number of clusters' && input.choice2=='Stepped-wedge'||  input.choice4=='Determine number of clusters' && input.choice2=='Multi cross-over' ||  input.choice4=='Determine number of clusters' && input.choice2=='Upload matrix'",
      tabsetPanel(
        tabPanel("Power", fixedPanel(h1(plotlyOutput("PowerByNoClusters_bysequence")),
        h6("Curve shows the increase in power as the number of clusters increases (for a fixed cluster-period size). Hover cursor over curve to see actual power values"), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary and count outcomes"),
        h4("Parameters"),
        conditionalPanel(
          condition = "input.choice=='Continuous'",
          h6(textOutput("continuousG"))),
        conditionalPanel(
          condition = "input.choice=='Binomial'",
          h6(textOutput("binaryG"))),
        conditionalPanel(
          condition = "input.choice=='Count'",
          h6(textOutput("countG"))),
        h6(textOutput("alphaG")),
        conditionalPanel(
          condition = "input.choice4!='Trade-off for fixed power' & input.choice2!='Upload matrix' & input.choice_corr!='dtd' & input.choice_corr!='ctd' ",
          h6(textOutput("tdistG"))),
        conditionalPanel(
          condition = "input.varyingclustersizes=='Yes'",
          h6(textOutput("cvG"))),
        conditionalPanel(
          condition = "input.choice2=='Stepped-wedge'",
          h6(textOutput("wG"))),
        conditionalPanel(
          condition = "input.choice2=='Multi cross-over'",
          h6(textOutput("tG"))),
          h6(textOutput("mbG")),
        conditionalPanel(
          condition = "input.choice_corr=='exch'",
          h6(textOutput("iccG"))),
        conditionalPanel(
          condition = "input.choice_corr!='exch'",
          h6(textOutput("wp_iccG"))),
        conditionalPanel(
          condition = "input.choice3=='Cohort'",
          h6(textOutput("iacG")))
        )),
        tabPanel("Precision", fixedPanel(h1(plotlyOutput("PrecisionByNoClusters_bysequence")),h6("Curve shows the increase in precision as the number of clusters increases (for a fixed cluster-period size). Hover cursor over curve to see actual precision values"),  h6("Precision is defined as 1/variance of the treatment effect. When the precision no longer increases the confidence intervals around the estimated effect size will no longer decrease."), h6("Warning: caution is needed with CRTs with a small number of clusters due to risk of lack of internal and external validity; and appropriateness of calculations used particularly for binary outcomes"))),
        tabPanel("Design matrix", h4(tableOutput("DesignmatrixDispNP3")),h6("Design matrix (displayed with one cluster per treatment sequence only)")),
        tabPanel("References and Contacts",
                 h4("References and Contacts"), 
                 p("This function has been written by Karla Hemming University of Birmingham UK; Jesica Kasza Monash University Australia; and with input from Jim Hughes, Washington University USA. Please email k.hemming@bham.ac.uk if you spot any bugs, errors or have any feedback. The function follows methodology pubished elsewhere:"),
                 p("Hooper R, Teerenstra S, de Hoop E, Eldridge S. Sample size calculation for
                  stepped wedge and other longitudinal cluster randomised trials. Stat Med. 2016
                 Nov 20;35(26):4718-4728. doi: 10.1002/sim.7028. Epub 2016 Jun 28. PubMed PMID:
                 27350420."),
                 p("Hooper R, Bourke L. Cluster randomised trials with repeated cross sections:
                 alternatives to parallel group designs. BMJ. 2015 Jun 8;350:h2925. doi:
                 10.1136/bmj.h2925. PubMed PMID: 26055828."),
                 h2("Acknowedgements"),
                 h6("Karla Hemming is part funded by the NIHR CLAHRC West Midlands+ initiative. This paper presents independent research and the views expressed are those of the author(s) and not necessarily those of the NHS, the NIHR or the Department of Health.")
         )))
     )
)))
