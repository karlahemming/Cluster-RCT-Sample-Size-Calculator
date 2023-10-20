

shinyServer(function(input, output, session) {
  ###input parameters to output as text
  output$continuousA <-renderText({ 
    paste("Mean difference:", input$meandiff,"standard deviation:", input$sd)
  })
  output$binaryA <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countA <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "over dispersion:", input$thi)
  })
  output$alphaA <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tA <-renderText({ 
    paste("Number of periods:", input$T)
  })
 
  output$cvA <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wA <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistA <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kA <-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbA <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mA <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbA <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccA <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccA <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u, "CAC:", input$cac)
  })
  output$cacA <-renderText({ 
    paste("Cluster auto-correlation:", input$cac)
  })
  output$iacA <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatA <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlA <-renderText({ 
    paste("ICC under control condition:", input$icc_control, "Cluster size control condition:", input$m_control, "Number clusters control condition:", input$k_control)
  })
  output$wp_icc_controlA <-renderText({ 
    paste("WP-ICC under control condition:", input$wp_icc_control)
  })
  
  ######
  output$continuousB <-renderText({ 
    paste("Mean difference:", input$meandiff,"Standard deviation:", input$sd)
  })
  output$binaryB <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countB <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "Over dispersion:", input$thi)
  })
  output$alphaB <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tB <-renderText({ 
    paste("Number of periods:", input$T)
  })
  output$cvB <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wB <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistB <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kB <-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbB <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mB <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbB <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccB <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccB <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u , "CAC:",input$cac)
  })
  output$cacB <-renderText({ 
    paste("Cluster auto-correlation:", input$cac)
  })
  output$iacB <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatB <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlB <-renderText({ 
    paste("ICC under control condition:", input$icc_control)
  })
  output$wp_icc_controlB <-renderText({ 
    paste("WP-ICC under control condition:", input$wp_icc_control)
  })
  
  ######
  ######
  output$continuousC <-renderText({ 
    paste("Mean difference:", input$meandiff,"Standard deviation:", input$sd)
  })
  output$binaryC <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countC <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "Over dispersion:", input$thi)
  })
  output$alphaC <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tC <-renderText({ 
    paste("Number of periods:", input$T)
  })
  output$cvC <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wB <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistC <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kC <-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbC <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mC <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbC <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccC <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccC <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u, "CAC:", input$cac )
  })
  output$cacC <-renderText({ 
    paste("Cluster auto-correlation:", input$cac)
  })
  output$iacC <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatC <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlC <-renderText({ 
    paste("ICC under control condition:", input$icc_control)
  })
  output$wp_icc_controlC <-renderText({ 
    paste("WP-ICC under control condition:", input$wp_icc_control)
  })
  output$powerC <-renderText({ 
    paste("Power:", input$power)
  })
  ######
  
  ######
  output$continuousD <-renderText({ 
    paste("Mean difference:", input$meandiff,"Standard deviation:", input$sd)
  })
  output$binaryD <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countD <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "Over dispersion:", input$thi)
  })
  output$alphaD <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tD <-renderText({ 
    paste("Number of periods:", input$T)
  })
  output$cvD <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wD <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistD <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kD<-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbD <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mD <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbD <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccD <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccD <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u )
  })
  output$powerD <-renderText({ 
    paste("Power:", input$power_d)
  })
  output$iacD <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatD <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlD <-renderText({ 
    paste("ICC under control condition:", input$icc_control, "Cluster size control condition:", input$ma_control_d, "Cluster size treatment arm:", input$ma_d)
  })
  ######
  ######
  output$powerE <-renderText({ 
    paste("Power:", input$power)
  })
  output$continuousE <-renderText({ 
    paste("Mean difference:", input$meandiff,"Standard deviation:", input$sd)
  })
  output$binaryE <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countE <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "Over dispersion:", input$thi)
  })
  output$alphaE <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tE <-renderText({ 
    paste("Number of periods:", input$T)
  })
  output$cvE <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wE <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistE <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kE<-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbE <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mE <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbE <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccE <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccE <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u )
  })
  output$cacE <-renderText({ 
    paste("Cluster auto-correlation:", input$cac)
  })
  output$iacE <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatE <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlE <-renderText({ 
    paste("ICC under control condition:", input$icc_control)
  })
  output$wp_icc_controlE <-renderText({ 
    paste("WP-ICC under control condition:", input$wp_icc_control)
  })
  output$wp_iccE <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u , "CAC:",input$cac)
  })
  ######
  ######
  output$continuousF <-renderText({ 
    paste("Mean difference:", input$meandiff,"standard deviation:", input$sd)
  })
  output$binaryF <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countF <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "over dispersion:", input$thi)
  })
  output$alphaF <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tF <-renderText({ 
    paste("Number of periods:", input$T)
  })
  output$cvF <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wF <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistF <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kF<-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbF <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mF <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbF <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccF <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccF <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u, "CAC:", input$cac )
  })
  output$cacF <-renderText({ 
    paste("Cluster auto-correlation:", input$cac)
  })
  output$iacF <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatF <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlF <-renderText({
    paste("ICC under control condition:", input$icc_control, "Cluster size control condition:", input$m_control,"Number clusters condition:", input$k_control)
  })
  output$wp_icc_controlF <-renderText({ 
    paste("WP-ICC under control condition:", input$wp_icc_control)
  })
  
  ######
  ######
  output$continuousG <-renderText({ 
    paste("Mean difference:", input$meandiff,"standard deviation:", input$sd)
  })
  output$binaryG <-renderText({ 
    paste("p0:", input$p0,"p1:", input$p1)
  })
  output$countG <-renderText({ 
    paste("r0:", input$r0,"r1:", input$r1, "over dispersion:", input$thi)
  })
  output$alphaG <-renderText({ 
    paste("Significance level:", input$alpha)
  })
  output$tG <-renderText({ 
    paste("Number of periods:", input$T)
  })
  output$cvG <-renderText({ 
    paste("Coefficient of variation of cluster sizes:", input$cv)
  })
  output$wG <-renderText({ 
    paste("Number of sequences:", input$w)
  })
  output$tdistG <-renderText({ 
    paste("Normal-distribution (if false then t-distribution):", input$choice6)
  })
  output$kG<-renderText({ 
    paste("Number clusters per arm:", input$k)
  })
  output$kbG <-renderText({ 
    paste("Number clusters per sequence:", input$kb)
  })
  output$mG <-renderText({ 
    paste("Cluster size:", input$m)
  })
  output$mbG <-renderText({ 
    paste("Cluster size per period:", input$mb)
  })
  output$iccG <-renderText({ 
    paste("ICC:", input$icc, "ICC (lower):", input$icc_l,"ICC (upper):", input$icc_u )
  })
  output$wp_iccG <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u, "CAC:",input$cac )
  })
  output$cacG <-renderText({ 
    paste("Cluster auto-correlation:", input$cac)
  })
  output$iacG <-renderText({ 
    paste("Individual auto-correlation:", input$iac)
  })
  output$icc_treatG <-renderText({ 
    paste("Treatment condition SD:", input$icc_treat)
  })
  output$icc_controlG <-renderText({ 
    paste("ICC under control condition:", input$icc_control)
  })
 
  output$wp_iccG <-renderText({ 
    paste("WP-ICC:", input$wp_icc, "WP-ICC (lower):", input$wp_icc_l,"WP-ICC (upper):", input$wp_icc_u , "CAC:",input$cac)
  })
  
###Set up restrictions  
 #Restrict correlation structures for two period designs 
  observeEvent(input$choice2,{
    if(input$choice2=="Cross-over" || input$choice2=="Before and After"){
      updateRadioButtons(session, "choice_corr",
                         label = "Correlation Structure", 
                         choiceNames = c("Exchangable", "Two-period decay"),
                         choiceValues = c("exch", "clusbytime"))
    }}                    
  )
  
  observeEvent(input$choice_corr,{
   reset("cv")
    }                    
  )
  observeEvent(input$choice2,{
    reset("choice5")
  }                    
  )
  
 #Unrestrict correlation structure for other designs 
  observeEvent(input$choice2,{
    if(input$choice2=="Stepped-wedge" || input$choice2=="Multi cross-over" || input$choice2=="Parallel" || input$choice2=="Upload matrix" ){
      updateRadioButtons(session, "choice_corr",
                         label = "Correlation Structure", 
                         choiceNames = c("Exchangable", "Two-period decay", "Discrete time decay"),
                         choiceValues = c("exch", "clusbytime", "dtd"))
    }}                    
  )
 #Restrict differential clustering for exchangeable correlation structure
  observeEvent(input$choice5,{
    if(input$choice5=="True"){
      updateRadioButtons(session, "choice_corr",
                         label = "Correlation Structure", 
                         choiceNames = c("Exchangable"),
                         choiceValues = c("exch"))
    }}                    
  )
  #Unrestrict correlation structure fot non-differntial clustering
  observeEvent(input$choice5,{
    if(input$choice5=="False"){
      updateRadioButtons(session, "choice_corr",
                         label = "Correlation Structure", 
                         choiceNames = c("Exchangable", "Two-period decay", "Discrete time decay"),
                         choiceValues = c("exch", "clusbytime", "dtd"))
    }}                    
  )
  #Restrict sampling structure for parallel designs
  observeEvent(input$choice2,{
    if(input$choice2=="Parallel"){
      updateRadioButtons(session, "choice3",
                         label = "Sampling Structure", 
                         choiceValues = c("Cross-sectional"),
                         choiceNames = c("Cross-sectional sample"))
    }}                    
  )
  observeEvent(input$choice2,{
    if(input$choice2!="Parallel"){
      updateRadioButtons(session, "choice3",
                         label = "Sampling Structure", 
                         choiceValues = c("Cross-sectional", "Cohort"),
                         choiceNames = c("Cross-sectional sample", "Cohort"))
    }}                    
  )
  
  #Making slider input have max value as number needed under individual randomisation 
    observe({
    #Outcomes
    if(input$choice=="Continuous"){
      delta <- input$meandiff 
      sigma <- input$sd 
    }
    if(input$choice=="Binomial"){
      delta2 <-(input$p1-input$p0)^2
      delta<-sqrt(delta2)
      sigma2 <-((input$p0*(1-input$p0))+(input$p1*(1-input$p1)))/2
      sigma<-sqrt(sigma2)
    }
    if(input$choice=="Count"){
      delta <- input$r1-input$r0
      sigma2 <-input$thi*(input$r0+input$r1)/2
      sigma<-sqrt(sigma2)
    }
    es <- delta/sigma  
    a<-input$alpha
    power<-0.99
    ss<-ceiling(2*(((qnorm(power)+qnorm(1-a/2))*sigma/delta)^2))
    val<-ss
    updateSliderInput(session, "plotrangea",max = val) 
    updateSliderInput(session, "plotrangeb",max = val) 
    updateSliderInput(session, "plotrange2a",max = val) 
    updateSliderInput(session, "plotrange2b",max = val) 
    updateSliderInput(session, "plotrange3a",max = val) 
    updateSliderInput(session, "plotrange3b",max = val) 
    updateSliderInput(session, "plotrange3a_d",max = val) 
    updateSliderInput(session, "plotrange3b_d",max = val)
  ##  
  })
  
    
    
    
    
   #Button to create plot
   v <- reactiveValues(button1=FALSE)
  observeEvent(input$button1, {
    v$button1 = TRUE
  })
  #Set up code so user can download allocation 
  output$downloadCurve <- downloadHandler(
    filename = function() {
      paste("save_curve", ".csv", sep = "")
    },
  content = function(file) {
      write.csv(CurrentData(), file, row.names = FALSE) 
    }
  )
  output$downloadCurveDESMAT <- downloadHandler(
    filename = function() {
      paste("save_curve", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CurrentDataDESMAT(), file, row.names = FALSE) 
    }
  )
  output$downloadCurveDECAY <- downloadHandler(
    filename = function() {
      paste("save_curve", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(CurrentDataDECAY(), file, row.names = FALSE) 
    }
  )
  
#Main function (used unless upload design as matrix or correlation structure is discrete time decay)
  CurrentData <- reactive({
    #Set up default values
    icc_treat <- 0
    cv<-0
    cac<-1
    cac_l<-1
    cac_u<-1
    s<-2
    a<-0.05
    thi<-1
    #Correlations
    rho_control<-0
    iac<-0
    if(input$choice_corr=="exch"){
    rho<-input$icc
    rho_l<-input$icc_l
    rho_u<-input$icc_u
    }
    if(input$choice_corr!="exch"){
    rho<-input$wp_icc
    rho_l<-input$wp_icc_l
    rho_u<-input$wp_icc_u
    }
    if(input$choice5=="True"){
    rho_control<-input$icc_control
    }
    if(input$choice_corr=="dtd" || input$choice_corr=="clusbytime" ){
      cac<-input$cac
      cac_l<-0.8*cac
      cac_u<-1.2*cac
      if(cac_u>=1){cac_u=1}}
    #Individual corrlations
    if(input$choice3=="Cohort"){
      iac <- input$iac 
    }
    if(input$choice3!="Cohort"){
      iac <- 0
    }
    #Design parameters
    if(input$varyingclustersizes=="Yes"){cv<-input$cv}
    a <- input$alpha
    t<-input$T
    w<-input$w
    if(input$choice2=="Stepped-wedge")(s<-w)
    #Cluster size (total or by period)
    if(input$choice4=="Determine cluster size"){
        if(input$choice_corr=="exch"){
          mmin<-input$plotrangea[1]
          mmax<-input$plotrangea[2] 
      }
        if(input$choice_corr!="exch"){
          mmin<-input$plotrangeb[1]
          mmax<-input$plotrangeb[2] 
        } 
    #Number clusters (per arm or sequence)  
    if(input$choice2=="Parallel"  || input$choice2=="Before and After"){
      k <- input$k   
    }
    if(input$choice2== "Cross-over" || input$choice2=="Stepped-wedge" || input$choice2=="Multi cross-over" || input$choice2=="Upload matrix"){
       k <- input$kb 
      }
    }
    #Trade off 
    if(input$choice4=="Trade-off for fixed power" & input$choice5=="False"){
      power<-input$power
      if(input$choice_corr=="exch"){
        mmin<-input$plotrange3a[1]
        mmax<-input$plotrange3a[2]}
      if(input$choice_corr!="exch"){
        mmin<-input$plotrange3b[1]
        mmax<-input$plotrange3b[2]}
    }
    if(input$choice4=="Trade-off for fixed power" & input$choice5=="True"){
      power<-input$power_d
     if(input$choice2=="Parallel"){
        kmin<-input$plotrange3a_d[1]
        kmax<-input$plotrange3a_d[2]
        m<-input$ma_d
        m_control<-input$ma_control_d
        }
      if(input$choice2!="Parallel"){
        kmin<-input$plotrange3b_d[1]
        kmax<-input$plotrange3b_d[2]
        m<-input$mb_d
        m_control<-input$mb_control_d
        }
    }
    #Determine number clusters
    #Cluster size (total or by period)
    #IMPORTANT MODIFICAITON ON MARCH 3 2019
    if(input$choice4=="Determine number of clusters"){
      #if(input$choice_corr=="exch"){
      if(input$choice_corr=="exch" & input$choice2=="Parallel"){
        m<-input$m
      } 
      #if(input$choice_corr!="exch"){
      if(input$choice2=="Parallel" && input$choice_corr!="exch" || input$choice2=="Before and After"||  input$choice2=="Cross-over"|| input$choice2=="Stepped-wedge"|| input$choice2=="Multi cross-over" || input$choice2=="Upload matrix"){
        m<-input$mb
      }
      #Number clusters (per arm or sequence) 
        if(input$choice2=="Parallel"  || input$choice2=="Before and After"){
        kmin<-input$plotrange2a[1]
        kmax<-input$plotrange2a[2]
        }
      if(input$choice2== "Cross-over" || input$choice2=="Stepped-wedge" || input$choice2=="Multi cross-over" || input$choice2=="Upload matrix"){
       kmin<-input$plotrange2b[1]
        kmax<-input$plotrange2b[2]
        }
    
    }
    
    #Differential clustering
    if(input$choice5=="True" & input$choice4!="Trade-off for fixed power"){
      if(input$choice2=="Parallel"){
        m_control<-input$m_control
      }
      if(input$choice2=="Parallel"){
        k_control <- input$k_control  
      }
      }
   
    #Outcomes
    if(input$choice=="Continuous"){
    delta <- input$meandiff 
    sigma <- input$sd 
    }
    if(input$choice=="Binomial"){
      delta2 <-(input$p1-input$p0)^2
      delta<-sqrt(delta2)
      sigma2 <-((input$p0*(1-input$p0))+(input$p1*(1-input$p1)))/2
      sigma<-sqrt(sigma2)
    }
    if(input$choice=="Count"){
      delta <- input$r1-input$r0
      sigma2 <-input$thi*(input$r0+input$r1)/2
      sigma<-sqrt(sigma2)
    }
    updateNumericInput(session,"icc_treat", "Treatment condition SD", value = 0, min=0, max=NA, step=NA)

    
    #Design effects
    es <- delta/sigma 
    de_clustering=function(x){1+(x-1)*rho}
    de_clustering_l=function(x){1+(x-1)*rho_l}
    de_clustering_u=function(x){1+(x-1)*rho_u}
    
    if(input$varyingclustersizes=="Yes"){
      de_clustering=function(x){1+(x*(1+cv^2)-1)*rho}
      de_clustering_l=function(x){1+(x*(1+cv^2)-1)*rho_l}
      de_clustering_u=function(x){1+(x*(1+cv^2)-1)*rho_u}
    }
    if(input$choice5=="True"){
      de_clustering_differential=function(x,y){((x*(1+cv^2)*y)+(m_control*k_control))*(((1+(x*(1+cv^2)-1)*rho)/((x*(1+cv^2)*y)))+((1+(m_control-1)*rho_control)/(m_control*k_control)))}
      de_clustering_differential_l=function(x,y){((x*(1+cv^2)*y)+(m_control*k_control))*(((1+(x*(1+cv^2)-1)*rho_l)/((x*(1+cv^2)*y)))+((1+(m_control-1)*rho_control)/(m_control*k_control)))}
      de_clustering_differential_u=function(x,y){((x*(1+cv^2)*y)+(m_control*k_control))*(((1+(x*(1+cv^2)-1)*rho_u)/((x*(1+cv^2)*y)))+((1+(m_control-1)*rho_control)/(m_control*k_control)))}
    }
    r_fn=function(x){((x*rho*cac)+((1-rho)*iac))/(1+(x-1)*rho)}
    r_fn_cl=function(x){((x*rho*cac_l)+((1-rho)*iac))/(1+(x-1)*rho)}
    r_fn_cu=function(x){((x*rho*cac_u)+((1-rho)*iac))/(1+(x-1)*rho)}
    r_fn_l=function(x){((x*rho_l*cac)+((1-rho_l)*iac))/(1+(x-1)*rho_l)}
    r_fn_u=function(x){((x*rho_u*cac)+((1-rho_u)*iac))/(1+(x-1)*rho_u)}
    
    if(input$varyingclustersizes=="Yes"){
      r_fn=function(x){((x*(1+cv^2)*rho*cac)+((1-rho)*iac))/(1+(x*(1+cv^2)-1)*rho)}
      r_fn_cl=function(x){((x*(1+cv^2)*rho*cac_l)+((1-rho)*iac))/(1+(x*(1+cv^2)-1)*rho)}
      r_fn_cu=function(x){((x*(1+cv^2)*rho*cac_u)+((1-rho)*iac))/(1+(x*(1+cv^2)-1)*rho)}
      r_fn_l=function(x){((x*(1+cv^2)*rho_l*cac)+((1-rho_l)*iac))/(1+(x*(1+cv^2)-1)*rho_l)}
      r_fn_u=function(x){((x*(1+cv^2)*rho_u*cac)+((1-rho_u)*iac))/(1+(x*(1+cv^2)-1)*rho_u)}
    }
    
    if(input$choice2=="Before and After"){
      t<-2
      de_repeated=function(x){2*(1-r_fn(x)^2)}
      de_repeated_l=function(x){2*(1-r_fn_l(x)^2)}
      de_repeated_u=function(x){2*(1-r_fn_u(x)^2)}
      de_repeated_cl=function(x){2*(1-r_fn_cl(x)^2)}
      de_repeated_cu=function(x){2*(1-r_fn_cu(x)^2)}
    }
    
    if(input$choice2=="Parallel" && input$choice_corr=="exch"){
      t<-1
      de_repeated=function(x){1}
      de_repeated_l=function(x){1}
      de_repeated_u=function(x){1}
      de_repeated_cl=function(x){1}
      de_repeated_cu=function(x){1}
    }
    if(input$choice2=="Cross-over"){
      #Under two period decay
      t<-2
      de_repeated=function(x){2*(1-r_fn(x))/2}
      de_repeated_l=function(x){2*(1-r_fn_l(x))/2}
      de_repeated_u=function(x){2*(1-r_fn_u(x))/2}
      de_repeated_cl=function(x){2*(1-r_fn_cl(x))/2}
      de_repeated_cu=function(x){2*(1-r_fn_cu(x))/2}
    }
    if(input$choice2=="Stepped-wedge"){
      #Under two period decay 
      t<-w+1
      de_repeated=function(x){(w+1)*(3*w*(1-r_fn(x))*(1+w*r_fn(x)))/(((w^2)-1)*(2+w*r_fn(x)))}
      de_repeated_l=function(x){(w+1)*(3*w*(1-r_fn_l(x))*(1+w*r_fn_l(x)))/(((w^2)-1)*(2+w*r_fn_l(x)))}
      de_repeated_u=function(x){(w+1)*(3*w*(1-r_fn_u(x))*(1+w*r_fn_u(x)))/(((w^2)-1)*(2+w*r_fn_u(x)))}
      de_repeated_cl=function(x){(w+1)*(3*w*(1-r_fn_cl(x))*(1+w*r_fn_cl(x)))/(((w^2)-1)*(2+w*r_fn_cl(x)))}
      de_repeated_cu=function(x){(w+1)*(3*w*(1-r_fn_cu(x))*(1+w*r_fn_cu(x)))/(((w^2)-1)*(2+w*r_fn_cu(x)))}
    }
    
    if(input$choice2=="Parallel" && input$choice_corr =="clusbytime"){
      de_repeated=function(x){t*(1+((t-1)*r_fn(x)))/t}
      de_repeated_l=function(x){t*(1+((t-1)*r_fn_l(x)))/t}
      de_repeated_u=function(x){t*(1+((t-1)*r_fn_u(x)))/t}
     de_repeated_cl=function(x){t*(1+((t-1)*r_fn_cl(x)))/t}
     de_repeated_cu=function(x){t*(1+((t-1)*r_fn_cu(x)))/t}
    }
    
    if(input$choice2=="Multi cross-over"){
      #Under two period decay 
      de_repeated=function(x){t*(1-r_fn(x))/(t)}
      de_repeated_l=function(x){t*(1-r_fn_l(x))/(t)}
      de_repeated_u=function(x){t*(1-r_fn_u(x))/(t)}
      de_repeated_cl=function(x){t*(1-r_fn_cl(x))/(t)}
      de_repeated_cu=function(x){t*(1-r_fn_cu(x))/(t)}
    }
    
    if(input$choice4=="Determine number of clusters"){
      xtmp<-c(kmin:kmax)
      #dof<-(xtmp*t*s)-(t+xtmp)
      dof<-(xtmp*s)-t-1
      #Normal distribution
      if(input$choice6== "True"){
        power2=function(x){pnorm(sqrt(s*t*x*m*(es^2)/(4*de_clustering(m)*de_repeated(m)))-qnorm(1-a/2))}
        power2_l=function(x){pnorm(sqrt(s*t*x*m*(es^2)/(4*de_clustering_l(m)*de_repeated_l(m)))-qnorm(1-a/2))}
        power2_u=function(x){pnorm(sqrt(s*t*x*m*(es^2)/(4*de_clustering_u(m)*de_repeated_u(m)))-qnorm(1-a/2))}
        power2_cl=function(x){pnorm(sqrt(s*t*x*m*(es^2)/(4*de_clustering(m)*de_repeated_cl(m)))-qnorm(1-a/2))}
        power2_cu=function(x){pnorm(sqrt(s*t*x*m*(es^2)/(4*de_clustering(m)*de_repeated_cu(m)))-qnorm(1-a/2))}
        precision2 =function(x){(s*t*m*x)/(4*sigma^2*de_clustering(m)*de_repeated(m))}
        precision2_l =function(x){(s*t*m*x)/(4*sigma^2*de_clustering_l(m)*de_repeated_l(m))}
        precision2_u =function(x){(s*t*m*x)/(4*sigma^2*de_clustering_u(m)*de_repeated_u(m))}
        precision2_cl =function(x){(s*t*m*x)/(4*sigma^2*de_clustering(m)*de_repeated_cl(m))}
        precision2_cu=function(x){(s*t*m*x)/(4*sigma^2*de_clustering(m)*de_repeated_cu(m))}
      }
      #T-distribution
      if(input$choice6== "False"){
      power2=function(x){pt(sqrt(s*t*x*m*(es^2)/(4*de_clustering(m)*de_repeated(m)))-qt(1-a/2, df=dof), df=dof)}
      power2_l=function(x){pt(sqrt(s*t*x*m*(es^2)/(4*de_clustering_l(m)*de_repeated_l(m)))-qt(1-a/2, df=dof), df=dof)}
      power2_u=function(x){pt(sqrt(s*t*x*m*(es^2)/(4*de_clustering_u(m)*de_repeated_u(m)))-qt(1-a/2, df=dof), df=dof)}
      power2_cl=function(x){pt(sqrt(s*t*x*m*(es^2)/(4*de_clustering(m)*de_repeated_cl(m)))-qt(1-a/2, df=dof), df=dof)}
      power2_cu=function(x){pt(sqrt(s*t*x*m*(es^2)/(4*de_clustering(m)*de_repeated_cu(m)))-qt(1-a/2, df=dof), df=dof)}
      precision2 =function(x){(s*t*m*x)/(4*sigma^2*de_clustering(m)*de_repeated(m))}
      precision2_l =function(x){(s*t*m*x)/(4*sigma^2*de_clustering_l(m)*de_repeated_l(m))}
      precision2_u =function(x){(s*t*m*x)/(4*sigma^2*de_clustering_u(m)*de_repeated_u(m))}
      precision2_cl =function(x){(s*t*m*x)/(4*sigma^2*de_clustering(m)*de_repeated_cl(m))}
      precision2_cu =function(x){(s*t*m*x)/(4*sigma^2*de_clustering(m)*de_repeated_cu(m))}
      }
      if(input$choice5=="True"){
        #Normal distribution
        if(input$choice6=="True"){
        power2=function(x){pnorm(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(m,x)*de_repeated_l(m)))-qnorm(1-a/2))}
        power2_l=function(x){pnorm(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_l(m,x)*de_repeated_l(m)))-qnorm(1-a/2))}
        power2_u=function(x){pnorm(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_u(m,x)*de_repeated_u(m)))-qnorm(1-a/2))}
        power2_cl=function(x){pnorm(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(m,x)*de_repeated_cl(m)))-qnorm(1-a/2))}
        power2_cu=function(x){pnorm(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(m,x)*de_repeated_cu(m)))-qnorm(1-a/2))}
        precision2 =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(m,x)*de_repeated(m))}   
        precision2_l =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_l(m,x)*de_repeated_l(m))}   
        precision2_u =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_u(m,x)*de_repeated_u(m))}   
        precision2_cl =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(m,x)*de_repeated_cl(m))}   
        precision2_cu =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(m,x)*de_repeated_cu(m))}   
        
        
        }
        #Normal distribution
        if(input$choice6=="False"){
          power2=function(x){pt(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(m,x)*de_repeated_l(m)))-qt(1-a/2, df=dof), df=dof)}
          power2_l=function(x){pt(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_l(m,x)*de_repeated_l(m)))-qt(1-a/2, df=dof), df=dof)}
          power2_u=function(x){pt(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_u(m,x)*de_repeated_u(m)))-qt(1-a/2, df=dof), df=dof)}
          power2_cl=function(x){pt(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(m,x)*de_repeated_cl(m)))-qt(1-a/2, df=dof), df=dof)}
          power2_cu=function(x){pt(sqrt(((t*x*m)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(m,x)*de_repeated_cu(m)))-qt(1-a/2, df=dof), df=dof)}
          precision2 =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(m,x)*de_repeated(m))}   
          precision2_l =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_l(m,x)*de_repeated_l(m))}   
          precision2_u =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_u(m,x)*de_repeated_u(m))}   
          precision2_cl =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(m,x)*de_repeated_cl(m))}   
          precision2_cu =function(x){((t*x*m)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(m,x)*de_repeated_cu(m))}   
          
          }
        }

      power_x<-power2(xtmp)
      power_x_l<-power2_l(xtmp)
      power_x_u<-power2_u(xtmp)
      power_x_cl<-power2_cl(xtmp)
      power_x_cu<-power2_cu(xtmp)
      precision_x<-precision2(xtmp)
      precision_x_l<-precision2_l(xtmp)
      precision_x_u<-precision2_u(xtmp)
      precision_x_cl<-precision2_cl(xtmp)
      precision_x_cu<-precision2_cu(xtmp)
      no_clusters_x<-xtmp
      no_clusters_l<-xtmp
      no_clusters_u<-xtmp
      no_clusters_cl<-xtmp
      no_clusters_cu<-xtmp
    }
    
    if(input$choice4=="Determine cluster size"){
    xtmp<-c(mmin:mmax)
    #dof<-(k*t*s)-(t+k)
    #dof<-(xtmp*s)-t-1
    # Oct 2023 change the dof
    dof<-(k*s*t)-t-1
    #Normal approximation 
    if(input$choice6=="True"){
    precision =function(x){(s*t*x*k)/(4*sigma^2*de_clustering(x)*de_repeated(x))} 
    precision_l =function(x){(s*t*x*k)/(4*sigma^2*de_clustering_l(x)*de_repeated_l(x))} 
    precision_u =function(x){(s*t*x*k)/(4*sigma^2*de_clustering_u(x)*de_repeated_u(x))} 
    precision_cl =function(x){(s*t*x*k)/(4*sigma^2*de_clustering(x)*de_repeated_cl(x))} 
    precision_cu =function(x){(s*t*x*k)/(4*sigma^2*de_clustering(x)*de_repeated_cu(x))} 
    power=function(x){pnorm(sqrt(s*t*x*k*(es^2)/(4*de_clustering(x)*de_repeated(x)))-qnorm(1-a/2))}
    power_l=function(x){pnorm(sqrt(s*t*x*k*(es^2)/(4*de_clustering_l(x)*de_repeated_l(x)))-qnorm(1-a/2))}
    power_u=function(x){pnorm(sqrt(s*t*x*k*(es^2)/(4*de_clustering_u(x)*de_repeated_u(x)))-qnorm(1-a/2))}
    power_cl=function(x){pnorm(sqrt(s*t*x*k*(es^2)/(4*de_clustering(x)*de_repeated_cl(x)))-qnorm(1-a/2))}
    power_cu=function(x){pnorm(sqrt(s*t*x*k*(es^2)/(4*de_clustering(x)*de_repeated_cu(x)))-qnorm(1-a/2))}
    }
    #T-distribution
    if(input$choice6=="False"){
      precision =function(x){(s*t*x*k)/(4*sigma^2*de_clustering(x)*de_repeated(x))} 
      precision_l =function(x){(s*t*x*k)/(4*sigma^2*de_clustering_l(x)*de_repeated_l(x))} 
      precision_u =function(x){(s*t*x*k)/(4*sigma^2*de_clustering_u(x)*de_repeated_u(x))} 
      precision_cl =function(x){(s*t*x*k)/(4*sigma^2*de_clustering(x)*de_repeated_cl(x))} 
      precision_cu =function(x){(s*t*x*k)/(4*sigma^2*de_clustering(x)*de_repeated_cu(x))} 
      power=function(x){pt(sqrt(s*t*x*k*(es^2)/(4*de_clustering(x)*de_repeated(x)))-qt(1-a/2, df=dof), df=dof)}
      power_l=function(x){pt(sqrt(s*t*x*k*(es^2)/(4*de_clustering_l(x)*de_repeated_l(x)))-qt(1-a/2, df=dof), df=dof)}
      power_u=function(x){pt(sqrt(s*t*x*k*(es^2)/(4*de_clustering_u(x)*de_repeated_u(x)))-qt(1-a/2, df=dof), df=dof)}
      power_cl=function(x){pt(sqrt(s*t*x*k*(es^2)/(4*de_clustering(x)*de_repeated_cl(x)))-qt(1-a/2, df=dof), df=dof)}
      power_cu=function(x){pt(sqrt(s*t*x*k*(es^2)/(4*de_clustering(x)*de_repeated_cu(x)))-qt(1-a/2, df=dof), df=dof)}
    }
     if(input$choice5=="True"){
      #Normal approximation 
      if(input$choice6=="True"){
      precision =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(x,k)*de_repeated(x))} 
      precision_l =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_l(x,k)*de_repeated_l(x))} 
      precision_u =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_u(x,k)*de_repeated_u(x))} 
      precision_cl =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(x,k)*de_repeated_cl(x))} 
      precision_cu =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(x,k)*de_repeated_cu(x))} 
      power=function(x){pnorm(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(x,k)*de_repeated(x)))-qnorm(1-a/2))}
      power_l=function(x){pnorm(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_l(x,k)*de_repeated_l(x)))-qnorm(1-a/2))}
      power_u=function(x){pnorm(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_u(x,k)*de_repeated_u(x)))-qnorm(1-a/2))}
      power_cl=function(x){pnorm(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(x,k)*de_repeated_cl(x)))-qnorm(1-a/2))}
      power_cu=function(x){pnorm(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(x,k)*de_repeated_cu(x)))-qnorm(1-a/2))} 
      }
      #T-distribution
      if(input$choice6=="False"){
        precision =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(x,k)*de_repeated(x))} 
        precision_l =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_l(x,k)*de_repeated_l(x))} 
        precision_u =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential_u(x,k)*de_repeated_u(x))} 
        precision_cl =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(x,k)*de_repeated_cl(x))} 
        precision_cu =function(x){((t*x*k)+(t*m_control*k_control))/(sigma^2*de_clustering_differential(x,k)*de_repeated_cu(x))} 
        power=function(x){pt(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(x,k)*de_repeated(x)))-qt(1-a/2, df=dof), df=dof)}
        power_l=function(x){pt(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_l(x,k)*de_repeated_l(x)))-qt(1-a/2, df=dof), df=dof)}
        power_u=function(x){pt(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential_u(x,k)*de_repeated_u(x)))-qt(1-a/2, df=dof), df=dof)}
        power_cl=function(x){pt(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(x,k)*de_repeated_cl(x)))-qt(1-a/2, df=dof), df=dof)}
        power_cu=function(x){pt(sqrt(((t*x*k)+(t*m_control*k_control))*(es^2)/(de_clustering_differential(x,k)*de_repeated_cu(x)))-qt(1-a/2, df=dof), df=dof)} 
       }  
      }
    
    power_x<-power(xtmp)
    power_x_l<-power_l(xtmp)
    power_x_u<-power_u(xtmp)
    power_x_cl<-power_cl(xtmp)
    power_x_cu<-power_cu(xtmp)
    precision_x<-precision(xtmp)
    precision_x_l<-precision_l(xtmp)
    precision_x_u<-precision_u(xtmp)
    precision_x_cl<-precision_cl(xtmp)
    precision_x_cu<-precision_cu(xtmp)
    no_clusters_x<-k
    no_clusters_l<-k
    no_clusters_u<-k
    no_clusters_cl<-k
    no_clusters_cu<-k
    }
    
    if(input$choice4=="Trade-off for fixed power"){
      if(input$choice5!="True"){
      xtmp<-c(mmin:mmax)
      power_x<-power
      power_x_l<-power
      power_x_u<-power
      power_x_cl<-power
      power_x_cu<-power
      no_clusters=function(x){(de_clustering(x)*de_repeated(x)*4*(qnorm(power)+qnorm(1-a/2))^2)/(s*t*x*es^2)}
      no_clusters_l=function(x){(de_clustering_l(x)*de_repeated_l(x)*4*(qnorm(power)+qnorm(1-a/2))^2)/(s*t*x*es^2)}
      no_clusters_u=function(x){(de_clustering_u(x)*de_repeated_u(x)*4*(qnorm(power)+qnorm(1-a/2))^2)/(s*t*x*es^2)}
      no_clusters_cl=function(x){(de_clustering(x)*de_repeated_cl(x)*4*(qnorm(power)+qnorm(1-a/2))^2)/(s*t*x*es^2)}
      no_clusters_cu=function(x){(de_clustering(x)*de_repeated_cu(x)*4*(qnorm(power)+qnorm(1-a/2))^2)/(s*t*x*es^2)}
      no_clusters_x<-no_clusters(xtmp)
      no_clusters_l<-no_clusters_l(xtmp)
      no_clusters_u<-no_clusters_u(xtmp)
      no_clusters_cl<-no_clusters_cl(xtmp)
      no_clusters_cu<-no_clusters_cu(xtmp)
      precision_x<-xtmp
      precision_x_l<-xtmp
      precision_x_u<-xtmp
      precision_x_cl<-xtmp
      precision_x_cu<-xtmp
      }
      if(input$choice5=="True"){
        xtmp<-c(kmin:kmax)
        power_x<-power
        power_x_l<-power
        power_x_u<-power
        power_x_cl<-power
        power_x_cu<-power
        no_clusters_d=function(x){((1+(m_control-1)*rho_control)/m_control)*((es^2/((qnorm(power)+qnorm(1-a/2))^2))-((1+(m-1)*rho)/(x*m)))^-1}
        no_clusters_d_l=function(x){((1+(m_control-1)*rho_control)/m_control)*((es^2/((qnorm(power)+qnorm(1-a/2))^2))-((1+(m-1)*rho_l)/(x*m)))^-1}
        no_clusters_d_u=function(x){((1+(m_control-1)*rho_control)/m_control)*((es^2/((qnorm(power)+qnorm(1-a/2))^2))-((1+(m-1)*rho_u)/(x*m)))^-1}
        fn_tss<-function(x){(m_control*no_clusters_d(x))+(m*x)}
        fn_tss_l<-function(x){(m_control*no_clusters_d_l(x))+(m*x)}
        fn_tss_u<-function(x){(m_control*no_clusters_d_u(x))+(m*x)}
        no_clusters_x<-no_clusters_d(xtmp)
        #NA values represent infeasible designs
        if(no_clusters_x<=0){no_clusters_x<-NA}
        no_clusters_l<-no_clusters_d_l(xtmp)
        if(no_clusters_l<=0){no_clusters_l<-NA}
        no_clusters_u<-no_clusters_d_u(xtmp)
        if(no_clusters_u<=0){no_clusters_u<-NA}
        total_ss<-fn_tss(xtmp)
        if(total_ss<=0){total_ss<-NA}
        total_ss_l<-fn_tss_l(xtmp)
        if(total_ss_l<=0){total_ss<-NA}
        total_ss_u<-fn_tss_u(xtmp)
        if(total_ss_u<=0){total_ss_u<-NA}
        #bit of abuse of notation so as can return tss under current dataframe set up
        precision_x<-total_ss
        precision_x_l<-xtmp
        precision_x_u<-xtmp
        precision_x_cl<-xtmp
        precision_x_cu<-xtmp
        no_clusters_cl<-total_ss_l
        no_clusters_cu<-total_ss_u
        }
    }
    data<-data.frame(xtmp,power_x,power_x_l,power_x_u,power_x_cl,power_x_cu,precision_x,precision_x_l,precision_x_u,precision_x_cl,precision_x_cu,no_clusters_x,no_clusters_l,no_clusters_u,no_clusters_cl,no_clusters_cu)
  })

#Discrete time decay correlation  

  CurrentDataDECAY <- reactive({
    
    #Defaults
    cv<-0
    cac<-1
    cac_l<-1
    cac_u<-1
    icc_treat <- 0
    iac<-0
    rho_control<-0
    a<-0.05
    s<-2
    #correlations
    rho<-input$wp_icc
    rho_l<-input$wp_icc_l
    rho_u<-input$wp_icc_u
    cac<-input$cac
    cac_l<-0.8*cac
    cac_u<-1.8*cac
    if(cac_u>=1){cac_u=1}
    if(input$choice3=="Cohort"){
      iac <- input$iac 
    }
    if(input$choice3!="Cohort"){
      iac <- 0
    }
    #Desgin parameters
    a <- input$alpha
    t<-input$T
    w<-input$w
    if(input$choice2=="Stepped-wedge")(s<-w)
    if(input$choice2=="Stepped-wedge")(t<-w+1)
    #Determine cluster size
    if(input$choice4=="Determine cluster size"){
      if(input$choice_corr!="exch"){
        mmin<-input$plotrangeb[1]
        mmax<-input$plotrangeb[2]
        if(mmin == 0) mmin <- 1
        if(mmax == 0) mmax <- 1
      } 
      if(input$choice_corr=="exch"){
        mmin<-input$plotrangea[1]
        mmax<-input$plotrangea[2]
        if(mmin == 0) mmin <- 1
        if(mmax == 0) mmax <- 1
      } 
      if(input$choice2=="Parallel"  || input$choice2=="Before and After"){
        k <- input$k   
      }
      if(input$choice2== "Cross-over" || input$choice2=="Stepped-wedge" || input$choice2=="Multi cross-over" || input$choice2=="Upload matrix"){
        k <- input$kb 
      }
  
    }
    
    if(input$choice4=="Trade-off for fixed power"){
      power<-input$power
      if(input$choice_corr!="exch"){
        mmin<-input$plotrange3b[1]
        mmax<-input$plotrange3b[2]
       
        if(mmin == 0) mmin <- 1
        if(mmax == 0) mmax <- 1
      }
    }
    
    if(input$choice4=="Determine number of clusters"){
      if(input$choice_corr=="exch"){
        m<-input$m
      } 
      if(input$choice_corr!="exch"){
        m<-input$mb
      }
      if(input$choice2=="Parallel" || input$choice2=="Before and After"){
        kmin<-input$plotrange2a[1]
        kmax<-input$plotrange2a[2]
      }
      
     if(input$choice2== "Cross-over" || input$choice2=="Stepped-wedge" || input$choice2=="Multi cross-over"|| input$choice2=="Upload matrix"){
        kmin<-input$plotrange2b[1]
        kmax<-input$plotrange2b[2]
      }
      if(kmin == 0) kmin <- 1
      if(kmax == 0) kmax <- 1
    }
    
    
   
    
    if(input$choice=="Continuous"){
      delta <- input$meandiff 
      sigma <- input$sd 
    }
    if(input$choice=="Binomial"){
      delta2 <-(input$p1-input$p0)^2
      delta<-sqrt(delta2)
      sigma2 <-((input$p0*(1-input$p0))+(input$p1*(1-input$p1)))/2
      sigma<-sqrt(sigma2)
    }
    
    if(input$choice=="Count"){
      delta <- input$r1-input$r0
      sigma2 <-input$thi*(input$r0+input$r1)/2
      sigma<-sqrt(sigma2)
    }
    es <- delta/sigma 
    
    if(input$choice4=="Determine number of clusters"){
      xtmp<-c(kmin:kmax)
      precision_x <- 1/sapply(xtmp, vartheta_Krep, input$choice2, t, m, rho, cac, iac, sigma)
      precision_x_cl <- 1/sapply(xtmp, vartheta_Krep, input$choice2, t, m, rho, cac_l, iac, sigma)
      precision_x_cu <- 1/sapply(xtmp, vartheta_Krep, input$choice2, t, m, rho, cac_u, iac, sigma)
      precision_x_l <- 1/sapply(xtmp, vartheta_Krep, input$choice2, t, m, rho_l, cac, iac, sigma)
      precision_x_u <- 1/sapply(xtmp, vartheta_Krep, input$choice2, t, m, rho_u, cac, iac, sigma)
    
      power_x <- pnorm(sqrt(precision_x)*(delta)-qnorm(1-a/2))
      power_x_l<-pnorm(sqrt(precision_x_l)*(delta)-qnorm(1-a/2))
      power_x_u<-pnorm(sqrt(precision_x_u)*(delta)-qnorm(1-a/2))
      power_x_cl<-pnorm(sqrt(precision_x_cl)*(delta)-qnorm(1-a/2))
      power_x_cu<-pnorm(sqrt(precision_x_cu)*(delta)-qnorm(1-a/2))
      
      #power_x <- pnorm(sqrt(precision_x)*(es)-qnorm(1-a/2))
      #power_x_l<-pnorm(sqrt(precision_x_l)*(es)-qnorm(1-a/2))
      #power_x_u<-pnorm(sqrt(precision_x_u)*(es)-qnorm(1-a/2))
      #power_x_cl<-pnorm(sqrt(precision_x_cl)*(es)-qnorm(1-a/2))
      #power_x_cu<-pnorm(sqrt(precision_x_cu)*(es)-qnorm(1-a/2))
      no_clusters_x<-xtmp
      no_clusters_l<-xtmp
      no_clusters_u<-xtmp
      no_clusters_cl<-xtmp
      no_clusters_cu<-xtmp
    }
    
    if(input$choice4=="Determine cluster size"){
      xtmp<-c(mmin:mmax)
      precision_x <- 1/sapply(xtmp, vartheta_m, input$choice2, t, k, rho, cac, iac, sigma)
      precision_x_cl <- 1/sapply(xtmp, vartheta_m, input$choice2, t, k, rho, cac_l, iac, sigma)
      precision_x_cu <- 1/sapply(xtmp, vartheta_m, input$choice2, t, k, rho, cac_u, iac, sigma)
      precision_x_l <- 1/sapply(xtmp, vartheta_m, input$choice2, t, k, rho_l, cac, iac, sigma)
      precision_x_u <- 1/sapply(xtmp, vartheta_m, input$choice2, t, k, rho_u, cac, iac, sigma)
      power_x <- pnorm(sqrt(precision_x)*(delta)-qnorm(1-a/2))
      power_x_l<-pnorm(sqrt(precision_x_l)*(delta)-qnorm(1-a/2))
      power_x_u<-pnorm(sqrt(precision_x_u)*(delta)-qnorm(1-a/2))
      power_x_cl<-pnorm(sqrt(precision_x_cl)*(delta)-qnorm(1-a/2))
      power_x_cu<-pnorm(sqrt(precision_x_cu)*(delta)-qnorm(1-a/2))
      no_clusters_x<-k
      no_clusters_l<-k
      no_clusters_u<-k
      no_clusters_cl<-k
      no_clusters_cu<-k
    }
    
    if(input$choice4=="Trade-off for fixed power"){
      xtmp<-c(mmin:mmax)
      power_x<-power
      power_x_l<-power
      power_x_u<-power
      power_x_cl<-power
      power_x_cu<-power
      precision_x <- 1/sapply(xtmp, vartheta_m, input$choice2, t, 1, rho, cac, iac, sigma)
      precision_x_cl <- 1/sapply(xtmp, vartheta_m, input$choice2, t, 1, rho, cac_l, iac, sigma)
      precision_x_cu <- 1/sapply(xtmp, vartheta_m, input$choice2, t, 1, rho, cac_u, iac, sigma)
      precision_x_l <- 1/sapply(xtmp, vartheta_m, input$choice2, t, 1, rho_l, cac, iac, sigma)
      precision_x_u <- 1/sapply(xtmp, vartheta_m, input$choice2, t, 1, rho_u, cac, iac, sigma)      
      no_clusters_x <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x*delta^2)
      no_clusters_l <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_l*delta^2)
      no_clusters_u <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_u*delta^2)
      no_clusters_cl <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_cl*delta^2)
      no_clusters_cu <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_cu*delta^2)
    }
    data<-data.frame(xtmp,power_x,power_x_l,power_x_u,power_x_cl,power_x_cu,precision_x,precision_x_l,precision_x_u,precision_x_cl,precision_x_cu,no_clusters_x,no_clusters_l,no_clusters_u,no_clusters_cl,no_clusters_cu)
  })
  
  
  #When user uploads design matrix 
  CurrentDataDESMAT <- eventReactive(input$button1,{
    #Defaults 
    cv<-0
    cac<-1
    cac_l<-1
    cac_u<-1
    icc_treat<-0
    rho_control<-0
    iac<-0
    a<-0.05
    #Correlations
    if(input$choice_corr=="exch"){
      rho<-input$icc
      rho_l<-input$icc_l
      rho_u<-input$icc_u
    }
    if(input$choice_corr!="exch"){
      rho<-input$wp_icc
      rho_l<-input$wp_icc_l
      rho_u<-input$wp_icc_u
    }
    if(input$choice_corr=="dtd" || input$choice_corr=="clusbytime" ){
      cac<-input$cac
      cac_l<-0.8*cac
      cac_u<-1.2*cac
      if(cac_u>=1){cac_u=1}}
    if(input$choice3=="Cohort"){
      iac <- input$iac 
    }
    if(input$choice3!="Cohort"){
      iac <- 0
    }
    if(input$treatment_variation==1){      
      icc_treat<-input$icc_treat
    }
    ##If the user selects a decaying structure, set the correlation matrix type correctly
    type <- 0 
    if(input$choice_corr=="dtd") type<-1
    #Design parameters
    a <- input$alpha
   
    if(input$choice4=="Determine cluster size"){
        mmin<-input$plotrangeb[1]
        mmax<-input$plotrangeb[2] 
        if(mmin == 0) mmin <- 1
        if(mmax == 0) mmax <- 1
        k <- input$kb 
    }
    
    if(input$choice4=="Trade-off for fixed power"){
      power<-input$power
        mmin<-input$plotrange3b[1]
        mmax<-input$plotrange3b[2]
        if(mmin == 0) mmin <- 1
        if(mmax == 0) mmax <- 1
    }
    
    if(input$choice4=="Determine number of clusters"){
        m<-input$mb
        kmin<-input$plotrange2b[1]
        kmax<-input$plotrange2b[2]
        if(kmin == 0) kmin <- 1
        if(kmax == 0) kmax <- 1
    }
    
 
    #Outcomes
    if(input$choice=="Continuous"){
      delta <- input$meandiff 
      sigma <- input$sd 
    }
    if(input$choice=="Binomial"){
      delta2 <-(input$p1-input$p0)^2
      delta<-sqrt(delta2)
      sigma2 <-((input$p0*(1-input$p0))+(input$p1*(1-input$p1)))/2
      sigma<-sqrt(sigma2)
    }
    
    if(input$choice=="Count"){
      delta <- input$r1-input$r0
      sigma2 <-input$thi*(input$r0+input$r1)/2
      sigma<-sqrt(sigma2)
    }
    es <- delta/sigma 

 
  #Input the user-defined desmat: 
   if(is.null(mymatrix$data)) stop("User needs to upload design matrix before the function can continue")
   if(!is.null(mymatrix$data)) desmat <- mymatrix$data
   
    
    if(input$choice4=="Determine number of clusters"){
      xtmp<-c(kmin:kmax)
      precision_x <- 1/sapply(xtmp, vartheta_Krep_desmat, desmat, m, rho, cac, iac, sigma, type, icc_treat)
      precision_x_cl <- 1/sapply(xtmp, vartheta_Krep_desmat, desmat, m, rho, cac_l, iac, sigma, type, icc_treat)
      precision_x_cu <- 1/sapply(xtmp, vartheta_Krep_desmat, desmat, m, rho, cac_u, iac, sigma, type, icc_treat)
      precision_x_l <- 1/sapply(xtmp, vartheta_Krep_desmat, desmat, m, rho_l, cac, iac, sigma, type, icc_treat)
      precision_x_u <- 1/sapply(xtmp, vartheta_Krep_desmat, desmat, m, rho_u, cac, iac, sigma, type, icc_treat)
      power_x <- pnorm(sqrt(precision_x)*(delta)-qnorm(1-a/2))
      power_x_l<-pnorm(sqrt(precision_x_l)*(delta)-qnorm(1-a/2))
      power_x_u<-pnorm(sqrt(precision_x_u)*(delta)-qnorm(1-a/2))
      power_x_cl<-pnorm(sqrt(precision_x_cl)*(delta)-qnorm(1-a/2))
      power_x_cu<-pnorm(sqrt(precision_x_cu)*(delta)-qnorm(1-a/2))
      no_clusters_x<-xtmp
      no_clusters_l<-xtmp
      no_clusters_u<-xtmp
      no_clusters_cl<-xtmp
      no_clusters_cu<-xtmp
  }
  
  if(input$choice4=="Determine cluster size"){
    xtmp<-c(mmin:mmax)
    precision_x <- 1/sapply(xtmp, vartheta_m_desmat, desmat, k, rho, cac, iac, sigma, type, icc_treat)
    precision_x_cl <- 1/sapply(xtmp, vartheta_m_desmat, desmat, k, rho, cac_l, iac, sigma, type, icc_treat)
    precision_x_cu <- 1/sapply(xtmp, vartheta_m_desmat, desmat, k, rho, cac_u, iac, sigma, type, icc_treat)
    precision_x_l <- 1/sapply(xtmp, vartheta_m_desmat, desmat, k, rho_l, cac, iac, sigma, type, icc_treat)
    precision_x_u <- 1/sapply(xtmp, vartheta_m_desmat, desmat, k, rho_u, cac, iac, sigma, type, icc_treat)
    power_x <- pnorm(sqrt(precision_x)*(delta)-qnorm(1-a/2))
    power_x_l<-pnorm(sqrt(precision_x_l)*(delta)-qnorm(1-a/2))
    power_x_u<-pnorm(sqrt(precision_x_u)*(delta)-qnorm(1-a/2))
    power_x_cl<-pnorm(sqrt(precision_x_cl)*(delta)-qnorm(1-a/2))
    power_x_cu<-pnorm(sqrt(precision_x_cu)*(delta)-qnorm(1-a/2))
    no_clusters_x<-k
    no_clusters_l<-k
    no_clusters_u<-k
    no_clusters_cl<-k
    no_clusters_cu<-k
  }
  
  if(input$choice4=="Trade-off for fixed power"){
    xtmp<-c(mmin:mmax)
    power_x<-power
    power_x_l<-power
    power_x_u<-power
    power_x_cl<-power
    power_x_cu<-power
    precision_x <- 1/sapply(xtmp, vartheta_m_desmat, desmat, 1, rho, cac, iac, sigma, type, icc_treat)
    precision_x_cl <- 1/sapply(xtmp, vartheta_m_desmat, desmat, 1, rho, cac_l, iac, sigma, type, icc_treat)
    precision_x_cu <- 1/sapply(xtmp, vartheta_m_desmat, desmat, 1, rho, cac_u, iac, sigma, type, icc_treat)
    precision_x_l <- 1/sapply(xtmp, vartheta_m_desmat, desmat, 1, rho_l, cac, iac, sigma, type, icc_treat)
    precision_x_u <- 1/sapply(xtmp, vartheta_m_desmat, desmat, 1, rho_u, cac, iac, sigma, type, icc_treat)      
    no_clusters_x <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x*delta^2)
    no_clusters_l <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_l*delta^2)
    no_clusters_u <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_u*delta^2)
    no_clusters_cl <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_cl*delta^2)
    no_clusters_cu <- ((qnorm(power)+qnorm(1-a/2))^2)/(precision_x_cu*delta^2)
  }
  
  data<-data.frame(xtmp,power_x,power_x_l,power_x_u,power_x_cl,power_x_cu,precision_x,precision_x_l,precision_x_u,precision_x_cl,precision_x_cu, no_clusters_x,no_clusters_l,no_clusters_u,no_clusters_cl,no_clusters_cu)
  
  })
  
  
  ##Code to output the design matrix
  output$DesignmatrixDisp <- renderTable({
    if(input$choice2=="Parallel")  desmat <- DesignMatrix(input$choice2, 1)
    head(desmat, n=nrow(desmat))
  },digits=0)    
  
  output$DesignmatrixDisp2 <- renderTable({
    if(input$choice2=="Parallel") myt <-1
    head(DesignMatrix(input$choice2, 1))
  },digits=0)   
  
  output$DesignmatrixDisp3 <- renderTable({
    myt <- input$T
    if(input$choice2=="Parallel") 
    head(DesignMatrix(input$choice2, myt))
  },digits=0)   
  
  output$DesignmatrixDispNP <- renderTable({
    if(input$choice2=="Parallel"){
      myt <- input$T
      mydisp <- 2
      mymat <- DesignMatrix(input$choice2, myt)
    }
    if(input$choice2=="Before and After" || input$choice2=="Cross-over"){
      myt <- 2
      mydisp <- 2
      mymat <- DesignMatrix(input$choice2, myt)
    }
    if(input$choice2=="Stepped-wedge"){
      myt <- input$w + 1
      mydisp <- input$w
      mymat <- DesignMatrix(input$choice2, myt)
    } 
    
    if(input$choice2=="Multi cross-over"){
      myt <- input$T
      mydisp <- 2      
      mymat <- DesignMatrix(input$choice2, myt)
    } 
    if(input$choice2=="Upload matrix"){
      mymat <- mymatrix$data
      mydisp <- nrow(mymat)
    }  
    head(mymat, n=mydisp)
  },digits=0)    
  
  output$DesignmatrixDispNP2 <- renderTable({
    if(input$choice2=="Parallel"){
      myt <- input$T
      mydisp <- 2
      mymat <- DesignMatrix(input$choice2, myt)
    }
    if(input$choice2=="Before and After" || input$choice2=="Cross-over"){
      myt <- 2
      mydisp <- 2
      mymat <- DesignMatrix(input$choice2, myt)
    }
    if(input$choice2=="Stepped-wedge"){
      myt <- input$w + 1
      mydisp <- input$w
      mymat <- DesignMatrix(input$choice2, myt)
    } 
    if(input$choice2=="Multi cross-over"){
      myt <- input$T
      mydisp <- 2      
      mymat <- DesignMatrix(input$choice2, myt)
    } 
    if(input$choice2=="Upload matrix"){
      mymat <- mymatrix$data
      mydisp <- nrow(mymat)
    }  
    head(mymat, n=mydisp)
  },digits=0)    
  
  output$DesignmatrixDispNP3 <- renderTable({
    if(input$choice2=="Parallel"){
      myt <- input$T
      mydisp <- 2
      mymat <- DesignMatrix(input$choice2, myt)
    }
    if(input$choice2=="Before and After" || input$choice2=="Cross-over"){
      myt <- 2
      mydisp <- 2
      mymat <- DesignMatrix(input$choice2, myt)
    }
    if(input$choice2=="Stepped-wedge"){
      myt <- input$w + 1
      mydisp <- input$w
      mymat <- DesignMatrix(input$choice2, myt)
    } 
    if(input$choice2=="Multi cross-over"){
      myt <- input$T
      mydisp <- 2      
      mymat <- DesignMatrix(input$choice2, myt)
    } 
    if(input$choice2=="Upload matrix"){
      mymat <- mymatrix$data
      mydisp <- nrow(mymat)
    }  
    head(mymat, n=mydisp)
  },digits=0)    
  
  mymatrix <- reactiveValues(data=NULL) 
  
  observe({
    req(input$file1)
    mymatrix$data <- read.csv(input$file1$datapath, header=FALSE)
  })
  
  observeEvent(input$reset, {
    mymatrix$data <- NULL
    reset('file1')
  })
  

   ##Power plots by number clusters per arm
   output$PowerByNoClusters_byarm <- renderPlotly({
     if(input$choice5=="True") {
     p_powerbynoclusters_byarm<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per treatment arm: ", xtmp, ";  Power: ", power_x)) %>%
       add_lines(y=~power_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per treatment arm:", xtmp, ";  Power: ", power_x_l))  %>%
       add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per treatment arm: ", xtmp, ";  Power: ", power_x_u)) %>%
         layout(xaxis=list(title="Number of clusters in treatment arm"),yaxis=list(title="Power"))
     }
    else if(input$choice2=="Parallel" && input$choice_corr == "exch" || input$choice2=="Before and After" && input$choice_corr == "exch" ) {
        #Note: if anything other than exchangeabel correlation, want plot by cluster size per period (below)
       p_powerbynoclusters_byarm<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm:", xtmp, ";  Power: ", power_x_l))  %>%
         add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_u)) %>%
         layout(xaxis=list(title="Number of clusters per arm"),yaxis=list(title="Power"))  
    }
    else if(input$choice2=="Parallel" && input$choice_corr=="clusbytime" || input$choice2=="Before and After" && input$choice_corr=="clusbytime") {
       p_powerbynoclusters_byarm<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm:", xtmp, ";  Power: ", power_x_l))  %>%
         add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_u)) %>%
         add_lines(y=~power_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_cl)) %>%
         add_lines(y=~power_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters per arm"),yaxis=list(title="Power"))  
     }
     else if(input$choice2=="Parallel" && input$choice_corr=="dtd" || input$choice2=="Before and After" && input$choice_corr=="dtd") {
       p_powerbynoclusters_byarm<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm:", xtmp, ";  Power: ", power_x_l))  %>%
         add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_u)) %>%
         add_lines(y=~power_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_cl)) %>%
         add_lines(y=~power_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Power: ", power_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters per arm"),yaxis=list(title="Power"))  
     }
     
   })
   
   ##Precision plots by number clusters per arm
   output$PrecisionByNoClusters_byarm <- renderPlotly({
     if(input$choice5=="True") {
       p_precisionbynoclusters_byarm<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per treatment arm: ", xtmp, ";  Precision: ", precision_x)) %>%
         add_lines(y=~precision_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per treatment arm:", xtmp, ";  Precision: ", precision_x_l))  %>%
         add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per treatment arm: ", xtmp, ";  Precision: ", precision_x_u)) %>%
         layout(xaxis=list(title="Number of clusters in treatment arm"),yaxis=list(title="Precision"))
     }
     else if(input$choice2=="Parallel" && input$choice_corr == "exch" || input$choice2=="Before and After" && input$choice_corr == "exch" ) {
       #Note: if anything other than exchangeabel correlation, want plot by cluster size per period (below)
       p_precisionbynoclusters_byarm<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x)) %>%
         add_lines(y=~precision_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm:", xtmp, ";  Precision: ", precision_x_l))  %>%
         add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x_u)) %>%
         layout(xaxis=list(title="Number of clusters per arm"),yaxis=list(title="Precision"))  
     }
     else if(input$choice2=="Parallel" && input$choice_corr=="clusbytime" || input$choice2=="Before and After" && input$choice_corr=="clusbytime") {
       p_precisionbynoclusters_byarm<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x)) %>%
         add_lines(y=~precision_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm:", xtmp, ";  Precision: ", precision_x_l))  %>%
         add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, "; Precision: ", precision_x_u)) %>%
         add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x_cl)) %>%
         add_lines(y=~precision_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters per arm"),yaxis=list(title="Precision"))  
     }
     else if(input$choice2=="Parallel" && input$choice_corr=="dtd" || input$choice2=="Before and After" && input$choice_corr=="dtd") {
       p_precisionbynoclusters_byarm<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base  ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x)) %>%
         add_lines(y=~precision_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm:", xtmp, ";  Precision: ", precision_x_l))  %>%
         add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x_u)) %>%
         add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, "; Precision: ", precision_x_cl)) %>%
         add_lines(y=~precision_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per arm: ", xtmp, ";  Precision: ", precision_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters per arm"),yaxis=list(title="Precision"))  
     }
     
   })
   
   
   #Power plots by number clusters per sequence
   output$PowerByNoClusters_bysequence <- renderPlotly({
     if(input$choice_corr == "dtd" && input$choice2!="Upload matrix") {
       p_powerbynoclusters_bysequence<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC; Base CAC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_l)) %>%
         add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_u)) %>%
         add_lines(y=~power_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_cl)) %>%
         add_lines(y=~power_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Power"))
     }
     else if(input$choice2=="Upload matrix") {
       p_powerbynoclusters_bysequence<-plot_ly(CurrentDataDESMAT(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC; Base CAC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_l)) %>%
         add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_u)) %>%
         add_lines(y=~power_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_cl)) %>%
         add_lines(y=~power_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusterd per sequence: ", xtmp, ";  Power: ", power_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Power"))
     }
     else if(input$choice_corr=="exch" && input$choice2!="Upload matrix"){
        p_powerbynoclusters_bysequence<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x)) %>%
          add_lines(y=~power_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_l) ) %>%
         add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_u)) %>%
           layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Power"))
     }
     #Fix issue on 27th Feb 2019
     #fix issue (Oct 2023) related to "ignores variable cluster size when graphing Number of clusters per sequence vs Power with 2-period decay"
     else if (input$choice_corr=="clusbytime" && input$choice2!="Upload matrix") {
       p_powerbynoclusters_bysequence<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC; Base CAC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_l) ) %>%
         add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_u)) %>%
         add_lines(y=~power_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_cl)) %>%
         add_lines(y=~power_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Power: ", power_x_cu)) %>%
         layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Power"))
     }
   })
     
     #Precision plots by number clusters per sequence
     output$PrecisionByNoClusters_bysequence <- renderPlotly({
       if(input$choice_corr == "dtd" && input$choice2!="Upload matrix") {
         p_precisionbynoclusters_bysequence<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x)) %>%
           add_lines(y=~precision_x_l, name="Lower ICC; Base CAC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_l)) %>%
           add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_u)) %>%
           add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision:", precision_x_cl)) %>%
           add_lines(y=~precision_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision:", precision_x_cu)) %>%
           layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Power"))
       }
       else if(input$choice2=="Upload matrix") {
         p_precisionbynoclusters_bysequence<-plot_ly(CurrentDataDESMAT(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x)) %>%
           add_lines(y=~precision_x_l, name="Lower ICC; Base CAC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_l)) %>%
           add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_u)) %>%
           add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_cl)) %>%
           add_lines(y=~precision_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusterd per sequence: ", xtmp, ";  Precision: ", precision_x_cu)) %>%
           layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Precision"))
       }
       else if(input$choice_corr=="exch" && input$choice2!="Upload matrix"){
         p_precisionbynoclusters_bysequence<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x)) %>%
           add_lines(y=~precision_x_l, name="Lower ICC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_l) ) %>%
           add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_u)) %>%
           layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Precision"))
       }
       else if (input$choice_corr=="clusbytime" && input$choice2!="Upload matrix") {
         p_precisionbynoclusters_bysequence<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x)) %>%
           add_lines(y=~precision_x_l, name="Lower ICC; Base CAC",line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision:", precision_x_l) ) %>%
           add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_u)) %>%
           add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC",line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_cl)) %>%
           add_lines(y=~precision_x_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Clusters per sequence: ", xtmp, ";  Precision: ", precision_x_cu)) %>%
           layout(xaxis=list(title="Number of clusters (per arm / sequence)"),yaxis=list(title="Precision"))
       }

   })
   
   #Power plots by cluster size 
   output$PowerByClusterSize <- renderPlotly({
     if(input$choice5=="True") {
     p_powerbyclustersize<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (treatment arm): ", xtmp, ";  Power: ", power_x)) %>%
       add_lines(y=~power_x_l, name="Lower ICC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (treatment arm): ", xtmp, ";  Power: ", power_x_l)) %>%
       add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (treatment arm): ", xtmp, ";  Power: ", power_x_u)) %>%
      layout(xaxis=list(title="Cluster size treatment arm"),yaxis=list(title="Power"))
     }
     else if(input$choice5!="True" && input$choice2=="Parallel" && input$choice_corr == "exch") {
    #Note: if anything other than exchangeabel correlation, want plot by cluster size per period (below)   
       p_powerbyclustersize<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size: ", xtmp, ";  Power: ", power_x)) %>%
         add_lines(y=~power_x_l, name="Lower ICC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size: ", xtmp, ";  Power: ", power_x_l)) %>%
         add_lines(y=~power_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size: ", xtmp, ";  Power: ", power_x_u)) %>%
         layout(xaxis=list(title="Cluster size"),yaxis=list(title="Power"))
     }
   }) 
   
   #Precision plots by cluster size 
   output$PrecisionByClusterSize <- renderPlotly({
     if(input$choice5=="True") {
       p_precisionbyclustersize<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (treatment arm): ", xtmp, ";  Precision: ", precision_x)) %>%
         add_lines(y=~precision_x_l, name="Lower ICC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (treatment arm): ", xtmp, ";  Precision: ", precision_x_l)) %>%
         add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (treatment arm): ", xtmp, ";  Precision: ", precision_x_u)) %>%
         layout(xaxis=list(title="Cluster size treatment arm"),yaxis=list(title="Precision"))
     }
     else if(input$choice5!="True" && input$choice2=="Parallel" && input$choice_corr == "exch") {
       #Note: if anything other than exchangeabel correlation, want plot by cluster size per period (below)   
       p_precisionbyclustersize<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size: ", xtmp, ";  Precision: ", precision_x)) %>%
         add_lines(y=~precision_x_l, name="Lower ICC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size: ", xtmp, ";  Precision: ", precision_x_l)) %>%
         add_lines(y=~precision_x_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size: ", xtmp, ";  Precision: ", precision_x_u)) %>%
         layout(xaxis=list(title="Cluster size"),yaxis=list(title="Precision"))
     }
   }) 
   
   
   
   #Power plots by cluster size per period
    output$PowerByClusterSize_perperiod <- renderPlotly({
      if(input$choice_corr == "dtd" && input$choice2!="Upload matrix") {
       p_powerbyclustersize_perperiod<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x)) %>%
      add_lines(y=~power_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_l)) %>%
      add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_u)) %>%
      add_lines(y=~power_x_cl, name="Base ICC; Lower CAC", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_cl)) %>%
      add_lines(y=~power_x_cu, color=I("red3"),name="Base ICC; Upper CAC",line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_cu)) %>%
      layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="Power"))
    }
    else if(input$choice2=="Upload matrix") {
        p_powerbyclustersize_perperiod<-plot_ly(CurrentDataDESMAT(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x)) %>%
          add_lines(y=~power_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_l)) %>%
          add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_u)) %>%
          add_lines(y=~power_x_cl, name="Base ICC; Lower CAC", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_cl)) %>%
          add_lines(y=~power_x_cu, color=I("red3"),name="Base ICC; Upper CAC",line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_cu)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="Power"))
    }
    
      else if(input$choice_corr=="exch" && input$choice2!="Upload matrix") {
        p_powerbyclustersize_perperiod<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x)) %>%
          add_lines(y=~power_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_l)) %>%
          add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_u)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="Power"))
      }
      else if(input$choice_corr=="clusbytime" && input$choice2!="Upload matrix"){
        p_powerbyclustersize_perperiod<-plot_ly(CurrentData(),x=~xtmp, y=~power_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x)) %>%
          add_lines(y=~power_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_l)) %>%
          add_lines(y=~power_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_u)) %>%
          add_lines(y=~power_x_cl, name="Base ICC; Lower CAC", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_cl)) %>%
          add_lines(y=~power_x_cu, color=I("red3"),name="Base ICC; Upper CAC",line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Power: ", power_x_cu)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="Power"))
      }
      
    
    }) 
    
    #Precision plots by cluster size per period
    output$PrecisionByClusterSize_perperiod <- renderPlotly({
      if(input$choice_corr == "dtd" && input$choice2!="Upload matrix") {
        p_precisionbyclustersize_perperiod<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x)) %>%
          add_lines(y=~precision_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_l)) %>%
          add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_u)) %>%
          add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_cl)) %>%
          add_lines(y=~precision_x_cu, color=I("red3"),name="Base ICC; Upper CAC",line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_cu)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="precision"))
      }
      else if(input$choice2=="Upload matrix") {
        p_precisionbyclustersize_perperiod<-plot_ly(CurrentDataDESMAT(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x)) %>%
          add_lines(y=~precision_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_l)) %>%
          add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_u)) %>%
          add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_cl)) %>%
          add_lines(y=~precision_x_cu, color=I("red3"),name="Base ICC; Upper CAC",line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_cu)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="precision"))
      }
      
      else if(input$choice_corr=="exch" && input$choice2!="Upload matrix") {
        p_precisionbyclustersize_perperiod<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x)) %>%
          add_lines(y=~precision_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_l)) %>%
          add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_u)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="precision"))
      }
      else if(input$choice_corr=="clusbytime" && input$choice2!="Upload matrix"){
        p_precisionbyclustersize_perperiod<-plot_ly(CurrentData(),x=~xtmp, y=~precision_x,type='scatter',mode='lines', name="Base ICC; Base CAC", line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x)) %>%
          add_lines(y=~precision_x_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_l)) %>%
          add_lines(y=~precision_x_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_u)) %>%
          add_lines(y=~precision_x_cl, name="Base ICC; Lower CAC", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  precision: ", precision_x_cl)) %>%
          add_lines(y=~precision_x_cu, color=I("red3"),name="Base ICC; Upper CAC",line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Precision: ", precision_x_cu)) %>%
          layout(xaxis=list(title="Cluster size (per period)"),yaxis=list(title="Precision"))
      }
      
      
    }) 
    
    
    
    #Trade off plot parallel AND exchangeable correlation
    output$TradeoffPlot_parallel <- renderPlotly({
      p_tradeoff_parallel<-plot_ly(CurrentData(),x=~xtmp, y=~no_clusters_x,type='scatter',mode='lines',name="Base ICC",line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_x)) %>%
        add_lines(y=~no_clusters_l, name="Lower ICC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_l)) %>%
        add_lines(y=~no_clusters_u, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_u)) %>%
        layout(yaxis=list(title="Number of clusters (per arm)"),xaxis=list(title="Cluster size"))
    })
    
    #3D plot for trade-off when differential clustering
    output$TradeoffPlot_parallel_d <- renderPlotly({
      p_tradeoff_parallel_d<-plot_ly(CurrentData(),x=~xtmp, y=~no_clusters_x, z=~precision_x, type='scatter3d', mode='lines',name="Base ICC",line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Number clusters treatment: ", xtmp, ";  Number clusters control: ", no_clusters_x, ";TSS: ", precision_x)) %>%
      #Updated to remove lower and upper ICC curves as this made plot very difficult to read
      #add_trace(x=~xtmp, y=~no_clusters_l, z=~no_clusters_cl, name="Lower ICC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Number clusters treatment: ", xtmp, ";  Number clusters control: ", no_clusters_l, "; TSS: ", precision_x)) %>%
      #add_trace(x=~xtmp, y=~no_clusters_u, z=~no_clusters_cu, name="Upper ICC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Number clusters treatment: ", xtmp, ";  Number clusters control: ", no_clusters_u, "; TSS: ", precision_x)) %>%
      layout(scene=list(yaxis=list(title="No. control",titlefont=list(size=12),color = 'rgb(205, 12, 24)'),xaxis=list(title="No. clusters treatment",titlefont=list(size=12),color = 'rgb(22, 96, 167)'), zaxis=list(title="TSS",titlefont=list(size=12))))
    })
    
   ##Trade off plots other (axis is cluster size per period)  
    output$TradeoffPlot_other  <- renderPlotly({
        if(input$choice_corr == "dtd" && input$choice2!="Upload matrix") { 
        p_tradeoff_other<-plot_ly(CurrentDataDECAY(),x=~xtmp, y=~no_clusters_x,type='scatter',mode='lines',name="Base ICC",line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_x)) %>%
        add_lines(y=~no_clusters_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_l)) %>%
        add_lines(y=~no_clusters_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_u)) %>%
        add_lines(y=~no_clusters_cl, name="Base ICC; Lower CAC;", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cl)) %>%
        add_lines(y=~no_clusters_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cu)) %>%
        layout(yaxis=list(title="Number of clusters (per arm / sequence)"),xaxis=list(title="Cluster size (per period)"))
      }
      else if(input$choice2=="Upload matrix") {
        p_tradeoff_other<-plot_ly(CurrentDataDESMAT(),x=~xtmp, y=~no_clusters_x,type='scatter',mode='lines',name="Base ICC",line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, "; Number clusters (per arm/sequence): ", no_clusters_x)) %>%
          add_lines(y=~no_clusters_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_l)) %>%
          add_lines(y=~no_clusters_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_u)) %>%
          add_lines(y=~no_clusters_cl, name="Base ICC; Lower CAC;", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cl)) %>%
          add_lines(y=~no_clusters_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cu)) %>%
          layout(yaxis=list(title="Number of clusters (per arm / sequence)"),xaxis=list(title="Cluster size (per period)"))
      }
    

        else if(input$choice_corr=="exch" && input$choice2!="Upload matrix" ){
          p_tradeoff_other<-plot_ly(CurrentData(),x=~xtmp, y=~no_clusters_x,type='scatter',mode='lines',name="Base ICC",line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_x)) %>%
            add_lines(y=~no_clusters_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_l)) %>%
            add_lines(y=~no_clusters_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_u)) %>%
            add_lines(y=~no_clusters_cl, name="Base ICC; Lower CAC;", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cl)) %>%
            add_lines(y=~no_clusters_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cu)) %>%
            layout(yaxis=list(title="Number of clusters (per arm / sequence)"),xaxis=list(title="Cluster size (per period)"))
        }
      
      else if(input$choice_corr=="clusbytime" && input$choice2!="Upload matrix"){
        p_tradeoff_other<-plot_ly(CurrentData(),x=~xtmp, y=~no_clusters_x,type='scatter',mode='lines',name="Base ICC",line = list(color = "black", width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_x)) %>%
          add_lines(y=~no_clusters_l, name="Lower ICC; Base CAC", line = list(color = 'rgb(205, 12, 24)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_l)) %>%
          add_lines(y=~no_clusters_u, name="Upper ICC; Base CAC", line = list(color = 'rgb(22, 96, 167)', width = 4), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_u)) %>%
          add_lines(y=~no_clusters_cl, name="Base ICC; Lower CAC;", line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cl)) %>%
          add_lines(y=~no_clusters_cu, name="Base ICC; Upper CAC", line = list(color = 'rgb(22, 96, 167)', width = 4, dash = 'dash'), hoverinfo="text", text = ~paste0("Cluster size (per period): ", xtmp, ";  Number clusters (per arm/sequence): ", no_clusters_cu)) %>%
          layout(yaxis=list(title="Number of clusters (per arm / sequence)"),xaxis=list(title="Cluster size (per period)"))
      }
      
        
        
        
          })

  
})
