my_server <- function(input, output, session) {
  
  # get help for non-inferiority
  runjs("document.getElementById('NIhelp').onclick = function() { 
           window.open('https://www.youtube.com/watch?v=ZexRMe2xbJw', '_blank');
         };"
  )
  
  # hide events (based on calculation type)
  observeEvent(input$type1, {
    if (!is.null(input$type1)){
      calctype = input$type1
      if (calctype == calcs[1]){
        shinyjs::hide(selector = ".forp")
        shinyjs::show(selector = ".forss")
      }
      else if (calctype == calcs[2]){
        shinyjs::hide(selector = ".forss")
        shinyjs::show(selector = ".forp")
      }
    }
    else {
      shinyjs::hide(selector = ".forss")
      shinyjs::hide(selector = ".forp")
    }
  }, ignoreInit = F)
  
  # hide events (based on data type)
  observeEvent(input$type2, {
    if (!is.null(input$type2)){
      datacat = input$type2
      if (datacat == datas[1]){
        shinyjs::show(id = "B-op")
        shinyjs::show(id = "BCsub")
        shinyjs::show(id = "Bsub")
        shinyjs::hide(id = "TTEsub")
        shinyjs::hide(id = "C-op")
        shinyjs::hide(id = "T-op")
      }
      else if (datacat == datas[2]){
        shinyjs::hide(id = "B-op")
        shinyjs::show(id = "BCsub")
        shinyjs::hide(id = "TTEsub")
        shinyjs::hide(id = "Bsub")
        shinyjs::show(id = "C-op")
        shinyjs::hide(id = "T-op")
      }
      else if (datacat == datas[3]){
        shinyjs::hide(id = "B-op")
        shinyjs::hide(id = "BCsub")
        shinyjs::show(id = "TTEsub")
        shinyjs::hide(id = "Bsub")
        shinyjs::hide(id = "C-op")
        shinyjs::show(id = "T-op")
        shinyjs::hide(id = "marginA")
      }
    }
    else {
      shinyjs::hide(id = "B-op")
      shinyjs::hide(id = "BCsub")
      shinyjs::hide(id = "Bsub")
      shinyjs::hide(id = "C-op")
      shinyjs::hide(id = "T-op")
      shinyjs::hide(id = "marginA")
    }
  }, ignoreInit = F)

  observeEvent(input$type4, {
    if (!is.null(input$type2)){
      datacat = input$type2
      if (datacat == datas[1] || datacat == datas[2]){
      if (!is.null(input$type4)){
        nonInf = input$type4
        if (nonInf == "sup"){
          shinyjs::hide(id = "marginA")
          shinyjs::hide(id = "des")
          shinyjs::show(selector = ".forsup")
          shinyjs::hide(selector = ".forninf")
        }
        else if (nonInf == "ninf"){
          shinyjs::show(id = "marginA")
          shinyjs::show(id = "des")
          shinyjs::hide(selector = ".forsup")
          shinyjs::show(selector = ".forninf")
        }
        else{
          shinyjs::hide(id = "marginA")
          shinyjs::hide(id = "des")
          shinyjs::hide(selector = ".forsup")
          shinyjs::hide(selector = ".forninf")
        }
      }
    }
    }
  }, ignoreInit = F)
  
  # GET USER DATA for binary calc
  output$selectB <- renderUI({
    # Applicable for All
    selectors <- div(class = "newclass",
                  div(class = 'desc',
                    HTML("<p class = 'indesc'><b>Anticipated %</b> of the <b>experimental <u>and</u> control</b> groups who will experience the outcome<sup>1</sup>:</p>"),
                    htmlOutput("restrictB"),
                  ),
                  div(class = "row",
                    div(class = "col-6",
                      numericInputIcon(inputId = "rrTreatment", NULL, value = 25, icon = list(NULL,'%'), step = 0.1),
                      p(class = "small text-center", "% in Experimental Group")),
                    div(class = "col-6",
                      numericInputIcon(inputId = "rrControl", NULL, value = 20, icon = list(NULL,'%'), step = 0.1),
                      p(class = "small text-center", "% in Control Group"))
                  ),
                  shinyjs::hidden(
                      div(id = "des", 
                        HTML("<p class = 'desc'>The <b>outcome</b> is:</p>"),
                        radioGroupButtons("type4D", choices = c("Desirable", "Undesirable"), status = "black", individual = TRUE)
                      )
                  ),
                  shinyjs::hidden(div(class = "forp",
                    div(class = 'desc',
                      HTML("<p class = 'indesc'><b>Size</b> of <b>experimental <u>and</u> control</b> groups (equal):</p>"),
                      htmlOutput("restrictBS"),
                    ),
                    numericInput("myNo", NULL, value = 6)
                  )))
    return(selectors)
  })
  
  # GET USER DATA for cts calc
  output$selectC <- renderUI({
    # Applicable for All
    selectors <- div(class = "newclass",
                  div(class = 'desc',
                    HTML("<p class = 'indesc'><b>Anticipated value</b> of outcome in the <b>experimental <u>and</u> control</b> groups<sup>1</sup>:</p>"),
                    htmlOutput("restrictC")
                  ),
                  div(class = "row",
                    div(class = "col-6",
                      numericInput(inputId = "mE", NULL, value = 8.0, step = 0.1),
                      p(class = "small text-center", "Value in Experimental Group")),
                    div(class = "col-6",
                      numericInput(inputId = "mC", NULL, value = 4.0, step = 0.1),
                      p(class = "small text-center", "Value in Control Group"))
                  ),
                  shinyjs::hidden(div(class = "forp",
                    div(class = 'desc',
                    HTML("<p class = 'indesc'><b>Size</b> of <b>experimental <u>and</u> control</b> groups:</p>"),
                    htmlOutput("restrictCS")
                    ),
                    div(class = "row",
                      div(class = "col-6",
                        numericInput("eVal", NULL, value = 10),
                        p(class = "small text-center", "Experimental Group")),
                      div(class = "col-6",
                        numericInput("cVal", NULL, value = 10),
                        p(class = "small text-center", "Control Group"))
                    )
                  )),
                  div(class = 'desc',
                    #HTML("<p class = 'indesc'><b>Standard deviation</b> of outcome<b>experimental <u>and</u> control</b> groups<sup>2</sup>:</p>"),
                    HTML("<p class = 'indesc'><b>Standard deviation</b> of outcome in population<sup>1</sup>:</p>"),
                    htmlOutput("restrictCD")
                  ),
                  numericInput(inputId = "sdChange", NULL, value = 4.3, step = 0.1))
    return(selectors)
  })

  # GET USER DATA for tte calc
  output$selectT <- renderUI({
    # Applicable for All
    selectors <- div(class = "newclass",
                    div(class = 'desc',
                    HTML(paste0("<p class = 'indesc'><b>Anticipated %</b> of the <b>experimental <u>and</u> control</b> groups who 
                             will experience the outcome over the maximum time period of the study ",htmlOutput("currentHR"),"<sup>1</sup>:</p>"))
                    ),
                    div(class = "row",
                      div(class = "col-6",
                        numericInputIcon(inputId = "pE", NULL, value = 5, icon = list(NULL,'%'), step = 0.1),
                        p(class = "small text-center", "% in Experimental Group")),
                      div(class = "col-6",
                        numericInputIcon(inputId = "pC", NULL, value = 10, icon = list(NULL,'%'), step = 0.1),
                        p(class = "small text-center", "% in Control Group"))
                    ),
                    shinyjs::hidden(div(class = "forp",
                      HTML("<p class = 'desc'><b>Size</b> of <b>experimental <u>and</u> control</b> groups:"),
                      div(class = "row",
                        div(class = "col-6",
                          numericInput("nE", NULL, value = 50),
                          p(class = "small text-center", "Experimental Group")),
                        div(class = "col-6",
                          numericInput("nC", NULL, value = 50),
                          p(class = "small text-center", "Control Group"))
                      )
                    )))
  })  
  
  # TEXT OUTPUT
  output$computed <- renderUI({
    
    # Get info
    calctype <- input$type1
    datacat <- input$type2
    
    # BINARY DATA
    if (datacat == datas[1]){
      # if (!is.null(input$type3)){
      #  studycat <- input$type3
      #  # FOR RANDOMIZED TRIAL
      # if (studycat == binarys[1]){
          a <- input$alpha
          ctrl <- input$rrControl
          trt <- input$rrTreatment
          if (!is.null(input$type4)){
            nonInf = input$type4
            if (nonInf == "sup"){
              tryCatch({
                if (calctype == calcs[1]){
                  p <- input$beta
                  ncase <- prop_n(NA, p, a, ctrl, trt)
                  if (ctrl > 0 & trt > 0 & ctrl < 100 & trt < 100){
                    return(HTML(paste0(
                      "<div class = preamble>
                      <p>A total sample of <b>", 2*ceiling(ncase), "</b>, with an experimental group of at least <b>",ceiling(ncase),"</b> and a control group of at least
                      <b>", ceiling(ncase), "</b> is required to sufficiently detect the change between a <b>",trt,"%</b> 
                      event occurrence rate in the experimental group and a <b>",ctrl,"%</b> event occurrence rate in the 
                      control group, assuming <b>",p*100,"%</b> power and a two-sided significance level of <b>",a,"</b>.</p>
                      <p>This estimate for power is obtained using the two-sample test for proportions<sup>1</sup>.</p>
                      <p>1. Rosner, Bernard (Bernard A.). Fundamentals of Biostatistics. 6th ed., Thomson-Brooks/Cole, 2006.</p>                  </div>")))
                  }
                  else return(inputinvalid)
                }
                else if (calctype == calcs[2]){
                  myn <- input$myNo
                  pval <- prop_p(myn, NA, a, ctrl, trt)
                  if (pval > 0 & myn > 0 & ctrl != trt){
                    return(HTML(paste0("<div class = preamble>
                                    <p>A power value of <b>",round(pval, 5),"</b> is obtained for a randomized control trial with equally-sized experimental and 
                                    control groups of <b>", myn, "</b> to sufficiently the change between a <b>",trt,"%</b> event occurrence rate in the experimental group 
                                    and a <b>",ctrl,"%</b> event occurrence rate in the control group, assuming a 
                                    two-sided significance level of <b>",a,"</b>.</p>
                                    <p>This estimate for power is obtained using the two-sample test for proportions<sup>1</sup>.</p>
                                    <p>1. Rosner, Bernard (Bernard A.). Fundamentals of Biostatistics. 6th ed., Thomson-Brooks/Cole, 2006.</p>
                                   </div>")))
                  }
                  else return(inputinvalid)
                }
              },
              warning = function(cond){return(inputinvalid)},
              error = function(cond){return(inputinvalid)}
              )
            }
            else if (nonInf == "ninf"){
              if (!is.null(input$type4D)){
                des = input$type4D
                myd <- input$delta
                tryCatch({
                  if (calctype == calcs[1]){
                    p <- input$beta
                    
                    if (des == "Undesirable"){
                      nsize <- epi.ssninfb(treat = (100 - trt)/100, control = (100 - ctrl)/100, delta = myd/100, n = NA, alpha = a, power = p)
                      nsize <- c(nsize$n.treat, nsize$n.control)
                    }
                    else if (des == "Desirable"){
                      nsize <- epi.ssninfb(treat = trt/100, control = ctrl/100, delta = myd/100, n = NA, alpha = a, power = p)
                      nsize <- c(nsize$n.treat, nsize$n.control)
                    }
                    
                    return(HTML(paste0(
                      "<div class = preamble>
                        <p>A total sample of <b>", 2*ceiling(nsize[1]), "</b>, with an experimental group of at least <b>",ceiling(nsize[1]),"</b> 
                                      and a control group of at least <b>", ceiling(nsize[1]), "</b>
                                      is required to sufficiently detect that a <b>",trt,"%</b> rate of occurrence of a <i>",tolower(des),"</i> outcome in the experimental group is
                                      <i>not inferior</i> to a <b>",ctrl,"%</b> rate of occurrence of the same outcome in the control group, 
                                      assuming a non-inferiority margin of <b>",myd,"%</b> (for absolute risk), <b>",p*100,"%</b>
                                      power and a one-sided significance level of <b>",a,"</b>.</p>
                                      
                                      <p>This estimate for sample size is obtained using the method described in Blackwelder, 1982.</p>
                                      
                                      <p>1. Blackwelder, W C. ''''Proving the null hypothesis in clinical trials.'' 
                                      Controlled clinical trials vol. 3,4 (1982): 345-53. doi:10.1016/0197-2456(82)90024-1</p>
                      </div>")))
                  }
                  else if (calctype == calcs[2]){
                    myn <- input$myNo
                    
                    if (des == "Undesirable"){
                      power <- epi.ssninfb(treat = (100-trt)/100, control = (100-ctrl)/100, delta = myd/100, n = myn, alpha = a, power = NA)$power
                    }
                    else if (des == "Desirable"){
                      power <- epi.ssninfb(treat = trt/100, control = ctrl/100, delta = myd/100, n = myn, alpha = a, power = NA)$power
                    }
                    
                    return(HTML(paste0(
                      "<div class = preamble>
                        <p>A power value of <b>", round(power,5), "</b> is obtained for a randomized control trial with equally-sized experimental and 
                                      control groups of <b>", myn, "</b> to sufficiently detect that a <b>",trt,"%</b> rate of occurrence of a <i>",tolower(des),"</i> outcome in the experimental group is
                                      <i>not inferior</i> to a <b>",ctrl,"%</b> rate of occurrence of the same outcome in the control group, 
                                      assuming a non-inferiority margin of <b>",myd,"%</b> (for absolute risk) and a one-sided significance level of <b>",a,"</b>.</p>
                                      
                                      <p>This estimate for sample size is obtained using the method described in Blackwelder, 1982.</p>
                                      
                                      <p>1. Blackwelder, W C. ''''Proving the null hypothesis in clinical trials.'' 
                                      Controlled clinical trials vol. 3,4 (1982): 345-53. doi:10.1016/0197-2456(82)90024-1</p>
                      </div>")))                
                    }
              },
                warning = function(cond){return(inputinvalid)},
                error = function(cond){return(inputinvalid)})
              }
            }
          }
      #  }
      # }
    }
    # CONTINUOUS DATA
    else if (datacat == datas[2]){
      # W/ PAIRED DATA
      mC <- input$mC
      mE <- input$mE
      m <- abs(mC - mE) 
      sd <- input$sdChange
      a <- input$alpha
      eVal <- 1
      cVal <- 1
      
      if (!is.null(input$type4)){
        nonInf = input$type4
        if (nonInf == "sup"){
          tryCatch(
            {
              if (calctype == calcs[1]){
                b <- input$beta
                if (m != 0 & eVal > 0 & cVal > 0 & sd != 0){
                  if (eVal == cVal){
                    ncase <- pair_n(NA, b, a, m, sd)
                    return(HTML(paste0(
                      "<div class = preamble>
                                    <p>A total sample of <b>", 2*ceiling(ncase), "</b>, with an experimental group of at least <b>",ceiling(ncase),"</b> 
                                    and a control group of at least <b>", ceiling(ncase), "</b>
                                    is required to sufficiently detect a change of <b>",m,"</b> in the means of experimental
                                    and control groups, for a population with a standard deviation of <b>",sd,"</b>, assuming <b>",b*100,"%</b>
                                    power and a two-sided significance level of <b>",a,"</b>.</p>
                                    
                                    <p>This estimate for sample size is obtained using the two-sample t-test<sup>1</sup>.</p>
                      <p>1. Rosner, Bernard (Bernard A.). Fundamentals of Biostatistics. 6th ed., Thomson-Brooks/Cole, 2006.</p>
                        </div>")))
                  }
                  else return (inputinvalid)
                }
                else return (inputinvalid)
              }
              else if (calctype == calcs[2]){
                myn <- input$myNo
                eVal <- input$eVal
                cVal <- input$cVal
                if (m > 0 & eVal > 0 & cVal > 0){
                  if (eVal == cVal){
                    pval <- pair_p(eVal, NA, a, m, sd)
                    return(HTML(paste0("<div class = preamble>
                                    <p>A power value of <b>",round(pval, 5),"</b> is obtained for an experiment with equally-sized experimental and 
                                    control groups of <b>", myn, "</b>, for a population with a standard deviation of <b>",sd,"</b>, to sufficiently detect a 
                                    change of <b>", m, "</b> with a two-sided significance level of <b>",a,"</b>.
                                    </p>
                                    
                                    <p>This estimate for power is obtained using the two-sample t-test<sup>1</sup>.</p>
                      <p>1. Rosner, Bernard (Bernard A.). Fundamentals of Biostatistics. 6th ed., Thomson-Brooks/Cole, 2006.</p>
                                    </div>")))
                  }
                  else return(inputinvalid)
                }
                else return(inputinvalid)
              }
            },
            warning = function(cond){return(inputinvalid)},
            error = function(cond){return(inputinvalid)}
          )
        }
        else if (nonInf == "ninf"){
          delta <- input$delta
            tryCatch({
              if (calctype == calcs[1]){
                b <- input$beta
                if (m != 0 & sd != 0){
                  nsize <- epi.ssninfc(treat = mE, control = mC, sigma = sd, n = NA, alpha = a, delta = delta/100, power = b)
                  nsize <- c(nsize$n.treat, nsize$n.control)
                  return(HTML(paste0(
                    "<div class = preamble>
                      <p>A total sample of <b>", 2*ceiling(nsize[1]), "</b>, with an experimental group of at least <b>",ceiling(nsize[1]),"</b> 
                                    and a control group of at least <b>", ceiling(nsize[1]), "</b>
                                    is required to sufficiently detect that a mean value of <b>",mE,"</b> in the experimental group is
                                    <i>not inferior</i> to a mean value of <b>",mC,"</b> in the control group, for a population with
                                    a standard deviation of <b>",sd,"</b>, assuming a non-inferiority margin of <b>",delta,"%</b> (for absolute risk), <b>",b*100,"%</b>
                                    power and a one-sided significance level of <b>",a,"</b>.</p>
                                    
                                    <p>This estimate for sample size is obtained using the method described in Blackwelder, 1982.</p>
                                    
                                    <p>1. Blackwelder, W C. ''''Proving the null hypothesis in clinical trials.'' 
                                    Controlled clinical trials vol. 3,4 (1982): 345-53. doi:10.1016/0197-2456(82)90024-1</p>
                                    PMID: 7160191.</p>
                  </div>")))
                }
                else return (inputinvalid)
              }
              else if (calctype == calcs[2]){
                myn <- input$myNo
                eVal <- input$eVal
                cVal <- input$cVal
                if (m != 0 & eVal > 0 & cVal > 0 & sd != 0 & eVal == cVal){
                  power <- epi.ssninfc(treat = mE, control = mC, sigma = sd, n = myn, alpha = a, delta = delta/100, power = NA)$power
                  
                  return(HTML(paste0("<div class = preamble>
                                    <p>A power value of <b>",round(power, 5),"</b> is obtained for an experiment with equally-sized experimental and 
                                    control groups of <b>", myn, "</b> to sufficiently detect that a mean value of <b>",mE,"</b> in the experimental group is
                                    <i>not inferior</i> to a mean value of <b>",mC,"</b> in the control group, for a population with
                                    a standard deviation of <b>",sd,"</b>, assuming a non-inferiority margin of <b>",delta,"%</b> (for absolute risk) and a 
                                    one-sided significance level of <b>",a,"</b>.</p>
                                    
                                    <p>This estimate for sample size is obtained using the method described in Blackwelder, 1982.</p>
                                    
                                    <p>1. Blackwelder, W C. ''''Proving the null hypothesis in clinical trials.'' 
                                    Controlled clinical trials vol. 3,4 (1982): 345-53. doi:10.1016/0197-2456(82)90024-1</p>
                                    </div>")))
                }
                else return (inputinvalid)
              }
              return(inputnotyet)
            },
            warning = function(cond){return(inputinvalid)},
            error = function(cond){return(inputinvalid)})
        }
      }
    }
    # TIME-TO-EVENT DATA
    else if (datacat == datas[3]){
      # if (!is.null(input$type3)){
      #  subdatacat <- input$type3
        # W/ PILOT DATASET
        #if (subdatacat == ttes[1]){
          # make "dat"
          a <- input$alpha
          pe <- input$pE
          pc <- input$pC
          RR <- log(1-pe/100)/log(1-pc/100)
          tryCatch(
            {
              if (calctype == calcs[1]){
                p <- input$beta
                # eVal <- input$eVal
                # cVal <- input$cVal
                eVal <- 1
                cVal <- 1
                if (eVal > 0 & cVal > 0 & pe/100 > 0 & pe/100 < 1 & pc/100 > 0 & pe/100 < 1 & RR != 0 & RR != 1 & pe != pc){
                  k <- round(eVal/cVal, 1)
                  sizes <- default_n(NA, NA, pe/100, pc/100, NA, a, p, k)
                  events <- sizes[1]
                  nE <- ceiling(k/(k+1) * sizes[2])
                  nC <- ceiling(1/(k+1) * sizes[2])
                  return(HTML(paste0("<div class = preamble>
                                  <p>Approximately <b>",events,"</b> events must be observed to sufficiently detect a hazard ratio of <b>", round(RR,1),
                                     "</b> with <b>",p*100,"%</b> power and a two-sided significance level of <b>",a,
                                     "</b>. This can be obtained from an experimental group of <b>",nE,"</b> and a control group of <b>", nC, "</b>, assuming a
                                        ratio of <b>",round(k,1), "</b> of experimental : control populations and a <b>",pe,"%</b> event occurrence rate in the experimental group 
                                        and a <b>",pc,"%</b> event occurrence rate in the control group. 
                                  </p>
                                        <p>This estimate for sample size is obtained using the method for sample size calculation
                                        described in Schoenfeld, 1983<sup>1</sup>.</p>
                                        
                                  <p>1.Schoenfeld, David A. “Sample-Size Formula for the Proportional-Hazards Regression Model.” 
                                  Biometrics, vol. 39, no. 2, 1983, pp. 499–503. JSTOR, https://doi.org/10.2307/2531021.</p>
                                  </div>")))
                }
                else return (inputinvalid)
              } 
              else if (calctype == calcs[2]){
                ne <- input$nE
                nc <- input$nC
                if (ne > 0 & nc > 0 & pe > 0 & pe < 100 & pc > 0 & pc < 100 & RR != 0 & RR != 1 & pe != pc){
                  pval <- default_p(ne, nc, pe, pc, RR, a, NA, NA)
                  return(HTML(paste0("<div class = preamble>
                                  <p>A power value of <b>",round(pval, 5),"</b> is obtained for a trial with an 
                                  experimental group of <b>",ne,"</b> and a control group of <b>", nc, "</b>, to 
                                  sufficiently detect a hazard ratio of <b>", round(RR,1), "</b> (a <b>",pe,"%</b> event occurrence rate in the 
                                  experimental group and a <b>",pc,"%</b> event occurrence rate in the control group) 
                                  with a two-sided significance level of <b>",a,"</b>.
                                  </p>
                                  
                                  <p>This estimate for power is obtained by reversing the method for sample size calculation
                                  described in Schoenfeld, 1983.
                                  </p>
                                  </div>")))}
                else return(inputinvalid)
              }
            },
            warning = function(cond){return(inputinvalid)},
            error = function(cond){return(inputinvalid)}
          )
        #}
      #}
    }
    
  })
  
  # restrict input values for binary data (rates)
  output$restrictB <- renderText({
    tt <- input$rrTreatment
    rr <- input$rrControl
    if (!is.null(tt) && !is.na(tt) && is.numeric(tt) && !is.null(rr) && !is.na(rr) && is.numeric(tt)){
      if (tt <= 0 || tt >= 100 || rr <= 0 || rr >= 100){
        return(HTML("<p class = 'danger'>Warning: Values must be greater than 0 and less than 100!</p>"))
      }
      else {
        if (!is.null(input$type4)){
          nonInf = input$type4
          if (nonInf == "sup"){
            if (rr == tt){return(HTML("<p class = 'danger'>Warning: Values cannot be equal!</p>"))}
            else return(HTML("<span></span>"))
          }
          else return(HTML("<span></span>"))
        }
      }
    }
    else{
      return(HTML("<p class = 'danger'>Warning: Values cannot be left empty!</p>"))
    }
  })
  
  # restrict sample size for binary calc
  output$restrictBS <- renderText({
    n <- input$myNo
    if (!is.null(n) && !is.na(n) && is.numeric(n)){
      if (n <= 0){
        return(HTML("<p class = 'danger'>Warning: Value must be 1 or greater!</p>"))
      }
      else return(HTML("<span></span>"))
    }
    else{
      return(HTML("<p class = 'danger'>Warning: Value cannot be left empty!</p>"))
    }
  })
  
  # restrict cts data input for standard deviation
  output$restrictCD <- renderText({
    sd <- input$sdChange
    if (!is.null(sd) && !is.na(sd) && is.numeric(sd)){
      if (sd == 0){
        return(HTML("<p class = 'danger'>Warning: Value must be non-zero!</p>"))
      }
      else return(HTML("<span></span>"))
    }
    else{
      return(HTML("<p class = 'danger'>Warning: Value cannot be left empty!</p>"))
    }
  })
  
  # restrict cts data input for mean values
  output$restrictC <- renderText({
    #HTML("<p class = 'desc'>Anticipated value of <b>experimental</b> group for <i>statistic of interest (test statistic)</i><sup>2</sup>:</p>"),
    m <- input$mE
    m1 <- input$mC
    if (!is.null(m) && !is.na(m) && is.numeric(m) && !is.null(m1) && !is.na(m1) && is.numeric(m1)){
        if (!is.null(input$type4)){
          nonInf = input$type4
          if (nonInf == "sup"){
            if (m == m1){return(HTML("<p class = 'danger'>Warning: Values cannot be equal!</p>"))}
            else return(HTML("<span></span>"))
          }
          else return(HTML("<span></span>"))
        }
    }
    else{
      return(HTML("<p class = 'danger'>Warning: Values cannot be left empty!</p>"))
    }
  })
  
  # restrict cts data input for sample sizes 
  output$restrictCS <- renderText({
    eVal <- input$eVal
    cVal <- input$cVal
    if (!is.null(eVal) && !is.na(eVal) && is.numeric(eVal) && !is.null(cVal) && !is.na(cVal) && is.numeric(cVal)){
      if (cVal > 1 & eVal > 1){
        return(HTML("<span></span>"))
      }
      else{
        return(HTML("<p class = 'danger'>Warning: Values must be greater than 1!</p>"))
      }
    }
    else {
      return(HTML("<p class = 'danger'>Warning: Values cannot be left empty!</p>"))
    }
  })
  
  # compute and return ratio of exp:ctrl populations in cts data
  output$currentratio <- reactive({
    eVal <- input$eVal
    cVal <- input$cVal
    if (!is.null(eVal) && !is.na(eVal) && is.numeric(eVal) && !is.null(cVal) && !is.na(cVal) && is.numeric(cVal)){
      if (cVal > 0 & eVal > 0){
        ecRatio <- round(eVal/cVal, 1)
        return(HTML(paste0("<p class = 'desc'>Ratio of <b>experimental to control</b> populations (the current value is approximately ",ecRatio,"):</p>")))
      }
      else{
        return(HTML("<p class = 'danger'>Warning: Values must be greater than 0.</p>"))
      }
    }
    else {
      return(HTML("<p class = 'danger'>Warning: Neither value can be left empty!</p>"))
    }
  })
  
  # compute and return hazard ratio from % values in experimental and ctrl populations for tte
  output$currentHR <- renderText({
    pe <- 1-input$pE/100
    pc <- 1-input$pC/100
    if (!is.null(pe) && !is.na(pe) && is.numeric(pe) && !is.null(pc) && !is.na(pc) && is.numeric(pc)){
      if (pe > 0 & pe < 1 & pc > 0 & pc < 1){
        if (pe == pc){
          return(HTML("<span class = 'danger'>(values cannot be equal)</span>"))
        }
        else {
          HR <- log(pe)/log(pc)
          return(paste0("(the current hazard ratio is approximately ",round(HR,1),")"))
        }
      }
      else {
        return(HTML("<span class = 'danger'>(warning: values must be greater than 0 and less than 100)</span>"))
      }
    }
    else {
      return(HTML("<span class = 'danger'>(warning: values cannot be left empty!)</span>"))
    }
  })

}

