# Define UI
my_ui <- page(
    #title = "powercalc.ca",
    #use_shiny_title(),
     # initialize shinyjs
     useShinyjs(),
     tags$head(
          tags$link(rel = "icon", type = "image/png", href = "favicon-32x32.png"),
          tags$link(rel = "stylesheet", type = "text/css", href = "style2.css"),
          tags$meta(name="description", content="This web app can quickly compute sample sizes and power values for data that is often encountered in epidemiological research scenarios without using R."),
          tags$meta(name="keywords", content="power calculator, power, sample size, statistics, research, proposal, grant, application, non-inferiority, epidemiology, sample size calculator, calculator, significance level"),
          tags$title("powercalc.ca")
     ),
     div(id = "page",
      # SETUP DATA/CALCTYPE
      div(class = "title text-center", h3("Power and Sample Size Calculator")),
      card(id = "selector",
           p(class = "desc", "Calculate:"),
           radioGroupButtons("type1", NULL, choices = calcs, status = "black", individual = TRUE),
           p(class = "desc", "For a primary outcome that is:"),
           radioGroupButtons("type2", NULL, choiceValues = datas, choiceNames = c(binarybig, ctsbig, ttebig), status = "black", individual = TRUE),
           # div(id = 'Bsub',
           #    radioGroupButtons("type3", "For study type:", choiceValues = binarys, choiceNames = bexps, status = "black", individual = TRUE),
           # ),
              div(class = "desc", 
                 div(class = "row",
                   div(class = "col-6",
                       p("For trial type:")
                    ),
                   div(class = "text-right col-6",
                       circleButton(
                         inputId = "NIhelp",
                         icon = icon("question"),
                         href = "https://www.youtube.com/watch?si=F5V5vOejmy-i5NV3&v=ZexRMe2xbJw&feature=youtu.be",
                         status = "primary"
                       )
                   )
                 )
              ),
              div(id = "BCsub",
                radioGroupButtons("type4", NULL, choiceValues = supinf, choiceNames = c(supbig, infbig), status = "black", individual = TRUE)
              ),
             shinyjs::hidden(div(id = "TTEsub",
                radioGroupButtons("type4T", NULL, choiceValues = supinf[1], choiceNames = c(supbig), status = "black", individual = TRUE)
             )),
           p(class = "footnote", "Note: This tool is for individual-level Randomized Control Trials (RCTs), not clustered RCTs.")
      ),
      # DATA ENTRY
      card(id = "data", 
        div(class = "preamble",
          div(id = "B-op", uiOutput("selectB")),
          div(id = "C-op", uiOutput("selectC")), 
          div(id = "T-op", uiOutput("selectT")),
          div(id = "marginA",            
              HTML("<p class = 'desc'><b>Non-inferiority margin</b> using absolute risk difference:</p>"),
              sliderInput("delta", NULL, min = 1, max = 20, step = 0.25, value = 10, post = '%')
          ),
          shinyjs::hidden(div(class = "forninf",            
              HTML("<p class = 'desc'>One-sided <b>significance level</b> or &alpha;-value (alpha):</p>"),
              sliderInput("alpha", NULL, min = 0.01, max = 0.1, value = 0.05),
              p(class = "small text-center", "Common/Recommended Value is 0.05")
          )),
          div(class = "forsup",            
              HTML("<p class = 'desc'>Two-sided <b>significance level</b> or &alpha;-value (alpha):</p>"),
              sliderInput("alpha", NULL, min = 0.01, max = 0.1, value = 0.05),
              p(class = "small text-center", "Common/Recommended Value is 0.05")
          ),
          div(class = "forss",
              HTML("<p class = 'desc'><b>Statistical power</b> or 1-&beta; value</b> (1-beta):</p>"),
              sliderInput("beta", NULL, min = 0.5, max = 0.99, value = 0.8),
              p(class = "small text-center", "Common/Recommended Value is 0.8") 
          ),
          p(class = "footnote", "1. Based on available data and literature from other cohort studies or Randomized Control Trials (RCTs).")
        )
      ),
      # RESULTS
      navset_card_tab(id = "results", nav_panel("Result", div(class = "preamble", uiOutput("computed")))
      ),
      # FOOTER OF PAGE
      div(class = "centered",
          h4(class = "half-bold text-center", "What is powercalc.ca?"),
          div(class = "answer text-center",
              # div(class = "img",
              #     img(src='apple-touch-icon.png', height = "50px", align = "left")
              # ),
              div(class = "imgtext2",
                  p("This web app can quickly compute sample 
                  sizes and power values for data that is often 
                  encountered in epidemiological research.")
              )
          )
      ),
      div(class = "centered",
          h4(class = "half-bold text-center", "Not sure where to submit your manuscript?"),
          div(class = "answer text-left",
              a(class = "img", href = "https://jrnowl.com", 
                  img(src='thumbnail_jrnowl.png', height = "50px", align = "left")
              ),
              div(class = "imgtext",
                  HTML("<a href = 'https://jrnowl.com'>Check out <u>www.jrnowl.com</u> which has a repository of
                  the submission guidelines for over 
                  260 journals,
                  including details on acceptance rate and time to first decision!</a>")
              )
          )
      ),
      a(class = "footer-noline", href = "https://fralicklab.com","Fralick Lab 2024"),
      a(class = "footerlink", href = "https://forms.gle/hYH9qJZm8srXuab68", "Provide Feedback"),
      HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=G-EK8VZNR4QT'></script>
             <script>
             window.dataLayer = window.dataLayer || [];
           function gtag(){dataLayer.push(arguments);}
           gtag('js', new Date());
           
           gtag('config', 'G-EK8VZNR4QT');
           </script>")
  )
)
