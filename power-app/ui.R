# Define UI
my_ui <- page(
    #title = "powercalc.ca",
    #use_shiny_title(),
     # initialize shinyjs
     useShinyjs(),
     tags$head(
          tags$link(rel = "icon", type = "image/png", href = "powercalclogo.png"),
          tags$link(rel = "stylesheet", type = "text/css", href = "style2.css"),
          tags$title("powercalc.ca")
     ),
     div(id = "page",
      # SETUP DATA/CALCTYPE
      div(class = "title text-center", h3("Power and Sample Size Calculator")),
      card(id = "selector",
           #card_header(class = "subtitle", "Describe your available data and objective below:"),
           radioGroupButtons("type1", "Calculate:", choices = calcs, status = "black", individual = TRUE),
           radioGroupButtons("type2", "For a primary outcome that is:", choiceValues = datas, choiceNames = c(binarybig, ctsbig, ttebig), status = "black", individual = TRUE),
           div(id = 'Bsub',
              radioGroupButtons("type3", "For study type:", choiceValues = binarys, choiceNames = bexps, status = "black", individual = TRUE),
           ),
           p(class = "footnote", "1. For individual-level Randomized Control Trials (RCTs), not clustered RCTs.")
      ),
      # DATA ENTRY
      card(id = "data", 
        div(class = "preamble",
          div(id = "B-op", uiOutput("selectB")),
          div(id = "C-op", uiOutput("selectC")), 
          div(id = "T-op", uiOutput("selectT")),
          div(id = "sig",            
              HTML("<p class = 'desc'><b>Significance level</b> or &alpha;-value</b> (alpha):</p>"),
              sliderInput("alpha", NULL, min = 0.01, max = 0.1, value = 0.05),
              p(class = "small text-center", "Common/Recommended Value is 0.05")
          ),
          div(class = "forss",
              HTML("<p class = 'desc'><b>Statistical power</b> or &beta;-value</b> (beta):</p>"),
              sliderInput("beta", NULL, min = 0.5, max = 0.99, value = 0.8),
              p(class = "small text-center", "Common/Recommended Value is 0.8") 
          ),
          p(class = "footnote", "2. Based on available data and literature from other cohort studies or Randomized Control Trials (RCTs).")
        )
      ),
      # RESULTS
      navset_card_tab(id = "results", nav_panel("Result", div(class = "preamble", uiOutput("computed")))
      ),
      # FOOTER OF PAGE
      div(class = "bottomtext text-center", h5("This web app can quickly compute sample sizes and power values for data that is often enountered in epidemiological research scenarios without using R.")),
      p(class = "footer", "Fralick Lab 2024"),
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
