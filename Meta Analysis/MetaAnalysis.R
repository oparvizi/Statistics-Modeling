if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(meta)) install.packages("meta", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")

# Define UI ----
ui <- fluidPage(
  HTML(paste0(
    "<br>",
    "<img style = 'display: block; margin-left: auto;margin-right: auto;' src='https://www.svgrepo.com/show/245581/learning.svg'; width = '50'; height='50'; background-color: white;>",
    "<script>",
    "var today = new Date();",
    "var yyyy = today.getFullYear();",
    "</script>",
    "<p style = 'text-align: center;'><small>&copy; - <a href='https://github.com/oparvizi/' target='_blank'>Statistics & Modeling</a> - <script>document.write(yyyy);</script></small></p>"
  )),
  navbarPage("Meta Analysis in R",
             tabPanel("Data description", br(),br(),
                      box(title = "Dataset: ", width = 12, height = "auto", reactableOutput("table")),br(),br(),
                      box(title = "Meta Analysis: ", width = 12, height = "auto", verbatimTextOutput("summary1")),
             ),
             tabPanel("Methods analysis", br(),br(),
                      box(title = "Forest Plots:", width = 8, height = "auto", plotOutput("plot1")),
                      box(title = "Funnel Plots:", width = 8, height = "auto", plotOutput("plot2")),
                      box(title = "Linear regression test of funnel plot asymmetry:", width = 12, height = "auto", verbatimTextOutput("summary2")),
                      box(title = "", width = 8, height = "auto", plotOutput("plot3")),
             ),
             tabPanel("About...", br(),br(),
                      verbatimTextOutput("us")
             ),
  )
)

# Define server logic ----
server <- function(input, output) {
  
  
  library(meta)
  dat <- data("Fleiss1993cont")
  res.flesiss =  metacont(n.psyc, mean.psyc, sd.psyc, 
                          n.cont, mean.cont, sd.cont,
                          comb.fixed = T, comb.random = T, studlab = study,
                          data = Fleiss1993cont, sm = "SMD") 
  res.flesiss
  
  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # Meta Analysis in R----------------------------------------------------------
  ##----------------------------------------------------------------------------
  # LIBRARY meta----------------------------------------------------------------
  # source: https://cran.r-project.org/web/packages/meta/index.html
  #          https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/
  #         https://www.r-bloggers.com/2021/08/meta-analysis-in-r/

  # Descriptive statistics and plots
  library(reactable)
  output$table = renderReactable({
    data("Fleiss1993cont")
    reactable(
      Fleiss1993cont[1:length(Fleiss1993cont$study), ],
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 5,
      paginationType = "jump",
      language = reactableLang(
        searchPlaceholder = "Search...",
        noData = "No entries found",
        pageInfo = "{rowStart} to {rowEnd} of {rows} entries",
        pagePrevious = "\u276e",
        pageNext = "\u276f",
        
        # Accessible labels for assistive technologies such as screen readers.
        # These are already set by default, but don't forget to update them when
        # changing visible text.
        pagePreviousLabel = "Previous page",
        pageNextLabel = "Next page"
      )
    )
  })
  
  # Descriptive statistics and plots
  output$summary1 <- renderPrint({#---------------------------------------------
    # Description of the data
    # meta-analysis with continuout outcome
    # comb.fixed/comb.random: indicator whether a fix/random effect mata-analysis to be conducted.
    # sm: Three different types of summary measures to choose,standardized mean difference (SMD),mean difference (MD), ratio of means (ROM)
    res.flesiss =  metacont(n.psyc, mean.psyc, sd.psyc, 
                            n.cont, mean.cont, sd.cont,
                            comb.fixed = T, comb.random = T, studlab = study,
                            data = Fleiss1993cont, sm = "SMD") 
    res.flesiss
   })    
  
  # Forest----------------------------------------------------------------------
  output$plot1 <- renderPlot({
    forest(res.flesiss, leftcols = c('studlab'))
  })
  
  # Funnel Plot----------------------------------------------------------------- 
  output$plot2 <- renderPlot({
    funnel(res.flesiss)
  })
  #----------------------------------------------------------------------------- 
  output$summary2 <- renderPrint({  
    res.flesiss =  metacont(n.psyc, mean.psyc, sd.psyc, 
                            n.cont, mean.cont, sd.cont,
                            comb.fixed = T, comb.random = T, studlab = study,
                            data = Fleiss1993cont, sm = "SMD") 
    res.flesiss
    metabias(res.flesiss, method.bias = 'linreg', k.min = 5, plotit = T)
  })
  #----------------------------------------------------------------------------- 
  output$plot3 <- renderPlot({
    metabias(res.flesiss, method.bias = 'linreg', k.min = 5, plotit = T)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)