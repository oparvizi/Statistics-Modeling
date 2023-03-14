if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")

# Define UI ----
ui <- fluidPage(
  titlePanel("NEGATIVE BINOMIAL REGRESSION"),
  sidebarLayout(
    sidebarPanel(
      #code('install.packages("shiny")'),
      h4("Problem Description"),
      p("School administrators study the attendance behavior of high school juniors at two schools. Predictors of the number of days of absence include the type of program in which the student is enrolled and a standardized test in math."),
      HTML(paste0(
        "<br>",
        "<img style = 'display: block; margin-left: auto;margin-right: auto;' src='https://www.svgrepo.com/show/245581/learning.svg'; width = '50'; height='50'; background-color: white;>",
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://github.com/oparvizi/' target='_blank'>Statistics & Modeling</a> - <script>document.write(yyyy);</script></small></p>"
      )),
    ),
    mainPanel(
      fluidRow( 
        box(title = "Dataset: ", width = 12, height = "auto", reactableOutput("table")),
        box(title = "Data Description: ", width = 12, height = "auto", verbatimTextOutput("summary1")),
        box(title = "Data Plots:", width = 12, height = "auto", plotOutput("plot1")),
        box(title = "Indication of the existence of Over-dispersion:", width = 12, height = "auto", verbatimTextOutput("summary2")),
        box(title = "Estimate a negative binomial regression:", width = 12, height = "auto", verbatimTextOutput("summary3")),
        box(title = "Likelihood ratio tests of Negative Binomial Models:", width = 12, height = "auto", verbatimTextOutput("summary4")),
        box(title = "Checking model assumption:", width = 12, height = "auto", verbatimTextOutput("summary5")),
        box(title = "Confidence intervals for the coefficients by profiling the likelihood function:", width = 12, height = "auto", verbatimTextOutput("summary6")), 
        box(title = "Incident rate ratios rather than coefficients:", width = 12, height = "auto", verbatimTextOutput("summary7")),
        box(title = "Predicted values:", width = 12, height = "auto", verbatimTextOutput("summary8")),
        box(title = "Obtain the mean predicted number of events for values:", width = 12, height = "auto", plotOutput("plot2")),
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # NEGATIVE BINOMIAL REGRESSION------------------------------------------------
  ##----------------------------------------------------------------------------
  # LIBRARY MASS----------------------------------------------------------------
  # source: https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
  
  library(foreign)
  library(ggplot2)
  library(MASS)
  dat <- read.dta("nb_data.dta")
  dat <- within(dat, {
    prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
    id <- factor(id)
  })
  # Descriptive statistics and plots
  library(reactable)
    output$table = renderReactable({
    dat <- read.dta("nb_data.dta")
    saveRDS(dat, file = "dat.Rds")
    reactable(
      dat[1:length(dat$id), ],
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 5,
      paginationType = "simple",
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
  output$summary1 <- renderPrint({
    # Description of the data--------------
    dat <- read.dta("nb_data.dta")
    
    # nb_data: data on 314 high school juniors from two urban high schools. 
    # daysabs: The response variable of interest is days absent.
    # math:    gives the standardized math score for each student. The variable prog is a three-level nominal variable indicating the type of instructional program in which the student is enrolled.
    
    dat <- within(dat, {
      prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
      id <- factor(id)
    })
      summary(dat)
  })    
  
  #---------------------------------------- 
  output$plot1 <- renderPlot({   
    ggplot(dat, aes(daysabs, fill = prog)) + geom_histogram(binwidth = 1) + 
      facet_grid(prog ~ ., margins = TRUE, scales = "free") + 
      scale_linewidth("Windspeed (mph)", range = c(0.5, 3))
  })
  output$summary2 <- renderPrint({
    with(dat, tapply(daysabs, prog, function(x) {
      sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
    }))
  })
  
  #---------------------------------------- 
  # Analysis methods you might consider
  # *Negative binomial regression
  # *Poisson regression
  # *Zero-inflated regression model
  # *OLS regression
  
  # Negative binomial regression analysis-- 
  
  output$summary3 <- renderPrint({
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat))
  })
  
  #---------------------------------------- 
  # Likelihood ratio tests of Negative Binomial Models
  
  output$summary4 <- renderPrint({
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    m2 <- update(m1, . ~ . - prog)
    anova(m1, m2)
  })
  
  #----------------------------------------- 
  # Checking model assumption---------------
  output$summary5 <- renderPrint({
    # Checking model assumption
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat)
    pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)
  }) 
  
  #---------------------------------------- 
  # Get the confidence intervals for the coefficients by profiling the likelihood function.
  output$summary6 <- renderPrint({ 
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    (est <- cbind(Estimate = coef(m1), confint(m1)))
  })
  
  #---------------------------------------- 
  # looking at incident rate ratios rather than coefficients. To do this, we can exponentiate our model coefficients. 
  # The same applies to the confidence intervals.
  output$summary7 <- renderPrint({
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    est <- cbind(Estimate = coef(m1), confint(m1))
    exp(est)
  })
  
  #----------------------------------------
  # Predicted values-----------------------
  output$summary8 <- renderPrint({
    
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    newdata1 <- data.frame(math = mean(dat$math), prog = factor(1:3, levels = 1:3, 
                                                                labels = levels(dat$prog)))
    newdata1$phat <- predict(m1, newdata1, type = "response")
    newdata1
  })
  
  #---------------------------------------- 
  # obtain the mean predicted number of events for values
  output$plot2 <- renderPlot({
    
    m1 <- glm.nb(daysabs ~ math + prog, data = dat)
    newdata2 <- data.frame(
      math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),
      prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
                      levels(dat$prog)))
    
    newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
    newdata2 <- within(newdata2, {
      DaysAbsent <- exp(fit)
      LL <- exp(fit - 1.96 * se.fit)
      UL <- exp(fit + 1.96 * se.fit)
    }
    )
    
    ggplot(newdata2, aes(math, DaysAbsent)) +
      geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
      geom_line(aes(colour = prog), size = 2) +
      labs(x = "Math Score", y = "Predicted Days Absent")+ 
      scale_linewidth("Windspeed (mph)", range = c(0.5, 3))
    
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
