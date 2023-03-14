#setwd("D:/CODES/R")

if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(lme4)) install.packages("lme4", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(emmeans)) install.packages("emmeans", repos = "http://cran.us.r-project.org")
if(!require(DBI)) install.packages("DBI", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(writexl)) install.packages("writexl", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(RODBC)) install.packages("RODBC", repos = "http://cran.us.r-project.org")
# Install RSQLite library (2 options)
if(!require(RSQLite)) install.packages("RSQLite", repos = "http://cran.us.r-project.org") # option 1
# install.packages("devtools")                                                            # option 2
#devtools::install_github("rstats-db/RSQLite")

#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
library(shinyWidgets)
# Define UI ----
ui <-   fluidPage( 
            HTML(paste0(
              "<br>",
              "<img style = 'display: block; margin-left: auto;margin-right: auto;' src='https://www.svgrepo.com/show/245581/learning.svg'; width = '50'; height='50'; background-color: white;>",
              "<script>",
              "var today = new Date();",
              "var yyyy = today.getFullYear();",
              "</script>",
              "<p style = 'text-align: center;'><small>&copy; - <a href='https://github.com/oparvizi/' target='_blank'>Statistics & Modeling</a> - <script>document.write(yyyy);</script></small></p>"
            )),
            navbarPage("Statistics & Modeling",
                 tabPanel("Model Generation", br(),br(),
                          verbatimTextOutput("generate"),br(),
                          verbatimTextOutput("summary0_0"),br(),
                          verbatimTextOutput("summary0_1"),br(),
                          reactableOutput("table")
                 ),
                 navbarMenu("Model Examples",
                            tabPanel("Attendance behavior", br(),br(),
                                     tabsetPanel(
                                       tabPanel("Problem Steatment", 
                                                verbatimTextOutput("problem1"),br(),
                                                verbatimTextOutput("ref1")
                                                ),
                                       tabPanel("Data Description", 
                                                verbatimTextOutput("summary1_0"),br(),
                                                reactableOutput("table_1"),br(),
                                                verbatimTextOutput("summary1_1"),br()
                                                ),
                                       tabPanel("Existence of Over-dispersion", 
                                                plotOutput("plot1_1"),br(),
                                                verbatimTextOutput("summary1_2")
                                                ),
                                       tabPanel("Analysis methods", 
                                                verbatimTextOutput("summary1_3"),br(),
                                                verbatimTextOutput("state1_1"), br(),
                                                verbatimTextOutput("summary1_4"),br(),
                                                verbatimTextOutput("state1_2")
                                                ),
                                       tabPanel("Checking model assumption",
                                                verbatimTextOutput("summary1_5"),br(),
                                                verbatimTextOutput("state1_3"),br(),
                                                verbatimTextOutput("summary1_6"),br(),
                                                verbatimTextOutput("state1_4"),br(),
                                                verbatimTextOutput("summary1_7"),br(),
                                                verbatimTextOutput("summary1_8"),br(),
                                                verbatimTextOutput("state1_5")
                                                ),
                                       tabPanel("Values prediction and Visualisation",
                                                verbatimTextOutput("state1_6"),br(),
                                                verbatimTextOutput("summary1_9"),br(),
                                                plotOutput("plot1_2")
                                                ), 
                            )),
                            tabPanel("Local adaptation",br(),br(),
                                     tabsetPanel(
                                       tabPanel("Problem Steatment",
                                                verbatimTextOutput("problem2"),br(),
                                                verbatimTextOutput("ref2")
                                                ),
                                       tabPanel("Data Description",        
                                                verbatimTextOutput("summary2_0"),br(),
                                                reactableOutput("table_2"),br(),
                                                verbatimTextOutput("summary2_1")
                                                ),
                                       tabPanel("Existence of Over-dispersion", 
                                                plotOutput("plot2_1"),br(),
                                                plotOutput("plot2_2")
                                                ),
                                       tabPanel("Analysis methods",         
                                                verbatimTextOutput("summary2_2"),br(),
                                                verbatimTextOutput("summary2_3"),br(),
                                                plotOutput("plot2_3"),br(),
                                                verbatimTextOutput("summary2_4"),br(),
                                                verbatimTextOutput("summary2_5"),br(),
                                                plotOutput("plot2_4"),br(),
                                                verbatimTextOutput("summary2_6"),br(),
                                                fluidRow(
                                                  column(6,
                                                       plotOutput("plot2_5")
                                                        ))
                                                ),
                            )),
                            tabPanel("Amet Salmonella",br(),br(),
                                     tabsetPanel(
                                       tabPanel("Problem Steatment",
                                                verbatimTextOutput("problem3"),br(),
                                                verbatimTextOutput("ref3")
                                                ),
                                       tabPanel("Data Description",          
                                                verbatimTextOutput("summary3_0"),br(),
                                                reactableOutput("table_3"),
                                                verbatimTextOutput("summary3_1")
                                                ),
                                       tabPanel("Analysis methods",
                                                verbatimTextOutput("state3_1"), br(),
                                                fluidRow(
                                                  column(5, #offset = 1,
                                                         verbatimTextOutput("summary3_2"),br(),
                                                         verbatimTextOutput("summary3_3"),br(),
                                                         verbatimTextOutput("summary3_4"),br(),
                                                         verbatimTextOutput("summary3_5"),br(),
                                                         verbatimTextOutput("summary3_6"),br(),
                                                         plotOutput("plot3_1")
                                                         ))
                                                ),
                            ))
                           ),
                           tabPanel("About...", br(),br(),
                                    verbatimTextOutput("us")
                                    ),
                          )  
)

# Define server logic ----
server <- function(input, output) {
  
  ##----------------------------------------------------------------------------
  # Generate Model databases----------------------------------------------------
  ##----------------------------------------------------------------------------
  # Load libraries
  library(DBI)     # for connecting to databases
  library(RSQLite)
  library(readxl)  # for reading Excel files
  library(writexl) # for creating new Excel files
  library(haven)   # for reading files from SPSS, SAS, and Stata
  library(dplyr)   # for combining the data sets
  # Establish a connection to our SQLite database
  con <- dbConnect(RSQLite::SQLite(), "wildpot.db")
  # Write our query using SQL syntax
  query <- "SELECT * FROM wildpot ORDER BY RANDOM() LIMIT 10;"
  # Execute the query, fetch the results, and store them in a data frame
  randomized_data <- dbFetch(dbSendQuery(con, query))
  #dbDisconnect(con)
  dat <- randomized_data
  saveRDS(dat, file = "dat.Rds")
  readRDS(dat, file = "dat.Rds")
  
  ##----------------------------------------------------------------------------
  # Model example: databases----------------------------------------------------
  ##----------------------------------------------------------------------------
  library(foreign)
  dat1 <- read.dta("nb_data.dta")
  dat1 <- within(dat1, {
    prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
    id <- factor(id)
  })
  saveRDS(dat1, file = "dat1.Rds")
  readRDS(dat1, file = "dat1.Rds")
  ##----------------------------------------------------------------------------
  dat2 <- read.csv("acer_transplant.csv")
  saveRDS(dat2, file = "dat2.Rds")
  readRDS(dat2, file = "dat2.Rds")
  ##----------------------------------------------------------------------------
  freq <- c(15, 16, 16, 27, 33, 20,
            21, 18, 26, 41, 38, 27,
            29, 21, 33, 60, 41, 42)
  dose <- rep(c(0, 10, 33, 100, 333, 1000), 3)
  plate <- rep(1:3, each = 6)
  (salmonella <- data.frame(freq, dose, plate))
  dat3 <- salmonella
  saveRDS(dat3, file = "dat3.Rds")
  readRDS(dat3, file = "dat3.Rds")
  ##----------------------------------------------------------------------------
  
  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # Model example: Attendance behavior------------------------------------------
  ##----------------------------------------------------------------------------
  # LIBRARY MASS----------------------------------------------------------------
  # source: https://stats.oarc.ucla.edu/r/dae/negative-binomial-regression/
  
  output$problem1 <- renderPrint({#--------------------one----------------------
      cat("Problem Statement: 
            School administrators study the attendance behavior of high school juniors at two schools. 
            Predictors of the number of days of absence include the type of program in which the student 
            is enrolled and a standardized test in math.")
  })
  
  # Descriptive statistics and plots
  library(ggplot2)

  output$summary1_0 <- renderPrint({#-------------------------------------------
      readRDS(dat1, file = "dat1.Rds")
      str(dat1)
  })#---------------------------------------------------------------------------
  library(reactable)
  output$table_1 = renderReactable({
        readRDS(dat1, file = "dat1.Rds")
        reactable(
          dat1[1:length(dat1$id), ],
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
  })#---------------------------------------------------------------------------
  output$summary1_1 <- renderPrint({
    # Description of the data
      readRDS(dat1, file = "dat1.Rds")
      summary(dat1)
  })#--------------------------------------------------------------------------- 
  output$plot1_1 <- renderPlot({   
      readRDS(dat1, file = "dat1.Rds")
      ggplot(dat1, aes(daysabs, colour = prog)) + geom_density(lwd=1) +  
        facet_grid(prog ~ ., margins = TRUE, scales = "free") + 
        ggtitle("Data visualisation & Indication of the existence of Over-dispersion") +
        scale_linewidth("Windspeed (mph)", range = c(0.5, 3))
  })#---------------------------------------------------------------------------
  output$summary1_2 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      with(dat1, tapply(daysabs, prog, function(x) {
        sprintf("Mean: %1.4f, VAR: %1.4f, SD: %1.4f", mean(x), var(x), sd(x))
      }))
  })#--------------------------------------------------------------------------- 
  # Negative binomial regression analysis
  output$summary1_3 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      summary(m1 <- glm.nb(daysabs ~ math + prog, data = dat1))
  })#---------------------------------------------------------------------------
  output$state1_1 <- renderPrint({#-------------------------------------------------
    cat("The upper analysis: m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
    showed that the variable math has a statistically significant coefficient of -0.005993.
    This means that the expected log number of days absent decreases by almost 0.006 when 
    the value for math increases by one unit.
    
    *Notice: To determine whether prog itself is statistically significant overall, compare 
    a model with and without prog as follows: m2 <- update(m1, . ~ . - prog); anova(m1, m2)
    The reason it is important to fit separate models is 
        to keep the overdispersion parameter constant")
  })#---------------------------------------------------------------------------
  # Likelihood ratio tests of Negative Binomial Models
  
  output$summary1_4 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      m2 <- update(m1, . ~ . - prog)
      anova(m1, m2)
  })#---------------------------------------------------------------------------
  output$state1_2 <- renderPrint({
    cat("- The two degree-of-freedom chi-square test indicates that prog is a statistically 
        significant predictor of daysabs.
        - AIC(m1) ## [1] 1741.258
        # AIC(m2) ## [1] 1782.306
        The theta parameter shown is the dispersion parameter (equal to the inverse of the 
        dispersion parameter in SAS, Stata, and SPSS- 1/0.968 = 1.033).")
  })#---------------------------------------------------------------------------
  # Checking model assumption
  output$summary1_5 <- renderPrint({
    readRDS(dat1, file = "dat1.Rds")
    # Checking model assumption
    m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
    m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat1)
    summary(m3) 
  })#--------------------------------------------------------------------------- 
  output$state1_3 <- renderPrint({
    cat("- Above is the output of the Poisson model and follows the likelihood ratio test
    used to compare Poisson and negative binomial:
            pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)
    - The associated chi-squared value estimated from 2*(logLik(m1) – logLik(m3)) is 926.03 
    with one degree of freedom. Which suggests the negative binomial model, estimating the
    dispersion parameter, is more appropriate than the Poisson model.")
  })#---------------------------------------------------------------------------
  output$summary1_6 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      # Checking model assumption
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      m3 <- glm(daysabs ~ math + prog, family = "poisson", data = dat1)
      pchisq(2 * (logLik(m1) - logLik(m3)), df = 1, lower.tail = FALSE)
  })#--------------------------------------------------------------------------- 
  output$state1_4 <- renderPrint({
    cat("In the following, the confidence intervals for the coefficients are represented by 
    profiling the likelihood function and occurrence rate ratios are considered instead of coefficients.")
  })#---------------------------------------------------------------------------
  # Get the confidence intervals for the coefficients by profiling the likelihood function.
  output$summary1_7 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      (est <- cbind(Estimate = coef(m1), confint(m1)))
  })#---------------------------------------------------------------------------
  # looking at incident rate ratios rather than coefficients. To do this, we can exponentiate our model coefficients. 
  # The same applies to the confidence intervals.
  output$summary1_8 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      est <- cbind(Estimate = coef(m1), confint(m1))
      exp(est)
  })#---------------------------------------------------------------------------
  output$state1_5 <- renderPrint({
    cat("The output above indicates that the incident rate for prog = 2 is 0.64 times the 
        incident rate for the reference group (prog = 1). Likewise, the incident rate for 
        prog = 3 is 0.28 times the incident rate for the reference group holding the other 
        variables constant.")
  })#---------------------------------------------------------------------------
  # Predicted values
  output$summary1_9 <- renderPrint({
      readRDS(dat1, file = "dat1.Rds")
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      newdata1 <- data.frame(math = mean(dat1$math), prog = factor(1:3, levels = 1:3, 
                                                                  labels = levels(dat1$prog)))
      newdata1$phat <- predict(m1, newdata1, type = "response")
      newdata1
  })#---------------------------------------------------------------------------
  # obtain the mean predicted number of events for values
  output$state1_6 <- renderPrint({
    cat("Below, new datasets were created with values of math and prog and then the 
        prediction command was used to calculate the predicted number of events. The 
        predicted number of events (e.g., days absent) for a general program is  
        10.237, holding math at its mean.")
  })#---------------------------------------------------------------------------
  output$plot1_2 <- renderPlot({
      readRDS(dat1, file = "dat1.Rds")
      m1 <- glm.nb(daysabs ~ math + prog, data = dat1)
      newdata2 <- data.frame(
        math = rep(seq(from = min(dat1$math), to = max(dat1$math), length.out = 100), 3),
        prog = factor(rep(1:3, each = 100), levels = 1:3, labels =
                        levels(dat1$prog)))
      
      newdata2 <- cbind(newdata2, predict(m1, newdata2, type = "link", se.fit=TRUE))
      newdata2 <- within(newdata2, {
        DaysAbsent <- exp(fit)
        LL <- exp(fit - 1.96 * se.fit)
        UL <- exp(fit + 1.96 * se.fit)
      }
      )
      
      ggplot(newdata2, aes(math, DaysAbsent)) +
        geom_ribbon(aes(ymin = LL, ymax = UL, fill = prog), alpha = .25) +
        geom_line(aes(colour = prog), size = 1) +
        labs(x = "Math Score", y = "Predicted Days Absent")+ 
        scale_linewidth("Windspeed (mph)", range = c(0.5, 3))
  })
  output$ref1 <- renderPrint({#-------------------------------------------------
      cat("Ref: Bruin, J. 2006. newtest: command to compute new test.  
          UCLA: Statistical Consulting Group.  
          https://stats.oarc.ucla.edu/stata/ado/analysis/.")
  })

  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # Model example: Local adaptation---------------------------------------------
  ##----------------------------------------------------------------------------
  # LIBRARY MASS----------------------------------------------------------------
  # source: https://pmarchand1.github.io/ECL8202/notes_cours/06-Modeles_generalises_mixtes2.html#contenu_du_cours
  
  # Descriptive statistics and plots
  output$problem2 <- renderPrint({#--------------------two----------------------
      cat("problem statement: 
          Local adaptation of trees at the range margins impact range shifts in the face of climate change.")
  })
  output$ref2 <- renderPrint({#-------------------------------------------
    cat("Ref: Solarik, K.A.,Messier, C., Ouimet, R., Bergeron, Y., Gravel, D. (2018). 
          Local adaptation of trees at the range margins impact range shifts in the face of climate change. 
          Global Ecology and Biogeography, DOI:10.1111/geb.12829.")
  })
  output$summary2_0 <- renderPrint({#-------------------------------------------
      readRDS(dat2, file = "dat2.Rds")
      str(dat2)
  })
  library(reactable)
  output$table_2 = renderReactable({
    readRDS(dat2, file = "dat2.Rds")
      dat2
      reactable(
        dat2[1:length(dat2$stand), ],
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
  output$summary2_1 <- renderPrint({#-------------------------------------------
    readRDS(dat2, file = "dat2.Rds")
    summary(dat2)
  })
  # This suggested overdispersion
  output$plot2_1 <- renderPlot({#-----------------------------------------------
      readRDS(dat2, file = "dat2.Rds")
      ggplot(dat2, aes(x = first)) +
        geom_histogram(color = "white", bins=30) +
        scale_y_continuous(expand = c(0, 0))
  })    
  output$plot2_2 <- renderPlot({#-----------------------------------------------
      readRDS(dat2, file = "dat2.Rds")      
      ggplot(dat2, aes(x = origin, y = first, color = stand)) +
        geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.4)) +
        scale_color_brewer(palette = "Dark2") 
  })
  output$summary2_2 <- renderPrint({#-------------------------------------------
      readRDS(dat2, file = "dat2.Rds")
      library(lme4)
      #library(dplyr)
      dat2_p <- glmer(first ~ stand * origin + (1 | site), data = dat2, family = poisson, control = glmerControl(optimizer = "bobyqa"))
      summary(dat2_p)
      #print(dat2_p, correlation=TRUE) 
  })
  output$summary2_3 <- renderPrint({#-------------------------------------------
    readRDS(dat2, file = "dat2.Rds")
    library(lme4)
    #library(dplyr)
    dat2_p <- glmer(first ~ stand * origin + (1 | site), data = dat2, family = poisson, control = glmerControl(optimizer = "bobyqa"))
    # find a large and statistically significant dispersion
    chi2 <- sum(residuals(dat2_p, "pearson")^2)
    sprintf("statistically significant dispersion: [chi2/df.residual(dat2_p)]: %1.4f,  [1 - pchisq(chi2, df = df.residual(dat2_p))]: %1.4f", chi2 / df.residual(dat2_p), 1 - pchisq(chi2, df = df.residual(dat2_p)))
    #chi2 / df.residual(dat2_p)
    #1 - pchisq(chi2, df = df.residual(dat2_p))
  })
  output$plot2_3 <- renderPlot({#-----------------------------------------------
   # Estimate the parameters of the Poisson GLMM, including the interaction between the original population and forest type, as well as the random effect of the site.
      readRDS(dat2, file = "dat2.Rds")
      library(lme4)
      #library(dplyr)
      dat2_p <- glmer(first ~ stand * origin + (1 | site), data = dat2, family = poisson, control = glmerControl(optimizer = "bobyqa"))
     # find a large and statistically significant dispersion
     chi2 <- sum(residuals(dat2_p, "pearson")^2)
     chi2 / df.residual(dat2_p)
     # overdispersion is also apparent by simulating from the model adjusted to produce prediction intervals of 95%
     sim_dat2_p <- simulate(dat2_p, nsim = 1000, re.form = NULL)
     dat2_pred <- mutate(dat2, pred = predict(dat2_p, type = "response"),
                         q025 = apply(sim_dat2_p, 1, quantile, probs = 0.025),
                         q975 = apply(sim_dat2_p, 1, quantile, probs = 0.975)) %>%
                         arrange(pred)
     
     ggplot(dat2_pred, aes(x = 1:nrow(dat2_pred), y = pred, ymin = q025, ymax = q975)) +
       geom_ribbon(alpha = 0.3) +
       geom_line() +
       geom_point(aes(y = first))+
       ggtitle("Overdispersion evident by simulation from the fitted poisson regression model")
  })
  
  output$summary2_4 <- renderPrint({#-------------------------------------------
    # corresponding negative binomial model 
    readRDS(dat2, file = "dat2.Rds")
    dat2_nb <- glmer.nb(first ~ stand * origin + (1 | site), dat2,
                        control = glmerControl(optimizer = "bobyqa"))
    summary(dat2_nb)
    #print(dat2_nb, correlation=TRUE) 
  })
  output$summary2_5 <- renderPrint({#-------------------------------------------
    readRDS(dat2, file = "dat2.Rds")
    dat2_nb <- glmer.nb(first ~ stand * origin + (1 | site), dat2,
                        control = glmerControl(optimizer = "bobyqa"))
    # find a large and statistically significant dispersion
    chi2 <- sum(residuals(dat2_nb, "pearson")^2)
    sprintf("statistically significant dispersion: [chi2/df.residual(dat2_nb)]: %1.4f,  [1 - pchisq(chi2, df = df.residual(dat2_nb))]: %1.4f", chi2 / df.residual(dat2_nb), 1 - pchisq(chi2, df = df.residual(dat2_nb)))
  })
  output$plot2_4 <- renderPlot({#-----------------------------------------------
  # corresponding negative binomial model 
      readRDS(dat2, file = "dat2.Rds")
      dat2_nb <- glmer.nb(first ~ stand * origin + (1 | site), dat2,
                          control = glmerControl(optimizer = "bobyqa"))
      #  illustrate the prediction intervals for each observation in the dataset
      sim_dat2_nb <- simulate(dat2_nb, nsim = 1000, re.form = NULL)
      dat2_pred <- mutate(dat2, pred = predict(dat2_nb, type = "response"),
                          q025 = apply(sim_dat2_nb, 1, quantile, probs = 0.025),
                          q975 = apply(sim_dat2_nb, 1, quantile, probs = 0.975)) %>%
        arrange(pred)
      
      ggplot(dat2_pred, aes(x = 1:nrow(dat2_pred), y = pred, ymin = q025, ymax = q975)) +
        geom_ribbon(alpha = 0.3) +
        geom_line() +
        geom_point(aes(y = first)) +
        ggtitle("Overdispersion evident by simulation from the fitted negative binomial regression model")
  }) 
  output$summary2_6 <- renderPrint({#-------------------------------------------
      cat("below, emmeans(acer_nb, ~ origin | stand)indicates to compare the average effects of different 
          origins within each forest type. In this graph, the horizontal axis, emmean, shows the average 
          effect of each treatment on the scale of the linear predictor (thus the logarithm of the average
          number of seedlings). The shaded areas show the 95% confidence interval for each mean, while the 
          red arrows (obtained by specifying comparisons = TRUEin plot) indicate which effects are significantly different.")
  }) 
  output$plot2_5 <- renderPlot({#-----------------------------------------------
      # indicates to compare the average effects of different origins within each type of forest
      library(emmeans)
      readRDS(dat2, file = "dat2.Rds")
      dat2_nb <- glmer.nb(first ~ stand * origin + (1 | site), dat2,
                          control = glmerControl(optimizer = "bobyqa"))
      plot(emmeans(dat2_nb, ~ origin | stand), comparisons = TRUE)
  })
  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # Model example: Ames test----------------------------------------------------
  ##----------------------------------------------------------------------------
  # LIBRARY brglm2----------------------------------------------------------------
  # source: https://cran.r-project.org/web/packages/brglm2/vignettes/negativeBinomial.html
  
  # Descriptive statistics and plots
  output$problem3 <- renderPrint({#--------------------two----------------------
      cat("problem statement: 
      The Ames test is a widely employed method that uses bacteria to test whether a given chemical can cause mutations in the DNA of the test organism. 
      This part estimatied the negative binomial regression model with log link and model formula; ames_f <- freq ~ dose + log(dose + 10).")
  })
  output$summary3_0 <- renderPrint({#-------------------------------------------
      readRDS(dat3, file = "dat3.Rds")
      str(dat3)
  })
  output$summary3_1 <- renderPrint({#-------------------------------------------
    readRDS(dat3, file = "dat3.Rds")
    summary(dat3)
  })
  library(reactable)#-----------------------------------------------------------
  output$table_3 = renderReactable({
      readRDS(dat3, file = "dat3.Rds")
      dat3
      reactable(
        dat3[1:length(dat3$freq), ],
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
  output$state3_1 <- renderPrint({
    cat("Estimation of negative binomial regression model with logarithmic linkage and model formula.
     various estimation methods (up-down): 
            - Maximum likelihood estimation
            - Bias reduction
                   Asymptotic mean-bias correction
                   Mean-bias reducing adjusted score equations
            - Median-bias reducing adjusted score equations
            - Mixed bias reducing adjusted score equations
        ")
  })#---------------------------------------------------------------------------
  output$summary3_2 <- renderPrint({
      # Estimating the negative binomial regression model with log link and model formula
      ames_f <- freq ~ dose + log(dose + 10)
      # Maximum likelihood estimation
      library("brglm2")
      ames_ML <- brnb(ames_f, link = "log", data = salmonella,
                      transformation = "identity",  type = "ML")
      ## Estimated regression and dispersion parameters
      est <- coef(ames_ML, model = "full")
      ## Estimated standard errors for the regression parameters
      sds <- sqrt(c(diag(ames_ML$vcov.mean), ames_ML$vcov.dispersion))
      round(cbind(est, sds), 4)
  })
  ## Bias reduction
  # Asymptotic mean-bias correction
  output$summary3_3 <- renderPrint({#-------------------------------------------
      ames_f <- freq ~ dose + log(dose + 10)
      # Maximum likelihood estimation
      library("brglm2")
      ames_ML <- brnb(ames_f, link = "log", data = salmonella,
                      transformation = "identity",  type = "ML")
 
      # updates the model fit using asymptotic mean-bias correction for estimating the model parameters
      ames_BC <- update(ames_ML, type = "correction")
      ## Estimated regression and dispersion parameters
      est <- coef(ames_BC, model = "full")
      ## Estimated standard errors for the regression parameters
      sds <- sqrt(c(diag(ames_BC$vcov.mean), ames_BC$vcov.dispersion))
      round(cbind(est, sds), 4)
  })
  # Mean-bias reducing adjusted score equations
  output$summary3_4 <- renderPrint({# ------------------------------------------
      ames_f <- freq ~ dose + log(dose + 10)
      # Maximum likelihood estimation
      library("brglm2")
      ames_ML <- brnb(ames_f, link = "log", data = salmonella,
                      transformation = "identity",  type = "ML")

      ames_BRmean <- update(ames_ML, type = "AS_mean")
      ## Estimated regression and dispersion parameters
      est <- coef(ames_BRmean, model = "full")
      ## Estimated standard errors for the regression parameters
      sds <- sqrt(c(diag(ames_BRmean$vcov.mean), ames_BRmean$vcov.dispersion))
      round(cbind(est, sds), 4)
  }) 
  ## Median-bias reducing adjusted score equations
  output$summary3_5 <- renderPrint({# ------------------------------------------
      
  ## Mean-bias reducing adjusted score equations    
      ames_f <- freq ~ dose + log(dose + 10)
      # Maximum likelihood estimation
      library("brglm2")
      ames_ML <- brnb(ames_f, link = "log", data = salmonella,
                      transformation = "identity",  type = "ML")
      ames_BRmedian <- update(ames_ML, type = "AS_median")
      ## Estimated regression and dispersion parameters
      est <- coef(ames_BRmedian, model = "full")
      ## Estimated standard errors for the regression parameters
      sds <- sqrt(c(diag(ames_BRmedian$vcov.mean), ames_BRmedian$vcov.dispersion))
      round(cbind(est, sds), 4)
      
  })
  ## Mixed bias reducing adjusted score equations
  output$summary3_6 <- renderPrint({#-------------------------------------------
      ames_f <- freq ~ dose + log(dose + 10)
      # Maximum likelihood estimation
      library("brglm2")
      ames_ML <- brnb(ames_f, link = "log", data = salmonella,
                      transformation = "identity",  type = "ML")

      ames_BRmixed <- update(ames_ML, type = "AS_mixed")
      ## Estimated regression and dispersion parameters
      est <- coef(ames_BRmixed, model = "full")
      ## Estimated standard errors for the regression parameters
      sds <- sqrt(c(diag(ames_BRmixed$vcov.mean), ames_BRmixed$vcov.dispersion))
      round(cbind(est, sds), 4)
  })
  output$ref3 <- renderPrint({#-------------------------------------------
      cat("Ref: - Agresti, A. 2015. Foundations of Linear and Generalized Linear Models. 
      Wiley Series in Probability and Statistics. Wiley.
      - Kenne Pagui, E. C., A. Salvan, and N. Sartori. 2020. “Accurate Inference in Negative Binomial Regression.” 
      Eprint arXiv:2011.02784. https://arxiv.org/abs/2011.02784.
      - Kosmidis, Ioannis, Euloge Clovis Kenne Pagui, and Nicola Sartori. 2020. “Mean and Median Bias Reduction in Generalized Linear Models.”
      Statistics and Computing 30: 43–59. https://doi.org/10.1007/s11222-019-09860-6.
      - Margolin, Barry H., Byung Soo Kim, and Kenneth J. Risko. 1989. “The Ames Salmonella/Microsome Mutagenicity Assay: Issues of Inference and Validation.” 
      Journal of the American Statistical Association 84 (407): 651–61. https://doi.org/10.1080/01621459.1989.10478817.."       )
  })

 
  
  ##### Model Generation
  ##----------------------------------------------------------------------------
  ##-------------------------------------------------------SQLite---------------
  
  # Descriptive statistics and plots
  output$generate <- renderPrint({#--------------------two----------------------
      cat("Statement: 
          Generating a methods for Analysing.")
  })
  output$summary0_0 <- renderPrint({#-------------------------------------------
      readRDS(dat, file = "dat.Rds")
      str(dat)
  })
  output$summary0_1 <- renderPrint({#-------------------------------------------
      readRDS(dat, file = "dat.Rds")
      summary(dat)
  })
  library(reactable)
  output$table = renderReactable({
      reactable(
        dat[1:length(dat$stand), ],
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
}

# Run the app ----
shinyApp(ui = ui, server = server)
