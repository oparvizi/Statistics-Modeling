
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")

if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(HSAUR2)) install.packages("HSAUR2", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
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
  navbarPage("",
             navbarMenu("Modeling in R",
                        tabPanel("Functions for probability distributions", br(),br(),
                                 tabsetPanel(
                                   tabPanel("Functions", 
                                            verbatimTextOutput("state1"),br(),
                                            plotOutput("plot1")
                                   ),
                                   tabPanel("Sampling", 
                                            verbatimTextOutput("state2"),br(),
                                            plotOutput("plot2"),br(),
                                            verbatimTextOutput("state3")
                                   )
                                 )),
                        tabPanel("General statistical functions",br(),br(),
                                 tabsetPanel(
                                   tabPanel("General statistical functions",
                                            verbatimTextOutput("state4"),br(),br(),
                                            verbatimTextOutput("state5"),br(),br(),
                                            plotOutput("plot3"),br(),br(),
                                            verbatimTextOutput("state6"),br(),br(),
                                            plotOutput("plot4"),br(),br(),
                                            verbatimTextOutput("state7"),br(),br(),
                                            plotOutput("plot5")
                                   )
                                 )),
                        tabPanel("Continuous univariate distributions",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Continuous univariate distributions",
                                            plotOutput("plot6"),br(),
                                            plotOutput("plot7"),br(),
                                            plotOutput("plot8"),br(),
                                            plotOutput("plot9"),br(),
                                            plotOutput("plot10")
                                   )
                                 )),
                        tabPanel("Non-negative univariate distributions",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Non-negative univariate distributions",
                                            plotOutput("plot11"),br(),
                                            plotOutput("plot12"),br(),
                                            plotOutput("plot13"),br(),
                                            plotOutput("plot14"),br(),
                                            plotOutput("plot15"),br(),
                                            plotOutput("plot16"),br(),
                                            #plotOutput("plot17"),br(),
                                            plotOutput("plot18")
   
                                   )
                                 )),
                        tabPanel("Univariate Distributions on [0; 1]",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Univariate Distributions on [0; 1]",
                                            plotOutput("plot19")
                                   )
                                 )),
                        tabPanel("Discrete (univariate) distributions",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Discrete (univariate) distributions",
                                            plotOutput("plot20"),br(),
                                            #plotOutput("plot21"),br(),
                                            plotOutput("plot22"),br(),
                                            plotOutput("plot23"),br(),
                                            plotOutput("plot24"),br(),
                                            plotOutput("plot25"),br(),
                                            plotOutput("plot26"),br(),
                                            plotOutput("plot27")
                                   )
                                 )),
                        tabPanel("Formula interface for predictive models",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Formula interface for predictive models",
                                            verbatimTextOutput("statment1"),br(),br(),
                                            plotOutput("plotM0"),br(),br(),
                                            verbatimTextOutput("statment1_1")
                                   )
                                 )),
                        tabPanel("Linear and nonlinear models",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Linear and nonlinear models",
                                            verbatimTextOutput("statment2"),br(),br(),
                                            plotOutput("plotM1"),br(),br(),
                                            plotOutput("plotM2"),br(),br(),
                                            plotOutput("plotM3"),br(),br(),
                                            plotOutput("plotM4"),br(),br(),
                                            plotOutput("plotM5"),br(),br(),
                                            verbatimTextOutput("statment3"),br(),br(),
                                            plotOutput("plotM6"),br(),br(),
                                            verbatimTextOutput("statment4"),br(),br(),
                                            plotOutput("plotM7"),br(),br(),
                                            verbatimTextOutput("statment5")
                                   )
                                 )),
                        tabPanel("Numerical optimization",br(),br(),
                                 tabsetPanel(
                                   tabPanel("Numerical optimization",
                                            verbatimTextOutput("statment6"),br(),br(),
                                            plotOutput("plotM8"),br(),br(),
                                            verbatimTextOutput("statment7"),br(),br(),
                                            verbatimTextOutput("statment8"),br(),br(),
                                            plotOutput("plotM9"),br(),br(),
                                            verbatimTextOutput("statment9"),br(),br(),
                                            plotOutput("plotM10")                                   )
                                 ))
             ),
             tabPanel("About...", br(),br(),
                      verbatimTextOutput("us")
             )
  )
)

# Define server logic ----
server <- function(input, output) {

    # Descriptive statistics and plots
  output$state1 <- renderPrint({#---------------------------------------------
    cat("
    Distribution density function                                 dname (x,...)
    Calculates the density values f(xi ) at the locations x[i].
    Distribution parameters are passed as (named) arguments.
    Cumulative distribution function                              pname (q,...)
    Calculates probabilities P(X qi ) at the locations q[i].
    Quantile                                                      qname (p,...)
    Calculates the values qi 2 IR with P(X qi ) = p[i].
    Random values                                                 rname (n,...)
    Dices out many random values under the specified distribution.")
  })    
  
  # ---------------------------------------------------------------------------
  output$plot1 <- renderPlot({
    par(mfrow = c(2,2))
    x <- seq (0,6,by=0.1)
    fx <- dnorm (x, mean=2, sd=1, log=F)
    plot (x, fx, col="red", type="b", main="dname(x, «param1» ..... «paramK», log=FALSE)")
    gx <- dnorm (x, mean=3, sd=2, log=F)
    lines (x, gx, col="green", type="l")

    x <- seq (0,6,by=0.1)
    Fx <- pnorm (q=x, mean=2, sd=1)
    plot (x, Fx, col="red", type="s", main="pname(q, «param1» ..... «paramK», lower.tail=TRUE, log.p=FALSE)")
    Gx <- pnorm (q=x, mean=2, sd=1, low=F)
    lines (x, Gx, col="green", type="S")
    logFx <- pnorm (q=x, mean=2, sd=1, log=T)
    lines (x, 0.8+logFx, col="blue", type="l")

    p <- seq (0.01,0.99,length=8)
    q <- qnorm (p=p, mean=1, sd=2)
    plot (p, q, col="red", type="b", main="qname(p, «param1» ..... «paramK», lower.tail=TRUE, log.p=FALSE)")
    q <- qnorm (p=p, mean=1, sd=1)
    lines (p, q, col="green", type="c")
    q <- qnorm (p=p, mean=1, sd=0.5)
    lines (p, q, col="green", type="o")

  })
  output$state2 <- renderPrint({#---------------------------------------------
    cat("
    With the command set.seed (seed, kind) the random number generator can be set 
    to a defined state seed belong to IN.
    kind belong to  ’Wichmann-Hill’ 
                    ’Marsaglia-Multicarry’
                    ’Super-Duper’
                    ’Mersenne-Twister’
                    ’Knuth-TAOCP-2002’ ")
  })  
  # ----------------------------------------------------------------- 
  output$plot2 <- renderPlot({
    par(mfrow = c(1,2))
    x <- rnorm (100, mean=2, sd=1)
    hist (x, breaks=20,
          col="cyan",
          labels=T, plot=T,
    main="rname(n, «param1» ..... «paramK»)",
    sub="1D Sampling - Dicing of random values" )

    x <- rnorm (100, mean=0, sd=1)
    y <- rnorm (100, mean=0, sd=1)
    plot (x, y, col="red", type="p",
          main = "rname(n, «param1» ..... «paramK»)",
          sub="2D Sampling - dicing of random values"
          )
    x <- rnorm (100, mean=1, sd=0.5)
    y <- rnorm (100, mean=1, sd=0.5)
    lines (x, y, col="green", type="p")
    x <- rnorm (100, mean=-1, sd=0.25)
    y <- rnorm (100, mean=1, sd=0.25)
    lines (x, y, col="blue", type="p")
  })
  #----------------------------------------------------------------------------- 
  output$state3 <- renderPrint({  
    cat("Sampling - Dicing in Finite W-Spaces:
      sample (x, size, replace=FALSE, prob=NULL)
   Dragging without laying back
      sample (x=0:9, size=5)                                  3 7 1 5 8
      sample (x=letters, size=5)                    ’m’ ’y’ ’o’ ’f’ ’b’
  sample (x=49, size=6)                               14 13 30 23 25 45
   Random permutation
      sample (x=0:9)                                3 1 4 5 2 9 6 0 7 8
      sample (x=6)                                          1 6 4 2 5 3
   Draw with layback
      sample (x=c(T,F), size=5, rep=T)      TRUE FALSE FALSE TRUE FALSE
      sample (x=c(’C’,’G’,’A’,’T’), size=8, rep=TRUE)
                                        ’T’ ’T’ ’G’ ’C’ ’G’ ’A’ ’G’ ’T’ 
   Default proportions
      sample (x=2, size=9, rep=T, prob=c(3,1))        1 1 2 1 1 1 1 2 1
      ")

  })
  #----------------------------------------------------------------------------- 
  output$state4 <- renderPrint({  
    cat("Arithmetic and trimmed mean values: mean(x, trim=0, na.rm=FALSE, ...)
    Arithmetic mean of a number quantity
    Mean values of the attributes of a data set
    Mean value of a number matrix
    Trimmed mean values (trim = proportion of deleted lower/upper outliers)    
Variance and standard deviation: var (x, y=NULL, na.rm=FALSE, use)       
     Variance of a data vector
     Standard deviation of a data vector   
     Covariance of two data vectors   
     (Co)variance calculation according to Pearson
Covariance and correlation: cov (x, y=NULL, method = c(’pearson’,’kendall’,’spearman’))
     Covariances for the columns of a matrix
     Correlations for the columns of a matrix
Sorting, rank and order
     Sorting values of a vector
     Calculate rank numbers
     Calculate rank numbers - incl. duplicates
     Create sort index
Quantiles - Ranking statistics
     Value range limits
     Tukey-Synopse: 0%,25%,50%,75%,100%-Quantile
     Quantiles (general)
     Univariate quantile ")
  })
  #----------------------------------------------------------------------------- 
  output$state5 <- renderPrint({  
    cat("the curse of the dimension
    Equidistant lattice of the feature space (hypercubes)")
  })

  output$plot3 <- renderPlot({
    require(ggplot2)
    par(mfrow = c(3,2))
    #for (m in c(2,3,4,6,8,10)) {
      A <- sapply (
        iris[1:4],
        cut, breaks=2,
        labels=LETTERS[1:2])
      S <- apply (A, MARGIN=1,
                  paste, collapse="")
      f <- table (S)
      F <- table (f)
      barplot (F,
               main=paste (2, "Zellen/Achse"),
               xlab="Zellenbesetzung",
               col="lightblue")
      A <- sapply (
        iris[1:4],
        cut, breaks=3,
        labels=LETTERS[1:3])
      S <- apply (A, MARGIN=1,
                  paste, collapse="")
      f <- table (S)
      F <- table (f)
      barplot (F,
               main=paste (3, "Zellen/Achse"),
               xlab="Zellenbesetzung",
               col="lightblue")
      A <- sapply (
        iris[1:4],
        cut, breaks=4,
        labels=LETTERS[1:4])
      S <- apply (A, MARGIN=1,
                  paste, collapse="")
      f <- table (S)
      F <- table (f)
      barplot (F,
               main=paste (4, "Zellen/Achse"),
               xlab="Zellenbesetzung",
               col="lightblue")
      A <- sapply (
        iris[1:4],
        cut, breaks=6,
        labels=LETTERS[1:6])
      S <- apply (A, MARGIN=1,
                  paste, collapse="")
      f <- table (S)
      F <- table (f)
      barplot (F,
               main=paste (6, "Zellen/Achse"),
               xlab="Zellenbesetzung",
               col="lightblue")
      A <- sapply (
        iris[1:4],
        cut, breaks=8,
        labels=LETTERS[1:8])
      S <- apply (A, MARGIN=1,
                  paste, collapse="")
      f <- table (S)
      F <- table (f)
      barplot (F,
               main=paste (8, "Zellen/Achse"),
               xlab="Zellenbesetzung",
               col="lightblue")
      A <- sapply (
        iris[1:4],
        cut, breaks=10,
        labels=LETTERS[1:10])
      S <- apply (A, MARGIN=1,
                  paste, collapse="")
      f <- table (S)
      F <- table (f)
      barplot (F,
               main=paste (10, "Zellen/Achse"),
               xlab="Zellenbesetzung",
               col="lightblue")
    #}
  })
  #----------------------------------------------------------------------------- 
  output$state6 <- renderPrint({  
    cat("Comparison of two distributions
Probability-Probability Plot versus Quantile-Quantile Plot
    Univariate distribution
        - distribution density
        - Cumulative distribution function
        - quantile function
    PP rendering
    QQ representation

    
Example: Distribution similarity vs. correlation
    qqplot (x, y, plot.it=TRUE, xlab=, ylab=, ...)")
  })
  output$plot4 <- renderPlot({
    par(mfrow = c(2,2))
    #
    attach (iris)
    layout (matrix (c(1,2,3,3), 2))
    hist (Sepal.Width,
          col="lightblue")
    hist (Petal.Length,
          col="lightblue")
    qqplot (
      Sepal.Width, Petal.Length,
      col="red", pch=19, cex=1.5)
    points (
      Sepal.Width, Petal.Length,
      col="gray")
    legend ("top",
            legend=c("QQ-Plot","XY-Plot"),
            fill=c("red","gray"))
  })
  #----------------------------------------------------------------------------- 
  output$state7 <- renderPrint({  
    cat("Normal distribution property of a set of points
    qqnorm (y, ylim, main=, xlab=, ylab=, plot.it=TRUE, datax=FALSE, ...)")
  })
  
  output$plot5 <- renderPlot({
    par(mfrow = c(2,2))
    attach (iris)
    layout (matrix (c(1,2,3,4), 2))
    hist (Sepal.Width,
          col="lightblue")
    hist (Petal.Length,
          col="lightblue")
    qqnorm (Sepal.Width,
            main="Normal Q-Q Sepal.Width",
            col="red", pch=19, cex=1.2)
    qqline (Sepal.Width, lty=2, lwd=2)
    qqnorm (Petal.Length,
            main="Normal Q-Q Petal.Length",
            col="red", pch=19, cex=1.2)
    qqline (Petal.Length, lty=2, lwd=2)
  })
  #----------------------------------------------------------------------------- 
  output$plot6 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (dunif (x, min=3, max=5), col="red", type="l", main= "Equal, rectangular or uniform distribution
        dunif (x, min=0, max=1)")
  })
  #-----------------------------------------------------------------------------
  output$plot7 <- renderPlot({
    x <- seq (0,6,by=0.01)
    plot (dnorm (x, mean=2, sd=2, log=F), col="green", type="l",
          main="Normal distribution
dnorm (x, mean=0, sd=1)")
    lines (dnorm (x, mean=2, sd=1, log=F), col="green", type="l")
    lines (dnorm (x, mean=2, sd=2, log=F), col="green", type="l")
    lines (dnorm (x, mean=2, sd=3, log=F), col="green", type="l")
    lines (dnorm (x, mean=3, sd=1, log=F), col="red", type="l")
    lines (dnorm (x, mean=4, sd=2, log=F), col="blue", type="l")
    lines (dnorm (x, mean=5, sd=3, log=F), col="blue", type="l")
    lines (dnorm (x, mean=6, sd=4, log=F), col="blue", type="l")
  })
  #-----------------------------------------------------------------------------
  output$plot8 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (dcauchy (x, location=3, scale=1), col="red", type="l", main="Cauchy distribution
            dcauchy (x, location=0, scale=1))")
    lines (dcauchy (x, location=3, scale=2), col="blue", type="l")
    lines (dcauchy (x, location=3, scale=3), col="blue", type="l")
    lines (dcauchy (x, location=3, scale=4), col="blue", type="s")
  })
  #----------------------------------------------------------------------------- 
  output$plot9 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (dlogis (x, location=3, scale=1), col="red", type="l", main="Logistic distribution
            dlogis (x, location=0, scale=1)")
    lines (dlogis (x, location=3, scale=2), col="blue", type="l")
    lines (dlogis (x, location=3, scale=3), col="blue", type="l")
    lines (dlogis (x, location=3, scale=4), col="blue", type="s")
  })
  #----------------------------------------------------------------------------- 
  output$plot10 <- renderPlot({
    x <- seq (-2,8,by=0.1)
    plot (dt (x, df=0.5, ncp=0), col="red", type="l", main="Student t-distribution
        dexp (x, rate=1)")
    lines (dt (x, df=0.4, ncp=0), col="blue", type="l")
    lines (dt (x, df=0.3, ncp=0), col="blue", type="l")
    lines (dt (x, df=0.5, ncp=1), col="blue", type="l")
    lines (dt (x, df=0.5, ncp=2), col="green", type="l")
    lines (dt (x, df=0.5, ncp=3), col="green", type="l")
    lines (dt (x, df=0.5, ncp=4), col="green", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot11 <- renderPlot({
    x <- seq (0,15,by=0.1)
    plot (dexp (x, rate=1), col="red", type="l", main="Exponential distribution
    dt (x, df, ncp=0)")
    lines (dexp (x, rate=0.8), col="blue", type="l")
    lines (dexp (x, rate=0.6), col="blue", type="l")
    lines (dexp (x, rate=0.4), col="blue", type="l")
    lines (dexp (x, rate=0.2), col="green", type="l")
    lines (dexp (x, rate=0.1), col="green", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot12 <- renderPlot({
    x <- seq (0,30,by=1)
    plot (dlnorm (x, meanlog=3, sdlog=0.3), col="red", type="l", main="lognormal distribution
        dlnorm (x, meanlog=0, sdlog=1)")
    #lines (x, dlnorm (x, meanlog=0, sdlog=0.2), col="green", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot13 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (dweibull (x, shape=2, scale=1), col="red", type="l", main="Weibull distribution
        dweibull (x, shape, scale=1)")
  })
  #----------------------------------------------------------------------------- 
  output$plot14 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (dchisq (x, df=3), col="red", type="l", main="Central Chi^2 distribution
        dchisq (x, df)")
    #lines (x, dchisq (x, df=5), col="blue", type="l")
    #lines (x, dchisq (x, df=7), col="blue", type="l")
    #lines (x, dchisq (x, df=9), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot15 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (dchisq (x, df=3, ncp=2), col="red", type="l", main="Non-Central Chi^2 distribution
        dchisq (x, df, ncp=0)")
    #lines (dchisq (x, df=3, ncp=2), col="blue", type="l")
    #lines (dchisq (x, df=3, ncp=3), col="blue", type="l")
    #lines (dchisq (x, df=3, ncp=4), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot16 <- renderPlot({
    x <- seq (0,6,by=0.1)
    plot (x, df (x, df1=3, df2=5), col="red", type="l", main="F distribution
        df (x, df1, df2)")
  })
  #----------------------------------------------------------------------------- 
  output$plot17 <- renderPlot({
      x <- seq (0,6,by=0.1)
      shape = 2; scale = 1;
      plot (dgamma (x, shape, rate), col="red", type="l", main="Gamma distribution
      dgamma (x, shape, rate=1, scale=1/rate)")
      shape = 3; scale = 2;
      lines (dgamma (x, shape, scale=1/rate), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  #output$plot18 <- renderPlot({
  #  x <- seq (0,10,by=0.1)
  #  plot (ptukey (q, nmeans=6, df=1, nranges=1, lower.tail=TRUE, log.p=FALSE), col="red", type="l",
  #  main=" Tukey's Studentized Range Distribution
  #  ptukey (q, nmeans, df, nranges=1, lower.tail=TRUE, log.p=FALSE)")
  #  lines (ptukey (q, nmeans=6, df=2, nranges=1, lower.tail=TRUE, log.p=FALSE), col="blue", type="l")
  #  lines (ptukey (q, nmeans=6, df=8, nranges=1, lower.tail=TRUE, log.p=FALSE), col="blue", type="l")
  #  lines (ptukey (q, nmeans=6, df=6, nranges=2, lower.tail=TRUE, log.p=FALSE), col="green", type="l")
  #  lines (ptukey (q, nmeans=6, df=6, nranges=8, lower.tail=TRUE, log.p=FALSE), col="green", type="l")
  #})
  #-----------------------------------------------------------------------------
  output$plot19 <- renderPlot({
    x <- seq (0,1,by=0.01)
    plot (dbeta (x, shape1=3, shape2=2, ncp=0), col="red", type="l", main="Beta distribution
    dbeta (x, shape1, shape2, ncp=0)")
  })
  #----------------------------------------------------------------------------- 
  output$plot20 <- renderPlot({
    x <- c(0:20)
    plot (dbinom (x, size=20, prob=0.8), col="red", type="l", main="Binomial distribution
            dbinom (x, size, prob)")
    lines (dbinom (x, size=12, prob=0.7), col="blue", type="l")
    lines (dbinom (x, size=12, prob=0.5), col="blue", type="l")
    lines (dbinom (x, size=12, prob=0.3), col="blue", type="l")
    text(-2, 0.3, expression(f(x) == paste(frac(1, sqrt(2 * pi * sigma^2)), " ", e^{ frac(-(x - mu)^2, 2 * sigma^2)})))
  })
  #----------------------------------------------------------------------------- 
  #output$plot21 <- renderPlot({
  #  plot (x, dmultinom (10, size=100, probc(1/3,1/3,1/3)), col="red", type="l", main="Multinomial distribution
  #          dmultinom (x, size=sum(x), prob)")
  #})
  #----------------------------------------------------------------------------- 
  output$plot22 <- renderPlot({
    x <- c(0:20)
    m = 30; n = 70; k = 20;
    plot (dhyper (x, m, n, k), col="red", type="l", main="Hypergeometric Distribution
            dhyper (x, m, n, k)")
    #lines (dhyper (x, m, n, k), col="blue", type="l")
    #lines (dhyper (x, m, n, k), col="blue", type="l")
    #lines (dhyper (x, m, n, k), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot23 <- renderPlot({
    x <- c(0:20)
    plot (dgeom (x, prob= 0.2), col="red", type="l", main="Geometric distribution
            dgeom (x, prob)")
    lines (dgeom (x, prob= 0.3), col="blue", type="l")
    lines (dgeom (x, prob= 0.4), col="blue", type="l")
    lines (dgeom (x, prob= 0.5), col="blue", type="l") 
  })
  #----------------------------------------------------------------------------- 
  output$plot24 <- renderPlot({
    x <- c(0:20)
    size = 20; prob = 0.8; 
    plot (dnbinom (x, size, prob), col="red", type="l", main="Negative binomial distribution
            dnbinom (x, size, prob, mu)")
    size = 20; prob = 0.6; 
    lines (dnbinom (x, size, prob), col="blue", type="l")
    #lines (dnbinom (x, size, prob, mu), col="blue", type="l")
    #lines (dnbinom (x, size, prob, mu), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot25 <- renderPlot({
    x <- c(0:12)
    lambda = 5;
    plot (dpois (x, lambda), col="red", type="l", main="Poisson distribution
            dpois (x, lambda)")
    lambda = 7;
    lines (x, dpois (x, lambda), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot26 <- renderPlot({
    x <- c(0:400)
    m = 20; n = 21;
    plot (dwilcox (x, m, n), col="red", type="l", main="Wilcoxon rank sum distribution
            dwilcox (x, m, n)")
    #lines (dwilcox (x, m, n), col="blue", type="l")
    #lines (dwilcox (x, m, n), col="blue", type="l")
    #lines (dwilcox (x, m, n), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$plot27 <- renderPlot({
    x <- c(0:50)
    n = 10;
    plot (x, dsignrank (x, n), col="red", type="l", main="Wilcoxon signed rank sum distribution
            dsignrank (x, n)")
    #lines (x, dsignrank (x, n), col="blue", type="l")
    #lines (x, dsignrank (x, n), col="blue", type="l")
    #lines (x, dsignrank (x, n), col="blue", type="l")
  })
  #----------------------------------------------------------------------------- 
  output$statment1 <- renderPrint({  
    cat("Data, Probabilities and Prediction:
    Rolling out a sample - maximum likelihood estimate
    - Probability Model: Multivariate Distribution Density
    - Record: Matrix (objects x characteristics)
    - Model - > data: Draw (without replacing)    
    - data -> model: Maximum data generation probability

    
Chain rule and prediction model
    - chain rule of probability theory Product of conditional probabilities (a posteriori)
    - multivariate statistische <- Vorhersage: y x1,...,xn
        * regression analysis: Most probable y value
        * Balance calculation (OLS): Minimum Square prediction error

        
Forecasting models: 'Jack of all trades' for pattern recognition, data mining & artificial intelligence
    - Interpolation of function curves: Interpolation point values yi = h(xi ) unknown functional dependencies h( )
    - Modeling of observed function values: Noisy examples yi h(xi ) of unknown functional Dependencies h( )
        + Smoothing of the function history
        + Higher-order correlation and dependencies
        + global data model according to chain rule
        + Estimated estimated values for time series (extrapolation)
    - Classification: Pattern features, Class variable
 
    
Linear forecasting model: Linear combination of source variables & offset in y-direction
    - Univariate OLS model- best fit line y = a + b.x")
  })
  #----------------------------------------------------------------------------- 
  output$plotM0 <- renderPlot({
    h <- function (x, a=17, b=4) a+b*x
    x <- matrix (runif (32), ncol=1)
    y <- h(x) + rnorm (length(x), mean=0, sd=0.5)
    plot (x, y, pch=24, bg="cyan",
          sub="Samples of y = 17 + 4*x")
    abline (reg=lm.fit (x, y), col=3, lty=3)
    abline (reg=lm.fit (cbind(1,x), y), col=2, lty=2)
    abline (reg=lm.fit (cbind(x,1), y), col=4, lty=4)
    #legend ("topleft", lty=2:4, col=2:4, cex=1.5, legend=leg.text)
    title("Univariate OLS model")
  })
  output$statment1_1 <- renderPrint({    
    cat("Linear OLS model: lm.fit (x, y, offset=NULL, method=’qr’, tol=1e-7, singular.ok=TRUE, ...)
    
    - Sample call:
        
        lm.fit (cbind (1, as.matrix (iris[-5])), iris$Species==’setosa’)
            List object lobj with entries (among others):
           lobj$coefficients 0.120 0.066 0.240 -0.220 -0.057
            Model coefficients aj (per x-column)
           lobj$fitted.values 0.979 0.844 0.902 0.826 0.997 1.017 ... 0.008 -0.013
          Forecast values ^yi = aTxi (per x-row)
           lobj$residuals 0.021 0.156 0.098 0.174 0.003 -0.017 ... -0.008 0.013
          Target-actual differences ri = yi - ^yi (per x-row)
              
    - Problem
          Cumbersome data matrix construction         columns, 1=offset, interactions?
          Explicit (uneconomic) expansion             polynomial terms!!
          Non-numerical attribute scales              multiple binary coding of factors
          Unintuitive calling syntax
 
        
Model forms: Compact notation for forecast models   
    - Syntax for model formulas: «formula» ::= «response» ~ «explaining variables» 
          + Target variable (response) are data vectors known in the namespace
          + Set of explanatory variables is a linguistic expression for a combination of terms
          + union operator (the constant '1' is always implicit)
          + Difference operator


Linear (square mean) models          
    - lm (formula, data, subset, weights, ...)
          + Probable model for noisy data
                x <- 1:100
                y <- 4*x + 17 + rnorm(length(x))
                Wanted: the coefficients of the LSE model
          + Calculation of an affine model
                lm.aff < - lm (y~x)
          + Calculation of a linear model
                lm.lin <- lm (y ~ x-1)
          + Calculation using data set variables
                lm.iris <- lm (Petal.Width ~ Sepal.Width, data=iris)
          + All except target variable
                lm.iris <- lm (petal.width ~ ., data=iris[-5])
 
                    
Model-driven prediction
    - Model-driven prediction
          + Prediction for the model's learning data
                guess <- predict (object=lm.aff)
                (polymorphic call with lm object)
          + Prediction for fresh input data
                rate <- prediction (lm.aff, newdata=list(x=1:5))
                (Data set with matching variable names!)
          + Prediction for records
                guess <- predict (lm.iris, newdata=iris[1:50,-5])
 
                    
Interaction terms in model formulas
    - Set algebra versus variable arithmetic   
          + Elementary sets of terms
                Variable names, intercept '1', dot operator '.'
          + Term set algebra
                Plus '+', minus '-', round brackets
          + Interaction terms
                all pairwise products, no doublets, no squares
          + Cumulative Interaction Terms
          + power operator


Variable calculations in model formulas
    - Set algebra versus variable arithmetic   
    - variable type: For numeric variables, interaction ^= product formation.
          + The inhibit operator: prevents a quantity interpretation in formula expressions
          + Creation of squares and cubes
          + Accidental creation of duplicates
          + Unprotected arithmetic
 
         
Model formulas
    - Construction, conversion & other functionalities
          + Formula versus string
          + Constructor: Call specifically with string argument           formula ('y ~ x+z') y ~ x+z
          + String representation
          + Miscellaneous:
              | operator for conditional models ?coplot
              %in% operator for nested models
              specials: offset, strata, cluster ?terms.formula
        ")
  })
  #----------------------------------------------------------------------------- 
  output$statment2 <- renderPrint({  
    cat("Model classes and their default methods
    - Example: Linear OLS model lm() and the iris data 
          + 'Setosa' flower (50) or not (100) - that is the question:
              lm (Species=='setosa' ˜ ., data=iris) -> o is.factor (Species) !
          + prediction (of class membership)
              set <- predict (o); sum ((set>0.5) == (Species=='setosa')) 150
          + Residuum (target-actual difference)
              res <- resid (o); length(res); mean(res)
          + Model Parameters (offsets & term weights)
              coef (o)
          + Akaike information coefficient (prediction error + penalty term)
              AIC(o)-145.8171
          + Confidence intervals of the parameter estimates
              confint (o, level=0.95)
              
              
Model classes and their default methods: ... for advanced ...             
    - Classic analysis of variance (ANOVA)
          anova (o) 
    - Analysis of covariance between parameter estimates
          vcov (o)


Generalized linear prediction model
    - Linear combination joint function error distribution
          + Link function for source-destination coupling
          + Linear model for g(y) instead of y itself
          + Inverse link function for parameter estimation
                Find optimal values a under postulated distribution assumption for        
        

Generalized linear model
    - Typical assumption configurations for link & distribution
          + Identity: Normal-distribution
          + logarithm: Poisson distribution
          + logit: Binomial distribution
          + Reciprocal: Gamma-distribution
 
        
Generalized linear model in R
    - glm (formula, family=gaussian, data, weights, subset, contrasts, ...)
          + Model & learning data: formula, data, weights, subset 
          + IRLS algorithm: start, etastart, mustart, method, control
          + Error Density & Lin
                gaussian, binomial, Gamma,poisson , inverse.gaussian, quasi, quasibinomial, quasipoisson
        
    * Example: Iris species setosa
          o <- glm (Species=='setosa' ~ ., family=binomial, data=iris)
          predict (o, type='link')
          predict (o, type='response')
        ")
  })
  #----------------------------------------------------------------------------- 
  output$plotM1 <- renderPlot({
    attach (cars)
    plot (dist~speed, pch=24, bg="cyan", cex=1.4)
    spd <- seq (min(speed), max(speed), len=100)
    frms <- list (
      linear=dist~speed,
      quadratic=dist~speed + I(speed^2),
      cubic=dist~speed + I(speed^2) + I(speed^3)
    )
    fams <- list ("gaussian", "poisson", "Gamma")
    for (i in seq(along=frms))
      for (j in seq(along=fams)) {
        o <- glm (
          formula=frms[[i]],
          family=fams[[j]])
        guess <- predict (o,
                          newdata=list (speed=spd),
                          type="response")
        lines (spd, guess, lty=0+i, col=1+j, lwd=3)
      }
    title("Estimate braking distance from speed")
    legend ("topleft",
            lty=rep (0+seq(along=frms), times=length(fams)),
            col=rep (1+seq(along=fams), each=length(frms)),
            legend=paste (
              rep (names(frms), times=length(fams)),
              rep (fams, each=length(frms)))
    )
  })
  #----------------------------------------------------------------------------- 
  output$plotM2 <- renderPlot({
    o <- glm (
      dist ~ speed+I(speed^2)+I(speed^3),
      family=Gamma, data=cars)
    cf <- signif (coef(o), 2)
    poly <- paste (cf, "*s^", 0:3, collapse="+")
    body <- paste ("1/(", poly, ")")
    decl <- paste ("fun <- function(s)", body)
    eval (parse (text=decl))
    plot (dist ~ speed, data=cars,
          pch=24, bg="cyan", cex=1.4)
    curve (fun, add=TRUE, lwd=3, col="red")
    legend ("topleft",
            legend=body (fun),
            bg="yellow", lty=1, col="red")
    title("The most successful (AIC) braking distance model")
  })
  #----------------------------------------------------------------------------- 
  output$plotM3 <- renderPlot({
    Treated <- subset (Puromycin, state=="treated")
    plot (rate ~ conc, data=Treated,
          pch=21, cex=3, bg="cyan")
    WMM <- function (rate, conc, Vmax, Km) {
      guess <- (Vmax * conc) / (Km + conc)
      (rate - guess) / sqrt (guess)
    }
    o <- nls (0 ~ WMM (rate, conc, Vmax, Km),
              data=Treated,
              start=list (Vmax=200, Km=0.1))
    cf <- coef (o)
    for (i in seq(along=cf))
      assign (names(cf)[i], cf[i])
    curve ((Vmax*x)/(Km+x), add=TRUE, lwd=5, col="red")
    title("Nonlinear models in R")
  })
  #----------------------------------------------------------------------------- 
  output$plotM4 <- renderPlot({
    par(mfrow=c(2,2))
    #for (sp in seq (.15, 1.1, .3))     # seq (.15, 1.1, .3): [1] 0.15 0.45 0.75 1.05
    #{
      plot (dist~speed,
            data=cars,
            pch=23, bg="cyan")
      lines (cars$speed,
             predict (loess (
               dist~speed,
               data=cars,
               span=0.15)),
             lwd=2, col="red")
      legend ("topleft",
              legend="span=0.15")
      title("Local polynomial regression")

      plot (dist~speed,
            data=cars,
            pch=23, bg="cyan")
      lines (cars$speed,
             predict (loess (
               dist~speed,
               data=cars,
               span=0.45)),
             lwd=2, col="red")
      legend ("topleft",
              legend="span=0.45")
      
      plot (dist~speed,
            data=cars,
            pch=23, bg="cyan")
      lines (cars$speed,
             predict (loess (
               dist~speed,
               data=cars,
               span=0.75)),
             lwd=2, col="red")
      legend ("topleft",
              legend="span=0.75")
      
      plot (dist~speed,
            data=cars,
            pch=23, bg="cyan")
      lines (cars$speed,
             predict (loess (
               dist~speed,
               data=cars,
               span=1.05)),
             lwd=2, col="red")
      legend ("topleft",
              legend="span=1.05")
    #}
    #lowess (x, y=NULL, f=2/3, iter=3, delta=0.01*diff(range(xy$x[o])))
  })
  #----------------------------------------------------------------------------- 
  output$plotM5 <- renderPlot({
    library (HSAUR2); attach (pottery)
    plot (K2O ~ Al2O3, lwd=4,
          col=1+unclass (pottery$kiln))
    abline (lm (K2O ~ Al2O3), lty=4, lwd=5)
    #for (i in seq(along=levels(kiln))) #seq(along=levels(kiln)): [1] 1 2 3 4 5
    abline (lm (K2O ~ Al2O3,
                  subset=unclass(kiln)==1), lty=2, lwd=2, col=1+1)
    title("Factors as source attributes")
    abline (lm (K2O ~ Al2O3,
                  subset=unclass(kiln)==2), lty=2, lwd=2, col=1+2)
    abline (lm (K2O ~ Al2O3,
                subset=unclass(kiln)==3), lty=2, lwd=2, col=1+3)
    abline (lm (K2O ~ Al2O3,
                subset=unclass(kiln)==4), lty=2, lwd=2, col=1+4)
    abline (lm (K2O ~ Al2O3,
                subset=unclass(kiln)==5), lty=2, lwd=2, col=1+5)
  })
  #----------------------------------------------------------------------------- 
  output$statment3 <- renderPrint({  
    cat("Factors and prediction formulas 
    factor attribute => numeric helper attributes IR
    Example Al2O3 prediction (pottery data)  
    y =                   K2O   kiln  K2O+kiln  K2O:kiln  K2O*kiln
            1. Intercept  21.9  16.9    8.68    6.04      4.44
      +     x. K2O        -1.94         2.66              4.02
      +    k2. kiln2            -4.36  -7.07             -0.93
      +    k3. kiln3            -5.22  -9.13              0.23
      +    k4. kiln4             1.26   3.99             -4.27
      +    k5. kiln5             0.40   3.42             19.89
      +  x.k1. K2O:kiln1                        3.50
      +  x.k2. K2O:kiln2                        1.59     -1.83
      +  x.k3. K2O:kiln3                        1.24     -2.48
      +  x.k4. K2O:kiln4                        5.86      4.66
      +  x.k5. K2O:kiln5                        5.66     -7.59
      AIC                201.1  173.8   160.8  162.3     159.4


Which values do the auxiliary variables get?    
    - model.matrix (object, data=environment(object), contrasts.arg=NULL, ...)
        No interactions between kiln and K2O
        model.matrix (Al2O3˜K2O+kiln, pottery)
            (Intercept) K2O   kiln2 kiln3 kiln4 kiln5
           1          1 3.20  0     0     0     0
           2          1 3.05  0     0     0     0...
        With interactions between kiln and K2O
        model.matrix (Al2O3˜K2O:kiln, pottery)
           (Intercept)  K2O:kiln1   K2O:kiln2   K2O:kiln3   K2O:kiln4   K2O:kiln5
           1          1 3.20        0.00        0.00        0.00        0.00
           2          1 3.05        0.00        0.00        0.00        0.00...
        
        
Factors and contrast matrices
    - contrasts (x, contrasts=TRUE, sparse=FALSE) — zum Abfragen und Setzen    
    

Geometrie von l Punkten des IR^l-1
        ")
  })
  #----------------------------------------------------------------------------- 
  output$plotM6 <- renderPlot({
    contr <- paste ("contr", c(
      "treatment",
      "helmert",
      "sum",
      "poly"
    ), sep=".")
    attach (iris)
    layout (matrix (1:4, 2, 2))
    for (i in seq(along=contr)) {
      contrasts (Species) <- contr[i]
      X <- contrasts (Species)
      plot (X, asp=1, main=contr[i])
      polygon (X, col=rainbow(4)[i])
      abline (h=0, v=0, lty=2)
      points (0, 0,
              cex=4, col="yellow", pch=19)
      points (X, pch=19, cex=3)
      text (X, labels=1:3,
            col="white", cex=1.4)
    }
  })
  output$statment4 <- renderPrint({  
    cat("Factors as target attributes
    Use of predictive models for classification purposes
      - Factors cannot be target variables!
      - Tinker contrast matrix
      - Create target matrix
      - Multiple Prediction Mode
    
Example - Linear OLS classification")
  })
  output$plotM7 <- renderPlot({
    attach (iris)
    y <- diag(3)[Species,]
    o <- lm (y~.-Species, data=iris)
    u <- predict (o, newdata=iris)
    par (pch=19, cex.main=2)
    layout (matrix (1:4, 2, 2))
    newplot <- function (S, brx=NULL, ...) {
      plot (c(1,nrow(S)), range(S),
            xlab="", ylab="", type="n", ...)
      abline (v=1/2+brx, lty=2)
    }
    clab <- levels (Species)
    for (k in seq(along=clab)) {
      newplot (u, c(50,100), main=clab[k])
      points (u[,k], col=1+k)
    }
    u.max <- apply (u, MARGIN=1, max)
    k.max <- apply (u, MARGIN=1, which.max)
    newplot (u, c(50,100), main="winner class")
    points (u.max, col=1+k.max)
  })
  output$statment5 <- renderPrint({  
    cat("More prediction models
- Correlated prediction errors
    Generalized least squares                 nlme::gls()
- Effects of source variables on parameters
    Linear mixed-effects model                nlme::lme()
    Non-linear mixed-effects model            nlme::nlme()
- Generalized linear models
    Spliner regression, automatic smoothing   mgcv::gam()
- Prediction with neural networks
    Single hidden layer perceptron            nnet::nnet()
- Prediction of ordered factors
    Proportional-odds logistic regression     MASS::polr()")
  })
  output$statment6 <- renderPrint({  
    cat("What is optimization?
Special form of the inverse task for function evaluation 
    Search space  = domain
    Discrete/combinatorial optimization 
    Scalar (univariate) optimization 
    Multidimensional (multivariate) optimization 
    
    search strategy                       search information
    traversing the space                  function evaluation f(x)
    Ordered search (candidate list)       gradient ascent/descent f'(x)
    Evolutionary Search                   Increment determination f''(x)
    
    global vs. local optimum              deterministic vs. stochastic


Scalar optimization (golden section)
    - optimize (f=, interval=, ..., lower, upper, maximum=FALSE)
         minimum of a parabola
            optimize (f=function(r) (r-3)^2, interval=c(-5,+5))
         Maximum of the sine wave
            optimize (f=sin, interval=c(0,5), maximum=TRUE)
         Maximum of the sine wave
            optimize (f=get(’*’), interval=c(-5,+5), e2=2)
         Occupying excessive function arguments
            g <- function(x) ifelse (
              x>-1, ifelse (
                x<4, exp (-1/abs(x-1)), 1), 1)
            optimize(g, c(-7,20))
            optimize(g, c(-4,20))
            
            
Example - Maximum Likelihood Estimation
      - Optimal parameters of a normally distributed data sample            
             Generate a univariate data sample
                x <- rnorm (100, mean=13, sd=3)
             Define probability function
                like <- function (mu=0, sd=1, data=x)
                      sum (dnorm (data, mu, sd, log=TRUE))
             Maximize in terms of mu
                optimize (like, c(0,100), maximum=TRUE)
             Maximize regarding sd
                optimize (function(s) -like (sd=s), c(0,100))
             Use the estimated Mu value
                optimize (function(s) -like (mu=12.9, sd=s), c(0,100))
    
    
Univariate root search
      - Interval required
          uniroot (f=cos, interval=c(-1,+2))
      - Interval limits only with change of sign!
          uniroot (f=cos, lower=-1, upper=+1)
      - Only one zero is sought
          uniroot (function(x) (x-2)*(x-4), c(0,3))
      - Excessive functional arguments
          uniroot (function(x,z) x*x-z, c(1,2), z=2)
      - Argument names from error message
          uniroot (f=’-’, c(-1,+5), a=3)
            
  
(Complex) polynomial roots (Jenkins & Traub 1972)  
    polyroot (z) mit p(x) = z1 + z2 . x + z3 . x^2 + : : : + zn . x^n-1
      - Quadratic polynomial
      - Multiple zeros")
  })
  #----------------------------------------------------------------------------- 
  output$plotM8 <- renderPlot({
    a <- runif (9)
    poly <- function (x)
      apply (outer(x,a,"-"), 1, prod)
    plot (poly, min(a), max(a),
          col="red", lwd=5)
    abline (v=a, lty=2, col="blue")
    abline (h=0, lty=1, col="blue")
    rug (
      Re (polyroot (choose (a, 0:a))),
      col="blue", lwd=5,
      ticksize=0.1)
      title("(Komplexe) Polynomwurzeln (Jenkins & Traub 1972)")
  }) 
  #----------------------------------------------------------------------------- 
  output$statment7 <- renderPrint({ 
    cat("Multivariate Optimierung – Newtons Methode
    nlm (f, p, ..., hessian=FALSE, gradtol=1e-6, iterlim=100)
    
    -   Generate univariate data sample  
          x <- rnorm (100, mean=13, sd=3)
    -   Define negative likelihood function
          neglike <- function (para, data=x)
            -sum (dnorm (data, para[1], para[2], log=TRUE))
    -   Minimize simultaneously with respect to para = (mu; sigma)
        nlm (f=neglike, p=c(10,2))

Newton descent requires gradient and Hessian matrix 
implicitly: lpf and Hpf are numerically approximated by nlm.
explicit: Calculation functions are passed as attributes gradient 
and hessian of f.


Multivariate minimization with constraints
nlminb (start, objective, gradient, hessian, ..., lower=-Inf, upper=Inf)

    -  Maximum likelihood (like nlm)
          nlminb (start=c(0,1), objective=neglike)
    -  Maximum span sought
          foo <- function (z) -abs (diff (range (z)))
    -  Pathological without barriers:
          nlm (f=foo, p=1:5) 
          nlminb (start=1:5, objective=foo)
    -  Well defined with bounds:
          nlminb (start=1:5, obj=foo, lower=-883, upper=+4711)
    -  Focusing at relative minima
          foo <- function(z) sin(z[1])+cos(z[2]) 
          nlminb (c(35,35), foo, lo=33, up=37)

    
Multivariate general purpose minimizer    
    - optim (par, fn, gr=NULL, ..., method, lower, upper, control=list())  
         Rosenbrock's banana function
            frb <- function(x)
              100 * (x[2]-x[1]ˆ2)ˆ2 + (1-x[1])ˆ2
         ... and their first derivatives
            grb <- function(x) c(
              -400 * x[1] * (x[2]-x[1]ˆ2) - 2 * (1-x[1]),
                200 * (x[2]-x[1]ˆ2))
         No success with Nelder-Mead
            optim (c(-1.2,1), frb)
         BFGS with gradients
            optim (c(-1.2,1), frb, grb, method=’BFGS’) ")
  })
  #-----------------------------------------------------------------------------   
  output$statment8 <- renderPrint({ 
    cat("Scalar optimization — many local minima          
    - Simulated annealing with 10000 evaluations and hand-picked cooling policy")
  })
  #----------------------------------------------------------------------------- 
  output$plotM9 <- renderPlot({
    foo <- function (x) log (
      10*sin(0.3*x)*sin(1.3*x^2) +
        0.00001*x^4 + 0.2*x + 80
    )
    plot (foo, -30, +50, n=1000,
          col="green4", main="Start = 50")
    o <- nlm (f=foo, p=50)
    points (o$estimate, o$minimum,
            col="red", pch=8, cex=3)
    o <- optimize (f=foo, interval=c(-50,+50))
    points (o$minimum, o$objective,
            col="red", pch=9, cex=3)
    o <- optim (par=50, fn=foo, method="Nelder-Mead")
    points (o$par, o$value,
            col="red", pch=10, cex=3)
    o <- optim (par=50, fn=foo, method="SANN",
                control=list(parscale=20))
    points (o$par, o$value,
            col="red", pch=11, cex=3)
  }) 
  output$statment9 <- renderPrint({ 
    cat("Traveling Salesperson Problem (TSP)
- Approximate solution by 'Simulated Annealing'
      remark:
          optim's SANN method expects as Argument gr has no function 
          for the gradient but for the subsequent candidates.
        ")
  }) 
  
  
  #----------------------------------------------------------------------------- 
  #output$plotM10 <- renderPlot({
  #  distance <- function (s, P=xy)
  #    sum (as.matrix (dist (P))[embed(s,2)])
  #  newseq <- function (s, P=xy) {
  #    idx <- 3:nrow(P) - 1
  #    changepoints <- sample (idx, size=2)
  #    s[changepoints] <- rev (s[changepoints])
  #    s}
  #  idx <- 1:nrow(xy)
  #  s <- c (idx, 1)
  #  tsp <- xy[s,]
  #  plot (xy, asp=1,
  #        axes=FALSE, pch=19, col="cyan")
  #  arrows (tsp[idx,1], tsp[idx,2],
  #          tsp[idx+1,1], tsp[idx+1,2],
  #          lty=3, length=0, angle=10, col="red")
  #  set.seed (883)
  #  o <- optim (s, distance, newseq, method="SANN")
  #  tsp <- xy[o$par,]
  #  arrows (tsp[idx,1], tsp[idx,2],
  #          tsp[idx+1,1], tsp[idx+1,2],
  #          lty=1, length=0.2, angle=10, col="blue")
  #  text (xy, rownames (xy), cex=0.8)
  #})
  
  #----------------------------------------------------------------------------- 
  output$plot <- renderPlot({
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)