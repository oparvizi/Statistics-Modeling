#setwd("D:/CODES/R/PublishR")
# source: https://cran.r-project.org/web/packages/Hmsc/vignettes/vignette_3_multivariate_high.pdf
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(Hmsc)) install.packages("Hmsc", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(fields)) install.packages("fields", repos = "http://cran.us.r-project.org")
if(!require(ape)) install.packages("ape", repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS", repos = "http://cran.us.r-project.org")
if(!require(coda)) install.packages("coda", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")

library(shiny)
library(Hmsc)
library(corrplot)
library(MASS)
library(coda)


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
  navbarPage("Generating simulated data for a large community",
             tabPanel("Simulating phylogeny and species traits", br(),br(),
                      verbatimTextOutput("state1_1"),br(),
                      plotOutput("plot1_1"),br(),
                      verbatimTextOutput("code1_1")
                      #includeMarkdown("code1_1.md")
             ),
             tabPanel("HMSC analyses model", br(),br(), 
                      #verbatimTextOutput("state2_1"),br(),
                      #plotOutput("plot2_1"),br(),        # HMSC analyses of the data with the “correct” model
                      verbatimTextOutput("state2_2"),br(),
                      plotOutput("plot2_2"),br(),        # MCMC convergence
                      verbatimTextOutput("state2_3"),br(),
                      plotOutput("plot2_3"),br(),        # Model fit and variance partitioning
                      verbatimTextOutput("state2_4"),br(),
                      plotOutput("plot2_4"),br(),        # Parameter estimates
                      plotOutput("plot2_4_1"),br(),
                      plotOutput("plot2_4_2"),br(),
                      verbatimTextOutput("state2_5"),br(),
                      plotOutput("plot2_5"),br(),        # Plotting variation over environmental gradients
                      plotOutput("plot2_5_1"),br(),
                      plotOutput("plot2_5_2"),br(),
                      verbatimTextOutput("code2_1")
             ),       
             tabPanel("About...", br(),br(),
                      verbatimTextOutput("us")
             ),
  )  
)

# Define server logic ----
server <- function(input, output) {
  
  # LIBRARY Hmsc----------------------------------------------------------------
  # source: https://cran.r-project.org/web/packages/Hmsc/vignettes/vignette_3_multivariate_high.pdf

  output$state1_1 <- renderPrint({#--------------------one----------------------
    cat("Generated simulation trait values for two traits for each of the 50 species
Source: https://cran.r-project.org/web/packages/Hmsc/vignettes/vignette_3_multivariate_high.pdf
        ")
  })
  output$plot1_1 <- renderPlot({   
    library(Hmsc)
    library(corrplot)
    library(MASS)
    set.seed(1)
    # Simulating phylogeny and species traits-----------------------------------
    ns = 50
    phy = ape::rcoal(n=ns, tip.label = sprintf('species_%.3d',1:ns), br = "coalescent")
#*********     
    save(phy, file="phy.RData")   
    plot(phy, show.tip.label = FALSE, no.margin = TRUE)
    
    # Generate simulated trait values 
    library(ape)
    C = ape::vcv(phy, model = "Brownian", corr = TRUE)
    spnames = colnames(C)
#*********     
    save(spnames, file="spnames.RData")    
    traits = matrix(NA,ncol =2,nrow = ns)
    for (i in 1:2){  
      traits[,i] = matrix(mvrnorm(n = 1, mu = rep(0,ns), Sigma=C))
    }
    rownames(traits) = spnames
    colnames(traits) = c("habitat.use","thermal.optimum")
    traits = as.data.frame(traits)
#*********     
    save(traits, file="traits.RData")
    
    
    # Visualize the trait distributions by plotting
    par(fig = c(0,0.6,0,0.8), mar=c(6,0,2,0))
    plot(phy, show.tip.label = FALSE, sub="Figure: Phylogenetic patterns of species’ trait values")
    par(fig = c(0.6,0.9,0.025,0.775), mar=c(6,0,2,0), new=T)
    plot.new()
    image.plot(t(traits),axes=FALSE,legend.width = 3,legend.shrink=1,
               col = colorRampPalette(c("blue","white","red"))(200))
    text(x=1.1, y=0.72, srt = 90, "H", cex=0.9, pos = 4)
    text(x=1.4, y=0.72, srt = 90, "T", cex=0.9, pos = 4)
    #dev.off()
  })
  output$code1_1 <- renderPrint({
    cat("
    library(Hmsc)
    library(corrplot)
    set.seed(1)
    # Simulating phylogeny and species traits-----------------------------------
    ns = 50
    phy = ape::rcoal(n=ns, tip.label = sprintf('species_%.3d',1:ns), br = 'coalescent')
    plot(phy, show.tip.label = FALSE, no.margin = TRUE)
    
    # Generate simulated trait valuesC = vcv(phy, model = 'Brownian', corr = TRUE)
    C = vcv(phy, model = 'Brownian', corr = TRUE)
    spnames = colnames(C)
    traits = matrix(NA,ncol =2,nrow = ns)
    for (i in 1:2){
      traits[,i] = matrix(mvrnorm(n = 1, mu = rep(0,ns), Sigma=C))
    }
    rownames(traits) = spnames
    colnames(traits) = c('habitat.use','thermal.optimum')
    traits = as.data.frame(traits)
    
    
    # Visualize the trait distributions by plotting
    par(fig = c(0,0.6,0,0.8), mar=c(6,0,2,0))
    plot(phy, show.tip.label = FALSE, sub='Figure: Phylogenetic patterns of species’ trait values')
    par(fig = c(0.6,0.9,0.025,0.775), mar=c(6,0,2,0), new=T)
    plot.new()
    image.plot(t(traits),axes=FALSE,legend.width = 3,legend.shrink=1,
               col = colorRampPalette(c('blue','white','red'))(200))
    text(x=1.1, y=0.72, srt = 90, 'H', cex=0.9, pos = 4)
    text(x=1.4, y=0.72, srt = 90, 'T', cex=0.9, pos = 4)
        ")
  }) 
  output$state2_1 <- renderPrint({#---------------------------------------------
    cat("HMSC analyses of the data with the “correct” model   
        ")
  })
  # HMSC analyses of the data with the “correct” model
  output$plot2_1 <- renderPlot({#-----------------------------------------------
    #
    #
    #
    #
    # simulate the environmental and species data
    n = 200
    habitat = factor(sample(x = c("forest","open"), size = n, replace=TRUE))
  #*********     
    save(habitat, file="habitat.RData")  
    climate = rnorm(n)
  #*********     
    save(climate, file="climate.RData")  
    
    # define the species niches
    ns = 50
    nc = 4
    mu = matrix(0,nrow=nc,ncol=ns)
    load("traits.RData"); traits;
    #expected niche of each species related to the "covariate" intercept
    mu[1, ] = -traits$thermal.optimum^2/4-traits$habitat.use
    #expected niche of each species related to the covariate forest
    #(open area as reference level, so included in intercept)
    mu[2, ] = 2*traits$habitat.use
    #expected niche of each species related to the covariate climate
    mu[3, ] = traits$thermal.optimum/2
    #expected niche of each species related to the covariate climate*climate
    mu[4, ] = -1/4
    beta = mu + 0.25*matrix(rnorm(n = ns*nc), ncol=ns)
    X = cbind(rep(1,ns), as.numeric(habitat=="forest"), climate, climate*climate)
    L = X%*%beta
    
    
    # generate the species data
    library(MASS)
    Y = L + mvrnorm(n=n, mu=rep(0,ns), Sigma=diag(ns))
    load("spnames.RData"); spnames;
    colnames(Y) = spnames
    
    
    ## HMSC analyses of the data with the “correct” model+++++++++++++++++++++++++++
    
    # model, included a random effect at the sampling unit level
    XData = data.frame(climate = climate, habitat = habitat)
    XFormula = ~habitat + poly(climate,degree = 2,raw = TRUE)
    TrFormula = ~habitat.use + thermal.optimum
    studyDesign = data.frame(sample = sprintf('sample_%.3d',1:n), stringsAsFactors=TRUE)
    rL = HmscRandomLevel(units = studyDesign$sample)
    rL$nfMax = 15
#*********       
    load("phy.RData"); phy;   
    m = Hmsc(Y = Y, XData = XData, XFormula = XFormula,
             TrData = traits, TrFormula = TrFormula,
             phyloTree = phy,
             studyDesign = studyDesign, ranLevels = list(sample = rL))

    #  MCMC sampling parameters fit the model.
    nChains = 2
    test.run = FALSE
    if (test.run){
      #with this option, the vignette evaluates in ca. 10 minutes in a laptop
      thin = 1
      samples = 100
      transient = 50
    } else {
      #with this option, the vignette evaluates in ca. 2 hrs in a laptop
      thin = 10
      samples = 100#1000
      transient = 50#500
    }
    verbose = 0
    m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
                   nChains = nChains, nParallel = nChains, verbose = verbose)
    ## setting updater$Gamma2=FALSE due to specified phylogeny matrix
#*********     
    save(m, file="m.RData")
  #  
  #  
  #  
  #  
  })
  output$state2_2 <- renderPrint({#---------------------------------------------
    cat("MCMC convergence
        ")
  })
  # MCMC convergence
  output$plot2_2 <- renderPlot({#-----------------------------------------------
    # evaluate MCMC convergence in terms of four kinds of parameters
#*********
    ns = 50
    load("m.RData"); m;    
    mpost = convertToCodaObject(m)
#*********     
    save(mpost, file="mpost.RData")    
    par(mfrow=c(2,3))
    ess.beta = effectiveSize(mpost$Beta)
    psrf.beta = gelman.diag(mpost$Beta, multivariate=FALSE)$psrf
    hist(ess.beta, sub =" Histograms of effective sample sizes and potential scale reduction factors (psrf) for Beta, Gamma,
and Omega parameters")
    hist(psrf.beta)
    ess.gamma = effectiveSize(mpost$Gamma)
    psrf.gamma = gelman.diag(mpost$Gamma, multivariate=FALSE)$psrf
    hist(ess.gamma)
    hist(psrf.gamma)
    sppairs = matrix(sample(x = 1:ns^2, size = 100))
    tmp = mpost$Omega[[1]]
    for (chain in 1:length(tmp)){
      tmp[[chain]] = tmp[[chain]][,sppairs]
    }
    ess.omega = effectiveSize(tmp)
    psrf.omega = gelman.diag(tmp, multivariate=FALSE)$psrf
    hist(ess.omega)
    hist(psrf.omega)
    #dev.off()
    
    print("ess.rho:")
    ## [1] "ess.rho:"
    effectiveSize(mpost$Rho)
    ## var1
    ## 1448.713
    print("psrf.rho:")
    ## [1] "psrf.rho:"
    gelman.diag(mpost$Rho)$psrf
    ## Point est. Upper C.I.
    ## [1,] 1.006644 1.035817
  })
  output$state2_3 <- renderPrint({#---------------------------------------------
    cat("Model fit and variance partitioning
        ")
  })
  # Model fit and variance partitioning
  output$plot2_3 <- renderPlot({#-----------------------------------------------
    # measure the explanatory power of the model by R^2.
#********* 
    load("m.RData"); m;
    load("habitat.RData"); habitat; 
    load("climate.RData"); climate; 
    par(mfrow=c(1,2))
    preds = computePredictedValues(m)
    MF = evaluateModelFit(hM=m, predY=preds)
    hist(MF$R2, xlim = c(0,1),sub="Histogram of species-specific explanatory r2 values", main=paste0("Mean = ", round(mean(MF$R2),2)))
    
    #head(m$X)
    
    VP = computeVariancePartitioning(m, group = c(1,1,2,2), groupnames = c("habitat","climate"))
    plotVariancePartitioning(m, VP = VP, sub = "Variance partitioning for each of 50 species")
    #dev.off()
    head(m$X)
    library(knitr)
    kable(VP$R2T$Beta)
    
    VP$R2T$Y
  })
  output$state2_4 <- renderPrint({#---------------------------------------------
    cat("Parameter estimates
        ")
  })
  # Parameter estimates
  output$plot2_4 <- renderPlot({#-----------------------------------------------
    # Beta parameters model species niches
    load("m.RData"); m;
    postBeta = getPostEstimate(m, parName = "Beta")
    plotBeta(m, post = postBeta, param = "Support",
             plotTree = TRUE, supportLevel = 0.95, split=.4, spNamesNumbers = c(F,F))
  })
  output$plot2_4_1 <- renderPlot({#-----------------------------------------------
    load("m.RData"); m;  
    # the Gamma parameters model how species traits influence their niches
    postGamma = getPostEstimate(m, parName = "Gamma")
    plotGamma(m, post=postGamma, param="Support", supportLevel = 0.95) 
  })  
  output$plot2_4_2 <- renderPlot({#-----------------------------------------------
    load("m.RData"); m; 
    # visualize the estimated residual associations among the species.
    OmegaCor = computeAssociations(m)
    supportLevel = 0.95
    toPlot = ((OmegaCor[[1]]$support>supportLevel)
              + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean
    corrplot(toPlot, method = "color",
             col=colorRampPalette(c("blue","white","red"))(200),
             tl.cex=.6, tl.col="black",
             title=paste("random effect level:", m$rLNames[1]), mar=c(0,0,1,0))
#*********   
    load("mpost.RData"); mpost;    
    summary(mpost$Rho)
  })
  output$state2_5 <- renderPrint({#---------------------------------------------
    cat("Plotting variation over environmental gradients
        ")
  })
  # Plotting variation over environmental gradients
  output$plot2_5 <- renderPlot({#-----------------------------------------------
    # construct an environmental gradient over the climate types.
#*********   
    load("m.RData"); m; 
    load("habitat.RData"); habitat; 
    load("climate.RData"); climate;

    Gradient = constructGradient(m,focalVariable = "climate",
                                 non.focalVariables = list("habitat"=list(3,"open")))
    Gradient$XDataNew
    
    predY = predict(m, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew,
                    ranLevels=Gradient$rLNew, expected=TRUE)
    par(mfrow=c(1,3))
    plotGradient(m, Gradient, pred=predY, measure="S", showData = TRUE)
    
    
    plotGradient(m, Gradient, pred=predY, measure="Y", index = 1, showData = TRUE)
    
    plotGradient(m, Gradient, pred=predY, measure="T", index = 3, showData = TRUE)
    #dev.off()
  })  
  output$plot2_5_1 <- renderPlot({#-----------------------------------------------
#*********    
    load("m.RData"); m;
    load("habitat.RData"); habitat; 
    load("climate.RData"); climate;
    # construct an environmental gradient over the habitat types.
    Gradient = constructGradient(m,focalVariable = "habitat",
                                 non.focalVariables = list("climate"=list(1)))
    Gradient$XDataNew
    
    predY = predict(m, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew,
                    ranLevels=Gradient$rLNew, expected=TRUE)

    plotGradient(m, Gradient, pred=predY, measure="Y", index=which.max(m$TrData$habitat.use),
                 showData = TRUE, jigger = 0.2)
  })  
  output$plot2_5_2 <- renderPlot({#-----------------------------------------------
    #*********    
    load("m.RData"); m;
    load("habitat.RData"); habitat; 
    load("climate.RData"); climate;
    # construct an environmental gradient over the habitat types.
    Gradient = constructGradient(m,focalVariable = "habitat",
                                 non.focalVariables = list("climate"=list(1)))
    Gradient$XDataNew
    
    predY = predict(m, XData=Gradient$XDataNew, studyDesign=Gradient$studyDesignNew,
                    ranLevels=Gradient$rLNew, expected=TRUE)
    
    plotGradient(m, Gradient, pred=predY, measure="Y", index=which.max(m$TrData$habitat.use),
                 showData = TRUE, jigger = 0.2)   
    
    plotGradient(m, Gradient, pred=predY, measure="T", index=2, showData = TRUE, jigger = 0.2)

  })
  

}

# Run the app ----
shinyApp(ui = ui, server = server)
