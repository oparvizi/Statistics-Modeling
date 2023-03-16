setwd("D:/CODES/R")

if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(terra)) install.packages("terra", repos = "http://cran.us.r-project.org")

# Load libraries
library(terra)
library(reactable)

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
  navbarPage("Statistics & Modeling",
             #h4("Description"),
             #p("An attempt to analyze species distribution data with R."),
             tabPanel("Species distribution analysis", br(),br(),
                      reactableOutput("table"),br(),br(),
                      plotOutput("distribPlot1"),br(),br(),br(),
                      verbatimTextOutput("summary0_0"),br(),br(),
                      verbatimTextOutput("summary0_1")
             ),
             tabPanel("About...", br(),br(),
                      verbatimTextOutput("us")
             ),
  )  
)

# Define server logic ----
server <- function(input, output) {

  library(terra)
  
  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # ANALYZING SPECIES DISTRIBUTION DATA-----------------------------------------
  ##----------------------------------------------------------------------------
  # Source: https://rspatial.org/cases/3-speciesdistribution.html
  dat <- read.csv("wildpot.csv")
  saveRDS(dat, file = "dat.Rds")
  readRDS(file = "dat.Rds")
  ##  The coordinate data is in degrees, minutes, seconds---------------------
  # first coerce character values to numbers
  dat <- read.csv("wildpot.csv")
  for (i in c('LongD', 'LongM', 'LongS', 'LatD', 'LatM', 'LatS')) {
    dat[, i] <- as.numeric(dat[,i])
  }
  dat$lon <- -1 * (dat$LongD + dat$LongM / 60 + dat$LongS / 3600)
  dat$lat <- dat$LatD + dat$LatM / 60 + dat$LatS / 3600
  # Southern hemisphere gets a negative sign
  dat$lat[dat$LatH == 'S'] <- -1 * dat$lat[dat$LatH == 'S']
  saveRDS(dat, file = "dat.Rds")
  
    # Descriptive statistics and plots
  output$summary0_0 <- renderPrint({
    # Description of the data
    readRDS(file = "dat.Rds")
    str(dat)
  })#--------------------------------------------------------------------------- 
  output$summary0_1 <- renderPrint({
    # Description of the data
    readRDS(file = "dat.Rds")
    summary(dat)
  })#--------------------------------------------------------------------------- 
  output$table = renderReactable({
    
    library(reactable)
    readRDS(file = "dat.Rds")
    reactable(
      dat[1:length(dat$ID), ],
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
  })#--------------------------------------------------------------------------- 
   output$distribPlot1 <- renderPlot({
    readRDS(file = "dat.Rds")
    ##  Get a SpatVector with most of the countries of the Americas.----------------
    #source: https://github.com/rspatial/rspat
    spat_data <- function(name) {
      name <- paste0(tools::file_path_sans_ext(name[1]), ".rds")
      #fn <- system.file(file.path("rds", name), package="rspat")
      if (!(file.exists(name))) {
        stop(paste(name, "is not a valid data set name."))
      }
      x <- readRDS(name)
      if (inherits(x, "PackedSpatVector")) {
        x <- terra::vect(x)
      } else if (inherits(x, "PackedSpatRaster")) {
        x <- terra::rast(x)
      }
      x
    }
     cn <- spat_data("pt_countries")
    ##  Make a quick map------------------------------------------------------------
    layout(matrix(1:12, ncol=6, byrow=TRUE))
    plot(cn, xlim=c(-120, -40), ylim=c(-40,40), axes=TRUE)
    points(dat$lon, dat$lat, cex=.5, col='red')
    
    ## create a SpatVector for the potato data with the formula approach------------
    sp <- vect(dat, crs="+proj=longlat +datum=WGS84")
    
    table(dat$COUNTRY)
    dat$COUNTRY <- toupper(dat$COUNTRY)
    table(dat$COUNTRY)

    # same fix for the SpatVector
    sp$COUNTRY <- toupper(sp$COUNTRY)
    
    ##  determine the country using a spatial query---------------------------------
        vv <- intersect(sp[, "COUNTRY"], cn)
    names(vv)[1] <- "ptCountry"
    head(vv)
    table(vv$COUNTRY)
    # some fixes first
    # apparantly in the ocean (small island missing from polygon data)
    vv$COUNTRY[is.na(vv$COUNTRY)] <- ""
    # some spelling differenes
    vv$COUNTRY[vv$COUNTRY=="UNITED STATES, THE"] <- "UNITED STATES"
    vv$COUNTRY[vv$COUNTRY=="BRASIL"] <- "BRAZIL"
    i <- which(toupper(vv$ptCountry) != vv$COUNTRY)
    #i
    as.data.frame(vv[i,])
    plot(cn, xlim=c(-120, -40), ylim=c(-40,40), axes=TRUE)
    points(sp, cex=.25, pch='+', col='blue')
    points(vv[i,], col='red', pch='x', cex=1.5)
    
    ##  compute the number of species for each country.-----------------------------
        spc <- tapply(dat$SPECIES, sp$COUNTRY, function(x)length(unique(x)) )
    spc <- data.frame(COUNTRY=names(spc), nspp = spc)
    # merge with country SpatVector --- fix the names in the polygons this time
    cn$COUNTRY[cn$COUNTRY=="UNITED STATES, THE"] <- "UNITED STATES"
    cn$COUNTRY[cn$COUNTRY=="BRASIL"] <- "BRAZIL"
    cns <- merge(cn, spc, by="COUNTRY", all.x=TRUE)
    plot(cns, "nspp", col = rev(terrain.colors(25)), breaks=c(1,5,10,20,30,40,90))
    
    tb <- table(dat[ c('COUNTRY', 'SPECIES')])
    #dim(tb)
    tb[,2:3]

    ## Projecting spatial data------------------------------------------------------
    # the CRS we want
    laea <-"+proj=laea  +lat_0=0 +lon_0=-80"
    clb <- project(cn, laea)
    pts <- project(sp, laea)
    plot(clb)
    points(pts, col='red', cex=.5)
    
    ## Species richness-------------------------------------------------------------
    r <- rast(clb)
    # 200 km = 200000 m
    res(r) <- 200000
    
    rich <- rasterize(pts, r, "SPECIES", function(x, ...) length(unique(na.omit(x))))
    plot(rich)
    lines(clb)
    
    obs <- rasterize(pts, r, field="SPECIES", fun=function(x, ...)length((na.omit(x))) )
    plot(obs)
    lines(clb)
    
    plot(obs, rich, cex=1, xlab="Observations", ylab="Richness")
    
    d <- dat[, c('lat', 'SPECIES')]
    d$lat <- round(d$lat)
    g <- tapply(d$SPECIES, d$lat, function(x) length(unique(na.omit(x))) )
    plot(names(g), g)
    # moving average
    
    movingFun <- function(x, n, fun=mean, type='around', circular=FALSE, na.rm=FALSE)  { 
      n <- round(abs(n))
      if (n == 0) { stop('n == 0')  }
      x = as.vector(x)
      lng <- length(x)
      if (type == 'around') {
        hn <- floor(n/2)
        if (circular) {	x <- c(x[(lng-hn+1):lng], x, x[1:hn])
        } else { x <- c(rep(NA, hn), x, rep(NA, hn)) }
      } else if (type == 'to') {
        if (circular) { x <- c(x[(lng-n+2):lng], x)
        } else { x <- c(rep(NA, n-1), x) }
      } else if (type == 'from') {
        if (circular) { x <- c(x,  x[1:n])
        } else { x <- c(x, rep(NA, n))	}
      } else {
        stop('unknown type; should be "around", "to", or "from"')
      }
      m <- matrix(ncol=n, nrow=lng)
      for (i in 1:n) { m[,i] <- x[i:(lng+i-1)] }
      apply(m, MARGIN=1, FUN=fun, na.rm=na.rm)
    }
  
    
    .roll <- function(x, n) {
      # by Josh O'Brien
      x[(seq_along(x) - (n+1)) %% length(x) + 1]
    }
    
    
    lines(names(g), movingFun(g, 3))
    
    ##  Range size------------------------------------------------------------------
    spp <- unique(pts$SPECIES)
    maxD <- rep(NA, length(spp))
    for (s in 1:length(spp)) {
      # get the coordinates for species 's'
      p <- pts[pts$SPECIES == spp[s], ]
      if (nrow(p) < 2) next
      # distance matrix
      d <- as.matrix(distance(p))
      # ignore the distance of a point to itself
      diag(d) <- NA
      # get max value
      maxD[s] <- max(d, na.rm=TRUE)
    }
    # Note the typical J shape
    plot(rev(sort(maxD))/1000, ylab="maxD (km)")
    
    ##  Compute CA------------------------------------------------------------------
    
    CA <- rep(NA, length(spp))
    for (s in 1:length(spp)) {
      p <- pts[pts$SPECIES == spp[s], ]
      # run "circles" model
      m <- aggregate(buffer(p, 50000))
      CA[s] <- expanse(m)
    }
    # standardize to the size of one circle
    CA <- CA / (pi * 50000^2)
    plot(rev(sort(CA)), ylab='CA50')
    
    ##  Make convex hull range polygons---------------------------------------------
    
    hull <- list()
    for (s in 1:length(spp)) {
      p <- unique(pts[pts$SPECIES == spp[s], ])
      # need at least three (unique) points for hull
      if (nrow(p) > 3) {
        h <- convHull(p)
        if (geomtype(h) == "polygons") {
          hull[[s]] <- h
        }
      }
    }
    
    ## Plot the hulls. First remove the empty hulls (you cannot make a hull if you do not have at least three points).
    
    # which elements are NULL
    i <- which(!sapply(hull, is.null))
    h <- hull[i]
    # combine them
    hh <- do.call(rbind, h)
    hh
    plot(hh)
    
    ahull <- expanse(hh)
    plot(rev(sort(ahull))/1000, ylab="Area of convex hull")
    
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)