if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(rspat)) install.packages("rspat", repos = "http://cran.us.r-project.org")
if(!require(terra)) install.packages("terra", repos = "http://cran.us.r-project.org")

# Load libraries
library(ggplot2)
library(rspat)
library(terra)
library(reactable)


# Define UI ----
ui <- fluidPage(
  titlePanel("ANALYZING SPECIES DISTRIBUTION DATA"),
  sidebarLayout(
    sidebarPanel(
      h4("Description"),
      p("An attempt analyze species distribution data with R."),
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
        box(title = "Species distribution modeling", plotOutput("distribPlot1", width = "auto", height = "800"), width = 12, height = "auto"),
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {

  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # ANALYZING SPECIES DISTRIBUTION DATA-----------------------------------------
  ##----------------------------------------------------------------------------
  # Source: https://rspatial.org/cases/3-speciesdistribution.html
  
  library(rspat)
  library(terra)

  # Descriptive statistics and plots
  library(reactable)
  output$table = renderReactable({

    f <- system.file("wildpot.csv", package="rspat")
    #basename(f)
    ## [1] "wildpot.csv"
    v <- read.csv2(f)

    ##  The coordinate data is in degrees, minutes, seconds---------------------
    # first coerce character values to numbers
    for (i in c('LongD', 'LongM', 'LongS', 'LatD', 'LatM', 'LatS')) {
      v[, i] <- as.numeric(v[,i])
    }
    v$lon <- -1 * (v$LongD + v$LongM / 60 + v$LongS / 3600)
    v$lat <- v$LatD + v$LatM / 60 + v$LatS / 3600
    # Southern hemisphere gets a negative sign
    v$lat[v$LatH == 'S'] <- -1 * v$lat[v$LatH == 'S']
    head(v)
    v
    saveRDS(v, file = "v.Rds")
    
    reactable(
      v[1:length(v$ID), ],
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
  
  output$distribPlot1 <- renderPlot({
    
    ##  extracted file is a csv file (comma-seperated-by values).-------------------
    hh=spp=maxD=CA=NULL;

    f <- system.file("wildpot.csv", package="rspat")
    #basename(f)
    ## [1] "wildpot.csv"
    v <- read.csv2(f)
    #v <- queryT
    ##  The coordinate data is in degrees, minutes, seconds-------------------------
    
    # first coerce character values to numbers
    for (i in c('LongD', 'LongM', 'LongS', 'LatD', 'LatM', 'LatS')) {
      v[, i] <- as.numeric(v[,i])
    }
    v$lon <- -1 * (v$LongD + v$LongM / 60 + v$LongS / 3600)
    v$lat <- v$LatD + v$LatM / 60 + v$LatS / 3600
    # Southern hemisphere gets a negative sign
    v$lat[v$LatH == 'S'] <- -1 * v$lat[v$LatH == 'S']
    #head(v)

    ##  Get a SpatVector with most of the countries of the Americas.----------------
    
    cn <- spat_data("pt_countries")
    class(cn)
    ## [1] "SpatVector"
    ## attr(,"package")
    ## [1] "terra"
    
    ##  Make a quick map------------------------------------------------------------
    par(mfrow = c(4,3))
    
    plot(cn, xlim=c(-120, -40), ylim=c(-40,40), axes=TRUE)
    points(v$lon, v$lat, cex=.5, col='red')
    
    ## create a SpatVector for the potato data with the formula approach------------
    
    sp <- vect(v, crs="+proj=longlat +datum=WGS84")
    
    ##  summarize the data by country-----------------------------------------------
    
    table(v$COUNTRY)
    ##...
    ##        MEXICO        PANAMA      PARAGUAY          Peru          PERU
    ##           843            13            19             1          1043
    ##...
    ##           157             4            12
    # note Peru and PERU
    v$COUNTRY <- toupper(v$COUNTRY)
    table(v$COUNTRY)
    ##
    ##     ARGENTINA       BOLIVIA        BRAZIL         CHILE      COLOMBIA
    ##          1474           985            17           100           107
    ## ...
    # same fix for the SpatVector
    sp$COUNTRY <- toupper(sp$COUNTRY)
    
    ##  determine the country using a spatial query---------------------------------
    
    vv <- intersect(sp[, "COUNTRY"], cn)
    names(vv)[1] <- "ptCountry"
    head(vv)
    ##   ptCountry   COUNTRY
    ## 1 ARGENTINA ARGENTINA
    ## 2 ARGENTINA ARGENTINA
    ## ...
    table(vv$COUNTRY)
    ##
    ##          ARGENTINA            BOLIVIA             BRASIL              CHILE
    ##               1473                985                 17                 94
    ##...
    
    # some fixes first
    # apparantly in the ocean (small island missing from polygon data)
    vv$COUNTRY[is.na(vv$COUNTRY)] <- ""
    # some spelling differenes
    vv$COUNTRY[vv$COUNTRY=="UNITED STATES, THE"] <- "UNITED STATES"
    vv$COUNTRY[vv$COUNTRY=="BRASIL"] <- "BRAZIL"
    i <- which(toupper(vv$ptCountry) != vv$COUNTRY)
    i
    ## [1]  581  582 1616 1634 3214 3516
    as.data.frame(vv[i,])
    ##   ptCountry   COUNTRY
    ## 1  COLOMBIA   ECUADOR
    ## 2   ECUADOR  COLOMBIA
    ## 3  COLOMBIA   ECUADOR
    ## 4  COLOMBIA VENEZUELA
    ## 5 GUATEMALA    MEXICO
    ## 6  COLOMBIA VENEZUELA
    plot(cn, xlim=c(-120, -40), ylim=c(-40,40), axes=TRUE)
    points(sp, cex=.25, pch='+', col='blue')
    points(vv[i,], col='red', pch='x', cex=1.5)
    
    ##  compute the number of species for each country.-----------------------------
    
    spc <- tapply(v$SPECIES, sp$COUNTRY, function(x)length(unique(x)) )
    spc <- data.frame(COUNTRY=names(spc), nspp = spc)
    # merge with country SpatVector --- fix the names in the polygons this time
    cn$COUNTRY[cn$COUNTRY=="UNITED STATES, THE"] <- "UNITED STATES"
    cn$COUNTRY[cn$COUNTRY=="BRASIL"] <- "BRAZIL"
    cns <- merge(cn, spc, by="COUNTRY", all.x=TRUE)
    plot(cns, "nspp", col = rev(terrain.colors(25)), breaks=c(1,5,10,20,30,40,90))
    
    tb <- table(v[ c('COUNTRY', 'SPECIES')])
    # a big table
    dim(tb)
    ## [1]  16 195
    # show two columns
    tb[,2:3]
    ##                SPECIES
    ## COUNTRY         S. achacachense Cßrdenas S. acroglossum Juz.
    ##   ARGENTINA                            0                   0
    ##   BOLIVIA                              8                   0
    
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
    
    d <- v[, c('lat', 'SPECIES')]
    d$lat <- round(d$lat)
    g <- tapply(d$SPECIES, d$lat, function(x) length(unique(na.omit(x))) )
    plot(names(g), g)
    # moving average
    lines(names(g), raster::movingFun(g, 3))
    
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