if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("lattice", repos = "http://cran.us.r-project.org")
if(!require(qicharts)) install.packages("qicharts", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")

# Load libraries
library(reactable)
library(lattice)
library(shiny)
library(qicharts)
library(ggplot2)


# Define UI ----
ui <- fluidPage(
  titlePanel("Statistics for Quality Control: Modelling Quality with R"),
  sidebarLayout(
    sidebarPanel(
      h4("Description"),
      p("It's provided the necessary background to understand the fundamental ideas of descriptive and inferential statistics in three steps. 1- The Description of Variability. 2- Probability Distributions. 3- Inference About Distribution Parameters."),
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
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("Table", reactableOutput("table1")),
                    tabPanel("The Description of Variability", plotOutput("Descrip1")),
                    tabPanel("Description-2", plotOutput("Descrip2")),
                    tabPanel("Description-3", plotOutput("Descrip3")),
                    #tabPanel("other", verbatimTextOutput("other")),
                    #tabPanel("Plot", plotOutput("plot")),
                    #tabPanel("Table", reactableOutput("table"))
        
        ),
        hr(),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Probability Distributions", plotOutput("Probability1")),
                    tabPanel("Probability-2 ", plotOutput("Probability2")),
                    tabPanel("Probability-3", plotOutput("Probability3")),
                    #tabPanel("other", verbatimTextOutput("other")),
                    #tabPanel("Plot", plotOutput("plot")),
                    #tabPanel("Table", reactableOutput("table"))
                    
        ),
        hr(),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel("Inference About Distribution Parameters", verbatimTextOutput("Inference")),
                    #tabPanel("other", verbatimTextOutput("other")),
                    #tabPanel("Plot", plotOutput("plot")),
                    #tabPanel("Table", reactableOutput("table"))
                    
        )
        
      )
    )
  )
)


# Define server logic ----
server <- function(input, output) {

  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # Quality Control with R
  # An ISO Standards Approach---------------------------------------------------
  ##----------------------------------------------------------------------------
  # Source: https://link.springer.com/book/10.1007/978-3-319-24046-6
  
  library(reactable)
  output$table1 = renderReactable({
  day1 <- c(0.821, 0.846, 0.892, 0.750, 0.773, 0.786,
            0.956, 0.840, 0.913, 0.737, 0.793, 0.872)
  day2 <- c(0.678, 0.742, 0.684, 0.766, 0.721, 0.785,
            0.759, 0.708, 0.789, 0.732, 0.804, 0.758)
  plates <- data.frame(thickness = c(day1, day2),
                       day = rep(c("Day1", "Day2"), each = 12))
  saveRDS(plates, file = "plates.Rds")
  plates<-readRDS("plates.RDS")
  reactable(
    plates[1:length(plates$thickness), ],
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
  
  
  output$Descrip1 <- renderPlot({
    
    plates<-readRDS("plates.RDS")
    par(mfrow = c(2,1))
    # Graphical Description of Variation----------------------------------------
    # Metal plates thickness.------------------
    #library(lattice)
    #trellis.par.set(canonical.theme(color = FALSE))
    #trellis.par.set("background", list(col = gray(0.85)))
    #trellis.par.set("panel.background", list(col = "white"))
    #histogram(~ thickness | day, data = plates,
              #main = "Histogram of Thickness by day",
              #xlab = "Thickness (in)",
              #ylab = "Frequency",
              #type = "count")
    

    # Run Chart: Metal plates thickness (cont.)-----------------
    
    plot(plates$thickness,
         type = "b",
         main = "Run Chart of Thickness",
         las = 1,
         ylab = "Thickness",
         xlab = "Plate number",
         pch = 20)
    abline(h = median(plates$thickness),
           lwd = 2)
    
    library(qicharts)
    qic(thickness,
        data = plates,
        freeze = 12,
        pre.text = "Day 1",
        post.text = "Day 2",
        runvals = TRUE)
  })
  
  output$Descrip2 <- renderPlot({
    
    library(lattice)
    plates<-readRDS("plates.RDS")
    # Metal plates thickness (cont.) Tier chart.-----
    plates$shift <- factor(paste0("Shift",
                                  (rep(1:2, each = 6))))
    plates$dayshift <- factor(paste(plates$day,
                                    plates$shift, sep = "."))
    
    dotplot(thickness ~ dayshift,
            data = plates,
            pch = "-",
            cex = 4,
            panel = function(x, y, ...){
              panel.dotplot(x, y, ...)
              panel.superpose(x, y,
                              subscripts = 1:length(x), x,
                              panel.groups = "llines",
                              col = "black",
                              type = "l",
                              lty = 1)
            })
  })
  
  
  output$Descrip3 <- renderPlot({ 
    
    plates<-readRDS("plates.RDS")
    # Box-and-Whisker Plot---------------------
    plates$shift <- factor(paste0("Shift",
                                  (rep(1:2, each = 6))))
    plates$dayshift <- factor(paste(plates$day,
                                    plates$shift, sep = "."))
    #boxplot(plates$thickness)
    #boxplot(thickness ~ day, data = plates)
    bwplot(thickness ~ shift | day , data = plates)
    
    # Numerical Description of Variation----------------------------------------
    # Central Tendency
  }) 
    
  #-----------------------------------------------------------------------------
  
  output$other <- renderText({ 
    print("im processing")
  })  
    
  output$Probability1 <- renderPlot({ 
    
    Days345 <- c(0.608, 0.700, 0.864, 0.643, 1.188, 0.610,
                 0.741, 0.646, 0.782, 0.709, 0.668, 0.684,
                 1.034, 1.242, 0.697, 0.689, 0.759, 0.700,
                 0.604, 0.676, 0.687, 0.666, 0.612, 0.638,
                 0.829, 0.838, 0.944, 0.829, 0.826, 0.649,
                 0.702, 0.764, 0.873, 0.784, 0.697, 0.658)
    
    library(qcc)
    qcc(Days345, type = "xbar.one")
  })  
  

  output$Probability2 <- renderPlot({   
  
  Days345 <- c(0.608, 0.700, 0.864, 0.643, 1.188, 0.610,
               0.741, 0.646, 0.782, 0.709, 0.668, 0.684,
               1.034, 1.242, 0.697, 0.689, 0.759, 0.700,
               0.604, 0.676, 0.687, 0.666, 0.612, 0.638,
               0.829, 0.838, 0.944, 0.829, 0.826, 0.649,
               0.702, 0.764, 0.873, 0.784, 0.697, 0.658)
  library(MASS)
  boxcox(Days345 ~ 1, lambda = seq(-5, 5, 0.1))
  
  })  
  
  output$Probability3 <- renderPlot({ 
    
    Days345 <- c(0.608, 0.700, 0.864, 0.643, 1.188, 0.610,
                 0.741, 0.646, 0.782, 0.709, 0.668, 0.684,
                 1.034, 1.242, 0.697, 0.689, 0.759, 0.700,
                 0.604, 0.676, 0.687, 0.666, 0.612, 0.638,
                 0.829, 0.838, 0.944, 0.829, 0.826, 0.649,
                 0.702, 0.764, 0.873, 0.784, 0.697, 0.658)
    library(car)
    d345.trans <- powerTransform(Days345)
    summary(d345.trans)
    d345.lambda <- coef(d345.trans, round = TRUE)
    d345.lambda
    transformed.Days345 <- bcPower(Days345,
                                   lambda = d345.lambda)
    qcc(transformed.Days345, type = "xbar.one")

  })
  output$Inference <- renderText({   
    print("im processing")
  })    
    
}
# Run the app ----
shinyApp(ui = ui, server = server)