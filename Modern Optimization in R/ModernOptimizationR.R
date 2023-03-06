if(!require(reactable)) install.packages("reactable", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(scatterplot3d)) install.packages("scatterplot3d", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")
#if(!require()) install.packages("", repos = "http://cran.us.r-project.org")

# Load libraries
library(reactable)
library(shiny)
library(scatterplot3d)
library()


# Define UI ----
ui <- fluidPage(
  titlePanel("Modern Optimization"),
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
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    tabPanel("monte carlo search", verbatimTextOutput("monte_carlo")),
                    tabPanel("other", verbatimTextOutput("other")),
                    #tabPanel("Plot", plotOutput("plot")),
                    #tabPanel("Table", tableOutput("table"))
        
        )
        
      )
    )
  )
)

# Define server logic ----
server <- function(input, output) {

  ##### CALCULATION
  ##----------------------------------------------------------------------------
  # Modern Optimization with R--------------------------------------------------
  ##----------------------------------------------------------------------------
  # Source: https://link.springer.com/book/10.1007/978-3-030-72819-9
  
  output$monte_carlo <- renderPrint({  
  ### test-mc.R file ###

  #-----------------------------------------------------------------------------
  # full bind search method
  #    search - matrix with solutions x D
  #    FUN - evaluation function
  #    type - "min" or "max"
  #    ... - extra parameters for FUN
  fsearch=function(search,FUN,type="min",...)
  {
    x=apply(search,1,FUN,...) # run FUN over all search rows
    ib=switch(type,min=which.min(x),max=which.max(x))
    return(list(index=ib,sol=search[ib,],eval=x[ib]))
  }
  
  # depth-first full search method
  #    l - level of the tree
  #    b - branch of the tree
  #    domain - vector list of size D with domain values
  #    FUN - eval function
  #    type - "min" or "max"
  #    D - dimension (number of variables)
  #    x - current solution vector
  #    bcur - current best sol
  #    ... - extra parameters for FUN
  #dfsearch=function(l=1,b=1,domain,FUN,type="min",D=length(domain),
  #                  x=rep(NA,D),
  #                  bcur=switch(type,min=list(sol=NULL,eval=Inf),
  #                              max=list(sol=NULL,eval=-Inf)),
  #                  ...)
  #{ if((l-1)==D) # "leave" with solution x to be tested:
  #{ f=FUN(x,...);fb=bcur$eval
  #ib=switch(type,min=which.min(c(fb,f)),
  #          max=which.max(c(fb,f)))
  #if(ib==1) return (bcur) else return(list(sol=x,eval=f))
  #}
  #  else # go through sub branches
  #  { for(j in 1:length(domain[[l]]))
  #  { x[l]=domain[[l]][j]
  #  bcur=dfsearch(l+1,j,domain,FUN,type,D=D,
  #                x=x,bcur=bcur,...)
  #  }
  #    return(bcur)
  #  }
  #}
  
  #-----------------------------------------------------------------------------
  # compute the bag factory profit for x:
  #    x - a vector of prices
  profit=function(x)    # x - a vector of prices
  { x=round(x,digits=0) # convert x into integer
    s=sales(x)          # get the expected sales
    c=cost(s)           # get the expected cost
    profit=sum(s*x-c)   # compute the profit
    return(profit) 
  # local variables x, s, c and profit are lost from here
  }
  
  # compute the cost for producing units:
  #    units - number of units produced
  #    A - fixed cost, cpu - cost per unit
  cost=function(units,A=100,cpu=35-5*(1:length(units)))
  { return(A+cpu*units) }
  
  # compute the estimated sales for x:
  #    x - a vector of prices, m - marketing effort
  #    A, B, C - constants of the estimated function
  sales=function(x,A=1000,B=200,C=141,
                 m=seq(2,length.out=length(x),by=-0.25))
  { return(round(m*(A/log(x+B)-C),digits=0))}
  
  # example of a simple recursive function:
  fact=function(x=0) # x - integer number 
  { if(x==0) return(1) else return(x*fact(x-1))}
  
  #-----------------------------------------------------------------------------
  
  ### montecarlo.R file ###
  # montecarlo uniform search method
  # N - number of samples
  # lower - vector with lowest values for each dimension
  # upper - vector with highest values for each dimension
  # domain - vector list of size D with domain values
  # FUN - evaluation function
  # type - "min" or "max"
  # ... - extra parameters for FUN
  mcsearch=function(N,lower,upper,FUN,type="min",...)
  { D=length(lower)
    s=matrix(nrow=N,ncol=D) # set the search space
    for(i in 1:N) s[i,]=runif(D,lower,upper)
    fsearch(s,FUN,type,...) # best solution
  }
  
  N=10000 # set the number of samples
  cat("monte carlo search (N:",N,")\n")
  # bag prices
  cat("bag prices:")
  S=mcsearch(N,rep(1,5),rep(1000,5),profit,"max")
  cat("s:",S$sol,"f:",S$eval,"\n")
  # real-value functions: sphere and rastrigin:
  sphere=function(x) sum(x^2)
  rastrigin=function(x) 10*length(x)+sum(x^2-10*cos(2*pi*x))
  D=c(2,30)
  label="sphere"
  for(i in 1:length(D))
  { S=mcsearch(N,rep(-5.2,D[i]),rep(5.2,D[i]),sphere,"min")
    cat(label,"D:",D[i],"s:",S$sol[1:2],"f:",S$eval,"\n")
  }
  label="rastrigin"
  for(i in 1:length(D))
  { S=mcsearch(N,rep(-5.2,D[i]),rep(5.2,D[i]),rastrigin,"min")
    cat(label,"D:",D[i],"s:",S$sol[1:2],"f:",S$eval,"\n")
  }
  })
  output$other <- renderText({ 
    print("im processing")
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)