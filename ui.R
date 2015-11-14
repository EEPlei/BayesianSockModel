library(shiny)

total_priors = c("Poisson" = "pois", 
                 "Negative Binomial" = "nbinom")
# discrete dist. for total # of socks in washer #

prop_priors = c("Beta" = "beta", 
                "Truncated Normal"="tnorm",
                "Uniform" = "unif")
# cont. dist for prop of socks that are pairs #

option_table = c("Yes" = "y", 
                 "No" = "n")

shinyUI( # create user interface #
  fluidPage( # create fluid page layout # 
    titlePanel( # create panel for app title #
      "Karl Broman's Socks" # app title # 
    ),
    sidebarPanel( # creates sidebar panel # 
      
      numericInput("n_sims", h4("Simulations:"), 
                   value = 10000, min = 100, step = 1),
      hr(),
      h4("Data:"),
      sliderInput("n_pairs1", "Number of Sock Pairs Picked:", 
                  min=0, max=30, value=0, step=1),
      sliderInput("n_odd1", "Number of Unique Socks Picked:", 
                  min=0, max=30, value=11, step=1),
      hr(),
      h4("Priors:"),
      radioButtons("total_prior", "Prior for Total Number of Socks", 
                  total_priors),
      radioButtons("prop_prior", "Prior for Proportion of Paired Socks", 
                  prop_priors),
      hr(),
      h4("Hyperparameters:"),
      #conditionalPanel() panel that may or may not be visible #
      conditionalPanel(
        condition="input.total_prior == 'pois'",
        sliderInput("total_lambda",HTML("Total Prior - &lambda;"), 
                    value = 50, min=1, max=120)
        ),
      conditionalPanel(
        condition="input.total_prior == 'nbinom'",
        numericInput("total_r",HTML("Total Prior - r"), 
                     value = 30, min=1, max=120),
        numericInput("total_p",HTML("Total Prior - p"), 
                     value = 0.5, min=0, max=1)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'beta'",
        numericInput("prop_alpha",HTML("Proportion Prior - &alpha;"), 
                     value = 1/3 , min=0, max=NA),
        numericInput("prop_beta",HTML("Proportion Prior - &beta;"), 
                     value = 1, min=0, max=NA)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'tnorm'",
        numericInput("prop_mu",HTML("Proportion Prior - &mu;"), 
                     value = 0.5, min=NA, max=NA),
        numericInput("prop_sigma",HTML("Proportion Prior - &sigma;"), 
                     value = 0.1, min=0, max=NA)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'unif'",
        numericInput("prop_min",HTML("Proportion Prior - Min;"), 
                     value = 0, min=0, max=1),
        numericInput("prop_max",HTML("Proportion Prior - Max;"), 
                     value = 0.5, min=0, max=1)
      ), 
      hr(),
      selectInput("table", "Summary Table Option", option_table), 
      hr(), 
      selectInput("table1", "95% Credible Interval Option", option_table),
      hr(), 
      selectInput("showTrueSocks", "Show True Values (only when your sampling result is the same as Karl's draw)", selected = "n", option_table)
    ),
    mainPanel(
      h4("Results:"),
      #Total Socks in Laundry
      plotOutput("prior_plot"), 
      br(),
      #Proportion of Socks in Pairs
      plotOutput("post_plot"), 
      br(),
      conditionalPanel(
        condition = "input.table == 'y'", 
        tableOutput("summary_table")
      ), 
      br(),
      conditionalPanel(
        condition = "input.table1 == 'y'", 
        tableOutput("credible_table")
      ),
      br(),
      conditionalPanel(
        condition = "input.showTrueSocks == 'y' & input.n_pairs1 == 0 & input.n_odd1 == 11", 
        tableOutput("True_Result")
      )
    )
  )
)
