library(truncnorm)
library(parallel)

shinyServer(
  function(input, output, session) 
  {
    # reactive() makes reactive expression 
    # an expression whose result will change over time 
    priors_total = reactive(
      {
        d_total = numeric()
        if (input$total_prior == "pois")
        {
          d_total = rpois(n = input$n_sims, 
                          lambda = input$total_lambda)  
          # generate n random deviates with lambda parameter
          # with poisson distribution
        } else {
          d_total = rnbinom(n = input$n_sims,
                            size = input$total_r,
                            prob = input$total_p)
          # generate n random deviates with 
          # size number of successful trials - must be pos.
          # and prob of success in each trial (0,1]
          # negative binomial distribution
        }
        d_total
      }
    )
    
    priors_prop = reactive(
      { 
        d_prop = numeric()
        if (input$prop_prior == "beta")
        {
          d_prop = rbeta(n = input$n_sims, 
                         shape1 = input$prop_alpha, 
                         shape2 = input$prop_beta)  
          # generate n random deviates with 
          # shape 1 and shape 2 as parameters 
          # beta distribution
        } 
        else if (input$prop_prior == "tnorm")
        {
          d_prop = rtruncnorm(n = input$n_sims,
                              a = 0,
                              b = 1,
                              mean = input$prop_mu,
                              sd = input$prop_sigma)
          # generate n random deviates with 
          # a = lower bound
          # b = upper bound
          # with mean and sd stand. dev.  
          # Truncated Normal Distribution
        }
        else 
        {
          d_prop = runif(n = input$n_sims, 
                         min = input$prop_min, 
                         max = input$prop_max)
          # generate n random deviates 
          # from min to max 
          # Uniform Distribution
        }
        d_prop
      }
    )
    
    sims = reactive(
      {
        n_picked <- input$n_odd + 2*input$n_pairs
        #Total socks in laundry
        n_socks <- priors_total()
        #Proportion of socks in pairs
        prop_pairs <- priors_prop()
        #number of sock pairs
        n_pairs <- round(floor(n_socks / 2) * prop_pairs)
        #number of odd socks
        n_odd <- n_socks - n_pairs * 2
        dataS <- data.frame(n_pairs, n_odd, n_socks)
        # Simulating picking out n_picked socks
        socks <- function(x){
          list_sim <- rep(seq_len(x[1] + x[2]), 
                          rep(c(2,1), c(x[1], x[2])))
          picked_socks <- sample(list_sim, size = min(n_picked, x[3]))
          sock_counts <- table(picked_socks)
          sock_sim <- sum(sock_counts == 1)
          return(sock_sim)
        }
        sock_uniq <- apply(dataS, 1, socks)
        
        # Returning the parameters and counts of the number of matched 
        # and unique socks among those that were picked out.
        
        
        sock_uniq
      }
    )
    
    posterior_N = reactive(
      {
        priors_total()[sims()==input$n_odd1]
        # from pair of priors produced 
        # subset the ones where 
        # the num of unique socks our gen_model produced 
        # equals the num of unique socks we picked 
        
      }
    )
    
    posterior_p = reactive(
      {
        priors_prop()[sims()==input$n_odd1]
        # from pair of priors produced 
        # subset the ones where 
        # the num of unique socks our gen_model produced 
        # equals the num of unique socks we picked 
      }
    )
    
    output$total_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1), mfrow = c(1,2))
        hist(posterior_N(), freq=FALSE,
             main="Posterior - Total Socks in Laundry",
             xlab = "Total Socks")
        
        lines(density(posterior_N()), col='blue',lwd=2)

        hist(priors_total(), freq=FALSE,
             main="Prior - Proportion of Socks in Pairs",
             xlab = "% of Total Socks Part of a Pair")
        lines(density(priors_total()), col='red',lwd=2)
      }
    )
    
    output$prop_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1), mfrow = c(1,2))
        hist(posterior_p(), freq=FALSE,
             main="Posterior - Proportion of Socks in Pairs",
             xlab = "% of Total Socks Part of a Pair")
        
        lines(density(posterior_p()), col='red',lwd=2)
        
        hist(priors_prop(), freq=FALSE,
             main="Prior - Proportion of Socks in Pairs",
             xlab = "% of Total Socks Part of a Pair")
        lines(density(priors_prop()), col='red',lwd=2)
      }
    )
  }
)

