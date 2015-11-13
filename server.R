library(truncnorm)

shinyServer(
  function(input, output, session) 
  {
    # reactive() makes reactive expression 
    # an expression whose result will change over time 
    priors = reactive(
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
        data.frame(total = d_total, prop = d_prop)
      }
    )
    
    sims = reactive(
      {
        gen_model = function(prior_N_total,prior_prop_total)
        {
          n_picked <- input$n_odd + 2*input$n_pairs
          #Total socks in laundry
          n_socks <- prior_N_total
          #Proportion of socks in pairs
          prop_pairs <- prior_prop_total
          #number of sock pairs
          n_pairs <- round(floor(n_socks / 2) * prop_pairs)
          #number of odd socks
          n_odd <- n_socks - n_pairs * 2
          
          # Simulating picking out n_picked socks
          socks <- rep(seq_len(n_pairs + n_odd), 
                       rep(c(2, 1), c(n_pairs, n_odd)))
          picked_socks <- sample(socks, size =  min(n_picked, n_socks))
          sock_counts <- table(picked_socks)
          
          # Returning the parameters and counts of the number of matched 
          # and unique socks among those that were picked out.
          sock_sim <- sum(sock_counts == 1)
          
          return(sock_sim)
        }
        # model generates num of paired and unique socks 
        # based on our prior 
        apply(priors(),1, function(x) gen_model(x[1],x[2]))
        # for each produced ith of the n pairs (total prior, prop prior)
        # run through gen_model to produce num of paired + unique socks
        # for each pair of priors 
      }
    )
    
    posterior = reactive(
      {
        priors()[sims()==input$n_odd,]
        # from pair of priors produced 
        # subset the ones where 
        # the num of unique socks our gen_model produced 
        # equals the num of unique socks we picked 
      }
    )
    
    output$total_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,1], freq=FALSE, 
             main="Total Socks in Laundry", 
             xlab = "Total Socks")
        lines(density(priors()$total), col='blue',lwd=2)
        abline(v = mean(posterior()[,1]), col = 'purple')
      }
    )
    
    output$prop_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,2], freq=FALSE, 
             main="Proportion of Socks in Pairs", 
             xlab = "% of Total Socks Part of a Pair")
        lines(density(priors()$prop), col='red',lwd=2)
        abline(v = mean(posterior()[,2]), col = 'orange')
      }
    )
  }
)