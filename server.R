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
        n_picked <- input$n_odd1 + 2*input$n_pairs1
        #Total socks in laundry
        n_socks <- priors_total()
        #Proportion of socks in pairs
        prop_pairs <- priors_prop()
        #number of sock pairs
        n_pairs <- round(floor(n_socks / 2) * prop_pairs)
        #number of odd socks
        n_odd <- n_socks - n_pairs * 2
        dataS <- data.frame(n_pairs, n_odd, n_socks)
        # data frame 
        # first column is number of sock pairs in washer 
        # second column is number of unique socks in washer 
        # third column is total number of socks in washer 
        # generated earlier 
        
        # Simulating picking out n_picked socks
        socks <- function(x){
          # take in vector of three numbers 
          # first number is nubmer of sock pairs in washer
          # second number is number of unique socks in washer
          # third number is total number of socks in washer
          list_sim <- rep(seq_len(x[1] + x[2]), 
                          rep(c(2,1), c(x[1], x[2])))
          # generate sequence to represent situation inside washer 
          picked_socks <- sample(list_sim, size = min(n_picked, x[3]))
          # from socks inside washer, take a sample 
          # sample size is either the sample size you determined earlier 
          # or the total number of socks inside washer
          # if you want to pick out 5 socks, but you only have 3 socks in washer
          # you pick out 3 socks 
          # if you want to pick out 5 socks an washer has 7 socks
          # you pick out 5 socks
          sock_counts <- table(picked_socks)
          sock_uniq <- sum(sock_counts == 1)
          # how many unique socks we have picked out
          sock_pair <- sum(sock_counts == 2)
          # how many sock pairs we have picked out
          sock_sim <- c(sock_uniq, sock_pair)
          return(sock_sim)
        }
        if(input$n_sims <= 5000){
        sock_sit <- apply(dataS, 1, socks)
        }
        else{
          t = floor(input$n_sims/4)
          x = list(dataS[1:t, ],dataS[(t+1):(2*t),], 
                   dataS[((2*t)+1):(3*t),],dataS[((3*t)+1):input$n_sims,])
          apply_func <- function(x){
            apply(x,1,socks)
          }
          y <- mclapply(x,apply_func, mc.cores = 4)
          sock_sit <- cbind(y[[1]], y[[2]], y[[3]], y[[4]])
        }
        # for each of the n simulated data 
        # the number of unique socks picked out
        # the number of sock pairs picked out
        sock_sit
      }
    )
    
    posterior_N = reactive(
      {
        priors_total()[sims()[1,]==input$n_odd1 & sims()[2,] == input$n_pairs1]
        # from n generated total number of socks in washer 
        # subset out the ones where the data simulated 
        # has the same number of odds picked 
        # same number of pairs picked 
        
      }
    )
    
    posterior_p = reactive(
      {
        priors_prop()[sims()[1,]==input$n_odd1 & sims()[2,] == input$n_pairs1]
        # from n generated total number of socks in washer 
        # subset out the ones where the data simulated 
        # has the same number of odds picked 
        # same number of pairs picked 
        
      }
    )
    
    output$post_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1), mfrow = c(2,2), cex.main = 0.8)
        hist(posterior_N(), freq=FALSE,
             main="Posterior - Total Socks in Laundry",
             xlab = "Total Socks",
             ylab = NULL)
        lines(density(posterior_N()), col='blue',lwd=2, 
              xlab = NULL,
              ylab = NULL)
        abline(v = mean(posterior_N()), col = 'orange')
        abline(v = median(posterior_N()), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
        hist(posterior_p(), freq=FALSE,
             main="Posterior - Proportion of Socks in Pairs",
             xlab = "% of Total Socks Part of a Pair",
             ylab = NULL)
        lines(density(posterior_p()), col='blue',lwd=2, 
              xlab = NULL,
              ylab = NULL)
        abline(v = mean(posterior_p()), col = 'orange')
        abline(v = median(posterior_p()), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
        hist(sims()[1,], freq = FALSE, 
             main = "Posterior - Odd Socks", 
             xlab = "# of Unique Socks", 
             ylab = NULL)
        lines(density(sims()[1,]), col = 'blue', lwd = 2, 
              xlab = NULL, 
              ylab = NULL)
        abline(v = mean(sims()[1,]), col = 'orange')
        abline(v = median(sims()[1,]), col = 'green')
        legend("topleft", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
        hist(sims()[2,], freq = FALSE, 
             main = "Posterior - Sock Pairs", 
             xlab = "# of Pairs", 
             ylab = NULL)
        lines(density(sims()[2,]), col = 'blue', lwd = 2, 
              xlab = NULL, 
              ylab = NULL)
        abline(v = mean(sims()[2,]), col = 'orange')
        abline(v = median(sims()[2,]), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
      }
    )
    
    output$prior_plot = renderPlot(
      {
        n_socks <- priors_total()
        prop_pairs <- priors_prop()
        n_pairs <- round(floor(n_socks / 2) * prop_pairs)
        n_odd <- n_socks - n_pairs * 2
        par(mar=c(4,4,4,0.1), mfrow = c(2,2), cex.main = 0.8)
        
        hist(priors_total(), freq=FALSE,
             main="Prior - Total Socks in Laundry",
             xlab = "Total Socks",
             ylab = NULL)
        lines(density(priors_total()), col='red',lwd=2, 
              xlab = NULL,
              ylab = NULL)
        abline(v = mean(priors_total()), col = 'orange')
        abline(v = median(priors_total()), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
        hist(priors_prop(), freq=FALSE,
             main="Prior - Proportion of Socks in Pairs",
             xlab = "% of Total Socks Part of a Pair",
             ylab = NULL)
        lines(density(priors_prop()), col='red',lwd=2, 
              xlab = NULL,
              ylab = NULL)
        abline(v = mean(priors_prop()), col = 'orange')
        abline(v = median(priors_prop()), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
        hist(n_pairs, freq = FALSE, 
             main = "Prior - Odd Socks", 
             xlab = "# of Unique Socks", 
             ylab = NULL)
        lines(density(n_pairs), col = 'red', lwd = 2, 
              xlab = NULL, 
              ylab = NULL)
        abline(v = mean(n_pairs), col = 'orange')
        abline(v = median(n_pairs), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
        hist(n_odd, freq = FALSE, 
             main = "Prior - Sock Pairs", 
             xlab = "# of Pairs", 
             ylab = NULL)
        lines(density(n_odd), col = 'red', lwd = 2, 
              xlab = NULL, 
              ylab = NULL)
        abline(v = mean(n_odd), col = 'orange')
        abline(v = median(n_odd), col = 'green')
        legend("topright", c("Mean","Median"), lty=c(1,1), lwd=c(2.5,2.5),col=c("orange","green"))
      }
    )
    output$summary_table <- renderTable(
      {
       x <- rbind(summary(posterior_N()),
                  summary(posterior_p()),
                  summary(sims()[1,]),
                  summary(sims()[2,]))
       rownames(x) <- c("Posterior Total Socks", "Posterior Proportion Paired",
                        "Posterior Odd Socks", "Posterior Paired Socks")
       x
      }
    )
    output$credible_table <- renderTable(
      {
        y <- rbind(quantile(posterior_N(), probs = c(0.025, 0.975)),
                   quantile(posterior_p(), probs = c(0.025, 0.975)),
                   quantile(sims()[1,], probs = c(0.025, 0.975)),
                   quantile(sims()[2,], probs = c(0.025, 0.975)))
        rownames(y) <- c("Posterior Total Socks", "Posterior Proportion Paired",
                         "Posterior Odd Socks", "Posterior Paired Socks")
        y
      }
    )
    
    output$True_Result <- renderTable(
      {
        y <- data.frame(21,3)
        colnames(y) <- c("Number of Pairs", "Number of Singletons")
        rownames(y) <- c("True Values")
        y
      }
    )
  }
)

