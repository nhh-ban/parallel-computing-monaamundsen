simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 

######Rewriting the function MTweedieTests######
maxcores <- 8
Cores <- min(parallel::detectCores(), maxcores)
cl <- makeCluster(Cores)
registerDoParallel(cl)

MTweedieTests <- function(N, M, sig, Cores) {
  results <- foreach(i = 1:(M/Cores), 
                     .combine = c, 
                     .multicombine = TRUE, 
                     .packages = c("tweedie"), 
                     .export = c("simTweedieTest")) %dopar% {
    replicate(Cores, simTweedieTest(N) < sig)
  }
  sum(unlist(results)) / M
}

df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 


for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05,
      Cores = Cores) 
} 

simDat <-
  function(N, type, mu) {
    if (type == "tweedie") {
      return(rtweedie(
        N,
        mu = mu,
        phi = 100,
        power = 1.9
      ))
    }
    if (type == "normal") {
      return(rnorm(N, mean = mu))
    }
    else{
      stop("invalid distribution")
    }
  }


simTest <-
  function(N, type, mu) {
    t.test(simDat(N = N,
                  type = type,
                  mu = mu),
           mu = mu)$p.value
  }

MTests <-
  function(N, M, type, sig) {
    sum(replicate(M,
                  simTest(
                    N = N,
                    type =
                      type,
                    mu =
                      10000
                  )) < sig) / M
  }


df <-
  expand.grid(
    N = c(10, 100, 1000, 5000),
    M = 1000,
    type = c("tweedie", "normal"),
    share_reject = NA
  ) %>%
  as_tibble()


for (i in 1:nrow(df)) {
  print(i)
  df$share_reject[i] <-
    MTests(df$N[i],
           df$M[i],
           df$type[i],
           .05)
}

df %>%
  ggplot2::ggplot(aes(x = log(N), y = share_reject, col = type)) +
  geom_line() +
  geom_hline(yintercept = .05) +
  theme_bw() 

stopCluster(cl)
