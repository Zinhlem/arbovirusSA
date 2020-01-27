#Article: Statics and dynamics of malaria infection in Anopheles mosquitoes
#AUthors: David L Smith and Ellis McKenzie (2004)

#Statics of mosquito populations

##parameters:
params <- c(g = ???, #g force of mortality
            A = ??, #age A
            f = ???, # mosquito feeding rate
            Q = ??, #proportion of bites taken from humans
            c = ??, #prob that mosquito becomes infcted
            n = ??, ##incubation period (ingesting gametocytes to being infectious)
            b = ???, #prob that human becomes infected
            E = ??? #constant mosq daily emergence rate
                    )

##Initial pop of humans and mosquitoes
init <- c(X = ?? #prop of infectious humans
         )

#prob that mosquito survies one day 
survive.one.day <- exp(-g) #p prob that mosq. survives one day

#OR 
survive.one.day <- (-log(p)) # g force of mortality

# prop of mosquitoes that survive to age A:
lambda <- exp(-g * A)

# average lifespan (force of mortality)
lifespan <- (1 / g)


#a: expected number of bites on humnas per mosquto oer day 
a <- Q*f

#S: Stability index (SI); 
SI <- a/g


#prop of surviving mosquitoes of age A that has ever bitten a human is (nu(A))
nu.A <- 1 - exp(-a*A)

## prop of mosq that survive to age A and have bitten a human is nu(A)*lambda(A)

nu.lambda.A <- a/(a + g)

#prop of surviving moaquitoes of age A that have ever become infected is:
 v <- 1 - exp(-a*A*c*X)

 # prop of ing=fected mosquitoes (Y) is:
 
 Y <- a*c*X/(g + a*c*X)

 
 #only mosquitoes that survived >= n days can be infectious
 #prob of surviving n days is :
 P.e <- exp(-g*n)

 #prop of mosquiotoes of age A that are infectious is:
 mu.A <- 1 - exp(-1*c*X(A-n))  # for A > n
 
 #prob that a mosquito ever becomes infectious (Z) is 
 mosq.infectious <- (a*c*X / (g + a*c*X))*exp(-g*n)
 
 
 #beta: lifetime transmission: product of the prob that a mosquito becomes infected
  beta <- (a^2)*b*c*X*exp(-gn)/(g*(g+a*c*X))

 
  #individual vectorial capacity (IC): expected # of infectious bites from a single vector after feeding on an infectious host
  IC <- (a*c*exp(-g*n))/g

 
  #EIR is the number of infectious bites received per day by a human
  
  #eq mosquito density per human (m)
  m <- E*g

  
  #EIR = maZ = Ebeta: a fxn of the prop of humans who are infectious 
  EIR <- (m*(a^2)*c*X*exp(-g*n)/ (g + a*c*X))

  # transmission potential of mosquitoes (vectorial capacity, C)
  #C: exepected # of humans infected per infected human per day assuming perfect transmission (b=c=1)
  C <- (m*(a^2)*exp(-g*n))/g
 
  
  