# define required functions for power and ss calcs
library(epiR)
library(pwr)
library(powerSurvEpi)

# (FOR RCT)
prop_p <- function(myn, b, a, prop1, prop2){
  if (!list(NULL) %in% list(myn, a, prop1, prop2)){
    return(power.prop.test(n = myn, p1 = prop1/100, p2 = prop2/100, power = NULL, sig.level = a)$power)
  }
}
prop_n <- function(myn, b, a, prop1, prop2){
  if (!list(NULL) %in% list(prop1, prop2, a, b)){
    return(power.prop.test(n = NULL, p1 = prop1/100, p2 = prop2/100, power = b, sig.level = a)$n)
  }
}

# (FOR PAIRED DATA)
pair_p <- function(myn, b, a, mean, sd) {
  if (length(myn) == 1){return(power.t.test(n = myn, delta = mean/sd, power = NULL, sig.level = a)$power)}
  else if (length(myn) == 2){return(pwr.t2n.test(n1 = myn[1], n2 = myn[2], d = mean/sd, power = NULL, sig.level = a)$power)}
}

#SUBSET
testfn <- function(x, a, b, rat) {
  val <- pwr.t2n.test(x, NULL, 0.5, sig.level = a, power = b)$n2
  myrat <- x/val
  myrat <- round(myrat, 1)
  return(abs(myrat - rat))
}
#OP FUNCTION
getset <- function(a,b,mean,sd,k){
  testfn <- function(x) {
    val <- pwr.t2n.test(x, NULL, mean/sd, sig.level = a, power = b)$n2
    myrat <- x/val
    myrat <- round(myrat, 1)
    if (myrat > k){
      return(abs(myrat - k))
    }
    else return (100)
  }
  return(optimize(testfn, c(0,100000)))
}

pair_n <- function(myn, b, a, mean, sd) {
  if (length(myn) == 1){return(power.t.test(n = NULL, delta = mean/sd, power = b, sig.level = a)$n)}
  else if (length(myn) == 2){
    #set ratio
    k = round(myn[1]/myn[2], 1)
    if (k < 1){
      k = 1/k
      val2 <- getset(a,b,mean,sd,k)$minimum
      low <- ceiling(val2/k)
      return(c(low,low*k))
    }
    else {
      val2 <- getset(a,b,mean,sd,k)$minimum
      low <- ceiling(val2/k)
      return(c(low*k,low))
    }
  }
}

# (FOR TTE W/ PILOT)
pilot_p <- function(data, ne, nc, rr, a, b, k) powerCT(Surv(t,s)~i, data, nE = ne, nC = nc, RR = rr, alpha = a)$power
pilot_n <- function(data, ne, nc, rr, a, b, k) ssizeCT(Surv(t,s)~i, data, k = k, RR = rr, alpha = a, power = b)$ssize

# (FOR TTE W/PREVALENCE)
default_p <- function(ne, nc, pe, pc, rr, a, b, k) {
  if (!is.na(rr) && !is.null(rr)){
    HR2 <- rr
  }
  else {
    HR2 <- log(1-pe)/log(1-pc)
  }
  k <- ne/nc
  numer <- sqrt(ne+nc) * log(HR2)^2
  denom <- 1+k
  zb <- numer/denom - qnorm(1-a/2)
  return(1-pnorm(zb))
}
default_n <- function(ne, nc, pe, pc, rr, a, b, k) {
  if (!is.na(rr) && !is.null(rr)){
    HR2 <- rr
  }
  else {
    HR2 <- log(1-pe)/log(1-pc)
  }
  numer <- (qnorm(1-a/2)+qnorm(b))^2
  denom <- log(HR2)^2*(k/(1+k)^2)
  events <- ceiling(numer/denom)
  ss <- events/(pe*k+pc)
  return(c(events,ceiling(ss)))
}

# text options to select
calcs <- c("Sample Size", "Power")
datas <- c("Binary", "Continuous", "Timetoevent")

binarybig <- HTML("<div class = 'btext'>
                      <p class = 'btitle'>Binary</p>
                      <p class = 'subtitle'> (ex. Death, Hospital Discharge) </p>
                  </div>")
ctsbig <- HTML("<div class = 'btext'>
                      <p class = 'btitle'>Continuous</p>
                      <p class = 'subtitle'> (ex. Blood Pressure, Lab Values, Height) </p>
                  </div>")
ttebig <- HTML("<div class = 'btext'>
                      <p class = 'btitle'>Time-to-Event</p>
                      <p class = 'subtitle'> (ex. Time to Hospitalization) </p>
                  </div>")

binarys <- c("Randomized Trial")
bexps <- c(HTML("<p> Randomized Trial<sup>1</sup></p>"))

#output option
inputinvalid <- HTML("<div class = text-center><p class = danger>One or more inputs is invalid (see above)</p></div>")
