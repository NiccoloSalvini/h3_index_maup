#  https://github.com/Rise-group/S-maup_data/blob/master/run_smaup/testSmaup.R
#  from the paper: S-maup: Statistical test to measure the sensitivity to the modifiable areal unit problem

# va riadattato al problema specifico
# passalo su chatgpt
testSmaup <- function(N, k, rhoEst){

  CV0_01 <- matrix(c(NA,25,100,225,400,625,900,
                     -0.9,0.83702,0.09218,0.23808,0.05488,0.07218,0.02621,
                     -0.7,0.83676,0.16134,0.13402,0.06737,0.05486,0.02858,
                     -0.5,0.83597,0.16524,0.13446,0.06616,0.06247,0.02851,
                     -0.3,0.83316,0.19276,0.13396,0.0633,0.0609,0.03696,
                     0,0.8237,0.17925,0.15514,0.07732,0.07988,0.09301,
                     0.3,0.76472,0.23404,0.2464,0.11588,0.10715,0.0707,
                     0.5,0.67337,0.28921,0.25535,0.13992,0.12975,0.09856,
                     0.7,0.52155,0.47399,0.29351,0.23923,0.20321,0.1625,
                     0.9,0.28599,0.28938,0.4352,0.4406,0.34437,0.55967),
                   nrow = 10, ncol = 7, byrow = TRUE)

  CV0_05 <- matrix(c(NA,25,100,225,400,625,900,
                     -0.9,0.83699,0.08023,0.10962,0.04894,0.04641,0.02423,
                     -0.7,0.83662,0.12492,0.08643,0.059,0.0428,0.02459,
                     -0.5,0.83578,0.13796,0.08679,0.05927,0.0426,0.02658,
                     -0.3,0.78849,0.16932,0.08775,0.05464,0.04787,0.03042,
                     0,0.81952,0.15746,0.11126,0.06961,0.06066,0.05234,
                     0.3,0.70466,0.21088,0.1536,0.09766,0.07938,0.06461,
                     0.5,0.59461,0.23497,0.18244,0.11682,0.10129,0.0886,
                     0.7,0.48958,0.37226,0.2228,0.2054,0.16144,0.14123,
                     0.9,0.2158,0.22532,0.27122,0.29043,0.23648,0.31424),
                   nrow = 10, ncol = 7, byrow = TRUE)
  CV0_10 <- matrix(c(NA, 25,100,225,400,625,900,
                     -0.9,0.69331,0.06545,0.07858,0.04015,0.03374,0.02187,
                     -0.7,0.79421,0.09566,0.06777,0.05058,0.03392,0.02272,
                     -0.5,0.689,0.10707,0.07039,0.05151,0.03609,0.02411,
                     -0.3,0.73592,0.14282,0.07076,0.04649,0.04001,0.02614,
                     0,0.71632,0.13621,0.08801,0.06112,0.04937,0.03759,
                     0.3,0.63718,0.18239,0.12101,0.08324,0.06347,0.05549,
                     0.5,0.46548,0.17541,0.14248,0.10008,0.08137,0.07701,
                     0.7,0.3472,0.28774,0.1817,0.16442,0.13395,0.12354,
                     0.9,0.1764,0.18835,0.21695,0.23031,0.19435,0.22411),
                   nrow = 10, ncol = 7, byrow = TRUE)

  if (N < 25){
    N <- 25
    warning("Warning: Please treat this result with caution because the computational experiment in this paper include, so far, values of N from 25 to 900.")
  }else if (N > 900){
    N <- 900
    warning("Warning: Please treat this result with caution because the computational experiment in this paper include, so far, values of N from 25 to 900.")
  }
  theta <- k/N
  b <- -2.2
  m <- 7.03
  L <- 1/(1+(exp(b+theta*m)))
  p <- exp(-0.6618)
  a <- 1.3
  eta <- p*(theta**(a))
  b0 <- 5.32
  b1 <- -5.53
  tau <- (theta*b1)+b0
  Smaup <- L/(1+eta*(exp(rhoEst*tau)))

  if ((0.8 < rhoEst) && (rhoEst < 1.0)){
    r <- 0.9
  }else if ((0.6 < rhoEst) && (rhoEst < 0.8)){
    r <- 0.7
  }else if ((0.4 < rhoEst) && (rhoEst < 0.6)){
    r <- 0.5
  }else if ((0.15 < rhoEst) && (rhoEst < 0.4)){
    r <- 0.3
  }else if ((-0.15 < rhoEst) && (rhoEst < 0.15)){
    r <- 0
  }else if ((-0.4 < rhoEst) && (rhoEst < -0.15)){
    r <- -0.3
  }else if ((-0.6 < rhoEst) && (rhoEst < -0.4)){
    r <- -0.5
  }else if ((-0.8 < rhoEst) && (rhoEst < -0.6)){
    r <- -0.7
  }else {
    r <- -0.9
  }
  Crit_val0_01 <- approxfun(c(25,100,225,400,625,900), CV0_01[c(FALSE, CV0_01[2:10,1] == r),2:7])
  Crit_val0_05 <- approxfun(c(25,100,225,400,625,900), CV0_05[c(CV0_05[2:10,1]) == r,2:7])
  Crit_val0_10 <- approxfun(c(25,100,225,400,625,900), CV0_01[c(CV0_10[2:10,1]) == r,2:7])

  if (Smaup > Crit_val0_01(N)){
    cat(" ================================\n",
        paste0("Statistic M = ", Smaup), "\n",
        "Pseudo p_value < 0.01 ***\n",
        "H0 is rejected\n",
        "================================\n")
  }else if (Smaup > Crit_val0_05(N)){
    cat(" ================================\n",
        paste0("Statistic M = ", Smaup), "\n",
        "Pseudo p_value < 0.05 **\n",
        "H0 is rejected\n",
        "================================\n")
  }else if (Smaup > Crit_val0_10(N)){
    cat(" ================================\n",
        paste0("Statistic M = ", Smaup), "\n",
        "Pseudo p_value < 0.10 *\n",
        "H0 is rejected\n",
        "================================\n")
  }else{
    cat(" ================================\n",
        paste0("Statistic M = ", Smaup), "\n",
        "Pseudo p_value > 0.10 *\n",
        "H0 is not rejected\n",
        "================================\n")
  }
  return(Smaup)
}

N = 1000
k = 90
rhoEst = 0.801
testSmaup(N, k, rhoEst)
