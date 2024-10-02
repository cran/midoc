## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(comment = NA)

## ----include=FALSE------------------------------------------------------------
library(midoc)

## ----eval=FALSE---------------------------------------------------------------
#  matage -> bmi7
#  mated -> matage
#  mated -> bmi7

## -----------------------------------------------------------------------------
descMissData(y="bmi7", 
             covs="matage mated", 
             data=bmi)

## ----eval=FALSE---------------------------------------------------------------
#  matage -> bmi7
#  mated -> matage
#  mated -> bmi7
#  sep_unmeas -> mated
#  sep_unmeas -> r

## ----eval=FALSE---------------------------------------------------------------
#  matage -> bmi7
#  mated -> matage
#  mated -> bmi7
#  mated -> r

## ----echo=FALSE, out.width="500px", out.height="400px", dpi=200---------------
plot(dagitty::dagitty('dag {
  bmi7 [pos="0,-0.5"]
  matage [pos="-2,-0.5"]
  mated [pos="-1,0.5"]
  sep_unmeas [pos="2,-1.5"]
  r [pos="2,-0.5"]
  mated -> bmi7 
  mated -> matage 
  matage -> bmi7 
  sep_unmeas -> mated 
  sep_unmeas -> r
  }'))

## -----------------------------------------------------------------------------
exploreDAG(mdag="matage -> bmi7 
                  mated -> matage 
                  mated -> bmi7 
                  sep_unmeas -> mated 
                  sep_unmeas -> r", 
           data=bmi)

## -----------------------------------------------------------------------------
checkCRA(y="bmi7", 
         covs="matage", 
         r_cra="r",
         mdag="matage -> bmi7 
               mated -> matage 
               mated -> bmi7 
               sep_unmeas -> mated 
               sep_unmeas -> r")

## -----------------------------------------------------------------------------
checkCRA(y="bmi7", 
         covs="matage mated", 
         r_cra="r",
         mdag="matage -> bmi7 
               mated -> matage 
               mated -> bmi7 
               sep_unmeas -> mated 
               sep_unmeas -> r")

## -----------------------------------------------------------------------------
checkCRA(y="bmi7", 
         covs="matage mated", 
         r_cra="r",
         mdag="matage -> bmi7 
               mated -> matage 
               mated -> bmi7 
               sep_unmeas -> mated 
               sep_unmeas -> r 
               bmi7 -> r")

## -----------------------------------------------------------------------------
descMissData(y="bmi7", 
             covs="matage mated pregsize bwt", 
             data=bmi)

## ----echo=FALSE, out.width="500px", out.height="400px", dpi=200---------------
plot(dagitty::dagitty('dag {
  bmi7 [pos="0,-0.5"]
  matage [pos="-2,-0.5"]
  mated [pos="-1,0.5"]
  r [pos="2,-0.5"]
  bwt [pos="1,-1"]
  pregsize [pos="0,-1.5"]
  sep_unmeas [pos="2,-1.5"]
  matage -> bmi7
  mated -> bmi7
  mated -> matage
  sep_unmeas -> mated
  sep_unmeas -> r
  pregsize -> bmi7
  pregsize -> bwt
  sep_unmeas -> bwt
  }'))

## -----------------------------------------------------------------------------
exploreDAG(mdag="matage -> bmi7 
                  mated -> matage 
                  mated -> bmi7 
                  sep_unmeas -> mated 
                  sep_unmeas -> r 
                  pregsize -> bmi7 
                  pregsize -> bwt  
                  sep_unmeas -> bwt", 
           data=bmi)

## ----R.options=list(width=80)-------------------------------------------------
checkCRA(y="bmi7", 
         covs="matage mated", 
         r_cra="r",
         mdag="matage -> bmi7 
               mated -> matage 
               mated -> bmi7 
               sep_unmeas -> mated 
               sep_unmeas -> r 
               pregsize -> bmi7 
               pregsize -> bwt  
               sep_unmeas -> bwt")

## ----R.options=list(width=80)-------------------------------------------------
checkMI(dep="bmi7", 
        preds="matage mated pregsize", 
        r_dep="r",
        mdag="matage -> bmi7 
              mated -> matage 
              mated -> bmi7 
              sep_unmeas -> mated 
              sep_unmeas -> r 
              pregsize -> bmi7 
              pregsize -> bwt  
              sep_unmeas -> bwt")

## ----R.options=list(width=80)-------------------------------------------------
checkMI(dep="bmi7", 
        preds="matage mated bwt", 
        r_dep="r",
        mdag="matage -> bmi7 
              mated -> matage 
              mated -> bmi7 
              sep_unmeas -> mated 
              sep_unmeas -> r 
              pregsize -> bmi7 
              pregsize -> bwt  
              sep_unmeas -> bwt")

## ----R.options=list(width=80)-------------------------------------------------
checkModSpec(formula="bmi7~matage+mated+pregsize", 
             family="gaussian(identity)", 
             data=bmi)

## ----R.options=list(width=80)-------------------------------------------------
checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize", 
             family="gaussian(identity)", 
             data=bmi)

## ----R.options=list(width=80)-------------------------------------------------
checkModSpec(formula="pregsize~matage+bmi7+mated", 
             family="binomial(logit)",
             data=bmi, 
             plot=FALSE)

## ----R.options=list(width=80)-------------------------------------------------
checkModSpec(formula="pregsize~matage+I(matage^2)+bmi7+mated", 
             family="binomial(logit)", 
             data=bmi)

## ----R.options=list(width=80)-------------------------------------------------
mimod_bmi7 <- checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize", 
                           family="gaussian(identity)", 
                           data=bmi,
                           message=FALSE)
miprop <- proposeMI(mimodobj=mimod_bmi7, 
                    data=bmi,
                    plotprompt=FALSE)

## ----eval=FALSE---------------------------------------------------------------
#  mimod_bmi7 <- checkModSpec(formula="bmi7~matage+I(matage^2)+mated+pregsize",
#                             family="gaussian(identity)",
#                             data=bmi,
#                             message=FALSE)
#  mimod_pregsize <- checkModSpec(formula="pregsize~bmi7+matage+I(matage^2)+mated",
#                                 family="binomial(logit)",
#                                 data=bmi,
#                                 message=FALSE)
#  proposeMI(mimodobj=list(mimod_bmi7, mimod_pregsize),
#            data=bmi)

## ----R.options=list(width=80)-------------------------------------------------
doMImice(miprop, seed=123, substmod="lm(bmi7 ~ matage + I(matage^2) + mated)")

## ----echo=FALSE---------------------------------------------------------------
results <- data.frame(approach="Full data",linest="1.17 (1.09-1.26)", quadest="0.86 (0.80-0.91)")
results[2,] <- c("CRA","1.16 (1.05-1.26)","0.84 (0.77-0.90)")
results[3,] <- c("MI fitting quadratic relationship, using pregnancy size","1.15 (1.05-1.25)","0.84 (0.78-0.91)")
results[4,] <- c("MI fitting quadratic relationship, using birth weight","1.16 (1.05-1.27)","0.83 (0.77-0.90)")
results[5,] <- c("MI fitting linear relationship, using pregnancy size","1.21 (1.07-1.34)","0.54 (0.46-0.62)")
results[6,] <- c("MI fitting linear relationship, using birth weight","1.20 (1.07-1.34)","0.53 (0.45-0.61)")

knitr::kable(results, caption = "Parameter estimates for maternal age",
             col.names=c("Approach","Linear term","Quadratic term"),
             align="lcc")

