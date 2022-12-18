bis\_meta
================
Bowen
10/27/2022

The following code provides the generation of results (Including estimates, confidence interval, credible interval for mean estimates, heterogeneity measures, forest plot, subgroup analysis and funnel plot). (Mac Version) \#meta and bayesian hiersachical meta-analysis

``` r
library(meta)
```

    ## Loading 'meta' package (version 5.5-0).
    ## Type 'help(meta)' for a brief overview.
    ## Readers of 'Meta-Analysis with R (Use R!)' should install
    ## older version of 'meta' package: https://tinyurl.com/dt4y5drs

``` r
library(bayesmeta)
```

    ## Warning: package 'bayesmeta' was built under R version 4.0.5

    ## Loading required package: forestplot

    ## Loading required package: grid

    ## Loading required package: magrittr

    ## Warning: package 'magrittr' was built under R version 4.0.5

    ## Loading required package: checkmate

    ## Loading required package: metafor

    ## Loading required package: Matrix

    ## Loading required package: metadat

    ## Warning: package 'metadat' was built under R version 4.0.5

    ## 
    ## Loading the 'metafor' package (version 3.8-1). For an
    ## introduction to the package please type: help(metafor)

    ## Loading required package: mvtnorm

    ## Loading required package: numDeriv

    ## 
    ## Attaching package: 'bayesmeta'

    ## The following object is masked from 'package:stats':
    ## 
    ##     convolve

``` r
setwd("~/Desktop/bis_project")
### Combining the results from Black 2021
black2021 = read.csv("black2021.csv")
meta.black2021<-metagen(studlab = study, log(black2021$effect),sm = "RR", black2021$SE,data = black2021)
### Combining the results from Safford 2014
safford2014 = read.csv("safford2014.csv")
meta.safford2014<-metagen(studlab = study, log(safford2014$effect),sm = "RR", safford2014$SE,data = safford2014)
### The update one combine the cohort and case-control studies.v
bis = read.csv("bis_update.csv")

#11 cohort studies, the Black 2011 one has been split into three
meta.bis<-metagen(studlab = study, log(bis$effect),sm = "RR", bis$SE,data = bis)
meta.bis_type = update(meta.bis,subgroup = ifelse(stud_type == "co","Cohort","Case-control"))
meta.bis_type$subgroup.name = ""
forest.meta(meta.bis_type,print.Q = FALSE,print.I2 = TRUE,print.tau2 = FALSE,print.pval.Q = TRUE,sortvar = bis$year,leftcols = c("studlab"),rightcols=c("effect", "ci"),,smlab = NULL,rightlabs=c("Effect Size","95% CI"),comb.fixed = FALSE,overall.hetstat = FALSE,test.subgroup.common = FALSE,test.subgroup.random = FALSE,common = FALSE)
```

![](bis_files/figure-markdown_github/meta-analysis-1.png)

``` r
###Bayes Meta
##cohort
bis_co = read.csv("bis_co.csv")
meta.bis_co<-metagen(studlab = study, log(bis_co$effect),sm = "RR", bis_co$SE,data = bis_co)
x = bayesmeta(y = log(bis_co$effect),sigma = bis_co$SE,labels = meta.bis_co$studlab)
fp = forestplot.bayesmeta(x,expo = TRUE,shrinkage = FALSE,xlog=TRUE,heterogeneity = FALSE)
```

![](bis_files/figure-markdown_github/meta-analysis-2.png)

``` r
1-x$pposterior(mu=0) 
```

    ## [1] 0.9913324

``` r
1-x$pposterior(mu = log(1.2)) 
```

    ## [1] 0.9637208

``` r
1-x$pposterior(mu = log(1.5)) 
```

    ## [1] 0.81343

``` r
##case_control: the object
bis_cc = read.csv("bis_cc.csv")
meta.bis_cc<-metagen(studlab = study, log(bis_cc$effect),sm = "RR", bis_cc$SE,data = bis_cc)
#### Funnel Plot
funnel(meta.bis_cc,comb.fixed = FALSE)
```

![](bis_files/figure-markdown_github/meta-analysis-3.png)

``` r
#### Bayesian Meta analysis
t = bayesmeta(y = log(bis_cc$effect),sigma = bis_cc$SE,labels = meta.bis_cc$studlab)
fp2 = forestplot.bayesmeta(t,expo = TRUE,shrinkage = TRUE,xlog=TRUE,heterogeneity = FALSE,prediction = FALSE)
```

![](bis_files/figure-markdown_github/meta-analysis-4.png)

``` r
1-t$pposterior(mu=0) 
```

    ## [1] 0.9998805

``` r
1-t$pposterior(mu = log(1.2)) 
```

    ## [1] 0.9997534

``` r
1-t$pposterior(mu = log(1.5)) 
```

    ## [1] 0.9993768

``` r
### Publication Bias
metabias(meta.bis_cc,method = 'Begg')
```

    ## Rank correlation test of funnel plot asymmetry
    ## 
    ## Test result: z = 0.41, p-value = 0.6808
    ## 
    ## Sample estimates:
    ##      ks   se.ks
    ##  6.0000 14.5831
    ## 
    ## - reference: Begg & Mazumdar (1993), Biometrics

``` r
metabias(meta.bis_cc,method = 'Egger')
```

    ## Linear regression test of funnel plot asymmetry
    ## 
    ## Test result: t = 3.91, df = 10, p-value = 0.0029
    ## 
    ## Sample estimates:
    ##    bias se.bias intercept se.intercept
    ##  3.4047  0.8718    0.3257       0.1767
    ## 
    ## Details:
    ## - multiplicative residual heterogeneity variance (tau^2 = 5.7401)
    ## - predictor: standard error
    ## - weight:    inverse variance
    ## - reference: Egger et al. (1997), BMJ

``` r
metabias(meta.bis_co,method = 'Begg')
```

    ## Rank correlation test of funnel plot asymmetry
    ## 
    ## Test result: z = 0.61, p-value = 0.5418
    ## 
    ## Sample estimates:
    ##       ks   se.ks
    ##  10.0000 16.3911
    ## 
    ## - reference: Begg & Mazumdar (1993), Biometrics

``` r
metabias(meta.bis_co,method = 'Egger')
```

    ## Linear regression test of funnel plot asymmetry
    ## 
    ## Test result: t = 0.41, df = 11, p-value = 0.6872
    ## 
    ## Sample estimates:
    ##    bias se.bias intercept se.intercept
    ##  0.3308  0.8002    0.4814       0.1320
    ## 
    ## Details:
    ## - multiplicative residual heterogeneity variance (tau^2 = 4.2417)
    ## - predictor: standard error
    ## - weight:    inverse variance
    ## - reference: Egger et al. (1997), BMJ

## Essentially, the posterior probability that the Bayesian estimates (aOR) is greater than 1, 1.2 and 1.5 is as follows:

Calculation of *P*(*μ* &gt; 0|*y*, *σ*) is equivalent to *P*(*R**R* &gt; 1|*y*, *σ*). Calculation of *P*(*μ* &gt; *l**o**g*(1.1)|*y*, *σ*) is equivalent to *P*(*R**R* &gt; 1.1|*y*, *σ*). Calculation of *P*(*μ* &gt; *l**o**g*(1.2)|*y*, *σ*) is equivalent to *P*(*R**R* &gt; 1.2|*y*, *σ*).
