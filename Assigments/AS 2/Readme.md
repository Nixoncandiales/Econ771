Assigment 2 - Empirical Exercise
================
Due: Friday, October 14

# Overview

In this assignment, we’re going to work through some applied issues
related to instrumental variables. For a long time, IV (or 2SLS) was a
very common identification strategy for applied empirical micro, but it
fell out of favor as people became more aware of the assumptions
underlying the estimator and better understood what IV actually
estimates (not the ATE in most cases). People also started to find other
strategies that were more compelling in some applications (and of course
with some other assumptions). In this assignment, we’re going to study
the effects of a physician’s affiliation with a hospital on physician
practice patterns, and we’ll instrument for physician affiliation using
some specific Medicare payment shocks.

Please “submit” your answers as a GitHub repository link. In this repo,
please include a final document with your main answers and analyses in a
PDF. Be sure to include in your repository all of your supporting code
files. Practice writing good code and showing me only what I would need
to recreate your results.

# Resources and data

The data for this assignment comes from three sources:

1.  [MD-PPAS](https://resdac.org/cms-data/files/md-ppas); The Medicare
    Data on Provider Practice and Specialty includes data on physician
    specialties, practice IDs, demographics, and place of service. Be
    sure to follow the link and read the data documentation. We’ll use
    these data to construct a measure of physician integration

2.  [Medicare Utilization and Payment
    Data](https://data.cms.gov/provider-summary-by-type-of-service/medicare-physician-other-practitioners/medicare-physician-other-practitioners-by-provider-and-service):
    These files provide data on the quantities and Medicare spending of
    each physician and service. We’ll use these data to construct an
    instrument for our integration measure and to capture total
    physician-level billing activity.

3.  [Physician Fee Schedule 2010
    Update](https://github.com/imccart/PFS_Update_2010): Our instrument
    mainly consists of a shock to physician payments introduced in 2010.
    The shock further increased payments for services in an outpatient
    facility compared to services billed in a physician’s office. The
    GitHub repo (linked above) provides code to recreate a dataset with
    service-specific price shocks introduced by the 2010 fee schedule
    update.

# Questions

In your GitHub repository, please be sure to clearly address/answer the
following questions.

1.  Provide and discuss a table of simple summary statistics showing the
    mean, standard deviation, min, and max of total physician-level
    Medicare spending, claims, and patients.

2.  Form a proxy for integration using the ratio: where $HOPD_{it}$
    reflects the total number of claims in which physician $i$ bills in
    a hospital outpatient setting, $OFFICE_{it}$ is the total number of
    claims billed to an office setting, and $ASC_{it}$ is the total
    number of claims billed to an ambulatory surgery center. Plot the
    mean of total physician-level claims for integrated versus
    non-integrated physicians over time (just pick a few thresholds for
    defining “integration”).

3.  Estimate the relationship between integration on total physician
    claims using OLS, with the following specification: where $INT_{it}$
    is defined in Equation @ref(eq:int), $x_{it}$ captures time-varying
    physician characteristics, and $\gamma_{i}$ and $\gamma_{t}$ denote
    physician and time fixed effects.

4.  How much should we be “worried” about endogeneity here? Extending
    the work of @altonji2005, @oster2019 derives the expression where
    $x_{1}$ captures our observable covariates; $\delta$ denotes the
    treatment effect of interest; $\hat{\delta}_{D,x_{1}}$ denotes the
    coefficient on $D$ from a regression of $y$ on $D$ and $x_{1}$;
    $R_{D,x_{1}}^{2}$ denotes the $R^{2}$ from that regression;
    $\hat{\delta}_{D}$ denotes the coefficient on $D$ from a regression
    of $y$ on $D$ only; $R_{D}^{2}$ reflects the $R^{2}$ from that
    regression; $R_{max}^{2}$ denotes an unobserved “maximum” $R^{2}$
    from a regression of $y$ on $D$, observed covariates $x_{1}$, and
    some unobserved covariates $x_{2}$; and $\rho$ denotes the degree of
    selection on observed variables relative to unobserved variables
    $$\delta \times \frac{Cov(W_{1},X)}{Var(W_{1})} = \frac{Cov(W_{2},X)}{Var(W_{2})}.$$
    Construct the value in Equation @ref(eq:oster) based on all
    combinations of $\rho \in (0, .5, 1, 1.5, 2)$ and
    $R_{max}^{2} \in (0.5, 0.6, 0.7, 0.8, 0.9, 1)$ and present your
    results in a table. What do your results say about the extent to
    which selection on observables could be problematic here? Hint: you
    can also look into `psacalc` in `Stata` or `robomit` in `R` for
    implementation of @oster2019 in `Stata` or `R`, respectively.

5.  Construct the total change in Medicare payments achievable for an
    integrated versus non-integrated physician due to the 2010 update to
    the physician fee schedule, $\Delta P_{it}$. Use this as an
    instrument for $INT_{it}$ in a 2SLS estimator following the same
    specification as in Equation @ref(eq:ols). Present your results
    along with those of your “first stage” and “reduced form”.

6.  Assess the “need” for IV by implementing a Durbin-Wu-Hausman test
    with an augmented regression. Do this by first estimating the
    regression,
    $INT_{it} = \lambda \Delta P_{it} + \beta x_{it} + \gamma_{i} + \gamma_{t} + \varepsilon_{it}$,
    take the residual $\hat{\nu} = INT_{it} - \hat{INT}_{it}$, and run
    the regression
    $$y_{it} = \delta INT_{it} + \beta x_{it} + \gamma_{i} + \gamma_{t} + \kappa \hat{\nu} + \varepsilon_{it}.$$
    Discuss your results for $\hat{\kappa}$.

7.  Now let’s pay attention to potential issues of weak instruments. As
    we discussed in class, one issue with weak instruments is that our
    typical critical values (say, 1.96 for a 95% confidence interval)
    from the equation of interest (sometimes called the structural
    equation) are too low in the presence of a weak first-stage. These
    issues are presented very clearly and more formally in the Andrews,
    Stock, and Sun (2019) survey article. For this question, you will
    consider two forms of inference in the presence of weak instruments:

    -   Present the results of a test of the null, $H_{0}: \delta=0$,
        using the Anderson-Rubin Wald statistic. Do your conclusions
        from this test differ from a traditional t-test following 2SLS
        estimation of Equation @ref(eq:ols)?
    -   Going back to your 2SLS results…inflate your 2SLS standard
        errors to form the $tF$ adjusted standard error, following Table
        3 in Lee et al. (2021). Repeat the test of the null,
        $H_{0}: \delta=0$, using standard critical values and the $tF$
        adjusted standard error.

8.  Following the Borusyak and Hull (2021) working paper (BH), we can
    consider our instrument as a function of some exogenous policy
    shocks and some possibly endogenous physician characteristics,
    $\Delta P_{it}=f\left(g_{pt}; z_{ipt}\right)$, where $g_{pt}$
    captures overall payment shocks for procedure $p$ at time $t$, and
    $z_{ip}$ denotes a physician’s quantity of different procedures at
    baseline. We can implement the BH re-centering approach as follows:

    -   Consider hypothetical price changes over a set of possible
        counterfactuals by assuming that the counterfactuals consist of
        different allocations of the observed relative price changes.
        For example, take the vector of all relative price changes,
        reallocate this vector randomly, and assign new hypothetical
        relative price changes. Do this 100 times. This isn’t “all”
        possible counterfactuals by any means, but it will be fine for
        our purposes.
    -   Construct the expected revenue change over all possible
        realizations from previously,
        $\mu_{it} = E [\Delta P_{it}]= \sum_{s=1}^{100} \sum_{p} g_{pt}^{s} z_{ip}$.
    -   Re-estimate Equation @ref(eq:ols) by 2SLS when instrumenting for
        $INT_{it}$ with
        $\tilde{\Delta} P_{it} = \Delta P_{it} - \mu_{it}$. Intuitively,
        this re-centering should isolate variation in the instrument
        that is only due to the policy and remove variation in our
        instrument that is due to physician practice styles (the latter
        of which is not a great instrument).

9.  Discuss your findings and compare estimates from different
    estimators.

10. Reflect on this assignment. What did you find most challenging? What
    did you find most surprising?

# References
