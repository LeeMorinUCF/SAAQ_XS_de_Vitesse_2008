# Validation of Fixed Effects and Cluster-Robust Variance Estimation

This is a comparison of the estimates of the fixed effects model
with cluster-robust standard errors. 

It calculates the estimates with a dataset with weight one on each observation
and estimates the model in Stata. 


Next, the data are aggregated in R, so that each distinct observation has only one line
in the dataset and the observations are weighted by frequency. 
For example, when a driver with three demerit points avoids getting a ticket
for 200 consecutive days, the observation with a zero event appears only once
in the dataset and has a fequency weight of 200.
In the actual dataset, this functionality reduces the size of the dataset
by a factor of 1000. 


## Estimates from the ```xtreg ... , fe vce(cluster seq)``` Command in Stata

These are the estimates using the full, unweighted dataset in Stata
(Stata does not allow for frequency weights that vary across individuals
in a panel):

```
. xtreg events_int i.curr_pts_grp_cat##policy_int, fe vce(cluster seq)

Fixed-effects (within) regression               Number of obs     =  5,113,500
Group variable: seq                             Number of groups  =      3,500

R-sq:                                           Obs per group:
     within  = 0.0220                                         min =      1,461
     between = 0.0124                                         avg =    1,461.0
     overall = 0.0220                                         max =      1,461

                                                F(27,3499)        =    5584.21
corr(u_i, Xb)  = 0.0002                         Prob > F          =     0.0000

                                               (Std. Err. adjusted for 3,500 clusters in seq)
---------------------------------------------------------------------------------------------
                            |               Robust
                 events_int |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
----------------------------+----------------------------------------------------------------
           curr_pts_grp_cat |
                         1  |  -.0785287   .0002266  -346.53   0.000     -.078973   -.0780844
                        10  |  -.0785291   .0002337  -336.01   0.000    -.0789873   -.0780709
                     11-20  |  -.0786755    .000243  -323.82   0.000    -.0791518   -.0781991
                         2  |  -.0785199   .0002263  -346.92   0.000    -.0789636   -.0780761
                     21-30  |  -.0783958   .0002586  -303.15   0.000    -.0789029   -.0778888
                         3  |  -.0785566   .0002265  -346.83   0.000    -.0790007   -.0781125
                    31-150  |  -.0788325   .0004935  -159.76   0.000       -.0798   -.0778651
                         4  |  -.0785107   .0002268  -346.18   0.000    -.0789553    -.078066
                         5  |  -.0785293   .0002279  -344.65   0.000    -.0789761   -.0780826
                         6  |  -.0785093   .0002288  -343.14   0.000    -.0789578   -.0780607
                         7  |  -.0784716   .0002285  -343.43   0.000    -.0789196   -.0780236
                         8  |  -.0785053   .0002277  -344.78   0.000    -.0789517   -.0780588
                         9  |  -.0785376   .0002314  -339.46   0.000    -.0789912    -.078084
                            |
               1.policy_int |  -.0277544   .0002536  -109.44   0.000    -.0282517   -.0272572
                            |
curr_pts_grp_cat#policy_int |
                       1#1  |   .0277601   .0002554   108.69   0.000     .0272594    .0282609
                      10#1  |    .027789   .0002684   103.52   0.000     .0272627    .0283153
                   11-20#1  |   .0279476   .0002796    99.94   0.000     .0273994    .0284959
                       2#1  |   .0277422   .0002552   108.71   0.000     .0272418    .0282425
                   21-30#1  |   .0275644   .0003225    85.47   0.000     .0269321    .0281967
                       3#1  |    .027774   .0002549   108.98   0.000     .0272743    .0282737
                  31-150#1  |   .0278545   .0006719    41.46   0.000     .0265372    .0291718
                       4#1  |   .0276892   .0002554   108.42   0.000     .0271884    .0281899
                       5#1  |   .0277546   .0002571   107.94   0.000     .0272505    .0282588
                       6#1  |   .0277697   .0002557   108.62   0.000     .0272685     .028271
                       7#1  |   .0277122   .0002586   107.15   0.000     .0272052    .0282193
                       8#1  |   .0277276   .0002605   106.46   0.000     .0272169    .0282382
                       9#1  |   .0277848   .0002614   106.28   0.000     .0272723    .0282974
                            |
                      _cons |   .0785278   .0001416   554.39   0.000     .0782501    .0788055
----------------------------+----------------------------------------------------------------
                    sigma_u |  .00722877
                    sigma_e |  .20845358
                        rho |  .00120112   (fraction of variance due to u_i)
---------------------------------------------------------------------------------------------
```



## Estimates from the ```FE_CRVE_lib.R``` Library in R

Compare these to the estimates using the frequency-weighted dataset in R.

```
                            Estimate   Std. Error     t value      Pr(>|t|)
(Intercept)             1.847082e-17 3.632208e-18    5.085288  3.864948e-07
dev_policy             -2.775443e-02 2.536092e-04 -109.437785  0.000000e+00
curr_pts_1             -7.852867e-02 2.266175e-04 -346.525233  0.000000e+00
curr_pts_2             -7.851986e-02 2.263332e-04 -346.921533  0.000000e+00
curr_pts_3             -7.855660e-02 2.265002e-04 -346.827952  0.000000e+00
curr_pts_4             -7.851068e-02 2.267930e-04 -346.177672  0.000000e+00
curr_pts_5             -7.852935e-02 2.278513e-04 -344.651812  0.000000e+00
curr_pts_6             -7.850925e-02 2.287954e-04 -343.141732  0.000000e+00
curr_pts_7             -7.847165e-02 2.284965e-04 -343.426087  0.000000e+00
curr_pts_8             -7.850526e-02 2.276971e-04 -344.779320  0.000000e+00
curr_pts_9             -7.853761e-02 2.313617e-04 -339.458093  0.000000e+00
curr_pts_10            -7.852910e-02 2.337090e-04 -336.012236  0.000000e+00
curr_pts_11_20         -7.867547e-02 2.429595e-04 -323.821293  0.000000e+00
curr_pts_21_30         -7.839584e-02 2.586044e-04 -303.149617  0.000000e+00
curr_pts_31_150        -7.883254e-02 4.934531e-04 -159.756887  0.000000e+00
curr_pts_1_policy       2.776014e-02 2.554109e-04  108.688141  0.000000e+00
curr_pts_2_policy       2.774216e-02 2.551882e-04  108.712538  0.000000e+00
curr_pts_3_policy       2.777401e-02 2.548658e-04  108.975027  0.000000e+00
curr_pts_4_policy       2.768917e-02 2.553936e-04  108.417650  0.000000e+00
curr_pts_5_policy       2.775464e-02 2.571282e-04  107.940874  0.000000e+00
curr_pts_6_policy       2.776974e-02 2.556542e-04  108.622239  0.000000e+00
curr_pts_7_policy       2.771224e-02 2.586256e-04  107.151987  0.000000e+00
curr_pts_8_policy       2.772757e-02 2.604595e-04  106.456359  0.000000e+00
curr_pts_9_policy       2.778485e-02 2.614230e-04  106.283090  0.000000e+00
curr_pts_10_policy      2.778901e-02 2.684463e-04  103.517942  0.000000e+00
curr_pts_11_20_policy   2.794762e-02 2.796314e-04   99.944499  0.000000e+00
curr_pts_21_30_policy   2.756438e-02 3.225024e-04   85.470297  0.000000e+00
curr_pts_31_150_policy  2.785447e-02 6.718762e-04   41.457740 1.510519e-305
```

The estimates are the same using both approaches, which
validates the estimation library for the fixed effects model 
and cluster-robust variance estimator
with frequency-weighted observations. 
