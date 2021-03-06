# Logistic Regression Models - Female Drivers

## Logistic Regression Results 

## Placebo Regressions 

## Regressions for Full Sample 



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                     -9.9081         0.0125715          -788.14                0   
policyTRUE                    -0.00383786         0.00268782         -1.42787         0.153329   
age_grp20-24                   0.788414         0.0114928          68.6006                0   
age_grp25-34                   0.631679         0.0110669          57.0785                0   
age_grp35-44                   0.573548         0.0110458          51.9243                0   
age_grp45-54                   0.422234         0.011077           38.118                0   
age_grp55-64                   0.207018         0.011391          18.1738         8.31698e-74   
age_grp65-199                 -0.130744         0.0121677         -10.7452         6.24373e-27   
curr_pts_grp1-3                 1.41588         0.00323697          437.408                0   
curr_pts_grp4-6                 1.93618         0.00497965          388.819                0   
curr_pts_grp7-9                 2.20745         0.00810984          272.193                0   
curr_pts_grp10-150              2.52492         0.0113869           221.74                0   
month02                        0.148981         0.00669389          22.2562         9.82068e-110   
month03                        0.172118         0.00654186          26.3103         1.46276e-152   
month04                        0.133452         0.00675083          19.7682         5.59378e-87   
month05                        0.182854         0.00654927          27.9198         1.53558e-171   
month06                        0.153932         0.00667899          23.0472         1.56799e-117   
month07                        0.100743         0.00672366          14.9834         9.42307e-51   
month08                        0.0256254         0.00678424           3.7772         0.000158602   
month09                        0.312983         0.00644006          48.5993                0   
month10                        0.258818         0.00642253          40.2985                0   
month11                        0.268431         0.00644043          41.6791                0   
month12                       -0.544506         0.00803619         -67.7568                0   
weekdayMonday                   0.62215         0.00580762          107.127                0   
weekdayTuesday                 0.750496         0.00569309          131.826                0   
weekdayWednesday               0.803012         0.00564744          142.191                0   
weekdayThursday                 0.75793         0.00568597          133.298                0   
weekdayFriday                  0.574812         0.00586629          97.9856                0   
weekdaySaturday                0.113317         0.00644666          17.5776         3.65665e-69  
```



```
AME          MER
-0.0822220224810217          -0.597166
17.7590431364961          85.103736
12.8757778712734          62.482858
11.0661517691468          54.951989
7.10501794176606          37.279601
2.91747863748177          16.324442
-1.44726503782442          -8.700638

```



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -10.5138         0.0222425         -472.688                0   
policyTRUE                     0.930723         0.0246089          37.8205                0   
age_grp20-24                    1.38072         0.0221575          62.3139                0   
age_grp25-34                    1.24138         0.0216685          57.2894                0   
age_grp35-44                    1.18518         0.0216438          54.7583                0   
age_grp45-54                    1.04306         0.0216792          48.1134                0   
age_grp55-64                   0.828711         0.0220097          37.6521                0   
age_grp65-199                  0.491155         0.022839          21.5051         1.39574e-102   
curr_pts_grp1-3                 1.41426         0.00323631          436.998                0   
curr_pts_grp4-6                 1.93503         0.0049797          388.584                0   
curr_pts_grp7-9                 2.20629         0.00811038          272.033                0   
curr_pts_grp10-150              2.52346         0.0113881          221.587                0   
month02                        0.148964         0.00669388          22.2538         1.03696e-109   
month03                         0.17209         0.00654185           26.306         1.63598e-152   
month04                        0.133537         0.00675083          19.7809         4.35291e-87   
month05                        0.182924         0.00654927          27.9305         1.13801e-171   
month06                        0.153986         0.00667898          23.0552         1.30313e-117   
month07                        0.100791         0.00672365          14.9906         8.4613e-51   
month08                        0.0256594         0.00678423           3.7822         0.000155446   
month09                        0.313007         0.00644006          48.6032                0   
month10                        0.258837         0.00642252          40.3016                0   
month11                        0.268446         0.00644042          41.6814                0   
month12                       -0.544498         0.00803617         -67.7559                0   
weekdayMonday                  0.622151         0.00580761          107.127                0   
weekdayTuesday                   0.7505         0.00569309          131.827                0   
weekdayWednesday               0.803017         0.00564743          142.192                0   
weekdayThursday                0.757935         0.00568596          133.299                0   
weekdayFriday                  0.574815         0.00586628          97.9863                0   
weekdaySaturday                0.113318         0.00644665          17.5778         3.64508e-69   
policyTRUE:age_grp20-24       -0.908542         0.0259703         -34.9839         3.94934e-268   
policyTRUE:age_grp25-34        -0.94124         0.0252163         -37.3267         6.06189e-305   
policyTRUE:age_grp35-44        -0.94514         0.025186         -37.5264                0   
policyTRUE:age_grp45-54       -0.963295         0.0252566         -38.1403                0   
policyTRUE:age_grp55-64       -0.965176         0.0258246         -37.3744         1.01978e-305   
policyTRUE:age_grp65-199      -0.965771         0.0272178         -35.4831         8.96371e-276  
```



```
AME          MER
8.95963107083409          59.451186
-19.5955157683945          -93.858376
-16.6511467875921          -80.770063
-15.3627920273075          -76.256328
-12.5313564430838          -65.724095
-9.47981801215223          -53.020620
-6.29630094107278          -37.835039
24.0402160270217          115.165060
19.6193632407884          95.174216
17.7006973302496          87.865059
13.5587365484537          71.111740
8.92911030144762          49.937478
4.08600582722139          24.550067

```

