# Logistic Regression Models - Male Drivers

## Logistic Regression Results 

## Regressions for Full Sample 



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -8.21782         0.00388546         -2115.02                0   
policyTRUE                    -0.100938         0.00120085         -84.0553                0   
age_grp20-24                   0.296925         0.00317094          93.6392                0   
age_grp25-34                   0.108549         0.00294657           36.839         4.38746e-297   
age_grp35-44                   0.122673         0.0029691          41.3166                0   
age_grp45-54                  -0.000931998         0.00297714        -0.313051         0.754242   
age_grp55-64                  -0.174493         0.00316209         -55.1829                0   
age_grp65-199                 -0.537512         0.00353167         -152.198                0   
curr_pts_grp1-3                0.859535         0.00147618          582.268                0   
curr_pts_grp4-6                 1.31798         0.00182997          720.217                0   
curr_pts_grp7-9                 1.57239         0.00240455          653.922                0   
curr_pts_grp10-150              1.82471         0.00249059          732.643                0   
month02                        0.181407         0.00301525          60.1634                0   
month03                        0.208552         0.00293339           71.096                0   
month04                        0.175202         0.00299733          58.4527                0   
month05                        0.204888         0.00295017          69.4493                0   
month06                         0.18201         0.0029889          60.8954                0   
month07                        0.150713         0.00298446          50.4991                0   
month08                        0.0447136         0.00306118          14.6067         2.54642e-48   
month09                        0.283772         0.00291599          97.3156                0   
month10                        0.247021         0.00291255          84.8126                0   
month11                        0.217196         0.00295585          73.4802                0   
month12                       -0.544916         0.00360287         -151.245                0   
weekdayMonday                  0.349463         0.00243392          143.581                0   
weekdayTuesday                 0.440547         0.00239019          184.315                0   
weekdayWednesday               0.490615         0.00236764          207.217                0   
weekdayThursday                0.444935         0.00238978          186.183                0   
weekdayFriday                  0.375026         0.00242315          154.768                0   
weekdaySaturday                0.091313         0.00257834          35.4155         9.86856e-275  
```



```
AME          MER
-6.48672183406766          -21.383801
28.4206016161361          57.237945
8.35388631216638          18.990767
9.39615897288925          21.616350
-0.0622386980098826          -0.154323
-9.86861816204118          -26.532005
-22.8299453453742          -68.928772

```



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                     -8.7685         0.00596647         -1469.63                0   
policyTRUE                     0.735591         0.00612641          120.069                0   
age_grp20-24                   0.902294         0.00582435          154.918                0   
age_grp25-34                   0.693804         0.00559783          123.942                0   
age_grp35-44                   0.668679         0.00562511          118.874                0   
age_grp45-54                   0.567485         0.00563369          100.731                0   
age_grp55-64                   0.406567         0.00582336          69.8167                0   
age_grp65-199                  0.0595799         0.00620341          9.60438         7.66184e-22   
curr_pts_grp1-3                0.849318         0.0014751          575.771                0   
curr_pts_grp4-6                 1.30673         0.00182905          714.429                0   
curr_pts_grp7-9                 1.56016         0.00240395          648.997                0   
curr_pts_grp10-150              1.80884         0.00249062          726.259                0   
month02                         0.18135         0.00301526          60.1442                0   
month03                        0.208386         0.0029334          71.0391                0   
month04                        0.175761         0.00299734          58.6388                0   
month05                        0.205363         0.00295019          69.6099                0   
month06                        0.182416         0.00298891           61.031                0   
month07                        0.151072         0.00298447          50.6194                0   
month08                        0.0449923         0.00306119          14.6976         6.6737e-49   
month09                        0.283979         0.002916          97.3864                0   
month10                        0.247184         0.00291256          84.8683                0   
month11                        0.217284         0.00295586          73.5096                0   
month12                       -0.544898         0.00360287          -151.24                0   
weekdayMonday                   0.34946         0.00243392          143.579                0   
weekdayTuesday                 0.440554         0.00239019          184.317                0   
weekdayWednesday               0.490617         0.00236765          207.217                0   
weekdayThursday                0.444959         0.00238978          186.192                0   
weekdayFriday                  0.375045         0.00242315          154.776                0   
weekdaySaturday                0.0913164         0.00257834          35.4167         9.43646e-275   
policyTRUE:age_grp20-24       -0.931995         0.00699657         -133.207                0   
policyTRUE:age_grp25-34       -0.894256         0.00662088         -135.066                0   
policyTRUE:age_grp35-44       -0.813324         0.00665451         -122.222                0   
policyTRUE:age_grp45-54        -0.86078         0.00668098          -128.84                0   
policyTRUE:age_grp55-64       -0.888393         0.00702284         -126.501                0   
policyTRUE:age_grp65-199       -0.92482         0.00770592         -120.014                0  
```



```
AME          MER
35.2128589279042          102.711719
-57.8021755192324          -116.095701
-42.1518307174949          -95.465030
-41.4728804434062          -95.039461
-34.4233087345309          -84.967326
-26.8474515438322          -71.798689
-16.7300792984264          -50.184152
68.9345286215538          138.439883
41.7977494830383          94.647522
39.2597476464769          89.958195
29.2630814612109          72.215669
17.7444600561274          47.440499
1.9370642657905          5.808013

```



### One-point violations (for speeding 11-20 over)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                     -11.026         0.014105         -781.713                0   
policyTRUE                     0.110308         0.00425733          25.9102         5.11184e-148   
age_grp20-24                   0.120422         0.0117874          10.2162         1.67762e-24   
age_grp25-34                   0.094419         0.0106777          8.84263         9.34907e-19   
age_grp35-44                   0.241838         0.0106324          22.7453         1.59786e-114   
age_grp45-54                   0.164202         0.0106144          15.4698         5.55106e-54   
age_grp55-64                   0.00550388         0.0111837         0.492133         0.622625   
age_grp65-199                 -0.412922         0.0124974         -33.0407         2.11908e-239   
curr_pts_grp1-3                0.842643         0.00519373          162.242                0   
curr_pts_grp4-6                 1.25732         0.00658474          190.945                0   
curr_pts_grp7-9                 1.56369         0.0085563          182.753                0   
curr_pts_grp10-150              1.92168         0.00855828           224.54                0   
month02                         0.21045         0.0106239          19.8091         2.48391e-87   
month03                        0.231197         0.010351          22.3358         1.65842e-110   
month04                        0.155993         0.0106676           14.623         2.0032e-48   
month05                        0.233411         0.0103918          22.4611         9.95886e-112   
month06                         0.18895         0.0105764          17.8653         2.1973e-71   
month07                          0.1207         0.0106474          11.3361         8.69282e-30   
month08                        0.0375299         0.0108774          3.45027         0.00056003   
month09                        0.377291         0.0101298          37.2455         1.25458e-303   
month10                         0.23408         0.0103527          22.6105         3.41574e-113   
month11                         0.17821         0.0105802          16.8436         1.16871e-63   
month12                       -0.546135         0.0127765         -42.7452                0   
weekdayMonday                  0.490021         0.0088019          55.6722                0   
weekdayTuesday                 0.605497         0.00861678          70.2695                0   
weekdayWednesday               0.622315         0.00859426          72.4106                0   
weekdayThursday                0.575033         0.0086724           66.306                0   
weekdayFriday                  0.454735         0.00886719          51.2829                0   
weekdaySaturday                0.0916176         0.00958595          9.55748         1.20654e-21  
```



```
AME          MER
0.566469522223267          1.385806
0.711756169250495          1.348198
0.485007002492796          1.043180
1.31880429496344          2.882196
0.797403391724127          1.879986
0.0227506633733789          0.058144
-1.24451633328647          -3.564017

```



### Two-point violations (speeding 21-30 over or 7 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.47438         0.00660545         -1434.33                0   
policyTRUE                    -0.00230962         0.00190908         -1.20981         0.226354   
age_grp20-24                    0.39625         0.00563547          70.3135                0   
age_grp25-34                   0.329236         0.00519924          63.3238                0   
age_grp35-44                   0.421211         0.0051991          81.0161                0   
age_grp45-54                   0.344451         0.00518783          66.3959                0   
age_grp55-64                   0.187746         0.00542031          34.6376         6.87676e-263   
age_grp65-199                 -0.233517         0.00596677         -39.1363                0   
curr_pts_grp1-3                0.824101         0.00231521          355.952                0   
curr_pts_grp4-6                 1.24844         0.00292566          426.721                0   
curr_pts_grp7-9                 1.46763         0.00393967          372.527                0   
curr_pts_grp10-150              1.61831         0.00430103           376.26                0   
month02                        0.219006         0.00476324          45.9784                0   
month03                        0.269807         0.00461243          58.4956                0   
month04                        0.200441         0.00474307          42.2597                0   
month05                          0.2232         0.00467808          47.7119                0   
month06                        0.144569         0.00480108          30.1117         3.40378e-199   
month07                        0.0992261         0.00480942          20.6316         1.42778e-94   
month08                       -0.0133712         0.00494759         -2.70257         0.00688064   
month09                         0.29677         0.00463103           64.083                0   
month10                        0.255919         0.00462982          55.2763                0   
month11                        0.241173         0.0046845          51.4832                0   
month12                       -0.607245         0.00585636          -103.69                0   
weekdayMonday                  0.427644         0.0039093          109.391                0   
weekdayTuesday                 0.514055         0.0038454          133.681                0   
weekdayWednesday               0.557125         0.0038157          146.008                0   
weekdayThursday                0.510346         0.00385097          132.524                0   
weekdayFriday                  0.422947         0.00391605          108.003                0   
weekdaySaturday                0.0854805         0.00421318          20.2888         1.61497e-91  
```



```
AME          MER
-0.0586536818701535          -0.155195
11.3174720232926          22.013812
8.15746402906588          17.653161
10.8845731139753          23.714109
7.98601943883191          18.617755
3.72542362471725          9.351413
-3.37622774440765          -9.431459

```



### Three-point violations (speeding 31-60 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -8.87227         0.00555289         -1597.78                0   
policyTRUE                    -0.179141         0.00173555         -103.218                0   
age_grp20-24                   0.327395         0.00449584          72.8219                0   
age_grp25-34                    0.10176         0.00420418          24.2045         1.99403e-129   
age_grp35-44                   0.0768876         0.00425245          18.0808         4.51636e-73   
age_grp45-54                  -0.0821545         0.00428183         -19.1868         4.77213e-82   
age_grp55-64                  -0.266925         0.00457302         -58.3695                0   
age_grp65-199                 -0.567455         0.00507199          -111.88                0   
curr_pts_grp1-3                0.880358         0.00214393          410.629                0   
curr_pts_grp4-6                 1.36424         0.00262434          519.839                0   
curr_pts_grp7-9                 1.62666         0.00341789          475.926                0   
curr_pts_grp10-150              1.88872         0.00351466          537.385                0   
month02                        0.150438         0.00433906          34.6707         2.18127e-263   
month03                        0.155682         0.00424101          36.7086         5.32152e-295   
month04                        0.143751         0.00431643          33.3032         3.47246e-243   
month05                        0.168333         0.0042514          39.5948                0   
month06                        0.185236         0.00426852          43.3957                0   
month07                        0.166226         0.00425035          39.1088                0   
month08                        0.056695         0.00436114               13         1.22309e-38   
month09                         0.23086         0.00421651          54.7514                0   
month10                        0.220039         0.00418661          52.5577                0   
month11                        0.189966         0.00424912          44.7072                0   
month12                       -0.526642         0.00511984         -102.863                0   
weekdayMonday                  0.325625         0.00351713          92.5827                0   
weekdayTuesday                 0.422709         0.00344912          122.556                0   
weekdayWednesday               0.486121         0.00340733          142.669                0   
weekdayThursday                0.436872         0.00344146          126.944                0   
weekdayFriday                  0.370615         0.00348737          106.273                0   
weekdaySaturday                0.100067         0.00370007          27.0448         4.40151e-161  
```



```
AME          MER
-5.52442902210299          -20.369555
16.599144090625          34.680521
4.04507756991306          9.592965
2.97754874915069          7.157494
-2.71766281111554          -7.064433
-7.41990048337097          -20.986479
-12.1552566537595          -38.799214

```



### Four-point violations (speeding 31-45 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -10.7482         0.0283132         -379.617                0   
policyTRUE                    -0.216495         0.0115695         -18.7126         3.9095e-78   
age_grp20-24                  -0.642471         0.0155841         -41.2261                0   
age_grp25-34                   -1.73346         0.0167388          -103.56                0   
age_grp35-44                   -2.39604         0.021433         -111.792                0   
age_grp45-54                   -2.94816         0.0258651         -113.982                0   
age_grp55-64                   -3.27779          0.03453         -94.9257                0   
age_grp65-199                   -3.5078         0.0434101         -80.8061                0   
curr_pts_grp1-3                0.855183         0.015782          54.1872                0   
curr_pts_grp4-6                 1.47462         0.0176588          83.5057                0   
curr_pts_grp7-9                 1.81677         0.0211519          85.8916                0   
curr_pts_grp10-150              2.27957         0.0186123          122.477                0   
month02                        0.0505444         0.0313981          1.60979         0.107443   
month03                        0.0573265           0.0306          1.87341         0.0610113   
month04                        0.242011         0.0300937          8.04192         8.8443e-16   
month05                        0.368829         0.0289813          12.7265         4.21536e-37   
month06                        0.420315         0.0288258          14.5812         3.69885e-48   
month07                        0.435107         0.0285225          15.2549         1.52832e-52   
month08                         0.40343         0.0286287          14.0918         4.26336e-45   
month09                        0.386845         0.0288762          13.3967         6.3215e-41   
month10                        0.375069         0.0287242          13.0576         5.75143e-39   
month11                        0.271308         0.0295005          9.19673         3.68992e-20   
month12                        0.00648417         0.0311266         0.208316         0.834982   
weekdayMonday                 -0.250882         0.0221018         -11.3512         7.3148e-30   
weekdayTuesday                -0.197732         0.0217814         -9.07804         1.10548e-19   
weekdayWednesday              -0.157088         0.0215384         -7.29338         3.0227e-13   
weekdayThursday               -0.106981         0.0212723         -5.02911         4.92773e-07   
weekdayFriday                  0.0625626         0.0203801          3.06979         0.00214213   
weekdaySaturday                0.169046         0.0198622          8.51094         1.72534e-17  
```



```
AME          MER
-0.154515345565548          -1.155523
-2.78129201914905          -5.349014
-4.11442476424616          -9.291318
-4.40578552526883          -10.257344
-4.18025738237345          -10.693476
-3.84834474408599          -10.859661
-3.38801373793396          -10.947118

```



### Five-point violations (speeding 46-60 over or a handheld device violation)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -11.0753         0.0225167         -491.872                0   
policyTRUE                    -0.671349         0.00799015         -84.0222                0   
age_grp20-24                   0.249725         0.0159156          15.6906         1.7549e-55   
age_grp25-34                  -0.210783         0.0152954         -13.7808         3.32557e-43   
age_grp35-44                  -0.659411         0.0164072         -40.1903                0   
age_grp45-54                   -1.04681         0.0173624         -60.2921                0   
age_grp55-64                   -1.47503         0.0209693         -70.3425                0   
age_grp65-199                  -2.22114         0.030105         -73.7796                0   
curr_pts_grp1-3                 1.10875         0.0100021          110.852                0   
curr_pts_grp4-6                 1.63982         0.011576          141.657                0   
curr_pts_grp7-9                 1.95391         0.0141417          138.166                0   
curr_pts_grp10-150              2.44091         0.0130015           187.74                0   
month02                         0.14952         0.0218131           6.8546         7.15107e-12   
month03                        0.229932         0.0209404          10.9803         4.75258e-28   
month04                        0.385291         0.0206554          18.6533         1.18799e-77   
month05                        0.419111         0.0203703          20.5746         4.6348e-94   
month06                        0.500457         0.0201435          24.8446         2.95919e-136   
month07                        0.563977         0.0197647          28.5346         4.36582e-179   
month08                        0.473011         0.0200887          23.5461         1.37768e-122   
month09                        0.642851         0.0195471          32.8873         3.33994e-237   
month10                        0.583644         0.0196373          29.7212         4.08708e-194   
month11                         0.46034         0.0201908          22.7995         4.63815e-115   
month12                       -0.389488         0.024679         -15.7821         4.13002e-56   
weekdayMonday                 -0.200758         0.0141391         -14.1988         9.32238e-46   
weekdayTuesday                 -0.24302         0.0143154         -16.9761         1.23459e-64   
weekdayWednesday              -0.201593         0.0141541         -14.2427         4.97515e-46   
weekdayThursday                -0.22474         0.0142518         -15.7693         5.06216e-56   
weekdayFriday                 -0.0767707         0.0136978          -5.6046         2.08735e-08   
weekdaySaturday               -0.00520248         0.0134387        -0.387126         0.698663  
```



```
AME          MER
-1.03558259334314          -7.204477
1.41905064931916          3.255781
-0.803214634333774          -2.181374
-1.9877546618993          -5.542227
-2.41080867021816          -7.448928
-2.56506944323271          -8.852676
-2.52808992684959          -10.233536

```



### Seven-point violations (speeding 61-80 over or combinations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -12.3458         0.0508953         -242.573                0   
policyTRUE                    -0.802205         0.0194411         -41.2633                0   
age_grp20-24                   0.0299728         0.0315515         0.949964         0.342131   
age_grp25-34                  -0.655389         0.0311033         -21.0714         1.45688e-98   
age_grp35-44                    -1.4654         0.0369104         -39.7015                0   
age_grp45-54                   -2.13147         0.0440059         -48.4359                0   
age_grp55-64                   -2.89826         0.0675699         -42.8927                0   
age_grp65-199                  -3.97972         0.127375          -31.244         2.68951e-214   
curr_pts_grp1-3                 1.19072         0.0255175           46.663                0   
curr_pts_grp4-6                 1.74623         0.0286812          60.8841                0   
curr_pts_grp7-9                 2.13864         0.0332261          64.3662                0   
curr_pts_grp10-150              2.85842         0.0279561          102.247                0   
month02                        0.0282387         0.0555301          0.50853         0.611082   
month03                        0.233757         0.0516398          4.52668         5.99175e-06   
month04                        0.486422         0.0502156          9.68667         3.43547e-22   
month05                        0.566372         0.0491441          11.5247         9.90266e-31   
month06                        0.714343         0.0480182          14.8765         4.68356e-50   
month07                        0.645435         0.0482753          13.3699         9.06686e-41   
month08                        0.614484         0.0484955           12.671         8.56639e-37   
month09                        0.771164         0.0473255          16.2949         1.07288e-59   
month10                        0.595018         0.0485294           12.261         1.46722e-34   
month11                        0.448425         0.0500833          8.95359         3.4409e-19   
month12                       -0.475848         0.0625786         -7.60401         2.87086e-14   
weekdayMonday                 -0.392054         0.0337931         -11.6016         4.04397e-31   
weekdayTuesday                -0.359537         0.0334883         -10.7362         6.88192e-27   
weekdayWednesday              -0.471648         0.0346497         -13.6119         3.40307e-42   
weekdayThursday                -0.43768         0.0343047         -12.7586         2.79131e-37   
weekdayFriday                 -0.199935         0.0320219          -6.2437         4.27331e-10   
weekdaySaturday                0.00299327         0.0303341         0.0986767         0.921395  
```



```
AME          MER
-0.215198680147258          -1.825474
0.0480491130929188          0.097708
-0.618405031043459          -1.543883
-0.946222774462022          -2.469589
-0.960632487059355          -2.830308
-0.906000631223632          -3.034378
-0.784799397192281          -3.151367

```



### All pairs of infractions 9 or over (speeding 81 or more and 10 other offences)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -28.0248          56.4827        -0.496167         0.619777   
policyTRUE                    -0.155766          17.5992        -0.00885076         0.992938   
age_grp20-24                   0.185958          51.7329         0.00359458         0.997132   
age_grp25-34                   0.615733          41.8449         0.0147146          0.98826   
age_grp35-44                   0.518019          42.5888         0.0121633         0.990295   
age_grp45-54                   0.430862          42.1328         0.0102263         0.991841   
age_grp55-64                  -0.0175759           45.538        -0.00038596         0.999692   
age_grp65-199                 -0.529981          50.6613        -0.0104613         0.991653   
curr_pts_grp1-3                -1.32992          36.0355        -0.0369058          0.97056   
curr_pts_grp4-6                -1.95476          81.8377        -0.0238858         0.980944   
curr_pts_grp7-9                -2.26776          152.358        -0.0148844         0.988124   
curr_pts_grp10-150             -1.64935          132.869        -0.0124133         0.990096   
month02                        0.190257          44.7135         0.00425502         0.996605   
month03                        0.217572          43.5473         0.00499623         0.996014   
month04                        0.220768           43.743         0.00504693         0.995973   
month05                         0.25494          43.0419         0.00592307         0.995274   
month06                        0.219074          43.7511         0.00500728         0.996005   
month07                        0.173574          43.8715         0.00395641         0.996843   
month08                        0.0599048          45.0686         0.00132919         0.998939   
month09                        0.301055          43.0033         0.00700073         0.994414   
month10                        0.272047          42.9099         0.00633995         0.994941   
month11                        0.235737          43.6378         0.00540214          0.99569   
month12                       -0.638207          55.1448        -0.0115733         0.990766   
weekdayMonday                  0.412326          36.0952         0.0114233         0.990886   
weekdayTuesday                 0.514694          35.3978         0.0145403         0.988399   
weekdayWednesday               0.557541          35.1252         0.0158729         0.987336   
weekdayThursday                0.513018          35.4309         0.0144794         0.988448   
weekdayFriday                  0.430331          36.0005         0.0119535         0.990463   
weekdaySaturday                0.104941          38.6081         0.00271812         0.997831  
```



```
AME          MER
-1.71649318691852e-08          -0.000000
1.65350659378179e-08          0.000000
7.32531623263283e-08          0.000000
5.76446455015836e-08          0.000000
4.78773773536615e-08          0.000000
-1.6481464940561e-09          -0.000000
-4.13144162450719e-08          -0.000000

```

