# Logistic Regression Models - Female Drivers

## Logistic Regression Results 

## Regressions for Full Sample 



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.39662         0.00674144         -1393.86                0   
policyTRUE                    -0.0176404         0.00186942         -9.43632         3.86103e-21   
age_grp20-24                   0.116717         0.0055452          21.0483         2.37258e-98   
age_grp25-34                  -0.0415627         0.00513697          -8.0909         5.92271e-16   
age_grp35-44                  -0.0739652         0.00512042         -14.4452         2.68961e-47   
age_grp45-54                  -0.264102         0.00517036           -51.08                0   
age_grp55-64                  -0.495831         0.00551781         -89.8601                0   
age_grp65-199                 -0.851528         0.00632092         -134.716                0   
curr_pts_grp1-3                 1.44962         0.00223165          649.575                0   
curr_pts_grp4-6                 1.97288         0.00331624          594.914                0   
curr_pts_grp7-9                 2.24914         0.00529753          424.564                0   
curr_pts_grp10-150              2.49983         0.00710565          351.809                0   
month02                        0.237237         0.00480903          49.3316                0   
month03                         0.30578         0.00464032          65.8963                0   
month04                        0.299691         0.00471282          63.5906                0   
month05                        0.324359         0.00464756          69.7913                0   
month06                        0.262994         0.00474532          55.4218                0   
month07                        0.168708         0.00480309           35.125         2.80385e-270   
month08                        0.082098         0.00490675          16.7316         7.70883e-63   
month09                        0.386802         0.00461125          83.8822                0   
month10                        0.352586         0.00460017          76.6462                0   
month11                        0.337187         0.00465415          72.4486                0   
month12                       -0.558345         0.00584718         -95.4896                0   
weekdayMonday                  0.635361         0.00406388          156.343                0   
weekdayTuesday                 0.777628         0.0039701          195.871                0   
weekdayWednesday               0.825962         0.00394098          209.583                0   
weekdayThursday                0.763873         0.00398104          191.878                0   
weekdayFriday                  0.605878         0.00408775          148.218                0   
weekdaySaturday                0.133581         0.00449993          29.6852         1.19056e-193  
```



```
AME          MER
-0.37319115208971          -2.613360
3.66281941868651          16.466849
-1.14348586282892          -5.416146
-1.97431814469702          -9.485577
-5.96613899343597          -30.887023
-9.31416188861281          -52.034191
-12.6024197877103          -76.317476

```



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -10.1127         0.0116455         -868.375                0   
policyTRUE                      1.00084         0.0119423           83.806                0   
age_grp20-24                   0.841839         0.0114731          73.3748                0   
age_grp25-34                    0.68619         0.011048            62.11                0   
age_grp35-44                   0.639116         0.0110296          57.9454                0   
age_grp45-54                   0.486772         0.0110657          43.9894                0   
age_grp55-64                   0.269916         0.0113844          23.7092         2.89574e-124   
age_grp65-199                 -0.0699627         0.0121651         -5.75109         8.86681e-09   
curr_pts_grp1-3                 1.44052         0.00223162          645.503                0   
curr_pts_grp4-6                 1.96347         0.00331622           592.08                0   
curr_pts_grp7-9                 2.23924         0.00529777          422.675                0   
curr_pts_grp10-150              2.48626         0.00710764          349.801                0   
month02                        0.237168         0.00480904          49.3171                0   
month03                        0.305588         0.00464033          65.8549                0   
month04                        0.300274         0.00471284          63.7141                0   
month05                        0.324838         0.00464758          69.8939                0   
month06                        0.263385         0.00474533          55.5041                0   
month07                        0.169054         0.0048031          35.1969         2.2316e-271   
month08                        0.0823568         0.00490676          16.7843         3.17775e-63   
month09                        0.386991         0.00461125          83.9231                0   
month10                        0.352732         0.00460018          76.6779                0   
month11                        0.337254         0.00465416          72.4629                0   
month12                       -0.558349         0.00584719         -95.4902                0   
weekdayMonday                  0.635357         0.00406388          156.342                0   
weekdayTuesday                 0.777633         0.00397011          195.872                0   
weekdayWednesday               0.825961         0.00394098          209.583                0   
weekdayThursday                0.763893         0.00398104          191.883                0   
weekdayFriday                  0.605895         0.00408776          148.222                0   
weekdaySaturday                0.133585         0.00449993          29.6859         1.16672e-193   
policyTRUE:age_grp20-24        -1.02738         0.0132117         -77.7624                0   
policyTRUE:age_grp25-34        -1.03361         0.0125408         -82.4198                0   
policyTRUE:age_grp35-44        -1.00519         0.0125139         -80.3258                0   
policyTRUE:age_grp45-54        -1.08149         0.0126062         -85.7906                0   
policyTRUE:age_grp55-64        -1.11257         0.0131881         -84.3618                0   
policyTRUE:age_grp65-199       -1.14513         0.0145602         -78.6479                0  
```



```
AME          MER
19.6856428657577          110.873869
-20.8968781954417          -93.539372
-16.9388869447609          -79.858594
-16.1320250041117          -77.136678
-12.4263401817019          -63.996390
-9.13542178278127          -50.742396
-5.90372406720532          -35.524039
19.0183158995227          85.123875
13.4871111807265          63.576945
12.0668626601605          57.692336
7.85346549314767          40.436173
3.59920437692782          19.985301
-0.724767397578093          -4.359352

```



### One-point violations (for speeding 11-20 over)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -11.9294         0.0219566         -543.316                0   
policyTRUE                     0.225136         0.00620182          36.3016         1.52456e-288   
age_grp20-24                  -0.00766238         0.017255        -0.444068         0.656994   
age_grp25-34                  -0.157033         0.0158081          -9.9337         2.97036e-23   
age_grp35-44                  -0.211402         0.0157786          -13.398         6.21136e-41   
age_grp45-54                  -0.391587         0.0159341         -24.5754         2.31589e-133   
age_grp55-64                  -0.608966         0.0170937         -35.6252         5.69997e-278   
age_grp65-199                  -1.00166         0.0199447         -50.2217                0   
curr_pts_grp1-3                 1.38831         0.0073274          189.469                0   
curr_pts_grp4-6                 1.76851         0.0115948          152.526                0   
curr_pts_grp7-9                 2.02061         0.0188406          107.248                0   
curr_pts_grp10-150              2.45849         0.0229344          107.197                0   
month02                        0.292387         0.0161996           18.049         8.03737e-73   
month03                        0.409322         0.0154888           26.427         6.71274e-154   
month04                        0.321177         0.0159758           20.104         6.81462e-90   
month05                        0.397784         0.015606          25.4892         2.59734e-143   
month06                        0.362573         0.0158238          22.9131         3.43944e-116   
month07                        0.195715         0.0162576          12.0383         2.23387e-33   
month08                         0.17934         0.0163575          10.9638         5.70662e-28   
month09                        0.616207         0.0150294          41.0001                0   
month10                        0.412458         0.015491          26.6257         3.42057e-156   
month11                         0.34424         0.0158532          21.7143         1.50465e-104   
month12                       -0.503157         0.0195725         -25.7074         9.66287e-146   
weekdayMonday                  0.785584         0.0137443           57.157                0   
weekdayTuesday                 0.916806         0.013476          68.0323                0   
weekdayWednesday                0.96164         0.0133968          71.7815                0   
weekdayThursday                0.890847         0.013541           65.789                0   
weekdayFriday                  0.698865         0.0139475          50.1068                0   
weekdaySaturday                0.143967         0.015562          9.25122         2.21952e-20  
```



```
AME          MER
0.438391772532287          2.583350
-0.0205062597103593          -0.078700
-0.372643815175481          -1.498361
-0.482472917550358          -1.964685
-0.767603651354423          -3.340825
-1.00975167076758          -4.702620
-1.30328878768291          -6.524061

```



### Two-point violations (speeding 21-30 over or 7 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -10.4204         0.0103669         -1005.16                0   
policyTRUE                     0.044492         0.00275373           16.157         1.01419e-58   
age_grp20-24                   0.168221         0.00860549          19.5481         4.28113e-85   
age_grp25-34                   0.0830027         0.00794872          10.4423         1.58957e-25   
age_grp35-44                   0.0701472         0.00791566          8.86183         7.87146e-19   
age_grp45-54                  -0.0998532         0.00797123         -12.5267         5.334e-36   
age_grp55-64                  -0.345467         0.0084508         -40.8798                0   
age_grp65-199                 -0.787088         0.0097191         -80.9836                0   
curr_pts_grp1-3                 1.41958         0.00328069          432.709                0   
curr_pts_grp4-6                 1.90501         0.00496494          383.693                0   
curr_pts_grp7-9                 2.12316         0.00818435          259.417                0   
curr_pts_grp10-150              2.27881         0.011522           197.78                0   
month02                        0.284825         0.00719657          39.5778                0   
month03                        0.401767         0.006882          58.3794                0   
month04                        0.380439         0.00700104          54.3404                0   
month05                        0.404925         0.00691048          58.5958                0   
month06                         0.30973         0.00710042          43.6213                0   
month07                        0.192374         0.00722168          26.6384         2.43943e-156   
month08                        0.112248         0.00736763          15.2353         2.06162e-52   
month09                        0.467528         0.00685924          68.1603                0   
month10                        0.418183         0.00686461          60.9187                0   
month11                        0.391573         0.00696049          56.2565                0   
month12                        -0.61686         0.00901072         -68.4585                0   
weekdayMonday                   0.71553         0.00607852          117.715                0   
weekdayTuesday                 0.856747         0.00594639          144.078                0   
weekdayWednesday               0.891327         0.00591714          150.635                0   
weekdayThursday                0.829957         0.0059742          138.924                0   
weekdayFriday                   0.65991         0.00613983           107.48                0   
weekdaySaturday                0.155289         0.0067884          22.8756         8.13634e-116  
```



```
AME          MER
0.434031582700504          2.670752
2.11853961171429          9.090118
0.954775085150137          4.294471
0.791718911925918          3.605817
-0.965598375224296          -4.715936
-2.76303039207935          -14.497650
-4.77462409314713          -27.043791

```



### Three-point violations (speeding 31-60 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                     -10.076         0.0101234         -995.317                0   
policyTRUE                    -0.115732         0.00286526         -40.3914                0   
age_grp20-24                   0.123687         0.00839959          14.7254         4.42721e-49   
age_grp25-34                  -0.079554         0.0078219         -10.1707         2.68029e-24   
age_grp35-44                  -0.113486         0.00779745         -14.5543         5.48592e-48   
age_grp45-54                  -0.321488         0.00788934         -40.7496                0   
age_grp55-64                  -0.538891         0.00842957         -63.9286                0   
age_grp65-199                 -0.811064         0.00952697         -85.1336                0   
curr_pts_grp1-3                 1.49491         0.00341991          437.119                0   
curr_pts_grp4-6                 2.07333         0.00495215          418.672                0   
curr_pts_grp7-9                 2.39653         0.00770189          311.161                0   
curr_pts_grp10-150              2.66121         0.0102577          259.436                0   
month02                        0.180749         0.00719311          25.1281         2.45428e-139   
month03                        0.186687         0.00703162          26.5497         2.59121e-155   
month04                        0.208385         0.0071089          29.3132         7.03858e-189   
month05                        0.221107         0.00702125          31.4911         1.15175e-217   
month06                        0.189431         0.00712897          26.5721         1.42812e-155   
month07                        0.133722         0.00715442          18.6908         5.88298e-78   
month08                        0.0225122         0.0073519          3.06209         0.00219796   
month09                        0.239298         0.00703255          34.0272         8.81207e-254   
month10                        0.265162         0.00692048          38.3155                0   
month11                        0.276651         0.00696114          39.7422                0   
month12                       -0.530962         0.0085626         -62.0094                0   
weekdayMonday                    0.5581         0.00616522          90.5239                0   
weekdayTuesday                  0.70973         0.00600661          118.158                0   
weekdayWednesday               0.774683         0.00594409          130.328                0   
weekdayThursday                0.711785         0.00600694          118.494                0   
weekdayFriday                  0.566292         0.00616026          91.9266                0   
weekdaySaturday                0.119977         0.00675584           17.759         1.46734e-70  
```



```
AME          MER
-1.04292133938412          -8.252788
1.81748903315373          8.785827
-0.996637725592106          -5.103684
-1.37594861855709          -7.160186
-3.25282991259905          -18.351121
-4.54122311631415          -27.810261
-5.55546399410969          -37.093275

```



### Four-point violations (speeding 31-45 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -12.7043         0.0752964         -168.723                0   
policyTRUE                    -0.0475898         0.0295655         -1.60964         0.107476   
age_grp20-24                  -0.770858         0.044203          -17.439         4.17099e-68   
age_grp25-34                    -1.7236         0.0443449          -38.868                0   
age_grp35-44                   -2.20109         0.0491882         -44.7484                0   
age_grp45-54                   -2.72738         0.057098         -47.7666                0   
age_grp55-64                   -3.22564         0.0809659         -39.8396                0   
age_grp65-199                  -3.19136         0.0960658         -33.2206         5.43562e-242   
curr_pts_grp1-3                 1.19231         0.038302          31.1291         9.73486e-213   
curr_pts_grp4-6                 1.89794         0.0521837          36.3704         1.25298e-289   
curr_pts_grp7-9                 2.26776         0.0773737          29.3092         7.92942e-189   
curr_pts_grp10-150              3.07171         0.0751373          40.8813                0   
month02                        0.0908122         0.078751          1.15316         0.248846   
month03                        0.165182         0.0756137          2.18455         0.0289219   
month04                        0.276927         0.0754029          3.67263         0.000240063   
month05                        0.331918         0.0737811          4.49868         6.83752e-06   
month06                        0.290038         0.0749238          3.87111         0.000108343   
month07                        0.242638         0.0750898          3.23131         0.00123226   
month08                        0.262295         0.0746284          3.51468         0.000440281   
month09                        0.398905         0.0728959          5.47225         4.44346e-08   
month10                        0.414245         0.0720665           5.7481         9.02525e-09   
month11                        0.300205         0.0742292          4.04429         5.24812e-05   
month12                       -0.0841146         0.0806678         -1.04273         0.297074   
weekdayMonday                 -0.0338858         0.057746        -0.586808         0.557333   
weekdayTuesday                 0.0916392         0.0559886          1.63675         0.101683   
weekdayWednesday               0.0656817         0.0563332          1.16595         0.243634   
weekdayThursday                0.136744         0.0554476          2.46619         0.0136557   
weekdayFriday                  0.220457         0.0543998          4.05254         5.06642e-05   
weekdaySaturday                0.151987         0.0552098           2.7529         0.00590703  
```



```
AME          MER
-0.00409535220862857          -0.053698
-0.354410785869519          -1.342140
-0.509216533972807          -2.051939
-0.540741630664825          -2.221130
-0.527639298704873          -2.334252
-0.505032643129171          -2.398342
-0.470090622896969          -2.394881

```



### Five-point violations (speeding 46-60 over or a handheld device violation)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -12.7267         0.0564021         -225.643                0   
policyTRUE                    -0.768794         0.0188049         -40.8825                0   
age_grp20-24                   0.153958         0.042672          3.60795         0.000308629   
age_grp25-34                  -0.358869         0.0410085         -8.75108         2.11313e-18   
age_grp35-44                   -0.72602         0.0420925         -17.2482         1.15396e-66   
age_grp45-54                   -1.15058         0.0441721         -26.0477         1.428e-149   
age_grp55-64                   -1.54081         0.051657         -29.8277         1.71076e-195   
age_grp65-199                  -2.26109         0.0747849         -30.2346         8.31032e-201   
curr_pts_grp1-3                 1.71218         0.0214856          79.6898                0   
curr_pts_grp4-6                 2.44681         0.0283494          86.3092                0   
curr_pts_grp7-9                 2.94858         0.039389          74.8579                0   
curr_pts_grp10-150              3.49858         0.0451767          77.4421                0   
month02                        0.197483         0.0508731          3.88188         0.000103652   
month03                        0.260186         0.0490565           5.3038         1.13418e-07   
month04                        0.444809         0.0482277          9.22309         2.8865e-20   
month05                        0.512865         0.047256          10.8529         1.93166e-27   
month06                        0.510277         0.047509          10.7406         6.55934e-27   
month07                        0.558598         0.0467128          11.9581         5.88619e-33   
month08                        0.475961         0.0474224          10.0366         1.05221e-23   
month09                         0.66935         0.0459459          14.5682         4.47371e-48   
month10                        0.618388         0.0460516          13.4282         4.13479e-41   
month11                        0.529762         0.0470155          11.2678         1.89208e-29   
month12                       -0.401823         0.0584393          -6.8759         6.16016e-12   
weekdayMonday                 -0.0989974         0.0320008         -3.09359         0.0019775   
weekdayTuesday                -0.223868          0.03312         -6.75929         1.38674e-11   
weekdayWednesday              -0.164943         0.0325915         -5.06093         4.17227e-07   
weekdayThursday               -0.228738         0.0331823         -6.89337         5.44852e-12   
weekdayFriday                 -0.146454         0.0324529         -4.51283         6.39689e-06   
weekdaySaturday               -0.0819344         0.0318767         -2.57036         0.0101594  
```



```
AME          MER
-0.17390533030212          -3.399643
0.133409198042205          0.904298
-0.222031059116961          -1.638306
-0.370324384497537          -2.804504
-0.439085115425213          -3.713956
-0.451715849231005          -4.269507
-0.458698276689028          -4.867056

```



### Seven-point violations (speeding 61-80 over or combinations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -14.5716          0.18682         -77.9978                0   
policyTRUE                    -0.972922         0.0700279         -13.8934         6.95066e-44   
age_grp20-24                  -0.0731883         0.116951        -0.625802         0.531445   
age_grp25-34                   -1.08793         0.118292         -9.19699         3.68103e-20   
age_grp35-44                   -1.64395         0.127749         -12.8685         6.76676e-38   
age_grp45-54                   -2.25948         0.146655         -15.4067         1.47507e-53   
age_grp55-64                   -3.08812         0.227416         -13.5792         5.32328e-42   
age_grp65-199                  -3.82635         0.390957         -9.78714         1.27865e-22   
curr_pts_grp1-3                 1.69678         0.0826629          20.5265         1.24949e-93   
curr_pts_grp4-6                 2.75385         0.095539          28.8243         1.0627e-182   
curr_pts_grp7-9                 3.01058         0.142738          21.0916         9.49523e-99   
curr_pts_grp10-150              4.13354         0.125221          33.0101         5.82358e-239   
month02                        0.293929          0.19366          1.51775         0.129076   
month03                          0.3125         0.188539          1.65748         0.0974227   
month04                        0.613527         0.182634          3.35932         0.000781345   
month05                        0.693948         0.179114          3.87433         0.000106919   
month06                          0.7952         0.176515          4.50501         6.63707e-06   
month07                        0.829204         0.174268          4.75822         1.95306e-06   
month08                        0.606392         0.180957          3.35102         0.000805133   
month09                        0.867601         0.173497          5.00066         5.71336e-07   
month10                         0.70626         0.177096            3.988         6.66324e-05   
month11                        0.659379         0.179092          3.68179         0.000231603   
month12                       -0.385553          0.22615         -1.70486         0.0882211   
weekdayMonday                 -0.351422         0.111133         -3.16217         0.001566   
weekdayTuesday                -0.539431          0.11781         -4.57884         4.67566e-06   
weekdayWednesday              -0.620399         0.120959         -5.12899         2.91305e-07   
weekdayThursday               -0.591799         0.119863          -4.9373         7.92098e-07   
weekdayFriday                  -0.41414         0.113404          -3.6519         0.00026031   
weekdaySaturday               -0.164451         0.105494         -1.55887         0.119026  
```



```
AME          MER
-0.0166825668911789          -0.687280
-0.00932676440387397          -0.083899
-0.0785541503091296          -0.788284
-0.0920888845954956          -0.959114
-0.0895983895915046          -1.064694
-0.0838416577683526          -1.134618
-0.075240534445482          -1.162910

```



### All pairs of infractions 9 or over (speeding 81 or more and 10 other offences)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                     -27.876          58.7376        -0.474585         0.635082   
policyTRUE                    -0.124909          18.3361        -0.00681218         0.994565   
age_grp20-24                  -0.705009          53.1134        -0.0132737         0.989409   
age_grp25-34                  -0.385715          41.7835        -0.00923127         0.992635   
age_grp35-44                  -0.360098          41.2368        -0.00873246         0.993033   
age_grp45-54                  -0.502434           41.264        -0.0121761         0.990285   
age_grp55-64                  -0.894721          45.4858        -0.0196703         0.984306   
age_grp65-199                 -0.987958          49.5488        -0.0199391         0.984092   
curr_pts_grp1-3                -2.01502          83.4369        -0.0241502         0.980733   
curr_pts_grp4-6                -2.55786          237.178        -0.0107846         0.991395   
curr_pts_grp7-9                -2.16125          371.404        -0.00581913         0.995357   
curr_pts_grp10-150             -1.16499          351.059        -0.00331849         0.997352   
month02                        0.213558          47.4587         0.00449987          0.99641   
month03                        0.276242          45.8743         0.00602172         0.995195   
month04                        0.289575          46.0427         0.00628927         0.994982   
month05                        0.319592          45.3613         0.00704546         0.994379   
month06                        0.255414          46.3824         0.00550669         0.995606   
month07                        0.165809          46.9501         0.00353159         0.997182   
month08                        0.0782257          47.9511         0.00163136         0.998698   
month09                        0.346876          45.4911         0.00762515         0.993916   
month10                        0.329381          45.2536         0.00727855         0.994193   
month11                        0.303585          45.9107         0.00661252         0.994724   
month12                       -0.486642            56.11        -0.008673          0.99308   
weekdayMonday                  0.587323           39.323         0.0149359         0.988083   
weekdayTuesday                 0.724312           38.423         0.018851          0.98496   
weekdayWednesday               0.766064          38.1706         0.0200695         0.983988   
weekdayThursday                0.707272          38.5525         0.0183457         0.985363   
weekdayFriday                  0.554372          39.5933         0.0140017         0.988829   
weekdaySaturday                0.111825           43.402         0.00257649         0.997944  
```



```
AME          MER
-1.01431515713721e-08          0.000000
-7.20236435645834e-08          -0.000000
-4.60625186685986e-08          -0.000000
-4.36391299119483e-08          -0.000000
-5.82894088723879e-08          -0.000000
-8.90746677669405e-08          -0.000000
-9.66754367050624e-08          -0.000000

```
