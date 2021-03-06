# Logistic Regression Models - Female Drivers

## Logistic Regression Results 

## Regressions for Full Sample 



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.30602         0.00674405         -1379.89                0   
policyTRUE                    -0.017352         0.00186959         -9.28118         1.67609e-20   
age_grp20-24                   0.0791609         0.00554427           14.278         3.0025e-46   
age_grp25-34                  -0.0810565         0.00513611         -15.7817         4.15892e-56   
age_grp35-44                  -0.125117         0.00511915          -24.441         6.2685e-132   
age_grp45-54                  -0.315375         0.00516978         -61.0036                0   
age_grp55-64                  -0.546829         0.00551793         -99.1003                0   
age_grp65-199                 -0.901945         0.00632166         -142.675                0   
curr_pts_grp1-3                  1.4041         0.00223168          629.171                0   
curr_pts_grp4-6                 1.92678         0.00331652          580.966                0   
curr_pts_grp7-9                 2.20242         0.00529788          415.716                0   
curr_pts_grp10-150              2.45157         0.00710621          344.989                0   
month02                        0.237246         0.00480904          49.3334                0   
month03                        0.305791         0.00464033          65.8985                0   
month04                        0.299625         0.00471284          63.5763                0   
month05                        0.324298         0.00464758           69.778                0   
month06                         0.26294         0.00474534          55.4102                0   
month07                        0.168665         0.00480311          35.1157         3.87768e-270   
month08                        0.082054         0.00490677          16.7226         8.96888e-63   
month09                        0.386766         0.00461126          83.8741                0   
month10                        0.352562         0.00460018          76.6409                0   
month11                        0.337168         0.00465417          72.4444                0   
month12                       -0.558352         0.00584721         -95.4904                0   
weekdayMonday                   0.63536         0.00406389          156.343                0   
weekdayTuesday                  0.77763         0.00397011          195.871                0   
weekdayWednesday               0.825965         0.00394099          209.583                0   
weekdayThursday                0.763878         0.00398105          191.878                0   
weekdayFriday                  0.605883         0.00408776          148.219                0   
weekdaySaturday                0.133583         0.00449994          29.6855         1.18157e-193  
```



```
AME          MER
-0.381755353182951          -2.588759
2.66232293708777          11.454545
-2.38654422896763          -10.828467
-3.53307521348466          -16.357513
-7.55549946981194          -37.628858
-10.92567139478          -58.610551
-14.2490423332272          -82.702978

```



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -10.0138         0.0116457         -859.869                0   
policyTRUE                     0.987588         0.0119436          82.6875                0   
age_grp20-24                    0.79596         0.0114731          69.3764                0   
age_grp25-34                    0.63835         0.0110478          57.7806                0   
age_grp35-44                   0.579441         0.011029          52.5379                0   
age_grp45-54                   0.426987         0.0110652          38.5883                0   
age_grp55-64                   0.210398         0.0113841          18.4816         2.90175e-76   
age_grp65-199                 -0.128951         0.012165         -10.6002         2.97417e-26   
curr_pts_grp1-3                  1.3948         0.00223194          624.927                0   
curr_pts_grp4-6                 1.91713         0.0033167          578.022                0   
curr_pts_grp7-9                 2.19226         0.00529827          413.769                0   
curr_pts_grp10-150              2.43762         0.00710842           342.92                0   
month02                         0.23718         0.00480905          49.3195                0   
month03                        0.305604         0.00464034           65.858                0   
month04                        0.300191         0.00471286           63.696                0   
month05                        0.324759         0.0046476          69.8767                0   
month06                        0.263313         0.00474535          55.4887                0   
month07                        0.168995         0.00480312          35.1845         3.44827e-271   
month08                        0.082299         0.00490678          16.7725         3.8775e-63   
month09                        0.386944         0.00461127          83.9126                0   
month10                        0.352701         0.00460019           76.671                0   
month11                         0.33723         0.00465418          72.4576                0   
month12                       -0.558359         0.00584721         -95.4915                0   
weekdayMonday                  0.635356         0.0040639          156.342                0   
weekdayTuesday                 0.777635         0.00397012          195.872                0   
weekdayWednesday               0.825964         0.003941          209.583                0   
weekdayThursday                0.763899         0.00398105          191.884                0   
weekdayFriday                    0.6059         0.00408777          148.223                0   
weekdaySaturday                0.133586         0.00449995          29.6862         1.15855e-193   
policyTRUE:age_grp20-24        -1.01345         0.0132125         -76.7044                0   
policyTRUE:age_grp25-34        -1.01967         0.0125418         -81.3019                0   
policyTRUE:age_grp35-44       -0.990991         0.0125149         -79.1852                0   
policyTRUE:age_grp45-54        -1.06735         0.0126073         -84.6611                0   
policyTRUE:age_grp55-64        -1.09843         0.0131892         -83.2828                0   
policyTRUE:age_grp65-199       -1.13092         0.0145612         -77.6664                0  
```



```
AME          MER
21.4851418466598          114.402500
-21.824065784396          -93.482621
-17.6408019271003          -79.660947
-16.5014252928359          -76.022867
-12.7428342461106          -63.122279
-9.39089617821185          -50.079236
-6.08575767323023          -35.091173
19.2937912805772          82.637057
13.4428415183494          60.695569
11.5795057804677          53.340571
7.30907037457054          36.196602
2.98574458537107          15.916848
-1.42710929423703          -8.225432

```



### One-point violations (for speeding 11-20 over)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -11.8381         0.0219649         -538.957                0   
policyTRUE                     0.225472         0.00620235          36.3527         2.38347e-289   
age_grp20-24                  -0.0452505         0.0172516         -2.62297         0.00871668   
age_grp25-34                  -0.196713         0.0158051         -12.4461         1.46782e-35   
age_grp35-44                  -0.263263         0.0157744         -16.6892         1.57073e-62   
age_grp45-54                  -0.443555         0.0159323           -27.84         1.42231e-170   
age_grp55-64                  -0.660633         0.0170942         -38.6466                0   
age_grp65-199                  -1.05272         0.0199472          -52.775                0   
curr_pts_grp1-3                 1.34223         0.0073275          183.177                0   
curr_pts_grp4-6                 1.72183         0.0115957          148.489                0   
curr_pts_grp7-9                 1.97325         0.0188416          104.728                0   
curr_pts_grp10-150              2.40945         0.0229363           105.05                0   
month02                         0.29239         0.0161997          18.0492         8.01e-73   
month03                        0.409325         0.0154888          26.4271         6.68688e-154   
month04                        0.321128         0.0159758          20.1008         7.25601e-90   
month05                         0.39774         0.015606          25.4863         2.79522e-143   
month06                        0.362535         0.0158239          22.9107         3.63843e-116   
month07                        0.195687         0.0162577          12.0366         2.28207e-33   
month08                        0.179309         0.0163576          10.9619         5.82943e-28   
month09                        0.616179         0.0150294          40.9982                0   
month10                        0.412442         0.015491          26.6246         3.52305e-156   
month11                        0.344225         0.0158532          21.7133         1.53738e-104   
month12                       -0.503159         0.0195725         -25.7075         9.63543e-146   
weekdayMonday                  0.785581         0.0137443          57.1567                0   
weekdayTuesday                 0.916804         0.0134761          68.0321                0   
weekdayWednesday               0.961639         0.0133968          71.7813                0   
weekdayThursday                0.890849         0.013541          65.7891                0   
weekdayFriday                  0.698867         0.0139475          50.1069                0   
weekdaySaturday                0.143968         0.015562          9.25127         2.21841e-20  
```



```
AME          MER
0.456591982142657          2.605716
-0.129840119779488          -0.476925
-0.499816957930908          -1.925037
-0.63625386282745          -2.495170
-0.92296119681742          -3.862031
-1.16613650934507          -5.212079
-1.46361022651652          -7.018313

```



### Two-point violations (speeding 21-30 over or 7 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -10.3294         0.0103704         -996.047                0   
policyTRUE                     0.0448531         0.00275394          16.2869         1.22288e-59   
age_grp20-24                   0.130283         0.00860418          15.1418         8.58349e-52   
age_grp25-34                   0.0430688         0.0079475          5.41917         5.98762e-08   
age_grp35-44                   0.0182958         0.00791388          2.31186         0.0207852   
age_grp45-54                  -0.151768         0.00797038         -19.0415         7.7313e-81   
age_grp55-64                  -0.397043         0.00845091         -46.9822                0   
age_grp65-199                 -0.838015         0.00972007         -86.2149                0   
curr_pts_grp1-3                 1.37451         0.00328078          418.958                0   
curr_pts_grp4-6                 1.85935         0.00496538          374.464                0   
curr_pts_grp7-9                 2.07687         0.00818486          253.745                0   
curr_pts_grp10-150              2.23101         0.0115227          193.618                0   
month02                        0.284835         0.00719658          39.5792                0   
month03                        0.401782         0.00688201          58.3815                0   
month04                        0.380353         0.00700105           54.328                0   
month05                        0.404847         0.0069105          58.5844                0   
month06                        0.309661         0.00710043          43.6115                0   
month07                        0.192318         0.00722169          26.6306         3.00238e-156   
month08                        0.112194         0.00736764          15.2279         2.30719e-52   
month09                        0.467483         0.00685925          68.1537                0   
month10                        0.418154         0.00686462          60.9144                0   
month11                        0.391551         0.00696049          56.2533                0   
month12                       -0.616867         0.00901072         -68.4592                0   
weekdayMonday                  0.715527         0.00607852          117.714                0   
weekdayTuesday                 0.856746         0.0059464          144.078                0   
weekdayWednesday               0.891328         0.00591715          150.635                0   
weekdayThursday                 0.82996         0.00597421          138.924                0   
weekdayFriday                  0.659913         0.00613983          107.481                0   
weekdaySaturday                 0.15529         0.00678841          22.8757         8.10516e-116  
```



```
AME          MER
0.45503831381738          2.712600
1.75840435240621          7.223763
0.530076770045958          2.284807
0.218392756037485          0.958598
-1.55582966995129          -7.311102
-3.37641789388535          -17.015854
-5.42656727318194          -29.468081

```



### Three-point violations (speeding 31-60 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.98598         0.0101262         -986.149                0   
policyTRUE                     -0.11551         0.00286511         -40.3159                0   
age_grp20-24                   0.0865135         0.00839712          10.3028         6.84689e-25   
age_grp25-34                  -0.118574         0.00781952         -15.1638         6.13845e-52   
age_grp35-44                  -0.163796         0.00779438         -21.0147         4.81659e-98   
age_grp45-54                   -0.37196         0.00788728         -47.1595                0   
age_grp55-64                  -0.589138         0.00842871         -69.8966                0   
age_grp65-199                 -0.860783         0.00952698         -90.3522                0   
curr_pts_grp1-3                 1.44931         0.00341964          423.819                0   
curr_pts_grp4-6                 2.02716         0.00495235          409.333                0   
curr_pts_grp7-9                 2.34974         0.00770226          305.072                0   
curr_pts_grp10-150              2.61293         0.0102584           254.71                0   
month02                        0.180757         0.00719206          25.1328         2.1774e-139   
month03                        0.186695         0.00703058          26.5547         2.26409e-155   
month04                        0.208327         0.00710786          29.3095         7.85753e-189   
month05                        0.221053         0.00702021          31.4881         1.26322e-217   
month06                        0.189383         0.00712792          26.5691         1.54451e-155   
month07                        0.133683         0.00715338           18.688         6.19381e-78   
month08                        0.0224724         0.00735083          3.05712         0.00223474   
month09                        0.239264         0.00703153          34.0273         8.79383e-254   
month10                        0.265139         0.00691947          38.3178                0   
month11                        0.276632         0.00696014          39.7452                0   
month12                       -0.530967         0.00856142         -62.0186                0   
weekdayMonday                  0.558097         0.00616433          90.5366                0   
weekdayTuesday                  0.70973         0.00600575          118.175                0   
weekdayWednesday               0.774684         0.00594325          130.347                0   
weekdayThursday                0.711789         0.00600609          118.511                0   
weekdayFriday                  0.566296         0.00615939          91.9403                0   
weekdaySaturday                0.119978         0.0067549          17.7617         1.39942e-70  
```



```
AME          MER
-1.08250960573466          -8.292573
1.36215821490569          6.299944
-1.58959373322356          -7.796288
-2.10042683624215          -10.534796
-3.99064402633332          -21.661079
-5.27946010345996          -31.048724
-6.29040256595011          -40.256566

```



### Four-point violations (speeding 31-45 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -12.6083         0.0753283         -167.377                0   
policyTRUE                    -0.0478829         0.0295766         -1.61895         0.105459   
age_grp20-24                   -0.80711         0.0441669         -18.2741         1.33095e-74   
age_grp25-34                   -1.76244         0.0443148         -39.7709                0   
age_grp35-44                   -2.25249         0.0491511         -45.8278                0   
age_grp45-54                   -2.77975         0.057082         -48.6974                0   
age_grp55-64                    -3.2786         0.0809667         -40.4932                0   
age_grp65-199                  -3.24465         0.0960756         -33.7719         5.10618e-250   
curr_pts_grp1-3                 1.13291         0.0382841          29.5922         1.88303e-192   
curr_pts_grp4-6                 1.83865         0.052172          35.2421         4.54105e-272   
curr_pts_grp7-9                 2.20787         0.0773676          28.5374         4.03086e-179   
curr_pts_grp10-150              3.00962         0.0751369          40.0551                0   
month02                        0.0907966         0.0787512          1.15296         0.248929   
month03                         0.16514         0.0756139          2.18398         0.0289634   
month04                        0.277103         0.0754036          3.67492         0.00023792   
month05                        0.332081         0.0737817          4.50087         6.7677e-06   
month06                        0.290178         0.0749243          3.87295         0.000107528   
month07                        0.242759         0.0750902           3.2329         0.00122542   
month08                        0.262393         0.0746288          3.51597         0.000438146   
month09                        0.398976         0.0728962          5.47321         4.4195e-08   
month10                        0.414298         0.0720667          5.74882         8.98709e-09   
month11                         0.30024         0.0742294          4.04476         5.23775e-05   
month12                       -0.0840891         0.0806679         -1.04241         0.297221   
weekdayMonday                  -0.03389         0.0577461         -0.58688         0.557285   
weekdayTuesday                 0.0916384         0.0559887          1.63673         0.101687   
weekdayWednesday               0.0656807         0.0563333          1.16593         0.243643   
weekdayThursday                0.136749         0.0554477          2.46627         0.0136527   
weekdayFriday                  0.220462         0.0543999          4.05262         5.06466e-05   
weekdaySaturday                 0.15199         0.0552099          2.75294         0.00590627  
```



```
AME          MER
-0.00428531248673292          -0.054053
-0.398611393632652          -1.435181
-0.560117977774474          -2.146549
-0.590601444095431          -2.318842
-0.576244222151981          -2.430486
-0.552246705137749          -2.493645
-0.515526608386209          -2.490273

```



### Five-point violations (speeding 46-60 over or a handheld device violation)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -12.6369         0.0564287         -223.945                0   
policyTRUE                    -0.769038         0.0188086         -40.8875                0   
age_grp20-24                   0.117962         0.042656          2.76544         0.00568462   
age_grp25-34                  -0.396454         0.0409934         -9.67117         3.9979e-22   
age_grp35-44                  -0.773369         0.0420712         -18.3824         1.81743e-75   
age_grp45-54                   -1.19845         0.0441592         -27.1393         3.38661e-162   
age_grp55-64                   -1.58889         0.0516531         -30.7608         8.7611e-208   
age_grp65-199                  -2.30918         0.0747942         -30.8738         2.68751e-209   
curr_pts_grp1-3                 1.66299         0.0214811          77.4165                0   
curr_pts_grp4-6                 2.39729         0.0283487          84.5644                0   
curr_pts_grp7-9                 2.89852         0.0393904          73.5845                0   
curr_pts_grp10-150              3.44733         0.0451811          76.3003                0   
month02                        0.197491         0.0508733          3.88201         0.000103596   
month03                        0.260189         0.0490567          5.30384         1.13389e-07   
month04                        0.444822         0.048228          9.22331         2.88068e-20   
month05                        0.512876         0.0472563          10.8531         1.9281e-27   
month06                        0.510284         0.0475093          10.7407         6.55315e-27   
month07                          0.5586         0.046713          11.9581         5.88766e-33   
month08                         0.47596         0.0474227          10.0366         1.05284e-23   
month09                        0.669347         0.0459461          14.5681         4.48206e-48   
month10                        0.618382         0.0460518           13.428         4.14527e-41   
month11                        0.529754         0.0470157          11.2676         1.89657e-29   
month12                       -0.401818         0.0584396         -6.87579         6.1646e-12   
weekdayMonday                 -0.0990009         0.0320009         -3.09369         0.00197683   
weekdayTuesday                -0.223868         0.0331201         -6.75927         1.38689e-11   
weekdayWednesday              -0.164943         0.0325916          -5.0609         4.17274e-07   
weekdayThursday               -0.228735         0.0331824         -6.89325         5.45308e-12   
weekdayFriday                 -0.146451         0.032453         -4.51271         6.40046e-06   
weekdaySaturday               -0.0819333         0.0318768         -2.57031         0.0101607  
```



```
AME          MER
-0.180917641344101          -3.414813
0.109468651792149          0.708162
-0.262543320897946          -1.851302
-0.41758185742289          -3.046207
-0.485852303634553          -3.950096
-0.496663878322854          -4.501681
-0.501922245726256          -5.094556

```



### Seven-point violations (speeding 61-80 over or combinations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -14.4808         0.186915         -77.4727                0   
policyTRUE                    -0.973888         0.0700535         -13.9021         6.15494e-44   
age_grp20-24                  -0.107306         0.116859        -0.918246          0.35849   
age_grp25-34                   -1.12365          0.11821         -9.50557         1.98949e-21   
age_grp35-44                   -1.68886         0.127641         -13.2313         5.78594e-40   
age_grp45-54                   -2.30535         0.146592         -15.7263         9.98158e-56   
age_grp55-64                   -3.13475         0.227399         -13.7853         3.12627e-43   
age_grp65-199                  -3.87362         0.390966         -9.90782         3.84964e-23   
curr_pts_grp1-3                 1.64216         0.0826165          19.8769         6.44691e-88   
curr_pts_grp4-6                 2.69927         0.0955074          28.2624         1.00231e-175   
curr_pts_grp7-9                 2.95557         0.142723          20.7084         2.9082e-95   
curr_pts_grp10-150              4.07749         0.125217          32.5634         1.35335e-232   
month02                         0.29392          0.19366          1.51771         0.129087   
month03                        0.312468         0.188539          1.65731         0.0974573   
month04                        0.613723         0.182636          3.36037         0.000778378   
month05                        0.694127         0.179115          3.87531         0.000106491   
month06                        0.795355         0.176515          4.50587         6.61026e-06   
month07                         0.82933         0.174268          4.75893         1.94622e-06   
month08                        0.606497         0.180957           3.3516         0.00080346   
month09                        0.867683         0.173498          5.00113         5.69966e-07   
month10                        0.706311         0.177096          3.98829         6.65519e-05   
month11                        0.659406         0.179092          3.68194         0.000231465   
month12                       -0.385528          0.22615         -1.70475         0.0882419   
weekdayMonday                 -0.351426         0.111133          -3.1622         0.00156581   
weekdayTuesday                 -0.53943          0.11781         -4.57883         4.67593e-06   
weekdayWednesday              -0.620398         0.120959         -5.12898         2.91312e-07   
weekdayThursday               -0.591794         0.119863         -4.93726         7.92279e-07   
weekdayFriday                 -0.414136         0.113404         -3.65186         0.000260348   
weekdaySaturday               -0.164449         0.105494         -1.55886         0.119031  
```



```
AME          MER
-0.0173688115604293          -0.689190
-0.0146420096780702          -0.125432
-0.0869490296657228          -0.832003
-0.100332193187904          -1.005036
-0.0973633339152479          -1.109830
-0.0910956758965981          -1.179129
-0.0819122993309886          -1.207147

```



### All pairs of infractions 9 or over (speeding 81 or more and 10 other offences)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -27.8684          60.6759        -0.459301         0.646018   
policyTRUE                    -0.124941          18.7237        -0.0066729         0.994676   
age_grp20-24                  -0.707669          55.1883        -0.0128228         0.989769   
age_grp25-34                  -0.389264          43.6964        -0.00890837         0.992892   
age_grp35-44                  -0.364051          43.0632        -0.00845386         0.993255   
age_grp45-54                  -0.506347          43.0945        -0.0117497         0.990625   
age_grp55-64                  -0.898102          47.2883        -0.0189921         0.984847   
age_grp65-199                 -0.990694          51.3486        -0.0192935         0.984607   
curr_pts_grp1-3                -2.01864          83.4741        -0.0241829         0.980707   
curr_pts_grp4-6                -2.56164           237.25        -0.0107972         0.991385   
curr_pts_grp7-9                -2.16511          371.515        -0.0058278          0.99535   
curr_pts_grp10-150             -1.16884          351.146        -0.00332864         0.997344   
month02                        0.213342          48.4617         0.00440228         0.996488   
month03                         0.27595          46.8451         0.0058907           0.9953   
month04                        0.289316          47.0108         0.00615425          0.99509   
month05                        0.319295          46.3158         0.00689386           0.9945   
month06                         0.25517          47.3584         0.00538808         0.995701   
month07                        0.165631           47.938         0.00345512         0.997243   
month08                        0.0781179          48.9593         0.00159557         0.998727   
month09                        0.346487          46.4514         0.00745912         0.994049   
month10                        0.329025          46.2091         0.00712035         0.994319   
month11                        0.303256          46.8804         0.0064687         0.994839   
month12                       -0.486176          57.2845        -0.00848704         0.993228   
weekdayMonday                  0.586785          40.1458         0.0146163         0.988338   
weekdayTuesday                 0.723648          39.2275         0.0184475         0.985282   
weekdayWednesday               0.765362            38.97         0.0196398         0.984331   
weekdayThursday                0.706624          39.3597         0.017953         0.985676   
weekdayFriday                  0.553857          40.4218         0.0137019         0.989068   
weekdaySaturday                0.111709          44.3072         0.00252124         0.997988  
```



```
AME          MER
-1.012304855559e-08          0.000000
-7.21837339453161e-08          -0.000000
-4.64405429650349e-08          -0.000000
-4.41689320379441e-08          -0.000000
-5.88513729383461e-08          -0.000000
-8.96612752470767e-08          -0.000000
-9.73132320389948e-08          -0.000000

```

