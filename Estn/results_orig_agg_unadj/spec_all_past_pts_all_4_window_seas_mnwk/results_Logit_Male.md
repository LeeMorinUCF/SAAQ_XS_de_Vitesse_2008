# Logistic Regression Models - Male Drivers

## Logistic Regression Results 

## Regressions for Full Sample 



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.20759         0.0194891         -472.448                0   
policyTRUE                    -0.111281         0.00119941         -92.7805                0   
age_grp16-19                    1.21701         0.0194597          62.5399                0   
age_grp20-24                    1.06763         0.0193656          55.1303                0   
age_grp25-34                   0.885448         0.0193295           45.808                0   
age_grp35-44                   0.784723         0.0193303          40.5955                0   
age_grp45-54                   0.691273         0.019332           35.758         4.97303e-280   
age_grp55-64                   0.539371         0.0193616          27.8578         8.66231e-171   
age_grp65-199                  0.201894         0.0194282          10.3918         2.70115e-25   
curr_pts_grp1-3                 1.08316         0.00147455          734.568                0   
curr_pts_grp4-6                  1.5342         0.00183329          836.856                0   
curr_pts_grp7-9                 1.77923         0.00241064          738.072                0   
curr_pts_grp10-150              2.00421         0.00250451          800.239                0   
month02                        0.181574         0.00301516          60.2201                0   
month03                        0.208627         0.00293331          71.1236                0   
month04                        0.182377         0.00299718          60.8496                0   
month05                        0.211402         0.00295005          71.6605                0   
month06                        0.187678         0.00298879          62.7939                0   
month07                        0.155454         0.00298435          52.0895                0   
month08                        0.0484484         0.00306109          15.8272         2.02096e-56   
month09                        0.286487         0.0029159          98.2501                0   
month10                        0.248718         0.00291246          85.3979                0   
month11                         0.21794         0.00295577          73.7337                0   
month12                       -0.544822         0.00360294         -151.216                0   
weekdayMonday                  0.349487         0.00243387          143.593                0   
weekdayTuesday                 0.440758         0.00239014          184.407                0   
weekdayWednesday               0.490753         0.00236759          207.279                0   
weekdayThursday                0.445137         0.00238973          186.271                0   
weekdayFriday                  0.375167         0.0024231           154.83                0   
weekdaySaturday                0.0914519         0.00257829          35.4699         1.43002e-275  
```



```
AME          MER
-5.83457056341734          -23.501063
80.4845221527286          182.541327
66.7577715152974          146.609353
41.7481225762924          109.437930
32.0436486021016          91.604699
24.9270853022469          76.586147
16.5589202098645          54.971247
4.59397406101816          17.208399

```



### All violations combined


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.25149         0.0271623         -340.601                0   
policyTRUE                    -0.0195442         0.0385736        -0.506673         0.612384   
age_grp16-19                    1.27069         0.0272716          46.5941                0   
age_grp20-24                    1.13008         0.0271269           41.659                0   
age_grp25-34                   0.948172         0.0270812          35.0121         1.47138e-268   
age_grp35-44                   0.827473         0.0270834          30.5528         5.19655e-205   
age_grp45-54                   0.725016         0.0270894          26.7638         8.52657e-158   
age_grp55-64                   0.566857         0.0271349          20.8903         6.56039e-97   
age_grp65-199                  0.197146         0.0272433          7.23651         4.60378e-13   
curr_pts_grp1-3                 1.08274         0.00147455          734.282                0   
curr_pts_grp4-6                 1.53384         0.00183323          836.687                0   
curr_pts_grp7-9                  1.7789         0.00241063          737.938                0   
curr_pts_grp10-150              2.00514         0.00250442          800.641                0   
month02                        0.181563         0.00301517          60.2166                0   
month03                        0.208608         0.00293331          71.1171                0   
month04                        0.182446         0.00299719          60.8725                0   
month05                        0.211474         0.00295005          71.6849                0   
month06                        0.187749         0.00298879          62.8177                0   
month07                        0.155519         0.00298436          52.1115                0   
month08                        0.0485052         0.00306109          15.8457         1.50489e-56   
month09                        0.286531         0.0029159          98.2649                0   
month10                         0.24875         0.00291247          85.4087                0   
month11                        0.217958         0.00295577            73.74                0   
month12                       -0.544813         0.00360294         -151.213                0   
weekdayMonday                  0.349486         0.00243387          143.593                0   
weekdayTuesday                 0.440759         0.00239014          184.407                0   
weekdayWednesday               0.490753         0.00236759          207.279                0   
weekdayThursday                0.445139         0.00238973          186.272                0   
weekdayFriday                  0.375169         0.0024231           154.83                0   
weekdaySaturday                0.0914523         0.0025783          35.4701         1.42212e-275   
policyTRUE:age_grp16-19       -0.110693         0.038911         -2.84479         0.00444411   
policyTRUE:age_grp20-24       -0.129964         0.0387191         -3.35658         0.000789138   
policyTRUE:age_grp25-34       -0.130092         0.0386565         -3.36532         0.000764542   
policyTRUE:age_grp35-44       -0.0891417         0.0386617         -2.30569         0.0211282   
policyTRUE:age_grp45-54        -0.07131         0.0386672          -1.8442         0.0651539   
policyTRUE:age_grp55-64       -0.0593858         0.0387282          -1.5334         0.125178   
policyTRUE:age_grp65-199       0.00114587         0.0388643         0.0294837         0.976479  
```



```
AME          MER
-0.371753598945313          -1.424745
-10.6130394307935          -24.059979
-10.8707763506863          -23.864489
-7.60297768544819          -19.923342
-4.50138553630107          -12.863727
-3.10649002215991          -9.541058
-2.08144316373571          -6.907698
0.0269396951723994          0.100887
83.0569944620784          188.336634
70.1629959294777          154.046514
44.3542645278904          116.244839
33.1280545033918          94.688382
25.4949699229914          78.318354
16.9058559936616          56.113951
4.28258121283058          16.039209

```



### One-point violations (for speeding 11-20 over)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -12.4296         0.0871711         -142.588                0   
policyTRUE                     0.0953111         0.00425351          22.4076         3.31795e-111   
age_grp16-19                    1.61247         0.0870857          18.5159         1.53565e-76   
age_grp20-24                    1.27642         0.0868508          14.6967         6.76403e-49   
age_grp25-34                    1.27697         0.0867014          14.7283         4.24069e-49   
age_grp35-44                    1.30377         0.0866888          15.0396         4.03765e-51   
age_grp45-54                     1.2599         0.0866868          14.5339         7.38469e-48   
age_grp55-64                    1.13589         0.086756          13.0929         3.61605e-39   
age_grp65-199                  0.745527         0.086939          8.57528         9.88418e-18   
curr_pts_grp1-3                 1.08957         0.00518973          209.948                0   
curr_pts_grp4-6                   1.498         0.00659642          227.093                0   
curr_pts_grp7-9                 1.79562         0.00857751           209.34                0   
curr_pts_grp10-150              2.12693         0.00861131          246.993                0   
month02                        0.210212         0.0106246          19.7854         3.97401e-87   
month03                        0.230535         0.0103516          22.2704         7.15772e-110   
month04                        0.164913         0.0106682          15.4584         6.62586e-54   
month05                         0.24146         0.0103923          23.2345         2.04136e-119   
month06                        0.195935         0.010577          18.5246         1.30659e-76   
month07                        0.126558         0.010648          11.8856         1.40565e-32   
month08                        0.0421981         0.0108781          3.87919         0.000104806   
month09                        0.380764         0.0101304          37.5862                0   
month10                        0.236343         0.0103534          22.8276         2.44021e-115   
month11                         0.17931         0.010581          16.9465         2.04159e-64   
month12                       -0.545867         0.0127773         -42.7217                0   
weekdayMonday                  0.490077         0.0088025          55.6747                0   
weekdayTuesday                 0.605756         0.00861737          70.2948                0   
weekdayWednesday               0.622473         0.00859484          72.4241                0   
weekdayThursday                0.575266         0.00867299          66.3284                0   
weekdayFriday                  0.454895         0.0088678          51.2974                0   
weekdaySaturday                0.091765         0.00958663          9.57219         1.04663e-21  
```



```
AME          MER
0.399264116056791          1.187184
6.15394718244306          13.301452
4.11159776829681          8.559945
3.41774874638018          8.566390
3.23777673653091          8.889045
2.82948232343387          8.365402
2.18952952285614          7.003414
1.01609307602096          3.669422

```



### Two-point violations (speeding 21-30 over or 7 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -11.2708         0.0517344         -217.858                0   
policyTRUE                    -0.0191113         0.00190777         -10.0176         1.27507e-23   
age_grp16-19                    2.00533         0.0517516          38.7492                0   
age_grp20-24                    1.96273         0.0516247          38.0191                0   
age_grp25-34                    1.90447         0.0515788          36.9235         1.93686e-298   
age_grp35-44                    1.87555         0.0515764          36.3644         1.55464e-289   
age_grp45-54                    1.83018         0.0515757          35.4852         8.30206e-276   
age_grp55-64                    1.69737         0.0515997           32.895         2.58944e-237   
age_grp65-199                   1.30941         0.0516619          25.3459         9.98025e-142   
curr_pts_grp1-3                 1.07696         0.0023142          465.373                0   
curr_pts_grp4-6                 1.49432         0.00293172          509.708                0   
curr_pts_grp7-9                  1.7044         0.00394955          431.542                0   
curr_pts_grp10-150              1.82742         0.00432219          422.799                0   
month02                        0.218549         0.00476318           45.883                0   
month03                         0.26876         0.00461238          58.2694                0   
month04                        0.209468         0.00474294          44.1642                0   
month05                        0.231217         0.00467797          49.4267                0   
month06                        0.151434         0.00480099          31.5424         2.2813e-218   
month07                        0.104917         0.00480933          21.8152         1.66356e-105   
month08                       -0.00886312         0.00494753         -1.79142         0.0732252   
month09                        0.300113         0.00463095          64.8059                0   
month10                        0.258098         0.00462975          55.7476                0   
month11                         0.24226         0.00468444          51.7159                0   
month12                       -0.606901         0.00585632         -103.632                0   
weekdayMonday                  0.427674         0.00390927            109.4                0   
weekdayTuesday                 0.514281         0.00384536          133.741                0   
weekdayWednesday               0.557256         0.00381566          146.044                0   
weekdayThursday                 0.51056         0.00385093          132.581                0   
weekdayFriday                  0.423092         0.00391602          108.041                0   
weekdaySaturday                0.0856271         0.00421315          20.3238         7.92786e-92  
```



```
AME          MER
-0.395973782485448          -1.301385
28.2450847278054          62.116593
27.5385613684869          59.124609
21.8630445662224          55.233968
19.5231660500408          53.384773
17.2985656590868          50.589596
13.6983831769096          43.099614
7.40017656554396          26.137202

```



### Three-point violations (speeding 31-60 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -9.50278         0.0233447         -407.065                0   
policyTRUE                    -0.187157         0.00173325         -107.981                0   
age_grp16-19                   0.840355         0.0232944          36.0755         5.50323e-285   
age_grp20-24                   0.749479         0.023126          32.4085         2.08023e-230   
age_grp25-34                   0.522401         0.0230709          22.6433         1.62334e-113   
age_grp35-44                   0.384797         0.023075          16.6759         1.96118e-62   
age_grp45-54                   0.255595         0.0230813          11.0737         1.68313e-28   
age_grp55-64                   0.0896033         0.0231379          3.87258         0.000107691   
age_grp65-199                 -0.190612         0.0232481         -8.19906         2.42276e-16   
curr_pts_grp1-3                 1.09529         0.00214121          511.526                0   
curr_pts_grp4-6                 1.57168         0.00262903          597.816                0   
curr_pts_grp7-9                 1.82474         0.00342674            532.5                0   
curr_pts_grp10-150              2.05988         0.00353489          582.727                0   
month02                        0.150806         0.00433902          34.7559         1.13068e-264   
month03                        0.156115         0.00424097          36.8111         1.22519e-296   
month04                        0.150027         0.00431628          34.7584         1.03425e-264   
month05                        0.174067         0.00425129          40.9445                0   
month06                        0.190232         0.00426842          44.5674                0   
month07                        0.170407         0.00425026          40.0934                0   
month08                        0.0599583         0.00436108          13.7485         5.20133e-43   
month09                        0.233208         0.00421645          55.3092                0   
month10                        0.221469         0.00418657          52.8999                0   
month11                        0.190544         0.00424908          44.8435                0   
month12                       -0.526657         0.00511981         -102.867                0   
weekdayMonday                  0.325655         0.00351711          92.5918                0   
weekdayTuesday                  0.42293         0.00344909          122.621                0   
weekdayWednesday               0.486279         0.0034073          142.717                0   
weekdayThursday                0.437084         0.00344144          127.006                0   
weekdayFriday                  0.370766         0.00348735          106.317                0   
weekdaySaturday                0.100206         0.00370005          27.0822         1.59393e-161  
```



```
AME          MER
-4.70863623164706          -21.266854
33.0245501703802          77.589051
28.9774249416934          65.739630
14.841347026666          40.428006
9.2804478893387          27.658867
5.34566401753358          17.165250
1.5886844046952          5.525753
-2.59780403866798          -10.231776

```



### Four-point violations (speeding 31-45 over or 9 other violations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -11.3558         0.0728126          -155.96                0   
policyTRUE                     -0.12517         0.0114167         -10.9637         5.70935e-28   
age_grp16-19                     1.1017         0.0689728          15.9729         1.97407e-57   
age_grp20-24                  -0.0868621         0.0691419         -1.25629         0.209012   
age_grp25-34                   -1.13045         0.0693255         -16.3065         8.87731e-60   
age_grp35-44                   -1.90872         0.0705333         -27.0613         2.81436e-161   
age_grp45-54                   -2.45551         0.0720588         -34.0765         1.64238e-254   
age_grp55-64                   -2.79066         0.0756329         -36.8974         5.08048e-298   
age_grp65-199                  -3.06336         0.0804463         -38.0796                0   
curr_pts_grp1-3                0.575663         0.0154951          37.1512         4.18725e-302   
curr_pts_grp4-6                 1.20489          0.01738          69.3263                0   
curr_pts_grp7-9                  1.5512         0.0209137          74.1716                0   
curr_pts_grp10-150              2.01696         0.0183715          109.787                0   
month02                        0.0598469         0.0313982          1.90606         0.0566419   
month03                        0.0739804         0.0305998          2.41767         0.0156201   
month04                        0.222895         0.0300877          7.40818         1.28044e-13   
month05                        0.354748         0.0289767          12.2425         1.84221e-34   
month06                        0.410018         0.0288226          14.2256         6.35859e-46   
month07                        0.427473         0.0285204          14.9883         8.75649e-51   
month08                        0.397509         0.0286274          13.8856         7.74228e-44   
month09                        0.382105         0.0288755          13.2328         5.67203e-40   
month10                        0.371055         0.028724           12.918         3.56469e-38   
month11                        0.267541         0.0295005          9.06905         1.2006e-19   
month12                        0.00267948         0.0311267         0.0860831           0.9314   
weekdayMonday                 -0.250895         0.0221019         -11.3517         7.27148e-30   
weekdayTuesday                -0.197849         0.0217815         -9.08336         1.05277e-19   
weekdayWednesday              -0.156953         0.0215386         -7.28705         3.16807e-13   
weekdayThursday               -0.106976         0.0212724         -5.02885         4.93439e-07   
weekdayFriday                  0.0625848         0.0203803          3.07086         0.00213446   
weekdaySaturday                0.169007         0.0198623          8.50889         1.75602e-17  
```



```
AME          MER
-0.072511071158636          -0.502382
4.95110133388778          9.357180
-0.21716071538093          -0.387485
-1.48618274153001          -3.153724
-1.72040639080315          -3.967049
-1.73558582795258          -4.257935
-1.67765015656815          -4.371766
-1.56884830104655          -4.440005

```



### Five-point violations (speeding 46-60 over or a handheld device violation)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -15.3228         0.497217         -30.8172         1.54197e-208   
policyTRUE                    -0.647036         0.00796054         -81.2805                0   
age_grp16-19                    4.56879         0.497047          9.19187         3.86063e-20   
age_grp20-24                    4.34947         0.496971          8.75196         2.09682e-18   
age_grp25-34                    3.87686         0.496951           7.8013         6.12716e-15   
age_grp35-44                    3.34289         0.496981          6.72638         1.73933e-11   
age_grp45-54                    2.96443         0.497017          5.96444         2.45471e-09   
age_grp55-64                    2.52424         0.497169          5.07722         3.82996e-07   
age_grp65-199                   1.77987         0.497689          3.57627         0.000348529   
curr_pts_grp1-3                 1.18526         0.00992771          119.389                0   
curr_pts_grp4-6                 1.71359         0.0115365          148.536                0   
curr_pts_grp7-9                 2.02257          0.01413           143.14                0   
curr_pts_grp10-150              2.48949         0.0130319          191.032                0   
month02                        0.153031         0.0218131          7.01555         2.29039e-12   
month03                        0.235982         0.0209402          11.2693         1.85993e-29   
month04                        0.380979         0.0206541          18.4456         5.65208e-76   
month05                        0.416202         0.0203693          20.4328         8.54016e-93   
month06                        0.498418         0.0201428          24.7442         3.57946e-135   
month07                        0.562457         0.0197642          28.4583         3.84336e-178   
month08                        0.471678         0.0200884          23.4801         6.51947e-122   
month09                        0.641589         0.0195469           32.823         2.76605e-236   
month10                        0.582333         0.0196372          29.6546         2.96261e-193   
month11                        0.459049         0.0201908          22.7356         1.99212e-114   
month12                       -0.390819         0.024679         -15.8361         1.75375e-56   
weekdayMonday                 -0.200742         0.0141391         -14.1977         9.47085e-46   
weekdayTuesday                -0.242855         0.0143154         -16.9646         1.50144e-64   
weekdayWednesday              -0.201404         0.0141541         -14.2293         6.0265e-46   
weekdayThursday               -0.224564         0.0142518         -15.7569         6.1572e-56   
weekdayFriday                 -0.0766119         0.0136978           -5.593         2.23178e-08   
weekdaySaturday               -0.00512966         0.0134387        -0.381707         0.702679  
```



```
AME          MER
-0.81231361830261          -6.508975
6.67909190419048          16.837428
5.65899487526231          13.487207
2.801319591728          8.341550
1.44674085004723          4.817556
0.890160407198489          3.244089
0.505682316916249          2.026088
0.187845361681897          0.869835

```



### Seven-point violations (speeding 61-80 over or combinations)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                    -16.6128         0.985048         -16.8649         8.15122e-64   
policyTRUE                    -0.739167         0.0192953         -38.3082                0   
age_grp16-19                    4.64294          0.98439          4.71657         2.39858e-06   
age_grp20-24                    4.23177         0.984276          4.29937         1.71286e-05   
age_grp25-34                    3.52958         0.984257          3.58604         0.000335736   
age_grp35-44                    2.63043         0.984448          2.67198         0.00754051   
age_grp45-54                    1.92845         0.984794          1.95823         0.0502036   
age_grp55-64                    1.17387         0.986244          1.19024         0.233952   
age_grp65-199                   0.12363          0.99252         0.124561         0.900871   
curr_pts_grp1-3                 1.10355         0.0251167           43.937                0   
curr_pts_grp4-6                 1.66413         0.0283365          58.7272                0   
curr_pts_grp7-9                 2.05698         0.0329568          62.4144                0   
curr_pts_grp10-150              2.76582         0.0277119          99.8061                0   
month02                        0.0354175         0.0555306         0.637801         0.523603   
month03                        0.246325          0.05164          4.77005         1.84181e-06   
month04                         0.47069         0.0502101          9.37441         6.95625e-21   
month05                        0.554429         0.0491399          11.2827         1.59813e-29   
month06                        0.705155         0.0480154           14.686         7.92277e-49   
month07                        0.638263         0.0482737          13.2217         6.57201e-40   
month08                        0.608703         0.0484946           12.552         3.87726e-36   
month09                        0.766479         0.0473253           16.196         5.3852e-59   
month10                        0.591033         0.0485297          12.1788         4.03198e-34   
month11                        0.445161         0.0500837          8.88833         6.20322e-19   
month12                       -0.478807         0.0625793          -7.6512         1.99113e-14   
weekdayMonday                 -0.392089         0.0337935         -11.6025         4.00171e-31   
weekdayTuesday                -0.359498         0.0334887         -10.7349         6.98071e-27   
weekdayWednesday              -0.471472         0.0346502         -13.6066         3.65746e-42   
weekdayThursday                 -0.4376         0.0343051         -12.7561         2.88175e-37   
weekdayFriday                 -0.199832         0.0320223          -6.2404         4.36442e-10   
weekdaySaturday                0.00296377         0.0303345         0.0977031         0.922168  
```



```
AME          MER
-0.160670462868219          -1.481465
1.9250545861679          4.236273
1.37540323521618          2.794250
0.521810557511096          1.363800
0.178565551789934          0.530522
0.0738465634802238          0.242153
0.0253999286287232          0.092040
0.00129316062386441          0.005421

```



### All pairs of infractions 9 or over (speeding 81 or more and 10 other offences)


``` 
Variable                      Estimate         Std. Error         z value         Pr(>|z|)     
(Intercept)                     -12.528         0.127054         -98.6034                0   
policyTRUE                    -0.250129         0.0169906         -14.7215         4.68841e-49   
age_grp16-19                   0.555292         0.123685          4.48957         7.13667e-06   
age_grp20-24                  -0.00893778         0.122954        -0.0726923         0.942051   
age_grp25-34                  -0.700128         0.122731         -5.70459         1.16622e-08   
age_grp35-44                   -1.13089           0.1233         -9.17183         4.65056e-20   
age_grp45-54                   -1.34216         0.123639         -10.8555         1.87845e-27   
age_grp55-64                   -1.54229         0.125258         -12.3129         7.72364e-35   
age_grp65-199                  -1.13917         0.124739          -9.1324         6.69996e-20   
curr_pts_grp1-3                0.800054         0.0219131          36.5103         7.59887e-292   
curr_pts_grp4-6                 1.23096         0.0272765           45.129                0   
curr_pts_grp7-9                 1.56427         0.0340003          46.0076                0   
curr_pts_grp10-150              2.16262         0.0293506          73.6823                0   
month02                        0.0874065         0.0436931          2.00046         0.0454505   
month03                       -0.00448358         0.0437256        -0.102539         0.918329   
month04                        0.177833         0.0425812          4.17632         2.96268e-05   
month05                        0.275228         0.0412581          6.67088         2.5427e-11   
month06                        0.230257         0.041965           5.4869         4.0906e-08   
month07                       -0.00132929         0.0440257        -0.0301936         0.975913   
month08                        0.0773882         0.0430671          1.79692         0.0723485   
month09                        0.337419          0.04086          8.25793         1.48222e-16   
month10                        0.234273         0.0414511          5.65181         1.58769e-08   
month11                        0.174618         0.0422587          4.13213         3.59421e-05   
month12                        0.0505688         0.0431828          1.17104         0.241583   
weekdayMonday                 -0.139878         0.0343548         -4.07159         4.66939e-05   
weekdayTuesday                 0.00810758         0.0330681         0.245178         0.806319   
weekdayWednesday               0.059526         0.032656          1.82282         0.0683307   
weekdayThursday                0.193114         0.0316845          6.09488         1.09517e-09   
weekdayFriday                  0.266805         0.0311615            8.562         1.10931e-17   
weekdaySaturday                0.211969         0.0315085          6.72734         1.72789e-11  
```



```
AME          MER
-0.065694884200273          -0.236341
0.638956059150843          0.800025
-0.00805726806347857          -0.009588
-0.379664471944435          -0.542529
-0.467508886156635          -0.729784
-0.477130116844013          -0.796018
-0.475093692417285          -0.847083
-0.373702773215442          -0.732651

```
