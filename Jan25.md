Lab 2
================

Testing an hypothesis using SEM.

> Innovativeness, Proactively, and Risktaking Jointly and Positively Influence Firm Performance

### Project Tasks

-   Figure out SEM
    -   Two spaces do this, SEM stands for SEM --- DONE
-   Import Data, Load Our Data --- DONE
-   Specify Model Equation
-   Evaluated Variables
-   Variable and Initial Model Visualization
-   Specify SEM Workflow
    -   Specify Model Syntax and Check With Equation
-   Run the Model
-   Model Rubustness Checks
    -   Chi-squared
    -   Modification Indicies
    -   (Lamda Loadings)
-   Create Results Table

------------------------------------------------------------------------

### Load our data

``` r
# Access the course data from drbanderson.com
library(tidyverse)
Lab2data.ds <- read.csv("http://www.drbanderson.com/data/ENT5587C_Spring2018.csv")
head(Lab2data.ds, 10)
```

    ##    FirmID logFirmAge logEmployees logAssets  logSales PublicPrivate
    ## 1       1          4     2.564949  5.950643  6.349139             1
    ## 2       4          8     3.135494  7.600903  9.093806             1
    ## 3       5          3     2.302585  5.991465  8.006368             1
    ## 4       6          6     3.871201  8.085486  8.309677             1
    ## 5       8          4     3.806663  8.517193  9.933046             1
    ## 6       9          5     2.302585  2.302585  4.382027             1
    ## 7      10          9     3.806663  9.003070  8.783549             1
    ## 8      12          5     3.555348  9.570041 10.249061             1
    ## 9      15          3     1.791759  5.703783  6.813445             1
    ## 10     21          5     1.609438  6.109248  7.047517             1
    ##    IndepDivision SellGlobally INN1 INN2 INN3 PRO1 PRO2 PRO3 RISK1 RISK2
    ## 1              0            1    4    4    4    7    1    1     1     2
    ## 2              0            0    5    6    6    6    6    3     6     5
    ## 3              0            0    7    5    7    7    7    5     4     5
    ## 4              0            1    5    5    5    6    5    5     4     4
    ## 5              0            0    7    7    7    6    6    6     5     6
    ## 6              0            1    5    1    1    4    1    4     4     1
    ## 7              0            1    5    4    4    5    5    5     5     6
    ## 8              0            0    6    5    5    5    6    6     5     5
    ## 9              0            0    3    5    5    3    5    4     3     3
    ## 10             0            1    7    6    7    4    7    5     5     6
    ##    RISK3 GLB1 GLB2 HOS1 HOS2 HOS3 DYN1 DYN2 DYN3 DYN4 SL1 SL2 SL3 PERF1
    ## 1      2    4    2    3    2    2    4    4    4    3   7   7   7    12
    ## 2      4    5    6    6    6    6    5    2    2    2   5   4   6    12
    ## 3      6    5    4    4    4    5    3    4    4    3   5   5   6    12
    ## 4      6    4    2    3    4    5    5    6    6    4   6   5   5    15
    ## 5      5    3    6    6    7    7    3    5    5    2   7   7   7    10
    ## 6      1    1    6    3    3    4    3    4    3    4   5   5   5    25
    ## 7      5    6    5    2    7    5    6    5    5    4   4   6   6    20
    ## 8      6    5    5    2    4    3    6    6    5    4   7   5   6    15
    ## 9      3    3    3    3    5    3    5    5    3    3   6   6   5    10
    ## 10     4    7    7    4    7    2    7    7    7    3   5   7   6    12
    ##    PERF2 PERF3 PERF4 PERF5 PERF6 PERF7 PERF8 OL1 OL2 OL3 OL4 OL5 OL6 OL7
    ## 1     12    12    12    12    12    12    12   2   3   6   6   3   4   4
    ## 2     12    10    10    10    10    10    15   6   6   6   3   6   5   6
    ## 3     12    12    12    12    12    12    12   4   4   4   5   6   5   4
    ## 4     16    16    16    20    16    15    15   5   3   5   4   6   5   6
    ## 5     10    10    10    10    10    10    10   6   5   6   3   7   7   6
    ## 6     25    25    16    20    25    20    20   3   4   4   4   5   3   4
    ## 7     20    20    20    20    20    20    16   6   4   5   5   5   5   6
    ## 8     15    15    16    15    12    16    16   4   4   5   6   5   6   6
    ## 9     10    15    12    16    12     9     9   5   4   5   4   4   4   4
    ## 10    12    15    12    15    20    15    20   7   7   6   7   6   6   6
    ##    OL8 OL9 OL10 OL11 OL12 OL13 OL14 OL15 OL16 OL17 OL18 OL19 IV1 IV2
    ## 1    6   6    6    7    6    5    5    6    6    6    4    7   7   2
    ## 2    6   5    5    6    3    5    5    7    5    6    3    6   5   6
    ## 3    4   4    4    3    3    4    5    3    5    4    3    4   5   4
    ## 4    5   7    6    7    5    6    6    7    7    7    5    3   6   2
    ## 5    6   5    6    2    3    5    5    7    7    6    5    7   7   6
    ## 6    4   3    4    3    3    3    4    4    4    4    5    4   5   6
    ## 7    5   4    5    6    5    5    5    7    5    5    4    6   4   5
    ## 8    6   6    7    3    3    6    7    6    6    7    3    7   7   5
    ## 9    5   5    5    5    5    5    3    5    5    5    4    6   6   3
    ## 10   6   6    7    7    7    5    6    6    2    7    4    7   5   7
    ##        Risk Performance
    ## 1  1.666667       12.00
    ## 2  5.000000       10.50
    ## 3  5.000000       12.00
    ## 4  4.666666       17.00
    ## 5  5.333334       10.00
    ## 6  2.000000       21.50
    ## 7  5.333334       20.00
    ## 8  5.333334       15.25
    ## 9  3.000000       13.25
    ## 10 5.000000       13.50
