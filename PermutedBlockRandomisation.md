Permuted Block Randomisation
================
Alessia Tosi
15/01/2018

### Aims

In this script,

1.  I perform a stratified randomisation, with Month of first selection (Jan-April) and business size (0-4, 5-9, and 10-19) as prognostic (i.e., stratifying) variables. This produces 12 strata which will then be taken into account in the analysis model.

2.  I then use a permuted block randomisation with varying block sizes to randomly assign businesses to conditions within each strata. That means:

    -   Businesses are randomly assigned with a 1:1 ratio to Control vs. Intervention.
    -   I perform the assignment according to a computer-generated random sequence in permuted block of (2,4,6,8) within each strata.

    An example of one possible of such sequences (where B/C stands for BI intervention/Control, one letter = one business = one assignment): \[C B\] \[B C\] \[C B B C B C\] \[C B\] \[B B C B B C C C\] ..

### Why are these two steps important?

1.  Stratified randomisation accounts for possible baseline imbalance of potentially confounding variables, and

2.  Permuted block randomisation ensures that we get an equal number of units (i.e., businesses) in each condition (Control vs. Intervention).

### Data

The data have been previously-preprocessed and fully anonymised. A subset of the tidy anonymised data are available in the Data folder.

### 1. Stratification

Let us reshuffle the rows in the dataset.

``` r
set.seed(42)
bsn_df <- bsn_df[sample(nrow(bsn_df)),]
```

Split data into individual strata datasets (i.e., one for each strata)

``` r
list_of_strata_df <- split(bsn_df, list(factor(bsn_df$newly_selected_month), factor(bsn_df$size)))
```

Let's take a look at one:

``` r
head(list_of_strata_df[[1]])
```

    ##        ID period_first_selected selected_for_201804 size_band size
    ## 1568 1568                201804                   Y         1 0004
    ## 1388 1388                201804                   Y         1 0004
    ## 1763 1763                201804                   Y         1 0004
    ## 1273 1273                201804                   Y         1 0004
    ## 614   614                201804                   Y         1 0004
    ## 72     72                201804                   Y         1 0004
    ##      newly_selected_month
    ## 1568                  Apr
    ## 1388                  Apr
    ## 1763                  Apr
    ## 1273                  Apr
    ## 614                   Apr
    ## 72                    Apr

### 2. Randomisation

In order to achieve stratified randomisation with block random assignment to condition within each strata (with varying-size blocks), we'll use the `? blockrand::blockrand` function and apply it to our list of lists.

Let's take a look at the arguments that `blockrand::blockrand` requires:

-   `n` This is the number of businesses to randomise in each strata
-   `num.levels` The number of conditions to randomise between, 2 in our cases
-   `levels` The names of the conditions, so `c("Control","BI")` in our case
-   `id.prefix` Prefix for the id column values, we'll use the stratum name
-   `stratum` Character string specifying the stratum generated, again the stratum name

Note that the `blockrand::blockrand` function uses the `sample` function internally to generate the blocks, so we'll use `set.seed` to define the RNG seed to enable us to reproduce the block sequencing in the future at implmentation.

We will also make sure that the randomisation sequence is as long as the number of cases in each stratum. This is necessary as the number of randomizations may end up being more than n (see `?blockrand`).

``` r
set.seed(123)

list_of_strata_df_2 <- lapply(names(list_of_strata_df), function(x) blockrand::blockrand(
      n = dim(list_of_strata_df[[x]])[1],
      num.levels = 2, 
      levels = c('Control', 'BI'),
      id.prefix = x, 
      block.prefix = x, 
      stratum = x)[
            1:dim(list_of_strata_df[[x]])[1],] #Ensures number of randomisations is of right length
      )
```

Finally, we append the randomisation list to the original list of strata to assign businesses to experimental conditions.

``` r
random_assigned_bsn_df <- NULL

for(l_idx in 1:length(list_of_strata_df)){
      random_assigned_bsn_df <- rbind(random_assigned_bsn_df, do.call(cbind, list(list_of_strata_df[[l_idx]], list_of_strata_df_2[[l_idx]])))
}
```

We verify that we obtained a balance design:

``` r
with(random_assigned_bsn_df, xtabs(~treatment + newly_selected_month + size))
```

    ## , , size = 0004
    ## 
    ##          newly_selected_month
    ## treatment Apr Feb Jan Mar
    ##   BI      157 116 169 135
    ##   Control 157 115 169 136
    ## 
    ## , , size = 0509
    ## 
    ##          newly_selected_month
    ## treatment Apr Feb Jan Mar
    ##   BI       55  40 136  56
    ##   Control  55  40 136  56
    ## 
    ## , , size = 1019
    ## 
    ##          newly_selected_month
    ## treatment Apr Feb Jan Mar
    ##   BI       17  10  38  17
    ##   Control  16  10  37  17
