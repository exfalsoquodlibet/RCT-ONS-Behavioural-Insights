Permuted Block Randomisation
================
Alessia Tosi
15/01/2018

### Aims

In this script,

1.  I perform a stratified randomisation, with Month of first selection (Jan-April) and business size (0-4, 5-9, and 10-19) as prognostic (i.e., stratifying) variables. This produces 12 strata which will then be taken into account in the analysis model.

2.  I then use a permuted block randomisation with varying block sizes process to randomly assign businesses to condition within each strata. That means:

    -   Businesses are randomly assigned with a 1:1 ratio to Control vs. Intervention.
    -   I perform the assignment according to a computer-generated random sequence in permuted block of (2,4,6,8) within each strata.

    An example of one possible of such sequences (where B/C stands for BI intervention/Control, one letter = one business = one assignment): \[C B\] \[B C\] \[C B B C B C\] \[C B\] \[B B C B B C C C\] ..

### Why are these two steps important?

1.  Stratified randomisation accounts for possible baseline imbalance of potentially confounding variables, and

2.  Permuted block randomisation ensures that we get an equal number of units (i.e., businesses) in each condition (Control vs. Intervention).

### Data

Data has been previously-preprocessed and fully anonymised. A subse of the tidy anonymised data are available in the Data folder.

### 1. Stratification

Let us reshuffle the rows in the dataset.

``` r
set.seed(42)
bsn_df <- bsn_df[sample(nrow(bsn_df)),]
```

Split data into individual strata datasets (one for each strata)

``` r
list_of_strata_df <- split(bsn_df, list(factor(bsn_df$newly_selected_month), factor(bsn_df$size)))
```

Let's take a look at one:

Create lookup named vector with the names of the strata as names, and the number of cases in each strata as elements. This vector will be the main input to the `?blockrand::blockrand` function (as `n`, the "number of subjects to randomise") that we will use for our randomisation.

``` r
strata_table <- bsn_df %>% 
      group_by(newly_selected_month, size) %>% 
      count()

strata_table$stratum <- with(strata_table, interaction(newly_selected_month, size))

# create a lookup named vector with stratum as name, and size of stratum n as elements
strata_vec <- setNames(strata_table[['n']], as.character(strata_table[['stratum']]))
```

    ## Apr.0504 Apr.0509 Apr.1019 Feb.0504 Feb.0509 Feb.1019 Jan.0504 Jan.0509 
    ##      314      110       33      231       80       20      338      272 
    ## Jan.1019 Mar.0504 Mar.0509 Mar.1019 
    ##       75      271      112       34

### 2. Randomisation

Stratified randomisation with block random assignment to condition within each strata (with varying-size blocks).

To do this, I create a custom function that embed `blockrand::blockrand` and that allows us to iterate the generation of randomisation sequences for our strata.

Note that the `blockrand::blockrand` function uses the `sample` function internally to generate the blocks, so we'll use `set.seed` to define the RNG seed to enable us to reproduce the block sequencing in the future at implmentation.

``` r
list_of_strata_df_2 <- lapply(names(list_of_strata_df), function(x) blockrand::blockrand(
      n = dim(list_of_strata_df[[x]])[1],
      num.levels = 2, 
      levels = c('Control', 'BI'),
      id.prefix = x, 
      block.prefix = x, 
      stratum = x)[
            1:dim(list_of_strata_df[[x]])[1],] #number of randomizations may end up being more than n (see ?blockrand) so this is to make sure it's of the right length
      )
```

``` r
# TODO
#do.call(cbind, list(list_of_strata_df[[1]], list_of_strata_df_2[[1]]))
```
