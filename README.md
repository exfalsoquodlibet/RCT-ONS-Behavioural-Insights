# Behavioural Insights for businesses: a Randomised Control Trial (RCT) at ONS

This project provides the R code for the a-priori sample size calculation and the random assignment to experimental conditions for the two-arm stratified Randomised Control Trial (RCT) (i.e., A/B testing in geeks terminology) that I designed and delivered at the UK Office for National Statistics (ONS) in 2018.
The RCT was delivered while conducting the ONS Construction Survey in the months of April and May 2018.

The project was the first application of behavioural science principles and RCTs to businesses at the ONS and successfully increased the number of businesses that returned the survey questionnaire by the specified deadline, thereby reducing resources spent response chasing.

You could learn more about the successful intervention here: 5th International Workshop on Business Data Collection Methodology - Lisbon 2018 (https://www.ine.pt/scripts/bdcm/index.html) 

The project was a collaboration between myself (as Behavioural Insight Project Lead), the ONS Behavioural Science Unit and the ONS Businesses Data Operations Division. 


- `PowerBySimulation.md` contains the R script to calculate the a-priori power and sample size for the two-arm trial

- `power_by_sim_fun.R` is a custom function to calculate the a-priori sample size given a desired effect of minimum practical significance and power

- `PermutedBlockRandomisation.md` contains the R script to assign businesses to experimental condition (Control vs Intervention) using Stratified Permuted-Block Randomisation with varying block size. 