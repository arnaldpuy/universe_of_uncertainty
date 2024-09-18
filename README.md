# Global sensitivity analysis can unveil the hidden universe of uncertainty in many-analysts studies

[Andrea Saltelli](https://www.andreasaltelli.eu/), [Alessio Lachi](https://scholar.google.com/citations?user=Je-Ja28AAAAJ&hl=it), [Arnald Puy](https://www.arnaldpuy.com/), [Nate Breznau](https://sites.google.com/site/nbreznau/)

We develop a method for improving many-analysts research. This method uses global sensitivity analysis (GSA) to investigate the sources of variance in a given dataset. We use the R package [sensobol](https://www.jstatsoft.org/article/view/v102i05) to run the GSA. 

## Abstract

*A wave of recent many-analysts studies struggle to explain ``hidden uncertainty" in their results. We propose global sensitivity analysis (GSA) as a method to plan for and better understand this uncertainty in many-analysts studies. GSA allows us to understand in the first place how much uncertainty we are likely to find and be able to explain. Thus it provides a method to pre-determine what can be gained from a study that otherwise carries high human capital if not financial costs. GSA is a major improvement on multiverse analysis because it (i) uses state of the art sampling strategies to explore the space of the modelling options and (ii) decomposes variance in results by single and higher order modelling choices. We demonstrate the effectiveness of GSA by replicating the many-analysts study by Breznau, Rinke and Wuttke et al., which involved 161 researchers in 73 teams and hoped to shed light on how immigration might impact public preferences for social policy. We explain most of the uncertainty they could not, but only via unique interactions of model specifications. No matter how many teams they gathered, they would not have found single modelling choices that mattered for the outcome, and this has strong implications for whether such a study should be run in the first place, in addition to interpretation of results.*


## Replication

### Data

The user needs a dataframe generated in the many-analysts study by Breznau, Rinke and Wuttke et al. (2022) named [df.rds](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/df.zip)

The code used to generate `df.rds` is [Multiverse_CRI.Rmd](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/Multiverse_CRI.Rmd). As it is already run the user can ignore it. 


### Code

The entire workflow can be run or read from a single file:

| Format | Download link |
| ------ | ------------- |
| R raw  | [Code_stroll.R](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/code_stroll_final_methodology_2nd_order.R) |
| Rmd |  [Code_stroll.Rmd](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/code_stroll_final_methodology_2nd_order.Rmd) |
| PDF | [Code_stroll.pdf](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/code_stroll_final_methodology_2nd_order.pdf) |



## Application

A walk through of our method:

1. Identify a dataset and hypothesis test. This is usually the first step in a many-analysts study.
2. Generate a set of all plausible models that might be run on these data to test the hypothesis.
3. Run a global sensitivity analysis.
4. Use the results to identify where the variance is located. We expect much variance to occur at higher order levels - interactions of modelling choices. As long as many-analysts studies continue to search for single modelling choices that might have a significant impact on results, they are likely to find little if any useful knowledge. At least this can be known in advance by analyzing the data prior to running a many-analysts study. We show here that the Breznau, Rinke and Wuttke et al. study could explain only little variance because they focused only on first order variance.
5. This method can be adapted to any dataset following our workflow. 

## References

[Breznau, Rinke and Wuttke et al.](https://www.pnas.org/doi/full/10.1073/pnas.2203150119) 2022. Observing many researchers using the same data and hypothesis reveals a hidden universe of uncertainty. Proceedings of the National Academy of Sciences 119(44), e2203150119.
