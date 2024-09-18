# Global sensitivity analysis can unveil the hidden universe of uncertainty in many-analysts studies

[Andrea Saltelli](https://www.andreasaltelli.eu/), [Alessio Lachi](https://scholar.google.com/citations?user=Je-Ja28AAAAJ&hl=it), [Arnald Puy](https://www.arnaldpuy.com/), [Nate Breznau](https://sites.google.com/site/nbreznau/)

We develop a method for improving many-analysts research. This method uses global sensitivity analysis (GSA) to investigate the sources of variance in a given dataset. We use the R package [sensobol](https://www.jstatsoft.org/article/view/v102i05) to run the GSA. 

## Abstract

*A wave of recent many-analysts studies struggle to explain ``hidden uncertainty" in their results. We propose global sensitivity analysis (GSA) as a method to plan for and better understand this uncertainty in many-analysts studies. GSA allows us to understand in the first place how much uncertainty we are likely to find and be able to explain. Thus it provides a method to pre-determine what can be gained from a study that otherwise carries high human capital if not financial costs. GSA is a major improvement on multiverse analysis because it (i) uses state of the art sampling strategies to explore the space of the modelling options and (ii) decomposes variance in results by single and higher order modelling choices. We demonstrate the effectiveness of GSA by replicating the many-analysts study by Breznau, Rinke and Wuttke et al., which involved 161 researchers in 73 teams and hoped to shed light on how immigration might impact public preferences for social policy. We explain most of the uncertainty they could not, but only via unique interactions of model specifications. No matter how many teams they gathered, they would not have found single modelling choices that mattered for the outcome, and this has strong implications for whether such a study should be run in the first place, in addition to interpretation of results.*

## Replication

### Data

The user needs a dataframe generated in the many-analysts study by Breznau, Rinke and Wuttke et al. (2022) named [df.rds]

The code used to generate this is [here]

### Code

The entire workflow can be run or read from a single file:

| Format | Download link |
| ------ | ------------- |
| R raw  | [Code_stroll.R](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/code_stroll_final_methodology_2nd_order.R) |
| Rmd |  [Code_stroll.Rmd](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/code_stroll_final_methodology_2nd_order.Rmd) |
| PDF | [Code_stroll.pdf](https://github.com/arnaldpuy/universe_of_uncertainty/blob/main/code_stroll_final_methodology_2nd_order.pdf) |



## Application

A walk through of our method

## References

Breznau et al. 2022. Observing many researchers using the same data and hypothesis reveals a hidden universe of uncertainty. Proceedings of the National Academy of Sciences 119(44), e2203150119.
