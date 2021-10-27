# SanskritStressModelling
Data and code to accompany "Stress Assignment and Prosodic Change in Sanskrit: From Lexical Accent to Metrical Stress System


**Warning:** The codebase in its current form is very much "unclean" and not yet in a form suitable to be used for the purposes of reproducibility.
In case of questions, please contact the author.

The files available here are the following:
*RIP_Stress_Grammar_BERLIN.txt* contains Optimality-Theoretic tableaux, which serve as inputs to iterative learning simulations. 

*RV_Iterated_Learning.R* contains functions for iterated learned simulations. It depends upon both *RIP_Stress_Grammar_BERLIN.txt* and functions contained in the further R scripts *RV_Prosodic_Shape_Parser.R* and *MaxEnt_Learning_2.R*.

*RV_Prosodic_Shape_Parser.R* contains scripts for the calculation of frequencies of prosodic shapes in the *R̥gveda*. It depends upon a corpus file, which is not publicly available here.

*MaxEnt_Learning_2.R* contains functions for the calculation of output probabilities of candidates in a Maximum-Entropy Optimality-Theoretic grammar. It contains functions for both the batch processing of frequency data and the online updating of constraint weights.
