# **Predicting violent conflict in Africa - Leveraging open geodata and deep learning for spatiotemporal event detection**


## Abstract 

Violent conflicts endanger human lives, the social cohesion of societies 
and the natural environment. While the number of intensive international conflicts 
has remained on a low level during the 21st century, civil wars are on the rise. 
Since the 1990s, research engages in predicting the outbreak of violence. However, 
findings on the role the natural environment plays in the emergence of 
violence remain mostly inconclusive. In order to contribute to the discussion this
thesis sets out to compare the predictive performance of deep learning models using 
data from the Uppsala Conflict Data Program (UCDP) on civil conflict between 2001 to 2019.
The data is simultaneously aggregated on administrative districts and sub-basin watersheds 
and combined with socio-economic and environmental predictors. The hyperparameters 
of CNN-LSTM architectures are optimized employing a Bayesian Optimization strategy. 
The results in terms of F2-score suggest significant improvements for aggregating 
predictors on sub-basin watersheds (+7.16,p=3.4e-11) as well as integrating 
environmental predictors (+3.98,p=5.9e-05) for a combined conflict class. For 
other conflict classes, the results tend to the same direction but are not significant. 
Through the comparison to existing conflict prediction tools, the thesis exposes 
the sensitivity of prediction models to spatial scale and units of aggregation. 
It is argued that in order to fulfill the requirements of effective conflict 
prevention efforts, prediction research will have to fully integrate modern deep 
learning frameworks and constant data streams on different earth processes in the future.

## Graphical Abstract
![](assets/wf.svg)

## Disclaimer

This thesis was submitted to the Department of Geography, University of Marburg, in partial fulfillment of the requirements for the degree of M.Sc. Phyisical Geography. It was written by customizing the {[huwiwidown](https://github.com/phister/huwiwidown)} template of the Berlin School of Buisness and Economics, HU Berlin. The online version of the thesis just wraps the original R Markdown files which were written to produce LaTex and builds a [workflowr](https://jdblischak.github.io/workflowr/) project out of it.  By the conversion to html some outputs my render not totally as expected. However, you can download the original pdf version from [here](https://github.com/goergen95/thesis-predicting-conflict/blob/master/report/thesis-output/2021-03-24-thesis.pdf).  All code available in this repository is licensed under [GPL-3](LICENSE.md). 

### Assets

- An online version of the thesis powered by {[workflowr](https://github.com/jdblischak/workflowr)} can be found [here](https://goergen95.github.io/thesis-predicting-conflict/index.html) 

- The original version of the thesis as a pdf can be found [here](https://github.com/goergen95/thesis-predicting-conflict/blob/main/report/thesis-output/2021-03-24-thesis.pdf), an occasionally updated version revising minor errors is found [here](https://github.com/goergen95/thesis-predicting-conflict/blob/main/report/thesis-output/thesis.pdf) 

- Slides for the final presentation are powered by {[xaringan](https://github.com/yihui/xaringan)} and are found [here](https://goergen95.github.io/thesis-predicting-conflict/presentation.html)

  

