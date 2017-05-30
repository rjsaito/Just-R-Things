---
title: "Auction Experiment Simulation"
output: html_notebook
---

## Purpose

We want to run an experiment on Used Mazda Cars sold through auctions (off-lease and company cars). We want to know whether or not setting the price floor to lower (or 0) will impact the bids and final sale price of the cars for large auctions.

## Motivation

We think that setting different floor prices will impact the final sale price. To a buyer, a price floor may convey some indication of the actual value or price range of the car the seller is expecting.However, disclosing that information may impact the bids the buyer offers.  The final offer then may be biased by the price floor and deviate from the actual market value. By removing the price floor, we give the seller less information, and thus we may see unbiased offers that indicate actual market value.
 
## Experiment

The experiment will be conducted by splitting our cars into two groups: the control group with a price floor as usual, and a treatment group with an adjusted price floor (e.g. a price floor of $0). Then we send off cars to the auction, gather results, analyze, and repeat. The length of experiment at this time is not determined.

![Experiment](http://neilpatel.com/wp-content/uploads/2016/06/image01-7.png)

### 1. Sampling Scheme

To ensure that the samples in the control group and the treatment group is about equal and unbiased, we will do a **stratified sampling** on the Model and (if possible) Model Year, Damage Rating, and Mileage. Stratified Sampling is the idea of random sampling within each subset of the data, so that for each subset the proportion of the samples are maintained (e.g. 50/50 split on all CX-5, 50/50 split on all CX-9, etc). 

![Stratified Sampling](http://www.fernuni-hagen.de/statliteracy/images/Stratifiedsampling.jpg)


### 2. Experiment Designs

We have 4 potential experiment designs: 
* A/B Test
* Epsilon-Greedy Multi Armed Bandit Test
* Epsilon-Decreasing Multi Armed Bandit Test
* UCB1 Multi Armed Bandit Test

The first test is optimal for the purpose of a purely statistical hypothesis testing, but the latter three are designed to optimize the total gain/reward of the experiment.

!(A/B Test)[https://blog.automizy.com/wp-content/uploads/2017/03/AB-testing.png]

!(Multi Armed Bandit)[https://blog.automizy.com/wp-content/uploads/2017/03/Multi-armed-bandit.png]


##



```{r}
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).
