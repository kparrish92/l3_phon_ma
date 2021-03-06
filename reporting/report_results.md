Report Meta-analyis
================

# Meta-analyisis of L3 phonology studies of VOT production

5 total empirical studies were examined, in which 20 total comparisons
could be made per language pairing (L1 and L2, L2 and L3, and L1 and
L3). These studies involved a variety of language trios, including
Polish, Spanish, English, French, and German. All studies examined late
bilinguals who produced word initial plosives in each of their three
languages. Some studies in this pool report means and standard
deviations per stop consonant (/p/, /t/, /k/). These cases were treated
as independent samples, although this may increase the risk of
pseudoreplication. Effect sizes were determined given means and standard
deviations of absolute VOT measurements and standard error was computed
for each effect size. Due to the presence of many language pairings, the
absolute value of effect size was taken, so that the magnitude of
effects could be focused on, rather than directionality.

\*The formula used to compute standard error of an effect size was
`SE = sqrt(2(1 - r)/n) + es/2n))`, where `r` is the correlation of
within subjects responses (set to .5), `n` is the number of participants
in the study, and `es` is the effect size, expressed as a standardized
mean difference.

## Analysis

In order to determine a pooled effect, a Bayesian Generalized
(Non-)Linear Multivariate Multilevel Model was fir using the `brm`
function in R. The effect size (a standardized mean difference) was the
outcome variable and was modeled as a function of stop consonant and
study. As a result, an intercept only model was fit with the random
effects of stop and study per language pairing. The pooled effect size
was determined by examining the intercept of the model, which provided a
distribution of plausible estimates of the pooled effect size for
language across studies.

The priors in this study follow Casillas (2021) in that it included
regularizing, weakly informative priors that were normally distributed
and centered at 0 with a standard deviation of 1 for all
population-level parameters, a cauchy prior set at 0 with scale 1 was
used for *??* and that the model was fit with 4000 iterations (2000
warm-up) and Hamiltonian Monte-Carlo sampling was carried out with 4
chains distributed across 4 processing cores.

## Results

### L1-L2 comparison

The results of all three language pairings suggest that the body of
literature has not provided evidence for clear results when it comes to
cross-language interactions of VOT in L3 learners. First, for the L1-L2
language pairing, the probability of the average effect size being
greater than .4 was 0.06. The plot below is a forest plot of the effect
size estimate per study, and includes a pooled estimate of all studies.
The estimate for the L1-L2 comparison was 0.19 \[95% HDI = -0.6, 0.97\].
This result is troublesome, since it is essentially a prerequisite of L3
research that speakers have relatively stable and independent categories
in their L1 and their L2. Without this clear difference between the L1
and L2, it is not plausible to draw conclusions as to which of these
languages affects L3 production. Due to the seemingly high variability
in VOT production, particularly in the L2, larger sample sizes are
necessary to provide evidence that bilinguals are producing their L1 and
L2 using language-specific distributions of VOT values. Alternatively,
smaller samples could be used if it could be verified that a participant
pool consistently produce native-like VOT in their L2, as the source of
variability could also be explained by variability in L2 proficiency.

<img src="/Users/kyleparrish/Documents/GitHub/l3_phon_ma/plots/l1_l2.png" width="1950" />

### L2-L3 and L1-L2 Comparisons

Given that the pooled estimate of L1-L2 difference was inconclusive, the
comparisons for other language pairing cannot reliably be attributed to
the L1 or the L2. Nonetheless, the results are reported here. For the
L2-L3 language pairing, the probability of the average effect size being
greater than .4 was 0.06, and the esimate was 0.14 \[95% HDI = -0.54,
0.86\]. For the L1-L3 language pairing, the probability of the average
effect size being greater than .4 was 0.06, and the estimate was 0.21
\[95% HDI = -0.54, 1.02\]. Forest plots for both language pairings can
be seen below.

<img src="/Users/kyleparrish/Documents/GitHub/l3_phon_ma/plots/l2_l3.png" width="1950" />

<img src="/Users/kyleparrish/Documents/GitHub/l3_phon_ma/plots/l1_l3.png" width="1950" />

## Brief Discussion

These results suggest that the VOT studies done with L3 learners are
largely inconclusive. Much larger samples are needed (likely n &gt; 80
per group). A larger sample ought to provide evidence that L3 learners
at least have two reliably separate distributions of VOT values that
could represent underlying language-specific categories. Without this
criterion as a starting point, there is not reasonable support to
evaluate the predictions of L3 models.

The figure below demonstrates that the plausible Bayesian estimates of
effect sizes per language pairing vary greatly. The vertical, dashed
lines represent small, medium and large effect sizes as standardized
mean differences (.4 = small, .7 = medium, 1 = large; Plonsky & Oswald,
2014).

As can be seen in the figure, the distribution of plausible effect sizes
includes 0, small effects, medium effects, and some large effects. As a
result, it is difficult to make clear and categorical conclusions based
on this data.

<img src="/Users/kyleparrish/Documents/GitHub/l3_phon_ma/plots/plot.png" width="1950" />
