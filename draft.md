---
author:
- 'Dominique Lamonica^1,2^'
- Hilaire Drouineau^3^
- Hervé Capra^1^
- Hervé Pella^1^
- Jérémy Piffady^1^
- Anthony Maire^4^
bibliography:
- biblio.bib
title: |
  Propositions :\
  1. Fish behavioral response to spatiotemporal variations in river
  environmental conditions\
  2. Fish behavioral response to thermal and hydraulic
  heterogeneity/variations within a large river section\
  3. Fish behavioral response to environmental fluctuations in a large
  river (section)\
  4. Patterns of fish behavioral responses to environmental fluctuations
  in a large river\
  5. Classification of fish behavioral responses to environmental
  fluctuations in a large river
---

1 - INRAE, RiverLy, HYNES (Irstea-EDF R&D), Villeurbanne, F-69625,
France\
2 - IRD\
3 - INRAE, EABX, HYNES (Irstea-EDF R&D), Cestas, F-33612, France\
4 - EDF R&D, LNHE (Laboratoire National d'Hydraulique et Environnement),
HYNES (Irstea - EDF R&D), 6 quai Watier, 78401 Chatou Cedex, France\
**Corresponding author:** Dominique Lamonica, address: , email:
dominique.lamonica\@ird.fr\
**Running title:**\
**Word count:** words

**Keywords:**

Introduction
============

Movement is a key feature of living organisms, affecting many ecological
processes [@nathan2008a]. In the last decades, the development of
individual marking and tracking methods and related analytical methods
[@jonsen2013] have revolutionised behavioural ecology [@cagnacci2010],
especially movement ecology [@nathan], which studies the link between
the environment, organism internal states and their resulting movement.
Most movements relate to the need for resources such as food or mates
[@dingle2007]. Station keeping refer to small scale movements within the
home range of individuals. It includes forging, i.e. a small scale and
generally regular movement to find appropriate resource. It also
included the defence of the habitat or predation avoidance and do not
require complex orientation and navigation systems [@dingle2006]. On the
other hand, ranging refers to larger scale exploratory movements outside
the home range to find more suitable habitats, that stop when the
appropriate resources are found [@jander1975]. Lastly, migration refer
to an oriented compulsory movement outside the home range, occurring on
a seasonal basis with a return movement [@dingle2007]. Analysing
individual movements can inform on their behaviours and on their
habitats needs. More importantly, movements can be used to assess the
perturbations due to anthropogenic modifications of habitats.

Rivers provide multiple ecosystem services, but are heavily anthropised
and modified by the implementation of dams or weirs, by water
intake/outake, regulation of discharges, modification of habitats \...
[@postel2003]. At large temporal and spatial scales, Arevalo et al.
[@arevalo2020a] show how global change have modified the thermal and
discharge regimes of rivers, and how such modifications can in turn
impact habitats suitability by fish [@arevalo2021a][@arevalo2021b]. At a
smaller spatial scale, the modifications of water velocity or thermal
regimes due to anthropogenic pressures also modify the spatial
distribution of fishes permanently or temporarily
[@lamouroux2006][@capra2017]. At both scales, both intraspecific and
interspecific variabilities are observed. High resolution trajectories
data appears very relevant to improve our understanding of the responses
of fishes to natural of human-induced variations in environmental
conditions. Many dedicated methods and models have been developed to
analysis such data [@joo2013][@jonsen2013][@nathan][@gurarie2016]. They
would allow quantifying the changes behaviours and habitat use and
explore for the existence of patterns of responses.

Yet, contrary to lakes or marine ecosystems, there is only a few studies
investigating the influence on local environment in river on fish
through the analysis of their trajectories. Many of these analysis focus
on fish migration to assess the impacts of dams and weirs
[@arenas2015][@benjebria2021]. Indeed, specific difficulties of movement
data acquisition/defects of available data and data analysis are
encountered in rivers. Most positioning systems use a triangulation
procedure which is highly sensitive to regularities in banks profiles
and ground irregularities [@cooke2013a]. As such, the precision is
spatially very variable [@berge2012b]. Moreover, contrary to open
systems, rivers are highly anisotropic with banks and flow direction
constraining fish movements [@quaglietta2019]. Given all these
specificities, Lamonica et al. [@lamonica2020] developed a specific
framework to preprocess fish trajectories data aiming to address these
limitations and then, to facilitate the exploration of the impacts of
environmental conditions on fish behaviours.

Behavioural ecology has shown intraspecies variability in habitat
preference and use (refs). This compels several processes of the
population dynamics (dev un peu). Similarly, variations in species
response to environmental changes can impact the community diversity,
distribution and abundance (refs?). To investigate individual behaviour
response to local environment variations, movement data appear to be an
appropriate window. Several statistical and modelling tools are
available to analyse and link movement data to individual behaviour
(Gurarie2016). In particular, state-space models are widely used, and
they offer the possibility of integrating environmental influence on
switching probabilities between behavioural states (dev examples/refs).

In this paper, we propose to assess: 1) the effect of local
environmental factors on behaviour change of individuals among
freshwater fish species, and 2) the inter- and intraspecies variability
of the response to environmental variations. We chose three freshwater
species (chubs, barbel and catfish) and we focused on three major
environmental factors, namely water depth, flow velocity and water
temperature (why those factors + ref plichard2017). Our hypotheses are
the following:.\
We developed a movement state-space model describing the effect of flow
velocity, water depth and water temperature on behavioural change
probabilities to investigate the variations in the effect of local
environment on individual habitat use of freshwater fish (Figure 1).
Movement data was derived from observed telemetry data, *i.e.* location
and time, and local environment input data was simulated using a 2D
hydraulic model. For each individual, independently model parameters
were inferred within a Bayesian framework. Behavioural change
probability functions were computed with the posterior distributions of
parameters describing the effect of environment on behavioural change.
For each environmental variable, independent classifications were then
performed to classify individuals based on probability function shapes
which describe their responses to a change in this environmental
variable.\

Materials and Methods
=====================

Study area
----------

The study river section is located on the Rhône river and is 1.8km long
and 140m wide (at a mean discharge of 465m^3^.s^-1^). This river section
is situated 363km upstream from the river mouth, near the Bugey nuclear
power plant (45$^{\circ}$47'44"N; 5$^{\circ}$16'25"E). The power plant
abstracts c.a. 100m ^3^.s^-1^ at the upstream end of our study reach to
cool its four reactors, and releases warmed water (between 7$^{\circ}$C
and 10$^{\circ}$C warmer than the upstream water) at two different
locations , creating a temperature difference between the left and the
right bank. More details on the study site can be found in Plichard2017.

Data collection and simulation
------------------------------

### Movement/trajectory data

Bergé et al. collected telemetry data on 94 freshwater fish individuals
of X species in the Rhône river using an HTI
(https://www.innovasea.com/fish-tracking/) acoustic fixed telemetry
system with X hydrophones positioned on the riverbed [@Berge2012].
Locations of these individuals have been tracked during three months at
a period of three seconds. Each tag emits a specific acoustic signal/at
a specific frequency that allows the identification of fish individual
and precise positioning through a triangulation process, provided that
the signal is detected by at least 3 hydrophones. Those data have been
pre-treated according to the method presented in Lamonica2020 to handle
usual defects of telemetry data (small and large gaps in location
recording, and specific artefactual patterns in some trajectories due to
the triangulation process of the HTI system). For each individual, we
obtained a set of independent trajectories with locations regularly
spaced in time (1 minute). We selected the individuals which have been
located more than xx hours (number of locations), namely 4 individuals
of barbel *Barbus barbus*, 8 individuals of European chub *Squalius
cephalus* and 6 individuals of European catfish *Silurus glanis*.

### Environment data

A two-dimensional unsteady hydraulic model of the reach considering the
longitudinal and lateral variations of depth-averaged velocities and
accounting for discharge variations have been used to simulate fish
local environment (Telemac 2D model, calibration details in Capra et
al., 2011). The Telemac 2D model also had a component that allowed us to
simulate water temperature conditions across the reach (Hervouet, 1999)
. For each time and location of each individual, depth-averaged velocity
($V$, m.s^-1^), water depth ($D$, m) and the difference between the
local water temperature and the water temperature recorded upstream of
the study section ($T$, $^{\circ}$C) have been simulated. Then those
three variables have been normalised across all individuals and
locations in order to compare the effect of the variables between
individuals and between the variables themselves on fish
movement/behaviour.

State space model + parameter inference
---------------------------------------

### State-space modelling

We developed a state-space model based on Lamonica2020 aiming at
discriminating the different individual behaviours displayed by an
individual during its trajectories, and at describing how fishes switch
between those behaviours as a response to environmental cues. Model
states correspond to the succession of fish behaviours at each
time-step, with two possible behaviours: \"Resting\" (denoted by $R$)
which corresponds to slow or erratic short-distance movements, and
\"Moving\" (denoted by $M$) which corresponds to fast oriented movements
(Lamonica2020). We assumed a quadratic influence of each environmental
variable on the behaviour switching probabilities between each
time-step, with a logit link to map the quadratic linear function to
switching probabilities . The quadratic function allows for non-linear
relationships, especially dome-shape relationships, between
environmental factors and behaviour changes. We also added a fixed
effect of the nychthemeral period (day, dust, dawn, night) to account
for possible different levels of activity throughout the day. The
observation model links the state at time $t$ to corresponding movement
variables (*i.e.* speed between two locations and turning angles between
two moves) [@Morales2004] and two additional variables which summarize
the raw data to help discriminating between behaviours Lamonica2020. The
model is written as follows:\
Transition matrix $$M_{q}(t)=
\begin{pmatrix}
   1-q_{R \rightarrow M}(t) &q_{R \rightarrow M}(t) \\
   q_{M \rightarrow R}(t)&1-q_{M \rightarrow R}(t) 
\end{pmatrix}$$

$$\text{logit}(q_{R \rightarrow M}(t)) = M_{a} \times M_{E}(t)$$

$$\text{logit}(q_{M \rightarrow R}(t)) = M_{b} \times M_{E}(t)$$

with $$M_{a}=(a_{1}, ..., a_{11})$$

$$M_{b}=(b_{1}, ..., b_{11})$$ and

$$M_{E}(t)=
\begin{pmatrix}
   0 \\
   V(t) \\
   V^2(t) \\
   D(t) \\
   D^2(t) \\
   T(t) \\
   T^2(t) \\
   \mathbb{1}_{DAY}(t) \\
   \mathbb{1}_{DUST}(t) \\
   \mathbb{1}_{NIGHT}(t) \\
   \mathbb{1}_{DAWN}(t)
\end{pmatrix}$$

State equation $$z_{t} \sim \mathcal{B}ernoulli(M_{q}(t)[z_{t-1}])$$
Observation model

$$\begin{array}{l}
y_{v_{t}} \sim \mathcal{G}amma(k[z_{t}], \theta[z_{t}])\\
y_{\phi_{t}} \sim \mathcal{V}on\mathcal{M}ises(m[z_{t}], \rho[z_{t}])\\
U_{1_{t}} \sim \mathcal{B}eta(\alpha[z_{t}], \beta[z_{t}])\\
U_{2_{t}} \sim \mathcal{N}ormal(\mu[z_{t}], \sigma[z_{t}])
\end{array}$$

with $q_{R \rightarrow M}$ and $q_{M \rightarrow R}$ the probability of
switching from resting, respectively moving, behaviour to moving,
respectively resting, behaviour, $z_{t}$ the behaviour at time $t$ ($R$
or $M$). Parameters $a_{1}$ and $b_{1}$ are the intercepts of the
switching probabilities, $a_{2}$ and $b_{2}$, and $a_{5}$ and $b_{5}$
are the linear and quadratic coefficients related to flow velocity,
$a_{3}$ and $b_{3}$, and $a_{6}$ and $b_{6}$ are the linear and
quadratic coefficients related to water depth, $a_{4}$ and $b_{4}$, and
$a_{7}$ and $b_{7}$ are the linear and quadratic coefficients related to
temperature difference.\
Concerning the data, $y_{v_{t}}$ is the observed speed between $t-1$ and
$t$ and $y_{\phi_{t}}$ is the observed turning angles between $t-2$ and
$t-1$ and $t-1$ and $t$, $U_{1_{t}}$ is the ratio between the covered
distance from interpolated data and the covered distance from raw data
between $t-1$ and $t$, and $U_{2_{t}}$ is the variance of turning angles
from raw data between $t-1$ and $t$. Concerning parameters, $k[z_{t}]$
and $\theta[z_{t}]$ are the parameters describing the speed for
behaviour $z$ at time $t$ and $m[z_{t}]$ and $\rho[z_{t}]$ are the
parameters describing the turning angle for behaviour $z$ at time $t$,
with $\alpha[z_{t}]$ and $\beta[z_{t}]$ the parameters describing
$U_{1}$ for behaviour $z$ at time $t$, $\mu[z_{t}]$ and $\sigma[z_{t}]$
the parameters describing $U_{2}$ for behaviour $z$ at time $t$.

### Parameter inference

Bayesian inference was used to fit the model to the data independently
for each individual. We defined prior distributions summarising all
available information on each parameter (see SI Table xx). Markov Chain
Monte Carlo (MCMC) computations were performed using JAGS software and
the *rjags* R package [@Plummer2009; @R]. A total of 5,000 iterations
were performed as a burn-in phase and inference was based on 20,000 to
70,000 additional iterations, depending on the individual, for each of
the three independent chains. We used the Gelman and Rubin tests
[@Gelman1992] to check the convergence of the estimation process.

Clustering of fishes
--------------------

We aimed at classifying the response of individuals to environmental
factors in terms of behavioral change, *i.e.* based on how the
probability of behaviour change varies along the environmental gradient.
For that we used the derivative of the probability functions according
to the target environmental variable, with the two other variables fixed
to zero (*i.e.* at their average value). The general form of the
derivative function for the environmental variable $X$ is thus written
as follow:
$$\frac{(c + 2\times d\times X)\exp(a + b + c\times X + d\times X^2)}{\exp(a + b + c\times X + d\times X^2) + 1} -
\frac{(c + 2\times d\times X)\exp(2\times (a + b) + 2\times c\times X + 2\times d\times X^2)}{(\exp(a + b + c\times X + d\times X^2) + 1)^2}$$
with $a$ the intercept of the considered switching probability (from
Resting to Moving $q_{R \rightarrow M}$, or Moving to Resting
$q_{M \rightarrow R}$), $b$ the coefficient of the nychthemeral period
fixed effect, $c$ and $d$ being the linear and quadratic coefficients
respectively, relative to the considered environmental variable $X$
(flow velocity $V$, water depth $D$, or water temperature difference
$T$).\
Six independent classifications were performed, *i.e.* two behaviour
changing probabilities X three environmental variables. To carry out a
clustering analysis, a metric is required to quantify the
distance/similarity between each pair of functions. In this aim, we
calculated for each pair of fucntions the absolute area between the two
curves, restrained on the main part of the environmental gradient
(x-axis range of standardised environmental variable = \[-1.5; 1.5\]),
using area between curve function from the *geiger* R package (ref).\
In order to take into account the uncertainties in estimates of the
transition matrix coefficients, we performed the classification using
$n$ replicates for each individual. Those replicates were generated by
randomly selecting $n$ iterations of the posterior distributions,
leading to $n$ values of the transition matrix coefficients which have
been used to compute $n$ derivative functions per individual, according
to equation (6).\
For each of the 6 independent classifications, we performed hierarchical
clustering using whatever function from whatever R package (ref). We set
the number of classes according to inertia decrease (ref?) (see SI
figure Ax). Each replicate of each individual fell into a class. To
assign an individual to a class, we selected the one where most of the
replicates fell into.

Results
=======

Nychthemeral period effect
--------------------------

For all individuals and for both switching behaviour probabilities, at
least the coefficient of the fixed effect for one nychthemeral period
was significantly different from the others (ANOVA SI Table 2, Figure
S2). However, the coefficients of nychthemeral period fixed effects
showed no general pattern. We chose to focus the results on the
switching probabilities computed for the day.

General effect of the environment
---------------------------------

Outside of the extreme range of environmental values, median switching
probabilities were below 0.5 (SI Figure S4), suggesting that individuals
tend to stay in the same behaviour. Overall, the effect of the water
temperature difference on switching probabilities was weaker than the
effect of water depth and flow velocity, as suggested by the boxplots of
the linear and quadratic coefficients being closer to zero for
temperature difference (SI Figure S3). Hence, the median switching
probabilities were less variable, almost constant, with temperature
difference for all individuals (SI Figure S4). Consequently, we
performed the classification on switching probabilities functions only
for water depth and flow velocity. For those two environmental factors,
there was no general pattern: probability functions were found to be
monotonously increasing or decreasing, or bell shaped concave or convex
(SI Figure S4). Simulated responses to water depth and flow velocity
(with temperature difference equal to 0) for the value ranges
encountered by each individual are showed Figure 2. Overall, in the
extreme values of the environmental gradient, the probability of
behaviour switch/change increases, in particular from resting to moving.
However, for some individuals one or both switching probabilities showed
very little variations according to the environmental factors.

Inter- and intra-species variability
------------------------------------

For the probability to switch from moving to resting according to flow
velocity, three groups were identified (Figure 3a): the individuals from
group one (red) showed an increasing then slightly decreasing
probability, the individuals from the second group (green) showed the
inverse pattern, while for the third group (blue) the probability showed
little variations. For the probability to switch from resting to moving
according to flow velocity, four groups were identified (Figure 3b): the
individuals from group one (red) showed a concave parabolic probability,
the third group (blue) showed the inverse pattern, the individuals from
the second group (green) showed a decreasing probability. For the
probability to switch from moving to resting according to water depth,
two groups were identified (Figure 3c): the individuals from group one
(red) showed a concave parabolic probability, the individuals from the
second group (blue) showed a convex parabolic probability or less
variations. For the probability to switch from resting to moving
according to water depth, three groups were identified (Figure 3d): the
individuals from group one (red) showed a concave parabolic probability,
the third group (blue) showed the inverse pattern, while for the second
group (blue) the probability was overall steadily increasing.\
Overall, classification of switching probability functions according to
water depth or flow velocity did not allow to identify any species- or
size- specific patterns (Figure 3). In addition, the groups were not
consistent across the couples probability-environmental factor,
suggesting that there is no pattern in the global response to the
environmental conditions.

Discussion
==========

-   what do those probability transitions mean ecologically, for the
    fish behaviour ?

-   how can we interpret the effect of environment according to habitat
    preferences and what we know about those species ecology ?

-   such an individual variability that we can not see any species/size
    patterns, given the limitations of the experiment. develop on exp
    limitations (river, telemetry, size of the section, loss of the
    signal, etc.)

Conclusions
===========

-   summary of results/interpretations: minor effect of water
    temperature difference compared to flow velocity and water depth +
    no general pattern in the local environment effect, strong inter
    individual variability + no group pattern + it seems that individual
    preferences, or/and other local environmental factors, are the major
    driver of behaviour changes.

-   perspectives: the model can help inferring/predicting in which
    behavioral state is/will be an individual, as well as changes in
    behaviour. In an environment with such a high anthropic pressure,
    knowing those behaviour modifications is helpful to assess how
    disturbed the individuals can be.

Figures
=======

![](Figure2.pdf){#fig2}
