# Summary of Main BST Findings
Based on `main_analysis.R`

## BASIC ANALYSES

## Stressors

### Acute stressor

It worked: the cold water bath was significantly more unpleasant (+4.1 on a scale of 1-7; t(38) = 13.7, p = 3e-16).

Mean control (lukewarm) bath rating was 2.0 (s.d. 1.0), while mean stress (cold) bath was 6.1 (s.d. 1.3).

_(Ratings did not appear to be affected by day, nor was the difference in ratings affected by the task order)_

### Chronic stressor

Roughly normally-distributed, with mean of 15.9 (s.d. = 5.7; range 3-27) on a scale of 0-40. **Mild-to-moderate chronic stress is present**.

> 11 **low** chronic stress (0-13 PSS)
>
> 24 **moderate** chronic stress (14-26 PSS)
>
> 1 **high** chronic stress (27-40)

_(Not affected by whether the acute stressor was on the day the PSS was filled out)_

## Trust assessments

### Trust ratings

Average rating of 4.7 on a scale from 1-9.

A simple regression (with day & stress) finds lower ratings under stress (a small effect size: -0.075).

**Sequential Effects:** 

By itself, the previous trial rating is strongly signficiant and in the direction expected. If the previous rating given was 'unstrustowrthy', the current rating is predicted to shift by -.244, +.244 of the previous rating was trustworthy.
Using the actual previous rating, the pattern remains the same, although the effect appears to be stronger

### Trust game

Average amount sent of $2.53 (from $0-$5).

A simple regression with day & stress suggests there may be a day effect in which people send more on day 2.

**Sequential Effects:** There are (at least) two kinds of trial-wise sequential effects in the trust game data.
1. Previous **ACTION**: After choosing to share, people share more (+0.22, p = 2e-16) on the next trial. Acute stress reduces this effect (-0.14, p = 6e-11).
2. Previous **OUTCOME**: After sharing is reciprocated, people share less (-0.046, p = 0.005) on the next trial (conversely, if sharing is _not_ reciprocated, people share _more_ on the next trial). Stress does not affect this (p = 0.5).

### Interrelation btwn ratings & sharing

Ratings & choices were made about different people, but **average behavior in the two domains is correlated**. The average rating of trust and the average amount shared in the game are correlated (r(37) = .36, p = 0.02, spearman's rho). This suggests that there are consistent individual baseline differences in the average perception of trustworthiness.

---
## STRESS AND TRUST

_NOTE: because the categorical PSS is very non-normally-distributed (a single high stress person), I'm going to focus on continuous-PSS analyses below. If you want to pursue categorical takes on the PSS, then I'd recommend either doing a median split (high/low) or a tripartite split (low/med/high)._

### Ratings & Stress _(trust ratings)_

When analyzing ratings alone, there is a **marginal** (p = 0.06) interaction between stress and the PSS, such that there's no effect of acute stress when chronic stress is low (low PSS), but **when chronic stress is high, acute stress reduces trust ratings**.

The inclusion of a _day_ effect doesn't alter this relationship (p = 0.059 with day included, p = 0.062 without day).

This is consistent with a story in which chronic stress potentiates the effects of acute stress.

Including previous trial ratings (binary version) doesn't change the story hugely, although the trending acute-pss interaction disappears. 
However, using the actual rating amount alters the story from both the binary version and the version without previous ratings. 
    Here acute stress, and the acute-pss interaction become significant with fairly large effect sizes, and these effects are flipped from elsewhere (decreased sharing under acute, increase as chronic increases)

### Sharing & Stress _(trust game)_

Effects of stress on sharing depend a lot on the inclusion of the **day** factor.

**Without Day**: Acute stress has **opposite** effects on sharing depending on levels of chronic stress. For individuals with LOW chronic stress, acute stress _increases_ the amount shared (+0.16, p = 0.0002), while for HIGH chronic stress individuals, acute stress _decreases_ the amount sent (overall effect = -0.18; interaction effect = -0.013, p = 2e-6).

**With Day**: There is no overall effect of acute or chronic stress (or their interaction) on sharing. There is only a day effect (and interaction). People **low in chronic stress share less** on day 2, but people who have a **high level of chronic stress share more** on day 2.

> _WHICH REGRESSION TO USE?_
>
> Model comparison indicates that the model with Day effects outperforms the model without (AIC(with) = 16684 < AIC(without) = 16700; ANOVA model comparison favors the model with Day, p = 8e-5).
>
> Day effects were not _a priori_ expected, however, and given the design of the study (two days, stress one day and control the other), and the relatively limited sample size (N = 39), there is the possibility that day & stress effects might be partially confounded.
>
> Some evidence that this may be happening here is that the design was slightly unbalanced when collection stopped. Specifically, N = 16 participants experienced Control/Stress (Day 1/2), and N = 23 experienced Stress/Control. Thus, what looks like "sharing is higher on Day 1" could also be "stress increases sharing".
>
> A REASONABLE APPROACH might be to **first**, use the model without day as the hypothesis-related model, and **second** mention (briefly in the main text, and in more detail in a footnote? supplement?) that the pattern of results is affected by the inclusion of control variables (Day) that are slightly collinear with stress.

### Context Effects in Sharing _(trust game)_

How acute & chronic stress alter the effects of context on sharing are complex, but analyses indicate that sharing overall is affected by **acute stress** and **chronic stress** (mirroring findings from analyses of stress & sharing, above); _and_ effects of recent sharing **actions** on subsequent sharing are modulated by **acute** and **chronic stress**.

#### Stress alters trust actions:
When chronic stress is low, more money is shared under acute stress. When chronic stress is high, less money is shared under acute stress. **This replicates the findings reported above.**

#### Stress modulates context effects:
When chronic stress is low, people share more money after previous sharing, and acute stress reverses this effect. When chronic stress is high, there is little difference between acute stress and control.

#### No effect of feedback:
There is no effect of previous feedback, nor interactions of acute or chronic stress with previous feedback found in the above regressions _or_ in regressions that drop the effect of previous sharing (to simplify the analysis).
