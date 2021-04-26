# Summary of Main BST Findings
Based on `main_analysis.R`

## BASIC ANALYSES

## Stressors

### Acute stressor

It worked: the bath was unpleasant (+4.1 on a scale of 1-7).

_(Ratings did not appear to be affected by day, nor was the difference in ratings affected by the task order)_

### Chronic stressor

Roughly normally-distributed, with mean of 15.9 on a scale of 0-40. **Mild-to-moderate chronic stress is present**.

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

**Sequential Effects:** ?

_Is it worth considering autocorrelation between ratings on one trial and the next, much like the analyses of trust game behavior analyze not just effect of prior feedback, but prior action too?_

### Trust game

Average amount sent of $2.53 (from $0-$5).

A simple regression with day & stress suggests there may be a day effect in which people send more on day 2.

**Sequential Effects:** There are (at least) two kinds of trial-wise sequential effects in the trust game data.
1. STRONGER effect: After choosing to share, people share more (+0.28) on the next trial. Stress reduces this effect.
2. WEAKER effect: After sharing is reciprocated, people share less (-0.046) on the next trial (conversely, if sharing is _not_ reciprocated, people share _more_ on the next trial). Stress does not affect this.

### Interrelation btwn ratings & sharing

Ratings & choices were made about different people, but **average behavior in the two domains is correlated**. The average rating of trust and the average amount shared in the game are correlated (r(37) = .36, p = 0.02, spearman's rho). This suggests that there are consistent individual baseline differences in the average perception of trustworthiness.

---
## STRESS AND TRUST

_NOTE: because the categorical PSS is very non-normally-distributed (a single high stress person), I'm going to focus on continuous-PSS analyses below. If you want to pursue categorical takes on the PSS, then I'd recommend either doing a median split (high/low) or a tripartite split (low/med/high)._

### Ratings & Stress _(trust ratings)_

When analyzing ratings alone, there is a **marginal** (p = 0.059) interaction between stress and the PSS, such that there's no effect of acute stress when chronic stress is low (low PSS), but **when chronic stress is high, acute stress reduces trust ratings**.

This is consistent with a story in which chronic stress potentiates the effects of acute stress.

### Sharing & Stress _(trust game)_

Examining acute & chronic stress effects on amounts shared identifies only a day effect (and interaction). People **low in chronic stress share less** on day 2, but people who have a **high level of chronic stress share more** on day 2.

### Context Effects in Sharing _(trust game)_

...

`Stopped here at line 435`
