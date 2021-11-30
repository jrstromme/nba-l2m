# NBA Last Two Minute Reports - Quick Analysis

This code calculates the error differential for each team for the last two minute reports. (among other EDA)


For each team, two ratios are computed: incorrect calls to correct non-calls (IC to CNC), and incorrect non-calls to correct calls (INC to CC).

Then, based on the average number of calls and non-calls in the last two minutes, we can predict the expected number of detrimental calls per last two minutes for each team.

![Which Teams are Hurt Most](https://raw.githubusercontent.com/jrstromme/nba-l2m/main/plots_figures/whistledisadvantage_out.png)

Because the last two minute reports already are censored to only include close games, we don't need to worry too much about endogeneity.

However, one finding of note is that in the super-close games, refs do swallow their whistles more (IC to CNC ratio). But there is no difference in the INC / CC ratio across game closeness. This means there may be a small endogeneity issue, bu likely isn't enough to care about adjusting for. (Other factors such as attendance, leading vs. trailing team, and playoff/regular season do not seem to matter at all.

![Tighter Games, Swallowed Whistles](https://raw.githubusercontent.com/jrstromme/nba-l2m/main/plots_figures/closegames.png)

