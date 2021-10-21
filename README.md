
## Report on Irish Senator voting activity and attendance, 2020/2021

### Summary

This repository contains the code used to analysis the voting and
attendance histories of members of the 26th Irish Seanad (Senate).

The analysis found that a subset of Senators regularly do not vote on
voting days. In addition, for some Senators, there was evidence of
attendance (defined as either a recorded vote or a verbal contribution
to the debate) on &lt;30% of the days they claimed they attended for the
purpose of the parliamentary allowance.

This analysis was performed using the `targets` framework. To re-run the
pipeline, install and load the `targets` pacakge and call `tar_make()`.

 

### Voting days

The initial analysis was a straightforward summary of the proportion of
voting days on which a member of the 26th Seanad cast at least one vote.
“Voting days” were defined as the number of sitting days (as votes can
only take place on a sitting day) on which at least one vote was held. I
decided to focus on voting days rather than total number of votes, as
the clustering of votes on particular days meant that missing a single
day could have a large effect on the number of votes missed (some voting
days had up to 11 individual votes).

Figure 1 illustrates the proportion of the 48 voting days between June
29th 2020 and July 31st 2021 a given Senator voted on. This study window
was chosen as attendance records used in subsequent analyses are only
available up until 31st July 2021.

 

<div class="figure" style="text-align: center">

<img src="figures/seanad_percentage.png" alt="Summary of voting activity by Senator" width="80%" />
<p class="caption">
Summary of voting activity by Senator
</p>

</div>

 

Of interest, all six Senators elected by the NUI (Alice Mary Higgins,
Michael McDowell, Rónán Mullen) and TCD (Ivana Bacik, David Norris, Lynn
Ruane) University constituencies are in the bottom half of all Senators
ranked by the number of voting days on which they voted.

From this graph, there seemed to be two obvious candidates for further
investigation. Both David Norris and Denis O’Donovan cast a vote on only
4 of the 48 voting days in the study period.

 

### Days voted on vs days claimed as attended

However, it could be that due to the ongoing pandemic, these Senators
did not attend voting days (potentially due to ill health or travel
difficulties) and so could not have voted.

In order to investigate whether this was the case, I scraped the voting
days on which a Senators’ attendance at the Seanad was recorded for the
purposes of the parliamentary allowance. This data is available in a PDF
format from the Oireachtas website.

If Senators were not voting on voting days because they were not in
attendance, we would expect that as the number of days on which Senators
recorded their attendance increases, so to does the number of days on
which they voted (this assumption is indicated by the line on the
chart). This expectation is largely true for the majority of Senators
but is not applicable to our outliers of interest (denoted in red and
with labels in the plot below). David Norris and Denis O’Donovan
attended far fewer voting days versus the number of days on which they
claimed attendance. This indicates that the finding from the initial
barchart - that these two Senators attended very few votes - is not due
to their absence from the Seanad.

 

<div class="figure" style="text-align: center">

<img src="figures/seanad_attendance_vs_voting_days.png" alt="Number of voting days a Senator voted on versus number of voting days for which they claimed attendance. Line indicates a 1:1 relationship." width="80%" />
<p class="caption">
Number of voting days a Senator voted on versus number of voting days
for which they claimed attendance. Line indicates a 1:1 relationship.
</p>

</div>

 

### Days with evidence of attendance vs days claimed as attended

So, some Senators are not voting on many of the days they are supposedly
present in the Seanad. But political life is complicated - they may have
been present on the voting day, but due to bad luck, have consistently
missed the actual votes.

To examine this, my next step was to try and work out whether there was
any record of Senators being present in the Seanad on days where they
claimed to be in attendance. I defined evidence of attendance as either:

-   A recorded vote
-   A verbal contribution to the debate

Verbal contributions were obtained by scraping the official text for
each sitting of the Seanad during my study window, and counting the
number of times a Senator spoke.

This data on verbal contributions, combined with the voting data
presented in the section above, allowed me to plot the number of days on
which a Senator claimed they were present against the number of days for
which there was evidence for them actually being present.

 

<div class="figure" style="text-align: center">

<img src="figures/seanad_claimed_vs_actual_attendence.png" alt="Number of voting days with evidence of attendance (vote/contribution) vs number of voting days claimed as attended. Line indicates a 1:1 relationship." width="80%" />
<p class="caption">
Number of voting days with evidence of attendance (vote/contribution) vs
number of voting days claimed as attended. Line indicates a 1:1
relationship.
</p>

</div>

 

On some (n=8) of the voting days on which David Norris did not vote, he
made verbal contributions to the debate providing evidence of
attendance. In contrast, Denis O’Donovan did not make any verbal
contributions on days he claimed attendance for but did not vote on.
