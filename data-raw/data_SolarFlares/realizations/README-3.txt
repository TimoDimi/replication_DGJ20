Dear colleague;

Please find present the forecast arrays, event-list arrays and code
to evaluate operational flare forecasts.  These data and code are the
basis for the publications associated with the 2017 Nagoya University /
ISEE/CICR workshop on "BenchMarks for Operational Solar Flare Forecasts".
Use of the data and/or code herein requires acknowledgment to Leka et
al. 2019a.  

The testing interval covered was 2016.01.01 -- 2017.12.31 inclusive (731
full-disk days).  Two event definitions are used: C1.0+ (exceedance)
/ 0hr latency / 24hr validity and M1.0+ (exceedance) / 0hr latency /
24hr validity.  Only those forecasts are available here; additional
submitted forecasts (e.g. using different thresholds or validity periods)
are not available except by direct request to the provider.

A few forecasts were submitted for flare levels bounded on both upper
and lower thresholds; these are the 'C_only', etc.  They were converted
to exceedance according to the appendix of Leka et al 2019a.   In those
cases, both the original and the exceedance forecasts are included here.
In the majority of the forecasts, the upper-bounded forecasts are arrays
of "-1.0" which signals "no forecast".

Both the forecast files (.csv or IDL .sav files) and the event-lists
(CSV format, listed as ".txt." files) are suitable for the skill-score
computing code "NWRA_skill_score_guts.pro".  The latter spans a
much larger time-span but dates are included; it thus provides
the data for computing the longer-range climatology, as well.
Additional codes included (Version 1): Plot_Reliabilities_Release.pro
and Plot_ROC_Release.pro for producing Reliability plots and
Receiver-Operating Characteristic (ROC) plots.

For any questions please contact Dr. KD Leka (leka@nwra.com) or
Dr. Sung-Hong Park (park@isee.nagoya-u.ac.jp).  Questions about any
specific method should be directed to the author representing that method.

