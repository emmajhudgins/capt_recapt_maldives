# Maldivian sea turtle capture-recapture analysis
# Code written by Emma J Hudgins, emma.hudgins@carleton.ca

 Repo to accompany Hudgins et al. 2022 A brighter future? Stable and growing sea turtle populations in the Republic of Maldives

*Code heavily borrowed from Santostasi et al. 2016 doi:10.1371/journal.pone.0166650*

Ensure you have followed MARK software to interface with RMARK via the instructions [here](http://www.phidot.org/software/mark/rmark/)

## File organization 

* turtles_2016_2019.R uses green and hawksbill photo ID capture histories (files *gr_covid.csv* for greens and *hk_covid.csv* for hawksbills) and MARK software through the RMARK interface to fit a series of capture-recapture population models via robust design analysis

* plotting_script.R plots outputted figures 2-4 based on data in *greens - Table 1.csv*, *hawksbills - Table 1.csv*, and *survival.csv*. Figure 1 was created using google sheets on *Sea turtle populations - Sex_size.csv*.