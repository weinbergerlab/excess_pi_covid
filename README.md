# excess_pi_covid
Code for the manuscript "Estimating excess mortality due to P&I in the US related to COVID-19"

By Daniel M. Weinberger, Jenny Chen, Ted Cohen, Forrest W. Crawford, Farzad Mostashari, Don Olson, Virginia E Pitzer, Nicholas G Reich, Marcus Russi, Lone Simonsen, Anne Watkins, Cecile Viboud 

The goal for these analyses is to estimate excess mortality by state and for the entire US during the early part of the COVID-19 pandemic. The main analysis file is us_pi_excess.Rmd. Estimates of reporting delays are made in the file under_reporting.Rmd.

The code is set up to automatically pull the latest version of data from NCHS. If you want to update regularly, change the option: maxage=hours(9999999)) around line 96. This determiens how often to update the data. E.g., if you want to update the data every 2 days, set this to hours(48). Otherwise, the program will use the most recent vintage of data available.

A snapshot of the repository, as used in the most recent analyses through May 9 can be found here:
[![DOI](https://zenodo.org/badge/252549546.svg)](https://zenodo.org/badge/latestdoi/252549546)

The most recent results can be seen at https://weinbergerlab.github.io/excess_pi_covid/
