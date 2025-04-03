# Boyle Crossing Warning App code

This repository provides the R code to build this [web app](apps.rainfall.nz/BoyleCrossingWarning/).

The web app is intended to help with deciding when and where to cross the Boyle River to get to the track up the Doubtful River.

The app is based on [guidelines from Doubtless Conservation](https://www.doubtlessconservation.org.nz/volunteer-information.html). In particular the instructions for trapping up the Doubtful River.

The app checks the ECan river height data for the Hope River (downstream from the Boyle) and compares it to two thresholds:
1. Below 0.5 m and the Boyle is probably OK to cross directly between the carpark opposite the Doubtful, and the main Doubtful Valley rtack on the true right of the Doubtful.
This crossing is downstream of where the Doubtful joins the Boyle.
2. Above 0.8 m and the Boyle is probably too dangerouos to cross.

If the Hope River height is between 0.5 m and 0.8 m then it may be possible to cross the Boyle River above the confluence with the Doubtful River in a section that is braided, and then cross the Doubtful Riverr above the confluence where it is also braided.
