Analysis Folder contains scripts for processing and analyzing IAT output.

"proc" files import the data into matlab from output txt files.

"analyze" files calculate IAT scores.


ExpAttitudes.docx contains all of the explicit surveys we ran.

IATs contains all files needed to run IATs in psychtoolbox 3

There are 2 types of IAT contained in here
norm_bwIAT is the standard black/white good/bad IAT
norm_fiPIC_IAT is a flowers/insects (pictures) good/bad IAT

The folders contain the stimuli.

To run the BW IAT:
At the command prompt, enter

> norm_bwIAT("subjectID")

in which subject ID is a string.

Currently this randomizes the first association (e.g. whether black is paired with good or bad first) using switchAssoc and it randomizes which side the stimuli are on (counterbalancing left and right hand associations) with switchSide.

