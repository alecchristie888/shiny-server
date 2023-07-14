# shiny-server for the Evidence-to-Decision Tool

The Evidence-to-Decision tool (E2D; www.evidence2decisiontool.com) has been co-designed between Conservation Evidence and practitioners from several organisations to help guide practitioners through the process of making an evidence-based decision. The tool is structured to help you consider and combine several forms of evidence (e.g., scientific evidence, tacit knowledge, values, costs) to reach a transparent decision, documenting each stage of the process so that the logic and reasoning behind decisions can be open and traceable.

The tool is deployed online using a shiny app on an Open Source Shiny Server and hosted with a Digital Ocean droplet. 

This README file is about the shiny app and server underlying the online tool.

# Shiny app
The code to run the shiny app is contained with a single file called app.R within the evidence2decisiontool folder (shiny-server/evidence2decisiontool/app.R). 

## App.R
The app draws upon two Rmarkdown files to produce reports of the information entered into the online tool (one (reporteg.Rmd) produces an example report for reference, and one (report.Rmd) produces the actual report using the entered information by a user). 

## www
The www folder, as is convention with all shiny apps, contains static files and data that the app draws upon to run. This includes images, guidance documents, and an offline template. 

# Running the app on your own shiny server or locally
I used this excellent online blog (https://deanattali.com/2015/05/09/setup-rstudio-shiny-server-digital-ocean) to set up my own shiny server using a digital ocean droplet, which I would recommend as a cost-effective way to develop your own versions or iterations of this tool or related tools.
To run the shiny app locally, you should to simply select and run all of the lines of code in `app.R`. In R Studio, there is also a button for `Run App`. The app should open in a browser on your computer or in R Studio depending on your settings.

# Copyright
There are two separate licenses, one for the open source code and one for the online tool (i.e., about its name and concept).

The repository shiny-server is copyright (c) 2021 Alec Christie, but it is Open Source and licensed under the MIT License.

The Evidence-to-Decision Tool (including its online implementation at www.evidence2decisiontool.com) by Alec P. Christie and Conservation Evidence is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (based on a work at https://github.com/alecchristie888/shiny-server). 

<a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a><br /><a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons Attribution-ShareAlike 4.0 International License</a>.
