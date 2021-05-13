#devtools::install_github("rstudio/shiny")
library(shiny)
library(rmarkdown)
library(knitr)
library(glue)
library(tinytex)
library(data.table)
library(rsconnect)
library(DT)
#library(shinyBS)
library(tippy)
library(dplyr)
#library('bslib')

ui <- function(request){fluidPage(
  titlePanel(title=tags$a(href='http://conservationevidence.com', target="_blank",
                          tags$img(src="image1.jpg", width = 240, height = 80)),
             windowTitle = "Evidence-to-Decision tool"),
  titlePanel(title = "Evidence-to-Decision tool"),
  sidebarLayout(
    sidebarPanel(width=2,
      h4("Bookmark your work"),
      p("We recommended that you bookmark regularly to save your work. Your browser URL will update each time you do this (leaving the page or refreshing before doing so could lose your work)."),
      bookmarkButton(label="Bookmark", title="Bookmarking your work will create a new unique URL in your browser that saves your work."),
      br(),
      br(),
      h4("Add or remove actions"),
      fixedRow(column(tippy("<strong> Action to add </strong>", tooltip = '<p style="font-size:15px">On reaching Step 2.A Identify potential actions you can add actions below to continue the process.</p>', allowHTML=TRUE,placement="top"),
      textInput("newactionname",label=NULL, value=NULL, placeholder = "e.g., Install culverts or tunnels as road crossings"),width=12)),
      actionButton("add", "Add this action"),
      br(),
      br(),
      fixedRow(column(tippy("<strong> Action to remove </strong>", tooltip = '<p style="font-size:15px">If you make a mistake in adding an action, you can remove the incorrect name below.</p>', allowHTML=TRUE,placement="top"),
      textInput("newactionnamerem",label=NULL, value=NULL, placeholder = "e.g., Install barrier fencing along roads"),width=12)),
      actionButton("remove", "Remove this action")
      ),
    mainPanel(width=10,
      navbarPage("",id="maintabs",
                 tabPanel("Introduction",id="tab01",value='tab01_val',
                          tags$style(
                            ".navbar-nav li a {
                            font-size: 20px;
                            font-weight: bold;
                              }
                            "
                          ),
                          h4("What's this tool for?"),
                          #textOutput("numids"),
                          paste("The Evidence-to-Decision tool has been co-designed between the"),
                          a("Conservation Evidence", href="https://www.conservationevidence.com", target="_blank"),
                          paste("group and practitioners from several organisations to help guide practitioners through the process of making an evidence-based decision. 
                                The tool is structured to help you consider and combine several forms of evidence (e.g., scientific evidence, tacit knowledge, values, costs) to reach a transparent decision, documenting each stage of the process so that the logic and reasoning behind decisions can be open and traceable. If you wish to use an offline template of the tool, please"),
                          a("click here.",href="Evidence_to_Decision_Tool_Offline_Template.docx", target="_blank"),
                          br(),br(),
                          p("The tool is structured using three steps: 1.) Define the Decision Context (i.e., What is the problem you want to solve?); 2.) Gather Evidence (i.e., What actions are likely to be the most effective to address my problem in my local context?);
                            3.) Make an Evidence-Based Decision (i.e., What are the next steps? Which actions will be implemented based on the evidence you have assessed?). The diagram below lays out the detailed steps that this tool will guide you through."),
                          p("This tool is best suited for use by individual landowners, reserve managers, and small NGOs working on specific projects to come to an evidence-based decision for a specific problem. The tool was designed to streamline an evidence-based decision-making process with limited time and resources. 
                            The tool can also be used to begin thinking about how to tackle major decisions, laying the foundation for a more in-depth decision-making process using other tools and frameworks (e.g., Structured Decision-Making, Multi-Criteria Decision Analysis, or Theory of Change etc.)."), 
                          paste("To begin using the tool, click on 1. Define the Decision Context. Throughout this tool you will be prompted to enter information which will be compiled and documented in a report which you will be able to download in the final step. (If you wish to see an example of what the tool produces, please go to tab 3. Make an Evidence-Based Decision and scroll to the bottom). Download the"), 
                          a("E2D Tool Guide", href="e2dguide.pdf", target="_blank"), paste("for extra guidance. Tips will also appear if you hover over the bold text directly above text boxes in the tool."),
                          br(),
                          tags$img(src="image3.png", width = 700, height = 780),
                          br(),
                          p("This tool was created by Dr Alec Christie, University of Cambridge. Thank you to all the practitioners who took part in the co-design of this tool, including (in no particular order): Steve Weeks, Alison Ruyter, Rory Harding, and Paul Tinsley-Marshall from the Kent Wildlife Trust; Tom McPherson from Ingleby Farms (also for giving feedback on the manuscript); the Woodland Trust; Peoples’ Trust for Endangered Species; Jon Flanders and Winifred Frick at Bat Conservation International; David O’Brien at NatureScot; Kathy Wormald at Froglife; the Medway Valley Countryside Partnership; Sheffield & Rotherham Wildlife Trust; Bedfordshire, Buckinghamshire, and Oxfordshire Wildlife Trust; Catherine McNicol at Gloucestershire Wildlife Trust. Thanks also to Harriet Downey, Matthew Grainger, Thomas White, Michael Winter, and William Sutherland for their help in producing the tool.")
                          ),
                 tabPanel("1. Define the Decision Context",id="tab02",value='tab02_val',
                          em(strong("What is the problem and desired outcomes? What is the relevant ecological, physical, and social context underlying the decision?")),
                          p("It is important to carefully define and detail the context surrounding the decision to be made. 
                            This includes the problem (or direct threat) being tackled, the location the decision affects,
                            the ultimate goal you want to achieve (i.e., the desired outcomes), the focal target of any action (i.e., the species, habitat, or group),
                            and any other relevant contextual information (e.g., socio-ecological factors, constraints on decision-making."),
                          fixedRow(column(tippy("<strong> Name of decision-maker </strong>", tooltip = '<p style="font-size:15px"> Add you name here to include this in your downloadable report.</p>', allowHTML=TRUE,placement="top"),
                                          textAreaInput("reportname",label=NULL,width="200%",height="200%",rows=1),width=4)),
                          fixedRow(column(tippy("<strong> Name of decision-maker's organisation </strong>", tooltip = '<p style="font-size:15px"> Add you organisation name here to include this in your downloadable report.</p>', allowHTML=TRUE,placement="top"),
                                          textAreaInput("reportorgname",label=NULL,width="200%",height="200%",rows=1),width=4)),
                          fixedRow(column(tippy("<strong> What is the problem? </strong>", tooltip = '<p style="font-size:15px"> e.g., Large numbers of amphibians are being killed when crossing a road that runs through part of the reserve.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action01",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column(tippy("<strong> Location </strong>", tooltip = '<p style="font-size:15px"> State the location for the proposed action.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action02",label=NULL, width="100%",height="100%",rows=1),width=4)),
                          fixedRow(column(tippy("<strong> What is the ultimate goal? </strong>", tooltip = '<p style="font-size:15px"> e.g., Reduce amphibian mortality but still enable the movement of frogs across the road.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action03",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column(tippy("<strong> What is the focal target? </strong>", tooltip = '<p style="font-size:15px"> e.g., Prevent the decline of a Natterjack toad (Epidalea calamita) population.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action04",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column(tippy("<strong> What is the relevant ecological, physical, and social context underlying the decision? What constraints are there on your decision-making? </strong>", tooltip = '<p style="font-size:15px"> e.g., The road was built in 2017 and since then our monitoring and anectodotal evidence has suggested that a substantial proportion of amphibians are being killed on a key section of the road. We believe this may be a major factor behind the continued decline of a population of Natterjack toads (Epidalea calamita) despite our best efforts to provide suitable habitat. This toad is a key species we need to protect due to their wider national decline. We have a keen group of volunteers who can help with the work. The road is owned and maintained by the local council.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action05",label=NULL,width="200%",height="200%",rows=3),width=12))
                 ),
                 tabPanel("2. Gather Evidence",id="tab03",value='tab03_val',
                          p("You are now asked to gather evidence for and against the implementation of different action(s) to address the Decision Context defined in the previous tab. We suggest you start by identifying potential management actions to address your problem (2.a), and then consider diverse forms of evidence on the effectiveness, costs, acceptability, and feasibility of each action by using the menu on the right (this will bring up more content below)."),
                          h4("2.A. Identify potential actions"),
                          em(strong("Which action(s) could be taken to address the problem, regardless of their cost, acceptability, or feasibility?")),
                          br(),
                          paste("Brainstorm and note as many potential actions as you can to address the decision or problem you are considering - for possible ways to do this, see the"), 
                          a("E2D Tool Guide.", href="e2dguide.pdf", target="_blank"),
                          paste("Don't forget to look into the literature and search websites like"),
                          a("Conservation Evidence", href="https://www.conservationevidence.com", target="_blank"),
                          paste("to inform your list of actions. Make sure to include actions regardless of their acceptability, feasibility, or costs at this stage. These actions can be ruled out later if they are too expensive, unfeasible, or unacceptable. 
                            Once you have entered your list of potential actions below, add them one at a time using the right-hand menu (this will generate content below for you to fill in for each one)."),
                          br(),
                          br(),
                          fixedRow(column( tippy("<strong> Potential actions </strong>", tooltip = '<p style="font-size:15px"> e.g., Install culverts or tunnels as road crossings.\nInstall barrier fencing along roads.\nUse humans to assist migrating amphibians across roads.\n...\n...\n... </p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action06",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          br(),
                          tabsetPanel(id="tab03_extra")
                 ),
                 tabPanel(title="3. Make an Evidence-Based Decision",id="tab13",value='tab13_val',
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          br(),
                          h4("Summary assessment table for each action"),
                          p("Table legend: Darker yellow = greater certainty. (White = Very low certainty or unsure, Pale yellow = Low certainty, Yellow = Moderate certainty, Dark yellow/Gold = High certainty)."),
                          DTOutput("testtab"),
                          br(),
                          h4("Summary of evidence for and against implementation of each action:"),
                          htmlOutput("step3summarytext"),
                          br(),
                          h4("3.A. Weigh up the evidence for and against different actions"),
                          em(strong("Reflecting on the problem you face and the evidence and information you have gathered, what is your decision and why?")),
                          p("Using the accumulated evidence, the relative advantages and disadvantages of each modified action can be compared and related back to the original decision or problem being considered (in Step 1. Define the Decision Context). This involves weighing up how locally effective, cost-effective, acceptable, and feasible each action is and whether its implementation is justified."),
                          paste("We would suggest that users could prioritise actions based on their place in the Mitigation and Conservation Hierarchy"), 
                          a("(see here).", href="https://academic.oup.com/view-large/figure/118141473/biy029fig1.jpeg", target="_blank"),  
                          paste("For example, we would recommend that actions that avoid and minimize threats should be prioritised, before restoration and compensatory measures are considered.
                                Several types of biases can also affect your decision-making, so we recommend reading the"),
                          a("E2D Tool Guide", href="e2dguide.pdf", target="_blank"),
                          paste("which provides some common pitfalls to be aware of when evaluating the evidence for and against different actions."),
                          br(),
                          br(),
                          fixedRow(column( tippy("<strong> <em> Which action(s), if any, are the best ones to implement to achieve the ultimate goal(s) you defined at the beginning? Name and justify your choices. </em> </strong>", tooltip = '<p style="font-size:15px">e.g., Install barrier fencing along roads: This action has been shown to be effective from the evidence I have considered if it is implemented properly. The costs will be less than installing culverts or tunnels, and it should take less time to get permissions to install the fencing along the road. If we target the fencing at strategic positions, and make it high enough so Natterjack toads not climb over it, we can funnel them to natural watercourses underneath the road.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action20",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column( tippy("<strong> <em> Which action(s), if any, should not be implemented? Name and justify your choices. </em> </strong>", tooltip = '<p style="font-size:15px">e.g., Use humans to assist migrating amphibians across roads: This is because based on my assessment of the evidence this action is unlikely to be effective on the reserve, as it has shown to not prevent population declines elsewhere and might divert volunteers away from activities that are more important. \n \nInstall culverts or tunnels as road crossings: This is because although this could be an effective action based on the scientific evidence, it can lead to mortality of amphibians within the culverts and tunnels. It would also cost a significant amount of money which would likely be beyond our budget, and would also take a lot of time to get permission to construct and build.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action21",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          h4("3.B. Justify overall decision and next steps"),
                          em(strong("What is the overall decision, what are the next steps, and why?")),
                          p("Summarise your overall decision and the next steps you will take. This could be implementing these actions, pausing to make a more detailed assessment, gather more evidence, or decide to do nothing."),
                          fixedRow(column( tippy("<strong> Decision and next steps </strong>", tooltip = '<p style="font-size:15px">e.g., We will now investigate the correct height, material, and length of fencing needed and identify key strategic points along the road to place the fencing. We will also request permission to install the fencing and trial it during the next migration season. We will write up the findings of this trial in a report, comparing it to previous years mortality, and publish this online through a practitioner-focused journal such as the Conservation Evidence journal.</p>', allowHTML=TRUE,placement="top"),
                            textAreaInput("action22",label=NULL,width="200%",height="200%",rows=3),width=12)),
                          br(),
                          h4("3.C. Document and report decision (download report)"),
                          strong("Example summary report"),
                          p("To download an example of a summary report without having to fill any information select a format and then click below. This may take a little time depending on your browser."),
                          radioButtons('formateg', 'Example report format', c('PDF', 'Word'),
                                       inline = TRUE),
                          downloadButton("downloadReporteg", "Download example summary report", style='padding:4px; font-size:125%'),
                          br(),
                          br(),
                          strong("Your customised summary report"),
                          p("This summary report has been created using the text you've entered into this tool. To download this summary report select a document format and then click below."),
                          radioButtons('format', 'Report format', c('PDF', 'Word'),
                                       inline = TRUE),
                          downloadButton("downloadReport", "Download summary report", style='padding:4px; font-size:125%'),
                          br(),br()
                 )
                 )
      ),position="right"
  )
)}


server <- function(input, output, session) {
  

  observeEvent(input$add, {
    id <- input$newactionname
    appendTab(inputId = "tab03_extra",
              tabPanel(title = glue("2.B-G. ",id), id=glue("2.B-G. ",id), value=glue("2.B-G. ",id,"_val"),
                       tags$style(
                         "li a {
                         font-size: 20px;
                         font-weight: bold;
                          }
                         "
                       ),
                       br(),
                       p("Remember to complete the steps below for each action by generating separate tabs using the right-hand menu above."),
                       p("Please expand in more detail on the proposed action below, including what the action involves and what the focus of the action is (i.e., a species, group, or habitat - this is not necessarily the focal target you defined earlier as this action could be indirect)."),
                       fixedRow(
                         column(tippy("<strong> Describe action </strong>", tooltip = '<p style="font-size:15px"> e.g., Install culverts or tunnels that can act as underpasses for amphibians. </p>', allowHTML=TRUE,placement="top"),
                                textAreaInput(paste0("action07",id),label=NULL,width="100%",height="100%",rows=2),width=6),
                         column(tippy("<strong> Focus of action </strong>", tooltip = '<p style="font-size:15px"> e.g., Assist amphibians, particularly Natterjack toads, across the road through constructing culverts or tunnels under the road. </p>',placement="top"),
                                textAreaInput(paste0("action08",id),label=NULL,width="100%",height="100%",rows=2),width=6)),
                       h4("2.B. Assess desirable and undesirable effects on the focal target and uncertainty"),
                       strong(em("What do different types of evidence tell us about the desirable and undesirable effects of each action on the focal target? How certain are we of the credibility of this evidence?")),
                       p("In this section you are asked to assess the likely effectiveness of this action within your local decision context (specifically regarding your focal target - there is space later in the tool to assess cost-effectiveness, side-effects, and wider impacts of actions)."),
                       h4("2.B.i. Scientific literature"),
                       em(strong("How locally effective is this action likely to be based on evidence from the scientific literature? What is the overall certainty (reliability) of this evidence?")),
                       br(),
                       paste("Please describe below where you found evidence for this action and your assessment of its findings. For example, you could draw from the Conservation Evidence assessment for each action if you have used this "),
                       a("website", href="https://www.conservationevidence.com", target="_blank"), 
                       paste(" to look for evidence. Or you might have access to reports or studies from your own organisation (i.e., the grey literature) that provide useful evidence - see also"),
                       a("Applied Ecology Resources.", href="https://www.britishecologicalsociety.org/applied-ecology-resources/search/", target="_blank"),
                       paste("Ensure you consider the strength (quality) of the evidence (i.e., the reliability of the study or experimental design used) and the relevance of this evidence (i.e., are the results likely to apply to your local setting?). Please use the"),
                       a("E2D Tool Guide", href="e2dguide.pdf", target="_blank"), 
                       paste("for further information on how to critically appraise evidence and account for biases (see the evidence hierarchy diagram below too)."),
                       br(),
                       br(),
                       fixedRow(
                         column(width=12,tippy("<strong> Evidence sources considered </strong>", tooltip = '<p style="font-size:15px"> e.g., Conservation Evidence, systematic reviews, reports. </p>', allowHTML=TRUE,placement="top"),
                            textAreaInput(paste0("action09",id),label=NULL,width="200%",height="200%",rows=2))),
                       fixedRow(
                         column(width=12,tippy("<strong> Detailed assessment of evidence </strong>", tooltip = '<p style="font-size:15px"> e.g., This action was assessed as likely to be beneficial on Conservation Evidence by a panel of experts. Several studies found... </p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action10",id),label=NULL,width="200%",height="200%",rows=3))),
                       fixedRow(
                         column(width=12,tippy("<strong> Summarise assessment of evidence </strong>", tooltip = '<p style="font-size:15px"> e.g., Overall, based on the sources above this action is likely to be beneficial, but we need to consider how to apply it effectively in our local setting. </p>', allowHTML=TRUE,placement="top"),
                            textAreaInput(paste0("action11",id),label=NULL,width="200%",height="200%",rows=2))),
                                           tags$img(src="evidencehierarchy.png", width = 682, height = 305),
                       br(),
                       paste("An evidence hierarchy tool adapted from "), 
                       a("Mupepele et al. (2016)", href="https://doi.org/10.1890/15-0595", target="_blank"), 
                       paste("to help assess the strength of evidence from different sources of scientific evidence. A useful diagram of different study designs can be found in"),
                       a("Christie et al. 2021.",href="https://www.nature.com/articles/s41467-020-20142-y/figures/1", target="_blank"),
                       br(),
                       br(),
                       h4("2.B.ii. Undocumented knowledge"),
                       em(strong("How locally effective is this action likely to be based on you and your stakeholders’ knowledge? What is the overall certainty (reliability) of this knowledge?")),
                       br(),
                       paste("We use the term ‘undocumented knowledge’ for the purposes of this tool to specify information that is not published or written down, which typically includes a knowledge holder’s intuition, experience, wisdom, and values (also known as ‘tacit’ knowledge).
                         For example, have you attempted this action yourself in the past? Are there any descriptive notes or reports from your organisation that can help? Do local stakeholders have any information or local knowledge you can integrate? 
                             Try to critically assess the uncertainty associated with this knowledge (i.e., How much expertise does the person offering the local knowledge have? What biases are they prone to suffer from and how does this affect the trustworthiness of their evidence?). See the"),
                       a("E2D Tool Guide", href="e2dguide.pdf", target="_blank"), 
                       paste("for more guidance."),
                       br(),
                       br(),
                       fixedRow(
                         column(tippy("<strong> Undocumented knowledge </strong>", tooltip = '<p style="font-size:15px"> e.g., The last ranger sent me a photo and some notes on a previous trial of a tunnel under the old road, that has since been resurfaced and rerouted, when it was first built did not record any Natterjack toads using it over a two year period. I have never seen them use any Natterjack toads using tunnels and culverts in other places on this reserve. </p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action12",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(column(width=12,tippy("<h4> Score effectiveness based on scientific literature and undocumented knowledge </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered. Note you should only consider effectiveness in terms of the focal target (i.e., harmful means harmful to the focal target, not more widely - side-effects are considered later).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actioneff",id),choices=c("Harmful","Ineffective","Weakly effective","Moderately effective","Highly effective","Trade-off between benefits and harms","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actioneffcert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       br(),
                       h4("2.C. Assess costs, risks, and wider benefits"),
                       h4("2.C.i. Assess financial and resource-based cost-effectiveness"),
                       em(strong("How much does the action cost financially and what are its resource requirements? What is the overall certainty (reliability) of these costs?")),
                       p("Resource requirements and financial costs form the core of assessing the cost-effectiveness of each action. These can be broadly defined as the resources and finances required to implement a conservation action."),
                       p("It is good practice to ensure estimates of cost include the direct costs of implementation (including labour, time, consumables, overheads and equipment) and possibly changes in future finances predicted as a result of the action including opportunity costs (i.e., loss of income) and costs of future management and monitoring. Any cost benefits, for example solving a problem (e.g., removing an invasive species) and not having to pay recurrent costs, can also be considered. Cost information can be collated from literature, guidance and accounts but also from experience and knowledge. It is useful to ensure that costs for each action are considered on the same scale so that they are comparable - for example, the cost per unit area or per unit of effort."),
                       fixedRow(
                         column( tippy("<strong> Financial and resource-based cost-effectiveness </strong>", tooltip = '<p style="font-size:15px">e.g., This is likely to cost a significant amount in time, construction labour, and materials...</p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action13",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(column(width=12,tippy("<h4> Score cost-effectiveness based on financial and resource-based costs </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actioncost1",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actioncost1cert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       h4("2.C.ii. Assess the non-financial costs, risks, and wider benefits for non-target species, habitats, and stakeholders"),
                       em(strong("What are the wider non-financial costs, risks, and wider benefits of implementing this action?")),
                       p("Non-financial costs and wider benefits are the wider undesirable and desirable effects of the action on species, habitats, and stakeholders that are not the focus of the action. Costs may include socio-cultural considerations if the action did not target socio-cultural outcomes; for example, considering whether using pesticides, excluding access, or removing invasive species may have 'reputational costs' to the practitioner, stakeholders, or their organisations (i.e., has a negative impact on how they are perceived by the general public or other groups). "),
                       fixedRow(
                         column( tippy("<strong> Non-financial, non-target costs and risks </strong>", tooltip = '<p style="font-size:15px">e.g., Tunnels and culverts could cause the deaths of other species of amphibians and animals...</p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action14",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(
                         column( tippy("<strong> Wider benefits </strong>", tooltip = '<p style="font-size:15px">e.g., Tunnels and culverts could also save many other species from suffering road mortality...</p>', allowHTML=TRUE,placement="top"),
                                 textAreaInput(paste0("action15",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(column(width=12,tippy("<h4> Score balance between non-financial, non-target costs, risks, and wider benefits </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actioncost2",id),choices=c("Costs/risks far greater","Costs/risks slightly greater","Trade-off between costs/risks and benefits","Benefits slightly greater","Benefits far greater", "Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actioncost2cert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       br(),
                       br(),
                       h4("2.D. Assess acceptability"),
                       em(strong("Are the effects of implementing this action acceptable to all the key stakeholders? Are there sociocultural barriers to implementing this action?")),
                       p("Carefully consider whether it is acceptable to implement this action - do the outcomes of this action align to the values held by yourself and key stakeholders? Before you decide, it may be helpful to identify the major relevant values held by yourself and key stakeholders."),
                       fixedRow(
                         column(tippy("<strong> Acceptability </strong>", tooltip = '<p style="font-size:15px">e.g., If the tunnels and culverts cause many deaths of amphibians then our reputation could suffer. This is likely to be unacceptably risky.</p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action16",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(column(width=12,tippy("<h4> Score Acceptability </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actionacc",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actionacccert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       br(),
                       br(),
                       h4("2.E. Assess feasibility"),
                       em(strong("Can this action be successfully accomplished and properly implemented?")),
                       p("Assessing the feasibility of actions involves considering both the costs and acceptability of the action to key stakeholders. For example, resistance to the action from key stakeholders will be important if cooperation is a part key of its success and so if the action is likely to be unacceptable to key stakeholders, its feasibility is also likely to be low. Considering access or availability of equipment, resources, or staff to undertake a management action will also be important; for example, an action may not be feasible if the equipment needed cannot be moved to the location of interest. Feasibility may also involve considering the costs associated with each action, such as whether the action exceeds a strict budget or will be able to be approved by any stakeholders that must agree to its implementation."),
                       p("Carefully consider whether it is feasible to implement this action."),
                       fixedRow(
                         column(tippy("<strong> Feasibility </strong>", tooltip = '<p style="font-size:15px">e.g., We would need to get permission to install these structures under the road, which could take time...</p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action17",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(column(width=12,tippy("<h4> Score Feasibility </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actionfeas",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actionfeascert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       br(),
                       br(),
                       h4("2.F. Consider modifications"),
                       em(strong("How can the action be modified based on the previous evidence gathered?")),
                       p("By assessing the evidence from different sources on effectiveness, costs, acceptability, and feasibility of the action, modifications can be considered that might improve it. For example, there may be strong evidence from the scientific literature to suggest that creating certain habitats for great crested newts and white-faced darters will be beneficial, but a practitioner's explicit or tacit local knowledge also suggests that these species have slightly different habitat preferences in this region, and so a modification to this action may be necessary for it to be locally effective. Or an action such as an education campaign may not be acceptable to a key stakeholder if it is designed in a certain way, so modifications are necessary to ensure the action is acceptable. A structural action may also be too expensive to implement using certain materials and to be more cost-effective and ultimately more feasible, the action must be modified by using cheaper materials."),
                       fixedRow(
                         column(tippy("<strong> Consider effectiveness of modifications </strong>", tooltip = '<p style="font-size:15px">e.g., We could try certain designs of culverts and tunnels that limit mortality...</p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action18",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                       br(),
                       fixedRow(column(width=12,tippy("<h4> Score potential of modifications to improve action </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actionmod",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                       radioButtons(paste0("actionmodcert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                       br(),
                       br(),
                       h4("2.G. Summarise the evidence gathered"),
                       em(strong("How likely is this action to be locally effective based on all the evidence and information you have gathered?")),
                       em(strong("What is the overall level of uncertainty associated with these conclusions?")),
                       p("Once the previous steps have been considered, it may be useful to summarise the likely local effectiveness of each action (whether modified or not), and the important costs, acceptability, and feasibility considerations that come with them. This draws together all the evidence previously gathered so that an evidence-based decision can be made in the next step, considering the relative advantages and disadvantages of each action alongside each other.
                         Uncertainty is also important to consider here, in particular to understand whether the evidence that has been gathered is sufficient in its reliability and relevance to make robust conclusions. It is also important to consider if there is conflicting evidence from different sources - for example, how much trust can be placed in the evidence drawn from the scientific literature versus evidence drawn from local knowledge?"),
                       fixedRow(
                         column(tippy("<strong> Summarise evidence for and against implementation </strong>", tooltip = '<p style="font-size:15px"> e.g., Overall, this action may be an effective action for us to use but could have negative impacts on amphibians that make this action unacceptably risky - not only in terms of damage to wildlife, but also to the reputation of an organisation even if the design of the culverts and tunnels was modified. The installation of these structures will also take time and require extensive permissions, alongside costing a substantial amount, so this is also unlikely to be a feasible action to implement.</p>', allowHTML=TRUE,placement="top"),
                           textAreaInput(paste0("action19",id),label=NULL,width="200%",height="200%",rows=2),width=12))
                       ),select=TRUE, session=session)
    
})
  
  updateidlist <- reactiveValues(data = NULL)
  
  onBookmark(function(state){
    state$values$currentids <- updateidlist$data
    state$values$addedids <- addednames$data
    state$values$removedids <- removednames$data
  })
  
  addednames <- reactiveValues(data = NULL)
  removednames <- reactiveValues(data = NULL)
  
  observeEvent(input$add,{
    addednames$data <- c(addednames$data,input$newactionname)
    if(length(grep(input$newactionname,removednames$data))>0){
    removednames$data <- removednames$data[-grep(input$newactionname,removednames$data)]
    }
  })
  
  observeEvent(input$remove,{
    id <- input$newactionnamerem
    removeTab(inputId = "tab03_extra", target = glue("2.B-G. ",id,"_val"), session=session)
    removednames$data <- c(removednames$data,input$newactionnamerem) 
  })
  
  observeEvent({input$remove|input$add},{
    updateidlist$data <- setdiff(addednames$data,removednames$data)
  })
  
  onRestore(function(state){
    
    updateidlist$data <- state$values$currentids
    addednames$data <- state$values$addedids
    removednames$data <- state$values$removedids
    
    idstorestore <- updateidlist$data
    for(i in 1:length(idstorestore)){
      id <- idstorestore[i]
      appendTab(inputId = "tab03_extra",
                tabPanel(title = glue("2.B-G. ",id), id=glue("2.B-G. ",id), value=glue("2.B-G. ",id,"_val"),
                         tags$style(
                           "li a {
                           font-size: 20px;
                           font-weight: bold;
    }
                           "
                         ),
                         br(),
                         p("Remember to complete the steps below for each action by generating separate tabs using the right-hand menu above."),
                         p("Please expand in more detail on the proposed action below, including what the action involves and what the focus of the action is (i.e., a species, group, or habitat - this is not necessarily the focal target you defined earlier as this action could be indirect)."),
                         fixedRow(
                           column(tippy("<strong> Describe action </strong>", tooltip = '<p style="font-size:15px"> e.g., Install culverts or tunnels that can act as underpasses for amphibians. </p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action07",id),label=NULL,width="100%",height="100%",rows=2),width=6),
                           column(tippy("<strong> Focus of action </strong>", tooltip = '<p style="font-size:15px"> e.g., Assist amphibians, particularly Natterjack toads, across the road through constructing culverts or tunnels under the road. </p>',placement="top"),
                                  textAreaInput(paste0("action08",id),label=NULL,width="100%",height="100%",rows=2),width=6)),
                         h4("2.B. Assess desirable and undesirable effects on the focal target and uncertainty"),
                         strong(em("What do different types of evidence tell us about the desirable and undesirable effects of each action on the focal target? How certain are we of the credibility of this evidence?")),
                         p("In this section you are asked to assess the likely effectiveness of this action within your local decision context (specifically regarding your focal target - there is space later in the tool to assess cost-effectiveness, side-effects, and wider impacts of actions)."),
                         h4("2.B.i. Scientific literature"),
                         em(strong("How locally effective is this action likely to be based on evidence from the scientific literature? What is the overall certainty (reliability) of this evidence?")),
                         br(),
                         paste("Please describe below where you found evidence for this action and your assessment of its findings. For example, you could draw from the Conservation Evidence assessment for each action if you have used this "),
                         a("website", href="https://www.conservationevidence.com", target="_blank"), 
                         paste(" to look for evidence. Or you might have access to reports or studies from your own organisation (i.e., the grey literature) that provide useful evidence - see also"),
                         a("Applied Ecology Resources.", href="https://www.britishecologicalsociety.org/applied-ecology-resources/search/", target="_blank"),
                         paste("Ensure you consider the strength (quality) of the evidence (i.e., the reliability of the study or experimental design used) and the relevance of this evidence (i.e., are the results likely to apply to your local setting?). Please use the"),
                         a("E2D Tool Guide", href="e2dguide.pdf", target="_blank"), 
                         paste("for further information on how to critically appraise evidence and account for biases (see the evidence hierarchy diagram below too)."),
                         br(),
                         br(),
                         fixedRow(
                           column(width=12,tippy("<strong> Evidence sources considered </strong>", tooltip = '<p style="font-size:15px"> e.g., Conservation Evidence, systematic reviews, reports. </p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action09",id),label=NULL,width="200%",height="200%",rows=2))),
                         fixedRow(
                           column(width=12,tippy("<strong> Detailed assessment of evidence </strong>", tooltip = '<p style="font-size:15px"> e.g., This action was assessed as likely to be beneficial on Conservation Evidence by a panel of experts. Several studies found... </p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action10",id),label=NULL,width="200%",height="200%",rows=3))),
                         fixedRow(
                           column(width=12,tippy("<strong> Summarise assessment of evidence </strong>", tooltip = '<p style="font-size:15px"> e.g., Overall, based on the sources above this action is likely to be beneficial, but we need to consider how to apply it effectively in our local setting. </p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action11",id),label=NULL,width="200%",height="200%",rows=2))),
                         tags$img(src="evidencehierarchy.png", width = 682, height = 305),
                         br(),
                         paste("An evidence hierarchy tool adapted from "), 
                         a("Mupepele et al. (2016)", href="https://doi.org/10.1890/15-0595", target="_blank"), 
                         paste("to help assess the strength of evidence from different sources of scientific evidence. A useful diagram of different study designs can be found in"),
                         a("Christie et al. 2021.",href="https://www.nature.com/articles/s41467-020-20142-y/figures/1", target="_blank"),
                         br(),
                         br(),
                         h4("2.B.ii. Undocumented knowledge"),
                         em(strong("How locally effective is this action likely to be based on you and your stakeholders’ knowledge? What is the overall certainty (reliability) of this knowledge?")),
                         br(),
                         paste("We use the term ‘undocumented knowledge’ for the purposes of this tool to specify information that is not published or written down, which typically includes a knowledge holder’s intuition, experience, wisdom, and values (also known as ‘tacit’ knowledge).
                               For example, have you attempted this action yourself in the past? Are there any descriptive notes or reports from your organisation that can help? Do local stakeholders have any information or local knowledge you can integrate? 
                               Try to critically assess the uncertainty associated with this knowledge (i.e., How much expertise does the person offering the local knowledge have? What biases are they prone to suffer from and how does this affect the trustworthiness of their evidence?). See the"),
                         a("E2D Tool Guide", href="e2dguide.pdf", target="_blank"), 
                         paste("for more guidance."),
                         br(),
                         br(),
                         fixedRow(
                           column(tippy("<strong> Undocumented knowledge </strong>", tooltip = '<p style="font-size:15px"> e.g., The last ranger sent me a photo and some notes on a previous trial of a tunnel under the old road, that has since been resurfaced and rerouted, when it was first built did not record any Natterjack toads using it over a two year period. I have never seen them use any Natterjack toads using tunnels and culverts in other places on this reserve. </p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action12",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(column(width=12,tippy("<h4> Score effectiveness based on scientific literature and undocumented knowledge </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered. Note you should only consider effectiveness in terms of the focal target (i.e., harmful means harmful to the focal target, not more widely - side-effects are considered later).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actioneff",id),choices=c("Harmful","Ineffective","Weakly effective","Moderately effective","Highly effective","Trade-off between benefits and harms","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actioneffcert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         br(),
                         h4("2.C. Assess costs, risks, and wider benefits"),
                         h4("2.C.i. Assess financial and resource-based cost-effectiveness"),
                         em(strong("How much does the action cost financially and what are its resource requirements? What is the overall certainty (reliability) of these costs?")),
                         p("Resource requirements and financial costs form the core of assessing the cost-effectiveness of each action. These can be broadly defined as the resources and finances required to implement a conservation action."),
                         p("It is good practice to ensure estimates of cost include the direct costs of implementation (including labour, time, consumables, overheads and equipment) and possibly changes in future finances predicted as a result of the action including opportunity costs (i.e., loss of income) and costs of future management and monitoring. Any cost benefits, for example solving a problem (e.g., removing an invasive species) and not having to pay recurrent costs, can also be considered. Cost information can be collated from literature, guidance and accounts but also from experience and knowledge. It is useful to ensure that costs for each action are considered on the same scale so that they are comparable - for example, the cost per unit area or per unit of effort."),
                         fixedRow(
                           column( tippy("<strong> Financial and resource-based cost-effectiveness </strong>", tooltip = '<p style="font-size:15px">e.g., This is likely to cost a significant amount in time, construction labour, and materials...</p>', allowHTML=TRUE,placement="top"),
                                   textAreaInput(paste0("action13",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(column(width=12,tippy("<h4> Score cost-effectiveness based on financial and resource-based costs </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actioncost1",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actioncost1cert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         h4("2.C.ii. Assess the non-financial costs, risks, and wider benefits for non-target species, habitats, and stakeholders"),
                         em(strong("What are the wider non-financial costs, risks, and wider benefits of implementing this action?")),
                         p("Non-financial costs and wider benefits are the wider undesirable and desirable effects of the action on species, habitats, and stakeholders that are not the focus of the action. Costs may include socio-cultural considerations if the action did not target socio-cultural outcomes; for example, considering whether using pesticides, excluding access, or removing invasive species may have 'reputational costs' to the practitioner, stakeholders, or their organisations (i.e., has a negative impact on how they are perceived by the general public or other groups). "),
                         fixedRow(
                           column( tippy("<strong> Non-financial, non-target costs and risks </strong>", tooltip = '<p style="font-size:15px">e.g., Tunnels and culverts could cause the deaths of other species of amphibians and animals...</p>', allowHTML=TRUE,placement="top"),
                                   textAreaInput(paste0("action14",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(
                           column( tippy("<strong> Wider benefits </strong>", tooltip = '<p style="font-size:15px">e.g., Tunnels and culverts could also save many other species from suffering road mortality...</p>', allowHTML=TRUE,placement="top"),
                                   textAreaInput(paste0("action15",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(column(width=12,tippy("<h4> Score balance between non-financial, non-target costs, risks, and wider benefits </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actioncost2",id),choices=c("Costs/risks far greater","Costs/risks slightly greater","Trade-off between costs/risks and benefits","Benefits slightly greater","Benefits far greater", "Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actioncost2cert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         br(),
                         br(),
                         h4("2.D. Assess acceptability"),
                         em(strong("Are the effects of implementing this action acceptable to all the key stakeholders? Are there sociocultural barriers to implementing this action?")),
                         p("Carefully consider whether it is acceptable to implement this action - do the outcomes of this action align to the values held by yourself and key stakeholders? Before you decide, it may be helpful to identify the major relevant values held by yourself and key stakeholders."),
                         fixedRow(
                           column(tippy("<strong> Acceptability </strong>", tooltip = '<p style="font-size:15px">e.g., If the tunnels and culverts cause many deaths of amphibians then our reputation could suffer. This is likely to be unacceptably risky.</p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action16",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(column(width=12,tippy("<h4> Score Acceptability </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actionacc",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actionacccert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         br(),
                         br(),
                         h4("2.E. Assess feasibility"),
                         em(strong("Can this action be successfully accomplished and properly implemented?")),
                         p("Assessing the feasibility of actions involves considering both the costs and acceptability of the action to key stakeholders. For example, resistance to the action from key stakeholders will be important if cooperation is a part key of its success and so if the action is likely to be unacceptable to key stakeholders, its feasibility is also likely to be low. Considering access or availability of equipment, resources, or staff to undertake a management action will also be important; for example, an action may not be feasible if the equipment needed cannot be moved to the location of interest. Feasibility may also involve considering the costs associated with each action, such as whether the action exceeds a strict budget or will be able to be approved by any stakeholders that must agree to its implementation."),
                         p("Carefully consider whether it is feasible to implement this action."),
                         fixedRow(
                           column(tippy("<strong> Feasibility </strong>", tooltip = '<p style="font-size:15px">e.g., We would need to get permission to install these structures under the road, which could take time...</p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action17",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(column(width=12,tippy("<h4> Score Feasibility </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actionfeas",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actionfeascert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         br(),
                         br(),
                         h4("2.F. Consider modifications"),
                         em(strong("How can the action be modified based on the previous evidence gathered?")),
                         p("By assessing the evidence from different sources on effectiveness, costs, acceptability, and feasibility of the action, modifications can be considered that might improve it. For example, there may be strong evidence from the scientific literature to suggest that creating certain habitats for great crested newts and white-faced darters will be beneficial, but a practitioner's explicit or tacit local knowledge also suggests that these species have slightly different habitat preferences in this region, and so a modification to this action may be necessary for it to be locally effective. Or an action such as an education campaign may not be acceptable to a key stakeholder if it is designed in a certain way, so modifications are necessary to ensure the action is acceptable. A structural action may also be too expensive to implement using certain materials and to be more cost-effective and ultimately more feasible, the action must be modified by using cheaper materials."),
                         fixedRow(
                           column(tippy("<strong> Consider effectiveness of modifications </strong>", tooltip = '<p style="font-size:15px">e.g., We could try certain designs of culverts and tunnels that limit mortality...</p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action18",id),label=NULL,width="200%",height="200%",rows=2),width=12)),
                         br(),
                         fixedRow(column(width=12,tippy("<h4> Score potential of modifications to improve action </h4>", tooltip = '<p style="font-size:15px">Please select a score based on the evidence gathered.</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actionmod",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         fixedRow(column(width=12,tippy("<strong> Score certainty in this score </strong>", tooltip = '<p style="font-size:15px">Please select a score based on your confidence in the score you have given (and the evidence gathered).</p>', allowHTML=TRUE,placement="top"),
                                         radioButtons(paste0("actionmodcert",id),choices=c("Very low","Low","Moderate","High","Unsure"),selected="Unsure",inline=TRUE,label=NULL,width="200%"))),
                         br(),
                         br(),
                         h4("2.G. Summarise the evidence gathered"),
                         em(strong("How likely is this action to be locally effective based on all the evidence and information you have gathered?")),
                         em(strong("What is the overall level of uncertainty associated with these conclusions?")),
                         p("Once the previous steps have been considered, it may be useful to summarise the likely local effectiveness of each action (whether modified or not), and the important costs, acceptability, and feasibility considerations that come with them. This draws together all the evidence previously gathered so that an evidence-based decision can be made in the next step, considering the relative advantages and disadvantages of each action alongside each other.
                           Uncertainty is also important to consider here, in particular to understand whether the evidence that has been gathered is sufficient in its reliability and relevance to make robust conclusions. It is also important to consider if there is conflicting evidence from different sources - for example, how much trust can be placed in the evidence drawn from the scientific literature versus evidence drawn from local knowledge?"),
                         fixedRow(
                           column(tippy("<strong> Summarise evidence for and against implementation </strong>", tooltip = '<p style="font-size:15px"> e.g., Overall, this action may be an effective action for us to use but could have negative impacts on amphibians that make this action unacceptably risky - not only in terms of damage to wildlife, but also to the reputation of an organisation even if the design of the culverts and tunnels was modified. The installation of these structures will also take time and require extensive permissions, alongside costing a substantial amount, so this is also unlikely to be a feasible action to implement.</p>', allowHTML=TRUE,placement="top"),
                                  textAreaInput(paste0("action19",id),label=NULL,width="200%",height="200%",rows=2),width=12))
                         ),select=TRUE, session=session)
  }
  })
  
  output$testtab <- renderDT(server=FALSE,{
    idlist <- updateidlist$data
    if(length(idlist)>0){
      actnames<-lapply(1:length(idlist),
                       function(i){data.table("Action"=idlist[i],
                                              "Local Effectiveness"=input[[glue("actioneff",idlist[i])]],
                                              "Cost-effectiveness"=input[[glue("actioncost1",idlist[i])]],
                                              "Balance between wider non-target costs, risks, and benefits"=input[[glue("actioncost2",idlist[i])]],
                                              "Acceptability"=input[[glue("actionacc",idlist[i])]],
                                              "Feasibility"=input[[glue("actionfeas",idlist[i])]],
                                              "Modification potential"=input[[glue("actionmod",idlist[i])]],
                                              "Local Effectiveness Certainty"=input[[glue("actioneffcert",idlist[i])]],
                                              "Cost-effectiveness Certainty"=input[[glue("actioncost1cert",idlist[i])]],
                                              "Balance between wider non-target costs, risks, and benefits Certainty"=input[[glue("actioncost2cert",idlist[i])]],
                                              "Acceptability Certainty"=input[[glue("actionacccert",idlist[i])]],
                                              "Feasibility Certainty"=input[[glue("actionfeascert",idlist[i])]],
                                              "Modification potential Certainty"=input[[glue("actionmodcert",idlist[i])]]
                                     )})
      
    table1 <- data.table(do.call(rbind,actnames))
    colvalscert <- c("#FFFFFF","#FFFFCC","#FFFF80","#FFCC00","#FFFFFF")
    colvalnames <- c("Very low","Low","Moderate","High","Unsure")
    
    datatable(table1,rownames=FALSE,height="100%",
              extensions=c('ColReorder','Responsive','FixedHeader'),
              options = list(searching=FALSE, paging=FALSE, info=FALSE,
                fixedHeader = TRUE,
                scrolly = TRUE,
                colReorder = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = 0:6),list(visible=FALSE,targets=7:12)) # columns aligment to center, javascript starts at 0 not 1!
              )
              
    ) %>% formatStyle(columns="Local Effectiveness", valueColumns="Local Effectiveness Certainty", backgroundColor = styleEqual(colvalnames,colvalscert))%>%
     formatStyle(columns="Cost-effectiveness", valueColumns="Cost-effectiveness Certainty", backgroundColor = styleEqual(colvalnames,colvalscert))%>%
      formatStyle(columns="Balance between wider non-target costs, risks, and benefits", valueColumns="Balance between wider non-target costs, risks, and benefits Certainty", backgroundColor = styleEqual(colvalnames,colvalscert))%>%
        formatStyle(columns="Acceptability", valueColumns="Acceptability Certainty", backgroundColor = styleEqual(colvalnames,colvalscert))%>%
          formatStyle(columns="Feasibility", valueColumns="Feasibility Certainty", backgroundColor = styleEqual(colvalnames,colvalscert))%>%
            formatStyle(columns="Modification potential", valueColumns="Modification potential Certainty", backgroundColor = styleEqual(colvalnames,colvalscert))
    }
  else{
    table1 <- data.frame(cbind(rep("Start assessing actions in the tool to display a summary output here."),"","","","","",""))
    colnames(table1)<- c("Action","Local Effectiveness", "Cost-effectiveness", "Balance between wider non-target costs, risks, and benefits", "Acceptability","Feasibility","Modification potential")
    datatable(table1,rownames=FALSE,height="100%",
              extensions=c('ColReorder','Responsive','FixedHeader'),
              options = list(searching=FALSE, paging=FALSE, info=FALSE,
                fixedHeader = TRUE,
                scrolly = TRUE,
                colReorder = TRUE),
              editable="cell"
    )
    
  }
  })  
  
    
  output$step3summarytext <- renderUI({
    idlist <- updateidlist$data
    if(length(idlist)>0){
      lapply(1:length(idlist), function(i){
        HTML(paste0(paste0('<b>',idlist[i],sep='</b>'), paste0(": ",input[[glue("action19",idlist[i])]], sep = '<br/><br/>')))
             })
    }
    else{HTML(paste0("(Please complete Step 2 to display summary text here for different actions)."))}
    
  })
    
    
  setBookmarkExclude(c("add","remove","newactionname","newactionnamerem"))
  
  
  #output$numids <- renderText({sort(updateidlist$data)})
  
  #/srv/shiny-server/evidence2decisiontool/ remove this to download reports when piloting local version.
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste(list('Evidence-to-decision_summary',Sys.Date()), sep = '.', switch(
          input$format, PDF = 'pdf',  Word = 'docx'
        ))
      },
      
      content = function(file) {
        library(rmarkdown)
          out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
  
  #/srv/shiny-server/evidence2decisiontool/ remove this to download reports when piloting local version.
  output$downloadReporteg <- downloadHandler(
      filename = function() {
        paste(list('Example_evidence-to-decision_summary',Sys.Date()), sep = '.', switch(
          input$formateg, PDF = 'pdf',  Word = 'docx'
        ))
      },
      
      content = function(file) {
        library(rmarkdown)
        out <- render('reporteg.Rmd', switch(
          input$formateg,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
  

  textsummary <- function(){
    idlist <- updateidlist$data
    if(length(idlist)>0){
      lapply(1:length(idlist), 
             function(i){
               cat("# ",idlist[i],"  \n  \n")
               cat("## Describe action  \n",input[[glue("action07",idlist[i])]],"  \n  \n")
               cat("## Focus of action  \n",input[[glue("action08",idlist[i])]],"  \n  \n  \n")
               cat("## Assess desirable and undesirable effects on the focal target and uncertainty  \n  \n")
               cat("### Scientific literature  \n")
               cat("#### Evidence sources considered  \n",input[[glue("action09",idlist[i])]],"  \n  \n")
               cat("#### Detailed assessment of evidence  \n",input[[glue("action10",idlist[i])]],"  \n  \n")
               cat("#### Summarise assessment of scientific evidence  \n",input[[glue("action11",idlist[i])]],"  \n  \n  \n")
               cat("### Undocumented knowledge  \n",input[[glue("action12",idlist[i])]],"  \n  \n")
               cat("#### Assessment of effectiveness  \n","Score: ",input[[glue("actioneff",idlist[i])]],"  \n  \n","Certainty: ", input[[glue("actioneffcert",idlist[i])]],"  \n  \n  \n")
               cat("## Costs and risks  \n")
               cat("### Financial and resource-based costs  \n",input[[glue("action13",idlist[i])]],"  \n  \n")
               cat("#### Assessment of cost-effectiveness  \n","Score: ",input[[glue("actioncost1",idlist[i])]],"  \n  \n","Certainty: ",input[[glue("actioncost1cert",idlist[i])]],"  \n  \n  \n")
               cat("### Non-financial and non-target costs and risks  \n",input[[glue("action14",idlist[i])]],"  \n  \n")
               cat("### Wider benefits  \n",input[[glue("action15",idlist[i])]],"  \n  \n")
               cat("#### Assessment of balance between wider non-target costs, risks, and benefits  \n","Score: ",input[[glue("actioncost2",idlist[i])]],"  \n  \n","Certainty: ",input[[glue("actioncost2cert",idlist[i])]],"  \n  \n  \n")
               cat("## Acceptability  \n",input[[glue("action16",idlist[i])]],"  \n  \n")
               cat("## Assessment of Acceptability  \n","Score: ",input[[glue("actionacc",idlist[i])]],"  \n  \n","Certainty: ",input[[glue("actionacccert",idlist[i])]],"  \n  \n  \n")
               cat("## Feasibility  \n",input[[glue("action17",idlist[i])]],"  \n  \n")
               cat("## Assessment of Feasibility  \n","Score: ",input[[glue("actionfeas",idlist[i])]],"  \n  \n","Certainty: ",input[[glue("actionfeascert",idlist[i])]],"  \n  \n  \n")
               cat("## Consider modifications  \n",input[[glue("action18",idlist[i])]],"  \n  \n")
               cat("## Assessment of potential for modifications to improve action  \n","Score: ",input[[glue("actionmod",idlist[i])]],"  \n  \n","Certainty: ",input[[glue("actionmodcert",idlist[i])]],"  \n  \n  \n")
               cat("## Summarise the evidence gathered  \n",input[[glue("action19",idlist[i])]],"  \n  \n")
               cat("\\newpage  \n  \n")
             })
    }
    if(length(idlist)==0){
      cat("Please add some actions to display them in this report.")
    }
  }
  onBookmarked(updateQueryString)
  }

enableBookmarking(store="server")
shinyApp(ui, server)
