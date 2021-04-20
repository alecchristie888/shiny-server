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

ui <- function(request){fluidPage(
  titlePanel(title=tags$a(href='http://conservationevidence.com', target="_blank",
                          tags$img(src="image1.jpg", width = 240, height = 80)),
             windowTitle = "Evidence-to-Decision tool"),
  titlePanel(title = "Evidence-to-Decision tool"),
  sidebarLayout(
    sidebarPanel(
      h4("Bookmark your work"),
      p("We recommended that you bookmark regularly to save your work. Your browser URL will update each time you do this (leaving the page or refreshing before doing so could lose your work)."),
      bookmarkButton(label="Bookmark", title="Bookmarking your work will create a new unique URL in your browser that saves your work."),
      br(),
      br(),
      h4("Add or remove actions"),
      with_tippy(textInput("newactionname","Name of action to add", value=NULL, placeholder ="Install culverts or tunnels as road crossings"),
      tooltip="On reaching step '2.a Identify possible actions' you can add actions below to continue the process."), 
      actionButton("add", "Add this action"),
      br(),
      br(),
      textInput("newactionnamerem","Name of action to remove", value=NULL, placeholder = "Install barrier fencing along roads"),
      actionButton("remove", "Remove this action")
      ),
    mainPanel(
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
                                The tool is structured to help you consider and combine several forms of evidence (e.g., scientific evidence, tacit knowledge, values, costs) to reach a transparent decision, documenting each stage of the process so that the logic and reasnoning behind decisions can be open and traceable."),
                          br(),br(),
                          p("The tool is structured using three steps: 1.) Define the Decision Context (i.e., What is the problem you want to solve?); 2.) Gather Evidence (i.e., What actions are likely to be the most effective to address my problem in my local context?);
                            3.) Make an Evidence-Based Decision (i.e., What are the next steps? Which actions will be implemented based on the evidence you have assessed?). The diagram below lays out the detailed steps that this tool will guide you through."),
                          p("This tool is best suited for use by individual landowners, reserve managers, and small NGOs working on specific projects to come to an evidence-based decision for a specific problem. The tool was designed to streamline an evidence-based decision-making process with limited time and resources. 
                            The tool can also be used to begin thinking about how to tackle major decisions, laying the foundation for a more in-depth decision-making process using other tools and frameworks (e.g., Structured Decision-Making, Multi-Criteria Decision Analysis, or Theory of Change etc.)."), 
                          paste("To begin using the tool, click on 1. Define the Decision Context. Throughout this tool you will be prompted to enter information which will be compiled and documented in a report which you will be able to download in the final step. (If you wish to see an example of what the tool produces, please go to tab 3. Make an Evidence-Based Decision and scroll to the bottom). To download a guide of how to use the tool"), 
                          a("click here.", href="", target="_blank"), paste("Tips will also appear if you hover over text boxes in the tool."),
                          tags$img(src="image3.png", width = 750, height = 780),
                          br(),
                          p("Thank you to all the practitioners who took part in the co-design of this tool, including (in no particular order): Steve Weeks, Alison Ruyter, Rory Harding, and Paul Tinsley-Marshall from the Kent Wildlife Trust; Tom McPherson from Ingleby Farms (also for giving feedback on the manuscript); the Woodland Trust; Peoples’ Trust for Endangered Species; Jon Flanders and Winifred Frick at Bat Conservation International; David O’Brien at NatureScot; Kathy Wormald at Froglife; the Medway Valley Countryside Partnership; Sheffield & Rotherham Wildlife Trust; Bedfordshire, Buckinghamshire, and Oxfordshire Wildlife Trust; Catherine McNicol at Gloucestershire Wildlife Trust.")
                          ),
                 tabPanel("1. Define the Decision Context",id="tab02",value='tab02_val',
                          em(strong("What is the problem and desired outcomes? What is the relevant ecological, physical, and social context underlying the decision?")),
                          p("It is important to carefully define and detail the context surrounding the decision to be made. 
                            This includes the problem (or direct threat) being tackled, the location the decision affects,
                            the ultimate goal you want to achieve (i.e., the desired outcomes), the focal target of any action (i.e., the species, habitat, or group),
                            and any other relevant contextual information (e.g., socio-ecological factors, constraints on decision-making."),
                          fixedRow(column(
                            with_tippy(textAreaInput("action01",label="What is the problem?",width="200%",height="200%",rows=3),
                                                         tooltip="e.g., Large numbers of amphibians are being killed when crossing a road that runs through part of the reserve.",placement="top"),width=12)),
                          fixedRow(column(
                            with_tippy(textAreaInput("action02",label="Location", width="100%",height="100%",rows=1),
                                          tooltip="State the location for the proposed action.",placement="top"),width=12)),
                          fixedRow(column(
                            with_tippy(textAreaInput("action03",label="What is the ultimate goal?",width="200%",height="200%",rows=3),
                                       tooltip="e.g., Reduce amphibian mortality but still enable the movement of frogs across the road.",placement="top"),width=12)),
                          fixedRow(column(
                            with_tippy(textAreaInput("action04",label="What is the focal target?",width="200%",height="200%",rows=3),
                                       tooltip="e.g., Prevent the decline of a Natterjack toad (Epidalea calamita) population.",placement="top"),width=12)),
                          fixedRow(column(
                            with_tippy(textAreaInput("action05",label="What is the relevant ecological, physical, and social context underlying the decision?",width="200%",height="200%",rows=3),
                                       tooltip="e.g., The road was built in 2017 and since then our monitoring and anectodotal evidence has suggested that a substantial proportion of amphibians are being killed on a key section of the road. We believe this may be a major factor behind the continued decline of a population of Natterjack toads (Epidalea calamita) despite our best efforts to provide suitable habitat. This toad is a key species we need to protect due to their wider national decline. We have a keen group of volunteers who can help with the work. The road is owned and maintained by the local council.",placement="top"),width=12))
                 ),
                 tabPanel("2. Gather Evidence",id="tab03",value='tab03_val',
                          em(strong("What evidence is available and what does it suggest about the action(s) that should be taken to address the problem?")),
                          p("Information can now be gathered to assess which action(s) to take to achieve the focal targets and goals defined in the previous tab (1. Define the Decision Context). This process starts with identfying possible actions to address your problem (e.g., different management actions) in the next tab (2.a), and then considering various types of evidence and information on the effectiveness, costs, acceptability, and feasibility of each action (one at a time - use the function on the side to add or remove different actions to reveal this tab). Once evidence and information has been gathered and assessed, you can move to tab '3. Make an Evidence-Based Decision' to draw together what you have found to summarise and justify your decision and next steps. All the information that you enter here will be used to create a downloadable summary report (you can view an example in the final tab)."),
                          br(),
                          h4("2.a Identify possible actions"),
                          em(strong("Which action(s) could be taken to address the problem, regardless of their cost, acceptability, or feasibility?")),
                          p("Brainstorm and note as many possible actions as you can to address the decision or problem you are considering."),
                          p("Don't forget to look into the literature and search websites like",a("Conservation Evidence", href="https://www.conservationevidence.com", target="_blank"),"to inform your list of actions. This is called solution scanning. Make sure to include actions regardless of their acceptability, feasibility, or costs at this stage. These actions can be removed later if they are too expensive, unfeasible, or unacceptable."),
                          p("Once you have decided on your list of possible actions below, type them into the side bar on the right one at a time and click 'add action'. Each time you click 'add action' you will create tabs below to continue the process (steps 2.b-g) where you assess the evidence for each action. If you need to remove an action (e.g., because of a typo) simply enter the incorrect name of the action and click 'remove this action'."),
                          fixedRow(column( 
                            with_tippy(textAreaInput("action06",label="Possible actions",width="200%",height="200%",rows=3),
                                                         tooltip="Install culverts or tunnels as road crossings.\nInstall barrier fencing along roads.\nUse humans to assist migrating amphibians across roads.\n...\n...\n...",placement="top"),width=12)),
                          br(),
                          tabsetPanel(id="tab03_extra")
                 ),
                 tabPanel(title="3. Make an Evidence-Based Decision",id="tab13",value='tab13_val',
                          #                         br(),
                          #                         DTOutput("summtab"),
                          em(strong("Reflecting on the problem you face and the evidence and information you have gathered, what is your decision and why?")),
                          p("Using the accumulated evidence, the relative advantages and disadvantages of each modified action can be compared and related back to the original decision or problem being considered (in Step 1. Define the Decision Context). This involves weighing up how locally effective, cost-effective, acceptable, and feasible each action and whether its implementation is justified."),
                          em(strong("Which action(s), if any, are the best ones to implement to achieve the ultimate goal(s) you defined at the beginning? Name and justify your choices.")),
                          fixedRow(column( 
                            with_tippy(textAreaInput("action20",label=NULL,width="200%",height="200%",rows=3),
                                                     tooltip="e.g., Install barrier fencing along roads: This action has been shown to be effective from the evidence I have considered if it is implemented properly. The costs will be less than installing culverts or tunnels, and it should take less time to get permissions to install the fencing along the road. If we target the fencing at strategic positions, and make it high enough so Natterjack toads not climb over it, we can funnel them to natural watercourses underneath the road.",placement="top"),width=12)),
                          em(strong("Which action(s), if any, are not going to be implemented? Name and justify your choices.")),
                          fixedRow(column( 
                            with_tippy(textAreaInput("action21",label=NULL,width="200%",height="200%",rows=3),
                                                     tooltip="e.g., Use humans to assist migrating amphibians across roads: This is because based on my assessment of the evidence this action is unlikely to be effective on the reserve, as it has shown to not prevent population declines elsewhere and might divert volunteers away from activities that are more important. \n \nInstall culverts or tunnels as road crossings: This is because although this could be an effective action based on the scientific evidence, it can lead to mortality of amphibians within the culverts and tunnels. It would also cost a significant amount of money which would likely be beyond our budget, and would also take a lot of time to get permission to construct and build.",placement="top"),width=12)),
                          em(strong("What is your overall decision, what are the next steps, and why? Justify your decision and next steps.")),
                          p("Summarise your overall decision and the next steps you will take. This could be implementing these actions, pausing to make a more detailed assessment, gather more evidence, or decide to do nothing."),
                          fixedRow(column( 
                            with_tippy(textAreaInput("action22",label=NULL,width="200%",height="200%",rows=3),
                                                     tooltip="e.g., We will now investigate the correct height, material, and length of fencing needed and identify key stragetic points along the road to place the fencing. We will also request permission to install the fencing and trial it during the next migration season. We will write up the findings of this trial in a report, comparing it to previous years mortality, and publish this online through the Conservation Evidence journal.",placement="top"),width=12)),
                          br(),
                          h4("3.c. Document and report decision (download report)"),
                          strong("Example summary report"),
                          p("To download an example of a summary report without having to fill any information select a format and then click below."),
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
              tabPanel(title = glue("2.b-g ",id), id=glue("2.b-g ",id), value=glue("2.b-g ",id,"_val"),
                       tags$style(
                         "li a {
                         font-size: 20px;
                         font-weight: bold;
                          }
                         "
                       ),
                       br(),
                       h4("2.b Assess effects on the focal target"),
                       em("What does scientific evidence and local knowledge tell us about the desirable and undesirable effects of each action on the focal target?"),
                       p("Now you can assess the evidence on the likely effectiveness of this action in your local setting. Remember to do this for each action separately using the add or remove action buttons on the side."),
                       p("Move through the subtabs to consider what the effectiveness of this action is likely to be for your chosen setting based on evidence and information gathered from scientific evidence and local knowledge. Once you have done this you can consider the costs, acceptability, and feasibility of the action. Then it may be helpful to identify any specific modifications you may wish to make to tailor this conservation action to your local setting. Finally you can summarise what the evidence and information you have gathered suggests about the effectiveness, costs, acceptability, and feasibility of this action."),
                       br(),
                       br(),
                       h4("2.b.i Scientific Evidence and uncertainty"),
                       em(strong("How locally effective is this action likely to be based on the available scientific evidence?")),
                       em(strong("What is the overall certainty (quality) of the scientific evidence?")),
                       fixedRow(column(textAreaInput(paste0("action07",id),label="Describe action", placeholder="Please expand in more detail on the proposed action here. e.g., Install culverts or tunnels that can act as underpasses for amphibians.",width="100%",height="100%",rows=3),width=6),
                                column(textAreaInput(paste0("action08",id),label="Focus of action", placeholder="i.e., species, species group or habitat, e.g., Assist amphibians, particularly Natterjack toads, across the road through constructing culverts or tunnels under the road.",width="100%",height="100%",rows=3),width=6)),
                       p("Please describe below where you found evidence for this action and what assessment you have made of its likely effectiveness. You could use the Conservation Evidence assessment for each action if you have used this ",a("website", href="https://www.conservationevidence.com", target="_blank"), " to look for evidence. Or you might have access to reports or studies from your own organisation (i.e., the 'grey literature') that provide useful evidence. Ensure you consider the strength (quality) of the scientific evidence you look at in terms of the study or experimental design used and its local relevance (i.e., are the results likely to apply to your local setting?)."),
                       fixedRow(column(width=12,textAreaInput(paste0("action09",id),label="Evidence sources considered",placeholder="e.g., Conservation Evidence, systematic reviews, reports.",width="200%",height="200%",rows=4))),
                       fixedRow(column(textAreaInput(paste0("action10",id),label="Detailed assessment of evidence",placeholder="This action was assessed as likely to be beneficial on Conservation Evidence by a panel of experts. Several studies found...",
                                                     width="200%",height="200%",rows=4),width=12)),
                       fixedRow(column(width=12,textAreaInput(paste0("action11",id),label="Summarise assessment of evidence",placeholder="Overall, based on the sources above this action is likely to be beneficial, but we need to consider how to apply it effectively in our local setting.",
                                                              width="200%",height="200%",rows=10))),
                       tags$img(src="evidencehierarchy.png", width = 682, height = 305),
                       br(),
                       paste("An evidence hierarchy tool adapted from "), a("Mupepele et al. (2016)", href="https://doi.org/10.1890/15-0595", target="_blank"), paste("to help assess the strength of evidence from different sources of scientific evidence."),
                       br(),
                       br(),
                       br(),
                       h4("2.b.ii Local Knowledge and uncertainty"),
                       em(strong("How locally effective is this action likely to be based on local knowledge?")),
                       em(strong("What is the overall certainty (quality) of local knowledge?")),
                       p("e.g., Have you attempted this action yourself in the past? Are there any descriptive notes or reports from your organisation that can help? Do local stakeholders have any information or local knowledge you can integrate?"),
                       em(strong("Explicit knowledge")),
                       p("Explicit local knowledge is evidence that is documented, but typically not peer-reviewed or unpublished, and may often take the form of descriptive case studies or anecdotes from practitioners. Try to critically assess the confidence you have in this evidence (i.e., how much expertise does the person offering the local knowledge have?)"),
                       fixedRow(column( textAreaInput(paste0("action12",id),label=NULL,placeholder="The last ranger sent me a photo and some notes on a previous trial of a tunnel under the old road, that has since been resurfaced and rerouted, when it was first built didn't record any Natterjack toads using it over a two year period.",width="200%",height="200%",rows=2),width=12)),
                       em(strong("Tacit knowledge")),
                       p("Tacit or 'soft' local knowledge is evidence that is not documented and typically includes a knowledge holder's intuition, wisdom, and values. Try to think critically about the confidence and certainty you have in this evidence."),
                       fixedRow(column( textAreaInput(paste0("action13",id),label=NULL,placeholder="I have never seen them use any Natterjack toads using tunnels and culverts in other places on this reserve.",width="200%",height="200%",rows=2),width=12)),
                       br(),
                       br(),
                       h4("2.c.i Assess financial and resource-based costs"),
                       em(strong("How much does each action cost and what are its resource requirements?")),
                       p("Resource requirements and financial costs form the core of assessing the cost-effectiveness of each action. These can be broadly defined as the resources and finances required to implement a conservation action."),
                       p("It is good practice to ensure estimates of cost include the direct costs of implementation (including labour, time, consumables, overheads and equipment) and possibly changes in future finances predicted as a result of the action including opportunity costs (i.e., loss of income) and costs of future management and monitoring. Any cost benefits, for example solving a problem (e.g., removing an invasive species) and not having to pay recurrent costs, can also be considered. Cost information can be collated from literature, guidance and accounts but also from experience and knowledge. It is useful to ensure that costs for each action are considered on the same scale so that they are comparable - for example, the cost per unit area or per unit of effort."),
                       fixedRow(column( textAreaInput(paste0("action14",id),label="Financial costs and resource requirements",placeholder="This is likely to cost a significant amount in time, construction labour, and materials...",width="200%",height="200%",rows=3),width=12)),
                       br(),
                       br(),
                       h4("2.c.ii Assess the non-financial and non-target costs and benefits"),
                       em(strong("What are the wider non-financial costs and benefits of implementing this action? What are the costs and benefits for non-target species, habitats, and stakeholders?")),
                       p("Non-financial costs and benefits are the wider undesirable and desirable effects of the action on species, habitats, and stakeholders that are not the focus of the action. Costs may include socio-cultural considerations if the action did not target socio-cultural outcomes; for example, considering whether using pesticides, excluding access, or removing invasive species may have 'reputational costs' to the practitioner, stakeholders, or their organisations (i.e., has a negative impact on how they are perceived by the general public or other groups). "),
                       fixedRow(column( textAreaInput(paste0("action15",id),label="Non-financial, non-target costs and benefits",placeholder="Tunnels and culverts could cause the deaths of other species of amphibians and animals, but if successful could also save many other species from suffering road mortality...",width="200%",height="200%",rows=3),width=12)),
                       br(),
                       br(),
                       h4("2.d Assess acceptability"),
                       em(strong("Are the effects of implementing this action acceptable to you and to the key stakeholders you are considering?")),
                       p("Carefully consider whether it is acceptable to implement this action - do the outcomes of this action align to the values held by yourself and key stakeholders? Before you decide, it may be helpful to identify the major relevant values held by yourself and key stakeholders."),
                       fixedRow(column(textAreaInput(paste0("action16",id),label="Acceptability",placeholder="If the tunnels and culverts cause many deaths of amphibians then our reputation could suffer. This is likely to be unacceptably risky in this regard...",width="200%",height="200%",rows=2),width=12)),
                       br(),
                       br(),
                       h4("2.e Assess feasibility"),
                       em(strong("Can this action be successfully accomplished and properly implemented?")),
                       p("Assessing the feasibility of actions involves considering both the costs and acceptability of the action to key stakeholders. For example, resistance to the action from key stakeholders will be important if cooperation is a part key of its success and so if the action is likely to be unacceptable to key stakeholders, its feasibility is also likely to be low. Considering access or availability of equipment, resources, or staff to undertake a management action will also be important; for example, an action may not be feasible if the equipment needed cannot be moved to the location of interest. Feasibility may also involve considering the costs associated with each action, such as whether the action exceeds a strict budget or will be able to be approved by any stakeholders that must agree to its implementation."),
                       p("Carefully consider whether it is feasible to implement this action."),
                       fixedRow(column(textAreaInput(paste0("action17",id),label="Feasibility",placeholder="We would need to get permission to install these structures under the road, which could take time....",width="200%",height="200%",rows=2),width=12)),
                       br(),
                       br(),
                       h4("2.f Identify possible modifications"),
                       em(strong("How can the action be modified based on the previous evidence gathered?")),
                       p("By assessing the evidence from different sources on effectiveness, costs, acceptability, and feasibility of the action, modifications can be considered that might improve it. For example, there may be strong evidence from the scientific literature to suggest that creating certain habitats for great crested newts and white-faced darters will be beneficial, but a practitioner's explicit or tacit local knowledge also suggests that these species have slightly different habitat preferences in this region, and so a modification to this action may be necessary for it to be locally effective. Or an action such as an education campaign may not be acceptable to a key stakeholder if it is designed in a certain way, so modifications are necessary to ensure the action is acceptable. A structural action may also be too expensive to implement using certain materials and to be more cost-effective and ultimately more feasible, the action must be modified by using cheaper materials."),
                       fixedRow(column(textAreaInput(paste0("action18",id),label="Consider effectiveness of modificiations",placeholder="We could try certain designs of culverts and tunnels that limit mortality...",width="200%",height="200%",rows=2),width=12)),
                       br(),
                       br(),
                       h4("2.g Summarise evidence, information, and uncertainty for this action"),
                       em(strong("How likely is this action to be locally effective based on all the evidence and information you have gathered?")),
                       em(strong("What is the overall level of uncertainty associated with these conclusions?")),
                       p("Once the previous steps have been considered, it may be useful to summarise the likely local effectiveness of each action (whether modified or not), and the important costs, acceptability, and feasibility considerations that come with them. This draws together all the evidence previously gathered so that an evidence-based decision can be made in the next step, considering the relative advantages and disadvantages of each action alongside each other.
                         Uncertainty is also important to consider here, in particular to understand whether the evidence that has been gathered is sufficient in its reliability and relevance to make robust conclusions. It is also important to consider if there is conflicting evidence from different sources - for example, how much trust can be placed in the evidence drawn from the scientific literature versus evidence drawn from local knowledge?"),
                       fixedRow(column(textAreaInput(paste0("action19",id),label="Summarise all the evidence for action",placeholder="Overall, this action may be an effective action for us to use but could have negative impacts on amphibians that make this action unacceptably risky - not only in terms of damage to wildlife, but also to the organisation's reputation even if the design of the culverts and tunnels was modified. The installation of these structures will also take time and require extensive permissions, alongside costing a substantial amount, so this is also unlikely to be a feasible action to implement.",width="200%",height="200%",rows=2),width=12))
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
  })
  
  observeEvent(input$remove,{
    id <- input$newactionnamerem
    removeTab(inputId = "tab03_extra", target = glue("2.b-g ",id,"_val"), session=session)
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
                tabPanel(title = glue("2.b-g ",id), id=glue("2.b-g ",id), value=glue("2.b-g ",id,"_val"),
                         tags$style(
                           "li a {
                           font-size: 20px;
                           font-weight: bold;
                            }
                           "
                         ),
                         br(),
                         h4("2.b Assess effects on the focal target"),
                         em(strong("What does scientific evidence and local knowledge tell us about the desirable and undesirable effects of each action on the focal target?")),
                         p("Now you can assess the evidence on the likely effectiveness of this action in your local setting. Remember to do this for each action separately using the add or remove action buttons on the side."),
                         p("Move through the subtabs to consider what the effectiveness of this action is likely to be for your chosen setting based on evidence and information gathered from scientific evidence and local knowledge. Once you have done this you can consider the costs, acceptability, and feasibility of the action. Then it may be helpful to identify any specific modifications you may wish to make to tailor this conservation action to your local setting. Finally you can summarise what the evidence and information you have gathered suggests about the effectiveness, costs, acceptability, and feasibility of this action."),
                         br(),
                         br(),
                         h4("2.b.i Scientific Evidence and uncertainty"),
                         em(strong("How locally effective is this action likely to be based on the available scientific evidence?")),
                         em(strong("What is the overall certainty (quality) of the scientific evidence?")),
                         fixedRow(column(textAreaInput(paste0("action07",id),label="Describe action", placeholder="Please expand in more detail on the proposed action here. e.g., Install culverts or tunnels that can act as underpasses for amphibians.",width="100%",height="100%",rows=3),width=6),
                                  column(textAreaInput(paste0("action08",id),label="Focus of action", placeholder="i.e., species, species group or habitat, e.g., Assist amphibians, particularly Natterjack toads, across the road through constructing culverts or tunnels under the road.",width="100%",height="100%",rows=3),width=6)),
                         p("Please describe below where you found evidence for this action and what assessment you have made of its likely effectiveness. You could use the Conservation Evidence assessment for each action if you have used this ",a("website", href="https://www.conservationevidence.com", target="_blank"), " to look for evidence. Or you might have access to reports or studies from your own organisation (i.e., the 'grey literature') that provide useful evidence. Ensure you consider the strength (quality) of the scientific evidence you look at in terms of the study or experimental design used and its local relevance (i.e., are the results likely to apply to your local setting?)."),
                         fixedRow(column(width=3,textAreaInput(paste0("action09",id),label="Evidence sources considered",placeholder="e.g., Conservation Evidence, systematic reviews, reports.",width="200%",height="200%",rows=4))),
                         fixedRow(column(textAreaInput(paste0("action10",id),label="Detailed assessment of evidence",placeholder="This action was assessed as likely to be beneficial on Conservation Evidence by a panel of experts. Several studies found...",
                                                       width="200%",height="200%",rows=4),width=12)),
                         fixedRow(column(width=12,textAreaInput(paste0("action11",id),label="Summarise assessment of evidence",placeholder="Overall, based on the sources above this action is likely to be beneficial, but we need to consider how to apply it effectively in our local setting.",
                                                                width="200%",height="200%",rows=10))),
                         tags$img(src="evidencehierarchy.png", width = 682, height = 305),
                         br(),
                         paste("An evidence hierarchy tool adapted from "), a("Mupepele et al. (2016)", href="https://doi.org/10.1890/15-0595", target="_blank"), paste("to help assess the strength of evidence from different sources of scientific evidence."),
                         br(),
                         br(),
                         br(),
                         h4("2.b.ii Local Knowledge and uncertainty"),
                         em(strong("How locally effective is this action likely to be based on local knowledge?")),
                         em(strong("What is the overall certainty (quality) of local knowledge?")),
                         p("e.g., Have you attempted this action yourself in the past? Are there any descriptive notes or reports from your organisation that can help? Do local stakeholders have any information or local knowledge you can integrate?"),
                         em(strong("Explicit knowledge")),
                         p("Explicit local knowledge is evidence that is documented, but typically not peer-reviewed or unpublished, and may often take the form of descriptive case studies or anecdotes from practitioners. Try to critically assess the confidence you have in this evidence (i.e., how much expertise does the person offering the local knowledge have?)"),
                         fixedRow(column( textAreaInput(paste0("action12",id),label="",placeholder="The last ranger sent me a photo and some notes on a previous trial of a tunnel under the old road, that has since been resurfaced and rerouted, when it was first built didn't record any Natterjack toads using it over a two year period.",width="200%",height="200%",rows=2),width=12)),
                         em(strong("Tacit knowledge")),
                         p("Tacit or 'soft' local knowledge is evidence that is not documented and typically includes a knowledge holder's intuition, wisdom, and values. Try to think critically about the confidence and certainty you have in this evidence."),
                         fixedRow(column( textAreaInput(paste0("action13",id),label="",placeholder="I have never seen them use any Natterjack toads using tunnels and culverts in other places on this reserve.",width="200%",height="200%",rows=2),width=12)),
                         br(),
                         br(),
                         h4("2.c.i Assess financial and resource-based costs"),
                         em(strong("How much does each action cost and what are its resource requirements?")),
                         p("Resource requirements and financial costs form the core of assessing the cost-effectiveness of each action. These can be broadly defined as the resources and finances required to implement a conservation action."),
                         p("It is good practice to ensure estimates of cost include the direct costs of implementation (including labour, time, consumables, overheads and equipment) and possibly changes in future finances predicted as a result of the action including opportunity costs (i.e., loss of income) and costs of future management and monitoring. Any cost benefits, for example solving a problem (e.g., removing an invasive species) and not having to pay recurrent costs, can also be considered. Cost information can be collated from literature, guidance and accounts but also from experience and knowledge. It is useful to ensure that costs for each action are considered on the same scale so that they are comparable - for example, the cost per unit area or per unit of effort."),
                         fixedRow(column( textAreaInput(paste0("action14",id),label="Financial costs and resource requirements",placeholder="This is likely to cost a significant amount in time, construction labour, and materials...",width="200%",height="200%",rows=3),width=12)),
                         br(),
                         br(),
                         h4("2.c.ii Assess the non-financial and non-target costs and benefits"),
                         em(strong("What are the wider non-financial costs and benefits of implementing this action? What are the costs and benefits for non-target species, habitats, and stakeholders?")),
                         p("Non-financial costs and benefits are the wider undesirable and desirable effects of the action on species, habitats, and stakeholders that are not the focus of the action. Costs may include socio-cultural considerations if the action did not target socio-cultural outcomes; for example, considering whether using pesticides, excluding access, or removing invasive species may have 'reputational costs' to the practitioner, stakeholders, or their organisations (i.e., has a negative impact on how they are perceived by the general public or other groups). "),
                         fixedRow(column( textAreaInput(paste0("action15",id),label="Non-financial, non-target costs and benefits",placeholder="Tunnels and culverts could cause the deaths of other species of amphibians and animals, but if successful could also save many other species from suffering road mortality...",width="200%",height="200%",rows=3),width=12)),
                         br(),
                         br(),
                         h4("2.d Assess acceptability"),
                         em(strong("Are the effects of implementing this action acceptable to you and to the key stakeholders you are considering?")),
                         p("Carefully consider whether it is acceptable to implement this action - do the outcomes of this action align to the values held by yourself and key stakeholders? Before you decide, it may be helpful to identify the major relevant values held by yourself and key stakeholders."),
                         fixedRow(column(textAreaInput(paste0("action16",id),label="Acceptability",placeholder="If the tunnels and culverts cause many deaths of amphibians then our reputation could suffer. This is likely to be unacceptably risky in this regard...",width="200%",height="200%",rows=2),width=12)),
                         br(),
                         br(),
                         h4("2.e Assess feasibility"),
                         em(strong("Can this action be successfully accomplished and properly implemented?")),
                         p("Assessing the feasibility of actions involves considering both the costs and acceptability of the action to key stakeholders. For example, resistance to the action from key stakeholders will be important if cooperation is a part key of its success and so if the action is likely to be unacceptable to key stakeholders, its feasibility is also likely to be low. Considering access or availability of equipment, resources, or staff to undertake a management action will also be important; for example, an action may not be feasible if the equipment needed cannot be moved to the location of interest. Feasibility may also involve considering the costs associated with each action, such as whether the action exceeds a strict budget or will be able to be approved by any stakeholders that must agree to its implementation."),
                         p("Carefully consider whether it is feasible to implement this action."),
                         fixedRow(column(textAreaInput(paste0("action17",id),label="Feasibility",placeholder="We would need to get permission to install these structures under the road, which could take time....",width="200%",height="200%",rows=2),width=12)),
                         br(),
                         br(),
                         h4("2.f Identify possible modifications"),
                         em(strong("How can the action be modified based on the previous evidence gathered?")),
                         p("By assessing the evidence from different sources on effectiveness, costs, acceptability, and feasibility of the action, modifications can be considered that might improve it. For example, there may be strong evidence from the scientific literature to suggest that creating certain habitats for great crested newts and white-faced darters will be beneficial, but a practitioner's explicit or tacit local knowledge also suggests that these species have slightly different habitat preferences in this region, and so a modification to this action may be necessary for it to be locally effective. Or an action such as an education campaign may not be acceptable to a key stakeholder if it is designed in a certain way, so modifications are necessary to ensure the action is acceptable. A structural action may also be too expensive to implement using certain materials and to be more cost-effective and ultimately more feasible, the action must be modified by using cheaper materials."),
                         fixedRow(column(textAreaInput(paste0("action18",id),label="Consider effectiveness of modificiations",placeholder="We could try certain designs of culverts and tunnels that limit mortality...",width="200%",height="200%",rows=2),width=12)),
                         br(),
                         br(),
                         h4("2.g Summarise evidence, information, and uncertainty for this action"),
                         em(strong("How likely is this action to be locally effective based on all the evidence and information you have gathered?")),
                         em(strong("What is the overall level of uncertainty associated with these conclusions?")),
                         p("Once the previous steps have been considered, it may be useful to summarise the likely local effectiveness of each action (whether modified or not), and the important costs, acceptability, and feasibility considerations that come with them. This draws together all the evidence previously gathered so that an evidence-based decision can be made in the next step, considering the relative advantages and disadvantages of each action alongside each other.
                           Uncertainty is also important to consider here, in particular to understand whether the evidence that has been gathered is sufficient in its reliability and relevance to make robust conclusions. It is also important to consider if there is conflicting evidence from different sources - for example, how much trust can be placed in the evidence drawn from the scientific literature versus evidence drawn from local knowledge?"),
                         fixedRow(column(textAreaInput(paste0("action19",id),label="Summarise all the evidence for action",placeholder="Overall, this action may be an effective action for us to use but could have negative impacts on amphibians that make this action unacceptably risky - not only in terms of damage to wildlife, but also to the organisation's reputation even if the design of the culverts and tunnels was modified. The installation of these structures will also take time and require extensive permissions, alongside costing a substantial amount, so this is also unlikely to be a feasible action to implement.",width="200%",height="200%",rows=2),width=12))
                         ),select=TRUE, session=session)
  }
  })
  
  
  #  output$summtab <- renderDT({
  #    idlist <- sort(names(input))
  #    actnames<-lapply(1:((length(idlist)-15)/16), ###########need to change these numbers if update rest of code and numbers of inputs!
  #                     function(i){c(gsub("summarytabeff","",idlist[grep("summarytabeff",idlist)][i]),input[[idlist[grep("summarytabeff",idlist)][i]]],input[[idlist[grep("summarytabcost",idlist)][i]]],input[[idlist[grep("summarytabacc",idlist)][i]]])})
  #    table1 <- data.frame(do.call(rbind,actnames))
  #    colnames(table1)<- c("Actions","Local Effectivenss", "Costs", "Acceptability")
  #    datatable(table1,editable="cell")
  #    })
  
  setBookmarkExclude(c("add","remove","newactionname","newactionnamerem"))
  
  
  #output$numids <- renderText({sort(updateidlist$data)})
  
  
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste(list('Evidence-to-decision_summary',Sys.Date()), sep = '.', switch(
          input$format, PDF = 'pdf',  Word = 'docx'
        ))
      },
      
      content = function(file) {
        library(rmarkdown)
          out <- render('/srv/shiny-server/evidence2decisiontool/report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
  
  
  output$downloadReporteg <- downloadHandler(
      filename = function() {
        paste(list('Example_evidence-to-decision_summary',Sys.Date()), sep = '.', switch(
          input$formateg, PDF = 'pdf',  Word = 'docx'
        ))
      },
      
      content = function(file) {
        library(rmarkdown)
        out <- render('/srv/shiny-server/evidence2decisiontool/reporteg.Rmd', switch(
          input$formateg,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
  

  textsummary <- function(){
    idlist <- updateidlist$data
    if(length(idlist)>0){
      lapply(1:length(idlist), ###########need to change these numbers if update rest of code and numbers of inputs!
             function(i){
               cat("# ",idlist[i],"  \n  \n")
               cat("## Describe action  \n",input[[glue("action07",idlist[i])]],"  \n  \n")
               cat("## Focus of action  \n",input[[glue("action08",idlist[i])]],"  \n  \n")
               cat("## Scientific evidence and uncertainty  \n")
               cat("### Evidence sources considered  \n",input[[glue("action09",idlist[i])]],"  \n  \n")
               cat("### Detailed assessment of evidence  \n",input[[glue("action10",idlist[i])]],"  \n  \n")
               cat("### Summarise assessment of evidence  \n",input[[glue("action11",idlist[i])]],"  \n  \n")
               cat("## Local knowledge and uncertainty  \n")
               cat("### Explicit knowledge  \n",input[[glue("action12",idlist[i])]],"  \n  \n")
               cat("### Tacit knowledge  \n",input[[glue("action13",idlist[i])]],"  \n  \n")
               cat("## Costs  \n")
               cat("### Financial costs and resource requirements  \n",input[[glue("action14",idlist[i])]],"  \n  \n")
               cat("### Non-financial and non-target costs and benefits  \n",input[[glue("action15",idlist[i])]],"  \n  \n")
               cat("## Acceptability  \n",input[[glue("action16",idlist[i])]],"  \n  \n")
               cat("## Feasibility  \n",input[[glue("action17",idlist[i])]],"  \n  \n")
               cat("## Consider effectiveness of modificiations  \n",input[[glue("action18",idlist[i])]],"  \n  \n")
               cat("## Summarise all the evidence for action  \n",input[[glue("action19",idlist[i])]],"  \n  \n")
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
