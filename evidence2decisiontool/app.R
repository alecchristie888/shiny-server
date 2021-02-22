#devtools::install_github("rstudio/shiny")
library(shiny)
library(glue)
library(tinytex)
library(data.table)
library(rsconnect)
library(DT)
idlistrem <- list()

ui <- function(request){fluidPage(
  titlePanel(title=tags$a(href='http://conservationevidence.com', target="_blank",
                          tags$img(src="image1.jpg", width = 240, height = 80)),
             windowTitle = "Evidence-to-Decision tool"),
  titlePanel(title = "Evidence-to-Decision tool"),
  sidebarLayout(
    sidebarPanel(
      strong(paste("Warning: Please ensure you click bookmark to save your work. You will get a url link to restore the tool (make sure you keep this safe). Otherwise the app will timeout after 60 minutes. Be aware that the url will become very long as you enter text in the tool - we are working on setting up a server so this won't be needed in future.")),
      br(),
      br(),
      bookmarkButton(),
      br(),
      br(),
      br(),
      strong(p("Add and remove actions")),
      p("Once you get to tab '2.a Identify possible actions', to continue the process you need to add some actions using the panel below to assess the evidence for each action. This will bring up several subtabs under '2.b-g. <YOUR ACTION NAME HERE>)'."),
      textInput("newactionname","Name of action to add", "Install culverts or tunnels as road crossings"),
      actionButton("add", "Add this action"),
      br(),
      br(),
      textInput("newactionnamerem","Name of action to remove", "Install barrier fencing along roads"),
      actionButton("remove", "Remove this action"),
      br(),
      br(),
      p("Small warning: Once you have entered and action name and pressed 'add this action', you won't be able to edit the name. You will need to 'remove this action' and add a new action. Removing an action will also remove the text you enter in the tab and subtabs that appear (2.b-g).")
    ),
    mainPanel(
      navbarPage("",id="maintabs",
                 tabPanel("Introduction",id="tab01",value='tab01_val',
                          h3("What's this tool for?"),
#                         textOutput("numids"),
                          paste("The Evidence-to-Decision tool is designed to help you combine the available scientific evidence (e.g., from"),
                          a("Conservation Evidence,", href="https://www.conservationevidence.com", target="_blank"), 
                          paste("systematic reviews, scientific reports, local reports and grey literature), along with your local knowledge, whilst also considering other important information (e.g., costs, feasibility, and acceptability) to make practical evidence-based decisions tailored to your local context."),br(), br(),
                          p("The general structure of the tool is to 1.) Define the decision context (what is the problem you want to solve?), 2.) Gather evidence and information (what actions are likely to be the most effective to address my problem in my local context?), 3.) Make an evidence-based decision (what actions are you going to take to address your problem based on the evidence and information you have assessed?). The diagram below lays out the detailed steps that this tool will guide you through."),
                          p("This tool is best suited for use by individual landowners, reserve managers, and small NGOs working on specific projects to come to an evidence-based decision for a specific problem. The tool was designed to streamline an evidence-based decision-making process where there is little time or resources available. Where greater time and resources can be allocated to making a complex decision, potentially on a set of strategies, there are many different types of decision analysis, theory of change, and cost-benefit analyses that can be used."), 
                          p("This tools works by taking inputs from you, the user, in different tabs from 1 to 3 (tabs 2.b-g will appear once you identify actions to assess). The tool will prompt you to summarise the evidence available to you, assess various factors such as costs, feasibility, and acceptability, and then draw that information together to come to a decision. Your inputs will be stored and used to generate a summary report that you can download in the final tab (you can download an example report in the final tab to see what the output of this tool looks like). The rationale behind this tool is to help you make more transparent and documented decisions using evidence and the prompts you are given are just suggestions on how you may summarise your thinking. Tip: You can drag and enlarge text boxes to write in by dragging the bottom right hand corner of them."),
                          tags$img(src="image3.png", width = 750, height = 780)
                 ),
                 tabPanel("1. Define the decision context",id="tab02",value='tab02_val',
                          br(),h3("Define the decision or problem to be considered and the context behind it"),
                          br(),
                          p("It is important to explicitly define and describe the context surrounding the decision or problem that is being considered. This includes the problem or direct threat being tackled, the focal target and ultimate goal of any action(s) (i.e., desired outcomes on certain species or groups of people) to be taken and important socio-ecological and physical contextual information (e.g., habitat types, species present, climate, location, background on relevant stakeholders), as well as constraints which may influence the decision (e.g., regulatory structures/legislation, budget available, personal/organisational values)."),
                          fixedRow(column( textAreaInput("action01",label="What is the problem?",
                                                         placeholder="Large numbers of amphibians are being killed when crossing a road that runs through part of the reserve.",width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column(textAreaInput("action02",label="Location", placeholder="State the location for the proposed action.",width="100%",height="100%",rows=1),width=12)),
                          fixedRow(column( textAreaInput("action03",label="What is the ultimate goal?",
                                                         placeholder="Reduce amphibian mortality but still enable the movement of frogs across the road.",width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column( textAreaInput("action04",label="What is the focal target?",
                                                         placeholder="Prevent the decline of a Natterjack toad (Epidalea calamita) population.",width="200%",height="200%",rows=3),width=12)),
                          fixedRow(column( textAreaInput("action05",label="What is the relevant ecological, physical, and social context underlying the decision?",
                                                         placeholder="The road was built in 2017 and since then our monitoring and anectodotal evidence has suggested that a substantial proportion of amphibians are being killed on a key section of the road. We believe this may be a major factor behind the continued decline of a population of Natterjack toads (Epidalea calamita) despite our best efforts to provide suitable habitat. This toad is a key species we need to protect due to their wider national decline. We have a keen group of volunteers who can help with the work. The road is owned and maintained by the local council.",width="200%",height="200%",rows=3),width=12))
                          ),
                 tabPanel("2. Gather evidence - an overview",id="tab03",value='tab03_val',
                          br(),
                          h3("What evidence is available and what does it suggest about the action(s) that should be taken to address the problem?"),
                          br(),
                          p("Information can now be gathered to assess which action(s) to take to achieve the focal targets and goals defined in the previous tab (1. Define the decision context). This process starts with identfying possible actions to address your problem (e.g., different management actions) in the next tab (2.a), and then considering various types of evidence and information on the effectiveness, costs, acceptability, and feasibility of each action (one at a time - use the function on the side to add or remove different actions to reveal this tab). Once evidence and information has been gathered and assessed, you can move to tab '3. Make an evidence-based decision' to draw together what you have found to summarise and justify your decision and next steps. All the information that you enter here will be used to create a downloadable summary report (you can view an example in the final tab).")),
                 tabPanel("2.a Identify possible actions",
                          br(),
                          h3("Which action(s) could be taken to address the problem, regardless of their cost, acceptability, or feasibility?"),
                          br(),
                          p("Brainstorm and note as many possible actions as you can to address the decision or problem you are considering."),
                          br(),
                          p("Don't forget to look into the literature and search websites like",a("Conservation Evidence", href="https://www.conservationevidence.com", target="_blank"),"to inform your list of actions. This is called solution scanning. Make sure to include actions regardless of their acceptability, feasibility, or costs at this stage. These actions can be removed later if they are too expensive, unfeasible, or unacceptable."),
                          br(),
                          p("Once you have decided on your list of possible actions below, type them into the side bar on the right one at a time and click 'add action'. Each time you click 'add action' you will bring up more tabs to continue the process and assess the evidence for each action (this will show as '2.b-g. <YOUR ACTION NAME HERE>)'."),
                          fixedRow(column( textAreaInput("action06",label="Possible actions",
                                                         placeholder="Install culverts or tunnels as road crossings.\nInstall barrier fencing along roads.\nUse humans to assist migrating amphibians across roads.\n...\n...\n...",width="200%",height="200%",rows=3),width=12))
                          
                          ),
                 tabPanel(title="3.a-b. Make an evidence-based decision",id="tab13",value='tab13_val',
#                         br(),
#                         DTOutput("summtab"),
                          br(),
                          p("Using the accumulated evidence, the relative advantages and disadvantages of each modified action can be compared and related back to the original decision or problem being considered (in Step 1. Define the decision context). This involves weighing up how locally effective, cost-effective, acceptable, and feasible each action and whether its implementation is justified."),
                          p("In a rapid decision-making process, you could narrow down which actions to implement in several ways. For example, there may often be strict limits for the amount of money available, and so actions that are likely to substantially exceed these limits can be discounted. Actions that are expensive and less effective than comparable alternatives (i.e., with very low relative cost-effectiveness) may also not be considered any further. Actions with the same or lower relative costs but greater effectiveness, or with the same relative effectiveness and lower costs are likely to be the ones considered further as they can be justified on the grounds of cost-effectiveness."),
                          p("Justifiable actions to implement may be narrowed down further by rejecting actions that are clearly unacceptable to yourself or key stakeholders. Similarly, actions that are clearly not feasible to implement may also be rejected. Considering previously gathered evidence on cost-effectiveness, acceptability, and feasibility will be important to making these judgments. If you have more time and resources, it might be useful to undertake a more detailed assessment of costs and cost-effectiveness (e.g., multi-criteria decision analysis). But here, as we are aiming to make evidence-based decisions more rapid, you could quickly place the actions in a prioritised order from most suitable to least suitable based on local cost-effectiveness, acceptability, and feasibility."),
                          br(),
                          h3("Reflecting on the problem you face and the evidence and information you have gathered, what is your decision and why?"),
                          br(),
                          h4(strong("Which action(s), if any, are the best ones to implement to achieve the focal targets and goals you defined at the beginning?")),
                          fixedRow(column( textAreaInput("action20",label="Name and justify your choices.",placeholder="Install barrier fencing along roads: This action has been shown to be effective from the evidence I have considered if it is implemented properly. The costs will be less than installing culverts or tunnels, and it should take less time to get permissions to install the fencing along the road. If we target the fencing at strategic positions, and make it high enough so Natterjack toads not climb over it, we can funnel them to natural watercourses underneath the road.",width="200%",height="200%",rows=3),width=12)),
                          br(),
                          h4(strong("Which action(s), if any, are not going to be implemented?")),
                          fixedRow(column( textAreaInput("action21",label="Name and justify your choices.",placeholder="Use humans to assist migrating amphibians across roads: This is because based on my assessment of the evidence this action is unlikely to be effective on the reserve, as it has shown to not prevent population declines elsewhere and might divert volunteers away from activities that are more important. \n \nInstall culverts or tunnels as road crossings: This is because although this could be an effective action based on the scientific evidence, it can lead to mortality of amphibians within the culverts and tunnels. It would also cost a significant amount of money which would likely be beyond our budget, and would also take a lot of time to get permission to construct and build.",width="200%",height="200%",rows=3),width=12)),
                          br(),
                          h4(strong("What is your overall decision, what are the next steps, and why?")),
                          p("Summarise your overall decision and the next steps you will take. This could be implementing these actions, pausing to make a more detailed assessment, gather more evidence, or decide to do nothing."),
                          fixedRow(column( textAreaInput("action22",label="Justify your decision and next steps.",placeholder="We will now investigate the correct height, material, and length of fencing needed and identify key stragetic points along the road to place the fencing. We will also request permission to install the fencing and trial it during the next migration season. We will write up the findings of this trial in a report, comparing it to previous years mortality, and publish this online through the Conservation Evidence journal.",width="200%",height="200%",rows=3),width=12)),
                          br()),
                 tabPanel(title="3.c. Document and report decision (download report)",id="tab14",value='tab14_val',
                          br(),
                          h3("Example summary report"),
                          p("To download an example of a summary report without having to fill any information select a format and then click below."),
                          radioButtons('formateg', 'Example report format', c('PDF', 'Word'),
                                       inline = TRUE),
                          downloadButton("downloadReporteg", "Download example summary report", style='padding:4px; font-size:125%'),
                          br(),
                          br(),
                          br(),
                          br(),
                          h3("Your customised summary report"),
                          p("This summary report has been created using the text you've entered into this tool. To download this summary report select a document format and then click below."),
                          radioButtons('format', 'Report format', c('PDF', 'Word'),
                                       inline = TRUE),
                          downloadButton("downloadReport", "Download summary report", style='padding:4px; font-size:125%')
                 )
      )
    ),position="right"
  )
)}


server <- function(input, output, session) {
    observeEvent(input$add, {
    id <- input$newactionname
    insertTab(inputId = "maintabs",
              tabPanel(title = glue("2.b-g ",id),
                       tabsetPanel(
                         id = paste0("tab",id),
                         tabPanel(title="2.b Assess effects on the focal target",id=paste0("tab04",id),value=paste0("tab04_val",id),
                                  br(),
                                  h4(strong("What does scientific evidence and local knowledge tell us about the desirable and undesirable effects of each action on the focal target?")),
                                  br(),
                                  p("Now you can assess the evidence on the likely effectiveness of this action in your local setting. Remember to do this for each action separately using the add or remove action buttons on the side."),
                                  p("Move through the subtabs to consider what the effectiveness of this action is likely to be for your chosen setting based on evidence and information gathered from scientific evidence and local knowledge. Once you have done this you can consider the costs, acceptability, and feasibility of the action. Then it may be helpful to identify any specific modifications you may wish to make to tailor this conservation action to your local setting. Finally you can summarise what the evidence and information you have gathered suggests about the effectiveness, costs, acceptability, and feasibility of this action.")
                                  ),
                         tabPanel(title="2.b.i Scientific Evidence and uncertainty",id=paste0("tab05",id),value=paste0("tab05_val",id),
                                  br(),
                                  h4(strong("How locally effective is this action likely to be based on the available scientific evidence?")),
                                  h4(strong("What is the overall certainty (quality) of the scientific evidence?")),
                                  br(),
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
                                  br()),
                         tabPanel(title="2.b.ii Local Knowledge and uncertainty",id=paste0("tab06",id),value=paste0("tab06_val",id),
                                  br(),
                                  h4(strong("How locally effective is this action likely to be based on local knowledge?")),
                                  h4(strong("What is the overall certainty (quality) of local knowledge?")),
                                  p("e.g., Have you attempted this action yourself in the past? Are there any descriptive notes or reports from your organisation that can help? Do local stakeholders have any information or local knowledge you can integrate?"),
                                  br(),
                                  h4("Explicit knowledge"),p("Explicit local knowledge is evidence that is documented, but typically not peer-reviewed or unpublished, and may often take the form of descriptive case studies or anecdotes from practitioners. Try to critically assess the confidence you have in this evidence (i.e., how much expertise does the person offering the local knowledge have?)"),
                                  fixedRow(column( textAreaInput(paste0("action12",id),label="",placeholder="The last ranger sent me a photo and some notes on a previous trial of a tunnel under the old road, that has since been resurfaced and rerouted, when it was first built didn't record any Natterjack toads using it over a two year period.",width="200%",height="200%",rows=2),width=12)),
                                  br(),
                                  h4("Tacit knowledge"),p("Tacit or 'soft' local knowledge is evidence that is not documented and typically includes a knowledge holder's intuition, wisdom, and values. Try to think critically about the confidence and certainty you have in this evidence."),
                                  fixedRow(column( textAreaInput(paste0("action13",id),label="",placeholder="I have never seen them use any Natterjack toads using tunnels and culverts in other places on this reserve.",width="200%",height="200%",rows=2),width=12)),
                                  br()),
                         tabPanel(title="2.c.i Assess financial and resource-based costs",id=paste0("tab07",id),value=paste0("tab07_val",id),
                                  br(),
                                  h5(strong("How much does each action cost and what are its resource requirements?")),
                                  br(),
                                  p("Resource requirements and financial costs form the core of assessing the cost-effectiveness of each action. These can be broadly defined as the resources and finances required to implement a conservation action."),
                                  br(),
                                  p("It is good practice to ensure estimates of cost include the direct costs of implementation (including labour, time, consumables, overheads and equipment) and possibly changes in future finances predicted as a result of the action including opportunity costs (i.e., loss of income) and costs of future management and monitoring. Any cost benefits, for example solving a problem (e.g., removing an invasive species) and not having to pay recurrent costs, can also be considered. Cost information can be collated from literature, guidance and accounts but also from experience and knowledge. It is useful to ensure that costs for each action are considered on the same scale so that they are comparable - for example, the cost per unit area or per unit of effort."),
                                  br(),
                                  fixedRow(column( textAreaInput(paste0("action14",id),label="Financial costs and resource requirements",placeholder="This is likely to cost a significant amount in time, construction labour, and materials...",width="200%",height="200%",rows=3),width=12))
                                  ),
                         tabPanel(title="2.c.ii Assess the non-financial and non-target costs and benefits",id=paste0("tab08",id),value=paste0("tab08_val",id),
                                  br(),
                                  h5(strong("What are the wider non-financial costs and benefits of implementing this action? What are the costs and benefits for non-target species, habitats, and stakeholders?")),
                                  br(),
                                  p("Non-financial costs and benefits are the wider undesirable and desirable effects of the action on species, habitats, and stakeholders that are not the focus of the action. Costs may include socio-cultural considerations if the action did not target socio-cultural outcomes; for example, considering whether using pesticides, excluding access, or removing invasive species may have 'reputational costs' to the practitioner, stakeholders, or their organisations (i.e., has a negative impact on how they are perceived by the general public or other groups). "),
                                  br(),
                                  fixedRow(column( textAreaInput(paste0("action15",id),label="Non-financial, non-target costs and benefits",placeholder="Tunnels and culverts could cause the deaths of other species of amphibians and animals, but if successful could also save many other species from suffering road mortality...",width="200%",height="200%",rows=3),width=12))
                                  ),
                         tabPanel(title="2.d Assess acceptability",id=paste0("tab09",id),value=paste0("tab09_val",id),
                                  br(),
                                  h5(strong("Are the effects of implementing this action acceptable to you and to the key stakeholders you are considering?")),
                                  br(),
                                  p("Carefully consider whether it is acceptable to implement this action - do the outcomes of this action align to the values held by yourself and key stakeholders? Before you decide, it may be helpful to identify the major relevant values held by yourself and key stakeholders."),
                                  br(),
                                  fixedRow(column(textAreaInput(paste0("action16",id),label="Acceptability",placeholder="If the tunnels and culverts cause many deaths of amphibians then our reputation could suffer. This is likely to be unacceptably risky in this regard...",width="200%",height="200%",rows=2),width=12))
                                  ),
                         tabPanel(title="2.e Assess feasibility",id=paste0("tab10",id),value=paste0("tab10_val",id),
                                  br(),
                                  h5(strong("Can this action be successfully accomplished and properly implemented?")),
                                  br(),
                                  p("Assessing the feasibility of actions involves considering both the costs and acceptability of the action to key stakeholders. For example, resistance to the action from key stakeholders will be important if cooperation is a part key of its success and so if the action is likely to be unacceptable to key stakeholders, its feasibility is also likely to be low. Considering access or availability of equipment, resources, or staff to undertake a management action will also be important; for example, an action may not be feasible if the equipment needed cannot be moved to the location of interest. Feasibility may also involve considering the costs associated with each action, such as whether the action exceeds a strict budget or will be able to be approved by any stakeholders that must agree to its implementation."),
                                  br(),
                                  p("Carefully consider whether it is feasible to implement this action."),
                                  fixedRow(column(textAreaInput(paste0("action17",id),label="Feasibility",placeholder="We would need to get permission to install these structures under the road, which could take time....",width="200%",height="200%",rows=2),width=12))
                                  ),
                         tabPanel(title="2.f Identify possible modifications",id=paste0("tab11",id),value=paste0("tab11_val",id),
                                  br(),
                                  h5(strong("How can the action be modified based on the previous evidence gathered?")),
                                  br(),
                                  p("By assessing the evidence from different sources on effectiveness, costs, acceptability, and feasibility of the action, modifications can be considered that might improve it. For example, there may be strong evidence from the scientific literature to suggest that creating certain habitats for great crested newts and white-faced darters will be beneficial, but a practitioner's explicit or tacit local knowledge also suggests that these species have slightly different habitat preferences in this region, and so a modification to this action may be necessary for it to be locally effective. Or an action such as an education campaign may not be acceptable to a key stakeholder if it is designed in a certain way, so modifications are necessary to ensure the action is acceptable. A structural action may also be too expensive to implement using certain materials and to be more cost-effective and ultimately more feasible, the action must be modified by using cheaper materials."),
                                  br(),
                                  fixedRow(column(textAreaInput(paste0("action18",id),label="Consider effectiveness of modificiations",placeholder="We could try certain designs of culverts and tunnels that limit mortality...",width="200%",height="200%",rows=2),width=12)),
                                  br()),
                         
                         tabPanel(title="2.g Summarise evidence, information, and uncertainty for this action",id=paste0("tab12",id),value=paste0("tab12_val",id),
                                  br(),
                                  h5(strong("How likely is this action to be locally effective based on all the evidence and information you have gathered?")),
                                  h5(strong("What is the overall level of uncertainty associated with these conclusions?")),
                                  br(),
                                  p("Once the previous steps have been considered, it may be useful to summarise the likely local effectiveness of each action (whether modified or not), and the important costs, acceptability, and feasibility considerations that come with them. This draws together all the evidence previously gathered so that an evidence-based decision can be made in the next step, considering the relative advantages and disadvantages of each action alongside each other.
Uncertainty is also important to consider here, in particular to understand whether the evidence that has been gathered is sufficient in its reliability and relevance to make robust conclusions. It is also important to consider if there is conflicting evidence from different sources - for example, how much trust can be placed in the evidence drawn from the scientific literature versus evidence drawn from local knowledge?"),
                                  br(),
                                  fixedRow(column(textAreaInput(paste0("action19",id),label="Summarise all the evidence for action",placeholder="Overall, this action may be an effective action for us to use but could have negative impacts on amphibians that make this action unacceptably risky - not only in terms of damage to wildlife, but also to the organisation's reputation even if the design of the culverts and tunnels was modified. The installation of these structures will also take time and require extensive permissions, alongside costing a substantial amount, so this is also unlikely to be a feasible action to implement.",width="200%",height="200%",rows=2),width=12))
                         )
                         
                         )
                       
                       
              ),target="tab13_val",position=c("before"),select=TRUE)
    
  })
  
  onBookmark(function(state){
    state$values$actionnames <- c(do.call(rbind,strsplit(names(input)[grep("action09",names(input))],"9"))[,2])
    state$values$lastactionadded <- state$values$actionnames[length(state$values$actionnames)]
  })
  
  
  onRestore(function(state){
      idstorestore <- state$values$actionnames[-grep(state$values$lastactionadded,state$values$actionnames)]
      for(i in 1:length(idstorestore)){
        id <- idstorestore[i]
        insertTab(inputId = "maintabs",
                tabPanel(title = glue("2.b-g ",id),
                         tabsetPanel(
                           id = paste0("tab",id),
                           tabPanel(title="2.b Assess effects on the focal target",id=paste0("tab04",id),value=paste0("tab04_val",id),
                                    br(),
                                    h4(strong("What does scientific evidence and local knowledge tell us about the desirable and undesirable effects of each action on the focal target?")),
                                    br(),
                                    p("Now you can assess the evidence on the likely effectiveness of this action in your local setting. Remember to do this for each action separately using the add or remove action buttons on the side."),
                                    p("Move through the subtabs to consider what the effectiveness of this action is likely to be for your chosen setting based on evidence and information gathered from scientific evidence and local knowledge. Once you have done this you can consider the costs, acceptability, and feasibility of the action. Then it may be helpful to identify any specific modifications you may wish to make to tailor this conservation action to your local setting. Finally you can summarise what the evidence and information you have gathered suggests about the effectiveness, costs, acceptability, and feasibility of this action.")
                           ),
                           tabPanel(title="2.b.i Scientific Evidence and uncertainty",id=paste0("tab05",id),value=paste0("tab05_val",id),
                                    br(),
                                    h4(strong("How locally effective is this action likely to be based on the available scientific evidence?")),
                                    h4(strong("What is the overall certainty (quality) of the scientific evidence?")),
                                    br(),
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
                                    br()),
                           tabPanel(title="2.b.ii Local Knowledge and uncertainty",id=paste0("tab06",id),value=paste0("tab06_val",id),
                                    br(),
                                    h4(strong("How locally effective is this action likely to be based on local knowledge?")),
                                    h4(strong("What is the overall certainty (quality) of local knowledge?")),
                                    p("e.g., Have you attempted this action yourself in the past? Are there any descriptive notes or reports from your organisation that can help? Do local stakeholders have any information or local knowledge you can integrate?"),
                                    br(),
                                    h4("Explicit knowledge"),p("Explicit local knowledge is evidence that is documented, but typically not peer-reviewed or unpublished, and may often take the form of descriptive case studies or anecdotes from practitioners. Try to critically assess the confidence you have in this evidence (i.e., how much expertise does the person offering the local knowledge have?)"),
                                    fixedRow(column( textAreaInput(paste0("action12",id),label="",placeholder="The last ranger sent me a photo and some notes on a previous trial of a tunnel under the old road, that has since been resurfaced and rerouted, when it was first built didn't record any Natterjack toads using it over a two year period.",width="200%",height="200%",rows=2),width=12)),
                                    br(),
                                    h4("Tacit knowledge"),p("Tacit or 'soft' local knowledge is evidence that is not documented and typically includes a knowledge holder's intuition, wisdom, and values. Try to think critically about the confidence and certainty you have in this evidence."),
                                    fixedRow(column( textAreaInput(paste0("action13",id),label="",placeholder="I have never seen them use any Natterjack toads using tunnels and culverts in other places on this reserve.",width="200%",height="200%",rows=2),width=12)),
                                    br()),
                           tabPanel(title="2.c.i Assess financial and resource-based costs",id=paste0("tab07",id),value=paste0("tab07_val",id),
                                    br(),
                                    h5(strong("How much does each action cost and what are its resource requirements?")),
                                    br(),
                                    p("Resource requirements and financial costs form the core of assessing the cost-effectiveness of each action. These can be broadly defined as the resources and finances required to implement a conservation action."),
                                    br(),
                                    p("It is good practice to ensure estimates of cost include the direct costs of implementation (including labour, time, consumables, overheads and equipment) and possibly changes in future finances predicted as a result of the action including opportunity costs (i.e., loss of income) and costs of future management and monitoring. Any cost benefits, for example solving a problem (e.g., removing an invasive species) and not having to pay recurrent costs, can also be considered. Cost information can be collated from literature, guidance and accounts but also from experience and knowledge. It is useful to ensure that costs for each action are considered on the same scale so that they are comparable - for example, the cost per unit area or per unit of effort."),
                                    br(),
                                    fixedRow(column( textAreaInput(paste0("action14",id),label="Financial costs and resource requirements",placeholder="This is likely to cost a significant amount in time, construction labour, and materials...",width="200%",height="200%",rows=3),width=12))
                           ),
                           tabPanel(title="2.c.ii Assess the non-financial and non-target costs and benefits",id=paste0("tab08",id),value=paste0("tab08_val",id),
                                    br(),
                                    h5(strong("What are the wider non-financial costs and benefits of implementing this action? What are the costs and benefits for non-target species, habitats, and stakeholders?")),
                                    br(),
                                    p("Non-financial costs and benefits are the wider undesirable and desirable effects of the action on species, habitats, and stakeholders that are not the focus of the action. Costs may include socio-cultural considerations if the action did not target socio-cultural outcomes; for example, considering whether using pesticides, excluding access, or removing invasive species may have 'reputational costs' to the practitioner, stakeholders, or their organisations (i.e., has a negative impact on how they are perceived by the general public or other groups). "),
                                    br(),
                                    fixedRow(column( textAreaInput(paste0("action15",id),label="Non-financial, non-target costs and benefits",placeholder="Tunnels and culverts could cause the deaths of other species of amphibians and animals, but if successful could also save many other species from suffering road mortality...",width="200%",height="200%",rows=3),width=12))
                           ),
                           tabPanel(title="2.d Assess acceptability",id=paste0("tab09",id),value=paste0("tab09_val",id),
                                    br(),
                                    h5(strong("Are the effects of implementing this action acceptable to you and to the key stakeholders you are considering?")),
                                    br(),
                                    p("Carefully consider whether it is acceptable to implement this action - do the outcomes of this action align to the values held by yourself and key stakeholders? Before you decide, it may be helpful to identify the major relevant values held by yourself and key stakeholders."),
                                    br(),
                                    fixedRow(column(textAreaInput(paste0("action16",id),label="Acceptability",placeholder="If the tunnels and culverts cause many deaths of amphibians then our reputation could suffer. This is likely to be unacceptably risky in this regard...",width="200%",height="200%",rows=2),width=12))
                           ),
                           tabPanel(title="2.e Assess feasibility",id=paste0("tab10",id),value=paste0("tab10_val",id),
                                    br(),
                                    h5(strong("Can this action be successfully accomplished and properly implemented?")),
                                    br(),
                                    p("Assessing the feasibility of actions involves considering both the costs and acceptability of the action to key stakeholders. For example, resistance to the action from key stakeholders will be important if cooperation is a part key of its success and so if the action is likely to be unacceptable to key stakeholders, its feasibility is also likely to be low. Considering access or availability of equipment, resources, or staff to undertake a management action will also be important; for example, an action may not be feasible if the equipment needed cannot be moved to the location of interest. Feasibility may also involve considering the costs associated with each action, such as whether the action exceeds a strict budget or will be able to be approved by any stakeholders that must agree to its implementation."),
                                    br(),
                                    p("Carefully consider whether it is feasible to implement this action."),
                                    fixedRow(column(textAreaInput(paste0("action17",id),label="Feasibility",placeholder="We would need to get permission to install these structures under the road, which could take time....",width="200%",height="200%",rows=2),width=12))
                           ),
                           tabPanel(title="2.f Identify possible modifications",id=paste0("tab11",id),value=paste0("tab11_val",id),
                                    br(),
                                    h5(strong("How can the action be modified based on the previous evidence gathered?")),
                                    br(),
                                    p("By assessing the evidence from different sources on effectiveness, costs, acceptability, and feasibility of the action, modifications can be considered that might improve it. For example, there may be strong evidence from the scientific literature to suggest that creating certain habitats for great crested newts and white-faced darters will be beneficial, but a practitioner's explicit or tacit local knowledge also suggests that these species have slightly different habitat preferences in this region, and so a modification to this action may be necessary for it to be locally effective. Or an action such as an education campaign may not be acceptable to a key stakeholder if it is designed in a certain way, so modifications are necessary to ensure the action is acceptable. A structural action may also be too expensive to implement using certain materials and to be more cost-effective and ultimately more feasible, the action must be modified by using cheaper materials."),
                                    br(),
                                    fixedRow(column(textAreaInput(paste0("action18",id),label="Consider effectiveness of modificiations",placeholder="We could try certain designs of culverts and tunnels that limit mortality...",width="200%",height="200%",rows=2),width=12)),
                                    br()),
                           
                           tabPanel(title="2.g Summarise evidence, information, and uncertainty for this action",id=paste0("tab12",id),value=paste0("tab12_val",id),
                                    br(),
                                    h5(strong("How likely is this action to be locally effective based on all the evidence and information you have gathered?")),
                                    h5(strong("What is the overall level of uncertainty associated with these conclusions?")),
                                    br(),
                                    p("Once the previous steps have been considered, it may be useful to summarise the likely local effectiveness of each action (whether modified or not), and the important costs, acceptability, and feasibility considerations that come with them. This draws together all the evidence previously gathered so that an evidence-based decision can be made in the next step, considering the relative advantages and disadvantages of each action alongside each other.
                                      Uncertainty is also important to consider here, in particular to understand whether the evidence that has been gathered is sufficient in its reliability and relevance to make robust conclusions. It is also important to consider if there is conflicting evidence from different sources - for example, how much trust can be placed in the evidence drawn from the scientific literature versus evidence drawn from local knowledge?"),
                                    br(),
                                    fixedRow(column(textAreaInput(paste0("action19",id),label="Summarise all the evidence for action",placeholder="Overall, this action may be an effective action for us to use but could have negative impacts on amphibians that make this action unacceptably risky - not only in terms of damage to wildlife, but also to the organisation's reputation even if the design of the culverts and tunnels was modified. The installation of these structures will also take time and require extensive permissions, alongside costing a substantial amount, so this is also unlikely to be a feasible action to implement.",width="200%",height="200%",rows=2),width=12))
                                    )
                           
                         )
                         
                         
                ),target="tab13_val",position=c("before"),select=TRUE)
      }
      
    
    
    
  })
  
  
  
  
  observeEvent(input$remove, {
    id <- input$newactionnamerem
    removeTab(inputId = "maintabs", target = paste0("2.b-g ",id), session=session)
  })


#  output$summtab <- renderDT({
#    idlist <- sort(names(input))
#    actnames<-lapply(1:((length(idlist)-15)/16), ###########need to change these numbers if update rest of code and numbers of inputs!
#                     function(i){c(gsub("summarytabeff","",idlist[grep("summarytabeff",idlist)][i]),input[[idlist[grep("summarytabeff",idlist)][i]]],input[[idlist[grep("summarytabcost",idlist)][i]]],input[[idlist[grep("summarytabacc",idlist)][i]]])})
#    table1 <- data.frame(do.call(rbind,actnames))
#    colnames(table1)<- c("Actions","Local Effectivenss", "Costs", "Acceptability")
#    datatable(table1,editable="cell")
#    })

#  output$numids <- renderText({length(names(input))})

  
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
  
updateidlist <- eventReactive({input$remove|input$add},{
    id <- input$newactionnamerem
    if(id!=""){
      idlistrem <- c(idlistrem,names(input)[grep(id,names(input))])
    }
    idlistalt <- sort(names(input))
    if(length(idlistrem)>0){
      for(i in 1:length(idlistrem)){idlistalt <- idlistalt[-grep(idlistrem[[i]],idlistalt)]}
      }
    return(unlist(idlistalt))
  })

textsummary <- function(){
    idlist <- sort(names(input))
    idlistalt <- updateidlist()
    if(length(idlist)!=length(idlistalt)){idlist <- idlistalt}
    
    if(((length(idlist)-17)/14)>0){
    lapply(1:(((length(idlist)-17)/14)), ###########need to change these numbers if update rest of code and numbers of inputs!
                        function(i){
                          cat("# ",unlist(strsplit(idlist[grep("action07",idlist)][i],"7"))[2],"  \n  \n")
                          cat("## Describe action  \n",input[[idlist[grep("action07",idlist)][i]]],"  \n  \n")
                          cat("## Focus of action  \n",input[[idlist[grep("action08",idlist)][i]]],"  \n  \n")
                          cat("## Scientific evidence and uncertainty  \n")
                          cat("### Evidence sources considered  \n",input[[idlist[grep("action09",idlist)][i]]],"  \n  \n")
                          cat("### Detailed assessment of evidence  \n",input[[idlist[grep("action10",idlist)][i]]],"  \n  \n")
                          cat("### Summarise assessment of evidence  \n",input[[idlist[grep("action11",idlist)][i]]],"  \n  \n")
                          cat("## Local knowledge and uncertainty  \n")
                          cat("### Explicit knowledge  \n",input[[idlist[grep("action12",idlist)][i]]],"  \n  \n")
                          cat("### Tacit knowledge  \n",input[[idlist[grep("action13",idlist)][i]]],"  \n  \n")
                          cat("## Costs  \n")
                          cat("### Financial costs and resource requirements  \n",input[[idlist[grep("action14",idlist)][i]]],"  \n  \n")
                          cat("### Non-financial and non-target costs and benefits  \n",input[[idlist[grep("action15",idlist)][i]]],"  \n  \n")
                          cat("## Acceptability  \n",input[[idlist[grep("action16",idlist)][i]]],"  \n  \n")
                          cat("## Feasibility  \n",input[[idlist[grep("action17",idlist)][i]]],"  \n  \n")
                          cat("## Consider effectiveness of modificiations  \n",input[[idlist[grep("action18",idlist)][i]]],"  \n  \n")
                          cat("## Summarise all the evidence for action  \n",input[[idlist[grep("action19",idlist)][i]]],"  \n  \n")
                          cat("\\newpage  \n  \n")
                        })
    }
    if(((length(idlist)-17)/14)==0){
      cat("Please add some actions to display them in this report.")
    }
    }
  
}

enableBookmarking(store="server")
shinyApp(ui, server)
