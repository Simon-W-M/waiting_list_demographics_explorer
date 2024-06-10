library(shiny)
library(tidyverse)
library(reactable)
library(reactablefmtr)
library(tippy)
library(shinythemes)
library(shinyalert)

dat_sum <- read_rds('full_datab.Rdata')

dat_cov <- read_rds('cover_datab.Rdata')

df_sum <- read_rds('sum_datab.Rdata')

dat <- read_rds('dat_datab.Rdata')

clustered_table <-  read_rds('clustered_table.Rdata')

age_eth_gen_table <- function(area_tab, tfc_tab) {
  a <- dat_cov |>
    ungroup() |>
    filter(
      area %in% area_tab,
      tfc %in% tfc_tab,
      feature == "ethnic_group"
    ) |>
    dplyr::select(val, perc, lab) |>
    rename(
      eth = val,
      eth_perc = perc,
      eth_lab = lab
    )
  
  b <- dat_cov |>
    ungroup() |>
    filter(
      area %in% area_tab,
      tfc %in% tfc_tab,
      feature == "gender"
    ) |>
    dplyr::select(val, perc, lab) |>
    rename() |>
    rename(
      gen = val,
      gen_perc = perc,
      gen_lab = lab
    )
  
  c <- dat_cov |>
    ungroup() |>
    filter(
      area %in% area_tab,
      tfc %in% tfc_tab,
      feature == "age_band"
    ) |>
    dplyr::select(val, perc, lab) |>
    rename() |>
    rename(
      age = val,
      age_perc = perc,
      age_lab = lab
    )
  
  
  # find the largest number of rows in dataframe
  z <- max(c(nrow(a), nrow(b), nrow(c)))
  
  # pad the dataframes with blank rows to expand them to max number of rows
  aa <- data.frame(matrix(NA, nrow = z - nrow(a), ncol = 3))
  names(aa) <- names(a)
  a <- rbind(a, aa)
  
  bb <- data.frame(matrix(NA, nrow = z - nrow(b), ncol = 3))
  names(bb) <- names(b)
  b <- rbind(b, bb)
  
  cc <- data.frame(matrix(NA, nrow = z - nrow(c), ncol = 3))
  names(cc) <- names(c)
  c <- rbind(c, cc)
  
  # join all the data frames together side by side
  d <- cbind(a, b, c)
  
  # replace na in perc columns
  d <- d |>
    mutate(
      gen_perc = if_else(is.na(gen_perc), 0, gen_perc),
      age_perc = if_else(is.na(age_perc), 0, age_perc),
      eth_perc = if_else(is.na(eth_perc), 0, eth_perc)
    )
  
  tab <- d |>
    reactable(
      filterable = FALSE,
      columns = list(
        eth = colDef(
          header = "Ethnic group",
          width = 200
        ),
        eth_perc = colDef(
          header = "Percentage",
          width = 200,
          align = "left",
          cell = data_bars(d,
                           fill_color = c("#41B6E6", "#0072CE"),
                           max_value = 100,
                           tooltip = TRUE,
                           fill_gradient = TRUE,
                           animation = "width 2s ease",
                           box_shadow = TRUE,
                           text_position = "none",
                           text_color = "white"
          )
        ),
        eth_lab = colDef(
          header = "Breakdown",
          width = 250
        ),
        gen = colDef(
          header = "Gender",
          width = 100
        ),
        gen_perc = colDef(
          header = "Percentage",
          width = 200,
          align = "left",
          cell = data_bars(d,
                           fill_color = c("#41B6E6", "#0072CE"),
                           max_value = 100,
                           tooltip = TRUE,
                           fill_gradient = TRUE,
                           animation = "width 2s ease",
                           box_shadow = TRUE,
                           text_position = "none",
                           text_color = "white"
          )
        ),
        gen_lab = colDef(
          header = "Breakdown",
          width = 250
        ),
        age = colDef(
          header = "Age group",
          width = 100
        ),
        age_perc = colDef(
          header = "Percentage",
          width = 200,
          align = "left",
          cell = data_bars(d,
                           fill_color = c("#41B6E6", "#0072CE"),
                           max_value = 100,
                           tooltip = TRUE,
                           fill_gradient = TRUE,
                           animation = "width 2s ease",
                           box_shadow = TRUE,
                           text_position = "none",
                           text_color = "white"
          )
        ),
        age_lab = colDef(
          header = "Breakdown",
          width = 250
        )
      )
    )
  return (tab)
}


imd_pri_table <- function(area_tab, tfc_tab) {
  a <- dat_cov |>
    ungroup() |>
    filter(
      area %in% area_tab,
      tfc %in% tfc_tab,
      feature == "imd"
    ) |>
    dplyr::select(val, perc, lab) |>
    rename(
      imd = val,
      imd_perc = perc,
      imd_lab = lab
    )
  
  b <- dat_cov |>
    ungroup() |>
    filter(
      area %in% area_tab,
      tfc %in% tfc_tab,
      feature == "procedure_priority_code"
    ) |>
    dplyr::select(val, perc, lab) |>
    rename(
      ppc = val,
      ppc_perc = perc,
      ppc_lab = lab
    )
  
  # find the largest number of rows in dataframe
  z <- max(c(nrow(a), nrow(b)))
  
  # pad the dataframes with blank rows to expand them to max number of rows
  aa <- data.frame(matrix(NA, nrow = z - nrow(a), ncol = 3))
  names(aa) <- names(a)
  a <- rbind(a, aa)
  
  bb <- data.frame(matrix(NA, nrow = z - nrow(b), ncol = 3))
  names(bb) <- names(b)
  b <- rbind(b, bb)
  
  # join all the data frames together side by side
  d <- cbind(a, b)
  
  # replace na in perc columns
  d <- d |>
    mutate(
      imd_perc = if_else(is.na(imd_perc), 0, imd_perc),
      ppc_perc = if_else(is.na(ppc_perc), 0, ppc_perc)
    )
  
  d |>
    reactable(
      filterable = FALSE,
      defaultPageSize = 15,
      columns = list(
        imd = colDef(
          header = "IMD",
          width = 200
        ),
        imd_perc = colDef(
          header = "Percentage",
          width = 200,
          align = "left",
          cell = data_bars(d,
                           fill_color = c("#41B6E6", "#0072CE"),
                           max_value = 100,
                           tooltip = TRUE,
                           fill_gradient = TRUE,
                           animation = "width 2s ease",
                           box_shadow = TRUE,
                           text_position = "none",
                           text_color = "white"
          )
        ),
        imd_lab = colDef(
          header = "Breakdown",
          width = 250
        ),
        ppc = colDef(
          header = "Procedure priority",
          width = 100
        ),
        ppc_perc = colDef(
          header = "Percentage",
          width = 200,
          align = "left",
          cell = data_bars(d,
                           fill_color = c("#41B6E6", "#0072CE"),
                           max_value = 100,
                           tooltip = TRUE,
                           fill_gradient = TRUE,
                           animation = "width 2s ease",
                           box_shadow = TRUE,
                           text_position = "none",
                           text_color = "white"
          )
        ),
        ppc_lab = colDef(
          header = "Breakdown",
          width = 250
        )
      )  
    )
}



# function for adding tool tips tips
with_tooltip <- function(value, tooltip, ...) {
  div(style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
      tippy(value, tooltip, ...))
}

#################
# cluster table #
#################

# function for presenting clustered data
cluster_table <- function(data, icb , tf_c) {
  
  data <- data |>
    filter (icb_name == icb,
            tfc == tf_c) |>
    select (!c(icb_name, tfc))
  
  # filter out commentary into seperate datatable - this will be used as column names later
  com <- data |> 
    filter(feature == 'Commentary')
  
  # remove commentary and convert to numeric - this allows mix of dynamic commentary and numbers
  data <- data |>
    filter(!feature == 'Commentary') |>
    mutate(First = as.numeric(First),
           Second = as.numeric(Second),
           Third = as.numeric(Third),
           Forth = as.numeric(Forth),
           Fifth = as.numeric(Fifth))
  
  # find the max value across each group in order to scale graphs to same scale
  mx <- max(pmax(data$First, data$Second, data$Third, data$Forth, data$Fifth))
  
  # actual reactable stuff to create table
  data |> reactable(
    filterable = FALSE,
    compact = TRUE,
    
    rowStyle = function(index) {
      if (index %in% c(1)) {
        list(fontWeight = "bold")
      } 
      else if (index %in% c(2, 8, 13, 19)) {
        list(`border-top` = "thin solid")
      }
    },
    defaultPageSize = 23,
    columns = list(
      feature = colDef(
        header = ''),
      First = colDef(
        header = com$First[1],
        headerStyle = list(fontWeight = "normal"),
        html = TRUE,
        #width = 200,
        align = "left",
        cell = data_bars(data,
                         fill_color = c("#41B6E6", "#0072CE"),
                         max_value = mx,
                         tooltip = FALSE,
                         fill_gradient = TRUE,
                         animation = "width 2s ease",
                         box_shadow = TRUE,
                         text_position = "outside-base",
                         text_color = "black",
                         bold_text = TRUE
        )),
      Second = colDef(
        header = com$Second[1],
        headerStyle = list(fontWeight = "normal"),
        html = TRUE,
        #width = 200,
        align = "left",
        cell = data_bars(data,
                         fill_color = c("#41B6E6", "#0072CE"),
                         max_value = mx,
                         tooltip = FALSE,
                         fill_gradient = TRUE,
                         animation = "width 2s ease",
                         box_shadow = TRUE,
                         text_position = "outside-base",
                         text_color = "black",
                         bold_text = TRUE
        )),
      Third = colDef(
        header = com$Third[1],
        html = TRUE,
        #width = 200,
        headerStyle = list(fontWeight = "normal"),
        align = "left",
        cell = data_bars(data,
                         fill_color = c("#41B6E6", "#0072CE"),
                         max_value = mx,
                         tooltip = FALSE,
                         fill_gradient = TRUE,
                         animation = "width 2s ease",
                         box_shadow = TRUE,
                         text_position = "outside-base",
                         text_color = "black",
                         bold_text = TRUE
                         
        )),
      Forth = colDef(
        header = com$Forth[1],
        html = TRUE,
        headerStyle = list(fontWeight = "normal"),
        #width = 200,
        align = "left",
        cell = data_bars(data,
                         fill_color = c("#41B6E6", "#0072CE"),
                         max_value = mx,
                         tooltip = FALSE,
                         fill_gradient = TRUE,
                         animation = "width 2s ease",
                         box_shadow = TRUE,
                         text_position = "outside-base",
                         text_color = "black",
                         bold_text = TRUE
                         
        )),
      Fifth = colDef(
        header = com$Fifth[1],
        html = TRUE,
        #width = 200,
        headerStyle = list(fontWeight = "normal"),
        align = "left",
        cell = data_bars(data,
                         fill_color = c("#41B6E6", "#0072CE"),
                         max_value = mx,
                         tooltip = FALSE,
                         # background = 'white',
                         fill_gradient = TRUE,
                         animation = "width 2s ease",
                         box_shadow = TRUE,
                         text_position = "outside-base",
                         text_color = "black",
                         bold_text = TRUE
        ))
    ))
}


###################################################

dat$feature <- as.factor(dat$feature)

area_all <- unique(dat$area)

areas <- c(area_all[43:52],area_all[1:42])
areas_list <- as.list(areas)
names(areas_list) <- areas
icbs <- as.list(area_all[1:42])


tfcs <- unique(dat$tfc)
tfc_list <- as.list(tfcs)
names(tfc_list) <- tfcs

region <- unique(df_sum$region)
region_list <- as.list(region)
names(region_list) <- region

df_react <- df_sum

###################################################
# Define UI for application 
ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Waiting List Patient Demographic Explorer - for use by NHS England Employees only"),
                tabsetPanel(
                  tabPanel("Introduction",
                           mainPanel(
                             br(),
                             h1(strong("This tool allows the user to look at the demographic characteristics of patients on the waiting list, and explore differences between geographies, lengths of wait, and specialty. "), style = "font-size:14px;"),
                             br(),
                             p("The tool identifies where there is a statistically significant difference in the patient profile of those waiting over and under a selected number of weeks (52ww, 65ww, 75ww or 104ww) - essentially is there a difference between the longest waiters and the rest of the waiting list in terms of their demographic characteristics. "),
                             br(),
                             p("The tool allows comparisons between groups based on Age, Gender, Ethnicity or deprivation. Intersectional comparisons are not currently available, although this continues to be a work in progress.  "),
                             br(),
                             p("Users can look at differences at England, Regional or ICB level, as well as breaking down by specialty (using Treatment Function Codes). However, please note that at more granular levels there may be small numbers which will limit the ability to detect significant differences. "),
                             br(),
                             p(
                               span("WARNING", style = "color:blue"),
                               ":very long waiters can be small cohort with potentially unobserved factors that could explain why they are long waiters (including patient choice). "),
                             br(),
                             
                             
                             br()
                             
                           )),
                  tabPanel("Summary page",
                           fluidRow(
                             column(
                               2,
                               radioButtons(
                                 inputId = "wait_sum",
                                 label = "Select Waiting List Comparison",
                                 choices = list(
                                   "52+ weeks",
                                   "65+ weeks",
                                   "75+ weeks",
                                   "104+ weeks"
                                 )
                               )),
                             column(
                               2,
                               checkboxGroupInput(
                                 inputId = "region",
                                 label = "Choose Region",
                                 choices = region_list,
                                 selected = '.NATIONAL',
                                 inline = TRUE
                               )
                             )),
                           actionButton("preview", "Instructions",width = '400px', icon = icon('question-circle')),
                           
                           span(textOutput("selected_var_sum")),
                           
                           reactableOutput(outputId = "sum_table")),
                  tabPanel(
                    "Inequalities explorer",
                    fluidRow(
                      column(
                        2,
                        radioButtons(
                          inputId = "wait",
                          label = "Select Waiting List Comparison",
                          choices = list(
                            "52+ weeks",
                            "65+ weeks",
                            "75+ weeks",
                            "104+ weeks"
                          )
                        )),
                      column(
                        2,
                        radioButtons(
                          inputId = "feat",
                          label = "Feature",
                          choices = list(
                            "procedure_priority_code",
                            "imd",
                            "ethnic_group",
                            "age_band",
                            "gender"
                          )
                        )
                      ),
                      column(
                        2,
                        selectInput(
                          inputId = "gr",
                          label = "Select Area",
                          multiple = FALSE,
                          choices = areas_list,
                          selected = '.NATIONAL'
                        )
                      ),
                      column(
                        2,
                        selectInput(
                          inputId = "treat",
                          label = "Treatment Function Code",
                          multiple = FALSE,
                          choices = list('All Treatment Functions' = 'All Treatment Functions',
                                         'General Surgery - 100' = '100',
                                         'Urology Service - 101' = '101',
                                         'Trauma and Orthopaedic Service - 110' = '110',
                                         'Ear Nose and Throat Service - 120' = '120',
                                         'Ophthalmology Service - 130' = '130',
                                         'Oral Surgery - 140' = '140',
                                         'Neurosurgical Service - 150' = '150',
                                         'Plastic Surgery Service - 160' = '160',
                                         'Cardiothoracic Surgery  - 170' = '170',
                                         'General Internal Medicine - 300' = '300',
                                         'Gastroenterology Service - 301' = '301',
                                         'Cardiology Service - 320' = '320',
                                         'Dermatology Service - 330' = '330',
                                         'Elderly Medicine Service - 430' = '430',
                                         'Gynaecology Service - 502' = '502',
                                         'Other - Medical Services - X02' = 'X02',
                                         'Other - Mental Health Services - X03' = 'X03',
                                         'Other - Paediatric Services- X04' = 'X04',
                                         'Other - Surgical Services- X05' = 'X05',
                                         'Other - Other Services- X06' = 'X06')
                        )
                      )
                    ),
                    actionButton(inputId = "second_ins", label = "Instructions",width = '400px', icon = icon('question-circle')),
                    fluidRow(
                      # column (12,
                      span(textOutput("selected_var")),
                      tags$head(tags$style("#selected_var{color: blue;
                                 font-size: 20px;
                                 font-style: bold;
                                 }"
                      )),
                      
                      
                      reactableOutput(outputId = "table")
                    )
                  ),
                  tabPanel("Data Coverage",
                           fluidRow(
                             column(
                               2,
                               selectInput(
                                 inputId = "area_chart",
                                 label = "Select Area",
                                 multiple = FALSE,
                                 choices = areas_list,
                                 selected = '.NATIONAL'
                               )
                             ),
                             column(
                               2,
                               selectInput(
                                 inputId = "treat_chart",
                                 label = "Treatment Function Code",
                                 multiple = FALSE,
                                 choices = list('All Treatment Functions' = 'All Treatment Functions',
                                                'General Surgery - 100' = '100',
                                                'Urology Service - 101' = '101',
                                                'Trauma and Orthopaedic Service - 110' = '110',
                                                'Ear Nose and Throat Service - 120' = '120',
                                                'Ophthalmology Service - 130' = '130',
                                                'Oral Surgery - 140' = '140',
                                                'Neurosurgical Service - 150' = '150',
                                                'Plastic Surgery Service - 160' = '160',
                                                'Cardiothoracic Surgery  - 170' = '170',
                                                'General Internal Medicine - 300' = '300',
                                                'Gastroenterology Service - 301' = '301',
                                                'Cardiology Service - 320' = '320',
                                                'Dermatology Service - 330' = '330',
                                                'Elderly Medicine Service - 430' = '430',
                                                'Gynaecology Service - 502' = '502',
                                                'Other - Medical Services - X02' = 'X02',
                                                'Other - Mental Health Services - X03' = 'X03',
                                                'Other - Paediatric Services- X04' = 'X04',
                                                'Other - Surgical Services- X05' = 'X05',
                                                'Other - Other Services- X06' = 'X06'))
                             )
                           ),
                           
                           actionButton("third_ins", 
                                        "Instructions", 
                                        width = '400px', 
                                        icon = icon('question-circle')),
                           fluidRow(
                             # column (12,
                             reactableOutput(outputId = "tab_age_gen")
                           ),
                           fluidRow(
                             # column (12,
                             reactableOutput(outputId = "tab_imd_ppc")
                           )
                  ),
                  ## clustering page
                  tabPanel("52+ week waiters clustering",
                           fluidRow(
                             column(
                               2,
                               selectInput(
                                 inputId = "icb_clust",
                                 label = "Select Area",
                                 multiple = FALSE,
                                 choices = icbs,  
                                 selected = 'BATH AND NORTH EAST SOMERSET, SWINDON AND WILTSHIRE'
                               )
                             ),
                             column(
                               2,
                               selectInput(
                                 inputId = "treat_clust",
                                 label = "Treatment Function Code",
                                 multiple = FALSE,
                                 choices = list('General Surgery - 100' = '100',
                                                'Urology Service - 101' = '101',
                                                'Trauma and Orthopaedic Service - 110' = '110',
                                                'Ear Nose and Throat Service - 120' = '120',
                                                'Ophthalmology Service - 130' = '130',
                                                'Oral Surgery - 140' = '140',
                                                'Neurosurgical Service - 150' = '150',
                                                'Plastic Surgery Service - 160' = '160',
                                                'Cardiothoracic Surgery  - 170' = '170',
                                                'General Internal Medicine - 300' = '300',
                                                'Gastroenterology Service - 301' = '301',
                                                'Cardiology Service - 320' = '320',
                                                'Dermatology Service - 330' = '330',
                                                'Elderly Medicine Service - 430' = '430',
                                                'Gynaecology Service - 502' = '502',
                                                'Other - Medical Services - X02' = 'X02',
                                                'Other - Mental Health Services - X03' = 'X03',
                                                'Other - Paediatric Services- X04' = 'X04',
                                                'Other - Surgical Services- X05' = 'X05',
                                                'Other - Other Services- X06' = 'X06'))
                             ),
                             column(2,
                                    actionButton("forth_ins", "Instructions", width = '400px', icon = icon('question-circle')))
                           ),
                           fluidRow(
                             # column (2,
                             reactableOutput(outputId = "clust_tab")
                           )
                  ),    
                  tabPanel("Metadata",
                           mainPanel(
                             br(),
                             h1(strong("WLMDS data is restricted management information for NHS purposes only; information spanning multiple regions or indicators of national positions should not be made available outside of NHSE."), style = "font-size:14px;"),
                             br(),
                             p("The Waiting List Minimum Dataset (WLMDS) collection began in April 2021 as a new patient level weekly waiting list data submission by providers, enabling the waiting list position to be better understood and managed as part of the National Elective Restoration Programme. The Waiting List Minimum Dataset is an essential tool in the monitoring of elective recovery and is used as the basis for decision making both nationally and locally, as well as a central source of performance monitoring and reporting. It is utilised to support elective recovery (inc. policy and monitoring delivery) and to support providers in improving waiting list data quality."),
                             br(),
                             p("For statistical significance tests, we divided the waiting list into two groups of patients: longer waiters, those waiting for W weeks or more, and shorter waiters, those waiting for less than W weeks where W can be 52, 65, 78 or 104 weeks. We then considered whether these two groups are distinguishable based on certain features – age, ethnicity, gender, deprivation – at different granularities – geographical and by specialty. To conclude longer and shorter waiters are distinguishable (or correlated with a given feature), statistical tests must satisfy the following three tests: be valid, both including and excluding unknown/null entries; be significant at the 95% confidence level; be run on a sample of at least 20 pathways. "),
                             br(),
                             p("Ethnicity Coding. The Office for Health Improvement and Disparities (OHID) developed a process for assigning ethnicity from HES data, as detailed here. In broad terms, a patient is assigned the most recent usable ethnic code for that individual that is available in HES. Where there were missing values for ethnicity in WLMDS, ethnicity fields derived from SUS (rather than HES) using this OHID methodology were used. A value of ‘9: Not Known’ will only be present if there are no known ethnicities in any of the SUS data sets. To account for the overrepresentation of the ‘other’ ethnic group, if the most common ethnic group assigned by the previous method is ‘other’ then the second-most common usable ethnic group is assigned instead. A person will only be assigned to the ‘other’ ethnic group if there are no other usable ethnic groups. In instances where there are multiple ethnicities with the same frequency, latest date and source of data, Census data was used to inform which ethnicity us selected for a patient. The most common of the ethnicities listed in the UK population (from the Census) was assigned to these individuals. "),
                             br(),
                             p("A note on clustering of 52+ week patients.  Clustering is an unsupervised machine learning technique to group patients togther where they share the most common characteristics.  The clustering algorirm has been told to split the over cohort into 5 groups of the most similar patients.  The choice of 5 groups is arbitary and was based on the number of features and breakdown of those demographics.  The clsutering gives us an insight into how demographics correlate within the cohort.  Clustering was built on a Gowever distance and is carried out on only the patients waiting over 52 weeks and where there are at least 20 patients in the overal cohort."),
                             br(),
                             p("This report is in development and small numbers have not been suppressed in all areas"),
                             br(),
                             p("For further information contact england.electiverecovery-info@nhs.net "),
                           ))
                )
)


table_id <- 4


server <- function(input, output, session) {
  output$image1 <- renderImage({
    ImgTxt <- 'https://media.giphy.com/media/ICOgUNjpvO0PC/giphy.gif'
    width<- "80%"
    height<- "20%"
    list(src = ImgTxt,
         contentType = "image/gif",
         width = width,
         height = "auto"
    )
  }, deleteFile = FALSE)
  output$tab_age_gen <- renderReactable(age_eth_gen_table (input$area_chart, input$treat_chart))
  output$tab_imd_ppc <- renderReactable(imd_pri_table (input$area_chart, input$treat_chart))
  output$clust_tab <- renderReactable(cluster_table (clustered_table, input$icb_clust, input$treat_clust))
  output$selected_var <- renderText({ 
    paste(' --- ',input$wait,' --- ', input$gr, ' --- ', input$treat, ' --- ', input$feat, ' ---')})
  output$selected_var_sum <- renderText({ 
    paste(' --- ',input$wait_sum,' --- ', input$region)})
  
  output$sum_table <-
    renderReactable(df_sum |>
                      dplyr::filter (wait_compare %in% input$wait_sum,
                                     region %in% input$region) |> 
                      reactable(columns = list(
                        tot_sig_100 = colDef(header = with_tooltip("100", "General Surgery Service"), 
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_100',
                                               box_shadow = TRUE,
                                               text_size = 11
                                               
                                             )),
                        tot_sig_101 = colDef(header = with_tooltip("101", "Urology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum|>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_101',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_110 = colDef(header = with_tooltip("110", "Trauma and Orthopaedic Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_110',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_120 = colDef(header = with_tooltip("120", "Ear Nose and Throat Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_120',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_130 = colDef(header = with_tooltip("130", "Ophthalmology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_130',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_140 = colDef(header = with_tooltip("140", "Oral Surgery Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_140',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_150 = colDef(header = with_tooltip("150", "Neurosurgical Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_150',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_160 = colDef(header = with_tooltip("160", "Plastic Surgery Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_160',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_170 = colDef(header = with_tooltip("170", "Cardiothoracic Surgery Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_170',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_300 = colDef(header = with_tooltip("300", "General Internal Medicine Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_300',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_301 = colDef(header = with_tooltip("301", "Gastroenterology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_301',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_320 = colDef(header = with_tooltip("320", "Cardiology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_320',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_330 = colDef(header = with_tooltip("330", "Dermatology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_330',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_340 = colDef(header = with_tooltip("340", "Respiratory Medicine Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_340',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_400 = colDef(header = with_tooltip("400", "Neurology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_400',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_410 = colDef(header = with_tooltip("410", "Rheumatology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_410',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_430 = colDef(header = with_tooltip("430", "Elderly Medicine Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_430',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        tot_sig_502 = colDef(header = with_tooltip("502", "Gynaecology Service"),
                                             width = 80,
                                             cell = pill_buttons(
                                               data = df_sum |>
                                                 dplyr::filter (wait_compare %in% input$wait_sum,
                                                                region %in% input$region),
                                               color_ref = 'total_diff_502',
                                               box_shadow = TRUE,
                                               text_size = 11
                                             )),
                        `tot_sig_All Treatment Functions` = colDef(header = with_tooltip("All Treatment Functions", "All Treatment Functions"),
                                                                   width = 80,                     
                                                                   cell = pill_buttons(
                                                                     data = df_sum |>
                                                                       dplyr::filter (wait_compare %in% input$wait_sum,
                                                                                      region %in% input$region),
                                                                     color_ref = 'total_diff_All Treatment Functions',
                                                                     box_shadow = TRUE,
                                                                     text_size = 11
                                                                   )),
                        age_tot = colDef(header = with_tooltip("Age", "Number of Age Differences"),
                                         width = 35
                        ),
                        gen_tot = colDef(header = with_tooltip("Gender", "Number of Gender Differences"),
                                         width = 35
                        ),
                        eth_tot = colDef(header = with_tooltip("Ethnicity", "Number of Ethnicity Differences"),
                                         width = 35
                        ),
                        imd_tot = colDef(header = with_tooltip("IMD", "Number of IMD Differences"),
                                         width = 35
                        ),
                        # total_diff_110 =  colDef(align = 'center',
                        #                       cell = bubble_grid(df_sum)),
                        wait_compare = colDef(show = FALSE),
                        area = colDef(width = 200),
                        total_diff_100 = colDef(show = FALSE),
                        total_diff_101 = colDef(show = FALSE),
                        total_diff_110 = colDef(show = FALSE),
                        total_diff_120 = colDef(show = FALSE),
                        total_diff_130 = colDef(show = FALSE),
                        total_diff_140 = colDef(show = FALSE),
                        total_diff_150 = colDef(show = FALSE),
                        total_diff_160 = colDef(show = FALSE),
                        total_diff_170 = colDef(show = FALSE),
                        total_diff_300 = colDef(show = FALSE),
                        total_diff_301 = colDef(show = FALSE),
                        total_diff_320 = colDef(show = FALSE),
                        total_diff_330 = colDef(show = FALSE),
                        total_diff_340 = colDef(show = FALSE),
                        total_diff_400 = colDef(show = FALSE),
                        total_diff_410 = colDef(show = FALSE),
                        total_diff_430 = colDef(show = FALSE),
                        total_diff_502 = colDef(show = FALSE),
                        `total_diff_All Treatment Functions` = colDef(show = FALSE),
                        region = colDef(show = FALSE)),
                        pagination  = FALSE,
                        theme = reactableTheme(
                          borderColor = "#dfe2e5",
                          stripedColor = "#f6f8fa",
                          highlightColor = "#f0f5f9",
                          cellPadding = "8px 0px") )
    )        
  observeEvent(input$preview, {shinyalert(
    title = "Instructions",
    text = "Select waiting list comparison group and geography<br><br>
    The table will highlight the protected characteristics for which there are statistically significant differences between the selected long waiter category and the rest of the waiting list <br><br>
    The relevant demographic profiles are represented as age <b>(A)</b>, gender <b>(G)</b>, ethnicity <b>(E)</b> or deprivation <b>(I)</b> <br><br>
    A dash indicates no statistical significance. <br><br>
    For details of how the groups differ on a selected characteristic, use the Inequalities explorer tab. ",
    size = "l", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  )})
  observeEvent(input$second_ins, {shinyalert(
    title = "Instructions",
    text = "Please select waiting time of interest, demographic characteristic, TFC and geography using these filters <br><br>
    One row will be shown for each grouping within the selected characteristic <br><br>
    Data is shown as a % of the total waiting list over/under the selected waiting time who are categorised within each demographic group.  <br><br>
    The chart on the right hand side indicates the extent to which the profile differes between the long waiters and the remaining waiting list. The test indicates whether these differences are significant. <br><br>
    For selections at sub-England level the rate columns indicate whether the % of pathways in the given category is comparable to average levels in the population – this indicates whether any significant differences seen are common across geographies, or localised to the selected geography<br><br> ",
    size = "l", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  )})
  observeEvent(input$third_ins, {shinyalert(
    title = "Instructions",
    text = "Please select Region/ICB and Treatment Function Code using these filters <br><br>
     <br><br>
    This shows simple breakdown of demographic features.  <br><br> ",
    size = "l", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  )})
  observeEvent(input$forth_ins, {shinyalert(
    title = "Instructions",
    text = "Please select ICB and Treatment Function Code using these filters <br><br>
     <br><br>
    This shows results of clustering similar patients into groups.   More info can be found in the metadata tab.  It gives an indication of how demographics inter relate<br><br> ",
    size = "l", 
    closeOnEsc = TRUE,
    closeOnClickOutside = TRUE,
    html = TRUE,
    type = "warning",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = FALSE
  )})
  output$table <-
    renderReactable(dat %>%
                      # filter out patients outside of age range
                      dplyr::filter(
                        tfc == input$treat,
                        wait_compare == input$wait,
                        area == input$gr,
                        feature == input$feat
                      ) |>
                      reactable(
                        filterable = FALSE,
                        defaultPageSize = 15,
                        columns = list(
                          wait_compare = colDef(show = FALSE),
                          tfc = colDef(show = FALSE),
                          `control bar` = colDef(cell = data_bars(dat,
                                                                  fill_color = c("#41B6E6", "#0072CE"),
                                                                  max_value = 100,
                                                                  tooltip = TRUE,
                                                                  fill_gradient = TRUE,
                                                                  animation = "width 2s ease",
                                                                  box_shadow = TRUE,
                                                                  text_position = "none"
                          )),
                          `waiters bar` = colDef(cell = data_bars(dat,
                                                                  fill_color = c("#41B6E6", "#0072CE"),
                                                                  max_value = 100,
                                                                  tooltip = TRUE,
                                                                  fill_gradient = TRUE, box_shadow = TRUE,
                                                                  text_position = "none"
                          )),
                          area = colDef(show = FALSE),
                          val = colDef(style = list(borderRight = "2px solid rgba(0, 0, 0, 0.2)")),
                          `control rate` = colDef(style = list(borderRight = "2px solid rgba(0, 0, 0, 0.2)")),
                          change = colDef(
                            cell = data_bars(dat,
                                             # number_fmt = scales::percent,
                                             fill_color = c("#00A9CE", "#ED8B00"),
                                             force_outside = c(0, 0.5),
                                             # text_position = "above",
                                             # fill_gradient = TRUE,
                                             box_shadow = TRUE,
                                             max_value = 10,
                                             min_value = 10,
                                             tooltip = TRUE
                            )
                          )
                        )
                      ))
}




# Run the application 
shinyApp(ui = ui, server = server)
