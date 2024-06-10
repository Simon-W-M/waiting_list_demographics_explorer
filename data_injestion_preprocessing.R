
# Specify required packages
my_packages <- c("tidyverse",
                 "odbc", 
                 "DBI", 
                 "glue", 
                 "janitor", 
                 "cluster", 
                 "data.table", 
                 "mltools",
                 "cli") 

# Extract not installed packages
not_installed <- my_packages[!(my_packages %in% installed.packages()[ , "Package"])]  

# Install not installed packages
if(length(not_installed)) install.packages(not_installed)   

# Load all packages
lapply(my_packages, library, character.only = TRUE)                                    

###################
# UDAL connection #
###################

# this pulls a separate file to store connection string and user name
# you will have to set up your own

cat('\n\n\nChoose your personal credentials file\n*** NOTE: pop up may be hiding in your task bar ***\n\n\n')


boxx("Choose your personal credentials file\n*** NOTE: pop up may be hiding in your task bar ***", 
     padding = 0, 
     background_col = '#78BE20') 


source(choose.files())

# create conection critera
con_udal <- DBI::dbConnect(drv = odbc::odbc(),
                           driver = "ODBC Driver 17 for SQL Server",
                           server = serv,
                           database = db,
                           UID = user,
                           authentication = "ActiveDirectoryInteractive")

cli_progress_step("Downloading data")

boxx("Please enter two factor authentication\n*** NOTE: pop up may be hiding in your task bar ***", 
     padding = 0, 
     background_col = '#ED8B00') 


# reads the sql query
df_full_withoutmpi <- DBI::dbGetQuery(conn = con_udal, 
                                      statement = read_file("wl_joined.sql"))

###############################
# Data clean up and wrangling #
###############################

# list of valid codes
ethnicity_list <- c(
  "A", "M", "Z", "9", "J", "H", "C", "N",
  "L", "S", "G", "B", "K", "D", "P",
  "R", "99", "F", "E"
)

# retain only valid codes, any other codes change to NA
df_full_withoutmpi <- df_full_withoutmpi |>
  mutate(clean_eth = case_when(
    ETHNIC_CATEGORY %in% ethnicity_list ~ ETHNIC_CATEGORY,
    TRUE ~ "NA"
  ))

# add a flag where WL and MPI ethnicity are different
df <- df_full_withoutmpi |>
  mutate(mismatch_eth = ifelse(eth_CJ != clean_eth, 1, 0))

#clean and combine the data, assuming MPI has the more accurate ethnicity table
df_comb <- df |> mutate(clean_eth = if_else (clean_eth == "99", "9", clean_eth),
                        eth_CJ = if_else (eth_CJ == "99", "9", eth_CJ),
                        clean_eth = if_else (is.na(clean_eth), "9", clean_eth),
                        eth_CJ = if_else (is.na(eth_CJ), "9", eth_CJ))

# rules around precedence of ethnicity in case of mismatches
df_comb <- df_comb |> mutate(eth_comb = case_when(
  mismatch_eth == 0 ~ clean_eth,
  clean_eth == "NA" ~ eth_CJ,
  is.na(eth_CJ) ~ clean_eth,
  clean_eth != eth_CJ ~ eth_CJ,
  clean_eth == "S" & (eth_CJ != "9"| eth_CJ != "99") ~ eth_CJ,
  clean_eth == "Z" & (eth_CJ != "9"| eth_CJ != "99") ~ eth_CJ,
  clean_eth == "9" & eth_CJ == "9" ~ eth_CJ,
  TRUE ~ "error"
))

# clean column names to snake case
df <- clean_names(df_comb)


# tidy a few data items
# add a flag to weeks waiting 
# add ordered categories to imd
df <- df |>
  mutate(fiftytwo_wks_flag = if_else (weeks_group %in% c('052_To_065', '065_To_078','078_To_104', 'Over_104'),1,0),
         sixtyfive_wks_flag = if_else (weeks_group %in% c('065_To_078','078_To_104', 'Over_104'),1,0),
         seventyeight_wks_flag = if_else (weeks_group %in% c('078_To_104', 'Over_104'),1,0),
         onezerofour_wks_flag = if_else (weeks_group %in% c('Over_104'),1,0),
         imd = as.character(imd),
         imd = tidyr::replace_na(imd, 'Not known')   , 
         imd = ordered(imd, levels = c(1,2,3,4,5,6,7,8,9,10,'Not known')),
         ethnic_group = case_when (ethnic_group %in% c('Other ethnic groups',
                                                       'White',
                                                       'Mixed',
                                                       'Asian or Asian British', 
                                                       'Black or Black British') ~ ethnic_group,
                                   TRUE ~ 'Not known'))

##  Convert combined ethnicity to groups
df <- df |> mutate(ethnic_group = case_when (eth_comb == 'A' ~ 'White',
                                             eth_comb == 'M' ~ 'Black or Black British',
                                             eth_comb == 'N' ~ 'Black or Black British',
                                             eth_comb == '9' ~ 'Not known',
                                             eth_comb == 'H' ~ 'Asian or Asian British',
                                             eth_comb == 'J' ~ 'Asian or Asian British',
                                             eth_comb == 'L' ~ 'Asian or Asian British',
                                             eth_comb == 'C' ~ 'White',
                                             eth_comb == 'G' ~ 'Mixed',
                                             eth_comb == 'K' ~ 'Asian or Asian British',
                                             eth_comb == 'S' ~ 'Other ethnic groups',
                                             eth_comb == 'Z' ~ 'Not Stated',
                                             eth_comb == 'F' ~ 'Mixed',
                                             eth_comb == 'R' ~ 'Other ethnic groups',
                                             eth_comb == 'B' ~ 'White',
                                             eth_comb == 'P' ~ 'Black or Black British',
                                             eth_comb == '99' ~ 'Not known',
                                             eth_comb == 'E' ~ 'Mixed',
                                             eth_comb == 'D' ~ 'Mixed',
                                             eth_comb == 'NA' ~ 'Not known',
                                             TRUE ~ 'Not known'
))

# some defaults to feed in fo r testing purposes
#feature <- 'gender'
#group_area <- 'region'
#group_tfc <- '110'
#banding <- 'fiftytwo_wks_flag'

##############################
# Main fisher / chi function #
##############################

# Performs chi-squared and Fisher's exact tests to compare a categorical feature between two groups, 
# stratified by treatment function and area.

#' @param feature Character string specifying the name of the categorical feature to compare. 
#' #' Valid options are "procedure_priority_code", "ethnic_group","imd","age_band", "gender". 
#' @param group_area Character string indicating how to group the data geographically. 
#' #' Valid options are "icb", "region", or "national". 
#' @param group_tfc Character string indicating whether to group the data by treatment function. 
#' #' Valid options are "tfc" or "all". 
#' @param banding Character string specifying the binary grouping variable to use for comparison. 
#' #' Valid options are "fiftytwo_wks_flag","sixtyfive_wks_flag","seventyeight_wks_flag","onezerofour_wks_flag"
#' 
#' @return A data frame containing the following columns: 
#' - wait_compare: The binary grouping variable used for comparison. 
#' - area: The geographic area (ICB, region, or national). 
#' - feature: The name of the categorical feature being compared. 
#' - tfc: The treatment function code (if applicable). 
#' - val: The value of the categorical feature. 
#' - feature_count_0: The count of the feature value in group 0. (the short waits)
#' - feature_count_1: The count of the feature value in group 1. (the long waits)
#' - total_group_0: The total count of observations in group 0. 
#' - total_group_1: The total count of observations in group 1. 
#' - perc_feature_count_0: The percentage of the feature value in group 0. 
#' - perc_feature_count_1: The percentage of the feature value in group 1. 
#' - chi_test: The p-value from the chi-squared test (or "999" if not applicable). 
#' - chi_sig: An indicator of whether the chi-squared test is significant (1 for significant, 0 for not significant, 2 for not applicable). 
#' - fisher_test: The p-value from Fisher's exact test (or "999" if not applicable). 
#' - fish_sig: An indicator of whether Fisher's exact test is significant (1 for significant, 0 for not significant, 2 for not applicable). 
#' - chi_test_na: The p-value from the chi-squared test without nulls (or "999" if not applicable). 
#' - chi_test_na_sig: An indicator of whether the chi-squared test without nulls is significant (1 for significant, 0 for not significant, 2 for not applicable). 
#' - fisher_test_na: The p-value from Fisher's exact test without nulls (or "999" if not applicable). 
#' - fish_test_na_sig: An indicator of whether Fisher's exact test without nulls is significant (1 for significant, 0 for not significant, 2 for not applicable). 

#' @examples 
#' 
#' Compare the distribution of gender between 52-week waiters and non-waiters, 
#' stratified by ICB and over all TFCs 
#' chi_results <- chi_group_multi(feature = "gender", group_area = "icb", group_tfc = "all", banding = "fiftytwo_wks_flag")

chi_group_multi <- function(feature, group_area, group_tfc, banding) {
  
  options(dplyr.summarise.inform = FALSE)
  
  # Start with the input data frame
  chi_all <- df
  
  # Set the area column based on the group_area argument
  chi_all$area <- "National"
  if (group_area == "icb") {
    chi_all$area <- chi_all$icb_name
  }
  if (group_area == "region") {
    chi_all$area <- chi_all$region_name
  }
  
  # Set the treatment function column based on the group_tfc argument
  chi_all$tfc <- "All Treatment Functions"
  if (group_tfc == "tfc") {
    chi_all$tfc <- chi_all$monthly_mapped_rtt_reporting_tfc
  }
  
  # ----- Group and summarize data -----
  
  # Group by feature, banding flag, area, and tfc, and count occurrences
  chi_all <- chi_all |>
    # group the data by the feature selected, icb and time grouping flag
    group_by(
      !!sym(feature),
      !!sym(banding),
      area,
      tfc
    ) |>
    # create a count for that feature by above grouping then drop the grouping
    summarise(
      feature_count = sum(total_open_pathways),
      .groups = "drop"
    ) |>
    # Group by banding flag, area, and tfc
    group_by(
      !!sym(banding),
      area,
      tfc
    ) |>
    # calculate a percentage across grouping
    mutate(
      perc_feature_count = round((feature_count / sum(feature_count)) * 100, 2) # ,
      # fiftytwo_wks_flag = as.character(fiftytwo_wks_flag)
    ) |>
    # Pivot wider to create separate columns for each banding value
    pivot_wider(
      names_from = !!sym(banding),
      values_from = c(
        feature_count,
        perc_feature_count
      )
    ) |>
    # replace any NAs with zeros
    tidyr::replace_na(list(
      feature_count_0 = 0,
      feature_count_1 = 0,
      perc_feature_count_0 = 0,
      perc_feature_count_1 = 0
    ))
  
  # Add total counts and feature name
  chi_all <- chi_all |>
    group_by(
      area,
      tfc
    ) |>
    mutate(
      no_vars = n(),
      total_group_0 = sum(feature_count_0),
      total_group_1 = sum(feature_count_1),
      feature = feature
    ) |>
    rename(val = !!sym(feature))
  
  # ----- Perform chi-squared and Fisher's exact tests -----
  
  # Create a dataframe for chi-squared test results
  chi_filt <- chi_all |>
    # make a new grouping by tfc/icb or both
    group_by(
      area,
      tfc
    ) |>
    # filter any groups where there are less than 20 in 52 week waiters
    # or there is only one feature in a group (mostly all missing p codes)
    filter(
      total_group_0 > 20,
      total_group_1 > 20,
      no_vars > 1
    ) |>
    # caluclate chi square & fisher tests across the two groups
    mutate(
      feature = feature,
      chi_test = if_else(min(feature_count_1) > 5,
                         prettyNum(
                           chisq.test(matrix(
                             c(
                               feature_count_0,
                               feature_count_1
                             ),
                             ncol = 2
                           ))$p.value,
                           scientific = FALSE,
                           digits = 2
                         ),
                         prettyNum("999") # else {999}
      ),
      chi_sig = if_else(chi_test < 0.05, 1, 0),
      fisher_test = if_else(n() > 1, prettyNum(
        fisher.test(
          matrix(
            c(
              feature_count_0,
              feature_count_1
            ),
            ncol = 2
          ),
          simulate.p.value = TRUE,
          B = 10000
        )$p.value,
        scientific = FALSE,
        digits = 2
      ),
      prettyNum("999")
      ),
      fish_sig = if_else(fisher_test < 0.05, 1, 0)
    ) |>
    # simply arrange in order of p value
    arrange(chi_test) |>
    dplyr::select(area, tfc, chi_test, chi_sig, fisher_test, fish_sig) |>
    unique() |>
    ungroup()
  
  # run tests again without nulls
  chi_rm_na <- chi_all |>
    filter(!val %in% c("Not known", "Not Known")) |>
    group_by(
      area,
      tfc
    ) |>
    mutate(
      no_vars = n(),
      total_group_0 = sum(feature_count_0),
      total_group_1 = sum(feature_count_1)
    ) |>
    filter(
      total_group_0 > 20,
      total_group_1 > 20,
      no_vars > 1
    ) |>
    mutate(
      chi_test_na = if_else(min(feature_count_1) > 5 & n() > 1,
                            prettyNum(
                              chisq.test(matrix(
                                c(
                                  feature_count_0,
                                  feature_count_1
                                ),
                                ncol = 2
                              ))$p.value,
                              scientific = FALSE,
                              digits = 2
                            ),
                            prettyNum("999") # else {999}
      ),
      chi_test_na_sig = if_else(chi_test_na < 0.05, 1, 0),
      fisher_test_na = if_else(n() > 1 & sum(feature_count_1) > 5,
                               prettyNum(
                                 fisher.test(
                                   matrix(
                                     c(
                                       feature_count_0,
                                       feature_count_1
                                     ),
                                     ncol = 2
                                   ),
                                   simulate.p.value = TRUE,
                                   B = 10000
                                 )$p.value,
                                 scientific = FALSE,
                                 digits = 2
                               ),
                               prettyNum("999")
      ),
      fish_test_na_sig = if_else(fisher_test_na < 0.05, 1, 0)
    ) |>
    dplyr::select(area, tfc, chi_test_na, chi_test_na_sig, fisher_test_na, fish_test_na_sig) |>
    unique() |>
    ungroup()
  
  #  join the na chi sq results to the main results
  chi_all <- chi_all |>
    left_join(chi_filt, by = c("area", "tfc")) |>
    left_join(chi_rm_na, by = c("area", "tfc"))
  
  # return the joined table
  chi_all <- chi_all |>
    mutate(
      chi_test = ifelse(is.na(chi_test), "999", chi_test),
      chi_sig = ifelse(is.na(chi_sig), 2, chi_sig),
      fisher_test = ifelse(is.na(fisher_test), "999", fisher_test),
      fish_sig = ifelse(is.na(fish_sig), 2, fish_sig),
      chi_test_na = ifelse(is.na(chi_test_na), "999", chi_test_na),
      chi_test_na_sig = ifelse(is.na(chi_test_na_sig), 2, chi_test_na_sig),
      fisher_test_na = ifelse(is.na(fisher_test_na), "999", fisher_test_na),
      fish_test_na_sig = ifelse(is.na(fish_test_na_sig), 2, fish_test_na_sig),
      wait_compare = banding
    ) |>
    dplyr::select(
      wait_compare,
      area,
      feature,
      tfc,
      val,
      feature_count_0,
      feature_count_1,
      total_group_0,
      total_group_1,
      perc_feature_count_0,
      perc_feature_count_1,
      chi_test,
      chi_sig,
      fisher_test,
      fish_sig,
      chi_test_na,
      chi_test_na_sig,
      fisher_test_na,
      fish_test_na_sig
    ) 
  
  chi_all
}

########################
# create results table #
########################

# create us a blank dataframe to append all the results into

# columns to include
col <- c("feature",
         "area",
         "tfc",
         "val",
         "feature_count_0",
         "feature_count_1" ,
         "perc_feature_count_0",
         "perc_feature_count_1", 
         "mini",
         "chi_test" , 
         "chi_sig",
         "fisher_test" , 
         "fish_sig"  ,
         "exclude"  , 
         "chi_test_na" , 
         "chi_test_na_sig" ,  
         "fisher_test_na" ,   
         "fish_test_na_sig" )

# create table
full_table <- data.frame(matrix(ncol = length(col), nrow = 0))
colnames(full_table)<- col

#################################
# Run analysis function in loop #
#################################

# these are the features, areas, pathways and 
# waiting groups to iterate over

features <- c('procedure_priority_code',
              'ethnic_group',
              'imd',
              'age_band', 
              'gender')

areas <- c('National',
           'region', 
           'icb')

pathways <- c('All tfcs',
              'tfc')

groupings <- c('fiftytwo_wks_flag',
               'sixtyfive_wks_flag',
               'seventyeight_wks_flag',
               'onezerofour_wks_flag')

# this loopyness goes over each of the iteration types above

cli_progress_bar("Running fisher tests (ignore the ETA - go and make a cup of tea - real ETA is approx 30 mins)", total = 120)
for (i in features) {
  for (j in areas) {
    for (k in pathways) {
      for (l in groupings) {
        #print(paste('Started:',Sys.time(), '--- Feature:',i,'--- Area:', j, '--- Pathway:', k,'--- Grouping:', l))
        dat <- chi_group_multi (i, j, k, l)
        full_table <- rbind(full_table, dat)
        cli_progress_update()
      }
    }
  }
}

# results of stat tests can be saved as csv at this stage
write_csv(full_table,'full_table_240314.csv')

#################################
# calculate rates by population #
#################################

# pull icb codes back in
icb_codes <- df |>
  dplyr::select (icb_code, icb_name) |>
  unique ()

# load in populations 
# this is taken from file based on total populations that
# Evelyn runs,  this should not change significantly and
# probably needs updates yearly

mpi_pop <- read_csv("MPI_Pop_for_SWM_ST.csv")
mpi_pop <- clean_names(mpi_pop)

# load in eth_codes
eth_codes <- read_csv("eth_codes.csv")
eth_codes <- clean_names(eth_codes)

# pull just icb data
icb_data_only <- full_table |>
  left_join(icb_codes, by = c('area' =  'icb_name')) |>
  filter (feature != 'procedure_priority_code',
          !is.na(icb_code))  

# join eth codes to mpi_pop
mpi_pop <- mpi_pop |>
  mutate(val = case_when(is.na(value) ~ 'Not known',
                         value == 'Unknown'~ 'Not known',
                         value == '99'~ 'Not known',
                         value == '0'~ 'Not known',
                         TRUE ~ value)) 

# sum up groupings 
mpi_pop_eth <- mpi_pop |>
  filter(feature == 'ethnicity') |>
  left_join(eth_codes |> dplyr::select (category, main_code_text), 
            by = c('value' = 'main_code_text')) |>
  group_by(category, mpi_icb_code) |>
  summarise(eth_pop = sum(population, na.rm = TRUE))

# join ethnicity data
icb_data_only <- icb_data_only |>
  left_join(mpi_pop_eth , 
            by = c('val' = 'category', 
                   'icb_code' = 'mpi_icb_code'  ))

# gender data
mpi_pop_gender <- mpi_pop |>
  filter(feature == 'gender') |>
  mutate(gender = case_when (value == '1' ~ 'Male',
                             value == '2' ~ 'Female',
                             value == '0' ~ 'Not Specified',
                             value == '9' ~ 'Not Known',
                             TRUE ~ 'Error - please check'),
        gen_pop = population)

# join mpi pop data to base data
icb_data_only <- icb_data_only |>
  left_join(mpi_pop_gender |> dplyr::select(gender, mpi_icb_code, gen_pop) , 
            by = c('val' = 'gender', 
                   'icb_code' = 'mpi_icb_code'  ))

# age data
mpi_pop_age <- mpi_pop |>
  filter(feature == 'age') |>
  mutate(age_pop = population)

icb_data_only <- icb_data_only |>
  left_join(mpi_pop_age |> dplyr::select(value, mpi_icb_code, age_pop) , 
            by = c('val' = 'value', 
                   'icb_code' = 'mpi_icb_code'  ))

# imd data
mpi_pop_imd <- mpi_pop |>
  filter(feature == 'imd') |>
  mutate(imd_pop = population)

icb_data_only <- icb_data_only |>
  left_join(mpi_pop_imd |> dplyr::select(value, mpi_icb_code, imd_pop) , 
            by = c('val' = 'value', 
                   'icb_code' = 'mpi_icb_code'  ))

icb_data_only$population <- sum(icb_data_only$eth_pop + icb_data_only$gen_pop, na.rm = TRUE)

icb_data_only<- icb_data_only |>
  mutate(eth_pop = tidyr::replace_na(eth_pop,0),
         gen_pop =tidyr::replace_na(gen_pop,0),
         age_pop = tidyr::replace_na(age_pop,0),
         imd_pop = tidyr::replace_na(imd_pop,0),
         population = eth_pop + gen_pop + age_pop + imd_pop ) |>
  dplyr::select (!ends_with('pop'))

# calculate rates
icb_data_only <- icb_data_only |>
  mutate(rate_0 = (feature_count_0 / population) * 1000,
         rate_1 = (feature_count_1 / population) * 1000)

# calculate SDs from means across ICBs
icb_data_only <- icb_data_only |>
  group_by(val, tfc) |>
  mutate(av_mean_0 = mean (rate_0),
         up_one_sd_0 = mean (rate_0, na.rm = TRUE) + sd (rate_0, na.rm = TRUE),
         up_two_sd_0 = mean (rate_0, na.rm = TRUE) + (sd (rate_0, na.rm = TRUE))*2,
         down_one_sd_0 = mean (rate_0, na.rm = TRUE) - sd (rate_0, na.rm = TRUE),
         down_two_sd_0 = mean (rate_0, na.rm = TRUE) - (sd (rate_0, na.rm = TRUE))*2,
         av_mean_1 = mean (rate_1),
         up_one_sd_1 = mean (rate_1, na.rm = TRUE) + sd (rate_1, na.rm = TRUE),
         up_two_sd_1 = mean (rate_1, na.rm = TRUE) + (sd (rate_1, na.rm = TRUE))*2,
         down_one_sd_1 = mean (rate_1, na.rm = TRUE) - sd (rate_1, na.rm = TRUE),
         down_two_sd_1 = mean (rate_1, na.rm = TRUE) - (sd (rate_1, na.rm = TRUE))*2,
         deviant_flag_0 = case_when (rate_0 > up_two_sd_0 ~ 'Up 2',
                                     rate_0 > up_one_sd_0 ~ 'Up 1',
                                     rate_0 < down_two_sd_0 ~ 'Down 2',
                                     rate_0 < down_one_sd_0 ~ 'Down 1',
                                     TRUE ~ 'Average'),
         deviant_flag_1 = case_when (rate_1 > up_two_sd_1 ~ 'Up 2',
                                     rate_1 > up_one_sd_1 ~ 'Up 1',
                                     rate_1 < down_two_sd_1 ~ 'Down 2',
                                     rate_1 < down_one_sd_1 ~ 'Down 1',
                                     TRUE ~ 'Average'))

# pull not icb data & pri code
non_icb_data_only <- full_table |>
  left_join(icb_codes, by = c('area' =  'icb_name')) |>
  filter (feature == 'procedure_priority_code' |
            is.na(icb_code))  

# create final table
final <- rbind(icb_data_only, non_icb_data_only)


##############
# clustering #
##############

# expand dataframe to required shape for clustering

clust <- df |>
  dplyr::select(imd,
                ethnic_group,
                gender,
                age_band,
                monthly_mapped_rtt_reporting_tfc,
                icb_name,
                fiftytwo_wks_flag,
                total_open_pathways) |>
  rename(tfc = monthly_mapped_rtt_reporting_tfc)

# need to expand to patient level and a row number
clust <- data.frame(lapply(clust, rep, clust$total_open_pathways)) |>
  mutate(ID = row_number())

# convert features to factors
clust <- clust |>
  mutate(ethnic_group = as.factor(ethnic_group),
         imd = as.character(imd),
         imd = as.factor(imd),
         age_band = as.factor(age_band),
         gender = as.factor(gender))

#' Clusters patients based on demographic features and generates commentary
#' Output in form of table
#'
#' This function performs the following steps:
#' 1. Filters data for a specific ICB and treatment function code (TFC).
#' 2. Selects relevant demographic features: ethnicity group, IMD, age band, and gender.
#' 3. Converts data to a data.table for efficiency.
#' 4. One-hot encodes categorical features.
#' 5. Calculates pairwise distances using the Gower distance metric.
#' 6. Performs hierarchical clustering using complete linkage.
#' 7. Cuts the dendrogram into `k_clusters` clusters.  Default set to 5 clusters.
#' 8. Adds cluster assignment to the original dataset.
#' 9. Summarizes demographic features within each cluster.
#' 10. Rearranges columns by size of cluster.
#' 11. Generates automated commentary describing the characteristics of each cluster.
#' 12. Tidy up row and column names. 
#' 13. Suppress small numbers: 1-4 are converted to 5.
#' 14. Returns processed dataframe that can be appended to others in loop later.
#'
#' @param data A data frame containing patient data.
#' @param icb A character string specifying the ICB name to filter for.
#' @param tf_c A character string specifying the treatment function code to filter for.
#' @param k_clusters An integer specifying the number of clusters to create.
#'
#' @return A dataframe containing:
#'   - feature: String containing the feature -  the breakdown of each demographic
#'   - first: String first line contains commentary and later rows contains the numbers within the feature 
#'   - second: String first line contains commentary and later rows contains the numbers within the feature 
#'   - third: String first line contains commentary and later rows contains the numbers within the feature 
#'   - forth: String first line contains commentary and later rows contains the numbers within the feature 
#'   - fifth: String first line contains commentary and later rows contains the numbers within the feature
#'   
#' Note: the column names refer to the total number in that cluster and columns are ordered by size of cluster 
#'
#' @examples
#' cluster_func(data = clust, icb = "DEVON", tf_c = "110", k_clusters = 5)

cluster_func <- function(data, icb , tf_c, k_clusters) {
  
  options(dplyr.summarise.inform = FALSE)
  
  # filter data to icb and tfc for 52+ waits
  data <- data |>
    filter(icb_name == icb, 
           tfc == tf_c)  |>
    dplyr::select(ID, 
                  ethnic_group, 
                  imd, 
                  age_band, 
                  gender)
  
  # convert to data.table so that things are faster
  data <- as.data.table(data)
  
  # one hot encode features
  data <- one_hot(data)
  
  # calculate distance
  d_dist <- daisy(data, 
                  metric = "gower", 
                  warnType = FALSE)
  
  # hierarchical clustering
  hc <- hclust(d_dist, 
               method = "complete")
  
  # cut tree to k clusters (5)
  cluster <- cutree(hc, 
                    k=k_clusters)
  
  # add the columns with cluster info back into the main dataset
  data <- cbind(data, 
                as.factor(cluster))
  
  # create summary data per cluster
  res_sum <- data |>
    dplyr::select(-ID) |>
    group_by(V2) |>
    mutate(tot = as.character(n())) |>
    group_by(V2, tot) |>
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
    mutate(tot = as.numeric(tot)) 
  
  # transpose dataframe
  res_sum <- data.frame(t(res_sum))
  
  # convert dataframe to a matrix so that can reorder columns based to total size 
  res_sum <- as.matrix(res_sum)
  
  # reorder cols
  res_sum <- res_sum[, order(res_sum[which(rownames(res_sum) == 'tot'), ], decreasing = TRUE) ]
  
  # convert back to dataframe
  res_sum <- data.frame(res_sum)
  
  # add row names back as a column
  res_sum <- res_sum |> rownames_to_column(var = 'feature')
  
  # convert it all back to numeric
  res_sum$X1 <- as.numeric(res_sum$X1 )
  res_sum$X2 <- as.numeric(res_sum$X2 )
  res_sum$X3 <- as.numeric(res_sum$X3 )
  res_sum$X4 <- as.numeric(res_sum$X4 )
  res_sum$X5 <- as.numeric(res_sum$X5 )
  
  # remove V2 - (the cluster)
  res_sum <- res_sum |>
    filter (!feature %in% c('V2')) #, 'tot'))
  
  # add a total column
  res_sum <- res_sum |>
    adorn_totals(where = 'col')
  
  # rename columns based on their position - good old base r to the rescue
  names(res_sum)[2] <- 'First'
  names(res_sum)[3] <- 'Second'
  names(res_sum)[4] <- 'Third'
  names(res_sum)[5] <- 'Forth'
  names(res_sum)[6] <- 'Fifth'
  
  # add groups to features 
  res_sum <- res_sum |>
    mutate(grp = str_sub(feature,1,3),
           grp_l = case_when(grp == 'eth' ~ 'Ethnicity group',
                             grp == 'imd' ~ 'IMD group',
                             grp == 'age' ~ 'Age band',
                             grp == 'gen' ~ 'Gender',
                             TRUE ~ 'Total'))
  
  # add a max for each feature
  res_sum <- res_sum |>
    group_by(grp) |>
    mutate(across(is.numeric,  ~if_else(.x == max(.x), max(.x), 0), .names = "mx_{.col}")) |>
    mutate(across(c(2:6), ~ round(.x / sum (.x)*100,1), .names = "pc_{.col}")) |>
    ungroup() |>
    mutate(across(c(10:14), ~ rank(-.x, ties.method = "first"), .names = "rk_{.col}"))
  
  # automate commentary
  
  #tot_first
  perc<- round((res_sum$First[1] / res_sum$Total[1]) * 100 ,1)
  num_cohort <-res_sum$First[1] 
  
  #most important feature
  imp_feat_one <-  str_replace(res_sum$feature[res_sum$rk_mx_First == 2], '_', ' ')
  perc_feat_one <- round(res_sum$First[res_sum$rk_mx_First == 2] / res_sum$First[1] *100, 1)
  num_feat_one <- res_sum$First[res_sum$rk_mx_First == 2] 
  
  # second most important feature
  imp_feat_two <-  str_replace(res_sum$feature[res_sum$rk_mx_First == 3], '_', ' ')
  perc_feat_two <-  round(res_sum$First[res_sum$rk_mx_First == 3] / res_sum$First[1] *100, 1)
  num_feat_two <- res_sum$First[res_sum$rk_mx_First == 3] 
  
  # third most important feature
  imp_feat_thr <-  str_replace(res_sum$feature[res_sum$rk_mx_First == 4], '_', ' ')
  perc_feat_thr <-  round(res_sum$First[res_sum$rk_mx_First == 4] / res_sum$First[1] *100, 1)
  num_feat_thr <- res_sum$First[res_sum$rk_mx_First == 4] 
  
  # significance of lower imds in g
  low_imd_sig <- res_sum$pc_First[res_sum$feature == 'imd_1-2'] + res_sum$pc_First[res_sum$feature == 'imd_3-4'] 
  
  # significance of higher imds
  hi_imd_sig <- res_sum$pc_First[res_sum$feature == 'imd_7-8'] + res_sum$pc_First[res_sum$feature == 'imd_9-10'] 
  
  imd_fac <- case_when (low_imd_sig > 55 ~  'there is a higher level of  <b>more </b> deprived people in this cluster',
                        hi_imd_sig > 55 ~ 'there is a higher level of  <b>least </b> deprived people in this cluster',
                        TRUE ~ 'deprivation is not a strong feature within this cluster')
  
  
  clust_one <-glue('</b>This largest cluster of  <b>{num_cohort} </b> patients makes up  <b>{perc}%</b> of the overall cohort.<br>  
       <span style="background-color:#E8EDEE;">The most significant feature of this cluster is  <b>{imp_feat_one} </b> of  <b>{num_feat_one} </b> people which makes up  <b>{perc_feat_one}% </b> of this cohort.</span><br>
       The next most significant feature of <b>{num_feat_two} </b> is  <b>{imp_feat_two}</b> patients which make up  <b>{perc_feat_two}% </b> of this cluster.<br>
       <span style="background-color:#E8EDEE;">Not as significant for this cluster but still a factor is  <b>{imp_feat_thr} </b> of  <b>{num_feat_thr} </b> which makes up  <b>{perc_feat_thr}% </b> of this group.</span><br>
       We can observe {imd_fac}.')
  
  # commentary 2
  
  #tot_first
  perc<- round((res_sum$Second[1] / res_sum$Total[1]) * 100 ,1)
  num_cohort <-res_sum$Second[1] 
  
  #most important feature
  imp_feat_one <-  str_replace(res_sum$feature[res_sum$rk_mx_Second == 2], '_', ' ')
  perc_feat_one <- round(res_sum$Second[res_sum$rk_mx_Second == 2] / res_sum$Second[1] *100, 1)
  num_feat_one <- res_sum$Second[res_sum$rk_mx_Second == 2] 
  
  # second most important feature
  imp_feat_two <-  str_replace(res_sum$feature[res_sum$rk_mx_Second == 3], '_', ' ')
  perc_feat_two <-  round(res_sum$Second[res_sum$rk_mx_Second == 3] / res_sum$Second[1] *100, 1)
  num_feat_two <- res_sum$Second[res_sum$rk_mx_Second == 3] 
  
  # third most important feature
  imp_feat_thr <-  str_replace(res_sum$feature[res_sum$rk_mx_Second == 4], '_', ' ')
  perc_feat_thr <-  round(res_sum$Second[res_sum$rk_mx_Second == 4] / res_sum$Second[1] *100, 1)
  num_feat_thr <- res_sum$Second[res_sum$rk_mx_Second == 4] 
  
  # significance of lower imds in g
  low_imd_sig <- res_sum$pc_Second[res_sum$feature == 'imd_1-2'] + res_sum$pc_Second[res_sum$feature == 'imd_3-4'] 
  
  # significance of higher imds
  hi_imd_sig <- res_sum$pc_Second[res_sum$feature == 'imd_7-8'] + res_sum$pc_Second[res_sum$feature == 'imd_9-10'] 
  
  imd_fac <- case_when (low_imd_sig > 55 ~  'there is a higher level of  <b>more </b> deprived people in this cluster',
                        hi_imd_sig > 55 ~ 'there is a higher level of  <b>least </b> deprived people in this cluster',
                        TRUE ~ 'deprivation is not a strong feature within this cluster')
  
  
  clust_two <-glue('</b>This second largest cluster of  <b>{num_cohort} </b> patients makes up  <b>{perc}%</b> of the overall cohort.<br>  
       <span style="background-color:#E8EDEE;">The most significant feature of this cluster is  <b>{imp_feat_one} </b> of  <b>{num_feat_one} </b> people which makes up  <b>{perc_feat_one}% </b> of this cohort.</span><br>
       The next most significant feature of <b>{num_feat_two} </b> is  <b>{imp_feat_two}</b> patients which make up  <b>{perc_feat_two}% </b> of this cluster.<br>
       <span style="background-color:#E8EDEE;">Not as significant for this cluster but still a factor is  <b>{imp_feat_thr} </b> of  <b>{num_feat_thr} </b> which makes up  <b>{perc_feat_thr}% </b> of this group.</span><br>
       We can observe {imd_fac}.')
  
  # commentary 3
  #tot_Third
  perc<- round((res_sum$Third[1] / res_sum$Total[1]) * 100 ,1)
  num_cohort <-res_sum$Third[1] 
  
  #most important feature
  imp_feat_one <-  str_replace(res_sum$feature[res_sum$rk_mx_Third == 2], '_', ' ')
  perc_feat_one <- round(res_sum$Third[res_sum$rk_mx_Third == 2] / res_sum$Third[1] *100, 1)
  num_feat_one <- res_sum$Third[res_sum$rk_mx_Third == 2] 
  
  # second most important feature
  imp_feat_two <-  str_replace(res_sum$feature[res_sum$rk_mx_Third == 3], '_', ' ')
  perc_feat_two <-  round(res_sum$Third[res_sum$rk_mx_Third == 3] / res_sum$Third[1] *100, 1)
  num_feat_two <- res_sum$Third[res_sum$rk_mx_Third == 3] 
  
  # third most important feature
  imp_feat_thr <-  str_replace(res_sum$feature[res_sum$rk_mx_Third == 4], '_', ' ')
  perc_feat_thr <-  round(res_sum$Third[res_sum$rk_mx_Third == 4] / res_sum$Third[1] *100, 1)
  num_feat_thr <- res_sum$Third[res_sum$rk_mx_Third == 4] 
  
  # significance of lower imds in g
  low_imd_sig <- res_sum$pc_Third[res_sum$feature == 'imd_1-2'] + res_sum$pc_Third[res_sum$feature == 'imd_3-4'] 
  
  # significance of higher imds
  hi_imd_sig <- res_sum$pc_Third[res_sum$feature == 'imd_7-8'] + res_sum$pc_Third[res_sum$feature == 'imd_9-10'] 
  
  imd_fac <- case_when (low_imd_sig > 55 ~  'there is a higher level of  <b>more </b> deprived people in this cluster',
                        hi_imd_sig > 55 ~ 'there is a higher level of  <b>least </b> deprived people in this cluster',
                        TRUE ~ 'deprivation is not a strong feature within this cluster')
  
  
  clust_three <-glue('</b>This third cluster of  <b>{num_cohort} </b> patients makes up  <b>{perc}%</b> of the overall cohort.<br>  
       <span style="background-color:#E8EDEE;">The most significant feature of this cluster is  <b>{imp_feat_one} </b> of  <b>{num_feat_one} </b> people which makes up  <b>{perc_feat_one}% </b> of this cohort.</span><br>
       The next most significant feature of <b>{num_feat_two} </b> is  <b>{imp_feat_two}</b> patients which make up  <b>{perc_feat_two}% </b> of this cluster.<br>
       <span style="background-color:#E8EDEE;">Not as significant for this cluster but still a factor is  <b>{imp_feat_thr} </b> of  <b>{num_feat_thr} </b> which makes up  <b>{perc_feat_thr}% </b> of this group.</span><br>
       We can observe {imd_fac}.')
  
  # commentary 4
  
  #tot_first
  perc<- round((res_sum$Forth[1] / res_sum$Total[1]) * 100 ,1)
  num_cohort <-res_sum$Forth[1] 
  
  #most important feature
  imp_feat_one <-  str_replace(res_sum$feature[res_sum$rk_mx_Forth == 2], '_', ' ')
  perc_feat_one <- round(res_sum$Forth[res_sum$rk_mx_Forth == 2] / res_sum$Forth[1] *100, 1)
  num_feat_one <- res_sum$Forth[res_sum$rk_mx_Forth == 2] 
  
  # second most important feature
  imp_feat_two <-  str_replace(res_sum$feature[res_sum$rk_mx_Forth == 3], '_', ' ')
  perc_feat_two <-  round(res_sum$Forth[res_sum$rk_mx_Forth == 3] / res_sum$Forth[1] *100, 1)
  num_feat_two <- res_sum$Forth[res_sum$rk_mx_Forth == 3] 
  
  # third most important feature
  imp_feat_thr <-  str_replace(res_sum$feature[res_sum$rk_mx_Forth == 4], '_', ' ')
  perc_feat_thr <-  round(res_sum$Forth[res_sum$rk_mx_Forth == 4] / res_sum$Forth[1] *100, 1)
  num_feat_thr <- res_sum$Forth[res_sum$rk_mx_Forth == 4] 
  
  # significance of lower imds in g
  low_imd_sig <- res_sum$pc_Forth[res_sum$feature == 'imd_1-2'] + res_sum$pc_Forth[res_sum$feature == 'imd_3-4'] 
  
  # significance of higher imds
  hi_imd_sig <- res_sum$pc_Forth[res_sum$feature == 'imd_7-8'] + res_sum$pc_Forth[res_sum$feature == 'imd_9-10'] 
  
  imd_fac <- case_when (low_imd_sig > 55 ~  'there is a higher level of  <b>more </b> deprived people in this cluster',
                        hi_imd_sig > 55 ~ 'there is a higher level of  <b>least </b> deprived people in this cluster',
                        TRUE ~ 'deprivation is not a strong feature within this cluster')
  
  clust_four <-glue('</b>This second smallest cluster of  <b>{num_cohort} </b> patients makes up  <b>{perc}%</b> of the overall cohort.<br>  
       <span style="background-color:#E8EDEE;">The most significant feature of this cluster is  <b>{imp_feat_one} </b> of  <b>{num_feat_one} </b> people which makes up  <b>{perc_feat_one}% </b> of this cohort.</span><br>
       The next most significant feature of <b>{num_feat_two} </b> is  <b>{imp_feat_two}</b> patients which make up  <b>{perc_feat_two}% </b> of this cluster.<br>
       <span style="background-color:#E8EDEE;">Not as significant for this cluster but still a factor is  <b>{imp_feat_thr} </b> of  <b>{num_feat_thr} </b> which makes up  <b>{perc_feat_thr}% </b> of this group.</span><br>
       We can observe {imd_fac}.')
  
  # commentary 5
  
  #tot_first
  perc<- round((res_sum$Fifth[1] / res_sum$Total[1]) * 100 ,1)
  num_cohort <-res_sum$Fifth[1] 
  
  #most important feature
  imp_feat_one <-  str_replace(res_sum$feature[res_sum$rk_mx_Fifth == 2], '_', ' ')
  perc_feat_one <- round(res_sum$Fifth[res_sum$rk_mx_Fifth == 2] / res_sum$Fifth[1] *100, 1)
  num_feat_one <- res_sum$Fifth[res_sum$rk_mx_Fifth == 2] 
  
  # second most important feature
  imp_feat_two <-  str_replace(res_sum$feature[res_sum$rk_mx_Fifth == 3], '_', ' ')
  perc_feat_two <-  round(res_sum$Fifth[res_sum$rk_mx_Fifth == 3] / res_sum$Fifth[1] *100, 1)
  num_feat_two <- res_sum$Fifth[res_sum$rk_mx_Fifth == 3] 
  
  # third most important feature
  imp_feat_thr <-  str_replace(res_sum$feature[res_sum$rk_mx_Fifth == 4], '_', ' ')
  perc_feat_thr <-  round(res_sum$Fifth[res_sum$rk_mx_Fifth == 4] / res_sum$Fifth[1] *100, 1)
  num_feat_thr <- res_sum$Fifth[res_sum$rk_mx_Fifth == 4] 
  
  # significance of lower imds in g
  low_imd_sig <- res_sum$pc_Fifth[res_sum$feature == 'imd_1-2'] + res_sum$pc_Fifth[res_sum$feature == 'imd_3-4'] 
  
  # significance of higher imds
  hi_imd_sig <- res_sum$pc_Fifth[res_sum$feature == 'imd_7-8'] + res_sum$pc_Fifth[res_sum$feature == 'imd_9-10'] 
  
  imd_fac <- case_when (low_imd_sig > 55 ~  'there is a higher level of  <b>more </b> deprived people in this cluster',
                        hi_imd_sig > 55 ~ 'there is a higher level of  <b>least </b> deprived people in this cluster',
                        TRUE ~ 'deprivation is not a strong feature within this cluster')
  
  clust_five <-glue('</b>This smallest cluster of  <b>{num_cohort} </b> patients makes up  <b>{perc}%</b> of the overall cohort.<br>  
       <span style="background-color:#E8EDEE;">The most significant feature of this cluster is  <b>{imp_feat_one} </b> of  <b>{num_feat_one} </b> people which makes up  <b>{perc_feat_one}% </b> of this cohort.</span><br>
       The next most significant feature of <b>{num_feat_two} </b> is  <b>{imp_feat_two}</b> patients which make up  <b>{perc_feat_two}% </b> of this cluster.<br>
       <span style="background-color:#E8EDEE;">Not as significant for this cluster but still a factor is  <b>{imp_feat_thr} </b> of  <b>{num_feat_thr} </b> which makes up  <b>{perc_feat_thr}% </b> of this group.</span><br>
       We can observe {imd_fac}.')
  
  # create a dataframe with all the clustering commentary
  cluster_com <- data.frame(feature = 'Commentary', 
                            First = clust_one,
                            Second = clust_two,
                            Third = clust_three,
                            Forth = clust_four,
                            Fifth = clust_five)
  
  # select the dataframe in the correct order
  res_com <- res_sum |>
    dplyr::select (feature,
                   First,
                   Second,
                   Third,
                   Forth,
                   Fifth)
  
  # remove not knowns from analysis - also tidy up feature names
  res <- res_com |>
    filter(!feature %in% c('Commentary',
                           'ethnic_group_Not known',
                           'ethnic_group_Not known',
                           'imd_Not known', 'age_band_Not Known',
                           'gender_Not Known', 'imd_Error')) |>
    mutate(feature = case_when (feature == 'tot' ~ 'Cluster total',
                                feature == 'ethnic_group_Asian or Asian British' ~ 'Ethnic group: Asian or Asian British',
                                feature == 'ethnic_group_Black or Black British' ~ 'Ethnic group: Black or Black British',
                                feature == 'ethnic_group_Mixed' ~ 'Ethnic group: Mixed',
                                feature == 'ethnic_group_Other ethnic groups' ~ 'Ethnic group: Other ethnic groups',
                                feature == 'ethnic_group_Not Stated' ~ 'Ethnic group: Not stated',
                                feature == 'ethnic_group_White' ~ 'Ethnic group: White',
                                feature == 'imd_1-2' ~ 'IMD 1-2',
                                feature == 'imd_3-4' ~ 'IMD 3-4',
                                feature == 'imd_5-6' ~ 'IMD 5-6',
                                feature == 'imd_7-8' ~ 'IMD 7-8',
                                feature == 'imd_9-10' ~ 'IMD 9-10',
                                feature == 'age_band_0 - 15' ~ 'Age band: 0-15',
                                feature == 'age_band_16 - 18' ~ 'Age band: 16-18',
                                feature == 'age_band_19 - 44' ~ 'Age band: 19-44',
                                feature == 'age_band_45 - 64' ~ 'Age band: 45-64',
                                feature == 'age_band_65 - 79' ~ 'Age band: 65-79',
                                feature == 'age_band_80+' ~ 'Age band: 80+',
                                feature == 'gender_Female' ~ 'Gender: Female',
                                feature == 'gender_Male' ~ 'Gender: Male',
                                feature == 'gender_Not Specified' ~ 'Gender: Not specified',
                                TRUE ~ paste0('Error - please check: ', feature)
    )) 
  
  # small number suppression - convert numbers between 1-4 to 5s
  res <- res |>
    mutate(First = if_else(between(First,1,4),5, First),
           Second = if_else(between(Second,1,4),5, Second),
           Third = if_else(between(Third,1,4),5, Third),
           Forth = if_else(between(Forth,1,4),5, Forth),
           Fifth = if_else(between(Fifth,1,4),5, Fifth))  
  
  # add the commentary to the final numbers
  res <- rbind(cluster_com, res) |>
    mutate(icb_name = icb,
           tfc = tf_c)
  
  # return the final data frame   
  return (res)
}

#icbs <- c('NHS DORSET INTEGRATED CARE BOARD', 'NHS WEST YORKSHIRE INTEGRATED CARE BOARD')
#tfcs <- c('110','320')

# filter to 52+
# group imds into qunitiles
clust <- clust |>
  filter(fiftytwo_wks_flag == 1) |>
  mutate (imd = case_when (imd %in% c('1','2') ~ '1-2',
                           imd %in% c('3','4') ~ '3-4',
                           imd %in% c('5','6') ~ '5-6',
                           imd %in% c('7','8') ~ '7-8',
                           imd %in% c('9','10') ~ '9-10',
                           TRUE ~ 'Error'),
          imd = as.factor(imd),
          icb_name = str_replace(icb_name, " INTEGRATED CARE BOARD", ""),
          icb_name = str_replace(icb_name, "NHS ", "")) 

icbs <- unique(clust$icb_name)
tfcs <- unique(clust$tfc)

# create blank dataframe to put data into
clustered_table <- data.frame(feature = as.character(),
                              First = as.character(),
                              Second = as.character(),
                              Third = as.character(),
                              Forth = as.character(),
                              Fifth = as.character(),
                              icb_name = as.character(),
                              tfc = as.numeric()
)
library(cli)
# loop through iterations to create clustering table

cli_progress_bar("Clustering data", total = 966)
for (i in icbs) {
  for (j in tfcs) {
    cli_progress_update()
    # print(paste('ICB:',i,'--- Pathway:', j))
    if (nrow(clust |> filter(icb_name == i, tfc == j)) > 20) 
    {temp <- cluster_func(clust, i, j, 5)} 
    else 
      {temp <-  c('Not enough patients to cluster in this grouping','','','','','',i,j)}
  
    clustered_table <- rbind(clustered_table, temp)
  }
}

# in case of crash in clustering use
# print(paste('ICB:',i,'--- Pathway:', j))
# to see which combo caused the error 


##########################
# Prepare data for shiny #
##########################

dat <- final |>
  mutate(area = if_else (area == 'National', '.NATIONAL', area)) |>
  select (!c(chi_test, chi_sig, fisher_test, chi_test_na, chi_test_na_sig, fisher_test_na))

dat_cov <- dat
dat_sum <- dat

####################
# comparison table #
####################

dat <- dat |>
  mutate(
    control = paste0(
      perc_feature_count_0,
      "% (", feature_count_0, "/",
      total_group_0, ")"
    ),
    long_waiters = paste0(
      perc_feature_count_1,
      "% (", feature_count_1,
      "/", total_group_1, ")"
    ),
    change = round(perc_feature_count_1 - perc_feature_count_0, 1),
    difference = case_when(
      (fish_sig == 1 & fish_test_na_sig == 1) ~ "Statistically significant difference (P<0.05)",
      (fish_sig == 0 & fish_test_na_sig == 0) ~ "No significant difference",
      TRUE ~ "Cohort too small or DQ issues"
    ),
    wait_compare = case_when(
      wait_compare == "fiftytwo_wks_flag" ~ "52+ weeks",
      wait_compare == "sixtyfive_wks_flag" ~ "65+ weeks",
      wait_compare == "seventyeight_wks_flag" ~ "75+ weeks",
      wait_compare == "onezerofour_wks_flag" ~ "104+ weeks",
      TRUE ~ "Other"
    ),
    area = str_replace(area, " INTEGRATED CARE BOARD", ""),
    area = str_replace(area, "NHS ", "")
  ) |>
  dplyr::select(
    wait_compare, tfc, feature, val, control, perc_feature_count_0, deviant_flag_0,
    long_waiters,
    perc_feature_count_1, deviant_flag_1,
    change, difference, area
  ) |>
  mutate(
    deviant_flag_0 = case_when(
      deviant_flag_0 == "Average" ~ "Rate within average levels by population",
      deviant_flag_0 == "Up 1" ~ "Rate is higher than 70% of ICBs by population",
      deviant_flag_0 == "Up 2" ~ "Rate is higher than 95% of ICBs by population",
      deviant_flag_0 == "Down 1" ~ "Rate is lower than 70% of ICBs by population",
      deviant_flag_0 == "Down 2" ~ "Rate is lower than 95% of ICBs by population",
      TRUE ~ "Not applicable"
    ),
    deviant_flag_1 = case_when(
      deviant_flag_1 == "Average" ~ "Rate within average levels by population",
      deviant_flag_1 == "Up 1" ~ "Rate is higher than 70% of ICBs by population",
      deviant_flag_1 == "Up 2" ~ "Rate is higher than 95% of ICBs by population",
      deviant_flag_1 == "Down 1" ~ "Rate is lower than 70% of ICBs by population",
      deviant_flag_1 == "Down 2" ~ "Rate is lower than 95% of ICBs by population",
      TRUE ~ "Not applicable"
    )
  ) |>
  rename(
    `control bar` = perc_feature_count_0,
    `waiters bar` = perc_feature_count_1,
    `control rate` = deviant_flag_0,
    `waiters rate` = deviant_flag_1
  )

dat_cov <- dat_cov |>
  filter(wait_compare == "fiftytwo_wks_flag") |>
  mutate(
    feat_tot = feature_count_0 + feature_count_1,
    total = total_group_0 + total_group_1,
    perc = round((feat_tot / total) * 100, 1),
    colour_plot = if_else(val == "Not known", "#ED8B00", "#0072CE"),
    area = str_replace(area, " INTEGRATED CARE BOARD", ""),
    area = str_replace(area, "NHS ", ""),
    lab = paste0(perc, "% (", prettyNum(feat_tot, big.mark = ","), "/", prettyNum(total, big.mark = ","), ")")
  )


######################
# summary front page #
######################

df_sum <- dat_sum |>  
  filter( !startsWith(tfc,'X'))  |>
  mutate(bot_sig = fish_sig + fish_test_na_sig, # count as significant if both tests significant
         bot_chk = if_else(bot_sig == 2, 1, 0), # creates a flag for significant 
         area = str_replace(area, " INTEGRATED CARE BOARD", ""),  # shorten icb names
         area = str_replace(area, "NHS ", "")) |>
  group_by (wait_compare,   
            area,
            feature,
            tfc) |>
  summarise (sig =mean(bot_chk)) |>   
  ungroup() |>
  pivot_wider(names_from = feature,
              values_from = sig) |> #  create a row of significance per feature
  rowwise()|>
  mutate (total_diff = sum(ethnic_group, gender, age_band, imd, na.rm = TRUE),   # create a total score of significnace by tfc
          eth_sig = if_else (ethnic_group == 1 , "E", "-"),   # convert flag into text
          gen_sig = if_else (gender == 1 , "G", "-"),
          age_sig = if_else (age_band == 1 , "A", "-"),
          imd_sig = if_else (imd == 1 , "I", "-"),
          tot_sig = paste(age_sig, gen_sig, eth_sig, imd_sig),  # create final string of text 
          icon_sig = case_when(total_diff == 1 ~ 'user',   # used this for icons but not sure if will be in final
                               total_diff == 2 ~ 'user-group',
                               total_diff == 3 ~ 'users',
                               total_diff == 4 ~ 'people-group',
                               is.na(total_diff) ~ 'circle',
                               TRUE ~ 'circle')) |>
  ungroup() |>
  #group_by(wait_compare, area) |>
  mutate (eth_tot = sum (ethnic_group, na.rm = TRUE), 
          gen_tot = sum (gender, na.rm = TRUE), 
          age_tot = sum (age_band, na.rm = TRUE), 
          imd_tot = sum (imd, na.rm = TRUE), .by = c(wait_compare, area)) |>
  dplyr::select(wait_compare,
                area,
                tfc,
                tot_sig,
                total_diff,
                eth_tot,
                gen_tot,
                age_tot,
                imd_tot) |>
  pivot_wider (names_from = tfc,
               values_from = c(tot_sig, total_diff)) |>
  mutate(region = case_when (area == '.NATIONAL' ~ '.NATIONAL',
                             area == 'EAST OF ENGLAND' ~ '.NATIONAL',
                             area == 'LONDON' ~ '.NATIONAL',
                             area == 'MIDLANDS' ~ '.NATIONAL',
                             area == 'NORTH EAST AND YORKSHIRE' ~ '.NATIONAL',
                             area == 'NORTH WEST' ~ '.NATIONAL',
                             area == 'SOUTH EAST' ~ '.NATIONAL',
                             area == 'SOUTH WEST' ~ '.NATIONAL',
                             area == 'WALES REGION' ~ '.NATIONAL',
                             area == 'BEDFORDSHIRE, LUTON AND MILTON KEYNES' ~ 'EAST OF ENGLAND',
                             area == 'CAMBRIDGESHIRE AND PETERBOROUGH' ~ 'EAST OF ENGLAND',
                             area == 'HERTFORDSHIRE AND WEST ESSEX' ~ 'EAST OF ENGLAND',
                             area == 'MID AND SOUTH ESSEX' ~ 'EAST OF ENGLAND',
                             area == 'NORFOLK AND WAVENEY' ~ 'EAST OF ENGLAND',
                             area == 'SUFFOLK AND NORTH EAST ESSEX' ~ 'EAST OF ENGLAND',
                             area == 'NORTH CENTRAL LONDON' ~ 'LONDON',
                             area == 'NORTH EAST LONDON' ~ 'LONDON',
                             area == 'NORTH WEST LONDON' ~ 'LONDON',
                             area == 'SOUTH EAST LONDON' ~ 'LONDON',
                             area == 'SOUTH WEST LONDON' ~ 'LONDON',
                             area == 'BIRMINGHAM AND SOLIHULL' ~ 'MIDLANDS',
                             area == 'BLACK COUNTRY' ~ 'MIDLANDS',
                             area == 'COVENTRY AND WARWICKSHIRE' ~ 'MIDLANDS',
                             area == 'DERBY AND DERBYSHIRE' ~ 'MIDLANDS',
                             area == 'HEREFORDSHIRE AND WORCESTERSHIRE' ~ 'MIDLANDS',
                             area == 'LEICESTER, LEICESTERSHIRE AND RUTLAND' ~ 'MIDLANDS',
                             area == 'LINCOLNSHIRE' ~ 'MIDLANDS',
                             area == 'NORTHAMPTONSHIRE' ~ 'MIDLANDS',
                             area == 'NOTTINGHAM AND NOTTINGHAMSHIRE' ~ 'MIDLANDS',
                             area == 'SHROPSHIRE, TELFORD AND WREKIN' ~ 'MIDLANDS',
                             area == 'STAFFORDSHIRE AND STOKE-ON-TRENT' ~ 'MIDLANDS',
                             area == 'HUMBER AND NORTH YORKSHIRE' ~ 'NORTH EAST AND YORKSHIRE',
                             area == 'NORTH EAST AND NORTH CUMBRIA' ~ 'NORTH EAST AND YORKSHIRE',
                             area == 'SOUTH YORKSHIRE' ~ 'NORTH EAST AND YORKSHIRE',
                             area == 'WEST YORKSHIRE' ~ 'NORTH EAST AND YORKSHIRE',
                             area == 'CHESHIRE AND MERSEYSIDE' ~ 'NORTH WEST',
                             area == 'GREATER MANCHESTER' ~ 'NORTH WEST',
                             area == 'LANCASHIRE AND SOUTH CUMBRIA' ~ 'NORTH WEST',
                             area == 'BUCKINGHAMSHIRE, OXFORDSHIRE AND BERKSHIRE WEST' ~ 'SOUTH EAST',
                             area == 'FRIMLEY' ~ 'SOUTH EAST',
                             area == 'HAMPSHIRE AND ISLE OF WIGHT' ~ 'SOUTH EAST',
                             area == 'KENT AND MEDWAY' ~ 'SOUTH EAST',
                             area == 'SURREY HEARTLANDS' ~ 'SOUTH EAST',
                             area == 'SUSSEX' ~ 'SOUTH EAST',
                             area == 'BATH AND NORTH EAST SOMERSET, SWINDON AND WILTSHIRE' ~ 'SOUTH WEST',
                             area == 'BRISTOL, NORTH SOMERSET AND SOUTH GLOUCESTERSHIRE' ~ 'SOUTH WEST',
                             area == 'CORNWALL AND THE ISLES OF SCILLY' ~ 'SOUTH WEST',
                             area == 'DEVON' ~ 'SOUTH WEST',
                             area == 'DORSET' ~ 'SOUTH WEST',
                             area == 'GLOUCESTERSHIRE' ~ 'SOUTH WEST',
                             area == 'SOMERSET' ~ 'SOUTH WEST',
                             TRUE ~ 'OTHER'),
         wait_compare = case_when(
           wait_compare == "fiftytwo_wks_flag" ~ "52+ weeks",
           wait_compare == "sixtyfive_wks_flag" ~ "65+ weeks",
           wait_compare == "seventyeight_wks_flag" ~ "75+ weeks",
           wait_compare == "onezerofour_wks_flag" ~ "104+ weeks",
           TRUE ~ "Other"
         )
  ) |>
  
  # test <- df_sum |>   
  
  mutate_at(vars(matches("total_diff")), ~case_when (.x == '1' ~ "#ADD8E6",
                                                     .x  == '2' ~ "#41B6E6",
                                                     .x  == '3' ~ "#0072CE",
                                                     .x  == '4' ~ "#003087",
                                                     TRUE ~ "#FFFFFF")) |> 
  dplyr::select (area,
                 tot_sig_100,
                 tot_sig_101,
                 tot_sig_110,
                 tot_sig_120,
                 tot_sig_130,
                 tot_sig_140,
                 tot_sig_150,
                 tot_sig_160,
                 tot_sig_170,
                 tot_sig_300,
                 tot_sig_301,
                 tot_sig_320,
                 tot_sig_330,
                 tot_sig_340,
                 tot_sig_400,
                 tot_sig_410,
                 tot_sig_430,
                 tot_sig_502,
                 `tot_sig_All Treatment Functions`,
                 age_tot,
                 gen_tot,
                 eth_tot,
                 imd_tot,
                 everything())

####################
# save Rdata files #
####################
# 
# saveRDS(dat_sum,'full_datab.Rdata')
# saveRDS(dat_cov,'cover_datab.Rdata')
# saveRDS(df_sum,'sum_datab.Rdata')
# saveRDS(dat,'dat_datab.Rdata')
# saveRDS(clustered_table,'clustered_table.Rdata')
# 
