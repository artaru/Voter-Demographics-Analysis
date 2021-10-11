library(dplyr)
ces19 <- readRDS("ces19.rds")

# A vector containing the column names of the predictor variables.
predictor_variables <- c("age", "gender", "province", "sexuality",
                         "religion", "marital", "education", "union", "children",
                         "groups_therm_1", "groups_therm_2", "groups_therm_3",
                         "groups_therm_4", "groups_therm_5", "interest_gen_1",
                         "interest_elxn_1", "spend_educ", "spend_env",
                         "spend_just_law", "spend_defence", "spend_imm_min",
                         "econ_retro", "econ_fed", "ownfinanc_fed", "imm", "refugees",
                         "govt_confusing", "govt_say", "bornin_canada", "lib_promises")
feature_and_predictor_variables <- predictor_variables %>% append("votechoice")

ces15 <- read_csv("CES2015_Combined_CSVstrings.csv") %>%
  # Convert birthyear to age
  mutate(age=year - as.double(age)) %>%
  # Convert vote_for variable to votechoice factor
  filter(vote_for == "Bloc Quebecois" |
           vote_for == "Conservatives" |
           vote_for == "Liberal" |
           vote_for == "Green Party" |
           vote_for == "ndp" |
           grepl("don't know", vote_for)) %>%
  mutate(votechoice=case_when(
    vote_for == "Bloc Quebecois" ~ "Bloc Quebecois",
    vote_for == "Conservatives" ~ "Conservative Party",
    vote_for == "Liberal" ~ "Liberal Party",
    vote_for == "Green Party" ~ "Green Party",
    vote_for == "ndp" ~ "ndp",
    grepl("don't know", vote_for) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$votechoice))) %>%
  # Convert sex_r to gender factor
  mutate(gender=case_when(
    sex_r == "Male" ~ "A man",
    sex_r == "Female" ~ "A woman",
    is.na(sex_r) ~ "Other (e.g. Trans, non-binary, two-spirit, gender-queer)"
  ) %>% factor(levels=levels(ces19$gender))) %>%
  # Convert province to factor
  mutate(province=case_when(
    province == "bc" ~ "British Columbia",
    province == "nb" ~ "New Brunswick",
    province == "Nfld" ~ "Newfoundland and Labrador",
    province == "ns" ~ "Nova Scotia",
    province == "nwt" ~ "Northwest Territories",
    province == "pei" ~ "Prince Edward Island",
    province == "Sask" ~ "Saskatchewan",
    TRUE ~ province,
  ) %>% factor(levels=levels(ces19$province))) %>%
  # Add sexuality column (not present in 2015 data, so we will set it to "Prefer not to say")
  mutate(sexuality="Prefer not to say" %>% factor(levels=levels(ces19$sexuality))) %>%
  # Convert religion to factor
  mutate(religion=case_when(
    religion == "none, don't have one/ Atheist" ~ "None/ Don't have one/ Atheist",
    religion == "don't know/ Agnostic" ~ "Agnostic",
    religion == "Buddhist/Buddhism" ~ "Buddhist/ Buddhism",
    religion == "Hindu" ~ "Hindu",
    religion == "Jewish/Judaism/Jewish Orthodox" ~ "Jewish/ Judaism/ Jewish Orthodox",
    religion == "Muslim/ Islam" ~ "Muslim/ Islam",
    religion == "Sikh/ Sikhism" ~ "Sikh/ Sikhism",
    religion == "Anglican/Church of England" ~ "Anglican/ Church of England",
    religion == "Baptist" ~ "Baptist",
    religion == "Catholic/Roman Catholic/RC" ~ "Catholic/ Roman Catholic/ RC",
    religion == "Greek/Ukrainian/Russian Orthodox" ~ "Greek Orthodox/ Ukrainian Orthodox/ Russian Orthodox/ Eastern Orthodox",
    religion == "Jehovah's Witness" ~ "Jehovah's Witness",
    religion == "Lutheran" ~ "Lutheran",
    religion == "Mormon/Church of Latter Day Saints" ~ "Mormon/ Church of Jesus Christ of the Latter Day Saints",
    religion == "Pentecostal/Fundamentalist/Born Again" ~ "Pentecostal/ Fundamentalist/ Born Again/ Evangelical",
    religion == "Presbyterian" ~ "Presbyterian",
    religion == "Protestant (only after probe)" ~ "Protestant",
    religion == "United Church of Canada" ~ "United Church of Canada",
    religion == "Christian Reform" ~ "Christian Reformed",
    religion == "Salvation Army" ~ "Salvation Army",
    religion == "Mennonite" ~ "Mennonite",
    religion == "other (specify)" ~ "Other (please specify)",
    religion == "refused" | is.na(religion) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$religion))) %>%
  # Convert p_married to marital factor
  mutate(marital=case_when(
    p_married == "Never married" ~ "Never Married",
    p_married %in% c("Don't know", "Refused") | is.na(p_married) ~ "Don't know/ Prefer not to answer",
    TRUE ~ p_married
  ) %>% factor(levels=levels(ces19$marital))) %>%
  # Convert education to factor
  mutate(education=case_when(
    education == "no schooling" ~ "No schooling",
    education == "some elementary school" ~ "Some elementary school",
    education == "completed elementary school" ~ "Completed elementary school",
    education == "some secondary / high school" ~ "Some secondary/ high school",
    education == "completed secondary / high school" ~ "Completed secondary/ high school",
    education == "some technical, community college" ~ "Some technical, community college, CEGEP, College Classique",
    education == "completed technical, community college" ~ "Completed technical, community college, CEGEP, College Classique",
    education == "some university" ~ "Some university",
    education == "bachelor's degree" ~ "Bachelor's degree",
    education == "master's degree" ~ "Master's degree",
    education == "professional degree or doctorate" ~ "Professional degree or doctorate",
    education %in% c("don't know", "refused") | is.na(education) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$education))) %>%
  # Convert p_union to factor
  mutate(union=case_when(
    p_union %in% c("Don't know", "Refused") | is.na(p_union) ~ "Don't know/ Prefer not to say",
    TRUE ~ p_union
  ) %>% factor(levels=levels(ces19$union))) %>%
  # Convert kids column to children
  mutate(children=case_when(
    # The data has some outliers (such as kids == 999)...We remove them by setting a hard cap of 10 children.
    as.numeric(kids) >= 1 & as.numeric(kids) <= 10 ~ "Yes",
    TRUE ~ "No"
  ) %>% factor(levels=levels(ces19$children))) %>%
  # Convert spnd_educ to spend_educ factor
  mutate(spend_educ=case_when(
    spnd_educ == "spend less" ~ "Spend less",
    spnd_educ == "about the same as now" ~ "Spend about the same as now",
    spnd_educ == "spend more" ~ "Spend more",
    spnd_educ %in% c("refused", "don't know") | is.na(spnd_educ) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$spend_educ))) %>%
  # Convert spnd_envi to spend_env factor
  mutate(spend_env=case_when(
    spnd_envi == "spend less" ~ "Spend less",
    spnd_envi == "about the same as now" ~ "Spend about the same as now",
    spnd_envi == "spend more" ~ "Spend more",
    spnd_envi %in% c("refused", "don't know") | is.na(spnd_envi) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$spend_env))) %>%
  # Convert spnd_crim to spend_just_law
  mutate(spend_just_law=case_when(
    spnd_crim == "spend less" ~ "Spend less",
    spnd_crim == "about the same as now" ~ "Spend about the same as now",
    spnd_crim == "spend more" ~ "Spend more",
    spnd_crim %in% c("refused", "don't know") | is.na(spnd_crim) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$spend_just_law))) %>%
  # Convert spnd_defn to spend_defence
  mutate(spend_defence=case_when(
    spnd_defn == "spend less" ~ "Spend less",
    spnd_defn == "about the same as now" ~ "Spend about the same as now",
    spnd_defn == "spend more" ~ "Spend more",
    spnd_defn %in% c("refused", "don't know") | is.na(spnd_defn) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$spend_defence))) %>%
  # Convert spnd_immg to spend_imm_min
  mutate(spend_imm_min=case_when(
    spnd_immg == "spend less" ~ "Spend less",
    spnd_immg == "about the same as now" ~ "Spend about the same as now",
    spnd_immg == "spend more" ~ "Spend more",
    spnd_immg %in% c("refused", "don't know") | is.na(spnd_immg) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$spend_imm_min))) %>%
  # Convert econ_ret to econ_retro
  mutate(econ_retro=case_when(
    econ_ret == "become better" ~ "Got better",
    econ_ret == "stayed about the same" ~ "Stayed about the same",
    econ_ret == "become worse" ~ "Got worse",
    econ_ret %in% c("refused", "don't know") | is.na(econ_ret) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$econ_retro))) %>%
  # Convert econ_fdpol to econ_fed
  mutate(econ_fed=case_when(
    econ_fdpol == "better" ~ "Better",
    econ_fdpol == "not made much difference" ~ "Not made much difference",
    econ_fdpol == "worse" ~ "Worse",
    econ_fdpol %in% c("refused", "don't know") | is.na(econ_fdpol) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$econ_fed))) %>%
  # Convert pers_fedpol to ownfinanc_fed
  mutate(ownfinanc_fed=case_when(
    pers_fedpol == "better" ~ "Better",
    pers_fedpol == "not made much difference / just the economy, etc." ~ "Not made much difference",
    pers_fedpol == "worse" ~ "Worse",
    pers_fedpol %in% c("refused", "don't know") | is.na(pers_fedpol) ~ "Don't know/ Prefer not to answer"
  ) %>% factor(levels=levels(ces19$ownfinanc_fed))) %>%
  # Add imm column (not present in 2015 data, so we will set it to "Don't know/ Prefer not to answer")
  mutate(imm="Don't know/ Prefer not to answer" %>% factor(levels=levels(ces19$imm))) %>%
  # Add refugees column (not present in 2015 data, so we will set it to "Don't know/ Prefer not to answer")
  mutate(refugees="Don't know/ Prefer not to answer" %>% factor(levels=levels(ces19$refugees))) %>%
  # Add govt_confusing column (not present in 2015 data, so we will set it to "Don't know/ Prefer not to answer")
  mutate(govt_confusing="Don't know/ Prefer not to answer" %>% factor(levels=levels(ces19$govt_confusing))) %>%
  # Add govt_say column (not present in 2015 data, so we will set it to "Don't know/ Prefer not to answer")
  mutate(govt_say="Don't know/ Prefer not to answer" %>% factor(levels=levels(ces19$govt_confusing))) %>%
  # Convert cntry_born to bornin_canada
  mutate(bornin_canada=case_when(
    cntry_born %in% c("Canada", "Quebec") ~ "Yes",
    cntry_born %in% c("refused", "don't know") | is.na(cntry_born) ~ "Don't know/ Prefer not to say",
    TRUE ~ "No"
  ) %>% factor(levels=levels(ces19$bornin_canada))) %>%
  # Add lib_promises column (not present in 2015 data, so we will set it to "Don't know/ Prefer not to answer")
  mutate(lib_promises="Don't know/ Prefer not to answer" %>% factor(levels=levels(ces19$lib_promises))) %>%
  # Add groups_therm_1 column (not present in 2015, so we'll default it to a neutral response: 50)
  mutate(groups_therm_1=50) %>%
  # Add groups_therm_2 column (not present in 2015, so we'll default it to a neutral response: 50)
  mutate(groups_therm_2=50) %>%
  # Add groups_therm_3 column (not present in 2015, so we'll default it to a neutral response: 50)
  mutate(groups_therm_3=50) %>%
  # Add groups_therm_4 column (not present in 2015, so we'll default it to a neutral response: 50)
  mutate(groups_therm_4=50) %>%
  # Add groups_therm_5 column (not present in 2015, so we'll default it to a neutral response: 50)
  mutate(groups_therm_5=50) %>%
  # Add interest_elxn_1 column (not present in 2015, so we'll default it to a neutral response: 50)
  mutate(interest_elxn_1=50) %>%
  # Convert p_like_polit to interest_gen_1
  mutate(interest_gen_1=as.numeric(p_like_polit)) %>%
  select(all_of(feature_and_predictor_variables))

write_rds(ces15, "ces15.rds")