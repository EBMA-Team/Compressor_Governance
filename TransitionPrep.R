# Preparation for upgrade

# to add to governance report when ready to run

Deidentification for initial testing

Patient and complications table has already been read in

```{r}
#| label: read-tables2
#| execute: false
#| 


# Authenticate for sheets using the same token
gs4_auth(token = drive_token())


#To match to acctData
TreatTable <- googlesheets4::range_read(
  ss = SheetIDs$DbSS,
  sheet = "Treatment",
  range = "A11:BL",
  col_names = FALSE,
  col_types = "DcccciDcccccccccccDDDDtccDcccDcccccicccccccccccccccccccccccccccc"
)

Treat_Col <- googlesheets4::range_read(
  ss = SheetIDs$DbSS,
  sheet = "Treatment", 
  range = "A1:BL1"
)

colnames(TreatTable) <- colnames(Treat_Col)

```

```{r}
#| execute: false
# Extract DarrenLog to retrieve lastcontact date
# 
DarrenTable <- googlesheets4::range_read(
  ss = SheetIDs$DbSS,
  sheet = "Darren Log",
  range = "A1:H",
  col_names = TRUE,
  col_types = "Dccccccc"
)


```

Deidentify patient data

```{r}
#| execute: false


library(tidyverse)
library(digest)
library(lubridate)

# Generate Australian mobile number format from hash
generate_au_mobile <- function(hash_value) {
  # Extract numeric portion from hash
  numeric_hash <- as.numeric(paste0("0x", substr(hash_value, 1, 8)))
  
  # Generate 8-digit number and format as 04XX XXX XXX
  mobile_number <- (numeric_hash %% 100000000) |> 
    str_pad(width = 8, pad = "0")
  
  formatted <- paste0("04", substr(mobile_number, 1, 2), " ", 
                      substr(mobile_number, 3, 5), " ", 
                      substr(mobile_number, 6, 8))
  
  return(formatted)
}

# Generate scrambled date from hash (maintains date format)
generate_scrambled_date <- function(original_date, hash_value) {
  # Extract numeric portion from hash
  numeric_hash <- as.numeric(paste0("0x", substr(hash_value, 1, 8)))
  
  # Generate a date offset (within reasonable range: +/- 20 years)
  days_offset <- (numeric_hash %% 14600) - 7300
  
  # Apply offset to original date
  scrambled_date <- original_date + days(days_offset)
  
  return(scrambled_date)
}

# Scramble identifiers in a single dataframe using hash-based approach
scramble_identifiers <- function(df, id_columns, salt = "my_salt_123") {
  
  result <- df
  
  for (col in id_columns) {
    if (col %in% names(df)) {
      
      # Get unique values for this column
      unique_values <- df |>
        pull(!!col) |>
        unique() |>
        na.omit()
      
      # Store original class for type conversion later
      original_class <- class(df[[col]])[1]
      
      # Convert to character for hashing
      unique_values_char <- as.character(unique_values)
      
      # Generate hash-based scrambled values
      hashed_values <- map_chr(
        unique_values_char, 
        ~ digest::digest(paste0(.x, salt, col), algo = "md5")
      )
      
      # Format based on column type
      if (col == "Phone") {
        # Format phone numbers as Australian mobiles
        scrambled_values <- map_chr(hashed_values, generate_au_mobile)
      } else if (col == "DateOfBirth" && original_class == "Date") {
        # Generate scrambled dates that maintain Date format
        scrambled_values <- map2_dbl(unique_values, hashed_values, 
                                     ~as.numeric(generate_scrambled_date(.x, .y)))
        scrambled_values <- as.Date(scrambled_values, origin = "1970-01-01")
      } else {
        # Keep as hash for other columns
        scrambled_values <- hashed_values
      }
      
      # Create lookup table for this column
      lookup <- tibble(
        original = unique_values,
        scrambled = scrambled_values
      ) |>
        rename(!!col := original, !!paste0(col, "_new") := scrambled)
      
      # Join and replace
      result <- result |>
        left_join(lookup, by = col) |>
        mutate(!!col := coalesce(!!sym(paste0(col, "_new")), !!sym(col))) |>
        select(-!!paste0(col, "_new"))
    }
  }
  
  return(result)
}
```

```{r}
#| execute: false


PatientDeid <- scramble_identifiers(
  df = PatientTable,
  id_columns = c("FirstName", "LastName", "DateOfBirth", "Email", "Phone"),
  salt = "my_secret_salt"
)

TreatDeid <- scramble_identifiers(
  df = TreatTable,
  id_columns = c("FirstName", "LastName"),
  salt = "my_secret_salt"
)
```

Retrieve lastcontact

```{r}
#| execute: false


DarrenTable2 <- DarrenTable |>
  dplyr::mutate(
    PatientID = stringr::str_split_i(`Treatment UID`,"\\.",1)
  ) |>
  group_by(PatientID) |> 
  slice_max(`Time Stamp`,n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  arrange(desc(`Time Stamp`))



```

Merge tables

```{r}
#| execute: false


PatientTable2 <- PatientTable |> dplyr::select(
  -(c(
    FirstName,
    LastName,
    DateOfBirth,
    Email,
    Phone
  )
  )
) |> left_join(
  DarrenTable2 |> dplyr::select(
    PatientID,
    `Time Stamp`
  ),
  by = "PatientID"
) |> left_join(
  TreatTable |> dplyr::mutate(
    PatientID = stringr::str_split_i(TreatmentID,"\\.",1)
  ) |> group_by(
    PatientID
  ) |> slice_max(
    DateTreatmentRecordCreation,
    with_ties = FALSE,
    n = 1
  ) |> dplyr::select(
    PatientID,
    Postcode
  ),
  by = "PatientID"
)

```

```{r}
#| execute: false


# Example: Find duplicates based on columns 'col1' and 'col2'
duplicates <- PatientTable2 |>
  group_by(PatientID) |>
  filter(n() > 1) |>
  ungroup() # Ungroup for further operations
```

```{r}
#| execute: false


PatientTableNew <- bind_cols(
  PatientTable2,
  PatientDeid |> dplyr::select(
    FirstName,
    LastName,
    DateOfBirth,
    Email,
    Phone
  ))   |> rename(
    CreationDate = "PatientCreationDate",
    GivenNames = "FirstName",
    PatientLatestContactDate = "Time Stamp",
    RegistryStatusDate = "DateRegistryStatus"
  ) |> dplyr::mutate(
    Name = stringr::str_c(GivenNames,", ", LastName),
    LastChangeDate = lubridate::ymd("2026-Feb-10"),
    Sex = case_when(
      Sex == "F" ~ "Female",
      Sex == "M" ~ "Male",
      .default = NA_character_
    ),
    DateOfDeceased = if_else(
      stringr::str_detect(stringr::str_to_lower(RegistryStatusNotes),"decease*|mortality"),
      RegistryStatusDate,
      as.Date(NA)
    ),
    NextMessageDate = NA_character_, 
    Reminder1Date = NA_character_,
    Reminder2Date = NA_character_,
    TransferBetweenProviders = NA_character_,
    PatientID = stringr::str_c("888",PatientID)
  ) 

```

```{r}
#| execute: false

TreatTable2 <- TreatTable |> dplyr::select(
  -(c(
    FirstName,
    LastName
  )
  )
)

TreatTableNew <- bind_cols(
  TreatTable2,
  TreatDeid |> dplyr::select(
    FirstName,
    LastName
  )
) |> mutate(
  Name = stringr::str_c(FirstName,", ", LastName),
  RegistryCohortName2 = case_when(
    stringr::str_detect(RegistryCohortName,"TumourPelvis") & (stringr::str_detect(SurgicalTreatment,"Non") | is.na(SurgicalTreatment)) ~ "PelvisTumourNS",
    stringr::str_detect(RegistryCohortName,"TumourUpperLimb") & (stringr::str_detect(SurgicalTreatment,"Non") | is.na(SurgicalTreatment)) ~ "ULTumourLongNS",
    stringr::str_detect(RegistryCohortName,"TumourLowerLimb") & (stringr::str_detect(SurgicalTreatment,"Non") | is.na(SurgicalTreatment)) ~ "LLTumourLongNS",
    stringr::str_detect(RegistryCohortName,"ReinterventionTHA") & (stringr::str_detect(SurgicalTreatment,"Non") | is.na(SurgicalTreatment)) ~ "ReinterventionTHANS",
    stringr::str_detect(RegistryCohortName,"ReinterventionTKA") & (stringr::str_detect(SurgicalTreatment,"Non") | is.na(SurgicalTreatment)) ~ "ReinterventionTKANS",
    stringr::str_detect(RegistryCohortName,"TumourPelvis") & SurgicalTreatment == "Surgical" ~ "PelvisTumourSurg",
    stringr::str_detect(RegistryCohortName,"TumourUpperLimb") & SurgicalTreatment == "Surgical" ~ "ULTumourLongSurg",
    stringr::str_detect(RegistryCohortName,"TumourLowerLimb") & SurgicalTreatment == "Surgical" ~ "LLTumourLongSurg",
    stringr::str_detect(RegistryCohortName,"ReinterventionTHA") & SurgicalTreatment == "Surgical" ~ "ReinterventionTHASurg",
    stringr::str_detect(RegistryCohortName,"ReinterventionTKA") & SurgicalTreatment == "Surgical" ~ "ReinterventionTKASurg",
    .default = "LLGenLongNS"
    
  ),
  SurgeryRecommended = case_when(
    stringr::str_detect(RegistryCohortName2,"NS") & stringr::str_detect(EBMAComment,"recommended") ~ "Yes",
    stringr::str_detect(RegistryCohortName2,"NS") & (stringr::str_detect(EBMAComment,"recommended", negate = TRUE) | is.na(EBMAComment)) ~ "No",
    SurgicalTreatment ==  "Surgical" ~ "NA",
    .default = NA_character_
  ),
  PatientID = stringr::str_c("888",PatientID),
  TreatmentID = stringr::str_c("888",TreatmentID),
  Region = case_when(
    stringr::str_detect(RegistryCohortName2,"Pelvis") ~ "Pelvis",
    stringr::str_detect(RegistryCohortName2,"THA") ~ "Hip",
    stringr::str_detect(RegistryCohortName2,"TKA") ~ "Knee",
    stringr::str_detect(RegistryCohortName2,"UL") ~ "Upper Limb",
    stringr::str_detect(RegistryCohortName2,"LL") ~ "Lower Limb",
    .default = NA_character_
  ),
  Pre_InitialConsult = NA_character_,
  Post_6weeks = NA_character_,
  Post_9months = NA_character_,
  Provider = case_when(
    Provider == "RB" ~ "RBoyleOrtho",
    Provider == "PS" ~ "PStalleyOrtho",
    Provider == "MG" ~ "MGuzmanOrtho",
    Provider == "DF" ~ "DFranksOrtho",
    .default = NA_character_
  ),
  Surgeon = case_when(
    Surgeon == "RB" ~ "RBoyleOrtho",
    Surgeon == "PS" ~ "PStalleyOrtho",
    Surgeon == "MG" ~ "MGuzmanOrtho",
    Surgeon == "DF" ~ "DFranksOrtho",
    .default = NA_character_
  ),
  Facilty = case_when(
    stringr::str_detect(str_to_lower(Facility),"nsph") ~ "North Shore Private Hospital",
    Facility == "RNSH" ~ "Royal North Shore Hospital",
    Facility == "Lifehouse" ~ "Chris O'Brien Lifehouse",
    Facility == "Sydney Children's Hospital" ~ "Sydney Children's Hospital",
    Facility == "The New Childrens Hospital" ~ "Sydney Children's Hospital",
    Facility == "Westmead Children's Hospital" ~ "Westmead Children's Hospital",
    stringr::str_detect(Facility,"RPA|Alfred") ~ "Royal Prince Alfred",
    Facility == "Newtown Rooms" ~ "RPA Medical Centre",
    Facility == "Mater" ~ "Mater Misericordiae Hospital",
    .default = NA_character_
    
  ),
  # DiagnosisRawFinal = stringr::str_split_i(DiagnosisPrimary, " - ",2),
  # DiagnosisICD10Final = stringr::str_split_i(DiagnosisPrimary, " - ",1),
  LookerComplicationVars = NA_character_,
  dplyr::across(
    dplyr::contains("Date") & !dplyr::all_of("DateCurrentEndTimeWindow"),
    ~as.character(.x)
  )
) |> relocate(
  RegistryCohortName, .before = RegistryCohortName2
) |> left_join(
  DarrenTable2 |> dplyr::select(`Treatment UID`,`Time Stamp`),
  join_by(TreatmentID == `Treatment UID`)
) |> unite(
  "EBMAComment2",
  c(EBMAComment),
  na.rm = TRUE,
  remove = TRUE,
  sep = ";"
) |> dplyr::select(
  -(c(
    RegistryCohortName,
    DateLatestContact
  )
  )
) |> rename(
  PROMsTrigger = "PROMs Trigger",
  TimeNextAppointment = "NextAppointmentTime",
  ProcedureForm = "Procedure",
  RegistryCohortName = "RegistryCohortName2",
  DateLatestContact = "Time Stamp",
  EBMAComment = "EBMAComment2"
) 



```

Do some checks

```{r}
#| execute: false


# Identify rows in df_a that are not in df_b (based on the 'id' column)
rows_not_in_b <- anti_join(TreatTableNew, TreatTable2, by = "TreatmentID")
```

Write to CSV

```{r}
#| execute: false


readr::write_csv(
  PatientTableNew |> dplyr::select(
    CreationDate,
    PatientID,
    LastName,
    GivenNames,
    Name,
    DateOfBirth,
    Sex,
    RegistryStatus,
    RegistryStatusNotes,
    RegistryStatusDate,
    NotificationMethod,
    NoTreatmentRecords,
    Email,
    Phone,
    TrueNoTreatmentRecords,
    Postcode,
    LastChangeDate,
    AlternateID,
    DateOfDeceased,
    NextMessageDate,
    Reminder1Date,
    Reminder2Date,
    TransferBetweenProviders,
    PatientLatestContactDate
  ),
  na = "",
  "COMPRESSORPatient.csv"
)

readr::write_csv(
  TreatTableNew |> dplyr::select(
    DateTreatmentRecordCreation,
    PatientID,
    TreatmentActivity,
    TreatmentActivityNotes,
    PROMsTrigger,
    FollowupContactAttempt,
    DateLatestContact,TreatmentID,
    LastName,
    FirstName,
    Postcode,
    Surgeon,
    Provider,
    AnalysisLabel,
    ExternalStudyTag,
    ExternalStudyStatus,
    DateExternalStudyStatus,
    DateInitialExamination,
    DateTreatment,
    DateNextAppointment,
    TimeNextAppointment,
    TreatmentStatus,
    TreatmentStatusNotes,
    DateStatusChange,
    TreatmentType,
    AffectedSide,
    DateLastChecked,
    SurgeryRecommended,
    EBMAComment,
    ProcedureName,
    Facility,
    RegistryCohortName,
    RegistryCohortID,
    Region,
    ProcedureForm,
    ComplicationForm,
    CurrentTimepoint,
    Pre_InitialConsult,
    Pre_Treatment,
    Post_6weeks,
    Post_3months,
    Post_6months,
    Post_9months,
    Post_12months,
    Post_24months,
    Post_60months,
    Post_120months,
    CurrentFormID,
    CurrentPROMLink,
    CurrentPROMStatus,
    DateCurrentEndTimeWindow,
    ConsultListPROMS,
    ConsultListAppointmentType,
    DiagnosisRawPrelim,
    DiagnosisSNOMEDPrelim,
    DiagnosisRawFinal,
    DiagnosisSNOMEDFinal,
    DiagnosisICD10Final,
    DiagnosisICD10Prelim, 
    LookerComplicationVars
  ),
  "COMRPESSORTreat.csv",
  na = ""
)

```


--------------------------------------------
  
  
  Snapshot <- dplyr::mutate(
    Snapshot,
    across(contains("Score"), ~as.numeric(.))
  )

FigureVR12 <- Snapshot |> dplyr::filter(SurgicalTreatment2 == "Surgical") |>
  ggplot(aes(y = RegistryCohortName, x = VR12PCS_12months)) +
  stat_halfeye(
    point_interval = median_qi,  # median and interquartile range
    .width = c(0.50, 0.95),   # quartile ranges
    interval_size_range = c(0.5, 1.5),  # Thin and bold line weights,
    fill = "steelblue"
  )


knitr::knit_print(FigureVR12)





## GMRS-MRH

The surgical cases containing Stryker components are tagged in an additional field. For the purposes of this report, the label Hardware contains the first instance in the text field of GMRS or MRH. There are a number of cases where both components have been used that is not reflected in the data below (see specific sponsor reports).

```{r}
#| label: tbl-gmrs-demographics
#| tbl-cap: "Summary of demographics in GMRS-MRH cohort"

TableGMRSdemo <- gtsummary::tbl_summary(
  Snapshot |> dplyr::filter(
    stringr::str_detect(str_to_lower(Hardware), "gmrs|mrh"),
    RegistryCohortName != "TumourUpperLimb",
    Surgeon == "RB"
  ) |> dplyr::select(
    RegistryCohortName,
    TreatmentType,
    SurgicalTreatment2,
    TreatmentStatus,
    DateInitialExamination,
    AgeAtInitialExam,
    Sex2,
    Hardware,
    Retrospective
    # EducationLevel_Preop,
    # DiagnosisPrimary
  ),
  by = "RegistryCohortName",
  missing = "no",
  statistic = list(
    DateInitialExamination ~ "{min} - {max}"
  )
) |>
  add_overall()

knitr::knit_print(TableGMRSdemo)
```

The General cohort included in @tbl-gmrs-demographics include complex primary arthroplasties for deformity concomitant with osteoarthritis of the knee that have received the components of interest.

```{r}
#| label: survival-prep-gmrs

SnapshotGMRS <- Snapshot |> dplyr::filter(
  Hardware == "gmrs" | Hardware == "mrh",
  SurgicalTreatment2 == "Surgical",
  Planned != "Yes",
  Surgeon == "RB"
) 

SnapshotGMRS2 <- SnapshotGMRS |> dplyr::mutate(
  Status = if_else(
    TreatmentStatus == "Failed",
    1,
    0
  ),
  EndDate = case_when(
    !is.na(DateStatusChange) ~ DateStatusChange,
    .default = coalesce(DateStatusChange, ymd(CurrentDate)),
  ),
  Duration = as.numeric(as.duration(interval(ymd(DateTreatment), ymd(EndDate))),"years"),
)

```

```{r}
#| label: fig-survgmrs
#| fig-cap: "Survival curve for GMRS-MRH cases by treatment type"


FigureSurvGMRS <- survfit2(Surv(Duration, Status) ~ TreatmentType2,
                           data = SnapshotGMRS2
) |> ggsurvfit(linewidth = 1) +
  add_confidence_interval() +
  add_risktable() +
  add_quantile(y_value = 0.7, color = "gray50", linewidth = 0.75) +
  scale_ggsurvfit() +
  coord_cartesian(ylim = c(0.2, 1))

knitr::knit_print(FigureSurvGMRS)

```

```{r}
#| label: tbl-survgmrs
#| tbl-cap: "Summary of GMRS-MRH Procedure Survival"

TableSurvGMRS <- tbl_survfit(
  survfit2(Surv(Duration, Status) ~ TreatmentType2, 
           data = SnapshotGMRS2),
  times = c(1,2,5,10),
  label_header = "**{time} Years**",
  label = "Procedure Survival",
  statistic = "{estimate} ({conf.low} - {conf.high})"
)

knitr::knit_print(TableSurvGMRS)

```

The overall survival for surgical cases in this cohort are illustrated in @fig-survgmrs and summarised in @tbl-survgmrs , with 70% survival for revision cases occurring at \~ 5 years, while for primary cases this occurred \~ 10 years.

```{r}
#| label: fig-vr122
#| fig-cap: "Summary of VR12 by Timepoint"

#preop_position <- which(levels(PROMQDASH$TimePoint) == "Preop")

PROMVR122 <- left_join(
  PROMVR12,
  SnapshotGMRS |> dplyr::select(
    TreatmentID,
    Hardware,
    TreatmentType2
  ),
  by = "TreatmentID"
) |> filter(
  Hardware == "gmrs" | Hardware == "mrh"
)

FigureVR122 <- PROMVR122 |> filter(
  # RegistryCohortName != "ReinterventionTHA"
) |> ggplot(aes(y = VR12PCS, x = TimePoint, fill = TreatmentType2, color = TreatmentType2)) +
  stat_halfeye(
    alpha = 0.5,  # Transparency for overlap visibility
    position = "identity",  # Overlay the distributions
    na.rm = TRUE,
    scale = 0.9  # Slightly scale down to avoid too much overlap
  ) +
  # Add appropriate scale colors "Surgery recommended" = "darkred"
  scale_fill_manual(values = c(
    "Primary" = "steelblue", 
    "Revision" = "darkred"
  )
  ) +
  scale_color_manual(values = c(
    "Primary" = "steelblue", 
    "Revision" = "darkred"
  )
  ) +
  labs(
    y = "MSTSLowerTotal",
    x = "Time Point",
    fill = "Treatment Type",
    color = "Treatment Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + facet_wrap(
    ~RegistryCohortName, ncol = 2
  )

knitr::knit_print(FigureVR122)
```

The VR12 Physical component data (@fig-vr122) remain difficult to interpret due to the low sample sizes and cross sectional nature of the data. Initial observations identify relative stability across timepoints for ReinterventionTKA, while TumourLowerLimb displays a more complex pattern of recovery from 6 months onwards.

```{r}
#| label: fig-msts2
#| fig-cap: "Summary of MSTS by Timepoint for GMRS | MRH cases"

#preop_position <- which(levels(PROMQDASH$TimePoint) == "Preop")

PROMMSTS2 <- left_join(
  PROMMSTS,
  SnapshotGMRS |> dplyr::select(
    TreatmentID,
    Hardware,
    TreatmentType2,
    Planned
  ),
  by = "TreatmentID"
) |> filter(
  !is.na(Hardware)
)

FigureMSTS2 <- PROMMSTS2 |> filter(
  Hardware == "gmrs" | Hardware == "MRH",
  Planned != "Yes",
  RegistryCohortName == "ReinterventionTKA" | RegistryCohortName == "TumourLowerLimb",
  TimePoint != "Preop"
) |> ggplot(aes(y = MSTSLowerTotal, x = TimePoint, fill = TreatmentType2, color = TreatmentType2)) +
  stat_halfeye(
    alpha = 0.5,  # Transparency for overlap visibility
    position = "identity",  # Overlay the distributions
    na.rm = TRUE,
    scale = 0.9  # Slightly scale down to avoid too much overlap
  ) +
  # Add appropriate scale colors "Surgery recommended" = "darkred"
  scale_fill_manual(values = c(
    "Primary" = "steelblue", 
    "Revision" = "darkred"
  )
  ) +
  scale_color_manual(values = c(
    "Primary" = "steelblue", 
    "Revision" = "darkred"
  )
  ) +
  labs(
    y = "MSTSLowerTotal",
    x = "Time Point",
    fill = "Treatment Type",
    color = "Treatment Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) + facet_wrap(
    ~RegistryCohortName, ncol = 2
  )

knitr::knit_print(FigureMSTS2)
```

Similarly, the cross-sectional nature of the MSTS data for this subgroup may be misleading (@fig-msts2), however differences are notable within the TumourLowerLimb cohort between primary and revision cases.

```{r}
#| eval: false

# Why is TumourPelvis Blank?

SnapshotTP <- Snapshot |>  dplyr::filter(
  RegistryCohortName == "TumourPelvis",
  Hardware == "gmrs",
  Planned != "Yes"
)


# There are 9 cases, 3 ongoing and 3 prospective (2 ongoing)

```

### PROMs Capture

Target numbers retrieved from [Synopsis](https://docs.google.com/document/d/1u2V1-J337kgyolVJxOBVcHWOvEHkMGrmdLkX4it4mh8/edit)

::: {#synopsis-targets}
  Complex primary TKA: 67 cases
  
  Revision TKA: 79 cases
  
  Distal femoral reconstruction: 53 cases
  
  Proximal tibial reconstruction: 55 cases
  
  Proximal femoral reconstruction: 78 cases
  
  Total femoral replacement: As available
  
  Femoral diaphyseal defect reconstruction: As available
  :::
    
    ```{r}
  
  
  PROMKOOS12 <- Snapshot |> dplyr::select(
    TreatmentID,
    starts_with("KOOS12_Summary"),
    starts_with("EligibleAt"),
    -EligibleAtIntraop
  ) |> rename_with(
    ~gsub("KOOS12_Summary_TotalScore","KOOS12SummaryTotal",.x, fixed = TRUE)
  ) |>  rename_with(
    ~gsub("Atx","At_",.x, fixed = TRUE)
  ) |> rename(
    EligibleAt_Preop = "EligibleAtPreop"
  ) |> pivot_longer(
    cols = !TreatmentID,
    names_to = c(".value","TimePoint"),
    names_sep = "_",
    values_drop_na = TRUE
  ) |> mutate(
    TimePoint = factor(TimePoint, levels = c("Preop","3months","6months","12months","24months", "60months", "120months"), ordered = TRUE, exclude = NA),
    KOOS12SummaryTotal = as.numeric(KOOS12SummaryTotal)
  ) |> dplyr::filter(
    !is.na(TimePoint)
  ) |> left_join(
    Snapshot |> dplyr::select(
      TreatmentID,
      SurgicalTreatment2,
      TreatmentType,
      TreatmentStatus
    ),
    by = "TreatmentID"
  ) |> unite(
    "TimePointID",
    c("TreatmentID","TimePoint"),
    sep = ".",
    na.rm = FALSE,
    remove = FALSE
  )
  
  
  SnapshotGMRSPROM <- SnapshotGMRS2 |> dplyr::select(
    TreatmentID,
    TreatmentType,
    TreatmentActivity,
    RegistryCohortName,
    DiagnosisRawPrelim,
    starts_with("Eligible")
  ) |> dplyr::mutate(
    across(everything(), ~replace_na(.x, "No")),
    ComplexTKA = if_else((RegistryCohortName == "General") | (TreatmentType == "Primary" & stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"knee|mrh")),"ComplexPrimaryTKA",NA_character_),
    RevisionTKA = if_else((RegistryCohortName == "ReinterventionTKA") | (stringr::str_detect(str_to_lower(TreatmentType), "revision") & stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"knee|mrh")),"RevisionTKA",NA_character_),
    ProximalFemur = if_else(stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"proximal fem"), "ProximalFemur",NA_character_
    ),
    Intercalary = if_else(stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"intercalary"), "Intercalary",NA_character_
    ),
    TotalFemur = if_else(stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"total fem"),  "TotalFemur",NA_character_
    ),
    DistalFemur = if_else(stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"distal fem"),  "DistalFemur",NA_character_
    ),
    ProximalTibia = if_else(stringr::str_detect(str_to_lower(DiagnosisRawPrelim),"prox.*tibia"),  "ProximalTibia",NA_character_
    )
  ) |> pivot_longer(
    cols = ComplexTKA:ProximalTibia,
    names_to = "UsageComponent",
    values_to = "Usage",
    values_drop_na = TRUE
  ) |> pivot_longer(
    cols = starts_with("EligibleAt"),
    names_to = "TimePoint",
    values_to = "Eligibility",
    values_drop_na = TRUE,
    names_prefix = "EligibleAt"
  ) |> filter(
    TimePoint != "Intraop"
  ) |> mutate(
    TimePoint = stringr::str_remove(TimePoint,"x"),
    TimePoint = forcats::fct(TimePoint, levels = c("Preop","3months","6months","12months","24months","60months","120months"))
  ) |> unite(
    "TimePointID",
    c("TreatmentID","TimePoint"),
    sep = ".",
    na.rm = FALSE,
    remove = FALSE
  ) |> left_join(
    PROMVR12 |> dplyr::select(TimePointID,VR12PCS),
    by = "TimePointID"
  ) |> left_join(
    PROMMSTS |> dplyr::select(TimePointID,MSTSLowerTotal),
    by = "TimePointID"
  ) |> left_join(
    PROMHOOS12 |> dplyr::select(TimePointID,HOOS12SummaryTotal),
    by = "TimePointID"
  ) |> left_join(
    PROMKOOS12 |> dplyr::select(TimePointID,KOOS12SummaryTotal),
    by = "TimePointID"
  ) |> naniar::add_shadow(MSTSLowerTotal,HOOS12SummaryTotal,KOOS12SummaryTotal,VR12PCS)
  
  ```
  
  ```{r}
  
  #| label: tbl-promeligible
  #| tbl-cap: "Summary of PROMs Eligibility by Timepoint for GMRS | MRH cases"
  
  eligible_summary <- SnapshotGMRSPROM |>
    summarise(
      n_eligible = sum(Eligibility == "Yes", na.rm = TRUE),
      .by = c(TimePoint, Usage)
    ) |> arrange(TimePoint)
  
  # Main body: n_eligible per TimePoint x Usage
  main_table <- eligible_summary |>
    mutate(cell = as.character(n_eligible)) |>
    select(TimePoint, Usage, cell) |>
    tidyr::pivot_wider(
      names_from = Usage,
      values_from = cell
    ) |>
    rename(` ` = TimePoint)
  
  # Footer row 1: distinct TreatmentIDs per Usage category
  footer_total <- SnapshotGMRSPROM |>
    summarise(
      n_total = n_distinct(TreatmentID),
      .by = Usage
    ) |>
    mutate(cell = as.character(n_total)) |>
    select(Usage, cell) |>
    tidyr::pivot_wider(
      names_from = Usage,
      values_from = cell
    ) |>
    mutate(` ` = "N Cases (Total)", .before = 1)
  
  # Footer row 2: distinct TreatmentIDs where TreatmentActivity == "Active"
  footer_active <- SnapshotGMRSPROM |>
    filter(TreatmentActivity == "Active") |>
    summarise(
      n_active = n_distinct(TreatmentID),
      .by = Usage
    ) |>
    mutate(cell = as.character(n_active)) |>
    select(Usage, cell) |>
    tidyr::pivot_wider(
      names_from = Usage,
      values_from = cell
    ) |>
    mutate(` ` = "N Cases (Active)", .before = 1)
  
  # Bind and render
  bind_rows(main_table, footer_total, footer_active) |>
    gt::gt(rowname_col = " ") |>
    gt::tab_header(
      title = "Summary of PROMs Eligibility by Timepoint for GMRS | MRH cases"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_stub(rows = c("N Cases (Total)", "N Cases (Active)"))
    ) |>
    gt::opt_stylize(style = 1) |>
    gt::opt_table_font(font = gt::google_font("Inter"))
  
  ```
  
  ```{r}
  #| label: tbl-promcapture1
  #| tbl-cap: "Summary of PROMs Capture by Timepoint for GMRS | MRH cases"
  # N returned per TimePoint x Usage (any one PROM returned)
  returned_summary <- SnapshotGMRSPROM |>
    mutate(
      returned = VR12PCS_NA == "!NA" |
        MSTSLowerTotal_NA == "!NA" |
        HOOS12SummaryTotal_NA == "!NA" |
        KOOS12SummaryTotal_NA == "!NA"
    ) |>
    summarise(
      n_returned = sum(returned, na.rm = TRUE),
      .by = c(TimePoint, Usage)
    ) |> arrange(TimePoint)
  
  # Join to eligible counts to calculate percentage
  returned_table <- returned_summary |>
    left_join(
      eligible_summary,
      by = c("TimePoint", "Usage")
    ) |>
    mutate(
      pct = round((n_returned / n_eligible) * 100, 1),
      cell = glue::glue("{n_returned} ({pct}%)")
    ) |>
    select(TimePoint, Usage, cell) |>
    tidyr::pivot_wider(
      names_from = Usage,
      values_from = cell
    ) |>
    rename(` ` = TimePoint)
  
  # Render
  returned_table |>
    gt::gt(rowname_col = " ") |>
    gt::tab_header(
      title = "Summary of Returned PROMs by Timepoint for GMRS | MRH cases"
    ) |>
    gt::opt_stylize(style = 1) |>
    gt::opt_table_font(font = gt::google_font("Inter"))
  ```
  
  ```{r}
  #| label: tbl-promcapture2
  #| tbl-cap: "Summary of PROMs Capture by Timepoint for GMRS | MRH cases"
  
  
  TableEligible <- gtsummary::tbl_summary(
    SnapshotGMRSPROM,
    include = -(c(TreatmentID,TimePointID, UsageComponent,TreatmentType,DiagnosisRawPrelim,MSTSLowerTotal,HOOS12SummaryTotal,KOOS12SummaryTotal)),
    by = "Usage",
    label = list(
      MSTSLowerTotal_NA = "MSTS Captured",
      HOOS12SummaryTotal_NA = "HOOS12 Captured",
      KOOS12SummaryTotal_NA = "KOOS12 Captured"
    ),
    type = list(
      MSTSLowerTotal_NA = "dichotomous",
      HOOS12SummaryTotal_NA = "dichotomous",
      KOOS12SummaryTotal_NA = "dichotomous"
    ),
    value = list(
      MSTSLowerTotal_NA = "!NA",
      HOOS12SummaryTotal_NA = "!NA",
      KOOS12SummaryTotal_NA = "!NA"
    ),
    missing = "no"
  ) |> add_overall()
  
  
  knitr::knit_print(TableEligible)
  
  
  
  ```
  
  ```{r}
  #| label: eligible-save
  #| eval: false
  
  
  
  TableEligible_gt <- TableEligible |> gtsummary::as_gt()
  
  gt::gtsave(
    data = TableEligible_gt,
    filename = "GMRS_Eligible.png"
  )
  
  ```
  
  ```{r}
  #| label: tbl-initialcapture
  #| tbl-cap: "Summary of Captured PROMs by Type"
  #| eval: false
  
  TableCapture1 <- gtsummary::tbl_summary(
    SnapshotGMRSPROM |> filter(
      Usage == "ComplexPrimaryTKA"
    ),
    include = c(
      TimePoint,
      MSTSLowerTotal_NA,
      HOOS12SummaryTotal_NA,
      KOOS12SummaryTotal_NA
    ),
    label = list(
      MSTSLowerTotal_NA = "MSTS Captured",
      HOOS12SummaryTotal_NA = "HOOS12 Captured",
      KOOS12SummaryTotal_NA = "KOOS12 Captured"
    ),
    type = list(
      MSTSLowerTotal_NA = "dichotomous",
      HOOS12SummaryTotal_NA = "dichotomous",
      KOOS12SummaryTotal_NA = "dichotomous"
    ),
    value = list(
      MSTSLowerTotal_NA = "!NA",
      HOOS12SummaryTotal_NA = "!NA",
      KOOS12SummaryTotal_NA = "!NA"
    ),
    missing = "no",
    by = "TimePoint"
  )
  
  
  knitr::knit_print(TableCapture1)
  ```
  
  ```{r}
  #| label: generate-summary-func
  
  generate_usage_summaries <- function(data) {
    # Get distinct usage values
    usage_terms <- data |> 
      distinct(Usage) |> 
      pull(Usage)
    
    # Create summary tables for each usage term
    summary_tables <- map(usage_terms, \(usage_term) {
      epoxy("Creating summary table for Usage: {usage_term}")
      
      gtsummary::tbl_summary(
        data |> filter(Usage == usage_term),
        include = c(
          TimePoint,
          MSTSLowerTotal_NA,
          HOOS12SummaryTotal_NA,
          KOOS12SummaryTotal_NA
        ),
        label = list(
          MSTSLowerTotal_NA = "MSTS Captured",
          HOOS12SummaryTotal_NA = "HOOS12 Captured",
          KOOS12SummaryTotal_NA = "KOOS12 Captured"
        ),
        type = list(
          MSTSLowerTotal_NA = "dichotomous",
          HOOS12SummaryTotal_NA = "dichotomous",
          KOOS12SummaryTotal_NA = "dichotomous"
        ),
        value = list(
          MSTSLowerTotal_NA = "!NA",
          HOOS12SummaryTotal_NA = "!NA",
          KOOS12SummaryTotal_NA = "!NA"
        ),
        missing = "no",
        by = "TimePoint"
      ) |> 
        modify_header(all_stat_cols() ~ "**{level}**")
    })
    
    # Name the list elements with usage terms
    names(summary_tables) <- usage_terms
    
    # Stack the tables using tbl_stack with quiet = TRUE to suppress header messages
    stacked_table <- tbl_stack(summary_tables, group_header = names(summary_tables), quiet = TRUE)
    
    return(stacked_table)
  }
  
  
  ```
  
  ```{r}
  #| label: tbl-capture-stack
  #| tbl-cap: "Summary of PROMs capture for GMRS components per hardware category over timepoints"
  
  stacked_summary <- generate_usage_summaries(SnapshotGMRSPROM)
  
  knitr::knit_print(stacked_summary)
  ```
  
  ```{r}
  #| label: summary-save
  #| eval: false
  
  
  
  Stack_summary_gt <- stacked_summary |> gtsummary::as_gt()
  
  gt::gtsave(
    data = Stack_summary_gt,
    filename = "GMRS_Capture.png"
  )
  
  ```
  
  ### PROMs Export
  
  ```{r}
  
  #retrieve product details
  
  # Authenticate for sheets using the same token
  gs4_auth(token = drive_token())
  
  ProductUsage<- read_sheet("https://docs.google.com/spreadsheets/d/1e_rIjH1MQ5BVnxd8ziAiWpT-GxhN9SOlSh5ORtL4rRQ/edit",
                            sheet = "RB_usage",
                            col_names = TRUE,
                            col_types = "Dcci")
  
  ProductTable <- read_sheet("https://docs.google.com/spreadsheets/d/1e_rIjH1MQ5BVnxd8ziAiWpT-GxhN9SOlSh5ORtL4rRQ/edit",
                             sheet = "Items",
                             col_names = TRUE,
                             col_types = "cc")
  
  
  
  
  ```
  
  \
  
  Needs to be exported in the following column format;
  
  -   PatientID
  
  -   TreatmentID
  
  -   RegistryCohortName
  
  -   PROMSelect
  
  -   Eligible2year
  
  -   Eligible5year
  
  -   Eligible10year
  
  -   Score 2\|5\|10yr
  
  -   MortalityStatus
  
  -   ProductNum1\|2\|3\|4
  
  -   TreatmentType (PrimaryRevision)
  
  -   GMRSUsage (ProxFemur, ProxTib, etc)
  
  PivotLonger SnapshotGMRSPROM (collapse questionnaire columns into multiple rows) and then PivotWider (Splitout timepoint column rows into multiple columns for the same score row)
  
  ```{r}
  
  GMRSExport <- SnapshotGMRS2 |> dplyr::select(
    TreatmentID,
    RegistryCohortName,
    starts_with("Eligible"),
    TreatmentType,
    DateTreatment,
    DiagnosisRawPrelim,
    AffectedSide,
    TreatmentStatusNotes,
    RegistryStatusNotes,
    (contains("MSTSLower")|contains("KOOS12_Summary")|contains("HOOS12_Summary")) & contains("Total"),
    contains("VR12PCS")
  ) |> mutate(
    across(starts_with("Eligible"), ~tidyr::replace_na(.x, "No")),
    Mortality = if_else(
      stringr::str_detect(str_to_lower(TreatmentStatusNotes),"deceas") | stringr::str_detect(str_to_lower(RegistryStatusNotes),"deceas|mortality"),
      "Yes",
      "No"
    ),
    TreatmentType2 = if_else(
      str_detect(TreatmentType,"Revision"),
      "Revision",
      "Primary"
    ),
    Mortality = replace_na(Mortality,"No"),
    PROMSelect = case_when(
      RegistryCohortName == "General" ~ "VR12PCS",
      RegistryCohortName == "TumourLowerLimb" ~ "MSTSLowerTotal",
      RegistryCohortName == "ReinterventionTKA" ~ "KOOS12SummaryTotalScore",
      RegistryCohortName == "ReinterventionTHA" ~ "HOOS12SummaryTotalScore",
      RegistryCohortName == "TumourPelvis" ~ "MSTSLowerTotal"
    ),
    DateTreatment = lubridate::ymd(DateTreatment)
  ) |> relocate(
    Mortality,
    .after = RegistryStatusNotes
  ) |> relocate(
    EligibleAtx12months,
    .after = EligibleAtx6months
  )|> rename_with(
    ~ gsub("_", "", .x, fixed = TRUE)
  ) |> filter(
    if_any(c(EligibleAtx12months, EligibleAtx24months, EligibleAtx60months,EligibleAtx120months), ~.x == "Yes")
  ) |> dplyr::select(
    -(c(
      TreatmentStatusNotes,
      RegistryStatusNotes,
      ends_with("3months"),
      ends_with("6months"),
      ends_with("Intraop"),
      ends_with("Preop")
    )
    )
  )
  
  
  ```
  
  ```{r}
  
  GMRSExport2 <- GMRSExport |>
    mutate(
      Score1yr = case_when(
        PROMSelect == "MSTSLowerTotal" ~ MSTSLowerTotal12months,
        PROMSelect == "KOOS12SummaryTotalScore" ~ KOOS12SummaryTotalScore12months,
        PROMSelect == "HOOS12SummaryTotalScore" ~ HOOS12SummaryTotalScore12months,
        PROMSelect == "VR12PCS" ~ VR12PCS12months,
        .default = NA_real_
      ),
      Score2yr = case_when(
        PROMSelect == "MSTSLowerTotal" ~ MSTSLowerTotal24months,
        PROMSelect == "KOOS12SummaryTotalScore" ~ KOOS12SummaryTotalScore24months,
        PROMSelect == "HOOS12SummaryTotalScore" ~ HOOS12SummaryTotalScore24months,
        PROMSelect == "VR12PCS" ~ VR12PCS24months,
        .default = NA_real_
      ),
      Score5yr = case_when(
        PROMSelect == "MSTSLowerTotal" ~ MSTSLowerTotal60months,
        PROMSelect == "KOOS12SummaryTotalScore" ~ KOOS12SummaryTotalScore60months,
        PROMSelect == "HOOS12SummaryTotalScore" ~ HOOS12SummaryTotalScore60months,
        PROMSelect == "VR12PCS" ~ VR12PCS60months,
        .default = NA_real_
      ),
      Score10yr = case_when(
        PROMSelect == "MSTSLowerTotal" ~ MSTSLowerTotal120months,
        PROMSelect == "KOOS12SummaryTotalScore" ~ KOOS12SummaryTotalScore120months,
        PROMSelect == "HOOS12SummaryTotalScore" ~ HOOS12SummaryTotalScore120months,
        PROMSelect == "VR12PCS" ~ VR12PCS120months,
        .default = NA_real_
      )
    ) |> dplyr::select(
      TreatmentID,
      RegistryCohortName,
      TreatmentType2,
      DiagnosisRawPrelim,
      DateTreatment,
      PROMSelect,
      EligibleAtx12months,
      EligibleAtx24months,
      EligibleAtx60months,
      EligibleAtx120months,
      Score1yr,
      Score2yr,
      Score5yr,
      Score10yr,
      Mortality
    ) 
  
  ```
  
  Add product information
  
  ```{r}
  
  ProductUsage1 <- ProductUsage |> left_join(
    ProductTable,
    by = "Code"
  )
  
  ```
  
  ```{r}
  # try and left_join between treatment data and productinfo
  
  ProductUsage2 <- ProductUsage1 |> dplyr::filter(
    DateSurgery %in% GMRSExport2$DateTreatment
  )
  
  
  ```
  
  ```{r}
  
  #Holly requested exact match
  
  GMRSProduct <- GMRSExport2 |> dplyr::select(
    TreatmentID,
    DateTreatment
  ) |> left_join(
    ProductUsage2 |> dplyr::select(DateSurgery, Code, Description),
    join_by("DateTreatment" == "DateSurgery"),
    relationship = "many-to-many"
  ) |> group_by(
    TreatmentID
  ) |> distinct(Code, .keep_all = TRUE) |> mutate(
    ProductN = row_number()
  ) |> ungroup()
  
  
  # fuzzy_left_join(
  #   ProductUsage2 |> select(DateSurgery, Code, Description),
  #   by = c("DateTreatment" = "DateSurgery"),
  #   match_fun = list(function(x, y) abs(x - y) <= 5)
  
  
  ```
  
  The matching on date is not perfect - need to supplement with a fuzzy match
  
  ```{r}
  GMRSProductAlt <- GMRSExport2 |> 
    select(TreatmentID, DateTreatment, DiagnosisRawPrelim) |> 
    fuzzy_left_join(
      ProductUsage2 |> select(DateSurgery, Code, Description),
      by = c("DateTreatment" = "DateSurgery"),
      match_fun = list(function(x, y) abs(x - y) <= 5)
    ) |> mutate(
      DateDifference = as.numeric(abs(DateTreatment - DateSurgery))
    ) |> arrange(TreatmentID, DateTreatment, DateDifference) |> mutate(
      Match = case_when(
        DateDifference == 0 ~ "Match"
      )
    )
  ```
  
  ```{r}
  GMRSProduct2 <- GMRSProduct |>
    pivot_wider(
      id_cols = c(TreatmentID, DateTreatment),
      names_from = ProductN,
      values_from = Code,
      names_prefix = "ProductNum"
    )
  ```
  
  ```{r}
  
  GMRSExport3 <- left_join(
    GMRSExport2,
    GMRSProduct2 |> dplyr::select(
      TreatmentID,
      ProductNum1:ProductNum15
    ),
    by = "TreatmentID"
  ) |> arrange(DateTreatment)
  
  write_xlsx(x = GMRSExport3, file = "GMRSMilestoneExp.xlsx", overwrite = TRUE)
  ```
  
  ## 3DPI
  
  The surgical cases containing Ossis components are tagged in an additional field. For the purposes of this report, the label Hardware contains the first instance in the text field of GMRS or MRH. There are a number of cases where both components have been used that is not reflected in the data below (see specific sponsor reports).
  
  ```{r}
  PrintPelMaster <- STROBEInput |> mutate(
    PatientID = stringr::str_split_i(TreatmentID,"\\.",1),
    LimbID = stringr::str_c(PatientID,".",AffectedSide)
  ) |> group_by(
    LimbID
  ) |> arrange(
    DateTreatment
  ) |> mutate(
    Count = row_number()
  ) |> ungroup() |> dplyr::filter(
    stringr::str_detect(str_to_lower(AnalysisLabel), "ossis"),
    stringr::str_detect(str_to_lower(AnalysisLabel), "plan|in-situ", negate = TRUE),
    Count == 1
    #Surgeon == "RB"
  ) 
  
  PrintPelAll <- STROBEInput |> mutate(
    PatientID = stringr::str_split_i(TreatmentID,"\\.",1)
  ) |> dplyr::filter(
    PatientID %in% PrintPelMaster$PatientID
  )
  ```
  
  ```{r}
  #| label: tbl-ossis-demographics
  #| tbl-cap: "Summary of demographics in 3DPI(Ossis) cohort"
  
  Table3DPIdemo <- gtsummary::tbl_summary(
    Snapshot |> dplyr::filter(
      stringr::str_detect(str_to_lower(AnalysisLabel), "ossis")
      #Surgeon == "RB"
    ) |> dplyr::select(
      RegistryCohortName,
      TreatmentType,
      SurgicalTreatment2,
      TreatmentStatus,
      DateInitialExamination,
      AgeAtInitialExam,
      Sex2,
      #Hardware,
      Retrospective
      # EducationLevel_Preop,
      # DiagnosisPrimary
    ),
    by = "RegistryCohortName",
    missing = "no",
    statistic = list(
      DateInitialExamination ~ "{min} - {max}"
    )
  ) |>
    add_overall()
  
  knitr::knit_print(Table3DPIdemo)
  ```
  
  # Observations \| Interpretation
  
  -   PROMs feedback remains relatively sparse and should be interpreted with some caution
  
  -   The nature of some patients undergoing multiple sequential treatments within the registry complicates both PROMs capture for these cases, as well as data preparation and analysis.
  
  -   The followup of cases included in the registry remains largely cross-sectional, with few instances of serial capture of recovery
  
  -   Long-term PROMs should be interpreted cautiously due to the risk of survivor bias
  
  -   There are some noticeable trends with respect to pain (largely moderate at each timepoint) and physical function over time (linear decay for tumour lower limb) that point to the challenging recovery patients face after treatment
  
  -   Procedure survival should be interpreted with some skepticism due to the competing risks of mortality and contralateral surgery which has not been accounted for in the kaplan-meier curves so far.
  
  -   Nevertheless there are distinct patterns of survival between primary and revision presentations for all cohorts.
  
  # Recommendations
  
  -   Continuing efforts to retrieve PROMs information from patients
  
  -   Engage with patients regarding their results using the web-based COMPRESSOR portal.
  
  -   Consider capture of intraoperative data on prospective or retrospective cases
  
  -   Refine diagnosis coding within cohorts
  
  -   Implement strategies to improve patient engagement at later followup timepoints
  
  -   Identify additional opportunities for local/state data linkage activities to supplement outcomes tracking (complications, mortality, procedure survival)
  
  
  ```{r}
  #| label: fig-msts-tp
  #| fig-cap: "MSTS scores over time: Tumour Pelvis (surgical cases)"
  
  
  # Calculate counts for each timepoint
  count_data <- PROMMSTS |> 
    dplyr::filter(
      SurgicalTreatment2 == "Surgical",
      RegistryCohortName == "TumourPelvis",
      !is.na(MSTSLowerTotal)
    ) |> 
    dplyr::count(TimePoint, name = "n_responses")
  
  #preop_position <- which(levels(PROMGHI$TimePoint) == "Preop")
  
  FigureTPMSTS <- PROMMSTS |> dplyr::filter(
    SurgicalTreatment2 == "Surgical",
    RegistryCohortName == "TumourPelvis"
  ) |> ggplot(aes(y = MSTSLowerTotal, x = TimePoint)) +
    stat_halfeye(
      alpha = 0.5,  # Transparency for overlap visibility
      position = "identity",  # Overlay the distributions
      na.rm = TRUE,
      scale = 0.9  # Slightly scale down to avoid too much overlap
    ) +
    geom_text(
      data = count_data,
      aes(x = TimePoint, y = Inf, label = paste0("n=", n_responses)),
      vjust = 1.2,
      hjust = 0.5,
      size = 3.5,
      inherit.aes = FALSE
    ) +
    labs(
      y = "MSTS Total",
      x = "Time Point"
      # fill = "Treatment",
      # color = "Treatment"
    ) +
    theme_minimal() +
    theme(
      # legend.position = "top",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  
  knitr::knit_print(FigureTPMSTS)
  
  ```