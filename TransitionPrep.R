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