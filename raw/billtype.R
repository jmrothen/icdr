# as of 4/30/2023
# https://med.noridianmedicare.com/web/jea/topics/claim-submission/bill-types

d1 <- 0
d2 <- 1:9
d3 <- 1:9
d4 <- c(0:5,7:9,c('A','B','C','D','E','F','G','H','I','J','K','M','O','P','Q','X','Y','Z'))

data.frame(d1,d2) %>%
  merge(d3, all=T) %>%
  merge(data.frame(d4),all=T) %>%
  arrange(d1,d2,y,d4)-> dd
colnames(dd)[3] <- 'd3'

dd$billtype<- paste(dd$d1,dd$d2,dd$d3,dd$d4,sep='')
dd$billtype3 <-  paste(dd$d2,dd$d3,dd$d4,sep='')

dd$facility_type <- case_when(
  dd$d2 == 1 ~ 'Hospital',
  dd$d2 == 2 ~ 'Skilled Nursing',
  dd$d2 == 3 ~ 'Home Health',
  dd$d2 == 4 ~ 'Religious Nonmedical (Hospital)',
  dd$d2 == 5 ~ 'Reserved for national assignment (discontinued effective 10/1/05)',
  dd$d2 == 6 ~ 'Intermediate Care',
  dd$d2 == 7 ~ 'Clinic or Hospital Based Renal Dialysis Facility',
  dd$d2 == 8 ~ 'Special facility or hospital (Critical Access Hospital), Ambulatory Surgical Center (ASC) surgery',
  dd$d2 == 9 ~ 'Reserved for National Assignment'
)

dd$care_type <- case_when(
  dd$d3 == 1 & !(dd$d2 %in% c(7,8)) ~ 'Inpatient',
  dd$d3 == 2 & !(dd$d2 %in% c(7,8)) ~ 'Inpatient',
  dd$d3 == 3 & !(dd$d2 %in% c(7,8)) ~ 'Outpatient',
  dd$d3 == 4 & !(dd$d2 %in% c(7,8)) ~ 'Other',
  dd$d3 == 5 & !(dd$d2 %in% c(7,8)) ~ 'Intermediate Care Level 1',
  dd$d3 == 6 & !(dd$d2 %in% c(7,8)) ~ 'Intermediate Care Level 2',
  dd$d3 == 7 & !(dd$d2 %in% c(7,8)) ~ 'Subacute Inpatient Eight Swing Beds',
  dd$d3 == 8 & !(dd$d2 %in% c(7,8)) ~ 'N/A',
  dd$d3 == 9 & !(dd$d2 %in% c(7,8)) ~ 'Reserved for National Assignment',

  dd$d3 == 1 & dd$d2 == 7 ~ 'Rural Health Center (RHC)',
  dd$d3 == 2 & dd$d2 == 7 ~ 'Hospital based or Independent Renal Dialysis Center',
  dd$d3 == 3 & dd$d2 == 7 ~ 'Unused, previously Free Standing Provider-Based Federally Qualified Health Center (FQHC)',
  dd$d3 == 4 & dd$d2 == 7 ~ 'Other Rehabilitation Facility (ORF)',
  dd$d3 == 5 & dd$d2 == 7 ~ 'Comprehensive Outpatient Rehabilitation Facility (CORF)',
  dd$d3 == 6 & dd$d2 == 7 ~ 'Community Mental Health Center (CMHC)',
  dd$d3 == 7 & dd$d2 == 7 ~ 'Free-standing Provider-based Federally Qualified Health Center (FQHC)',
  dd$d3 == 8 & dd$d2 == 7 ~ 'Reserved for National Assignment',
  dd$d3 == 9 & dd$d2 == 7 ~ 'Other',

  dd$d3 == 1 & dd$d2 == 8 ~ 'Hospice (non-hospital based)',
  dd$d3 == 2 & dd$d2 == 8 ~ 'Hospice (hospital based)',
  dd$d3 == 3 & dd$d2 == 8 ~ 'ASC Services to Hospital Outpatients',
  dd$d3 == 4 & dd$d2 == 8 ~ 'Free Standing Birthing Center',
  dd$d3 == 5 & dd$d2 == 8 ~ 'Critical Access Hospital (CAH)',
  dd$d3 == 6 & dd$d2 == 8 ~ 'Residential Facility (not used for Medicare)',
  dd$d3 == 7 & dd$d2 == 8 ~ 'Reserved for National Assignment',
  dd$d3 == 8 & dd$d2 == 8 ~ 'Reserved for National Assignment',
  dd$d3 == 9 & dd$d2 == 8 ~ 'Other',
)

dd$frequency_type <- case_when(
  dd$d4 == '0' ~ 'Non-payment/Zero Claim',
  dd$d4 == '1' ~ 'Admit Through Discharge',
  dd$d4 == '2' ~ 'Interim - First Claim',
  dd$d4 == '3' ~ 'Interim-Continuing Claims (Not valid for Prospective Payment System (PPS) Bills)',
  dd$d4 == '4' ~ 'Interim - Last Claim (Not valid for PPS Bills)',
  dd$d4 == '5' ~ 'Late Charge Only',
  dd$d4 == '7' ~ 'Replacement of Prior Claim (See adjustment third digit)',
  dd$d4 == '8' ~ 'Void/Cancel of Prior Claim (See adjustment third digit)',
  dd$d4 == '9' ~ 'Final claim for a Home Health PPS Period',
  dd$d4 == 'A' ~ 'Admission/Election Notice for Hospice',
  dd$d4 == 'B' ~ 'Hospice Termination/ Revocation Notice',
  dd$d4 == 'C' ~ 'Hospice Change of Provider Notice',
  dd$d4 == 'D' ~ 'Hospice Election Void/Cancel',
  dd$d4 == 'E' ~ 'Hospice Change of Ownership',
  dd$d4 == 'F' ~ 'Beneficiary Initiated Adjustment Claim',
  dd$d4 == 'G' ~ 'CWF Initiated Adjustment Claim',
  dd$d4 == 'H' ~ 'CMS Initiated Adjustment Claim',
  dd$d4 == 'I' ~ 'FI Adjustment Claim (Other than QIO or Provider)',
  dd$d4 == 'J' ~ 'Initiated Adjustment Claim/Other',
  dd$d4 == 'K' ~ 'OIG Initiated Adjustment Claim',
  dd$d4 == 'M' ~ 'MSP Initiated Adjustment Claim',
  dd$d4 == 'O' ~ 'Nonpayment/Zero Claims',
  dd$d4 == 'P' ~ 'QIO Adjustment Claim',
  dd$d4 == 'Q' ~ 'Reopening/Adjustment',
  dd$d4 == 'X' ~ 'Void/Cancel a Prior Abbreviated Encounter Submission',
  dd$d4 == 'Y' ~ 'Replacement of Prior Abbreviated Encounter Submission',
  dd$d4 == 'Z' ~ 'New Abbreviated Encounter Submission'
)

billtype <- dd

colnames(dd) <- c('digit1','digit1','digit3','digit4','billtype','billtype3','facility_type','care_type','frequency_type')

usethis::use_data(billtype, overwrite=T)
