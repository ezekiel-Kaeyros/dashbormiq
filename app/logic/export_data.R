box::use(
  magrittr[`%>%`],dplyr
)

box::use(
  app/logic/import_data
)

df <- import_data$data
df$typeOfDiscrimination <- gsub("c\\(", "", df$typeOfDiscrimination)
df$typeOfDiscrimination <- gsub("\\)", "", df$typeOfDiscrimination)

df$formOfQueerphobia <- gsub("c\\(", "", df$formOfQueerphobia)
df$formOfQueerphobia <- gsub("\\)", "", df$formOfQueerphobia) #haveYouReportedYes

df$haveYouReportedYes <- gsub("c\\(", "", df$haveYouReportedYes)
df$haveYouReportedYes <- gsub("\\)", "", df$haveYouReportedYes)

df$gender <- gsub("c\\(", "", df$gender)
df$gender <- gsub("\\)", "", df$gender)

df$sexualOrientation <- gsub("c\\(", "", df$sexualOrientation)
df$sexualOrientation <- gsub("\\)", "", df$sexualOrientation)

df$organizationType <- gsub("c\\(", "", df$organizationType)
df$organizationType <- gsub("\\)", "", df$organizationType)

df$formOfDiscYes <- gsub("c\\(", "", df$formOfDiscYes)
df$formOfDiscYes <- gsub("\\)", "", df$formOfDiscYes)

df <- df %>% dplyr::select(identity, gender, age,valueDate,startDate,endDate, location, locationOnline,
                           formOfDisc, formOfDiscYes, haveYouReported, haveYouReportedYes, typeOfDiscrimination,
                           stadtteil, datePeriod, organizationType, numberOfEmployees, formOfQueerphobia,
                           sexualOrientation,otherformOfQueerphobiaFreeField,genderFreeField,haveYouReportedYesFreeField1,
                           haveYouReportedYesFreeField2,organizationTypeFreeField,typeOfDiscriminationFreeField
                           ) 