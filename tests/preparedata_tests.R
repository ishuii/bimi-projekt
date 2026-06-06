
test_df1 <- data.frame(
  V1 = c("1", "4", "a", NA, "10"),
  V2 = c("2", NA, "b", NA, "20"),
  V3 = c("3", "6", "7", NA, "30"),
  stringsAsFactors = FALSE
)

test_df2 <- data.frame(
  V1 = c("1", "4", "a", "10", "b"),
  V2 = c("2", NA, "b", "20", "NA"),
  V3 = c("3", "6", "7", "30", "b"),
  stringsAsFactors = FALSE
)

test_df3 <- data.frame(
  V1 = c("1", "4", "a", "10", "b"),
  V2 = c("2", NA, "b", "20", "6"),
  V3 = c("3", "6", "7", "30", "3"),
  stringsAsFactors = FALSE
)



prepare_data(test_df1)  #both dont work as there are rows with only NAs / non-numeric
prepare_data(test_df2)
prepare_data(test_df3)


