test_that("find_valid_group_chip_IDs", {

  tinytest::expect_equal(query_groupID_limslager%>%get_EDL_from_query_result(),
                         EDL_groupID_limslager)

  tinytest::expect_equal(EDL_groupID_limslager%>%extract_chipIDs_from_groupEDL(),
                         c("M1730408", "M1730410", "M1730412", "M1730414", "M1730416",
                           "M1730418", "M1730420", "M1730422", "M1730424", "M1730426", "M1708179",
                           "M1708181", "M1730444", "M1730446", "M1708167", "M1708169", "M1708175",
                           "M1708177", "M1764682", "M1730404", "M1730406", "M1764730"))

})
