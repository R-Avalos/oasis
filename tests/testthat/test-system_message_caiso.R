test_that("system_message_caiso() returns expected historcal value count", {
  expect_equal(
    length(system_message_caiso(
      from_date = as.Date("2024-03-01"),
      to_date = as.Date("2024-03-03"),
      message_severity = "Emergency"
    )$MSG_TEXT),
    3
  )
})
