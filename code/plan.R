plan <-
  drake_plan(
    read_data = read_pisa(),
    process_data = select_few(read_data),
  )
