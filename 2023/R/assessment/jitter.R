jitter <- function(dir = getwd(),
                   mydir = lifecycle::deprecated(),
                   Intern = lifecycle::deprecated(),
                   Njitter,
                   printlikes = TRUE,
                   jitter_fraction = NULL,
                   init_values_src = NULL,
                   exe = "ss3",
                   verbose = FALSE,
                   ...) {
  # deprecated variable warnings -----
  # soft deprecated for now, but fully deprecate in the future.
  if (lifecycle::is_present(Intern)) {
    lifecycle::deprecate_warn(
      when = "1.45.1",
      what = "jitter(Intern)",
      with = "jitter(show_in_console)"
    )
  }
  if (lifecycle::is_present(mydir)) {
    lifecycle::deprecate_warn(
      when = "1.46.0",
      what = "jitter(mydir)",
      with = "jitter(dir)"
    )
    dir <- mydir
  }
  
  # check for executable and keep cleaned name of executable file
  exe <- check_exe(exe = exe, dir = dir, verbose = verbose)[["exe"]]
  
  # Determine working directory on start and return upon exit
  startdir <- getwd()
  on.exit(setwd(startdir))
  setwd(dir)
  
  if (verbose) {
    message("Temporarily changing working directory to:\n", dir)
    if (!file.exists("Report.sso")) {
      message(
        "Copy output files from a converged run into\n",
        dir, "\nprior to running jitter to enable easier comparisons."
      )
    }
    message("Checking starter file")
  }
  # read starter file to test for non-zero jitter value
  starter <- SS_readstarter(verbose = verbose)
  starter[["parmtrace"]] <- ifelse(starter[["parmtrace"]] == 0, 1, starter[["parmtrace"]])
  if (starter[["jitter_fraction"]] == 0 & is.null(jitter_fraction)) {
    stop("Change the jitter value in the starter file to be > 0\n",
         "or change the 'jitter_fraction' argument to be > 0.",
         call. = FALSE
    )
  }
  if (!is.null(jitter_fraction)) {
    starter[["jitter_fraction"]] <- jitter_fraction
  }
  if (!is.null(init_values_src)) {
    starter[["init_values_src"]] <- init_values_src
  }
  r4ss::SS_writestarter(starter, overwrite = TRUE, verbose = FALSE)
  
  # This is not necessary, but maintaining for back compatibility
  # file_increment(0, verbose = verbose)
  
  # check length of Njitter input
  if (length(Njitter) == 1) {
    Njitter <- 1:Njitter
  }
  
  likesaved <- furrr::future_map_dbl(Njitter, ~ iterate_jitter(
    i = .x,
    dir = dir,
    printlikes = printlikes,
    exe = exe,
    verbose = verbose,
    init_values_src = starter[["init_values_src"]],
    ...
  ))
  
  # Move original files back (also maintaining for back compatibility)
  pattern0 <- list.files(pattern = "[a-z_]0\\.sso")
  file.copy(
    from = pattern0,
    to = gsub("([a-zA-Z])0|_0\\.sso", "\\1", pattern0),
    overwrite = TRUE
  )
  
  if (printlikes) {
    message("Table of likelihood values:")
    print(table(likesaved))
  }
  return(invisible(likesaved))
}

iterate_jitter <- function(i,
                           dir = getwd(),
                           printlikes = TRUE,
                           exe = "ss",
                           verbose = FALSE,
                           init_values_src = 0,
                           ...) {
  jitter_dir <- file.path(dir, paste0("jitter", i))
  copy_SS_inputs(
    dir.old = dir, dir.new = jitter_dir, overwrite = TRUE,
    verbose = verbose, copy_exe = TRUE,
    copy_par = as.logical(init_values_src)
  )
  # run model
  r4ss::run(dir = jitter_dir, exe = exe, verbose = verbose, ...)
  # Only save stuff if it converged
  if ("Report.sso" %in% list.files(path = jitter_dir)) {
    rep <- SS_read_summary(file.path(jitter_dir, "ss_summary.sso"))
    if (is.null(rep)) {
      report <- SS_output(
        dir = jitter_dir, forecast = FALSE,
        covar = FALSE, NoCompOK = TRUE,
        verbose = verbose, warn = verbose, hidewarn = !verbose, printstats = verbose
      )
      like <- report[["likelihoods_used"]][row.names(report[["likelihoods_used"]]) == "TOTAL", "values"]
    } else {
      like <- rep[["likelihoods"]][grep("TOTAL", row.names(rep[["likelihoods"]])), 1]
    }
    if (printlikes) {
      message("Likelihood for jitter ", i, " = ", like)
    }
    # rename output files and move them to base model directory
    to_copy <- list.files(
      path = jitter_dir,
      pattern = "^[CcPRw][a-zA-Z]+\\.sso|summary\\.sso|\\.par$"
    )
    new_name <- gsub(
      pattern = "par",
      replacement = "par_",
      x = gsub(
        pattern = "\\.sso|(\\.par)",
        replacement = paste0("\\1", i, ".sso"),
        x = to_copy
      )
    )
    file.copy(
      from = to_copy,
      to = file.path(dir, new_name),
      overwrite = TRUE
    )
    # delete jitter model directory
    unlink(jitter_dir, recursive = TRUE)
    return(like)
  } else {
    unlink(jitter_dir, recursive = TRUE)
    if (verbose) warning("No Report.sso file found from run ", i)
  }
}