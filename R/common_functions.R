#' check_trialids
#'
#'Checks that trialid's of an experiment in a folder follow the stantard expected rules
#' @param local_folder_tasks local folder to check
#'
#' @return
#' @export
#'
#' @examples
#' ""
check_trialids <- function(local_folder_protocol) {

  suppressMessages(suppressWarnings(library(dplyr)))
  suppressMessages(suppressWarnings(library(purrr)))
  suppressMessages(suppressWarnings(library(readr)))

  scripts = dir(path = paste0(local_folder_protocol, "/tasks"), pattern = ".js", recursive = TRUE, full.names = TRUE)
  if (length(scripts) == 0) stop(paste("Can't find anything in ", local_folder_protocol))

  find_trialids <- function(file_name) {

    # DEBUG
    # file_name = scripts[1]

    script = read_file(file_name)
    expres = ".*?trialid: '(.*?)'.*?"
    trialid = gsub(expres, "\\1; \n", script) %>% gsub("^(.*; \n).*", "\\1", .) %>% gsub(";", "", .) %>% gsub(" number \n", "", .)
    if (grepl("This document was made with test_maker", trialid)) trialid = ""
    strsplit(trialid, " \n")[[1]] %>% as_tibble() %>%
      mutate(file = file_name) %>%
      rename(trialid = value) %>%
      filter(!grepl("^Instructions|^Instructions_[0-9]{2}|^Fullscreen", trialid))

  }


  DF_all_trialids = map_df(scripts, find_trialids)

  rule_check_trialids = "^[a-zA-Z0-9]{1,100}_[0-9]{2,3}$|^[a-zA-Z0-9]{1,100}_[0-9]{2,3}_[0-9]{1,3}$" # NAME_001, NAMEexperiment_001_1
  DF_problematic_trialids =
    DF_all_trialids %>%
    filter(!grepl(rule_check_trialids, trialid)) %>%
    mutate(experiment = basename(file)) %>%
    select(-file)

  if (nrow(DF_problematic_trialids) > 0) {

    cat(crayon::red(nrow(DF_problematic_trialids), "ISSUES:\n"),
        "- experiment:", paste(DF_problematic_trialids %>% pull(experiment), collapse = ", "), "\n",
        "- trialid:   ", paste(DF_problematic_trialids %>% pull(trialid), collapse = ", "), "\n")

  } else {
    cat(crayon::green("All trialid's look great!\n"))
  }
}




#' debug_function
#'
#' Loads the parameters used in the functions present in _targets.R to make debugging easier
#'
#' @param name_function name of the function to debug
#'
#' @return
#' @export
#'
#' @examples
#' ""
debug_function <- function(name_function) {

  # DEBUG
  # name_function = "prepare_CRS"

  # Function to tar_load or assign the parameters
  load_parameters <- function(parameters_function_separated, NUM) {
    if (length(parameters_function_separated[[NUM]]) == 1) {
      targets::tar_load(parameters_function_separated[[NUM]], envir = .GlobalEnv)
    } else if (length(parameters_function_separated[[NUM]]) == 2) {
      assign(parameters_function_separated[[NUM]][1], parameters_function_separated[[NUM]][2], envir = .GlobalEnv)
    }
  }


  # Makes possible to use prepare_TASK or "prepare_TASK"
  if (substitute(name_function) != "name_function") name_function = substitute(name_function) #if (!interactive()) is so substitute do not overwrite name_function when in interactive mode

  # Parses _targets.R
  code <- parse("_targets.R")
  if (file.exists("targets/targets_main.R")) code <- c(code, parse("targets/targets_main.R"))
  # code <- parse("_targets.R")

  # Finds the chunk where name_function is, and cleans the "\"
  text_targets = grep(name_function, code, value = TRUE) %>% gsub("[^A-Za-z0-9\\(\\),_= ]", "", .)

  # Gets and separates then parameters of the function
  parameters_function_raw = gsub(paste0(".*", name_function, "\\((.*?)).*"), "\\1", text_targets) %>% gsub(" ", "", .)

  if (length(parameters_function_raw) > 0) {

    parameters_function_separated = strsplit(parameters_function_raw, ",") %>% unlist() %>% strsplit(., "=")

    # For each of the parameters, applies the load_parameters() function
    TEMP = seq_along(parameters_function_separated) %>% map(~ load_parameters(parameters_function_separated, NUM = .x))
    cat(crayon::green("Loaded: "), gsub(",", ", ", parameters_function_raw), "\n")

  } else {

    cat(crayon::red(paste0("'", name_function, "'", "not found in _targets.R")), "\n")

  }
}




#' sync_server_local
#'
#' Sync files between server and local or viceversa.
#'
#' @param server_folder folder in server
#' @param local_folder folder in local computer
#' @param direction local_to_server or server_to_local
#' @param only_test TRUE/FALSE, dry run or sync
#'
#' @return
#' @export
#'
#' @examples
#' ""
sync_server_local <- function(server_folder, local_folder, direction, only_test = TRUE) {

  # DEBUG
  # server_folder = "test/FONDECYT2021/"
  # local_folder = "canonical_protocol_DEV/"
  # direction = "server_to_local"

  if (only_test == TRUE) {
    extra_message = paste0(cli::col_red("THIS IS A dry-run"))
    dry_run = " --dry-run "
  } else {
    extra_message = ""
    dry_run = ""
  }

  local_folder = normalizePath(here::here(local_folder))
  local_folder_terminal = gsub(" ", "\\\\ ", local_folder)

  # CHECKS we have credentials and necessary software ------------------------

  credentials_exist = file.exists(".vault/.credentials")
  SSHPASS = Sys.which("sshpass") # Check if sshpass is installed
  RSYNC = Sys.which("rsync") # Check if rsync is installed

  if (credentials_exist) {
    # sshpass and rsync installed (?)
    if (SSHPASS != "" & RSYNC != "") {
      # cli::cli_text(cli::col_green("{cli::symbol$tick} "), "All is well.")
    } else {
      cli::cli_text(cli::col_red("{cli::symbol$cross} "), "'sshpass' or 'rsync' not installed. Can't use `sync_server_local()`")
    }
  } else {
    cli::cli_text(cli::col_red("{cli::symbol$cross} "), "Can find server credentials in '.vault/.credentials'")
  }



  # CHECK -------------------------------------------------------------------

  if (direction == "server_to_local") {
    message_text = paste0(cli::col_yellow("Will sync: "), cli::col_silver("cscn.uai.cl/", server_folder, " -->> ", local_folder), "\n", extra_message)
  } else if (direction == "local_to_server") {
    message_text = paste0(cli::col_yellow("Will sync: "), cli::col_silver(local_folder, " -->> ", "cscn.uai.cl/", server_folder), "\n", extra_message)
  } else {
    cli::cli_text(cli::col_red("{cli::symbol$cross} "), "direction should be either 'server_to_local' or 'local_to_server'")
    stop()
  }

  out <- utils::menu(c("yes", "no"), title = cat(message_text))


  # SYNC --------------------------------------------------------------------

  # Get server credentials
  list_credentials = source(".vault/.credentials")


  if (out == 1) {

    if (direction == "server_to_local") {

      # DOWNLOAD server to local
      system(
        paste0('sshpass -p ', list_credentials$value$password, ' rsync -av ', dry_run, ' --rsh=ssh ',
               list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ ',
               here::here(local_folder_terminal), '/ '
        )
      )


    } else if (direction == "local_to_server") {

      # UPLOAD local to server
      system(
        paste0('sshpass -p ', list_credentials$value$password, ' rsync -av ', dry_run, ' --rsh=ssh ',
               here::here(local_folder_terminal), '/ ',
               list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, server_folder, '/ '
        )
      )

    }

  } else {
    cat(crayon::green("Not doing anything..."))
  }

}








# OVERLAPS WITH sync_server_local -----------------------------------------------



#' update_data
#'
#' Update data/id_protocol folder using rsync
#'
#' @param id_protocol id
#' @param sensitive_tasks sensitive tasks (files to .vault)
#'
#' @return
#' @export
#'
#' @examples
#' ""
update_data <- function(id_protocol, sensitive_tasks = c("")) {

  # DEBUG
  # id_protocol = 0
  # sensitive_tasks = c("DEMOGR")

  cat(crayon::yellow(paste0("Synching files from pid ", id_protocol, "\n")))

  if (!dir.exists(paste0("data/", id_protocol, "/"))) stop("CAN'T find data/", id_protocol)

  if (!file.exists(".vault/.credentials")) {
    # If you do not have the .credentials file: rstudioapi::navigateToFile("setup/setup_server_credentials.R")
    cat(crayon::red("The file .vault/.credentials does NOT exist. Follow the steps in: "), "\n", crayon::yellow('rstudioapi::navigateToFile("setup/setup_server_credentials.R")\n'))
    stop("CAN'T find .vault/.credentials")
  }

  WD = gsub(" ", "\\ ", getwd(), fixed = TRUE) # Replace " " in path to avoid error
  list_credentials = source(".vault/.credentials")
  if (!dir.exists(paste0(getwd(), '/data/' , id_protocol, '/'))) dir.create(paste0(getwd(), '/data/' , id_protocol, '/'))
  system(paste0('sshpass -p ', list_credentials$value$password, ' rsync -av --rsh=ssh ', list_credentials$value$user, "@", list_credentials$value$IP, ":", list_credentials$value$main_FOLDER, id_protocol, '/.data/ ', WD, '/data/' , id_protocol, '/'))

  if (sensitive_tasks != "") {
    # MOVE sensitive data to .vault
    data_folder = paste0("data/", id_protocol)
    sensitive_files = list.files(data_folder, pattern = paste(sensitive_tasks, collapse = "|"), full.names = TRUE)

    destination_folder = paste0(".vault/data")
    destination_names = gsub(data_folder, destination_folder, sensitive_files)
    file.rename(from = sensitive_files, to = destination_names)

    cat(crayon::green(paste0("Moved ", length(destination_names), " files matching '", paste(sensitive_tasks, collapse = "|"), "' to ", destination_folder, "\n")))
  }

}
