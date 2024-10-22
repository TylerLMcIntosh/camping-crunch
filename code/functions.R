

#' Install and Load Required Packages Using pak
#'
#' This function checks if the specified packages (both CRAN and GitHub) are installed and loads them. 
#' If any packages are missing, it offers to install them automatically or asks for user permission.
#' It uses the `pak` package for faster and more efficient package installation.
#'
#' @param package_list A list of package names to check and install (non-string, e.g., `c(dplyr, here)`).
#' GitHub packages should be specified as `username/repo` in strings.
#' @param auto_install A character ("y" or "n", default is "n"). If "y", installs all required packages 
#' without asking for user permission. If "n", asks for permission from the user.
#' @return No return value. Installs and loads the specified packages as needed.
#' @examples
#' \dontrun{
#' install_and_load_packages(c(dplyr, here, "username/repo"))
#' }
#' @importFrom pak pkg_install
#' @export
install_and_load_packages <- function(package_list, auto_install = "n") {
  # Convert non-string package names to strings
  package_list <- lapply(package_list, function(pkg) {
    if (is.symbol(pkg)) {
      deparse(substitute(pkg))
    } else {
      pkg
    }
  })
  
  # Check if pak is installed; install if not
  if (!requireNamespace("pak", quietly = TRUE)) {
    cat("The 'pak' package is required for fast installation of packages.\n")
    response <- if (auto_install == "y") "y" else readline(prompt = "\nDo you want to install the 'pak' package? (y/n): ")
    if (tolower(response) == "y") {
      install.packages("pak")
    } else {
      stop("Installation cannot proceed without 'pak'. Please install it manually and rerun.")
    }
  }
  
  # Initialize lists to store missing CRAN and GitHub packages
  missing_cran_packages <- c()
  missing_github_packages <- c()
  
  # Helper function to get user input
  get_user_permission <- function(prompt_msg) {
    if (auto_install == "y") {
      return("y")
    } else {
      return(tolower(readline(prompt = prompt_msg)))
    }
  }
  
  # Check for missing packages
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      package_loaded <- require(package_name, character.only = TRUE, quietly = TRUE)
    } else { # CRAN package
      package_loaded <- require(pkg, character.only = TRUE, quietly = TRUE)
    }
    if (!package_loaded) {
      if (grepl("/", pkg)) {
        missing_github_packages <- c(missing_github_packages, pkg)
      } else {
        missing_cran_packages <- c(missing_cran_packages, pkg)
      }
    }
  }
  
  # Install missing CRAN packages using pak::pkg_install
  if (length(missing_cran_packages) > 0) {
    cat("The following CRAN packages are missing: ", paste(missing_cran_packages, collapse = ", "), "\n")
    response <- get_user_permission("\nDo you want to install the missing CRAN packages? (y/n): ")
    if (response == "y") {
      pak::pkg_install(missing_cran_packages, upgrade = TRUE)
    } else {
      cat("Skipping installation of missing CRAN packages.\n")
    }
  }
  
  # Install missing GitHub packages using pak::pkg_install
  if (length(missing_github_packages) > 0) {
    cat("The following GitHub packages are missing: ", paste(missing_github_packages, collapse = ", "), "\n")
    response <- get_user_permission("\nDo you want to install the missing GitHub packages? (y/n): ")
    if (response == "y") {
      pak::pkg_install(missing_github_packages, upgrade = TRUE)
    } else {
      cat("Skipping installation of missing GitHub packages.\n")
    }
  }
  
  # Load all packages after checking for installation
  for (pkg in package_list) {
    if (grepl("/", pkg)) { # GitHub package
      package_name <- unlist(strsplit(pkg, "/"))[2]
      if (!require(package_name, character.only = TRUE)) {
        cat("Failed to load GitHub package:", package_name, "\n")
      }
    } else { # CRAN package
      if (!require(pkg, character.only = TRUE)) {
        cat("Failed to load CRAN package:", pkg, "\n")
      }
    }
  }
  
  cat("All specified packages installed and loaded.\n")
}

#' Ensure Directory Exists
#'
#' This function checks if a directory exists at the specified path, and if not, creates a new directory.
#'
#' @param path A character string specifying the path to the new directory.
#' @return The function does not return any value. It creates a directory if it does not already exist.
#' @examples
#' # Ensure a directory named "data" exists
#' dir_ensure("data")
#'
#' @export
dir_ensure <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path)
    message("Directory created: ", path)
  } else {
    message("Directory already exists: ", path)
  }
}
