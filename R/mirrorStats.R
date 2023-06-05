#' Bioconductor Mirror Repository Statistics
#'
#' @aliases print.mirrorStats
#'
#' @description Summarize mirror packages and compare with the canonical
#'   repository
#'
#' @inheritParams checkBioCmirror
#'
#' @param version (Optional) `character(1)` or `package_version`
#'   indicating the _Bioconductor_ version (e.g., "3.8") for which
#'   repositories are required.
#'
#' @param ... further arguments passed to or from other methods (not used).
#'
#' @return a list of class `mirrorStats` with the following fields:
#'
#' * bioconductor_version: `package_version` the Bioconductor version given
#'   by `BiocManager::version` or by the user.
#'
#' * repository_exists: `logical(1)` TRUE if a mirror repository
#' exists for Bioconductor_Version version.
#'
#' * bioconductor_mirror: `character(1)` repository
#' location, if available, or NA if the repository does not exist.
#'
#' * n_software_packages: `integer(1)` number of software packages
#' in the Bioconductor source repository.
#'
#' * n_mirror_packages: `integer(1)` number of mirror packages
#' available. When a mirror repository exists, this number is likely
#' to be larger than the number of source software packages, because
#' it includes the mirror version of the source software packages, as
#' well as the (possibly CRAN) dependencies of the mirror packages
#'
#' * n_mirror_software_packages: `integer(1)` number of binary
#' packages derived from Bioconductor source packages. This number is
#' less than or equal to `n_software_packages`.
#'
#' * missing_pkgs: `integer(1)` the number of Bioconductor
#' source software packages that are not present in the binary
#' repository.
#'
#' * out_of_date_binaries: `integer(1)` the number of Bioconductor
#' source software packages that are newer than their binary
#' counterpart. A newer source software package
#' might occur when the main Bioconductor build system has
#' updated a package after the most recent run of the binary
#' build system.
#'
#' @author M. Morgan, M. Ramos
#'
#' @importFrom utils available.packages
#' @importFrom httr HEAD headers
#'
#' @examples
#' if (interactive()) {
#'   stats <- mirrorStats() # obtain statistics
#'   stats                       # display a summary
#' }
#'
#' @export
mirrorStats <- function(
    version = BiocManager::version(),
    mirror = getOption("BioC_mirror"),
    repoType =
        c("BioCsoft", "BioCann", "BioCexp", "BioCworkflows", "BioCbooks")
) {
    repo_type <- match.arg(repoType)
    if (is.null(mirror)) {
        utils::chooseBioCmirror()
        mirror <- getOption("BioC_mirror")
    }
    bioc_repository <- suppressMessages({
        BiocManager::repositories()[[repo_type]]
    })
    db_bioc <- available.packages(repos = bioc_repository)
    if (length(mirror)) {
        db_mirror <- available.packages(repos = mirror)
        packages <- paste0(utils::contrib.url(mirror), "/PACKAGES")
        response <- HEAD(packages)
        last_modified <- headers(response)$`last-modified`
        PACKAGES_mtime <-
            format(strptime(
                last_modified, "%a, %d %b %Y %H:%M", tz = "UTC"
            ), usetz = TRUE)
    } else {
        db_mirror <- db_bioc[NULL,]
        PACKAGES_mtime <- NA_character_
    }

    missing_pkgs <- setdiff(rownames(db_bioc), rownames(db_mirror))
    found_pkgs <- intersect(rownames(db_bioc), rownames(db_mirror))

    bioc_versions <- package_version(db_bioc[found_pkgs, "Version"])
    mirror_versions <- package_version(db_mirror[found_pkgs, "Version"])
    mirror_out_of_date <- bioc_versions > mirror_versions
    n_out_of_date_binaries <- sum(mirror_out_of_date)
    out_of_date_binaries <- found_pkgs[mirror_out_of_date]

    query_timestamp = format(
        Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC", usetz = TRUE
    )

    result <- list(
        bioconductor_version = version,
        bioconductor_mirror =
            if (length(mirror)) mirror else NA_character_,
        PACKAGES_mtime = PACKAGES_mtime,
        query_timestamp = query_timestamp,
        repository_exists = length(mirror) > 0L,
        n_software_packages = nrow(db_bioc),
        n_mirror_packages = nrow(db_mirror),
        n_mirror_software_packages = length(found_pkgs),
        missing_pkgs = missing_pkgs,
        out_of_date_binaries = out_of_date_binaries
    )
    class(result) <- c("mirrorStats", class(result))
    result
}

.mirrorStats_package_format <-
    function(x)
{
    msg <- paste(sort(x), collapse = " ")
    paste(strwrap(msg, indent = 2L, exdent = 2L), collaspe = "\n")
}

#' @describeIn mirrorStats Print a summary of package
#'     availability in mirror repositories.
#'
#' @param x the object returned by `mirrorStats()`.
#'
#' @export
print.mirrorStats <-
    function(x, ...)
{
    bioconductor_mirror <- ifelse(
        is.na(x$bioconductor_mirror),
        paste(" ", x$bioconductor_mirror),
        paste("\n  ", x$bioconductor_mirror)
    )
    cat(
        "Bioconductor version: ", as.character(x$bioconductor_version), "\n",
        "Bioconductor mirror:", bioconductor_mirror, "\n",
        "PACKAGES timestamp: ", x$PACKAGES_mtime, "\n",
        "Query timestamp: ", x$query_timestamp, "\n",
        "Bioconductor software packages: ", x$n_software_packages, "\n",
        "Mirror packages: ", x$n_mirror_packages, "\n",
        "Mirror software packages: ", x$n_mirror_software_packages, "\n",
        "Missing mirror software packages: ", length(x$missing_pkgs), "\n",
        if (x$repository_exists)
            .mirrorStats_package_format(x$missing_pkgs),
        "Out-of-date mirror software packages: ",
            length(x$out_of_date_binaries), "\n",
        if (x$repository_exists)
            .mirrorStats_package_format(x$out_of_date_binaries),
        sep = ""
    )
}

