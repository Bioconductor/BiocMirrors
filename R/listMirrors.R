.BIOC_MIRRORS_URL <- "https://bioconductor.org/BioC_mirrors.csv"

#' List Bioconductor mirrors
#'
#' @description The function reads the Bioconductor mirrors CSV file and returns
#'   a tibble. See the details for the columns.
#'
#' @details The function returns a tibble with the following columns:
#'   * `Name`: The name of the mirror
#'   * `Country`: The country of the mirror
#'   * `City`: The city of the mirror
#'   * `URL`: The URL of the mirror
#'   * `Host`: Institution hosting the mirror
#'   * `Maintainer`: The designated maintainer of the mirror
#'   * `OK`: Whether the mirror is OK
#'   * `CountryCode`: The country code of the mirror
#'   * `Comment`: A comment about the mirror
#'   * `Protocol`: The protocol of the mirror either `https` or `http`
#'     (parsed and added from URL column)
#'
#' @md
#'
#' @importFrom tibble as_tibble
#'
#' @return A tibble with the following columns:
#'
#' @export
listMirrors <- function() {
    mirrordf <- utils::read.csv(.BIOC_MIRRORS_URL)
    mirrordf[["Protocol"]] <- gsub("(http[s]?).*", "\\1", mirrordf[["URL"]])
    mirrordf |> tibble::as_tibble()
}
