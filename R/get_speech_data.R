#' Return all speeches of a speaker
#'
#' @param speaker Name of speaker - (Hiragana is acceptable for the member's name). Partial match search. If multiple single-byte spaces (U+0020) are specified as delimiters, the specified words will be OR-searched.
#' @param from Optional start date of search of the form YYYY-MM-DD
#' @param until Optional end date of search of the form YYYY-MM-DD
#' @param show_progress Boolean to show a progress bar; TRUE by default
#'
#' @examples
#' # Download all speech data for Shinzo Abe for the year 2006
#' abe_speeches_2006 <- get_speech_data(
#'   speaker = "安倍 晋三",
#'   from = "2006-01-01",
#'   until = "2006-12-31"
#' )
#'
#' @export
get_speech_data <- function(speaker, from = NULL, until = NULL, show_progress = TRUE) {
  res <- kokkai_call(
    unit = "speech",
    speaker = speaker,
    from = from,
    until = until,
    startRecord = 1,
    maximumRecords = 1
  )

  purrr::map_df(
    seq(from = 1, to = res$numberOfRecords, by = 100),
    \( i ) {
      res <- kokkai_call(
        unit = "speech",
        speaker = speaker,
        from = from,
        until = until,
        startRecord = i,
        maximumRecords = 100
      )
      res$speechRecord
    },
    .progress = show_progress
  ) |> tibble::as_tibble()
}
