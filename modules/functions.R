#shinyFiles::getVolumes() hardwires /media that doesn't exist on some Linux
#distribution and causes problem.
get_vols <- function()
{
  os <- Sys.info()["sysname"]

  root <- switch(os,
                 Darwin = c(root = "/Volumes"),
                 Linux = c(root = "/"),
                 Windows = {
                   wmic <- system("wmic logicaldisk get Caption,Volumename", intern = TRUE)
                   wmic <- strsplit(wmic, "\\s+")
                   ans <- NULL
                   for (i in seq_along(wmic)) {
                     if (grepl(":", wmic[[i]], fixed = TRUE)) {
                       ans[wmic[[i]][2]] <- wmic[[i]][1]
                     }
                   }
                   ans
                 },
                 stop("Unknown OS"))

  home <- c(home = path.expand("~"))

  c(home, root)
}

calc_intv <- function(abf,
                      manual_intv = FALSE, intv_start, intv_len,
                      manual_vol = FALSE, vol_delta, noisy_opt = FALSE,
                      common_intv = FALSE,
                      use_epoch = FALSE) {

  if (!abftools:::IsAbfList(abf)) {
    abf <- list(abf)
  }
  if (manual_intv) {
    intv <- Intv(startPos = intv_start, len = intv_len)
    abftools:::MatchList(intv, length(abf))
  } else {
    if (!manual_vol) {
      vol_delta <- NULL
    }
    if (common_intv) {
      intv <- FindSamplingInterval(abf, noisy_opt = noisy_opt, allowed_voltage_delta = vol_delta)
    } else {
      intv <- FindAllSamplingInterval(abf, noisy_opt = noisy_opt, allowed_voltage_delta = vol_delta)
    }
    #incase intv is not a list (only 1 abf)
    if (!is.list(intv)) {
      intv <- list(intv)
    }
    if (use_epoch) {
      epo <- GetMultiStepEpoch(abf[[1]])[1]
      intv <- lapply(seq_along(intv), function(id) {
        if (any(is.na(intv[[id]]))) {
          epoch <- GetEpochIntervals(abf[[id]])
          epoch <- epoch[, 1, epo]
          skip <- floor(epoch[3] * 0.1)
          Intv(startPos = epoch[1] + skip, endPos = epoch[2] - skip)
        } else {
          intv[[id]]
        }
      })
    }
    intv
  }
}
