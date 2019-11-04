#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyFiles)
library(DT)

library(ggplot2)
library(plotly)

library(abftools)

shinyServer(function(input, output, session) {

  #stores everything
  rv <- reactiveValues()

  #general functions----
  any_null_list <- function(x) {
    if (is.null(x) || !length(x)) {
      TRUE
    } else {
      any(sapply(x, is.null)) || any(!sapply(x, length))
    }
  }

  any_null <- function(...) {
    x <- list(...)
    any(sapply(x, any_null_list))
  }

  #base reactives----
  #initialise dir chooser
  vols <- get_vols()
  shinyDirChoose(input, id = "wd_loader", session = session, roots = vols)

  #wd & data index----
  get_wd <- reactive({
    wd <- parseDirPath(vols, input$wd_loader)
    if (any_null(wd)) {
      NULL
    } else {
      if (!file.exists(paste0(wd, "/index.csv")) || !dir.exists(paste0(wd, "/abf"))) {
        NULL
      } else {
        wd
      }
    }
  })
  get_abf_folder <- reactive({
    wd <- get_wd()
    if (any_null(wd)) {
      NULL
    } else {
      paste0(wd, "/abf")
    }
  })
  get_export_folder <- reactive({
    wd <- get_wd()
    if (any_null(wd)) {
      NULL
    } else {
      folder <- paste0(wd, "/export")
      if (!dir.exists(folder)) {
        dir.create(folder)
      }
      folder
    }
  })
  get_index <- reactive({
    wd <- get_wd()
    if (any_null(wd)) {
      NULL
    } else {
      tryCatch({
        fname <- paste0(wd, "/index.csv")
        v <- read.csv(fname)
        idx <- is.na(v[, 1L])
        v <- v[!idx, ]
        as.data.frame(lapply(v, function(col) {
          col <- factor(col)
          lvl <- levels(col)
          lvl[lvl == ""] <- "EMPTY_VAL"
          levels(col) <- lvl
          col
        }))
      }, error = function(e) {
        NULL
      })
    }
  })
  get_index_filtered <- reactive({
    idx <- get_index()
    row_idx <- input$index_rows_all
    if (any_null(idx, row_idx)) {
      NULL
    } else {
      idx[row_idx, ]
    }
  })
  get_index_var <- reactive({
    idx <- get_index_filtered()
    if (any_null(idx)) {
      NULL
    } else {
      cols <- colnames(idx)[-1]
      isna <- sapply(cols, function(col) {
        vals <- as.character(idx[[col]])
        "" %in% vals || anyNA(idx[[col]])
      })
      cols[!isna]
    }
  })

  #Index Panel----
  #Update group_by
  observe({
    v <- get_index_var()
    if (any_null(v)) {
      updateCheckboxGroupInput(session, "group_by", choices = character(0), selected = character(0))
    } else {
      updateCheckboxGroupInput(session, "group_by", choices = v, selected = character(0))
    }
  })
  #render index
  output$index <- DT::renderDataTable(get_index(), selection = "single", filter = 'top', options = list(pageLength = 10, autoWidth = TRUE))
  output$index_plt_waveform <- renderPlotly({
    idx <- get_index()
    row_id <- input$index_rows_selected
    if (any_null(idx, row_id)) {
      NULL
    } else {
      fname <- paste0(idx[row_id, 1L], ".abf")
      abf <- abf2_load(fname, folder = get_abf_folder())
      ggplotly(
        PlotChannelAbf(abf,
                       channel = GetAllChannels(abf),
                       time_unit = "s",
                       colour = TRUE,
                       title = as.character(idx[row_id, 1L]),
                       sample_ratio = input$index_plt_sample,
                       sample_func = "mean") +
          labs(y = "") +
          theme_bw()
      )
    }
  })
  #rv$abf_index----
  observe({

    index <- get_index_filtered()
    grp_by <- input$group_by

    if (any_null(index, grp_by)) {
      rv$abf_index <- NULL
    } else {
      id_col <- index[, 1L]
      grp_var <- index[grp_by]
      group <- interaction(grp_var, sep = "_")
      abf_index <- cbind(id = id_col, grp_var, group = group)
      rv$abf_index <- data.frame(lapply(abf_index, factor))
    }
  })

  #rv$intv_setting----
  intv_setting_default <- list(
    manual_intv = FALSE,
    intv_start = 1000L,
    intv_len = 200L,
    noisy_opt = FALSE,
    use_epoch = TRUE,
    manual_vol = FALSE,
    vol_delta = 15.0,
    common_intv = FALSE)
  #intv_setting_names <- names(intv_setting_default)
  observe({

    abf_index <- rv$abf_index

    if (any_null(abf_index)) {
      rv$intv_setting <- NULL
    } else {
      grps <- unique(abf_index$group)
      intv_setting <- lapply(grps, function(grp) {
        intv_setting_default
      })
      names(intv_setting) <- as.character(grps)
      rv$intv_setting <- intv_setting
    }
  })

  #rv$abf----
  observe({

    abf_folder <- get_abf_folder()
    abf_index <- get_index_filtered()

    if (any_null(abf_folder, abf_index)) {
      rv$abf <- NULL
    } else {
      id_col <- as.character(abf_index[, 1L])
      withProgress({
        abf <- lapply(id_col, function(fname) {
          incProgress(amount = 1, message = paste("Loading", fname))
          if (!endsWith(fname, ".abf")) {
            fname_abf <- paste0(fname, ".abf")
          } else {
            fname_abf <- fname
          }
          abf2_load(filename = fname_abf, folder = abf_folder, abf_title = fname)
        })
      }, max = length(id_col), message = "Loading files")
      names(abf) <- id_col
      rv$abf <- abf
    }
  })

  #rv$abf_bl----
  observe({

    abf <- rv$abf

    if (input$intv_baseline_switch == "disabled") {
      print("rv$abf_bl reset")
      rv$abf_bl <- abf

    }
  })
  observeEvent(input$intv_baseline_apply, {

    abf <- rv$abf

    print("rv$abf_bl")

    if (any_null(abf)) {
      return()
    }

    switch(input$intv_baseline_switch,
           disabled = return(),
           episode = {
             print("rv$abf_bl episode")
             epi <- input$intv_baseline_epi
             if (any_null(epi)) {
               return()
             }
             epi <- as.integer(epi)
             id <- names(abf)
             withProgress({
               abf <- lapply(id, function(id) {
                 incProgress(amount = 1, message = paste("Evaluating baseline", id))
                 abftools::RmblAbf(abf[[id]],
                                   channel = GetFirstCurrentChan(abf[[id]]),
                                   episode = epi,
                                   method = "episode")
               })
             }, max = length(id), message = "Evaluating baselines.")
             names(abf) <- id
             rv$abf_bl <- abf
           },
           holding = {
             print("rv$abf_bl epoch")
             idx_start <- input$intv_baseline_start
             idx_len <- input$intv_baseline_len
             if (any_null(idx_start, idx_len)) {
               return()
             }
             id <- names(abf)
             withProgress({
               abf <- lapply(id, function(id) {
                 incProgress(amount = 1, message = paste("Evaluating baseline", id))
                 abftools::RmblAbf(abf[[id]],
                                   channel = GetFirstCurrentChan(abf[[id]]),
                                   intv = Intv(startPos = idx_start, len = idx_len),
                                   method = "interval")
               })
             }, max = length(id), message = "Evaluating baselines.")
             names(abf) <- id
             rv$abf_bl <- abf
           })
  })

  #rv$intv----
  observe({

    abf <- rv$abf_bl
    abf_index <- rv$abf_index
    intv_setting <- rv$intv_setting

    if (any_null(abf, abf_index, intv_setting)) {
      rv$intv <- NULL
    } else {
      grps <- unique(abf_index$group)
      withProgress({
        intv <- tryCatch({
          lapply(as.character(grps), function(grp) {
            idx <- abf_index$group == grp
            id <- as.character(abf_index[idx, 1L])
            args_abf <- list(abf = abf[id])
            args_setting <- intv_setting[[grp]]
            incProgress(amount = 1, message = paste("Analysing group ", grp))
            intv <- do.call(calc_intv, args = c(args_abf, args_setting))
            names(intv) <- id
            intv
          })
        }, error = function(e) {
          showNotification(paste("There is an error occured when analysing sampling intervals.",
                                 "This is most likely due to inclusion of gap-free abf files.",
                                 "Please check your data. Returned error message:",
                                 e),
                           type = "error",
                           duration = NULL)
          print(e)
          NULL
        })
      }, max = length(grps))
      if (any_null_list(intv)) {
        rv$intv <- NULL
      } else {
        rv$intv <- do.call(c, args = intv)
      }
    }
  })

  #rv$plt_index----
  observe({

    abf_index <- rv$abf_index
    plt_by <- input$plot_by
    grp_by <- input$group_by

    if (any_null(abf_index, plt_by, grp_by)) {
      rv$plt_index <- NULL
    } else {
      plt_grps <- setdiff(grp_by, plt_by)
      if (any_null(plt_grps)) {
        rv$plt_index <- data.frame(id = abf_index$id, plt_by = abf_index[[plt_by]], plt_group = "ALL")
      } else {
        plt_group <- interaction(abf_index[plt_grps], sep = "_")
        rv$plt_index <- data.frame(id = abf_index$id, plt_by = abf_index[[plt_by]], plt_group = plt_group)
      }
    }

  })

  #rv$ivs----
  observe({

    plt_index <- rv$plt_index
    abf <- rv$abf_blank
    intv <- rv$intv
    iv_type <- input$iv_type_switch
    if (any_null(plt_index, abf, intv, iv_type)) {
      rv$ivs <- NULL
    } else {
      plt_by <- as.character(unique(plt_index$plt_by))
      withProgress({
        ivs <- lapply(plt_by, function(plt_by) {
          incProgress(amount = 1, message = paste("Analysing I-V group:", plt_by))
          idx <- plt_index$plt_by == plt_by
          plt_by_index <- plt_index[idx, ]
          grps <- as.character(unique(plt_by_index$plt_group))
          lapply(grps, function(grp) {
            idx <- plt_by_index$plt_group == grp
            id <- as.character(plt_by_index$id[idx])
            switch(input$iv_type_switch,
                   i = IVSummary(abf[id], intv[id], group = grp, unit = TRUE),
                   g = IGVSummary(abf[id], intv[id], group = grp, unit = TRUE),
                   spi = SpIVSummary(abf[id], intv[id], group = grp, unit = TRUE),
                   spg = SpIGVSummary(abf[id], intv[id], group = grp, unit = TRUE))
          })
        })
      }, max = length(plt_by))
      names(ivs) <- plt_by
      rv$ivs <- ivs
    }
  })
  #data export----
  observeEvent(input$exp_ivs, {

    plt_index <- rv$plt_index
    abf <- rv$abf_blank
    intv <- rv$intv

    if (any_null(plt_index, abf, intv)) {
      return()
    } else {
      plt_by <- as.character(unique(plt_index$plt_by))
      folder <- paste0(get_export_folder(), "/iv summary")
      if (!dir.exists(folder)) {
        dir.create(folder)
      }
      withProgress({
        tmp <- lapply(plt_by, function(plt_by) {
          incProgress(amount = 1, message = paste("Exporting group:", plt_by))
          idx <- plt_index$plt_by == plt_by
          plt_by_index <- plt_index[idx, ]
          grps <- as.character(unique(plt_by_index$plt_group))
          tmp <- lapply(grps, function(grp) {
            idx <- plt_by_index$plt_group == grp
            id <- as.character(plt_by_index$id[idx])
            if (input$exp_sp) {
              ivs <- SpIGVSummary(abf[id], intv[id], group = sprintf("%s-%s", plt_by, grp), unit = TRUE)
              fname <- sprintf("%s/specific-ivs-%s-%s.csv", folder, plt_by, grp)
            } else {
              ivs <- IGVSummary(abf[id], intv[id], group = sprintf("%s-%s", plt_by, grp), unit = TRUE)
              fname <- sprintf("%s/ivs-%s-%s.csv", folder, plt_by, grp)
            }
            write.csv(ivs, fname)
            ivs
          })
          tmp[[1]]
        })
      }, max = length(plt_by))
      output$export_preview <- renderDT(tmp[[1]])
      #TODO: also export LONG format
      withProgress({
        tmp <- lapply(plt_by, function(plt_by) {
          incProgress(amount = 1, message = paste("Composing I-V group:", plt_by))
          idx <- plt_index$plt_by == plt_by
          plt_by_index <- plt_index[idx, ]
          grps <- as.character(unique(plt_by_index$plt_group))
          tmp <- lapply(grps, function(grp) {
            idx <- plt_by_index$plt_group == grp
            id <- as.character(plt_by_index$id[idx])
            ChannelMeanLong(abf[id], intv[id], group = sprintf("%s-%s", plt_by, grp))
          })
          do.call(rbind, tmp)
        })
        fname <- sprintf("%s/long-iv.csv", folder)
        write.csv(do.call(rbind, tmp), fname)
      }, max = length(plt_by))
    }
  })

  observeEvent(input$exp_ivs_raw, {

    plt_index <- rv$plt_index
    abf <- rv$abf_blank
    intv <- rv$intv

    if (any_null(plt_index, abf, intv)) {
      return()
    } else {
      plt_by <- as.character(unique(plt_index$plt_by))
      folder <- paste0(get_export_folder(), "/iv individual")
      if (!dir.exists(folder)) {
        dir.create(folder)
      }
      withProgress({
        incProgress(amount = 1, message = "Calculating all individuals.")
        if (input$exp_sp) {
          ivs <- SpIGVSummary(abf, intv, raw = TRUE)
        } else {
          ivs <- IGVSummary(abf, intv, raw = TRUE)
        }
        incProgress(amount = 1, message = "Writing files.")
        tmp <- lapply(plt_by, function(plt_by) {
          idx <- plt_index$plt_by == plt_by
          plt_by_index <- plt_index[idx, ]
          grps <- as.character(unique(plt_by_index$plt_group))
          tmp <- lapply(grps, function(grp) {
            idx <- plt_by_index$plt_group == grp
            id <- as.character(plt_by_index$id[idx])
            tmp <- lapply(names(ivs), function(prop) {
              if (input$exp_sp) {
                fname <- sprintf("%s/specific-indivi-%s-%s-%s.csv", folder, plt_by, grp, prop)
              } else {
                fname <- sprintf("%s/indivi-%s-%s-%s.csv", folder, plt_by, grp, prop)
              }
              data <- ivs[[prop]][id]
              write.csv(data, fname)
              data
            })
            tmp[[1]]
          })
          tmp[[1]]
        })
      }, max = 2)
      output$export_preview <- renderDT(tmp[[1]])
    }
  })

  #rv$blnk_?----
  observe({

    abf <- rv$abf_bl

    if (input$iv_blank_switch == "disabled") {
      print("rv$abf_blnk reset")
      rv$abf_blank <- abf

      hide("iv_blank_by")
      hide("iv_blank_ref")
      hide("iv_blank_apply")
    } else {
      show("iv_blank_by")
      show("iv_blank_ref")
      show("iv_blank_apply")
    }
  })
  observe({

    abf_index <- rv$abf_index
    grp_by <- input$group_by
    blank_by <- input$iv_blank_by

    abf <- rv$abf_bl
    intv <- rv$intv
    blank_ref <- input$iv_blank_ref

    if (any_null(abf_index, grp_by, blank_by, abf, intv, blank_ref)) {
      rv$blank_index <- NULL
      return()
    } else {
      blank_grps <- setdiff(grp_by, blank_by)
      if (any_null(blank_grps)) {
        blank_index <- data.frame(id = abf_index$id, blank_by = abf_index[[blank_by]], blank_group = "ALL")
      } else {
        blank_group <- interaction(abf_index[blank_grps], sep = "_")
        blank_index <- data.frame(id = abf_index$id, blank_by = abf_index[[blank_by]], blank_group = blank_group)
      }
    }
    rv$blank_index <- blank_index

    blank_index <- blank_index[blank_index$blank_by == blank_ref, ]
    blank_grps <- as.character(unique(blank_index$blank_group))
    blank_ivs <- lapply(blank_grps, function(grp) {
      idx <- blank_index$blank_group == grp
      id <- as.character(blank_index$id[idx])
      IVSummary(abf[id], intv[id])
    })
    names(blank_ivs) <- blank_grps
    rv$blank_ivs <- blank_ivs

  }, priority = -5)
  observeEvent(input$iv_blank_apply, {

    abf <- rv$abf_bl
    blank_ivs <- rv$blank_ivs
    blank_index <- rv$blank_index

    if (input$iv_blank_switch == "disabled") {
      return()
    }

    if (any_null(abf, blank_index, blank_ivs)) {
      rv$abf_blank <- abf
      return()
    }

    id <- as.character(blank_index$id)
    withProgress({
      abf <- lapply(id, function(id) {
        incProgress(amount = 1, message = paste("Subtracing ", id))
        idx <- blank_index$id == id
        grp <- as.character(blank_index$blank_group[idx])
        if (any_null(blank_ivs[[grp]])) {
          showNotification(sprintf("Subtraction failed for %s since group %s does not exist.", id, grp), type = "warning", duration = NULL)
          abf[[id]]
        } else {
          BlnkAbf(abf[[id]], ref_data = blank_ivs[[grp]])
        }
      })
    }, max = length(id))
    names(abf) <- id

    rv$abf_blank <- abf
  })
  #selected group----
  grp_var_inputId <- function(var) {
    make.names(paste0("grp_var_", var))
  }
  output$intv_grp_sel <- renderUI({
    index <- rv$abf_index
    grp_var <- input$group_by
    if (is.null(grp_var) || is.null(index)) {
      NULL
    } else {
      sel <- lapply(grp_var, function(var) {
        id <- grp_var_inputId(var)
        val <- levels(index[, var])
        pickerInput(inputId = id,
                    label = var,
                    choices = val,
                    options = list(
                      `live-search` = TRUE
                    ))
      })
      header <- list(
        h4("Select a Group:"),
        hr()
      )
      tagList(c(header, sel))
    }
  })
  get_selected_grp <- reactive({
    grp_var <- input$group_by
    if (is.null(grp_var)) {
      NULL
    } else {
      grp <- sapply(grp_var, function(var) {
        id <- grp_var_inputId(var)
        input[[id]]
      })
      if (any_null_list(grp)) {
        NULL
      } else {
        make.names(paste0(grp, collapse = "_"))
      }
    }
  })
  get_selected_id <- reactive({
    grp <- get_selected_grp()
    abf_index <- rv$abf_index
    if (is.null(grp) || is.null(abf_index)) {
      NULL
    } else {
      idx <- abf_index$group == grp
      id <- as.character(abf_index$id[idx])
      if (!length(id)) {
        NULL
      } else {
        id
      }
    }
  })
  output$intv_grp_num <- renderText(sprintf("%d files in current group.", length(get_selected_id())))
  #seleceted group iv
  output$intv_grp_iv <- renderPlotly({

    abf <- rv$abf_bl
    intv <- rv$intv
    id <- get_selected_id()
    if (is.null(abf) || is.null(intv) || is.null(id)) {
      NULL
    } else {
      PlotIVAbf(abf[id], intv = intv[id], zero_axes = FALSE) + theme_bw()
    }
  })
  #selected group epi
  observeEvent({
    get_selected_id()
  }, {
    print("selected id changed, updating Episode list.")
    abf <- rv$abf_bl
    id <- get_selected_id()
    if (is.null(abf) || is.null(id)) {
      return()
    }
    abf <- abf[id]
    epi <- unique(sapply(abf, nEpi, USE.NAMES = FALSE))
    if (length(epi) > 1L) {
      showNotification("Episode/Sweep settings are not unified in selected group. Baseline removal by episode disabled.",
                       type = "warning", duration = 10)
      updatePickerInput(session, "intv_baseline_epi", choices = NULL)
    } else {
      choices <- seq_len(epi)
      names(choices) <- DefaultEpiLabel(epi)
      updatePickerInput(session, "intv_baseline_epi", choices = choices)
    }
  })
  #intv_plt----
  get_intv_plt_id <- function(x) {
    make.names(paste0("intv_plt_", x))
  }
  get_intv_brs_id <- function(x) {
    make.names(paste0("intv_brs_", x))
  }
  observeEvent({
    rv$abf_bl
    input$intv_grp_plot
    get_selected_grp()
    rv$intv_setting
  }, {
    if (input$intv_grp_plot) {
      show("intv_plt_sample")
      show("intv_plt")

      abf <- rv$abf_bl
      intv <- rv$intv
      id <- get_selected_id()

      if (is.null(abf) || is.null(intv) || is.null(id)) {
        output$intv_plt <- NULL
        return()
      }

      output$intv_plt <- renderUI({
        plts <- lapply(id, function(id) {
          plt_id <- get_intv_plt_id(id)
          brs_id <- get_intv_brs_id(id)
          plt <- plotOutput(plt_id,
                            brush = brushOpts(
                              id = brs_id,
                              direction = "x",
                              resetOnNew = TRUE
                            ))
        })
        do.call(tagList, plts)
      })

      #render plots
      withProgress({
        lapply(id, function(id) {
          plt_id <- get_intv_plt_id(id)
          output[[plt_id]] <- renderPlot({
            plt_cursor <- intv[[id]][1:2]
            PlotChannelAbf(abf[[id]],
                           cursor = plt_cursor,
                           auto_zoom = TRUE,
                           title = id,
                           sample_ratio = input$intv_plt_sample) +
              theme_bw()
          })
          incProgress(amount = 1, message = paste("Plotting", id))
          Sys.sleep(0.1)
        })
      }, min = 0, max = length(id), message = "Creating Plots")

    } else {
      hide("intv_plt_sample")
      hide("intv_plt")
    }
  }, priority = -1000)
  observe({
    if (input$intv_grp_plot) {
      id <- get_selected_id()
      if (is.null(id)) {
        return()
      }
      abf <- rv$abf_bl
      print("intv brush")
      lapply(id, function(id) {
        brs <- get_intv_brs_id(id)
        plt <- get_intv_plt_id(id)
        if (!is.null(input[[brs]])) {
          print(paste0("updating brush ", brs))
          new_intv <- Intv(startPos = input[[brs]]$xmin, endPos = input[[brs]]$xmax)
          rv$intv[[id]] <- new_intv
          print(sprintf("Selected intv %s: %g:%g", plt, new_intv[1], new_intv[2]))
          output[[plt]] <- renderPlot({
            PlotChannelAbf(abf[[id]], cursor = new_intv[1:2], title = id, sample_ratio = input$intv_plt_sample, auto_zoom = TRUE) +
              theme_bw()
          })
        }
      })
    }
  })

  observeEvent(input$intv_glb_switch, {
    if (input$intv_glb_switch) {
      print("showing global settings ui")
      show("intv_glb_manual_intv")
      if (input$intv_glb_manual_intv) {
        show("intv_glb_intv_start")
        show("intv_glb_intv_len")
        hide("intv_glb_noisy_opt")
        hide("intv_glb_use_epoch")
        hide("intv_glb_manual_vol")
        hide("intv_glb_vol_delta")
        hide("intv_glb_common_intv")
      } else {
        hide("intv_glb_intv_start")
        hide("intv_glb_intv_len")
        show("intv_glb_noisy_opt")
        show("intv_glb_use_epoch")
        show("intv_glb_manual_vol")
        if (input$intv_glb_manual_vol) {
          show("intv_glb_vol_delta")
        } else {
          hide("intv_glb_vol_delta")
        }
        show("intv_glb_common_intv")
      }
      print("hiding grp settings ui")
      hide("intv_grp_manual_intv")
      hide("intv_grp_intv_start")
      hide("intv_grp_intv_len")
      hide("intv_grp_noisy_opt")
      hide("intv_grp_use_epoch")
      hide("intv_grp_manual_vol")
      hide("intv_grp_vol_delta")
      hide("intv_grp_common_intv")
    } else {
      print("showing grp settings ui")
      show("intv_grp_manual_intv")
      if (input$intv_grp_manual_intv) {
        show("intv_grp_intv_start")
        show("intv_grp_intv_len")
        hide("intv_grp_noisy_opt")
        hide("intv_grp_use_epoch")
        hide("intv_grp_manual_vol")
        hide("intv_grp_vol_delta")
        hide("intv_grp_common_intv")
      } else {
        hide("intv_grp_intv_start")
        hide("intv_grp_intv_len")
        show("intv_grp_noisy_opt")
        show("intv_grp_use_epoch")
        show("intv_grp_manual_vol")
        if (input$intv_grp_manual_vol) {
          show("intv_grp_vol_delta")
        } else {
          hide("intv_grp_vol_delta")
        }
        show("intv_grp_common_intv")
      }
      print("hiding grobal settings ui")
      hide("intv_glb_manual_intv")
      hide("intv_glb_intv_start")
      hide("intv_glb_intv_len")
      hide("intv_glb_noisy_opt")
      hide("intv_glb_use_epoch")
      hide("intv_glb_manual_vol")
      hide("intv_glb_vol_delta")
      hide("intv_glb_common_intv")
    }
  }, priority = 10)

  observeEvent({
    #global settings changed
    input$intv_glb_manual_intv
    input$intv_glb_intv_start
    input$intv_glb_intv_len
    input$intv_glb_noisy_opt
    input$intv_glb_use_epoch
    input$intv_glb_manual_vol
    input$intv_glb_vol_delta
    input$intv_glb_common_intv
  }, {
    print("global setting changed")
    #overwrite all grp settings
    abf_index <- rv$abf_index
    if (is.null(abf_index)) {
      return()
    }
    grps <- unique(abf_index$group)
    setting_model <- intv_setting_default
    setting_model$manual_intv <- input$intv_glb_manual_intv
    setting_model$intv_start <- input$intv_glb_intv_start
    setting_model$intv_len <- input$intv_glb_intv_len
    setting_model$noisy_opt <- input$intv_glb_noisy_opt
    setting_model$use_epoch <- input$intv_glb_use_epoch
    setting_model$manual_vol <- input$intv_glb_manual_vol
    setting_model$vol_delta <- input$intv_glb_vol_delta
    setting_model$common_intv <- input$intv_glb_common_intv

    print("applying setting globally")
    lapply(grps, function(grp) {
      rv$intv_setting[[grp]] <- setting_model
    })

    if (input$intv_glb_switch) {
      print("updating global settings ui")
      if (input$intv_glb_manual_intv) {
        show("intv_glb_intv_start")
        show("intv_glb_intv_len")
        hide("intv_glb_noisy_opt")
        hide("intv_glb_use_epoch")
        hide("intv_glb_manual_vol")
        hide("intv_glb_vol_delta")
        hide("intv_glb_common_intv")
      } else {
        hide("intv_glb_intv_start")
        hide("intv_glb_intv_len")
        show("intv_glb_noisy_opt")
        show("intv_glb_use_epoch")
        show("intv_glb_manual_vol")
        if (input$intv_glb_manual_vol) {
          show("intv_glb_vol_delta")
        } else {
          hide("intv_glb_vol_delta")
        }
        show("intv_glb_common_intv")
      }
    }

    print("applying global settings to group settings ui")
    updateSwitchInput(session, "intv_grp_manual_intv", value = setting_model$manual_intv)
    updateNumericInput(session, "intv_grp_intv_start", value = setting_model$intv_start)
    updateNumericInput(session, "intv_grp_intv_len", value = setting_model$intv_len)
    updateSwitchInput(session, "intv_grp_noisy_opt", value = setting_model$noisy_opt)
    updateSwitchInput(session, "intv_grp_use_epoch", value = setting_model$use_epoch)
    updateSwitchInput(session, "intv_grp_manual_vol", value = setting_model$manual_vol)
    updateNumericInput(session, "intv_grp_vol_delta", value = setting_model$vol_delta)
    updateSwitchInput(session, "intv_grp_common_intv", value = setting_model$common_intv)

  }, ignoreInit = FALSE)

  observeEvent({
    #group settings changed
    input$intv_grp_manual_intv
    input$intv_grp_intv_start
    input$intv_grp_intv_len
    input$intv_grp_noisy_opt
    input$intv_grp_use_epoch
    input$intv_grp_manual_vol
    input$intv_grp_vol_delta
    input$intv_grp_common_intv
  }, {
    print("group setting changed")
    #overwrite current grp settings
    grp <- get_selected_grp()
    if (is.null(grp)) {
      return()
    }
    setting_model <- intv_setting_default
    setting_model$manual_intv <- input$intv_grp_manual_intv
    setting_model$intv_start <- input$intv_grp_intv_start
    setting_model$intv_len <- input$intv_grp_intv_len
    setting_model$noisy_opt <- input$intv_grp_noisy_opt
    setting_model$use_epoch <- input$intv_grp_use_epoch
    setting_model$manual_vol <- input$intv_grp_manual_vol
    setting_model$vol_delta <- input$intv_grp_vol_delta
    setting_model$common_intv <- input$intv_grp_common_intv

    if (!input$intv_glb_switch) {
      print("updating group settings ui")
      if (input$intv_grp_manual_intv) {
        show("intv_grp_intv_start")
        show("intv_grp_intv_len")
        hide("intv_grp_noisy_opt")
        hide("intv_grp_use_epoch")
        hide("intv_grp_manual_vol")
        hide("intv_grp_vol_delta")
        hide("intv_grp_common_intv")
      } else {
        hide("intv_grp_intv_start")
        hide("intv_grp_intv_len")
        show("intv_grp_noisy_opt")
        show("intv_grp_use_epoch")
        show("intv_grp_manual_vol")
        if (input$intv_grp_manual_vol) {
          show("intv_grp_vol_delta")
        } else {
          hide("intv_grp_vol_delta")
        }
        show("intv_grp_common_intv")
      }
    }

    print(paste("applying setting to group", grp))
    rv$intv_setting[[grp]] <- setting_model
  }, ignoreInit = FALSE)

  observeEvent(get_selected_grp(), {
    print("grp changed, updating group settings ui")
    grp <- get_selected_grp()
    setting_val <- rv$intv_setting[[grp]]
    updateSwitchInput(session, "intv_grp_manual_intv", value = setting_val$manual_intv)
    updateNumericInput(session, "intv_grp_intv_start", value = setting_val$intv_start)
    updateNumericInput(session, "intv_grp_intv_len", value = setting_val$intv_len)
    updateSwitchInput(session, "intv_grp_noisy_opt", value = setting_val$noisy_opt)
    updateSwitchInput(session, "intv_grp_use_epoch", value = setting_val$use_epoch)
    updateSwitchInput(session, "intv_grp_manual_vol", value = setting_val$manual_vol)
    updateNumericInput(session, "intv_grp_vol_delta", value = setting_val$vol_delta)
    updateSwitchInput(session, "intv_grp_common_intv", value = setting_val$common_intv)
  })

  observeEvent(input$intv_baseline_switch, {
    switch(input$intv_baseline_switch,
           disabled = {
             hide("intv_baseline_epi")
             hide("intv_baseline_start")
             hide("intv_baseline_len")
             hide("intv_baseline_apply")
           },
           episode = {
             show("intv_baseline_epi")
             hide("intv_baseline_start")
             hide("intv_baseline_len")
             show("intv_baseline_apply")
           },
           holding = {
             hide("intv_baseline_epi")
             show("intv_baseline_start")
             show("intv_baseline_len")
             show("intv_baseline_apply")
           })
  })

  #iv_plt ----
  observeEvent({
    input$group_by
  }, {

    grp_by <- input$group_by

    if (any_null(grp_by)) {
      updatePickerInput(session, "plot_by", choices = character())
    } else {
      updatePickerInput(session, "plot_by", choices = grp_by)
    }
  }, ignoreNULL = FALSE)
  get_iv_plt_id <- function(grp) {
    make.names(paste0("iv_plt_id_", grp))
  }
  observe({
    ivs <- rv$ivs

    if (any_null(ivs)) {
      output$iv_plt <- renderUI({
        NULL
      })
      return()
    }

    id <- names(ivs)
    output$iv_plt <- renderUI({
      plt <- lapply(id, function(id) {
        withSpinner(plotlyOutput(outputId = get_iv_plt_id(id)))
      })
      do.call(tagList, plt)
    })
    lapply(id, function(id) {
      plt_id <- get_iv_plt_id(id)
      output[[plt_id]] <- renderPlotly({
        p <- PlotIVSummary(ivs[[id]],
                           mode = input$iv_type_switch,
                           zero_axes = FALSE,
                           title = id,
                           smooth = input$iv_plot_smooth,
                           error_bar = TRUE) + theme_bw()
        if (input$iv_plot_zero) {
          p + ZeroCursor()
        } else {
          p
        }
      })
    })
  }, priority = -100)
  observe({
    grp_by <- input$group_by
    if (any_null(grp_by)) {
      updatePickerInput(session, "iv_blank_by", choices = NULL)
    } else {
      updatePickerInput(session, "iv_blank_by", choices = grp_by)
    }
  })
  observe({
    abf_index <- rv$abf_index
    blank_by <- input$iv_blank_by
    if (any_null(abf_index, blank_by)) {
      updatePickerInput(session, "iv_blank_ref", choices = NULL)
    } else {
      refs <- as.character(unique(abf_index[[blank_by]]))
      updatePickerInput(session, "iv_blank_ref", choices = refs)
    }
  })

  #DEBUG OBSERVER----
observeEvent(input$test_btn, {
  print("===========")
  print(rv$blank_index)
  print("===========")
  print(rv$blank_ivs)
})


})
