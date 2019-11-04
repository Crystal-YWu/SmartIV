#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(shinythemes)
library(shinyWidgets)

library(plotly)

shinyUI(fluidPage(theme = shinytheme("flatly"),


  titlePanel("SmartIV"),

  useShinyjs(),

  navbarPage(title = "v0.9.9", id = "nav_main",

             tabPanel(title = "Index",
                      sidebarLayout(
                        sidebarPanel(
                          shinyDirButton("wd_loader", label = "Load Working Folder", title = "Load Working Folder"),
                          hr(),
                          checkboxGroupInput("group_by", label = "Group by:", choices = NULL),
                          width = 3
                        ),
                        mainPanel(
                          h4("Filter Data Sample:"),
                          h5("Select Entry to Preview Waveform"),
                          DT::dataTableOutput("index"),
                          hr(),
                          sliderTextInput("index_plt_sample", label = "Adjust Preview Sample Ratio:", choices = c(1, 5, 10, 20, 50, 100, 1000), grid = TRUE, selected = 20),
                          withSpinner(plotlyOutput("index_plt_waveform"))
                        )
                      )
             ),

             tabPanel(title = "Interval & Preview NEW IDEAS",
                      sidebarPanel(
                        h4("I-V Interval Settings:"),
                        hr(),
                        h4("Apply Interval Settings Globally"),
                        switchInput("intv_glb_switch", onStatus = "danger", offStatus = "success", value = TRUE, size = "large"),
                        hr(),
                        h4("Interval Selection Method"),
                        #show global settings by default
                        switchInput("intv_glb_manual_intv",
                                    label = NULL,
                                    value = FALSE,
                                    offLabel = "Auto",
                                    offStatus = "success",
                                    onLabel = "Fixed"),
                        hidden(
                          numericInput("intv_glb_intv_start", "Interval Start", value = 1000L, min = 1L, step = 100L),
                          numericInput("intv_glb_intv_len", "Interval Length", value = 200L, min = 10L, step = 100L)
                        ),
                        materialSwitch("intv_glb_noisy_opt", "Noisy Data Enhancement", value = FALSE, status = "success", right = TRUE),
                        materialSwitch("intv_glb_use_epoch", "Use Full Epoch if No Interval Found", value = TRUE, status = "success", right = TRUE),
                        materialSwitch("intv_glb_manual_vol", "Manual Voltage Delta", value = FALSE, status = "success", right = TRUE),
                        hidden(
                          numericInput("intv_glb_vol_delta", "Voltage Delta", value = 15.0, min = 0.0)
                        ),
                        materialSwitch("intv_glb_common_intv", "One Interval for ALL", value = FALSE, status = "success", right = TRUE),
                        switchInput("intv_grp_manual_intv",
                                    label = NULL,
                                    value = FALSE,
                                    offLabel = "Auto",
                                    offStatus = "success",
                                    onLabel = "Fixed"),
                        hidden(
                          numericInput("intv_grp_intv_start", "Interval Start", value = 1000L, min = 1L, step = 100L),
                          numericInput("intv_grp_intv_len", "Interval Length", value = 200L, min = 10L, step = 100L)
                        ),
                        materialSwitch("intv_grp_noisy_opt", "Noisy Data Enhancement", value = FALSE, status = "success", right = TRUE),
                        materialSwitch("intv_grp_use_epoch", "Use Full Epoch if No Interval Found", value = TRUE, status = "success", right = TRUE),
                        materialSwitch("intv_grp_manual_vol", "Manual Voltage Delta", value = FALSE, status = "success", right = TRUE),
                        hidden(
                          numericInput("intv_grp_vol_delta", "Voltage Delta", value = 15.0, min = 0.0)
                        ),
                        materialSwitch("intv_grp_common_intv", "One Interval for ALL", value = FALSE, status = "success", right = TRUE),
                        hr(),
                        h4("Plot Preview Settings:"),
                        materialSwitch("intv_grp_plot", "Preview Waveform & Finetune Group Interval", value = FALSE, status = "success", right = TRUE),
                        hr(),
                        h4("Subtract Baseline:"),
                        h5("Select Baseline:"),
                        radioGroupButtons("intv_baseline_switch",
                                          label = NULL,
                                          choices = c("Disabled" = "disabled", "Episode" = "episode", "Holding Epoch" = "holding")),
                        hidden(
                          pickerInput("intv_baseline_epi", label = "Select Baseline Episode", choices = NULL, options = list(`live-search` = TRUE)),
                          numericInput("intv_baseline_start", label = "Baseline Start", value = 1, min = 1, step = 10),
                          numericInput("intv_baseline_len", label = "Baseline Length", value = 200, min = 10, step = 10),
                          actionButton("intv_baseline_apply", label = "Apply Baseline")
                        ),
                        width = 3
                      ),
                      mainPanel(
                        fluidRow(
                          column(width = 3,
                            uiOutput("intv_grp_sel"),
                            textOutput("intv_grp_num")
                          ),
                          column(width = 9,
                            withSpinner(plotlyOutput("intv_grp_iv"))
                          )
                        ),
                        hr(),
                        sliderTextInput("intv_plt_sample",
                                        label = "Adjust Plot Sample Ratio:",
                                        choices = c(1, 5, 10, 20, 50, 100, 1000),
                                        grid = TRUE,
                                        selected = 50),
                        uiOutput("intv_plt")
                      )
             ),

             tabPanel(title = "I-V Plots",
                      sidebarPanel(
                        h4("Plot by:"),
                        pickerInput("plot_by", label = NULL, choices = character(0)),
                        hr(),
                        h4("Plot Type:"),
                        radioGroupButtons("iv_type_switch",
                                          label = NULL,
                                          choices = c("I-V Plot" = "i",
                                                      "G-V Plot" = "g",
                                                      "Specific I-V" = "spi",
                                                      "Specific G-V" = "spg")),
                        hr(),
                        h4("Plot Settings:"),
                        materialSwitch("iv_plot_smooth", "Smoothed Lines", value = FALSE, status = "success"),
                        materialSwitch("iv_plot_zero", "Zero Crossing", value = TRUE, status = "success"),
                        hr(),
                        h4("Subtract Control:"),
                        radioGroupButtons("iv_blank_switch",
                                          label = NULL,
                                          choices = c("Disabled" = "disabled", "Enabled" = "enabled")),
                        pickerInput("iv_blank_by", label = "Subtract By", choices = NULL),
                        pickerInput("iv_blank_ref", label = "Control", choices = NULL),
                        actionButton("iv_blank_apply", label = "Apply"),
                        width = 3
                      ),
                      mainPanel(
                        uiOutput("iv_plt")
                      )
             ),

             tabPanel(title = "Export Data",
                      sidebarPanel(
                        materialSwitch("exp_sp", "Include Specific Properties", value = FALSE, status = "success"),
                        hr(),
                        actionButton("exp_ivs", "I-V Summaries"),
                        hr(),
                        actionButton("exp_ivs_raw", "I-V Individual"),

                        width = 3
                      ),
                      mainPanel(
                        h4("Last Exported Data"),
                        DT::dataTableOutput("export_preview")
                      )
             )
  )

))
