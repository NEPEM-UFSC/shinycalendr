ui <- fluidPage(
  titlePanel("shinycalendr: A Shiny interface for the CalendR package"),

  sidebarLayout(
    sidebarPanel(
      h4("Add/Edit Events"),
      selectInput(
        "date_selection",
        "Choose date range:",
        choices = c("Custom Data" = "custom", "Year" = "year", "Month" = "month"),
        selected = "custom"
      ),
      # Conditional panel for 'Custom Data'
      conditionalPanel(
        condition = "input.date_selection == 'custom'",
        textInput("event_name", "Event Name:", value = ""),
        fluidRow(
          column(6,
                 dateInput("start_date", "Start Date:", value = Sys.Date())
          ),
          column(6,
                 dateInput("end_date", "End Date:", value = Sys.Date())
          )
        ),
        checkboxInput("show_full_year", "Show entire year", value = FALSE),
        actionButton("add_event", "Add Event")
      ),
      # Conditional panel for 'Year'
      conditionalPanel(
        condition = "input.date_selection == 'year'",
        selectInput(
          "selected_year",
          "Select year:",
          choices = seq(1990, 2100),
          selected = format(Sys.Date(), "%Y")
        )
      ),

      # Conditional panel for 'Month'
      conditionalPanel(
        condition = "input.date_selection == 'month'",
        selectInput(
          "selected_month",
          "Select month:",
          choices = month.name,
          selected = month.name[as.numeric(format(Sys.Date(), "%m"))]
        ),
        checkboxInput("add_lunar", "Include moon phases", value = FALSE),
        conditionalPanel(
          condition = "input.add_lunar == true",
          fluidRow(
            column(6,
                   colourpicker::colourInput("lunar_col", "Moon phases color:", value = "#FFD700"),
            ),
            column(6,
                   numericInput("lunar_size", "Moon phases size:", value = 10)
            )
          )
        )
      ),
      hr(),
      h4("Calendar Settings"),
      tabsetPanel(type = "pills",
                  tabPanel(
                    "Layout",
                    fluidRow(
                      column(6,
                             selectInput("start_week", "Week starts on:",
                                         choices = c("Sunday" = "S", "Monday" = "M")),
                             selectInput("orientation", "Calendar orientation:",
                                         choices = c("portrait", "landscape"))
                      ),
                      column(6,
                             colourpicker::colourInput("bg_col", "Background color:",
                                         value = "white"),
                             numericInput("ncol", "Number of columns:", value = 2, min = 1)
                      )
                    )
                  ),
                  tabPanel(
                    "Title and Subtitle",
                    fluidRow(
                      column(6,
                             textInput("title", "Calendar title:", value = "Event Calendar"),
                             numericInput("title_size", "Title size:", value = 20),
                             colourpicker::colourInput("title_col", "Title color:", value = "gray30"),
                      ),
                      column(6,
                             textInput("subtitle", "Calendar subtitle:", value = ""),
                             numericInput("subtitle_size", "Subtitle size:", value = 10),
                             colourpicker::colourInput("subtitle_col", "Subtitle color:", value = "gray30"),

                      )
                    ),
                    colourpicker::colourInput("mbg_col", "Background color of the month names.", value = "forestgreen"),
                    fluidRow(
                      column(6,
                             selectInput("legend_position", "Legend position:", choices = c("bottom", "left", "right", "top"))
                      ),
                      column(6,
                             numericInput("nrowlegend", "Number of legend rows:", value = 1)

                      )
                    )
                  ),
                  tabPanel(
                    "Dates",
                    fluidRow(
                      column(
                        width = 4,
                        h4("Days"),
                        colourpicker::colourInput("days_col", "Color:", value = "gray30"),
                        numericInput("day_size", "Size:", value = 4)
                      ),
                      column(
                        width = 4,
                        h4("Weeks"),
                        colourpicker::colourInput("weeknames_col", "Color:", value = "black"),
                        numericInput("weeknames_size", "Size:", value = 5),
                        checkboxInput("week_number", "Add number?", value = FALSE),
                        conditionalPanel(
                          colourpicker::colourInput("week_number_col", "Number color:", value = "black"),
                          condition = "input.week_number == true",
                          numericInput("week_number_size", "Number size:", value = 8)
                        )
                      ),
                      column(
                        width = 4,
                        h4("Months"),
                        colourpicker::colourInput("months_col", "Color:", value = "white"),
                        numericInput("months_size", "Size:", value = 16),
                        numericInput("months_pos", "Alignment (0-1):", value = 0.5, min = 0, max = 1)
                      )
                    )
                  ),
                  tabPanel(
                    "Lines and Style",
                    fluidRow(
                      column(6,
                             numericInput("lwd", "Line thickness:", value = 0.5, min = 0),
                             numericInput("lty", "Line type:", value = 1)
                      ),
                      column(6,
                             selectInput("font_family", "Font type:", choices = c("sans", "serif", "mono"), selected = "sans"),
                             selectInput("font_style", "Font style:", choices = c("plain", "bold", "italic", "bold.italic"), selected = "plain")
                      )
                    )
                  ),
                  tabPanel(
                    "Other Settings",
                    numericInput("margin", "Calendar margin:", value = 1)
                  )
      )
    ),

    mainPanel(
      tabsetPanel(
        type = "pills",
        id = "main_tabs",  # Set an ID for controlling tabs
        tabPanel(
          "Event List",
          DT::DTOutput("event_table")
        ),
        tabPanel(
          "Generated Calendar",
          plotOutput("calendar_plot", height = "720px")
        ),
        tabPanel(
          "Download",
          h4("Download Settings"),
          fluidRow(
            column(6,
                   numericInput("download_width", "Width (in inches):", value = 10, min = 1)
            ),
            column(6,
                   numericInput("download_height", "Height (in inches):", value = 8, min = 1)
            )
          ),
          fluidRow(
            column(6,
                   numericInput("download_resolution", "Resolution (DPI):", value = 300, min = 72)
            ),
            column(6,
                   selectInput("download_format", "Format:", choices = c("png", "jpeg", "pdf"), selected = "png")
            )
          ),
          downloadButton("download_calendar", "Download Calendar")
        )
      )
    )

  )
)

server <- function(input, output, session) {
  # Reactive value to store events
  events <- reactiveVal(data.frame(
    event_name = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    stringsAsFactors = FALSE
  ))

  # Show or hide the "Lista de Eventos" tab
  observeEvent(input$date_selection, {
    if (input$date_selection == "custom") {
      showTab("main_tabs", "Event List")
    } else {
      hideTab("main_tabs", "Event List")
    }
  })

  # Row being edited
  editing_row <- reactiveVal(NULL)

  # Add a new event or update an existing one
  observeEvent(input$add_event, {
    req(input$event_name, input$start_date, input$end_date)
    # Validation: End date cannot be earlier than start date
    if (input$end_date < input$start_date) {
      showModal(modalDialog(
        title = "Erro nas Datas",
        "A data final não pode ser anterior à data inicial.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()  # Exit the observer
    }

    # Validation 2: Check for overlapping dates
    all_events <- events()
    overlap <- any(
      (input$start_date <= all_events$end_date & input$end_date >= all_events$start_date)
    )

    if (overlap) {
      showModal(modalDialog(
        title = "Conflito de Datas",
        "As datas do novo evento sobrepõem-se a um evento existente.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()  # Exit the observer
    }


    new_event <- data.frame(
      event_name = input$event_name,
      start_date = input$start_date,
      end_date = input$end_date,
      stringsAsFactors = FALSE
    )

    if (!is.null(editing_row())) {
      # Update the existing row
      all_events <- events()
      all_events[editing_row(), ] <- new_event
      events(all_events)
      editing_row(NULL)
    } else {
      # Add a new row
      events(rbind(events(), new_event))
    }
  })

  # Render the datatable with Edit/Delete buttons
  output$event_table <- DT::renderDT({
    data <- events()
    DT::datatable(
      data  |>
        transform(
          Edit = sprintf('<button class="edit-btn" id="edit_%s">✏️</button>', seq_len(nrow(data))),
          Delete = sprintf('<button class="delete-btn" id="delete_%s">🗑️</button>',seq_len(nrow(data)))
        ),
      escape = FALSE,
      selection = "none",
      options = list(dom = "t", paging = FALSE),
      callback = DT::JS("
          table.on('click', '.edit-btn', function() {
            var id = $(this).attr('id');
            Shiny.setInputValue('button_click', {id: id}, {priority: 'event'});
          });
          table.on('click', '.delete-btn', function() {
            var id = $(this).attr('id');
            Shiny.setInputValue('button_click', {id: id}, {priority: 'event'});
          });
        ")
    )
  })

  # Handle button clicks for Edit/Delete
  observeEvent(input$button_click, {
    btn_info <- input$button_click
    btn_id <- strsplit(btn_info$id, "_")[[1]]
    action <- btn_id[1]
    row_id <- as.numeric(btn_id[2])

    if (action == "edit") {
      event_data <- events()[row_id, ]
      updateTextInput(session, "event_name", value = event_data$event_name)
      updateDateInput(session, "start_date", value = event_data$start_date)
      updateDateInput(session, "end_date", value = event_data$end_date)
      editing_row(row_id)
    } else if (action == "delete") {
      events(events()[-row_id, ])
    }
  })

  # Generate calendar
  calendar_plot <- reactive({
    if (input$date_selection == "custom") {

      req(events()$event_name)
      # Determine range of dates
      if (input$show_full_year) {
        # Use the full year if the checkbox is checked
        start_date <- as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01"))
        end_date <- as.Date(paste0(format(Sys.Date(), "%Y"), "-12-31"))
      } else {
        # Use the range based on event dates
        start_date <- min(events()$start_date, na.rm = TRUE)
        end_date <- max(events()$end_date, na.rm = TRUE)

        # Round to full months
        start_date <- as.Date(format(start_date, "%Y-%m-01"))
        end_date <- as.Date(format(end_date, "%Y-%m-01")) + months(1) -  as.difftime(1, units = "days")
      }

      # Create sequence of dates
      all_dates <- seq.Date(start_date, end_date, by = "day")

      # Map events to dates
      event_names <- rep(NA, length(all_dates))
      for (i in seq_len(nrow(events()))) {
        start_idx <- which(all_dates == events()$start_date[i])
        end_idx <- which(all_dates == events()$end_date[i])
        event_names[start_idx:end_idx] <- events()$event_name[i]
      }

      # Generate colors
      unique_events <- unique(na.omit(event_names))
      num_colors <- length(unique_events)
      ggplot_colors <- function(n) {
        # adapted from https://stackoverflow.com/a/8197703
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }
      colors <- ggplot_colors(num_colors)

      # Generate calendar
      calendR::calendR(
        from = start_date,
        to = end_date,
        special.days = event_names,
        special.col = colors,
        orientation = input$orientation,
        mbg.col = input$mbg_col,
        bg.col = input$bg_col,
        title = input$title,
        title.size = input$title_size,
        title.col = input$title_col,
        subtitle = input$subtitle,
        subtitle.size = input$subtitle_size,
        subtitle.col = input$subtitle_col,
        day.size = input$day_size,
        days.col = input$days_col,
        # weeknames = input$weeknames,
        weeknames.col = input$weeknames_col,
        weeknames.size = input$weeknames_size,
        week.number = input$week_number,
        week.number.col = ifelse(input$week_number, input$week_number_col, NA),
        week.number.size = ifelse(input$week_number, input$week_number_size, NA),
        # monthnames = input$monthnames,
        months.size = input$months_size,
        months.col = input$months_col,
        months.pos = input$months_pos,
        legend.pos = input$legend_position,
        start = input$start_week,
        lwd = input$lwd,
        lty = input$lty,
        font.family = input$font_family,
        font.style = input$font_style,
        ncol = input$ncol,
        lunar = input$add_lunar,
        lunar.col = input$lunar_col,
        lunar.size = input$lunar_size,
        margin = input$margin
      ) +
        ggplot2::guides(fill = ggplot2::guide_legend(nrow = input$nrowlegend))

    } else if (input$date_selection == "year") {
      calendR::calendR(
        year = as.numeric(input$selected_year),
        orientation = input$orientation,
        mbg.col = input$mbg_col,
        bg.col = input$bg_col,
        title = input$title,
        title.size = input$title_size,
        title.col = input$title_col,
        subtitle = input$subtitle,
        subtitle.size = input$subtitle_size,
        subtitle.col = input$subtitle_col,
        day.size = input$day_size,
        days.col = input$days_col,
        # weeknames = input$weeknames,
        weeknames.col = input$weeknames_col,
        weeknames.size = input$weeknames_size,
        week.number = input$week_number,
        week.number.col = ifelse(input$week_number, input$week_number_col, NA),
        week.number.size = ifelse(input$week_number, input$week_number_size, NA),
        # monthnames = input$monthnames,
        months.size = input$months_size,
        months.col = input$months_col,
        months.pos = input$months_pos,
        start = input$start_week,
        lwd = input$lwd,
        lty = input$lty,
        font.family = input$font_family,
        font.style = input$font_style,
        ncol = input$ncol,
        margin = input$margin
      )

    } else if (input$date_selection == "month") {
      # Generate calendar
      calendR::calendR(
        month = match(input$selected_month, month.name),
        orientation = input$orientation,
        mbg.col = input$mbg_col,
        bg.col = input$bg_col,
        title = input$title,
        title.size = input$title_size,
        title.col = input$title_col,
        subtitle = input$subtitle,
        subtitle.size = input$subtitle_size,
        subtitle.col = input$subtitle_col,
        day.size = input$day_size,
        days.col = input$days_col,
        # weeknames = input$weeknames,
        weeknames.col = input$weeknames_col,
        weeknames.size = input$weeknames_size,
        week.number = input$week_number,
        week.number.col = ifelse(input$week_number, input$week_number_col, NA),
        week.number.size = ifelse(input$week_number, input$week_number_size, NA),
        # monthnames = input$monthnames,
        months.size = input$months_size,
        months.col = input$months_col,
        months.pos = input$months_pos,
        start = input$start_week,
        lwd = input$lwd,
        lty = input$lty,
        font.family = input$font_family,
        font.style = input$font_style,
        ncol = input$ncol,
        lunar = input$add_lunar,
        lunar.col = input$lunar_col,
        lunar.size = input$lunar_size,
        margin = input$margin
      )
    }
  })


  output$calendar_plot <- renderPlot({
    calendar_plot()
  })


  # Download handler for the calendar
  output$download_calendar <- downloadHandler(
    filename = function() {
      paste("calendar-", Sys.Date(), ".", input$download_format, sep = "")
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = calendar_plot(),
        device = input$download_format,
        width = input$download_width,
        height = input$download_height,
        dpi = input$download_resolution,
        units = "in"
      )
    }
  )
}
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
