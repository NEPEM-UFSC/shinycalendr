ui <- fluidPage(
  titlePanel("shinycalendr: A Shiny interface for the CalendR package"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel(
          "Add/Edit Events",
          br(),
          selectInput(
            "date_selection",
            "Type of calendar",
            choices = c("Year" = "year", "Month" = "month", "Events" = "events"),
            selected = "year"
          ),
          # Conditional panel for 'Custom Data'
          conditionalPanel(
            condition = "input.date_selection == 'events'",
            textInput("event_name", "Event Name", value = ""),
            dateRangeInput("date_range", "Date Range", start = Sys.Date(), end = Sys.Date()),
            # checkbox for recurrent events
            checkboxInput("recurrent", "Recurrent events?", value = FALSE),
            conditionalPanel(
              condition = "input.recurrent == true",
              selectInput("recurrence_type", "Recurrence type",
                          choices = c("weekly", "biweekly", "quarterly", "monthly"))
            ),

            checkboxInput("show_full_year", "Show entire year", value = TRUE),
            tags$style(
              HTML("
      #add_event {
        background-color: #228B22; /* Forest green */
        color: white;
        border: none;
        padding: 10px 20px;
        font-size: 16px;
        border-radius: 5px;
        transition: all 0.3s ease;
        cursor: pointer; /* Pointer cursor for better UX */
      }

      #add_event:hover {
        background-color: #1c731c; /* Slightly darker shade of green */
        box-shadow: 0 4px 10px rgba(0, 0, 0, 0.2); /* Shadow effect on hover */
      }

      #add_event:active {
        background-color: #145214; /* Even darker shade on click */
        transform: scale(0.98); /* Slight scale effect when clicked */
      }
    ")
            ),
            actionButton("add_event", "Add/Edit")
          ),
          # Conditional panel for 'Year'
          conditionalPanel(
            condition = "input.date_selection == 'year'",
            conditionalPanel(
              condition = "input.custom_rangedate == false",
              selectInput(
                "selected_year",
                "Select year",
                choices = seq(1990, 2100),
                selected = format(Sys.Date(), "%Y")
              ),
            ),
            conditionalPanel(
              condition = "input.custom_rangedate == true",
              dateRangeInput("date_rangecustom", "Date Range", start = Sys.Date(), end = Sys.Date()),
            ),
            checkboxInput("custom_rangedate", "Custom start and end dates", value = FALSE)
          ),



          # Conditional panel for 'Month'
          conditionalPanel(
            condition = "input.date_selection == 'month'",
            fluidRow(
              column(6,
                     selectInput(
                       "selected_month",
                       "Select month",
                       choices = month.name,
                       selected = month.name[as.numeric(format(Sys.Date(), "%m"))]
                     )
              ),
              column(6,
                     selectInput(
                       "selected_year_month",
                       "Select year",
                       choices = seq(1990, 2100),
                       selected = format(Sys.Date(), "%Y")
                     )
              )
            ),
            checkboxInput("add_lunar", "Include moon phases", value = FALSE),
            conditionalPanel(
              condition = "input.add_lunar == true",
              fluidRow(
                column(6,
                       colourpicker::colourInput("lunar_col", "Moon phases color", value = "#FFD700",
                                                 allowTransparent = TRUE,
                                                 returnName = TRUE),
                ),
                column(6,
                       numericInput("lunar_size", "Moon phases size", value = 10)
                )
              )
            ),
            checkboxInput("add_text", "Include Text", value = FALSE),
            conditionalPanel(
              condition = "input.add_text == true",
              textInput("events", "Enter Events (comma-separated):", value = ""),
              textInput("positions", "Enter Days (comma-separated):", value = ""),
              fluidRow(
                column(4,
                       colourpicker::colourInput("special_days_col",
                                                 "Color text",
                                                 value = "black",
                                                 allowTransparent = TRUE,
                                                 returnName = TRUE)
                ),
                column(4,
                       colourpicker::colourInput("special_days_colbg",
                                                 "Color background",
                                                 value = "gray90",
                                                 allowTransparent = TRUE,
                                                 returnName = TRUE)
                ),
                column(4,
                       numericInput("special_days_size", "Size", value = 4)
                )
              )
            )
          )
        ),
        tabPanel(
          "Import a Calendar",
          fileInput(
            "import_calendar",
            label = tagList(icon("upload"), "Import from CSV"),
            accept = c(".csv")
          ),
          p("The CSV file should have the following columns: 'event_name' (text), 'start_date' (YYYY-MM-DD format), and 'end_date' (YYYY-MM-DD format). Make sure the date columns are properly formatted.")
        ),
        tabPanel(
          "Calendar Settings",
          br(),
          tabsetPanel(type = "pills",
                      tabPanel(
                        "Title and Subtitle",
                        fluidRow(
                          column(6,
                                 textInput("title", "Calendar title", value = ""),
                                 numericInput("title_size", "Title size", value = 20),
                                 colourpicker::colourInput("title_col", "Title color", value = "gray30",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE),
                          ),
                          column(6,
                                 textInput("subtitle", "Calendar subtitle", value = ""),
                                 numericInput("subtitle_size", "Subtitle size", value = 10),
                                 colourpicker::colourInput("subtitle_col", "Subtitle color", value = "gray30",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE),

                          )
                        ),
                        colourpicker::colourInput("mbg_col", "Background color of the month names.", value = "forestgreen",
                                                  allowTransparent = TRUE,
                                                  returnName = TRUE),
                        fluidRow(
                          column(6,
                                 selectInput("legend_position", "Legend position", choices = c("bottom", "left", "right", "top")),
                                 colourpicker::colourInput("legend_color", "Color of the legend text",
                                                           value = "black",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE)

                          ),
                          column(6,
                                 numericInput("legend_size", "Legend size", value = 18),
                                 numericInput("nrowlegend", "Number of legend rows", value = 1)

                          )
                        )
                      ),
                      tabPanel(
                        "Layout",
                        fluidRow(
                          column(6,
                                 selectInput("start_week", "Week starts on",
                                             choices = c("Sunday" = "S", "Monday" = "M")),
                                 colourpicker::colourInput("bg_col", "Background color (area)",
                                                           value = "white",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE),
                                 colourpicker::colourInput("bg_col_day", "Background color (days)",
                                                           value = "white",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE)
                          ),
                          column(6,
                                 selectInput("orientation", "Calendar orientation",
                                             choices = c("landscape", "portrait")),
                                 numericInput("ncol", "Number of columns", value = NA, min = 1)
                          )
                        ),
                        # load an image here
                        # checkbox to define if a background image is used
                        # conditional panel to show the fileInput only if the checkbox is checked
                        checkboxInput("use_image", "Use background image?", value = FALSE),
                        conditionalPanel(
                          condition = "input.use_image == true",
                          selectInput("bgtype", "Background type",
                                      choices = c("Example 1" = "1.png",
                                                  "Example 2" = "2.png",
                                                  "Example 3" = "3.png",
                                                  "Example 4" = "4.png",
                                                  "Example 5" = "5.png",
                                                  "Own image")),
                        ),
                        conditionalPanel(
                          condition = "input.use_image == true & input.bgtype == 'Own image'",
                          fileInput("image", "Choose an image for background", accept = c(".png", ".jpg"))
                        )
                      ),
                      tabPanel(
                        "Dates",
                        fluidRow(
                          h3("Days"),
                          column(6,
                                 colourpicker::colourInput("days_col", "Color", value = "black",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE)
                          ),
                          column(6,
                                 numericInput("day_size", "Size", value = 4)
                          )
                        ),
                        fluidRow(
                          h3("Weeks"),
                          column(6,
                                 colourpicker::colourInput("weeknames_col", "Color", value = "black",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE)
                          ),
                          column(6,
                                 numericInput("weeknames_size", "Size", value = 5)
                          )
                        ),
                        checkboxInput("week_number", "Add number?", value = FALSE),
                        conditionalPanel(
                          condition = "input.week_number == true",
                          fluidRow(
                            column(6,
                                   colourpicker::colourInput("week_number_col", "Number color", value = "black",
                                                             allowTransparent = TRUE,
                                                             returnName = TRUE)
                            ),
                            column(6,
                                   numericInput("week_number_size", "Number size", value = 12)
                            )
                          )
                        ),
                        checkboxInput("week_names", "Change week names", value = FALSE),
                        conditionalPanel(
                          condition = "input.week_names == true",
                          textInput("week_names_vals", "Week names", value = "")
                        ),
                        fluidRow(
                          h3("Months"),
                          column(6,
                                 colourpicker::colourInput("months_col", "Color", value = "white",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE)
                          ),
                          column(6,
                                 numericInput("months_size", "Size", value = 16)
                          )
                        ),
                        checkboxInput("month_names", "Change month names", value = FALSE),
                        conditionalPanel(
                          condition = "input.month_names == true",
                          textInput("month_names_vals", "Month names", value = "")
                        ),
                        numericInput("months_pos", "Alignment (0-1)", value = 0.5, min = 0, max = 1, step = 0.5)
                      )
                      ,
                      tabPanel(
                        "Lines and Style",
                        fluidRow(
                          column(6,
                                 numericInput("lwd", "Line thickness", value = 0.5, min = 0),
                                 numericInput("lty", "Line type", value = 1),
                                 colourpicker::colourInput("linecolor", "Line color",
                                                           value = "black",
                                                           allowTransparent = TRUE,
                                                           returnName = TRUE),
                          ),
                          column(6,
                                 selectInput("font_family", "Font type", choices = c("sans", "serif", "mono"), selected = "sans"),
                                 selectInput("font_style", "Font style", choices = c("plain", "bold", "italic", "bold.italic"), selected = "plain")
                          )
                        )
                      ),
                      tabPanel(
                        "Margins",
                        fluidRow(
                          column(4,
                                 numericInput("margin", "Margin (cm)", value = 0.5, min = 0, step = 0.1)
                          ),
                          column(4,
                                 numericInput("margin_x", "Margin rows (cm)", value = 0.2, step = 0.1)
                          ),
                          column(4,
                                 numericInput("margin_y", "Margin columns (cm)", value = 0.2, step = 0.1)
                          )
                        )
                      )
          )

        )
      )

    ),

    mainPanel(
      tabsetPanel(
        # type = "pills",
        id = "main_tabs",  # Set an ID for controlling tabs
        tabPanel(
          "Generated Calendar",
          plotOutput("calendar_plot", height = "780px")
        ),
        tabPanel(
          "Event List",
          fluidRow(
            column(6,
                   actionButton("clearlist",
                                label = tagList(icon("trash"), "Clear Calendar"))
            ),
            column(6,
                   downloadButton(
                     "export_calendar",
                     label = tagList(icon("download"), "Export to CSV")
                   )
            )
          ),
          DT::DTOutput("event_table")
        ),
        tabPanel(
          "Download",
          h4("Download Settings"),
          selectInput("format", "Format", choices = c("pdf", "png", "jpeg"), selected = "pdf"),
          selectInput("papersize", "Paper size",
                      choices = c("A0", "A1", "A2", "A3", "A4", "A5", "A6"),
                      selected = "A4"),
          conditionalPanel(
            condition = "input.format != 'pdf'",
            numericInput("width", "Width (cm)", value = 10, min = 1),
            numericInput("height", "Height (cm)", value = 8, min = 1),
            numericInput("resolution", "Resolution (DPI)", value = 300, min = 72)
          ),
          downloadButton("calendar", "Download Calendar")
        ),
        tabPanel(
          "About",
          h3("About shinycalendr"),
          p(
            "This Shiny application provides an interactive interface for the ",
            a(href = "https://github.com/R-CoderDotCom/calendR", target = "_blank", "calendR package"),
            ", a powerful R package for generating customizable calendars. With shinycalendr, users can create, modify, and visualize a wide range of calendar layouts tailored to their needs."
          ),
          br(),
          h4("Key Features"),
          tags$ul(
            tags$li("Add and edit events with customizable start and end dates."),
            tags$li("Support for recurrent events with various recurrence types (weekly, biweekly, quarterly, monthly)."),
            tags$li("Generate annual, monthly, or custom date range calendars."),
            tags$li("Customize calendar appearance, including titles, subtitles, colors, fonts, and styles."),
            tags$li("Add moon phases to monthly calendars with adjustable color and size."),
            tags$li("Include a background image for calendars, with options to use predefined or user-uploaded images."),
            tags$li("Download generated calendars in PDF, PNG, or JPEG formats with adjustable resolution and size."),
            tags$li("Interactive event list management with options to edit or delete events."),
            tags$li("Fully responsive design for exploring and interacting with calendar settings in real time.")
          ),
          br(),
          h4("How to Use"),
          p(
            "Use the 'Add/Edit Events' tab to create events and specify their recurrence, dates, and other details. Navigate to the 'Calendar Settings' tab to personalize the calendar layout and style. The 'Generated Calendar' tab displays the resulting calendar, and the 'Event List' tab allows you to manage added events. Use the 'Download' tab to export the calendar in your preferred format."
          ),
          br(),
          h4("About calendR"),
          p(
            "The calendR package provides a flexible and comprehensive framework for creating visualizations of calendar data in R. It supports a wide range of customizations, making it ideal for creating calendars tailored to specific applications. For more information, visit the ",
            a(href = "https://github.com/R-CoderDotCom/calendR", target = "_blank", "calendR GitHub page"), "."
          )
        )
      )
    )

  )
)

server <- function(input, output, session) {



  output$export_calendar <- downloadHandler(
    filename = function() {
      paste("calendar_events", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(events(), file, row.names = FALSE)
    }
  )


  # Reactive value to store events
  events <- reactiveVal(data.frame(
    event_name = character(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    stringsAsFactors = FALSE
  ))

  observeEvent(input$import_calendar, {
    req(input$import_calendar)
    imported_data <- read.csv(input$import_calendar$datapath, stringsAsFactors = FALSE)
    imported_data$start_date <- as.Date(imported_data$start_date)
    imported_data$end_date <- as.Date(imported_data$end_date)
    if (all(c("event_name", "start_date", "end_date") %in% colnames(imported_data))) {
      events(imported_data)
    } else {
      showModal(modalDialog(
        title = "Invalid File",
        "The uploaded file must contain the columns: 'event_name', 'start_date', 'end_date'.",
        easyClose = TRUE
      ))
    }
  })

  # Reactive value to store events

  # Show or hide the "Lista de Eventos" tab
  observeEvent(input$date_selection, {
    if (input$date_selection == "events") {
      showTab("main_tabs", "Event List")
    } else {
      hideTab("main_tabs", "Event List")
    }
  })

  # Row being edited
  editing_row <- reactiveVal(NULL)

  observeEvent(input$add_event, {
    req(input$event_name, input$date_range[1], input$date_range[2])

    # Validation: End date cannot be earlier than start date
    if (input$date_range[2] < input$date_range[1]) {
      showModal(modalDialog(
        title = "Date Error",
        "The end date cannot be earlier than the start date.",
        easyClose = TRUE,
        footer = NULL
      ))
      return()  # Exit the observer
    }

    # Retrieve all events
    all_events <- events()

    if (is.null(editing_row())) {
      addEvent <- function(){
        if(input$recurrent){
          seqdata <- seq.Date(input$date_range[1], input$date_range[2], by = "day")
          if (input$recurrence_type == "weekly") {
            rcurrency <- round((input$date_range[2] - input$date_range[1]) / 7)
            dates <- input$date_range[1]
            for(i in 1:(rcurrency - 1)){
              dates[[i + 1]] <- dates[[i]] + lubridate::weeks(1)
            }
          } else if (input$recurrence_type == "biweekly") {
            rcurrency <- round((input$date_range[2] - input$date_range[1]) / 14)
            dates <- input$date_range[1]
            for(i in 1:(rcurrency - 1)){
              dates[[i + 1]] <- dates[[i]] + lubridate::weeks(2)
            }
          } else if (input$recurrence_type == "quarterly") {
            rcurrency <- round((input$date_range[2] - input$date_range[1]) / 21)
            dates <- input$date_range[1]
            for(i in 1:(rcurrency - 1)){
              dates[[i + 1]] <- dates[[i]] + lubridate::weeks(3)
            }
          } else if (input$recurrence_type == "monthly") {
            rcurrency <- round((input$date_range[2] - input$date_range[1]) / 30)
            dates <- input$date_range[1]
            for(i in 1:(rcurrency - 1)){
              dates[[i + 1]] <- dates[[i]] +months(1)
            }
          }
          # Add a new event
          new_event <- data.frame(
            event_name = input$event_name,
            start_date = dates,
            end_date = dates,
            stringsAsFactors = FALSE
          )

        } else{
          # Add a new event
          new_event <- data.frame(
            event_name = input$event_name,
            start_date = input$date_range[1],
            end_date = input$date_range[2],
            stringsAsFactors = FALSE
          )
        }
        events(rbind(events(), new_event))
      }

      # Validation for new rows: Check for overlapping dates
      overlap <- any(
        (input$date_range[1] <= all_events$end_date & input$date_range[2] >= all_events$start_date)
      )

      if (overlap) {
        showModal(modalDialog(
          title = "Date Conflict",
          "The dates of the new event overlap with an existing event. Do you want to add the event anyway?",
          easyClose = FALSE,
          footer = tagList(
            actionButton("confirm_yes", "Yes"),
            actionButton("confirm_no", "No")
          )
        ))

        observeEvent(input$confirm_yes, {
          removeModal()
          addEvent()  # Proceed to add the event despite overlap
        })

        observeEvent(input$confirm_no, {
          removeModal()
          return()  # Exit the observer without adding the event
        })
        return()  # Exit until confirmation is resolved
      }

      # No overlap detected, proceed to add the event
      addEvent()


    } else {
      # Validate for overlapping dates when editing
      other_events <- all_events[-editing_row(), ]  # Exclude the row being edited
      # Update the existing row
      row_id <- editing_row()
      all_events[row_id, ] <- data.frame(
        event_name = input$event_name,
        start_date = input$date_range[1],
        end_date = input$date_range[2],
        stringsAsFactors = FALSE
      )
      events(all_events)
      editing_row(NULL)  # Clear the editing row

      overlap <- any(
        (input$date_range[1] <= other_events$end_date & input$date_range[2] >= other_events$start_date)
      )


      if (overlap) {
        showModal(modalDialog(
          title = "Date Conflict",
          "The dates of the new event overlap with an existing event. Do you want to add the event anyway?",
          easyClose = FALSE,
          footer = tagList(
            actionButton("confirm_yes", "Yes"),
            actionButton("confirm_no", "No")
          )
        ))

        observeEvent(input$confirm_yes, {
          removeModal()
          # Update the existing row
          new_event <- data.frame(
            event_name = input$event_name,
            start_date = input$date_range[1],
            end_date = input$date_range[2],
            stringsAsFactors = FALSE
          )
          all_events[editing_row(), ] <- new_event
          events(all_events)
          editing_row(NULL)  # Clear the editing row
        })

        observeEvent(input$confirm_no, {
          removeModal()
          return()  # Exit the observer without adding the event
        })
        return()  # Exit until confirmation is resolved
      }


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

  ############### Handle button clicks for Edit/Delete ###############
  observeEvent(input$button_click, {
    btn_info <- input$button_click
    btn_id <- strsplit(btn_info$id, "_")[[1]]
    action <- btn_id[1]
    row_id <- as.numeric(btn_id[2])

    if (action == "edit") {
      # Load the selected event data into the input fields
      event_data <- events()[row_id, ]
      updateTextInput(session, "event_name", value = event_data$event_name)
      updateDateRangeInput(session, "date_range", start = event_data$start_date, end = event_data$end_date)
      editing_row(row_id)  # Store the row being edited
    } else if (action == "delete") {
      # Remove the selected row
      events(events()[-row_id, ])
    }
  })

  observeEvent(input$clearlist, {
    showModal(modalDialog(
      title = "You are about to clear the event list",
      "Are you sure you want to proceed?",
      easyClose = FALSE,
      footer = tagList(
        actionButton("confirm_yes", "Yes"),
        actionButton("confirm_no", "No")
      )
    ))

    observeEvent(input$confirm_yes, {
      removeModal()
      events(events()[-c(1:nrow(events())), ])
    })

    observeEvent(input$confirm_no, {
      removeModal()
      return()  # Exit the observer without adding the event
    })
    return()  # Exit until confirmation is resolved


  })

  ############## Generate calendar ##################
  calendar_plot <- reactive({
    week_names <- function(start = c("sunday", "monday")) {
      up <- function(x) {
        substr(x, 1, 1) <- toupper(substr(x, 1, 1))
        x
      }
      Day <- seq(as.Date("2020-08-23"), by = 1, len=7)
      weeknames <- c(up(weekdays(Day))[2:7], up(weekdays(Day))[1])
      if (start == "monday") {
        weeknames <- weeknames[c(7, 1:6)]
      }
      return(weeknames)
    }

    if(input$week_names){
      weeknames <- unlist(strsplit(input$week_names_vals, ","))
    } else{
      weeknames <- week_names(input$start_week)
    }

    if(input$use_image) {
      if(input$bgtype != "Own image"){
        bgimg <- paste0(system.file(input$orientation, package = "shinycalendr"), "/", input$bgtype)
      } else{
        if(is.null(input$image$datapath)) {
          bgimg <- ""
        } else{
          bgimg <- input$image$datapath
        }
      }
    } else{
      bgimg <- ""
    }

    # Function to define the number of rows for a calendar
    define_cols<- function(num_months, orientation = c("portrait", "landscape")) {
      # Match the orientation argument
      orientation <- match.arg(orientation)

      # Define rows based on orientation and number of months
      if (orientation == "portrait") {
        num_rows <- ceiling(num_months / 4)
      } else if (orientation == "landscape") {
        num_rows <- ceiling(num_months / 3)
      }
      return(num_rows)
    }

    if (input$date_selection == "events") {
      if(length(events()$event_name)==0){
        # Generate a sequence of dates for all months of a year
        seqdata <- seq.Date(from = as.Date("2024-01-01"),
                            to = as.Date("2024-12-01"),
                            by = "month")
        monthnames <- format(seqdata, "%B")
        monthnames <- paste0(toupper(substr(monthnames, 1, 1)), substr(monthnames, 2, nchar(monthnames)))
        calendR::calendR(
          from = paste0(format(Sys.Date(), "%Y"), "-01-01"),
          to = paste0(format(Sys.Date(), "%Y"), "-12-31"),
          monthnames = monthnames,
          months.size = input$months_size,
          months.col = input$months_col,
          months.pos = input$months_pos,
          mbg.col = input$mbg_col
        )
      } else{
        if (input$show_full_year) {
          # Use the full year if the checkbox is checked
          start_date <- as.Date(paste0(min(lubridate::year(events()$start_date)), "-01-01"))
          end_date <- as.Date(paste0(max(lubridate::year(events()$end_date)), "-12-31"))
          if(any(lubridate::year(events()$start_date) != lubridate::year(events()$end_date))){
            showModal(modalDialog(
              title = "Multiple years not allowed",
              "Events must be in the same year if 'Show entire year' is selected. Please correct the dates or uncheck the box to show only used months.",
              easyClose = TRUE,
              footer = NULL
            ))
            return()  # Exit the observer
          }
        } else{
          start_date <- min(events()$start_date, na.rm = TRUE)
          end_date <- max(events()$end_date, na.rm = TRUE)
        }

        start_date <- as.Date(format(start_date, "%Y-%m-01"))
        end_date <- as.Date(format(end_date, "%Y-%m-01")) + months(1) -  as.difftime(1, units = "days")
        # Create sequence of dates
        all_dates <- seq.Date(start_date, end_date, by = "day")
        num_months <- length(unique(format(all_dates, "%B")))
        # num_months <- ifelse(num_months == 0, 1, num_months)
        event_names <- rep(NA, length(all_dates))
        # Map events to dates
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
        if(input$month_names){
          monthnames <- unlist(strsplit(input$month_names_vals, ","))
        } else{
          monthnames <- unique(format(all_dates, "%B"))
          monthnames <- paste0(toupper(substr(monthnames, 1, 1)), substr(monthnames, 2, nchar(monthnames)))
        }
        # Generate calendar
        calendR::calendR(
          from = start_date,
          to = end_date,
          special.days = event_names,
          special.col = colors,
          # orientation = input$orientation,
          mbg.col = input$mbg_col,
          bg.col = input$bg_col,
          low.col = input$bg_col_day,
          title = input$title,
          title.size = input$title_size,
          title.col = input$title_col,
          subtitle = input$subtitle,
          subtitle.size = input$subtitle_size,
          subtitle.col = input$subtitle_col,
          day.size = input$day_size,
          days.col = input$days_col,
          weeknames = weeknames,
          weeknames.col = input$weeknames_col,
          weeknames.size = input$weeknames_size,
          week.number = input$week_number,
          week.number.col = ifelse(input$week_number, input$week_number_col, NA),
          week.number.size = ifelse(input$week_number, input$week_number_size, NA),
          monthnames = monthnames[1:num_months],
          months.size = input$months_size,
          months.col = input$months_col,
          months.pos = input$months_pos,
          legend.pos = input$legend_position,
          col = input$linecolor,
          lwd = input$lwd,
          lty = input$lty,
          font.family = input$font_family,
          font.style = input$font_style,
          ncol = ifelse(is.na(input$ncol), define_cols(num_months, input$orientation), input$ncol),
          lunar = input$add_lunar,
          lunar.col = input$lunar_col,
          lunar.size = input$lunar_size,
          margin = input$margin,
          bg.img = bgimg
        ) +
          ggplot2::theme(legend.text = ggplot2::element_text(size = input$legend_size, color = input$legend_color),
                         panel.spacing.x = ggplot2::unit(input$margin_x, "in"),
                         panel.spacing.y = ggplot2::unit(input$margin_y, "in"),
                         panel.background = ggplot2::element_rect(fill = NA)) +
          ggplot2::guides(fill = ggplot2::guide_legend(nrow = input$nrowlegend))
      }


    } else if (input$date_selection == "year") {
      # Determine month names based on user input or defaults
      if (input$custom_rangedate) {
        dates <- seq(input$date_rangecustom[1], input$date_rangecustom[2], by = 1)
      } else {
        dates <- seq(as.Date("2024-01-01"), by = "month", length.out = 12)
      }
      monthnames <- if (input$month_names) {
        unlist(strsplit(input$month_names_vals, ","))
      } else {
        format(dates, "%B")
      }
      monthnames <- unique(paste0(toupper(substr(monthnames, 1, 1)), substr(monthnames, 2, nchar(monthnames))))
      start_date <- if (input$custom_rangedate) input$date_rangecustom[1] else NULL
      end_date <- if (input$custom_rangedate) input$date_rangecustom[2] else NULL

      if (is.null(start_date) || is.null(end_date) || end_date > start_date) {
        calendR::calendR(
          year = as.numeric(input$selected_year),
          from = start_date,
          to = end_date,
          orientation = input$orientation,
          mbg.col = input$mbg_col,
          low.col = input$bg_col_day,
          bg.col = input$bg_col,
          title = input$title,
          title.size = input$title_size,
          title.col = input$title_col,
          subtitle = input$subtitle,
          subtitle.size = input$subtitle_size,
          subtitle.col = input$subtitle_col,
          day.size = input$day_size,
          days.col = input$days_col,
          weeknames = weeknames,
          weeknames.col = input$weeknames_col,
          weeknames.size = input$weeknames_size,
          week.number = input$week_number,
          week.number.col = ifelse(input$week_number, input$week_number_col, NA),
          week.number.size = ifelse(input$week_number, input$week_number_size, NA),
          monthnames = monthnames,
          months.size = input$months_size,
          months.col = input$months_col,
          months.pos = input$months_pos,
          col = input$linecolor,
          lwd = input$lwd,
          lty = input$lty,
          font.family = input$font_family,
          font.style = input$font_style,
          ncol = ifelse(is.na(input$ncol), define_cols(12, input$orientation), input$ncol),
          margin = input$margin,
          bg.img = bgimg
        ) +
          ggplot2::theme(legend.text = ggplot2::element_text(size = input$legend_size, color = input$legend_color),
                         panel.spacing.x = ggplot2::unit(input$margin_x, "in"),
                         panel.spacing.y = ggplot2::unit(input$margin_y, "in"))
      }

    } else if (input$date_selection == "month") {
      # Generate calendar
      montn <- match(input$selected_month, month.name)
      dates <- as.Date(paste0("2024-", montn, "-01-01"))
      if(input$month_names){
        monthnames <- unlist(strsplit(input$month_names_vals, ","))
      } else{
        monthnames <- format(dates, "%B")
        monthnames <- paste0(toupper(substr(monthnames, 1, 1)), substr(monthnames, 2, nchar(monthnames)))
      }
      ano <- seq(1990, 2100)

      if(input$add_text){
        # Split input strings into vectors
        text_vals <- strsplit(input$events, ",\\s*")[[1]]
        text_pos <- as.numeric(strsplit(input$positions, ",\\s*")[[1]])
      } else{
        text_vals <- NULL
        text_pos <- NULL
      }
      text <- ifelse(input$add_text, input$events, "")
      calendR::calendR(
        month = montn,
        year = ano[match(input$selected_year_month, ano)],
        orientation = input$orientation,
        mbg.col = input$mbg_col,
        bg.col = input$bg_col,
        low.col = input$bg_col_day,
        title = input$title,
        title.size = input$title_size,
        title.col = input$title_col,
        text = text_vals,
        text.pos = text_pos,
        text.col = input$special_days_col,
        day.size = input$day_size,
        special.days = text_pos,
        text.size = input$special_days_size,
        special.col = input$special_days_colbg,
        subtitle = input$subtitle,
        subtitle.size = input$subtitle_size,
        subtitle.col = input$subtitle_col,
        days.col = input$days_col,
        weeknames = weeknames,
        weeknames.col = input$weeknames_col,
        weeknames.size = input$weeknames_size,
        week.number = input$week_number,
        week.number.col = ifelse(input$week_number, input$week_number_col, NA),
        week.number.size = ifelse(input$week_number, input$week_number_size, NA),
        monthnames = monthnames,
        months.size = input$months_size,
        months.col = input$months_col,
        months.pos = input$months_pos,
        col = input$linecolor,
        lwd = input$lwd,
        lty = input$lty,
        font.family = input$font_family,
        font.style = input$font_style,
        ncol = ifelse(is.na(input$ncol), define_cols(num_months, input$orientation), input$ncol),
        lunar = input$add_lunar,
        lunar.col = input$lunar_col,
        lunar.size = input$lunar_size,
        margin = input$margin,
        bg.img = bgimg
      )
    }
  })


  output$calendar_plot <- renderPlot({
    calendar_plot()
  })

  getmeasures <- function(format, papersize, orientation){
    switch (papersize,
            A6 = {
              a <- 14.8
              b <- 10.5

            },
            A5 = {
              a <- 21.0
              b <- 14.8

            },
            A4 = {
              a <- 29.7
              b <- 21.0
            },
            A3 = {
              a <- 42.0
              b <- 29.7
            },
            A2 = {
              a <- 59.4
              b <- 42.0
            },
            A1 = {
              a <- 84.1
              b <- 59.4
            },
            A0 = {
              a <- 118.9
              b <- 84.1
            }
    )
    if(orientation == "landscape"){
      measures <- c(a, b)
    } else{
      measures <- c(b, a)
    }
    return(measures)
  }

  measures <- reactive({
    getmeasures(input$format, input$papersize, input$orientation)
  })

  observe({
    updateNumericInput(session, "width", value = measures()[1])
    updateNumericInput(session, "height", value = measures()[2])
  })

  # Download handler for the calendar
  output$calendar <- downloadHandler(
    filename = function() {
      paste0("calendar_", format( Sys.Date(), "%Y"), "_", input$orientation,  ".", input$format, sep = "")
    },
    content = function(file) {

      ggplot2::ggsave(
        filename = file,
        plot = calendar_plot(),
        device = input$format,
        width = input$width,
        height = input$height,
        dpi = input$resolution,
        units = "cm"
      )
    }
  )
}
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
