##----------------------------------------------------------------------------##
## Clear selection button event handler.
##----------------------------------------------------------------------------##
observeEvent(input[["spatial_projection_clear_selection"]], {
  ## Call JavaScript function to clear the plotly selection
  shinyjs::js$spatialClearSelection()
})

##----------------------------------------------------------------------------##
## Toggle visibility of clear selection button.
##----------------------------------------------------------------------------##
observe({
  req(spatial_projection_data_to_plot())
  
  if (
    !is.null(plotly::event_data("plotly_selected", source = "spatial_projection")) &&
    length(plotly::event_data("plotly_selected", source = "spatial_projection")) > 0
  ) {
    shinyjs::show("spatial_projection_clear_selection")
  } else {
    shinyjs::hide("spatial_projection_clear_selection")
  }
})
