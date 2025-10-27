library(R6)
#' A lightweight discrete event simulation class
#' @return an R6 class, including schedule_at(), remove(), clear(), now(), previous() and run()
DES =
  R6Class("DES",
          public = list(
            times = NULL,               # numeric (vector)
            events = NULL,              # list
            current_time = NULL,        # numeric (scalar)
            previous_event_time = NULL, # numeric (scalar)
            #' Function to schedule an event at the given time
            #'
            #' Note: the event queue is ordered to ensure the earliest occurs first
            #' (by event time and then by entry order).
            #'
            #' @param time time an event occurs
            #' @param event an event, which can be a character name or some other R value
            schedule_at = function(time,event) {
              if(time < self$current_time)
                stop("New event time less than current time")
              ## findInterval locates where to insert the new time, maintaining the
              ## the ordinal nature of the times
              insert_ord = findInterval(time,self$times)
              ## modifies times and events
              self$times = append(self$times,time,insert_ord)
              self$events = append(self$events,event,insert_ord)
              invisible(self)
            },
            #' Internal method to pop the head of the event queue and return its value.
            #' @return the first event with a "time" attribute.
            pop = function() {
              head = self$events[[1]]
              ## add attributes for time
              attr(head,"time") = self$times[1]
              ## Remove head from times, and event times and return head.
              self$times = self$times[-1]
              self$events = self$events[-1]
              return(head)
            },
            #' Method to clear the event queue
            clear = function() {
              ## just replacing times and events with empty values
              self$times = numeric()
              self$events = list()
              invisible(self)
            },
            #' Function to remove events that satisfy some indicator
            #' @param values the values for the events to be removed
            #' @examples
            #' remove("Adjuvant treatment")
            remove = function(values) {
              loc_indicator = (self$events %in% values)
              self$times = self$times[!loc_indicator]
              self$events = self$events[!loc_indicator]
              invisible(self)
            },
            #' @return the current time
            now = function() self$current_time,
            #' @return the previous event time
            previous = function() self$previous_event_time,
            #' Virtual method - this should be user-defined
            init = function() {stop("Virtual method to be implemented!")},
            #' Virtual method - this should be user-defined
            handle_event = function(event) {stop("Virtual method to be implemented!")},
            #' Virtual method - this can be user-defined
            final = function() {},
            #' Run for a single individual simulation. This is the core of the DES.
            #' @param init a function for initialising a simulation
            #' @param handle_event a function for responding to each event
            run = function(start_time = 0) {
              ## To run one simulation, clear the event queue and set previous_event_time = 0 and
              ## current_time = 0
              self$clear()
              self$previous_event_time = self$current_time = start_time
              ## initialize the other parameters and event queue (user-defined)
              self$init()
              ## While there are still some events (guaranteed to start 1 loop since init
              ## adds to the event and times list.
              while(length(self$times)>0) {
                ## run the DES by first getting the next event using pop()
                event = self$pop()
                ## setting the current_time when the event occured
                self$current_time = attr(event, "time")
                ## then for each event, doing the appropriate set of actions
                self$handle_event(event)
                ## then updating the previous event time.
                self$previous_event_time = self$current_time
              }
              self$final()
            }))
