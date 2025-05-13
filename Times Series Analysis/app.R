#https://zahirodini.shinyapps.io/wiki_dashboard/
library(shiny)
library(shinydashboard)
library(ggplot2)
library(fpp2)

start_date = as.Date("2015-07-01")
end_date = as.Date("2017-07-31")
date_seq = seq(from = start_date, to = end_date, by = 1)
dates = format(date_seq, "%b %Y")

df = read.csv('wikipedia_mobile_traffic.csv', row.names=1)

starting_ts = 'Idris_Elba'
x_start = ts(df[,starting_ts], start= c(1,4) , frequency=7)

ui = dashboardPage(
	dashboardHeader(title = "MA 611 Dashboard"),
	dashboardSidebar(
		suppressWarnings(selectizeInput("ts_choice",
										"Select Page",
										choices = colnames(df),
										options= list(maxOptions = ncol(df)),
										selected=starting_ts)),
		numericInput("x_freq",
					 label = "Frequency:",
					 value = 7),
		actionButton("update", "Update"),
		hr(),
		radioButtons("forecast_method",
					 "Forecast Method:",
					 choices = c('Naive' = 'naive',
					 			'Seasonal Naive' = 'snaive',
					 			'Holt-Winters'= 'hw',
					 			'SARIMA'= 'auto.arima'),
					 selected='snaive'),
		numericInput("fcast_horiz",
		             label = "Forecast Horizon:",
		             value = 30),
		radioButtons("fcast_level",
		             "Forecast CI:",
		             choices = c('None' = '0',
		                         '90%' = '.9',
		                         '95%'= '.95',
		                         '99%'= '.99'),
		             selected='.95')
	),
	
	dashboardBody(
		fluidRow(
			box(h5("This dashboard presents data on mobile device page visits for a specific Wikipedia page between July 1, 2015, and June 30, 2017. To view forecast values on the 'Time Series Plot', customize forecast method, horizon, and confidence interval. Further down the dashboard, you'll find comparative accuracy metrics of methods alongside a seasonal plot of historical data. Note that changing the frequency to any number other than 7 will render the plot incompatible with this data. Lastly, explore the 'Decomposition Plot' depicting trend, seasonality, and error, including the strength of trend and seasonality. "), width=12)
		),
		fluidRow(
			box(title = "Time Series Plot",
				status = "primary",
				solidHeader = TRUE,
				width=12,
				plotOutput("ts_plot", height = 300))
		),
		fluidRow(
			column(4,
				   fluidRow(h4("In-Sample Accuracy:")),
				   fluidRow(tableOutput('accuracy_table')),
				   fluidRow(h4("Component Strength:")),
				   fluidRow(tableOutput('strength_table'))),
			column(8,
				   fluidRow(box(plotOutput("season_plot", height = 300), width=12)),
				   fluidRow(box(plotOutput("decomp_plot", height = 500), width=12)))
		)
	)
)

server = function(input, output, session) {
	
	x = reactiveValues(data = x_start, ts_name = starting_ts, freq=7)
	
	observeEvent(input$update, {
		x$freq = input$x_freq
		x$ts_name = input$ts_choice
		x$data = ts(df[,x$ts_name], start=c(1,4), frequency=x$freq)
	})
	
	output$ts_plot = renderPlot({
		
		fcast_horiz = input$fcast_horiz
		fcast_level = as.numeric(input$fcast_level)
		
		if (input$forecast_method == 'naive'){
			x.plot = naive(x$data, h=fcast_horiz, level=fcast_level)
		} else if (input$forecast_method == 'snaive'){
			x.plot = snaive(x$data, h=fcast_horiz, level=fcast_level)
		} else if (input$forecast_method == 'hw'){
			x.plot = hw(x$data, h=fcast_horiz, level=fcast_level)
		} else if (input$forecast_method == 'auto.arima'){
			x.plot = forecast(auto.arima(x$data, stepwise = FALSE), h=fcast_horiz, level=fcast_level)
		}
		
		autoplot(x.plot,
				 color='blue',
				 xlab=NULL,
				 ylab='Page Visitors',
				 main= x$ts_name) +
			theme(title = element_text(size = 14)) +
			scale_x_continuous(
				breaks = 1 + (2 + c(1, 93, 185, 276, 367, 459, 551, 643, 731)) / x$freq,
				labels = dates[ 1 + (2 + c(1, 93, 185, 276, 367, 459, 551, 643, 731)) ]
			)
	})
	
	output$season_plot = renderPlot({
		
		ggseasonplot(x$data) +
			ggtitle('Seasonal Plot') +
			theme(legend.position="none")
	})
	
	output$decomp_plot = renderPlot({
	  
	  autoplot(decompose(x$data)) +
	    ggtitle('Decomposition Plot')
	})
	
	output$accuracy_table = renderTable({
		
		acc_table = t(data.frame(
			accuracy(naive(x$data))[,2:3],
			accuracy(snaive(x$data))[,2:3],
			accuracy(hw(x$data))[,2:3],
      accuracy(auto.arima(x$data))[,2:3])
		)
		
		rownames(acc_table) = c('NAIVE', 'SNAIVE', 'Holt-Winters', 'SARIMA')
		
		acc_table
		
	},
	rownames=TRUE
	)
	
	output$strength_table = renderTable({
	 	
	 	x.decomp = decompose(x$data)
	 	
	 	F_T = 1 - var(x.decomp$random, na.rm=T) / var(x.decomp$random + x.decomp$trend, na.rm=T)
	 	F_S = 1 - var(x.decomp$random, na.rm=T) / var(x.decomp$random + x.decomp$seasonal, na.rm=T)
	 	
	data.frame("Trend" = round(F_T, 3),
	           "Season" = round(F_S, 3))
	 })
	
}

shinyApp(ui, server)

