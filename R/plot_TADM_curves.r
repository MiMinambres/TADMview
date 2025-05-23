#' Reads the main table and plots all the TADM curves.
#'
#' This function reads the main table created with db_connection and  
#' plot all the TADM curves into the browser and/or pdf report.
#'
#' @param main_table main table created with db_connection
#' @param bln_runanalysis analysis mode, one of 1, 2 or 3 (1=browser; 2=pdf report; 3=both)
#' @param str_testname test name for the pdf report
#' @return TADM curves in browser and/or pdf report
#' @export
plot_TADM_curves <- function(main_table, bln_runanalysis, str_testname){

liquid_classes <- unique(main_table$LiquidClassName)

#loop to retrieve data from database and plot the TADM curves
for (i in liquid_classes){
	
	print(i)
	
	temp_table_1 <- subset(main_table, main_table$LiquidClassName == i)
	
	temp_table_1$StepType[which(temp_table_1$StepType == -533331728)] <- "Aspirating"
	
	temp_table_1$StepType[which(temp_table_1$StepType == -533331727)] <- "Dispensing"
	
	
	volumes <- unique(temp_table_1$Volume)
	
	for (j in volumes){
		
		print(j)
		
		temp_table_2 <- subset(temp_table_1, temp_table_1$Volume == j)
		
		steptype <- unique(temp_table_2$StepType)
		
		for (k in steptype){
			
			print(k)
			
			temp_table_3 <- subset(temp_table_2, temp_table_2$StepType == k)
			
			curve_points <- temp_table_3$CurvePoints
			
			curve_times <- substring(temp_table_3$TimeStamp, 12, 19)
			
			curve_names <- paste("Ch", temp_table_3$ChannelNumber, curve_times, sep = "_")

			names(curve_points) <- curve_names

			n_curves <- c(1:length(curve_points))
			
			loop_index <- 1
			
			for (l in n_curves){
			
				curve_values <- unlist(curve_points[[l]])
				
				curve_values <- readBin(curve_values, integer(), n = length(curve_values), size = 2)
				
				curve_name <- names(curve_points[l])

				if(loop_index == 1){
				
					TADM_table <- cbind(TADMview::get_n_increments(length(curve_values)), curve_values)
					
					colnames(TADM_table) <- c("Time", curve_name)
					
					} else {
						
						TADM_table <- cbind(TADM_table, curve_values)
					
						colnames(TADM_table)[ncol(TADM_table)] <- curve_name
						
						}
				
				loop_index <- loop_index + 1
				
				}
					
					

			
			TADM_table <- as.data.frame(TADM_table)

			channels <- colnames(TADM_table)[-1]

			color_background <- "#fafcfc"

			plot_title <- paste("Liquid class: ", i, "    ||    Volume: ", j, "    ||    Step: ", k, sep = "")

			Line <-  googleVis::gvisLineChart(TADM_table, xvar="Time", yvar=channels,
                       options=list(
                         title=plot_title,
                         titleTextStyle="{color:'red', 
                                           fontName:'Courier', 
                                           fontSize:16}",                         
                         backgroundColor=color_background,                          
                         vAxis="{title:'Pressure', titleTextStyle:{color:'blue'}, gridlines:{color:'pink', count:3}, viewWindowMode:'maximized'}",
                         hAxis="{title:'Time', titleTextStyle:{color:'blue'}}",
                         legend="right",
                         curveType="function",
                         width=1000,
                         height=500                         
                       ))

			if(bln_runanalysis == "1" || bln_runanalysis == "3"){
			
			browseURL(plot(Line))

			Sys.sleep(0.05)
			}
			
			#Reshape table for ggplot2
			
			ncols_table <- ncol(TADM_table)
			
			reshape_table <- TADM_table[, c(1,2)]
			
			reshape_table <- cbind(reshape_table, colnames(reshape_table)[2])
			
			colnames(reshape_table) <- c("Time", "TADM_value", "Channel")
			
			plot_title <- paste("Liquid class: ", i, "    ||    Volume: ", j, "    ||    Step: ", k, sep = "")

			if(ncols_table > 2){
			for (m in (3:(ncols_table))){
				
				temp_reshape_table <- TADM_table[, c(1,m)]
				
				temp_reshape_table <- cbind(temp_reshape_table, colnames(temp_reshape_table)[2])
				
				colnames(temp_reshape_table) <- c("Time", "TADM_value", "Channel")
				
				reshape_table <- rbind(reshape_table, temp_reshape_table)
				}
			}
			graph <- ggplot2::ggplot(reshape_table, ggplot2::aes(x=Time, y=TADM_value, group=Channel, color=Channel)) +
			ggplot2::geom_line() + ggplot2::ggtitle(plot_title)
			graph <- graph + ggplot2::theme(legend.position="bottom")
			graph <- graph + ggplot2::guides(fill=ggplot2::guide_legend(nrow=5, byrow=TRUE))
			
			if(! exists("graph_list")){graph_list <- list(graph)}else{graph_list <- c(graph_list, list(graph))}

			}
		}
	}

if(bln_runanalysis == "2" || bln_runanalysis == "3"){
time_temp <- Sys.time()
pdf_name <- paste("Reports/", substr(time_temp, 1, 10), "_", substr(time_temp, 12, 16), "_", str_testname, ".pdf", sep = "")
pdf_name <- gsub(":", "-", pdf_name)


pdf(pdf_name, onefile=TRUE, width=10, height=7)
invisible(lapply(graph_list, print))
dev.off()
}
}
