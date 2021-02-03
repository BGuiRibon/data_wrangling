#// version 0.4

## ----functions, include=FALSE, fig.width=16, fig.height=10----------------------------------------------------------------------------------

#// FUNCTION: save ggplots into PDF
     
pdf_plot <- function(plots, name, width, height) {
     
     #// ensure that previous plots are not overwritten     
     if (file.exists(paste0(name, ".pdf"))) { return(list(name = paste0(name, ".pdf"), saved = FALSE)) }
	
	#// open port to pdf
          pdf(file = paste0(name, ".pdf"),   #// The directory you want to save the file in
          width = width, #// The width of the plot in inches
          height = height) #// The height of the plot in inches

     #// display all plots
          for (i in plots) { print(i) }
     
     # close port to pdf
          dev.off()
          invisible(NULL)

     return(list(name = paste0(name, ".pdf"), saved = TRUE))
}


#// FUNCTION: Custom Box Plots

cust_boxplot <- function(df, x_val, y_val, color, r_fac, c_fac, title) {

     #// pipe dataframe into plot
          df %>% 

     #// basic plot setup
          ggplot(aes(x = .data[[x_val]], y = .data[[y_val]], fill = .data[[color]])) + 

     #// geoms for box_ and point_
          geom_boxplot(position = position_dodge(width = .9)) +
          geom_point(position = position_jitterdodge(dodge.width = 0.9)) +

     #// custom color scale for discrete variable
          scale_fill_viridis_d(begin = 0.2,
          				 end = 1,
          				 alpha = 1,
          				 direction = 1,
          				 na.value = "gray") + 

     #// theme and text layouts
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = {{title}},
          caption = caption) +
     #// facet layout
          facet_grid(rows = vars( .data[[r_fac]] ), cols = vars(.data[[c_fac]]) )     

}

#// FUNCTION: Plate Heat Maps

plate_heat_map_plot <- function(col, plate, df) { 
	
	#// code to adjust legend breaks and labels based on range
	#// get range of values to plot
	values <- df[df$Plate_ID == plate, colnames(df) == col]
	
	#// check if values are 0 or negative to set scale at identity and leave breaks/labels at default
	if ((min(values, na.rm = TRUE) <= 0 ) | max(values, na.rm = TRUE) / min(values, na.rm = TRUE) <= 100) {
		scale_trans <- "identity" 
		legend_breaks <- pretty(min(values, na.rm = TRUE):max(values, na.rm = TRUE), 6)
		legend_labels <- format(round(legend_breaks, 2), scientific = F)
	} else {
		#// set scale to log
		scale_trans <- "log"
		
		#// define very broad range of breaks
		#// determine if breaks should have half-log or full-log steps
		if ( max(values, na.rm = TRUE) / min(values, na.rm = TRUE) > 10000 ) {
			#// use full log steps if five or more log steps
			legend_breaks <- round(10^seq(-10, 10, 1), 2)
		} else {
			#// use half log steps if less than 5 log steps
			legend_breaks <- round(10^seq(-10, 10, 0.5), 2)
		}
		
		#// define legend labels as non-scientific for 'reasonable' values
		if ( (min(values, na.rm = TRUE) >= 0.001) & (max(values, na.rm = TRUE) <= 1000) ) { 
			legend_labels <- format(legend_breaks, scientific = F)
			
			#// use scientific notification if values are 'unreasonable'
		} else {
			legend_labels <- format(legend_breaks, digits = 2, width = 5, format = "e")
		}
	}
	
	#// nudge for offsetting text labels
	nudge <- c( rep(0.25, times = 16), rep(-0.25, times = 16))    
	#// define label independently from col and adjust to reflect correct info to display
	label <- col
	
	#// filter out all data exect for plate to plot
	df %>% filter(Plate_ID == {{ plate }}) %>% 
		
		#// generate plot by Row and Col
		ggplot(aes(x = Plate_Col, y = Plate_Row)) + 
		
		#// set up axis/grid
		#// set coord_fixed to maintain proper plate ration		
		coord_fixed() + 
		#// set labels for plate
		scale_x_discrete(limits = c(as.character(seq(1, 24)))) +
		scale_y_discrete(limits = rev(levels(as.factor(df$Plate_Row)))) +
		
		#// set up theme, title, and caption
		#// place legend at bottom of the plot
		theme(legend.position = "bottom",
			 legend.box = "horizontal",
			 text = element_text(size = 20)) +
		#// generate automatic title based on column and plate ID
		labs(title = paste("Readout:", col, "\n1st unique plate:", plate) ,
			#// Experiment ID as caption
			caption = caption) +
		
		#// plot data and labels
		geom_tile(aes(fill = .data[[col]]), color = "black") +
		
		scale_fill_viridis_c(begin = 0,
						 alpha = 0.5,
						 direction = -1,
						 na.value = "gray",
						 trans = {{ scale_trans }},
						 breaks = {{ legend_breaks }},
						 labels = {{ legend_labels }} ) +
		
		guides(fill = guide_colorbar(barwidth = unit(6, "in"),
							    barheight = unit(.6, "in"))) +
		
		geom_text(aes(x = Plate_Col,
				    y = Plate_Row, 
				    label = .data[[label]]), 
				check_overlap = TRUE,
				nudge_y = nudge, size = 4) 
	
}

#// FUNCTION: Propagation of positive and negative controls


ctrl_propagation <- function(df, ctrl_level, ctrl_var, group_vars) {

group_vars <- unlist(group_vars, use.names = F)
  
#// generate a list of lists that contains all relevant controls only
temp_ctrls <- df %>%
     #// group by variables with separate DMSO controls
     group_by( across(all_of(group_vars)) ) %>%
     #// identify and filter all controls
     filter( across(all_of(ctrl_var)) != "none") %>%
             # #// legacy filter below: across without all_of soon deprecated
             # #// identify and filter all controls
             # #// filter( across(ctrl_var) != "none") %>%
     #// remove column for Drug_ID
     select(-.data[[ctrl_lvl]]) %>%
     #// split groups into individual lists
     nest()
#// change names of data column to dmso
names(temp_ctrls)[[which(names(temp_ctrls) == "data")]] <- "ctrl"

#// generate a list of lists that contains all data that needs appending
temp_data <- df %>%
     #// keep only items that are not Drug_controls (pos or neg)
     filter(across(all_of(ctrl_var)) == "none") %>%
             # #// legacy filter below: across without all_of soon deprecated
             # #// keep only items that are not Drug_controls (pos or neg)
             # #// filter(across(ctrl_var) == "none") %>%
     #// group by variables now including Drug_ID
     group_by( across(all_of(c(group_vars, ctrl_lvl))) ) %>%
     #// split groups into individual lists
     nest()

#// merge two nested lists by Cell_Line, DOX, and time
temp_data <- full_join(temp_data, temp_ctrls)

#// keep only items that are not Drug_controls (pos or neg)
#data_ctrl <- filter(data_ctrl, Drug_control == "none")

#// assemble control and data and unnest
df_merged <- temp_data %>%
     #// create new list column with merged data + ctrl
     mutate(merged = map2(data, ctrl, rbind)) %>%
     #// remove extraneous data columns
     select(-data, -ctrl) %>%
     #// unnest everything into a single dataframe
     unnest(cols = c(merged))

return(df_merged)

}

