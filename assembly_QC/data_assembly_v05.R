## ----loading_libraries, include=FALSE--------------------------------------------------------------------------------------

set.seed(0815)

library(groundhog)
groundhog_day <- "2021-01-01"

groundhog.library("tidyverse",groundhog_day, ignore.deps = "xfun")
groundhog.library("DT", groundhog_day)
groundhog.library("readxl", groundhog_day)




## ----read_meta-------------------------------------------------------------------------------------------------------------

read_meta_data <- function(meta_in_file, in_dir) {

	#// read filetype of in_files
	in_file_type <- unique(tolower(str_extract(meta_in_file, "\\....$|\\.....$")))
	
	#// make sure that there is a single file_type
	if (length(meta_in_file) > 1) stop("Can't process more than one file with metadata at once.")
	
	#// make sure file_types are compatible with input
	if (in_file_type %in% c(".tsv", ".txt", ".csv", ".xls", ".xlsx") == FALSE) stop("Filetype not recognized.")
	
	if (in_file_type %in% c(".tsv", ".txt")) {
	     #// read tab delimited input_files 
		suppressMessages(meta <- read_tsv(paste0(in_dir, meta_in_file), col_names = FALSE))
	     } else if (in_file_type == c(".csv")) {
	          #// read tab delimited input_files 
			suppressMessages(meta <- read_csv(paste0(in_dir, meta_in_file), col_names = FALSE))
	     	} else { 
				#// read in excel input
	     		suppressMessages(meta <- read_excel(paste0(in_dir, meta_in_file), col_names = FALSE))
	     		}
	
	#// remove all NA rows and columns
	meta <- meta[rowSums(is.na(meta)) != ncol(meta),]
	meta <- meta[,colSums(is.na(meta)) != nrow(meta)]

	#// Validate the presence of Experiment_ID and Plate_ID
	if (length(which(meta == "Experiment_ID")) == 0) {
	     	stop("Experiment_ID not found. Please fix input.") }
	
	if (length(which(meta == "Experiment_ID")) > 2) {
	     	stop("Experiment_ID definition is not unique. Please fix input.") }
	
	if (length(which(meta == "Plate_ID")) == 0) { 
			stop("Plate_ID not found. Please fix input.") }

return(meta)

}



## ----read_raw, warning=TRUE, include=FALSE---------------------------------------------------------------------------------

#// read raw data into raw_list
read_raw_data <- function(raw_in_file, in_dir) {
	
	#// skip module if no raw_in_file available
	if (raw_in_file[[1]] == "") { return(FALSE) } 

	#// read filetype of in_files
	in_file_type <- unique(tolower(str_extract(raw_in_file, "\\....$|\\.....$")))
	
	#// make sure that there is a single file_type
	if (length(in_file_type) > 1) stop("Can't process more than one filetype at once.")
	
	#// make sure file_types are compatible with input
	if (in_file_type %in% c(".tsv", ".txt", ".csv", ".xls", ".xlsx") == FALSE) stop("Filetype not recognized.")
	
	#// empty out ls_data
	raw_list <- vector(mode = "list")
	
	if (in_file_type %in% c(".tsv", ".txt")) {
	     #// read tab delimited files
	     raw_list <- lapply(paste0(in_dir, raw_in_file), function(x) read_tsv(x, col_names = TRUE)) 
	     #// name the list items by names of in_file
	     names(raw_list) <- str_split(raw_in_file, "\\....$|\\.....$", 1)
	     } else if (in_file_type == c(".csv")) {
	          #// read comma delimited files
	          raw_list <- lapply(paste0(in_dir, raw_in_file), function(x) read_csv(x, col_names = TRUE))
	          #// name the list items by names of in_file
	          names(raw_list) <- str_split(raw_in_file, "\\....$|\\.....$", 1)
	     } else { 
	          #// read excel files
	          #// loop through each in_file individually and read in all sheets
	          for (i in raw_in_file) {
	               #// read sheet names and number of sheets
	               sheet_names <- excel_sheets(paste0(in_dir, i)) 
	               # read all sheets of input file
	               temp <- lapply( sheet_names, function(x) { read_excel(paste0(in_dir, i), x, col_names = TRUE ) } )
	               # name all sheets in list
	               names(temp) <- sheet_names
	               # append named input list to input list
	               raw_list <- append(raw_list, temp) }
			rm(temp, i, sheet_names)
	     }
	
	return(raw_list)
}



## ----split_plates_wells, echo=FALSE, warning=TRUE--------------------------------------------------------------------------

split_meta <- function(meta, Exp_ID) {

	#// identify entry with 'Experiment_ID'
	row <- which(meta == "Experiment_ID", arr.ind = T)[1]
	col <- which(meta == "Experiment_ID", arr.ind = T)[2]
	
	#// set name of Experiment_ID
	Experiment_ID <- toString(meta[row + 1, col])
	
	#// check that metadata file and provided experiment ID match up
	if (Experiment_ID != Exp_ID) {
		     stop("Provided Experiment_ID in script differs from meta data.")
		}
	
	#// subset all rows of meta that contain "experiment ID" 
	#// %in% checks the resulting vectorized evaluation of "(meta[,col] == "Experiment_ID" | meta[,col] == Experiment_ID)" and tests if it is TRUE
	plates <- meta[(meta[, col] == "Experiment_ID" | meta[,col] == Experiment_ID) %in% TRUE, ]
	
	#// subset all rows of meta with the logical inverse of plates
	wells <- meta[!((meta[, col] == "Experiment_ID" | meta[,col] == Experiment_ID) %in% TRUE), ]
	

return(list(plates, wells))

}




## ----clean_and_QC, echo=FALSE, warning=TRUE--------------------------------------------------------------------------------

parse_plates <- function(plates) {
	
	#// clean up and sanity check for plates 
		
#// don't think I need this; passed QC on 1/26 without--remove in next version
		# #// remove NA columns as above
		# plates <- plates[,colSums(is.na(plates)) != nrow(plates)]

		#// remove cols where header is empty
		plates <- plates[ ,as.vector(!is.na(plates[1, ]))]

		#// check for presence of any NA in rows
		if (sum(rowSums(is.na(plates))) > 0) {
		     stop("Plate annotation is incomplete. Missing values can't be interpreted.")
		}
		
		#// convert first row into names and remove that row
		colnames(plates) <- as.character(unlist(plates[1,]))
		plates <- plates[-1,]
		
		#// Set up Experiment_ID level and Plate_ID

		#// check that there is only one entry in Experiment_ID
		if (n_distinct(plates$Experiment_ID) != 1) {
		     stop("Experiment_ID is not identical across all plates.")
		}
		
		#// set up experiment_level dataframe and remove from plates
		exp_level <- data.frame(Experiment_ID = plates$Experiment_ID, stringsAsFactors = F)
		plates <- plates[ , !(names(plates) == "Experiment_ID")]
		
		#// check that there are nrow entries in Plate_ID
		if (n_distinct(plates$Plate_ID) != nrow(plates)) {
		     stop("Plate_ID is not unique across all plates.")
		}
		
		#// set up plate_level dataframe and remove from plates
		plate_level <- data.frame(Plate_ID = plates$Plate_ID, stringsAsFactors = F) 
		plates <- plates[ , !(names(plates) == "Plate_ID")]

	#// parsing of plates
			
		#// number of unique vals per column
		unique_vals <- unname( sapply(plates, function(x) length(unique(x))) )  
		unique_cols <- unname( sapply(plates, function(x) length(unique(x)) == 1) ) 

		#// append all unique columns to exp_level
		exp_level <- cbind(exp_level, plates[ ,unique_vals == 1])
		#// label all experiment columns with 'e.'
		colnames(exp_level) <- paste0("e.", colnames(exp_level))
		
		#// append the opposite selection to plate_level
		plate_level <- cbind(plate_level, plates[ ,unique_vals != 1])
		#// label all plate columns with 'p.'
		colnames(plate_level) <- paste0("p.", colnames(plate_level))
	
		#// insert Plate_No column after 'p.Plate_ID'
		plate_level <- add_column(plate_level, p.Plate_No = as.integer(1:nrow(plates)), .after = "p.Plate_ID") 

		#// Generate Plate_Row, _Col, Well_No
		#// label all well columns with 'w.'
		well_level <- data.frame(w.Plate_Row = as.factor(rep(LETTERS[seq(1:16)], 24)),
		                         w.Plate_Col = as.factor(trunc((seq(1:384) + 15)/16)),
		                         w.Well_No = as.factor(seq(1:384)))
		
		#// add column for Well_ID (A01-P24)
		well_level <- well_level %>% 
		     #// Well_ID is concatenation of Plate_Row and Plate_Col
		     #// str_pad adds a leading character ("0") to width (2)
		     mutate(w.Well_ID = as.factor(paste0(w.Plate_Row, str_pad(w.Plate_Col, 2, pad = "0")))) %>%
		     #// sort by well number
		     arrange(w.Well_No)

return(list(exp_level, plate_level, well_level))

}




## ----extract_well_annotation, echo=FALSE, warning=TRUE---------------------------------------------------------------------

parse_wells <- function(wells, well_level) {

	#// identify positions of "variable"
	variables <- data.frame(which(wells == "variable", arr.ind = TRUE))   	#// identify x/y of 'variables' 
	variables <- variables[order(variables$row),]                         	#// sort variables table by row

	#// return if there are no well-level variable
	if (nrow(variables) == 0) { return(well_level) }
	
	#// join 384 row well_level info with readouts
	end_row <- 0
	for (i in 1:nrow(variables)) {                                   
	
	#// empty out dataframe
	     slice <- data.frame()

	     #// put row/col value of current plate map in row and col
	     row <- variables[i,"row"]
	     col <- variables[i,"col"]
	
	#// identify variable type, names, and attribute
	     type <- wells[row, col + 1]
	     name <- wells[row + 1, col]
	     attribute <- wells[row + 1, col + 1]

	#// make sure there are entries for all variable info
	     if (sum(is.na(c(type, name, attribute))) > 0) { stop("Variable annotation in metadata file incomplete.") }
	     	
	#// make sure there are no underscore in attribute
	     if (str_detect(attribute, "_")) { stop("No underscore are tolerated in attribute field.") }
	     
	#// slice out plate based on location of 'variables'
	     if  (i == nrow(variables) ) { end_row <- nrow(wells) } else { end_row <- variables[i + 1,"row"] - 1 }
	     slice <- wells %>% slice((row + 2):end_row)

	#// remove NA rows and NA columns
	     slice <- slice[rowSums(is.na(slice)) != ncol(slice),] #// remove cols of all NA
	     slice <- slice[,colSums(is.na(slice)) != nrow(slice)] #// remove rows of all NA 

	#// remove 1st column if it matches A-P and 1st rows if it matches 1-24
		#// run test only if there are more than 24 columns (suggests annotation)
	     if (ncol(slice) > 24) { 
			#// collapse first column into single string
			collapse <- str_c(slice[[1]][!is.na(slice[ , 1])], sep = "", collapse = "")
			#// see if letter sequence is present and remove first col with letters
			if (str_detect(toupper(collapse), str_c(LETTERS[1:16], sep = "", collapse = ""))) {
				slice <- slice[-1]
			} 
			#// trim slice down to the first 24 columns
			slice <- slice[, 1:24]
		}	

	    	#// run test only if there are more than 16 rows (suggests annotation)
	     if (nrow(slice) > 16) { 	
			#// collapse first row into single string
			collapse <- str_c(slice[1 , ][!is.na(slice[ 1, ])], sep = "", collapse = "")
	     	#// see if number sequence is present and remove first col with letters
			if (str_detect(collapse, str_c(1:24, sep = "", collapse = "")) | 
			    str_detect(collapse, str_c(str_pad(1:24, 2, side = "left", pad = "0"), sep = "", collapse = ""))) {
				slice <- slice[-1, ]
			}
			#// trim slice down to the first 24 columns
			slice <- slice[1:16, ]
		}

	#// values are the readout of the parsed plate as single vector
	     values <- unlist(slice, use.names = F)
	
	#// ensure that values are properly parsed
	     if (length(values) != 384) { stop( paste0(name, "_", attribute, " couldn't be parsed accurately.")) }
	     
	#// append column to well_level dataframe
	     if (type == "numeric") {
	          	#// convert NULL values in numeric column to NA to suppress warnings
	     		values[values == "NULL"] <- NA     
	     		well_level <- cbind(well_level, as.numeric(values))
	          } else {
	               well_level <- cbind(well_level, values, stringsAsFactors = F)
	          }
	
	     #// add proper metadata heading to new df column 'Name_Attribute' e.g drug_id
	     #// add 'w.' to designate all well_level columns
	     names(well_level)[ncol(well_level)] <- paste0("w.", name,"_",attribute)
	     
	#// relevel so that NULL is last element
	     n <- ncol(well_level)
	     if (any(levels(well_level[ , n]) == "NULL")) 
	      { well_level[ , n] <- factor(well_level[ , n] , levels = c(setdiff(levels(well_level[ , n]), "NULL"),"NULL")) }                                       
	}                                                                   

return(well_level)

}
	



## ----assemble_plate_and_well, echo=FALSE, warning=TRUE---------------------------------------------------------------------

#// assemble the full data frame from plate and well info
assemble_meta <- function(exp_level, plate_level, well_level) {

	#// extract level variables
	well_level_names <- c(colnames(well_level[-(1:4)]))
	plate_level_names <- c(colnames(plate_level))
	exp_level_names <- c(colnames(exp_level)) 

	#// join experiment and plate-level metadata
	exp_plate_level <- cbind(exp_level, plate_level) %>%
		#// Duplicate each row in plates 384x
		slice(rep(row_number(), 384)) %>%
		#// Reorder the file by plate No 
		arrange(p.Plate_No)

	#// combine wells_long and plates_long
	meta <- cbind(exp_plate_level, well_level)
	
		#// check if there are duplicates in plate level and well level that need special attention
		duplicate <- intersect(str_sub(plate_level_names, start = 3, end = -1), str_sub(well_level_names, start = 3, end = -1))
		#// if there are no duplicates skip this module
		if (length(duplicate) != 0) {

		     #// set up list to collect the vectors for merge
		     master <- vector(mode = "list", length(duplicate))
		     names(master) <- paste0("w.", duplicate)

		     #// cycle through each duplicate
		     for (i in 1:length(duplicate)) {
		
		          #// set up list of vectors with what to merge
		          merge_cols_well_level <- str_which(colnames(well_level), paste0("w.", duplicate[i])) 
		          merge_cols_plate_level <- str_which(colnames(plate_level), paste0("p.", duplicate[i]))
		
		          #// cycle through each plate for each duplicate
		          for (j in 1:nrow(plate_level)) {
		
		               #// which level is the current duplicate to merge
		               level <- min( which(plate_level[,merge_cols_plate_level] == plate_level[j, merge_cols_plate_level] ) )
		
		               #// find which elements to add and append them to the merge list
		               master[[i]] <- append(master[[i]], well_level[1:384, merge_cols_well_level[level]])
		
		          } #// end of j-loop
		
		     } #// end of i-loop

		#// remove relevant names from plate level
		plate_level[colnames(plate_level) %in% paste0("p.", duplicate)] <- NULL
		plate_level_names <- c(colnames(plate_level))

#// don't think I need this; passed QC on 1/26 without--remove in next version
#		#// remove relevant names from well level
#		well_level[colnames(well_level) %in% paste0("w.", duplicate)] <- NULL
#		well_level_names <- c(colnames(well_level)[-(1:4)], duplicate)

		#// remove duplicate columns from meta
		meta[ colnames(meta) %in% paste0("p.", duplicate) ] <- NULL
		meta[ colnames(meta) %in% paste0("w.", duplicate) ] <- NULL

		#// merge remaining columns in meta with master list
		meta <- cbind( meta, data.frame(master, stringsAsFactors = F))
		} 

	#// sort by plate and well number
	meta <- arrange(meta, p.Plate_No, w.Plate_Col)
	
	#// give proper row names to complete file
	rownames(meta) <- c(seq(1:nrow(meta)))

	#// rearrange columns by using relocate
	#// go via creation of 'new order' vector in case exp_level or plate_level is empty
	#// this causes dplyr to throw an error
				#// experiment level columns
	new_order <- c("e.Experiment_ID", exp_level_names,
				#// plate level columns
				"p.Plate_ID", "p.Plate_No", plate_level_names,
				#// well level columns
				"w.Plate_Row", "w.Plate_Col", "w.Well_ID", "w.Well_No") 
	
	meta <- meta %>% relocate(new_order[]) 

	#// cross-check that every well_level_names all have a single variable with _ID
	#// regex looks for "_" followed by any character but a "_" until the string-end
	#// lapply then runs through all unique variables
	#// followed by check if that variable has a match with ID
	well_level_names <- c(setdiff(names(meta), new_order))  

	lapply(unique(str_replace(well_level_names, "_[^_]*$", "")), function(x) {
		if ((paste0(x, "_ID") %in% well_level_names) == FALSE) { stop(paste0("No ID variable detected for '", x, "'.")) }
	})


return(meta)

}




## ----ALTERNATIVE_raw_assembly----------------------------------------------------------------------------------------------

#// shorter version of raw data merger
#// based only on lapply and reduce
#// evaluate in parallel for a while
#// only put into production if there are no issues for a while

#// appending everything with _2

#// raw data assembly
parse_raw_2 <- function(raw_list, join_by) {

	#// read all data files into a list using sapply
	raw_list_2 <- raw_list
	
	#// check if join_by is diff for meta and raw, if yes, temporarily switch join_by to raw/raw
	names(join_by) <- unlist(lapply(join_by, '[[', 1), use.names = F)
	
	#// convert names into collapsed strings
	raw_names_2 <- sapply(raw_list_2, function(x) str_c(names(x), collapse = ""))
	
	#// generate index of which files have identical columns
	index_2 <- unname(sapply(raw_names_2, function(x) which(unique(raw_names_2) == x) ))
	
	#// lapply over identical indeces with rbind
	raw_list_2 <- lapply(unique(index_2), function(x) reduce(raw_list_2[index_2 == x], rbind) )

	#// check if full_join is needed
	#// reduce2() throws and error if raw_list_2 only has one element
	if (length(raw_list_2) == 1) { 
		raw_2 <- raw_list_2[[1]] 
	} else {
		# full_join over mergelist
		raw_2 <- reduce2(raw_list_2, list(join_by), full_join)
	}

return(raw_2)

}



## ----raw_assembly, warning=TRUE, include=FALSE-----------------------------------------------------------------------------

#// raw data assembly
parse_raw <- function(raw_list, join_by) {
	
	#// check if join_by is diff for meta and raw, if yes, temporarily switch join_by to raw/raw
	names(join_by) <- unlist(lapply(join_by, '[[', 1), use.names = F)
	
	#// see if any joining is needed
	#// only execute if there is more than one item in raw_list
	if (length(raw_list) == 1) { raw <- raw_list[[1]] } else {
	
		#// convert names into collapsed strings
		raw_names <- sapply(raw_list, function(x) str_c(names(x), collapse = ""))
		
		#// generate index of which files have identical columns
		index <- unname(sapply(raw_names, function(x) which(unique(raw_names) == x) ))
		
		#// list for full_join
		merge_list <- vector(mode = "list", length = length(unique(raw_names)))
	
			#// check if any dataframes need merging by rbind
			#// if not, dump input files into merge list and skip section
			if (length(unique(raw_names)) == length(raw_names)) { merge_list <- raw_list } else { 
				#// loop over number of unique names to merge by identical files by rbind
				for (i in 1:length(unique(raw_names))) {
					
					#// remove all list items that are not equivalent to index i
					temp <- raw_list
					temp[which(index != i)] <- NULL
					#// fill list[[i]] with first dataframe to merge
					merge_list[[i]] <- temp[[1]]
					temp[[1]] <- NULL
					
					#// rbind remaining items in temp with merge_list by rbind
					for (j in 1:length(temp) ) { merge_list[[i]] <- rbind( merge_list[[i]], temp[[j]] ) } 
				}
			}	
			#// see if joining by column is needed
			#// if not, dump single file from merge list into raw for joining with meta
			if (length(merge_list) == 1) { raw <- merge_list[[1]] } else {
				#// plate 1st item into final raw dataset
				raw <- merge_list[[1]]
				
				#// loop over remaining merge_list
				for (i in 2:length(merge_list)) {
					#// perform full join by intersection of col names
					raw <- full_join(raw, merge_list[[i]], by = join_by ) 
				}
			}
		#// clean-up
		rm(i, j, temp, merge_list, raw_list, raw_names, index)
	}

return(raw)
	
}	
	



## ----clean_raw_and_merge_with_meta, echo=FALSE, warning=TRUE---------------------------------------------------------------

merge_meta_raw <- function(meta, raw, join_by) {

	#// Clean up data to be merged

	#// manual shortening of microscopy names
	#// this list can be extending as more experiments with recurring column names are identified
	colnames(raw)[which(colnames(raw) == "% Positive W2 (MultiWaveScoring)")]						<- "Fraction_W2"
	colnames(raw)[which(colnames(raw) == "Cell: W2 Nucleus Average Intensity (MultiWaveScoring)")] 	<- "Nuc_W2"
	colnames(raw)[which(colnames(raw) == "Cell: W2 Cytoplasm Average Intensity (MultiWaveScoring)")]	<- "Cyto_W2"
	colnames(raw)[which(colnames(raw) == "Cell: W2 Cell Average Intensity (MultiWaveScoring)")]		<- "Cell_W2"
	colnames(raw)[which(colnames(raw) == "% Positive W3 (MultiWaveScoring)")]						<- "Fraction_W3"
	colnames(raw)[which(colnames(raw) == "Cell: W3 Nucleus Average Intensity (MultiWaveScoring)")] 	<- "Nuc_W3"
	colnames(raw)[which(colnames(raw) == "Cell: W3 Cytoplasm Average Intensity (MultiWaveScoring)")]	<- "Cyto_W3"
	colnames(raw)[which(colnames(raw) == "Cell: W3 Cell Average Intensity (MultiWaveScoring)")]		<- "Cell_W3"
	colnames(raw)[which(colnames(raw) == "Total Cells (MultiWaveScoring)")]						<- "Cell_No"
		
	#// replace empty spaces with underscores
	colnames(raw) <- gsub(" ", "_", colnames(raw))
	
	#// identify columns that are overlapping between meta and raw
	duplicate <- intersect(str_sub(colnames(meta), 3, -1), colnames(raw))
	
	#// remove duplicate columns used for joining
	duplicate <- setdiff(duplicate, unlist(join_by))

	#// tag all remaining columns with '_raw'
	colnames(raw)[colnames(raw) %in% duplicate] <- paste0(colnames(raw)[colnames(raw) %in% duplicate], "_raw")

	#// make sure that join_by names have a match
	if ( length( intersect(names(join_by), str_sub(colnames(meta), 3, -1)) ) != length(join_by) |
		length( intersect(join_by,        colnames(raw)) )                  != length(join_by) ) {
			stop("Columns for joining can't be unambiguously matched.") }
	
	#// grab join_by names from col_names to make sure the have appropriate prefix
	#// cycle through all names of join_by
	for (p in 1:length(names(join_by))) {
		#// replace names at position p with match in colnames
		names(join_by)[p] <- str_subset(colnames(meta), names(join_by)[p])
		
		#// check if meta column to join is numeric
		if ( is.numeric( meta[[ which(colnames(meta) == names(join_by)[p]) ]]) ) {
		#// if yes, turn raw column to join numeric as well
			raw[[ which(colnames(raw)  == join_by[p]) ]] <- as.numeric( raw[[ which(colnames(raw) == join_by[p]) ]]) 
		} else {
		#// if no , turn raw column to join into character
			raw[[ which(colnames(raw)  == join_by[p]) ]] <- as.character( raw[[ which(colnames(raw) == join_by[p]) ]]) 
		} #// end if_else
	
	} #// end for

	#// join by Plate_ID column and Well_ID
	complete <- full_join(meta, raw, by = join_by)

	#// make sure that all rows in raw were matched to an existing row in meta
	if (nrow(complete) > nrow(meta)) { stop("Some rows in the raw data couldn't be matched to the meta data.")}
		
	#// label are raw data columns with 'd.'
	colnames(complete)[str_detect(colnames(complete), "^e\\.|^p\\.|^w\\.", negate = TRUE)] <- 
		paste0("d.", colnames(complete)[str_detect(colnames(complete), "^e\\.|^p\\.|^w\\.", negate = TRUE)] )
	
	#// mutate all numeric columns by rounding to 10 significant digits to remove variation
	complete <- complete %>% mutate(across(where(is.numeric), ~signif(. , 10)))

return(complete)

}





## ----table_summary, echo=FALSE---------------------------------------------------------------------------------------------

#// table representation of experiment level annotation
generate_qc_tables <- function(complete, meta) {
	
	exp_level_names	<- str_subset(colnames(meta), "^e\\.")
	plate_level_names	<- str_subset(colnames(meta), "^p\\.") 
	well_level_names	<- str_subset(colnames(meta), "^w\\.")
													
	tables <- list()

	#// select only columns with names in 'exp_level_names'
	exp_level_table <- meta[1,exp_level_names]
	#// extract values w/o names as characters from every column
	exp_level_values <- list(values = unlist(lapply(exp_level_table, as.character), use.names = FALSE))
	#// generate new df with name and value column
	exp_level_table <- data.frame(names = exp_level_names, values = exp_level_values )
	
	#// experiment_level table
	tables[[length(tables) + 1]] <- datatable(exp_level_table,
			filter = 'top',
			caption = htmltools::tags$caption("Experiment-level metadata.", 
		     style = 'caption-side: top; text-align: left; color:blue; font-size:150% ;'),
	 		options = list(
				columnDefs = list(list(className = 'dt-center', targets = 1:ncol(exp_level_table))), 
				pageLength = 16,
				lengthMenu = c(8, 16, 96, 384),
				class = "stripe hover compact",
				style = "width:100%",
				scrollX = T))
		
	#// table representation of plate level annotation
	
	#// select all columns based on plate level name and extract distinct values based on each Plate ID
	plate_level_table <- meta %>% select(all_of(plate_level_names)) %>% group_by(p.Plate_ID) %>% distinct()
	#// plate_level table
	tables[[length(tables) + 1]] <- datatable(plate_level_table,
			filter = 'top',
			caption = htmltools::tags$caption("Plate-level metadata.", 
		     style = 'caption-side: top; text-align: left; color:black; font-size:150% ;'),
	 		options = list(
				columnDefs = list(list(className = 'dt-center', targets = 1:ncol(plate_level_table))), 
				pageLength = 16,
				lengthMenu = c(8, 16, 96, 384),
				class = "stripe hover compact",
				style = "width:100%",
				scrollX = T))
	
	#// table representation of well level annotation
	
	#// grepl("_ID", well_level) Searches for any list entry that contains the string "_ID" in the list of "well_level"
	#// substr cutting of the last three characters "_ID" through substring(x, 1, nchar(x)-3)
	well_level_ids <- substr(well_level_names[grepl("_ID", well_level_names)], 1, nchar(well_level_names[grepl("_ID", well_level_names)]) - 3)
	#// remove Well_ID and Well_No
	well_level_ids <- well_level_ids[well_level_ids != "Well"]
	
	#// subset all columns that start with well_level_ids
	well_level_table <- meta %>% select(all_of(starts_with(well_level_ids))) %>% lapply(unique)
	#// convert into characters so table can be better manipulated
	well_level_table <- lapply(well_level_table, as.character)
	#// fill up empties with NA and turn into a dataframe
	#// lapply makes length of all vectors the same as the max which fills them up with NA
	well_level_table <- data.frame(lapply(well_level_table, function(x) { length(x) <- max(lengths(well_level_table)); x }))
	#// well_level table
	tables[[length(tables) + 1]] <- datatable(well_level_table,
			filter = 'top',
			caption = htmltools::tags$caption("Well-level metadata.", 
		     style = 'caption-side: top; text-align: left; color:black; font-size:150% ;'),
	 		options = list(
				columnDefs = list(list(className = 'dt-center', targets = 1:ncol(well_level_table))), 
				pageLength = 16,
				lengthMenu = c(8, 16, 96, 384),
				class = "stripe hover compact",
				style = "width:100%",
				scrollX = T))
	
	#// full metadata
	tables[[length(tables) + 1]] <- datatable(meta,
			filter = 'top',
			caption = htmltools::tags$caption("All metadata.", 
		     style = 'caption-side: top; text-align: left; color:black; font-size:150% ;'),
			options = list(
				columnDefs = list(list(className = 'dt-center', targets = 1:ncol(meta))),
				pageLength = 16,
				lengthMenu = c(8, 16, 96, 384), class = "stripe hover compact",
				style = "width:100%",
				scrollX = T)
			)

	#// fully assembled dataset
	tables[[length(tables) + 1]] <- datatable(complete,
			filter = 'top',
			caption = htmltools::tags$caption("Fully assembled dataset.", 
		     style = 'caption-side: top; text-align: left; color:black; font-size:150% ;'),
			options = list(
				columnDefs = list(list(className = 'dt-center', targets = 1:ncol(complete))),
				pageLength = 16,
				lengthMenu = c(8, 16, 96, 384), class = "stripe hover compact",
				style = "width:100%",
				scrollX = T)
			) 
	
return(tables)
	
}




## ----QC_plot_function, echo=FALSE------------------------------------------------------------------------------------------

#// ggplot function to build plots
qc_plot <- function(col, plate, df, Experiment_ID, cur_date) { 

	#// col			columns of dataframe to plot QC report
	#// plate			plate ID of dataframe to plot QC report
	#// df			dataframe with all relevant data
	#// Experiment_ID	for labels
	#// cur_date		for labels

	#// nudge for offsetting text labels
	nudge <- c( rep(0.25, times = 16), rep(-0.25, times = 16))    
	#// define label independently from col and adjust to reflect correct info to display
	label <- col
     if (str_sub(label, start = -3) != "_ID" ) { label <- paste0(unlist(strsplit(label, "_"))[[1]], "_ID")}

if ( is.numeric( df[[col]] ) ) {
     
	#// plot for numeric variable

	#// code to adjust legend breaks and labels based on range
     #// get range of values to plot
     values <- df[df$p.Plate_ID == plate, colnames(df) == col]

     #// check if only a single number is either <= 0 and range is large
	#// setdiff(unique(values[values <= 0]), NA) is unique values smaller or equal zero
     #// leave all numbers as is but turn unique value 0 or below into NA
     if ( length(setdiff(unique(values[values <= 0]), NA)) == 1 &
     	max(values[values > 0], na.rm = TRUE) / min(values[values > 0], na.rm = TRUE) > 100 ) {
     	values[values == setdiff(unique(values[values <= 0]), NA)] <- NA
     }

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
    	
     #// plot for continuous variable
	plot <- df %>% 
     
	#// filter out all data exect for plate to plot
     filter(p.Plate_ID == plate) %>% 

	#// generate plot by Row and Col
     ggplot(aes(x = w.Plate_Col, y = w.Plate_Row)) + 
     
	#// set up axis/grid
	#// set coord_fixed to maintain proper plate ration		
	coord_fixed() + 
     #// set labels for plate
	scale_x_discrete(limits = c(as.character(seq(1, 24)))) +
     scale_y_discrete(limits = rev(levels(df$w.Plate_Row))) +
 
	#// set up theme, title, and caption
	#// place legend at bottom of the plot
	theme(legend.position = "bottom", legend.box = "horizontal", text = element_text(size = 20)) +
	#// generate automatic title based on column and plate ID
     labs(title = paste("Readout:", col, "\n1st unique plate:", plate) ,
     #// Experiment ID as caption
     caption = paste("Experiment ID:", Experiment_ID, "\nDate: ", cur_date)) +
     
	#// plot data and labels
     geom_tile(aes(fill = .data[[col]]), color = "black", size = 0.5) +
     
	scale_fill_viridis_c(begin = 0,
     				 alpha = 0.5,
     				 direction = 1,
     				 na.value = "gray",
     				 trans = {{ scale_trans }},
                          breaks = {{ legend_breaks }},
                          labels = {{ legend_labels }} ) +
     
	guides(fill = guide_colourbar(barwidth = unit(6, "in"),
     						barheight = unit(.6, "in"))) +
     geom_text(aes(x = w.Plate_Col,
     		    y = w.Plate_Row,
     		    label = .data[[label]]),
     		    check_overlap = TRUE,
     		    nudge_y = nudge,
     		    size = 4) 

     } else {

	#// if "NULL" in col, define breaks and remove NULL
     if ("NULL" %in% unique(df[ which(df$p.Plate_ID == plate) , c(col) ])) {
     	breaks <- c(unique(df[ which(df$p.Plate_ID == plate) , c(col) ])[which(unique(df[ which(df$p.Plate_ID == plate) , c(col) ]) != "NULL") ])
     #// if 0 in col, define breaks and remove 0
     } else if (0 %in% unique(df[ which(df$p.Plate_ID == plate) , c(col) ])) {
     	breaks <- c(unique(df[ which(df$p.Plate_ID == plate) , c(col) ])[which(unique(df[ which(df$p.Plate_ID == plate) , c(col) ]) != 0) ])
     #// otherwise, leave breaks as is
     } else {
          breaks <- c(unique(df[ which(df$p.Plate_ID == plate) , c(col) ]))
     }

     #// plot for categorical variable
	plot <- df %>% 
     #// filter out all data exect for plate to plot
     filter(p.Plate_ID == plate) %>% 

     #// generate plot by Row and Col
     ggplot(aes(x = w.Plate_Col, y = w.Plate_Row)) + 
     
	#// set up axis/grid
	#// set coord_fixed to maintain proper plate ration		
	coord_fixed() + 
     #// set labels for plate
	scale_x_discrete(limits = c(as.character(seq(1, 24)))) +
     scale_y_discrete(limits = rev(levels(df$w.Plate_Row))) +     
     		
	#// set up theme, title, and caption
	#// place legend at bottom of the plot
	theme(legend.position = "bottom", legend.box = "horizontal", text = element_text(size = 20)) +
	#// generate automatic title based on column and plate ID
     labs(title = paste("Readout:", col, "\n1st unique plate:", plate) ,
     #// Experiment ID as caption
     caption = paste("Experiment ID:", Experiment_ID, "\nDate: ", cur_date)) +

     #// plot data and labels		
     geom_tile(aes(fill = .data[[col]]), color = "black", size = 0.5) +
     scale_fill_viridis_d(begin = 0.1,
     				 alpha = 0.5,
     				 direction = -1,
     				 na.value = "gray",
     				 limits = breaks ) +
     geom_text(aes(x = w.Plate_Col, y = w.Plate_Row, label = .data[[label]]), check_overlap = TRUE, nudge_y = nudge, size = 4) 
     
     }
	
return(plot)
	
}





## ----plot_summary, echo=FALSE----------------------------------------------------------------------------------------------

#// function to identify plots to generate
generate_qc_plots <- function(meta, Experiment_ID, cur_date) {
	
	#// extract well-level names
	well_level_names	<- str_subset(colnames(meta), "^w\\.")
	
	#// remove columns that just describe the places
	well_level_names	<- setdiff(well_level_names, c("w.Plate_Row", "w.Plate_Col", "w.Well_ID", "w.Well_No"))
	
	#// abort function if there is no unique well-level data
	if (is_empty(well_level_names)) { return(NA) }
	
	# generate lookup table of unique plates that need to be summarized
	summary_plates <- 
	     meta %>%
		#// group by Plate_IDs for summary testing
		group_by(p.Plate_ID) %>%
		#// generate summary of all values per column as collapsed string
	     summarize(across(all_of(well_level_names), paste, collapse = ""), .groups = "drop") %>%
		#// check per column which one is duplicated (will get true or false value)
		mutate(across(all_of(well_level_names), ~!duplicated(.))) 
	
	#// generate list with entries for each plate to plot
	list_summary_plates <- lapply(summary_plates[2:length(summary_plates)], function(x) { summary_plates$p.Plate_ID[x] })

	#// generate sum_plate and sum_ID vectors for summary plots
	sum_IDs <- c(rep(names(list_summary_plates), lengths(list_summary_plates)))
	sum_plates <- as.character(unlist(list_summary_plates, use.names = F))

	#// call qc_plot with mapply using the IDs and corresponding plate as list arguments
	qc_plots <- mapply(qc_plot, sum_IDs, sum_plates, MoreArgs = list(meta, Experiment_ID, cur_date), SIMPLIFY = FALSE)

return(qc_plots)
	
}



## ----QC_output-------------------------------------------------------------------------------------------------------------

final_qc <- function(QC) {

	#// assembly save_log
	save_log <- as.data.frame(bind_cols(date = Sys.time(), bind_rows(QC[[3]])))
	
	#// print log output
	print(Sys.time())
	print(paste("Data assembly completed successfully:", QC[[1]]))
	print(paste("Alternative raw_merge process working:", QC[[2]]))
	for (i in 1:nrow(save_log)) {
		print(paste(save_log[[i, 2]], "saved:", save_log[[i, 3]]))
		if (save_log[[i, 3]] == FALSE) print("File already exists.")
	}
	
}



## ----core of program, fig.width=16, fig.height=12--------------------------------------------------------------------------

data_assembly <- function(exp_info) {

	#// ===============================================================================================================
	#// parse exp_info list into individual elements
	
	Exp_ID       <- exp_info[[1]]
	in_dir       <- exp_info[[2]]
	out_dir	   <- exp_info[[3]]
	meta_in_file <- exp_info[[4]]
	raw_in_file  <- exp_info[[5]]
	out_file     <- exp_info[[6]]
	join_by      <- exp_info[[7]]
	cur_date     <- exp_info[[8]]

	#// define empty list for log file
	save_log <- list()
	
	#// ===============================================================================================================
	#// read in meta and raw data files
	
	#// read meta data from file and perform some QC checks
	meta <- read_meta_data(meta_in_file, in_dir)
	
	#// read raw data file and perform some QC checks
	raw_list <- read_raw_data(raw_in_file, in_dir)
	
	
	#// ===============================================================================================================
	#// parse meta data file
	
	#// split meta in plate and well section
	out <- split_meta(meta, Exp_ID)
		plates			<- out[[1]]
		wells			<- out[[2]]

	#// clean up and QC of plates section
	out <- parse_plates(plates)
	
	 	exp_level 		<- out[[1]]
	 	plate_level		<- out[[2]]
	 	well_level		<- out[[3]]
	 
	#// parse wells section
	well_level <- parse_wells(wells, well_level)
	
	#// merge experiment, plate, and well meta data
	#meta <- assemble_meta(plates, wells, exp_level, plate_level, well_level)
	meta <- assemble_meta(exp_level, plate_level, well_level)
	
	#// ===============================================================================================================
	#// parse raw data

	#// Check if raw_in_file is available
	#// if empty, assembled meta is final output
	if (raw_in_file[[1]] == "") { 
		complete		<- meta 
		raw_merge_FLAG <- NA
	
	#// execute raw data parsing if need be
	} else {		
	
	#// parse raw data from raw_list (new and shortened version)
	raw_2 <- parse_raw_2(raw_list, join_by)
	
		#// remove this section after some more testing
		#// parse raw data from raw_list (new and shortened version)
		raw <- parse_raw(raw_list, join_by)
		#// check on raw merger
		if (identical(raw, raw_2) ) {
			raw_merge_FLAG <- TRUE
		} else {
			raw_merge_FLAG <- FALSE
		}	
	
	#// merge meta and raw data
	complete <- merge_meta_raw(meta, raw, join_by)

	}			#// end of skip for empty assembly

	
	#// ===============================================================================================================

	#// check if out_dir exists; if not make it
	if (!dir.exists(out_dir)) { dir.create(out_dir) }
	
	#// write fully assembled data
	if (file.exists(paste0(out_dir, out_file, ".tsv"))) { 
	     save_log[[length(save_log) + 1]] <- list(name = paste0(out_dir, out_file, ".tsv"), saved = FALSE)
	     } else {
		write_tsv(complete, paste0(out_dir, out_file, ".tsv"))
	     save_log[[length(save_log) + 1]] <- list(name = paste0(out_dir, out_file, ".tsv"), saved = TRUE)
	}	

	
	#// ===============================================================================================================
	#// generate QC data
	
	#// generate metadata QC tables
	qc_tables <- generate_qc_tables(complete, meta)
	
		#// display all tables
	     for (p in qc_tables) { print(p) }

	#// generate metadata QC plots
	qc_plots <- generate_qc_plots(meta, Exp_ID, cur_date)
	
	#// save QC plots
	if (file.exists(paste0(out_dir, out_file, "_qc_plots", ".pdf"))) { 
	     save_log[[length(save_log) + 1]] <- list(name = paste0(out_dir, out_file, "_qc_plots", ".pdf"), saved = FALSE)
	} else {
		save_log[[length(save_log) + 1]] <- list(name = paste0(out_dir, out_file, "_qc_plots", ".pdf"), saved = TRUE)
		#// open port to pdf
		     pdf(file = paste0(out_dir, out_file, "_qc_plots", ".pdf"),   #// The directory you want to save the file in
		     width = 16, #// The width of the plot in inches
		     height = 12) #// The height of the plot in inches
		
		     #// display all plots
		     for (p in qc_plots) { print(p) }
		     
		     # close port to pdf
		     dev.off()
		     invisible(NULL)
	}

		     
	#// ===============================================================================================================
	#// return results from assembly function for QC testing
	
	#return(list(c(TRUE, raw_merge)))
	return(list(complete = TRUE, raw_merge = raw_merge_FLAG, save_log = save_log))
}
     


## ----execute merger, eval=FALSE, warning=TRUE, include=FALSE---------------------------------------------------------------
## 
## #// call core assembly process
## QC <- data_assembly(exp_info)
## 
## #// global QC output
## final_qc(QC)
## 
## 
## #// current test data
## 
## exp_info <- list(
## 	Exp_ID       = c("20201130_AL"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20201130_AL_metadata.csv"),
## 	raw_in_file  = c("20201130_AL_IncuCyte_parsed_Plate2.tsv",
## 				  "20201130_AL_IncuCyte_parsed_Plate1.tsv",
## 				  "20201130_AL_PlateReader_parsed_Plate1.tsv",
## 				  "20201130_AL_PlateReader_parsed_Plate2.tsv"),
## 	out_file     = c("20201130_AL_data"),
## 	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference")  )
## 
## 


## ----eval=FALSE, include=FALSE---------------------------------------------------------------------------------------------
## 
## #// source data_assembly functions
## knitr::purl("data_assembly_v05.Rmd", output = "./scripts/assembly_functions.R")
## 


## ----backward compatibility testing, eval=FALSE, include=FALSE-------------------------------------------------------------
## 
## 
## experiment_list <- list()
## 
## #// 20210126_MN, synthetic example for tolerance to annotations
## 
## exp_info <-  list(
## 	Exp_ID       = c("20210126_MN"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20210126_MN_annotation_metadata.csv"),
## 	raw_in_file  = c(""),
## 	out_file     = c("20210126_MN_data"),
## 	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference")  )
## 
## #// 20210125_BG, Fireplex single plate
## 
## exp_info <- list(
## 	Exp_ID       = c("20210125_BG"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20210125_BG_metadata.csv"),
## 	raw_in_file  = c("20210125_BG_fireplex_results.xlsx"),
## 	out_file     = c("20210125_BG_data"),
## 	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference")  )
## 
## #// 20210101_MN, synthetic example for metadata w/o well-based data and w/o raw data
## 
## experiment_list[[length(experiment_list) + 1]] <- list(
## 	Exp_ID       = c("20210101_MN"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20210101_MN_no_wells_metadata.csv"),
## 	raw_in_file  = c(""),
## 	out_file     = c("20210101_MN_data"),
## 	join_by      = c("Scan_ID" = "Scan_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference") )
## 
## #// 20200908_AL, MARylation stressor screen
## 
## experiment_list[[length(experiment_list) + 1]] <- list(
## 	Exp_ID       = c("20200908_AL"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20200908_AL_metadata.csv"),
## 	raw_in_file  = c("20200908_2nd_stressor_assembled.tsv"),
## 	out_file     = c("20200908_AL_data"),
## 	join_by      = c("Scan_ID" = "Scan_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference") )
## 
## #// 20201130_AL, IncuCyte timecourse
## 
## experiment_list[[length(experiment_list) + 1]] <- list(
## 	Exp_ID       = c("20201130_AL"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20201130_AL_metadata.csv"),
## 	raw_in_file  = c("20201130_AL_IncuCyte_parsed_Plate2.tsv",
## 				  "20201130_AL_IncuCyte_parsed_Plate1.tsv",
## 				  "20201130_AL_PlateReader_parsed_Plate1.tsv",
## 				  "20201130_AL_PlateReader_parsed_Plate2.tsv"),
## 	out_file     = c("20201130_AL_data"),
## 	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference")  )
## 
## #// 20201215_JM, assembled and plotted (needed to adjust meta data input) 			--> [multiplate raw input]
## 
## experiment_list[[length(experiment_list) + 1]] <- list(
## 	Exp_ID       = c("20201215_JM"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20201215_JM_meta_data.csv"),
## 	raw_in_file  = c("20201215_JM_01_raw.csv",
## 				  "20201215_JM_02_raw.csv"),
## 	out_file     = c("20201215_JM_data"),
## 	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference")  )
## 
## #// 20201218_JM, assembled and plotted
## 
## experiment_list[[length(experiment_list) + 1]] <- list(
## 	Exp_ID       = c("20201218_JM"),
## 	in_dir       = c("./data/"),
## 	out_dir      = c("./data/"),
## 	meta_in_file = c("20201218 1007 4T1 CT26_meta.xlsx"),
## 	raw_in_file  = c("20201218_JM_raw_assembled.tsv"),
## 	out_file     = c("20201218_JM_data"),
## 	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
## 	date         = c("reference")  )
## 
## 

