#// load all important libraries for data_assembly
set.seed(0815)

library(groundhog)
groundhog_day <- "2021-01-01"

groundhog.library('tidyverse','2021-01-01',ignore.deps = "xfun")
groundhog.library("DT", groundhog_day)
groundhog.library("readxl", groundhog_day)

#// source data_assembly functions
knitr::purl("data_assembly_v05.Rmd", output = "./assembly_QC/data_assembly_v05.R")
source("./assembly_QC/data_assembly_v05.R")

#// define variables
version <- "0.5.0"
today <- as.character(Sys.Date())
FLAG <- FALSE
experiment_list <- list()

#// read filenames in data dir
if (is_empty(dir("./assembly_QC/out_data/")) == FALSE) { FLAG <- TRUE }


#// 20210126_MN, synthetic test
#// Pushing limits of annotation
#// no raw data

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20210126_MN"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20210126_MN_annotation_metadata.csv"),
	raw_in_file  = c(""),
	out_file     = c("20210126_MN_data"),
	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
	date         = c("reference")  )

#// 20210125_BG, test with fireplex data
#// first assembly of meta by BG

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20210125_BG"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20210125_BG_metadata.csv"),
	raw_in_file  = c("20210125_BG_fireplex_results.xlsx"),
	out_file     = c("20210125_BG_data"),
	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
	date         = c("reference")  )

#// 20210101_MN, synthetic test
#// no entry for unique well-level information
#// no raw data +
#// no well_level data which means no QC plot

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20210101_MN"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20210101_MN_no_wells_metadata.csv"),
	raw_in_file  = c(""),
	out_file     = c("20210101_MN_data"),
	join_by      = c("Scan_ID" = "Scan_ID", "Well_ID" = "Well_ID"),
	date         = c("reference") )

#// 20200908_AL, MARylation stressor screen

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20200908_AL"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20200908_AL_metadata.csv"),
	raw_in_file  = c("20200908_2nd_stressor_assembled.tsv"),
	out_file     = c("20200908_AL_data"),
	join_by      = c("Scan_ID" = "Scan_ID", "Well_ID" = "Well_ID"),
	cur_date     = c("reference") )

#// 20201130_AL, long IncuCyte timecourse (multi-plate raw input)

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20201130_AL"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20201130_AL_metadata.csv"),
	raw_in_file  = c("20201130_AL_IncuCyte_parsed_Plate2.tsv",
				  "20201130_AL_IncuCyte_parsed_Plate1.tsv",
				  "20201130_AL_PlateReader_parsed_Plate1.tsv",
				  "20201130_AL_PlateReader_parsed_Plate2.tsv"),
	out_file     = c("20201130_AL_data"),
	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
	cur_date     = c("reference") )

#// 20201215_JM, CRISPR pSTAT1 by IF (multi-plate raw input)

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20201215_JM"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20201215_JM_meta_data.csv"),
	raw_in_file  = c("20201215_JM_01_raw.csv",
				  "20201215_JM_02_raw.csv"),
	out_file     = c("20201215_JM_data"),
	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
	cur_date     = c("reference")  )

#// 20201218_JM, CRISPR CTG with additional perturbation

experiment_list[[length(experiment_list) + 1]] <- list(
	Exp_ID       = c("20201218_JM"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20201218 1007 4T1 CT26_meta.xlsx"),
	raw_in_file  = c("20201218_JM_raw_assembled.tsv"),
	out_file     = c("20201218_JM_data"),
	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
	cur_date     = c("reference")  )

#// check complete against reference
ref_comparison <- function(exp_info) {
	
	#// extract out_dir and out_file
	out_dir	   <- exp_info[[3]]
	out_file     <- exp_info[[6]]
	
	#// load in completed data file with current script
	suppressMessages(complete <- read_tsv(paste0(out_dir, out_file, ".tsv")))
	#// round numeric values to suppress rounding errors
	complete <- complete %>% mutate(across(where(is.numeric), ~signif(. , 5)))
	
	#// load in reference file
	suppressMessages(reference <- read_tsv(paste0("./assembly_QC/ref_data/", "REF_", out_file, ".tsv")))
	#// round numeric values to suppress rounding errors
	reference <- reference %>% mutate(across(where(is.numeric), ~signif(. , 5)))
	
	#// use identical vs. all_equal to figure out differences
	# print(paste("identical: ", identical(complete, reference)))
	# print(paste("all_equal:", dplyr::all_equal(complete, reference)))
	
	return(identical(complete, reference))
	
}


#// assemble experiment IDs with experiment list
experiment_IDs 		<- sapply(experiment_list, `[[`, 1)

#// call core assembly process with experiment list
merger_results 		<- sapply(experiment_list, data_assembly)

#// compare results against reference file
ref_comp_results		<- sapply(experiment_list, ref_comparison) 

#// remove row 3 from merger_results
#// convert to tibble
#// so that QC can be easily read out
#// this needs to be adjusted depending on what data_assembly is returning
merger_results <- as_tibble(merger_results[1:2, ], .name_repair = "universal")

#// parse merger results for overall merger and QC crosscheck on modules
assembly_results		<- sapply(merger_results, '[[', 1)
raw_merger_QC_results	<- sapply(merger_results, '[[', 2)

#// assembly QC file
qc_results <- tibble(Date = today,
				 Version = version, 
				 Experiment_ID = experiment_IDs, 
				 assembly = assembly_results, 
				 Data = ref_comp_results, 
				 Raw_merger_QC = raw_merger_QC_results)


#// warning if output directory wasn't empty
if (FLAG) { 
	print("QC not completed. Empty QC output directory and run again.") 
	} else {
	#// save qc_results with current date
	write_tsv(qc_results, paste0("./assembly_QC/qc_results/", today, "_qc_results.tsv"))
	print("QC completed and results saved.") 
	print(qc_results)
	}






#// ####################################################################################################################
#//
#// manual testing

exp_info <- list(
	Exp_ID       = c("20210125_BG"),
	in_dir       = c("./assembly_QC/in_data/"),
	out_dir      = c("./assembly_QC/out_data/"),
	meta_in_file = c("20210125_BG_metadata.csv"),
	raw_in_file  = c("20210125_BG_fireplex_results.xlsx"),
	out_file     = c("20210125_BG_data"),
	join_by      = c("Plate_ID" = "Plate_ID", "Well_ID" = "Well_ID"),
	date         = c("reference")  )

	#// extract out_dir and out_file
	out_dir	   <- exp_info[[3]]
	out_file     <- exp_info[[6]]

#// load in completed data file with current script
suppressMessages(complete <- read_tsv(paste0(out_dir, out_file, ".tsv")))
#// load in reference file
suppressMessages(reference <- read_tsv(paste0("./assembly_QC/ref_data/", "REF_", out_file, ".tsv")))

print(paste("identical: ", identical(complete, reference)))
print(paste("all_equal:", dplyr::all_equal(complete, reference)))

for (i in 1:length(colnames(complete))) {
	
	print(paste0(colnames(complete)[i], " is identical: ", identical(complete[[i]], reference[[i]]))) }
	

complete2 <- complete %>% mutate(across(where(is.numeric), ~signif(. , 3)))
reference2 <- reference %>% mutate(across(where(is.numeric), ~signif(. , 3)))

identical(complete, complete2)
identical(reference, reference2)
identical(complete, reference)


for (i in 1:length(colnames(complete))) {
	
	print(paste0(colnames(complete)[i], " is identical: ", identical(complete2[[i]], reference2[[i]]))) }

identical(complete2, reference2)









#// ####################################################################################################################
#//
#// Warning messages generated by 20201130_AL and 20210126_MN (2 each)





Warning messages:
	1: In self$trans$transform(x) : NaNs produced
2: Transformation introduced infinite values in discrete y-axis 
3: In self$trans$transform(x) : NaNs produced
4: Transformation introduced infinite values in discrete y-axis 
5: In self$trans$transform(x) : NaNs produced
6: Transformation introduced infinite values in discrete y-axis 
7: In self$trans$transform(x) : NaNs produced
8: Transformation introduced infinite values in discrete y-axis 

