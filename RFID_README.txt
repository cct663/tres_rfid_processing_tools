Processing RFID files to extract feeding rate and visitation patterns.

Written by Conor Taff
Last updated 9/6/2019

This is a series of scripts wrapped in an RStudio project that will process a batch of
raw RFID files saved as csvs. The scripts are meant to be run in order using the 
following sequence. Additional information can be found in the annotation within each script.

1. Place all rfid files to be processed in the '0_input_data' folder. See bottom of script
	for details on naming convention and common file problems.

2. Create reference files. Formatting conventions and names are critical for these files 
	to work, so copy the examples very carefully. These files must be placed in the 
	'2_reference_files' directory and saved as tab delimited text. Two files are needed: 
	
	a) 'FullRFIDReference.txt' - A full list of all RFID tags ever deployed in the Ithaca
		population. Use the previous year's file and just add any NEWLY deployed tags to 
		the list. 
	b) 'RFIDRef.txt' - Joins nest identities from file names to band, rfid, and important
		dates for each nest in the year being scored. Also used to determine breeding 
		stage for focal and visitor at each RFID read detected.
		
3. Ensure that the other folders in the directory are set up.
	
	- '1_modified_data' should be empty
	- '3_r_code' should contain the r scripts
	- '4_figure_output' and '5_file_output' should be empty

4. Open the 'RFID_Process' RStudio project. It should load up with several scripts numbered
	in the order that they need to be run in. Each script should be executed in full on
	the batch of files you are processing. A brief description of the action of each
	script follows:
	
	- '1_set_up_ref_files.R'
		This script loads a bunch of libraries (if you do not have them already you will
		need to install). Next it reads in your reference files and does some manipulations
		to get them ready for subsequent steps.
		
	- '2_clean_files.R'
		This reads in each file in your input directory in turn and performs some clean up
		functions. That includes stripping out relevant info from the file name and also 
		doing error checking for erroneous RFID reads (i.e., when reader mistakes one letter
		for another). The cleaned up file is written as a new version to the modified
		directory to be used in subsequent steps.
		
	- '3_merge_files.R'
		This combines all of the cleaned up files into a single large file that has all
		RFID reads from all boxes for the entire season. Reads are matched to their identity
		and timing of breeding stage is added to all. Reads that cannot be matched or that
		are duplicated (e.g., downloaded twice) are removed. The output is an 
		'AllVisitsMerged' tab delimited text file. This file is now a master file for the
		entire season and can be used for all kinds of things later on. Note that the file
		is often too big to open in Excel, so you will need to work with it in R.
		
	- '4_feeding_rate.r'
		This script calcules daily and hourly feeding rate using the merged file created
		above. In addition to recording the total number of raw rfid reads in each interval,
		the script also employs the thresholds used in Vitousek et al. PRSB to try to
		estimate unique feeding trips. That means that a string of reads close together
		are clumped into a single feeding trip. See the script for details. Two files are
		written to the output directory with hourly and daily feeding by males and females
		at each nest. These can be taken out to merge with other analyses.
		
	- '5_extract_visitors.r'
		This looks at the RFID reads made by non-box owners. Reads are grouped into unique
		visits based on adjustable time thresholds. An object is created with all unique
		visits in the entire season and is matched to timing of breeding stage for both
		the focal nest and the visitor. Additional objects are made to summarize total and 
		unique daily visits on a nest by nest level.
		
		
WARNINGS AND NOTES.
This section describes some common problems with RFID files that crop up every year and
how to deal with them. 

A few important notes about file naming and organization:

- Files should be named unit.nest.mm.dd.yyyy.csv and placed in the '0_input_data' directory.
- Files that have unexpected data will cause the code to crash and need to be removed
	or modified. The most common reasons files will cause problems are:
		- Weird characters inserted by faulty RFID (Greek/Chinese, etc). These can sometimes
			be deleted to save the file, but sometimes a whole file is corrupted.
		- Missing start tag. The code looks for 'xxxxDATA' to start reading. If this line
			was not copied it will fail. This can be inserted manually to fix.
		- No data. If there are no reads (or just 1?) the code may fail.
- Sometimes files are messed up with clocks that failed to keep time. The code may work
	on those, but the data are unusable and should be deleted.

After running, you should manually check to ensure that you have the data that you expect
	from each nest. A few common problems can cause entire nests to be missing or 
	incorrect in the final output. These include:
	
- Errors in matching names. The code links file names to records in the reference files.
	If there are mistakes (like '-' instead of '_', wrong band numbers, etc, etc) then
	the files will not merge and records will not be scored. There are many ways that 
	errors can be introduced so it is important to check carefully.
- Errors in file saving. One common error that we have encountered is that an RFID file
	named for one box actually includes extra records or ONLY records from another box.
	This can happen when someone is downloading multiple boards in a row and misses 
	hitting ctrl-c on one board, so that when they hit ctrl-v, the content pasted is 
	actually from the previous box. This happens every year! These are tricky to catch and
	likely require going back to the original files to compare the timestamps and board
	identities. In most cases one file will need to be deleted resulting in missing data.