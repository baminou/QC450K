__author__ = 'briceaminou'

### This script creates a quality control report of an Illumina 450K array's experiment.
### To run this script:
###
### python QC450K.py -i {input_folder}
###
### {input_folder}: Contains all the idat files
### ----------------------
### More info
###
### This script is going to read only the idat files in the input folder, so nothing else.
### Once the idat files are read, 9 quality control tests are done.
### 1. Bisulfite conversion I
### 2. Bisulfite conversion II
### 3. Extension
### 4. Hybridization
### 5. Non-polymorphic
### 6. Specificity I
### 7. Specificity II
### 8. Staining
### 9. Target Removal
###
### The script is splitted in two portions. The first portion makes the test for every controls
### and generate a matrix saying if samples failed or succeeded the test.
### The second portion generates a pdf from the previous output.
###
### Finally, the output plots and a pdf report are exported to the input folder in a folder named QC.
### ----------------------------
### This python scripts uses rpy2 package to call R.


from os import listdir
from os.path import isfile, join
from reportlab.lib import colors
from reportlab.lib.pagesizes import letter
from reportlab.platypus import SimpleDocTemplate, Table, TableStyle, Image, Paragraph, PageBreak
from reportlab.lib.styles import getSampleStyleSheet
from rpy2 import robjects
from rpy2.robjects.packages import importr
import os
import sys, getopt



### Check for the input directory.
### The input directory has to contain the idat files for the program to work
myopts, args = getopt.getopt(sys.argv[1:],"i:o:")
cpt = 0
for o, a in myopts:
    if o == '-i':
        cpt+=1
        os.chdir(a)

### If the input directory is missing, the program exits
if cpt == 0:
    print("You must enter an argument: -i folder_with_idat")
    exit(1)



### Shinymethyl is the main package used to work with the control probes
### on the 450k array 
shinyMethyl = importr("shinyMethyl")

### Minifi is used for idat reading
minfiData = importr("minfiData")

### This library contains the functions that do the statistical steps
### with every control probes.
QC450K = importr("QC450k")

utils = importr("utils")


base_dir = "./"
qc_directory = base_dir + "QC/"

robjects.globalenv['baseDir'] = base_dir

### Reading of the input folder containing the idat files
robjects.reval("RGset <- read.450k.exp(base = baseDir)")

### Summarization of the data read in Read and Green intensity
robjects.reval("summarized.data <- shinySummarize(RGset)")
robjects.globalenv['qc_path'] = qc_directory

### Create the QC directory if it does not exist
if not os.path.exists(qc_directory):
    os.makedirs(qc_directory)



### This step creates all the quality control plots and put everything in the qc output folder.
### It finally creates a file called report.csv whith a matrix of TRUE and FALSE to say if the
### sample passes the quality control test.
robjects.reval('''
qc <- t(data.frame(
  staining=staining(summarized.data,plot = T,output_file = paste(qc_path,"staining.png",sep="")),
  extension=extension(summarized.data,plot = T,output_file = paste(qc_path,"extension.png",sep="")),
  hybridization=hybridization(summarized.data,plot = T,output_file = paste(qc_path,"hybridization.png",sep="")),
  targetRemoval=targetRemoval(summarized.data,plot = T,output_file = paste(qc_path,"targetRemoval.png",sep="")),
  bisulfiteConversionI=bisulfiteConversionI(summarized.data,plot = T,output_file = paste(qc_path,"bisulfiteConversionI.png",sep="")),
  bisulfiteConversionII=bisulfiteConversionII(summarized.data,plot = T,output_file = paste(qc_path,"bisulfiteConversionII.png",sep="")),
  specificityI=specificityI(summarized.data,plot = T,output_file = paste(qc_path,"specificityI.png",sep="")),
  specificityII=specificityII(summarized.data,plot = T,output_file = paste(qc_path,"specificityII.png",sep="")),
  nonPolymorphic=nonPolymorphic(summarized.data,plot = T,output_file = paste(qc_path,"nonPolymorphic.png",sep="")),
  row.names=summarized.data@sampleNames
))
write.csv(t(qc),paste(qc_path,"report.csv",sep=""))
''')




### The next step consists in generating a pdf which shows only the tests that failed.


### Get the quality control output directort
root_directory = qc_directory

### Get the type of pictures output in the quality control directory
picture_format = ".png"

### Get the csv report 
csv_report = root_directory + "report.csv"

### Create a pdf document
doc = SimpleDocTemplate(qc_directory+"output.pdf",pagesize=letter)

elements = []


### List all the plots generated
pictures_path = [f for f in listdir(root_directory) if isfile(join(root_directory,f)) and f.endswith(".png")]






### After running all the R scripts, a file called report.csv is generated in the {input_file}/QC folder.
### This report contains a matrix where every row corresponds to a sample and every column corresponds to
### the output of the quality control test done. The output of the test is either TRUE or FALSE to say if the
### sample passed the test.
### ---------------------------
### Here is an example of the output:
### ---------------------------
### 		staining 	extension 	hybridization ...
### sample1	TRUE		TRUE		FALSE
### sample2	TRUE		TRUE		FALSE
### sample3	FALSE		TRUE		TRUE
### ...
### ---------------------------

### The next part of the script consists in generating a pdf file containing the plots of the tests
### that failed for at least one sample.


### Variable initialization
### -----------------------

### This array will receive all of the tests name
control_names = []

### This array will receive all of the sample names
sample_names = []

### This matrix will contains all the TRUE and FALSE from the report.csv file
matrix = []


### Read the csv report
with open(csv_report,'r') as infile:
    ### Record all the quality control tests
    control_names = infile.readline().split(",")[1:]
    ### For every line of the report, we add all sample names to the sample_names array.
    ### and add the result of the test.
    for line in infile:
        sample_names.append(line.split(',')[0])
        matrix.append(line.split(',')[1:])


### Create a table from the matrix
t = Table(matrix)

### This portion add a table to the pdf highlighting the failed test
for sample_idx in xrange(0,len(sample_names)):
    for control_idx in xrange(0,len(control_names)):
        if matrix[sample_idx][control_idx] == "FALSE":
            t.setStyle(TableStyle([('BACKGROUND',(control_idx,sample_idx),(control_idx,sample_idx),colors.red)]))

### Adding a line to the pdf
elements.append(Paragraph("Quality Control Failed Tests",getSampleStyleSheet()['Heading2']))

### Uncomment the following line to have the highlighted table
#elements.append(t)

### For every control test, check if the the test is false
### If the test is false, add to the pdf thew name of the test who failed
### and the name of the samples ou failed the test. Next add the image
### of the plot corresponding to the failing test and finally add a page break.
for control_idx in xrange(0,len(control_names)):
    cpt = False

    ### Check if the test fails
    for sample_idx in xrange(0,len(matrix)):
        if matrix[sample_idx][control_idx] == "FALSE":
            cpt = True
    if cpt==True:
            tmp = root_directory+control_names[control_idx].strip().strip('"')+picture_format
            elements.append(Paragraph(control_names[control_idx].strip('"'),getSampleStyleSheet()['Heading2']))
            for sample_idx in xrange(len(matrix)):
                if matrix[sample_idx][control_idx] == "FALSE":
                    elements.append(Paragraph(str(int(sample_idx+1))+" - "+sample_names[sample_idx],getSampleStyleSheet()['Normal']))
            elements.append(Image(tmp))
            elements.append(PageBreak())

### Build the PDF document
doc.build(elements)
