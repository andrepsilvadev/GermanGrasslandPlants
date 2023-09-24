#print("hello Andre Silva")
import Pkg; Pkg.add(url = "https://github.com/janablechschmidt/MetaRange.jl.git")
#Base.find_package("MetaRange")

# run metarange from specified folder
using MetaRange
pathname = raw"/Users/andrepdasilva/Documents/Projects/MetaRange_Jana/run_mytest/input" # folder copied from the "examples" folder in tutorial run
SD = read_input(pathname)
run_simulation!(SD)

#next meeeting (jana_robin)
#where is the output?
#How to read the output? 
#what are the ouput metrics?
#what is the abundances.gif file? input?
