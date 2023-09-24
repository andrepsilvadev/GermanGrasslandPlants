#print("hello Andre Silva")
import Pkg; Pkg.add(url = "https://github.com/janablechschmidt/MetaRange.jl.git")
Base.find_package("MetaRange")

using MetaRange
pathname = raw"/Users/andrepdasilva/Documents/Projects/MetaRange_Jana/run_mytest/input"
SD = read_input(pathname)
run_simulation!(SD)


