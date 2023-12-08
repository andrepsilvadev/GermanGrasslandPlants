#print("hello Andre Silva")
import Pkg; Pkg.add(url = "https://github.com/janablechschmidt/MetaRange.jl.git")
#Base.find_package("MetaRange")

# run metarange from specified folder
using MetaRange
pathname = raw"/Users/andrepdasilva/Documents/Projects/MetaRange_Jana/run_mytest/input/configuration.csv" # folder copied from the "examples" folder in tutorial run
SD = read_input(pathname)
Simulation_Variables(SD)
run_simulation!(SD)

#stoped here. are all the visualization output functions those that are mentioned in
#https://janablechschmidt.github.io/MetaRange.jl/dev/functions/#Visualization-Functions  

plot_abundances(SD::Simulation_Data)
abundance_gif(SD)
suitability_gif(SD)

#where is the output?
#How to read the output? 
#what are the ouput metrics?
#what is the abundances.gif file? input?
# how to change the input parameters?
# make a run with different settings - double check if the output changed
# weekly meeting or weekly update with robin or juliano? need to have some kind of pressure
