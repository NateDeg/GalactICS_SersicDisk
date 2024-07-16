import sys as sys
import os as os

#   Start by importing the default dictionaries
from . import SetDefaultDictionaries as SDD
from . import LoadInGalactICSParams as LIGP
from . import WriteInputFiles as WIF
from . import RunGalactICSCode as RGC
from . import CleanGalactICSRun as CGR


def MakeGalaxy():
    print("Making a GalactICS Galaxy")
    
    #   Initialize the dictionarys
    GalactICSDicts=SDD.SetDefaultDicts()
    
    #   Now load in the runtime dictionary
    GalactICSDicts=LIGP.LoadGalactICSInputs(GalactICSDicts)
    #   Make the target folder
    os.makedirs(GalactICSDicts['RunningDictionary']['TargFolder'],exist_ok=True)
    #Now it's time to prep various input files that will be necessary for GalactICS
    GalactICSDicts=WIF.WriteInputFiles(GalactICSDicts)
    #   And now to run GalactICS and build all the individual files!!!
    RGC.RunGalactICS(GalactICSDicts)
    #   Once everything is run, clean it all up by moving all files into the target folder
    CGR.CleanGalactICSOutputs(GalactICSDicts)
    
