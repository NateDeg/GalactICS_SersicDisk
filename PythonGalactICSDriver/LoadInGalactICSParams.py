import sys as sys
import os as os



def LoadGalactICSInputs(GalactICSDicts):
    print("Loading in GalactICS inputs")
    #   Start by getting the parameter file name
    ParamFile=GetParamsFileName()
    #   Now read in the parameter file
    GalactICSInputsMod=ReadGalactICSParamFile(ParamFile)
    #   All parameters in the file should now be read and stored in the GalactICSInputsMod.  We need to parse this
    GalactICSDicts=ParseGalactICSMod(GalactICSInputsMod,GalactICSDicts)
    #   Check that the dictionariaries are filled in correctly
    CheckGalactICSDicts(GalactICSDicts)
    #   Now that we've confirmed that all the dictionaries are filled in correctly, we will return and start writing up the GalactICS input files
    return GalactICSDicts


def GetParamsFileName():
    #   Grab the command line arguments
    Commands=sys.argv
    #   Make sure the user has supplied a python parameter file
    if len(Commands)==1:
        print("You must supply a python parameter file containing all the GalactICS input parameters")
        exit()
        
    #   Check that the python parameter file exists
    FileCheck=os.path.isfile(Commands[1])
    if FileCheck == False:
        print("The supplied parameter file does not exist")
        print(Commands[1])
        exit()
    ParamFile=Commands[1]
    return ParamFile


def ReadGalactICSParamFile(FileName):
    print("About to import parameter file ", FileName)
    print("Absolute path to parameter file:",os.path.abspath(FileName))
    #   Convert the path to an absolute path
    AbsPath=os.path.abspath(FileName)
    #   Convert the path to an absolute path
    AbsPath=os.path.abspath(FileName)
    #   Figure out the path and the module names by splitting the absolute path
    Package=AbsPath.rsplit("/",1)[0]
    ModName=AbsPath.rsplit("/",1)[1]
    #   Remove the .py from the ModName
    ModName=ModName.split(".")[0]
    #   Now import Sys
    import sys
    #   And add the path to the parameter file to sys.path
    sys.path.append(Package)
    #   Finally use the importlib module to import the module
    import importlib as il
    ModTest=il.import_module(ModName,package=Package)

    return ModTest

def ParseGalactICSMod(GInMod,GalactICSDicts):
    #   Start by checking on the required keys
    Vars=vars(GInMod)
    
    for key in GalactICSDicts['RequiredDicts']:
        #   Check whether the key is in the input dictionary
        if key in Vars:
            #   Then check that the key is a dictionary
            if type(Vars[key])==dict:
                #   Now add each key in the input dictionary to the GalactICSDicts
                for InKey in Vars[key]:
                    GalactICSDicts[key][InKey]=Vars[key][InKey]
            #   If it's not stored as a dictionary, quit
            else:
                print('Required dictionary ',key,'is not stored as a dictionary in input file')
                print("Stopping run here")
                exit()
        #   If there's a missing dictionary, quit
        else:
            print("Input file is missing required dictionary parameters: ", key)
            print("Stopping run here")
            exit()
    return GalactICSDicts

def CheckGalactICSDicts(GalactICSDicts):
    #   Start by checking the running dictionary
    print("Checking Running Dictionary")
    CheckGenericDictionary(GalactICSDicts['RunningDictionary'])
    #   Then the Halo
    print("Checking Halo dictionary")
    CheckGenericDictionary(GalactICSDicts['Halo'])
    #   Then the first stellar disk
    print("Checking Disk1 dictionary")
    CheckGenericDictionary(GalactICSDicts['Disk1'])
    #   Then the second stellar disk
    print("Checking Disk2 dictionary")
    CheckGenericDictionary(GalactICSDicts['Disk2'])
    #   Then the gas  disk
    print("Checking Gas Disk dictionary")
    CheckGenericDictionary(GalactICSDicts['GasDisk'])
    #   Then the bulge
    print("Checking Bulge dictionary")
    CheckGenericDictionary(GalactICSDicts['Bulge'])
    #   Then the Poisson Grid
    print("Checking Poisson dictionary")
    CheckGenericDictionary(GalactICSDicts['PoissonDict'])
    
    

def CheckGenericDictionary(GenericDict):
    for i in range(len(GenericDict['RequiredKeywords'])):
        CurrKey=GenericDict['RequiredKeywords'][i]
        TargType=GenericDict['RequiredVariableType'][i]
        
        GenericDictVariableCheck(GenericDict,CurrKey,TargType)
        
        
    
    if GenericDict[CurrKey]=='y':
        for i in range(len(GenericDict['SecondaryKeywords'])):
            CurrKey=GenericDict['SecondaryKeywords'][i]
            TargType=GenericDict['SecondaryVariableTypes'][i]
            
            GenericDictVariableCheck(GenericDict,CurrKey,TargType)
        

def GenericDictVariableCheck(GenericDict,CurrKey,TargType):

    if CurrKey in GenericDict:
        #   Then check that it's the right type
        #       If it's not, quit
        if type(GenericDict[CurrKey]) != TargType:
            print(CurrKey, ' must be of the type', TargType)
            print("The stored value is",GenericDict[CurrKey],"which has the type",type(GenericDict[CurrKey]))
            print("Stopping run here")
            exit()
    #   If not, quit
    else:
        print("Current dictionary is missing the required keyword", CurrKey)
        print("Stopping run here")
        exit()
