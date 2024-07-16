"""
This module contains a set of default dictionaries that will build a GalactICS MW model based on the McMillan 2017 model and it should match the Deg et al. 2019 GalactIC realization.
"""

import os as os

def SetDefaultDicts():
    #   Set the required dictionaries to be listed in the input folder
    RequiredDicts=['RunningDictionary','Halo','Disk1','Disk2','GasDisk','Bulge','PoissonDict']

    #   Start by defining the general running dictionary
    RunningDictionary={}
    RunningDictionary['RequiredKeywords']=['TargFolder']
    RunningDictionary['RequiredVariableType']=[str]
    
    CurrDir=os.getcwd()
    #   Get the WRKP directory
    ExecDir= os.path.dirname(os.path.realpath(__file__))
    #   Adjust the WRKP directory path slightly to point one directory up due to this file being located in WRKP/FitDriverScripts
    ExecDir=ExecDir.rsplit('/', 1)[0]+"/bin/"
    RunningDictionary['ExecDir']=ExecDir
    


    #   Set the Halo dictionary
    Halo={}
    Halo['RequiredKeywords']=['IncludeHalo']
    Halo['RequiredVariableType']=[str]
    
    Halo['SecondaryKeywords']=['ScaleRadius','ScaleVelocity','TrunctationRadius','dTruncRadius','InnerSlope','OuterSlope','ParticleNumber','SpinIsotropy','RandomSeed','RecenterSwitch']
    Halo['SecondaryVariableTypes']=[float,float,float,float,float,float,int,float,int,int]


    #   Set the thin stellar disk dictionary
    Disk1={}
    Disk1['RequiredKeywords']=['IncludeDisk']
    Disk1['RequiredVariableType']=[str]
    Disk1['SecondaryKeywords']=['DiskMass','ScaleRadius','ScaleHeight','TruncationRadius','HeightTruncation','SersicIndx','ScaleRadialVelDisp1','VelDispScaleRadius1','ScaleRadialVelDisp2','VelDispScaleRadius2','numSpline','numIterations','ParticleNumber','RandomSeed','RecenterSwitch']
    Disk1['SecondaryVariableTypes']=[float,float,float,float,float,float,float,float,float,float,int,int,int,int,int]
    Disk1['rhole']=0.
    Disk1['rcore']=0.

  

    #   Set the thick stellar disk dictionary
    Disk2={}
    Disk2['RequiredKeywords']=['IncludeDisk']
    Disk2['RequiredVariableType']=[str]
    Disk2['SecondaryKeywords']=['DiskMass','ScaleRadius','ScaleHeight','TruncationRadius','HeightTruncation','SersicIndx','ScaleRadialVelDisp1','VelDispScaleRadius1','ScaleRadialVelDisp2','VelDispScaleRadius2','numSpline','numIterations','ParticleNumber','RandomSeed','RecenterSwitch']
    Disk2['SecondaryVariableTypes']=[float,float,float,float,float,float,float,float,float,float,int,int,int,int,int]
    Disk2['rhole']=0.
    Disk2['rcore']=0.

    #   Set the gas disk dictionary
    GasDisk={}
    GasDisk['RequiredKeywords']=['IncludeGasDisk']
    GasDisk['RequiredVariableType']=[str]
    GasDisk['SecondaryKeywords']=['DiskMass','ScaleRadius','KinematicTemperature','TruncationRadius','HeightTruncation','ParticleNumber','RandomSeed','RecenterSwitch','nSplinePoints']
    GasDisk['SecondaryVariableTypes']=[float,float,float,float,float,int,int,int,int]
    #   Set the Equation of State and lock it from the main user
    GasDisk['EqnOfState']=1                 #unitless switch
    GasDisk['GasGamma']=-1.6667
    
    #   Set the bulge dictionary
    Bulge={}
    Bulge['RequiredKeywords']=['IncludeBulge']
    Bulge['RequiredVariableType']=[str]
    Bulge['SecondaryKeywords']=['SersicIndex','ScaleVelocity','ScaleVelocity','ParticleNumber','SpinIsotropy','RandomSeed','RecenterSwitch']
    Bulge['SecondaryVariableTypes']=[float,float,float,int,float,int,int]
    Bulge['PSwitch']=-1

    #   Set the Poisson Grid Dictionary
    PoissonDict={}
    PoissonDict['RequiredKeywords']=['dR','nR','lmax','npsi','nint']
    PoissonDict['RequiredVariableType']=[float,int,int,int,int]

    #   Store everything into a single large dictionary
    GalactICSDicts={'RunningDictionary':RunningDictionary,'Halo':Halo,'Disk1':Disk1,'Disk2':Disk2,'GasDisk':GasDisk,'Bulge':Bulge,'PoissonDict':PoissonDict,'RequiredDicts':RequiredDicts}

    return GalactICSDicts
