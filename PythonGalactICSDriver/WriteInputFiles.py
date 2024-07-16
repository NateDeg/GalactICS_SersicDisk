import sys as sys
import os as os



def WriteInputFiles(GalactICSDicts):
    #   We'll write here the input files necessary for GalactICS
    GalactICSDicts['InDBHFile']=WriteInDBH(GalactICSDicts)
    #   Write the input files needed for each component
    GalactICSDicts=WriteComponentFiles(GalactICSDicts)
    
    return GalactICSDicts
    
def WriteInDBH(GalactICSDicts):

    HKeys=['TrunctationRadius','ScaleVelocity','ScaleRadius','dTruncRadius','InnerSlope','OuterSlope']
    DKeys=['DiskMass','ScaleRadius','TruncationRadius','ScaleHeight','HeightTruncation','rhole','rcore','SersicIndx']
    GKeys=['DiskMass','ScaleRadius','TruncationRadius','KinematicTemperature','HeightTruncation','GasGamma']
    BKeys=['SersicIndex','PSwitch','ScaleVelocity','ScaleRadius']
    PKeys=['dR','nR','lmax']


    #   Set the input file name
    FName="in_dbh.txt"
    
    #   Add in the halo information
    InContents=GalactICSDicts['Halo']['IncludeHalo']+"\n"
    if GalactICSDicts['Halo']['IncludeHalo']=='y':
        InContents=GenericLineWrite(GalactICSDicts['Halo'],HKeys,InContents)
    #   And the first disk information
    InContents+=GalactICSDicts['Disk1']['IncludeDisk']+"\n"
    if GalactICSDicts['Disk1']['IncludeDisk']=='y':
        InContents=GenericLineWrite(GalactICSDicts['Disk1'],DKeys,InContents)
    #   And the second disk information
    InContents+=GalactICSDicts['Disk2']['IncludeDisk']+"\n"
    if GalactICSDicts['Disk2']['IncludeDisk']=='y':
        InContents=GenericLineWrite(GalactICSDicts['Disk2'],DKeys,InContents)
    #   Now the gas disk information
    InContents+=GalactICSDicts['GasDisk']['IncludeGasDisk']+"\n"
    if GalactICSDicts['GasDisk']['IncludeGasDisk']=='y':
        InContents=GenericLineWrite(GalactICSDicts['GasDisk'],GKeys,InContents)
    #   And the bulge information
    InContents+=GalactICSDicts['Bulge']['IncludeBulge']+"\n"
    if GalactICSDicts['Bulge']['IncludeBulge']=='y':
        InContents=GenericLineWrite(GalactICSDicts['Bulge'],BKeys,InContents)
    #   Finish with the Poisson Grid information
    InContents+="n\n"
    InContents+=str(GalactICSDicts['PoissonDict']['dR'])+"\t"+str(GalactICSDicts['PoissonDict']['nR'])+"\n"
    InContents+=str(GalactICSDicts['PoissonDict']['lmax'])+"\n"
    
    f=open(FName,'w')
    f.write(InContents)
    f.close()
    return FName
     

def GenericLineWrite(InDict,KeyList,InputStr):

    for key in KeyList:
        InputStr+=str(InDict[key])+"\t"
    InputStr+="\n"
    return InputStr

def WriteComponentFiles(GalactICSDicts):
    #   Go through each component and write the input files needed to build them
    if GalactICSDicts['Halo']['IncludeHalo']=='y':
        GalactICSDicts['Halo']['HInFile']=WriteInHalo(GalactICSDicts)
        
    if GalactICSDicts['Disk1']['IncludeDisk']=='y':
        GalactICSDicts['Disk1']['DInFile'],GalactICSDicts['Disk1']['D_DFInFile']=WriteInDisk(GalactICSDicts['RunningDictionary']['TargFolder'],GalactICSDicts['Disk1'],1)

    if GalactICSDicts['Disk2']['IncludeDisk']=='y':
        GalactICSDicts['Disk2']['DInFile'],GalactICSDicts['Disk2']['D_DFInFile']=WriteInDisk(GalactICSDicts['RunningDictionary']['TargFolder'],GalactICSDicts['Disk2'],2)
        
    if GalactICSDicts['Bulge']['IncludeBulge']=='y':
        GalactICSDicts['Bulge']['BInFile']=WriteInBulge(GalactICSDicts)

    if GalactICSDicts['GasDisk']['IncludeGasDisk']=='y':
        GalactICSDicts['GasDisk']['GInFile']=WriteInGas(GalactICSDicts)

    #   And the psi grid
    FName="in_gendenspsi.txt"
    PStr=str(GalactICSDicts['PoissonDict']['npsi'])+"\t"+str(GalactICSDicts['PoissonDict']['nint'])+"\n"
    f=open(FName,'w')
    f.write(PStr)
    f.close()
    GalactICSDicts['PoissonDict']['DensPsiInFile']=FName

    return GalactICSDicts

def WriteInHalo(GalactICSDicts):

    FName="in_halo.txt"
    

    Hcontents=str(GalactICSDicts['Halo']['SpinIsotropy'])+"\n"
    Hcontents+=str(GalactICSDicts['Halo']['ParticleNumber'])+"\n"
    Hcontents+=str(GalactICSDicts['Halo']['RandomSeed'])+"\n"
    Hcontents+=str(GalactICSDicts['Halo']['RecenterSwitch'])+"\n"
    f=open(FName,'w')
    f.write(Hcontents)
    f.close()
    
    return FName


def WriteInDisk(TargFolder,DiskDict,DSwitch):

    #   Start with the disk particle input
    FName="in_disk"+str(DSwitch)+".txt"
    

    Dcontents=str(DSwitch)+"\n"
    Dcontents+=str(DiskDict['ParticleNumber'])+"\n"
    Dcontents+=str(DiskDict['RandomSeed'])+"\n"
    Dcontents+=str(DiskDict['RecenterSwitch'])+"\n"
    f=open(FName,'w')
    f.write(Dcontents)
    f.close()
    
    #   And do the disk dF file
    FName2="in_diskdf"+str(DSwitch)+".txt"
    Dcontents=str(DSwitch)+"\n"
    Dcontents+=str(DiskDict['ScaleRadialVelDisp1'])+"\t"
    Dcontents+=str(DiskDict['VelDispScaleRadius1'])+"\t"
    Dcontents+=str(DiskDict['ScaleRadialVelDisp2'])+"\t"
    Dcontents+=str(DiskDict['VelDispScaleRadius2'])+"\n"
    Dcontents+=str(DiskDict['numSpline'])+"\n"
    Dcontents+=str(DiskDict['numIterations'])+"\n"
    
    f=open(FName2,'w')
    f.write(Dcontents)
    f.close()

    return FName,FName2

def WriteInBulge(GalactICSDicts):

    FName="in_bulge.txt"
    

    Hcontents=str(GalactICSDicts['Bulge']['SpinIsotropy'])+"\n"
    Hcontents+=str(GalactICSDicts['Bulge']['ParticleNumber'])+"\n"
    Hcontents+=str(GalactICSDicts['Bulge']['RandomSeed'])+"\n"
    Hcontents+=str(GalactICSDicts['Bulge']['RecenterSwitch'])+"\n"
    f=open(FName,'w')
    f.write(Hcontents)
    f.close()
    
    return FName
    
    
def WriteInGas(GalactICSDicts):

    FName="in_gas.txt"
    

    Gcontents=str(GalactICSDicts['GasDisk']['ParticleNumber'])+"\n"
    Gcontents+=str(GalactICSDicts['GasDisk']['RandomSeed'])+"\n"
    Gcontents+=str(GalactICSDicts['GasDisk']['RecenterSwitch'])+"\n"
    Gcontents+="dbh.dat\n"
    Gcontents+=str(GalactICSDicts['GasDisk']['nSplinePoints'])+"\n"
    Gcontents+=str(GalactICSDicts['GasDisk']['EqnOfState'])+"\n"
    f=open(FName,'w')
    f.write(Gcontents)
    f.close()
    
    return FName


