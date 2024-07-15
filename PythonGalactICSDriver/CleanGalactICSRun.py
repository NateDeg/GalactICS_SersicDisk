import sys as sys
import os as os



def CleanGalactICSOutputs(GalactICSDicts):
    print("Cleaning outputs")
    TargFolder=GalactICSDicts['RunningDictionary']['TargFolder']
    #   Start by moving all input files into the target folder
    ClnCmd="mv in_* "+TargFolder+"/."
    os.system(ClnCmd)
    #   Next move all the data files into the folder

    DBH_OutputNames=['dbh.dat','IniPotDens.out','mr.dat']
    
    Halo_OutputNames=['halo','h.dat','dfnfw.dat','denspsihalo.dat','dfsersic.dat','halodensity.out','InitialHaloDens.txt']
    
    Gas_OutputNames=['gasdisk','gasdensityestimate.out','zgasgrid.dat']
    
    Bulge_OutputNames=['bulge','b.dat','denspsibulge.dat']
    
    Disk1_OutputNames=['disk','cordbh.dat','freqdbh.dat','omekap2.dat','rcirc.disk1','toomre.dat','toomre2.5','cordbh_Int.txt','SigCheck.txt']
    
    Disk2_OutputNames=['disk2','cordbh2.dat','rcirc.disk2','stability.out']

    for f in DBH_OutputNames:
        BasicCleanCmd(f,TargFolder)

    if GalactICSDicts['Halo']['IncludeHalo']=='y':
        for f in Halo_OutputNames:
            BasicCleanCmd(f,TargFolder)
            
    if GalactICSDicts['Bulge']['IncludeBulge']=='y':
        for f in Bulge_OutputNames:
            BasicCleanCmd(f,TargFolder)
            
    if GalactICSDicts['GasDisk']['IncludeGasDisk']=='y':
        for f in Gas_OutputNames:
            BasicCleanCmd(f,TargFolder)
            
    if GalactICSDicts['Disk1']['IncludeDisk']=='y':
        for f in Disk1_OutputNames:
            BasicCleanCmd(f,TargFolder)
    if GalactICSDicts['Disk2']['IncludeDisk']=='y':
        for f in Disk2_OutputNames:
            BasicCleanCmd(f,TargFolder)
            
    #   Finally add a make file to the target folder
    WriteGalactICSMakefile(TargFolder,GalactICSDicts)
            
            
def BasicCleanCmd(fname,TargFolder):
    ClnCmd="mv "+fname+" "+TargFolder+"/."
    os.system(ClnCmd)


def WriteGalactICSMakefile(TargFolder,GalactICSDicts):
    fname=TargFolder+"/Makefile"
    
    MStr="BIN="+GalactICSDicts['RunningDictionary']['ExecDir']+"\n\n"
    
    MStr+="potential: dbh.dat\n\n"
    MStr+="disk: freqdbh.dat cordbh.dat dbh.dat in_disk2.txt\n\t$(BIN)/gendisk < in_disk2.txt\n\n"
    MStr+="disk2: freqdbh.dat cordbh2.dat dbh.dat in_disk2.txt\n\t$(BIN)/gendisk < in_disk2.txt\n\n"
    MStr+="bulge: dbh.dat in_bulge.txt\n\t$(BIN)/genbulge < in_bulge.txt\n\n"
    MStr+="gas: dbh.dat in_gas.txt\n\t$(BIN)/gengas < in_gas.txt\n\n"
    MStr+="halo: dbh.dat in_halo.txt\n\t$(BIN)/genhalo < in_halo.txt\n\n"
    MStr+="dbh.dat: in_dbh.txt\n\t$(BIN)/dbh < in_dbh.txt\n\n"
    MStr+="freqdbh.dat: dbh.dat h.dat\n\t$(BIN)/getfreqs\n\n"
    MStr+="cordbh.dat: dbh.dat freqdbh.dat in_diskdf1.txt\n\t$(BIN)/diskdf < in_diskdf1.txt\n\n"
    MStr+="cordbh2.dat: dbh.dat freqdbh.dat in_diskdf2.txt\n\t$(BIN)/diskdf < in_diskdf2.txt\n\n"
    
    MStr+="clean:\n\trm -f disk disk2 bulge halo gasdisk\n\n"
    MStr+="veryclean:\n\trm -f *.dat disk disk2 bulge halo gasdisk *.out *.disk1 *.disk2 *.png fort.* toomre2.5 cordbh_Int.txt InitialHaloDens.txt SigCheck.txt *~\n\n"
    
    f=open(fname,'w')
    f.write(MStr)
    f.close()
    
