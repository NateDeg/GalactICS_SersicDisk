import sys as sys
import os as os



def RunGalactICS(GalactICSDicts):
    print("Running GalactICS")
    #   Start by building the potential
    BuildPotential(GalactICSDicts)
    #   Now build components as needed
    BuildComponents(GalactICSDicts)

def BuildPotential(GalactICSDicts):
    #   Build the potential
    BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"/dbh < "+GalactICSDicts['InDBHFile']
    os.system(BuildCmd)
    #   Do a check that the potential output file was built
    if os.path.isfile('dbh.dat')==False:
        print("Something went wrong with the construction of the potential")
        print("Stopping here")
        exit()
    
    
def BuildComponents(GalactICSDicts):

    #   Build the halo component
    if GalactICSDicts['Halo']['IncludeHalo']=='y':
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"genhalo < "+GalactICSDicts['Halo']['HInFile']
        os.system(BuildCmd)
        if os.path.isfile('halo') == False:
            print("Something went wrong with the construction of the halo")
            ExitCmd()
    #   Build the bulge component
    if GalactICSDicts['Bulge']['IncludeBulge']=='y':
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"genbulge < "+GalactICSDicts['Bulge']['BInFile']
        os.system(BuildCmd)
        if os.path.isfile('bulge') == False:
            print("Something went wrong with the construction of the bulge")
            ExitCmd()
    #   Build the gas disk component
    if GalactICSDicts['GasDisk']['IncludeGasDisk']=='y':
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"gengas < "+GalactICSDicts['GasDisk']['GInFile']
        os.system(BuildCmd)
        if os.path.isfile('gasdisk') == False:
            print("Something went wrong with the construction of the gas disk")
            ExitCmd()

    #   Build the first stellar disk
    if GalactICSDicts['Disk1']['IncludeDisk']=='y':
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"getfreqs"
        os.system(BuildCmd)
        if os.path.isfile('freqdbh.dat') == False:
            print("Something went wrong with the disk frequency calculation")
            ExitCmd()
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"diskdf < in_diskdf1.txt"
        os.system(BuildCmd)
        if os.path.isfile('cordbh.dat') == False:
            print("Something went wrong with the disk frequencies")
            ExitCmd()
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"gendisk < in_disk1.txt"
        os.system(BuildCmd)
        if os.path.isfile('disk') == False:
            print("Something went wrong with the disk construction")
            ExitCmd()


    #   Build the second stellar disk
    if GalactICSDicts['Disk2']['IncludeDisk']=='y':
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"getfreqs"
        os.system(BuildCmd)
        if os.path.isfile('freqdbh.dat') == False:
            print("Something went wrong with the disk frequency calculation")
            ExitCmd()
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"diskdf < in_diskdf2.txt"
        os.system(BuildCmd)
        if os.path.isfile('cordbh.dat') == False:
            print("Something went wrong with the disk frequencies")
            ExitCmd()
        BuildCmd=GalactICSDicts['RunningDictionary']['ExecDir']+"gendisk < in_disk2.txt"
        os.system(BuildCmd)
        if os.path.isfile('disk') == False:
            print("Something went wrong with the disk construction")
            ExitCmd()


def ExitCmd():
    print("Stopping here")
    exit()
