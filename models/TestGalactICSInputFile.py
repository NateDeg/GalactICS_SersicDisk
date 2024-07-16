"""
This module contains a set of default dictionaries that will build a GalactICS MW model based on the McMillan 2017 model and it should match the Deg et al. 2019 GalactIC realization.
"""


#   Start by defining the general running dictionary
RunningDictionary={}
RunningDictionary['TargFolder']="McMillan2017"

#   Set the Halo dictionary
Halo={}
Halo['IncludeHalo']='y'
Halo['ScaleRadius']=19.6       #kpc
Halo['ScaleVelocity']=2.978      #100 km/s
Halo['TrunctationRadius']=150.  #kpc
Halo['dTruncRadius']=20.        #kpc
Halo['InnerSlope']=1.           #unitless
Halo['OuterSlope']=3.           #unitless
Halo['ParticleNumber']=50000   #unitless
Halo['SpinIsotropy']=0.5        #unitless
Halo['RandomSeed']=-1           #unitless
Halo['RecenterSwitch']=1        #unitless


#   Set the thin stellar disk dictionary
Disk1={}
Disk1['IncludeDisk']='y'
Disk1['DiskMass']=15.133            #2.325e9 Msol
Disk1['ScaleRadius']=2.5            #kpc
Disk1['ScaleHeight']=0.27649        #kpc
Disk1['TruncationRadius']=30.       #kpc
Disk1['HeightTruncation']=3.        #kpc
Disk1['SersicIndx']=1.              #unitless
Disk1['ScaleRadialVelDisp1']=1.     #100 km/s
Disk1['VelDispScaleRadius1']=2.7    #kpc
Disk1['ScaleRadialVelDisp2']=0.     #100 km/s
Disk1['VelDispScaleRadius2']=1.     #kpc
Disk1['numSpline']=50               #unitless
Disk1['numIterations']=10           #unitless
Disk1['ParticleNumber']=30000      #unitless
Disk1['RandomSeed']=-1              #unitless
Disk1['RecenterSwitch']=1           #unitless



#   Set the thick stellar disk dictionary
Disk2={}
Disk2['IncludeDisk']='y'
Disk2['DiskMass']=3.091             #2.325e9 Msol
Disk2['ScaleRadius']=3.02           #kpc
Disk2['ScaleHeight']=.82946         #kpc
Disk2['TruncationRadius']=30.       #kpc
Disk2['HeightTruncation']=3.        #kpc
Disk2['SersicIndx']=1.              #unitless
Disk2['ScaleRadialVelDisp1']=1.2    #100 km/s
Disk2['VelDispScaleRadius1']=2.71   #kpc
Disk2['ScaleRadialVelDisp2']=0.     #100 km/s
Disk2['VelDispScaleRadius2']=1.     #kpc
Disk2['numSpline']=50               #unitless
Disk2['numIterations']=10           #unitless
Disk2['ParticleNumber']=10000      #unitless
Disk2['RandomSeed']=-1              #unitless
Disk2['RecenterSwitch']=1           #unitless


#   Set the gas disk dictionary
GasDisk={}

GasDisk['IncludeGasDisk']='y'
GasDisk['DiskMass']=10.3                #2.325e9 Msol
GasDisk['ScaleRadius']=13.1             #kpc
GasDisk['KinematicTemperature']=0.0083    #Kelvin
GasDisk['TruncationRadius']=60.         #kpc
GasDisk['HeightTruncation']=3.          #kpc
GasDisk['ParticleNumber']=20000        #unitless
GasDisk['RandomSeed']=-1                #unitless
GasDisk['RecenterSwitch']=1             #unitless
GasDisk['nSplinePoints']=100            #unitless


#   Set the bulge dictionary
Bulge={}
Bulge['IncludeBulge']='y'
Bulge['SersicIndex']=2.0            #unitless
Bulge['ScaleVelocity']=3.04         #100 km/s
Bulge['ScaleRadius']=0.64           #kpc
Bulge['ParticleNumber']= 5000      #unitless
Bulge['SpinIsotropy']=0.5           #unitless
Bulge['RandomSeed']=-8              #unitless
Bulge['RecenterSwitch']=1           #unitless

#   Set the Poisson Grid Dictionary
PoissonDict={}
PoissonDict['dR']=0.05              #kpc
PoissonDict['nR']=10000             #unitless
PoissonDict['lmax']=10             #unitless
PoissonDict['npsi']=1000           #unitless
PoissonDict['nint']=20             #unitless
