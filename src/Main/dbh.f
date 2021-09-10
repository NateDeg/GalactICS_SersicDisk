ccccccc
c
c     dbh
c
c     This program is the main program for GalactICS
c
cccccccc
      program dbh
      use Globs
      use inDBHMod
      use iniDBHMod
      use TotPotEstMod
      use TotPotCalcMod
      use outDBHMod

      call InDBH()    !Read in the standard in.dbh file (see Inputs/dbhRuntimeInputs.f)
      call IniDBH()   !Initialize the galaxy components and other constants and arrays (see
                        !Initialization/dbhInitialization.f)
      call TotPotEst()  !Estimate the potential due to each component (see
                        !TotalPotentialEstimate/TotalPotEstimate.f)
      call CalcPot()    !Calculate the potential of the system (see CalcTotalPotential/TotPotCalc.f)

      call OutDBH()     !Output all DBH files (see Outputs/DBHOutputs.f)

      return
      end



ccccccccc
