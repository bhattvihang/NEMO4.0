MODULE trcbc
   !!======================================================================
   !!                     ***  MODULE  trcbc  ***
   !! TOP :  module for passive tracer boundary conditions
   !!=====================================================================
   !! History :  3.5 !  2014 (M. Vichi, T. Lovato)  Original
   !!            3.6 !  2015 (T . Lovato) Revision and BDY support
   !!            4.0 !  2016 (T . Lovato) Include application of sbc and cbc
   !!----------------------------------------------------------------------
   !!----------------------------------------------------------------------
   !!   Dummy module                              NO 3D passive tracer data
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_bc_ini( ntrc )        ! Empty routine
      INTEGER,INTENT(IN) :: ntrc                           ! number of tracers
      WRITE(*,*) 'trc_bc_ini: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bc_ini
   SUBROUTINE trc_bc( kt )        ! Empty routine
      WRITE(*,*) 'trc_bc: You should not have seen this print! error?', kt
   END SUBROUTINE trc_bc

   !!======================================================================
END MODULE trcbc
