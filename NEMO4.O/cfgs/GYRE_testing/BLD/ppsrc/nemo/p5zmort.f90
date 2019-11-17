MODULE p5zmort
   !!======================================================================
   !!                         ***  MODULE p5zmort  ***
   !! TOP :   PISCES Compute the mortality terms for phytoplankton
   !!======================================================================
   !! History :   1.0  !  2002     (O. Aumont)  Original code
   !!             2.0  !  2007-12  (C. Ethe, G. Madec)  F90
   !!             3.6  !  2015-05  (O. Aumont) PISCES quota
   !!----------------------------------------------------------------------
   !!   p5z_mort       :   Compute the mortality terms for phytoplankton
   !!   p5z_mort_init  :   Initialize the mortality params for phytoplankton
   !!----------------------------------------------------------------------
   USE oce_trc         !  shared variables between ocean and passive tracers
   USE trc             !  passive tracers common variables 
   USE sms_pisces      !  PISCES Source Minus Sink variables
   USE p4zlim
   USE p5zlim          !  Phytoplankton limitation terms
   USE prtctl_trc      !  print control for debugging

   IMPLICIT NONE
   PRIVATE

   PUBLIC   p5z_mort    
   PUBLIC   p5z_mort_init    

   !! * Shared module variables
   REAL(wp), PUBLIC :: wchln    !:
   REAL(wp), PUBLIC :: wchlp   !:
   REAL(wp), PUBLIC :: wchld   !:
   REAL(wp), PUBLIC :: wchldm  !:
   REAL(wp), PUBLIC :: mpratn   !:
   REAL(wp), PUBLIC :: mpratp  !:
   REAL(wp), PUBLIC :: mpratd  !:

   !!----------------------------------------------------------------------
   !! NEMO/TOP 4.0 , NEMO Consortium (2018)
   !! $Id: p5zmort.F90 11536 2019-09-11 13:54:18Z smasson $ 
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS

   SUBROUTINE p5z_mort( kt )
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_mort  ***
      !!
      !! ** Purpose :   Calls the different subroutine to initialize and compute
      !!                the different phytoplankton mortality terms
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER, INTENT(in) ::   kt ! ocean time step
      !!---------------------------------------------------------------------

      CALL p5z_nano            ! nanophytoplankton
      CALL p5z_pico            ! picophytoplankton
      CALL p5z_diat            ! diatoms

   END SUBROUTINE p5z_mort


   SUBROUTINE p5z_nano
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_nano  ***
      !!
      !! ** Purpose :   Compute the mortality terms for nanophytoplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompaph
      REAL(wp) :: zfactfe, zfactch, zfactn, zfactp, zprcaca
      REAL(wp) :: ztortp , zrespp , zmortp
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_nano')
      !
      prodcal(:,:,:) = 0.  !: calcite production variable set to zero
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zcompaph = MAX( ( trb(ji,jj,jk,jpphy) - 1e-9 ), 0.e0 )
               !   Squared mortality of Phyto similar to a sedimentation term during
               !   blooms (Doney et al. 1996)
               !   -----------------------------------------------------------------
               zrespp = wchln * 1.e6 * xstep * xdiss(ji,jj,jk) * zcompaph * trb(ji,jj,jk,jpphy)

               !   Phytoplankton linear mortality
               !   ------------------------------
               ztortp = mpratn * xstep  * zcompaph
               zmortp = zrespp + ztortp

               !   Update the arrays TRA which contains the biological sources and sinks

               zfactn  = trb(ji,jj,jk,jpnph)/(trb(ji,jj,jk,jpphy)+rtrn)
               zfactp  = trb(ji,jj,jk,jppph)/(trb(ji,jj,jk,jpphy)+rtrn)
               zfactfe = trb(ji,jj,jk,jpnfe)/(trb(ji,jj,jk,jpphy)+rtrn)
               zfactch = trb(ji,jj,jk,jpnch)/(trb(ji,jj,jk,jpphy)+rtrn)
               tra(ji,jj,jk,jpphy) = tra(ji,jj,jk,jpphy) - zmortp
               tra(ji,jj,jk,jpnph) = tra(ji,jj,jk,jpnph) - zmortp * zfactn
               tra(ji,jj,jk,jppph) = tra(ji,jj,jk,jppph) - zmortp * zfactp
               tra(ji,jj,jk,jpnch) = tra(ji,jj,jk,jpnch) - zmortp * zfactch
               tra(ji,jj,jk,jpnfe) = tra(ji,jj,jk,jpnfe) - zmortp * zfactfe
               zprcaca = xfracal(ji,jj,jk) * zmortp
               !
               prodcal(ji,jj,jk) = prodcal(ji,jj,jk) + zprcaca  ! prodcal=prodcal(nanophy)+prodcal(microzoo)+prodcal(mesozoo)
               !
               tra(ji,jj,jk,jpdic) = tra(ji,jj,jk,jpdic) - zprcaca
               tra(ji,jj,jk,jptal) = tra(ji,jj,jk,jptal) - 2. * zprcaca
               tra(ji,jj,jk,jpcal) = tra(ji,jj,jk,jpcal) + zprcaca
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zmortp
               tra(ji,jj,jk,jppon) = tra(ji,jj,jk,jppon) + zmortp * zfactn
               tra(ji,jj,jk,jppop) = tra(ji,jj,jk,jppop) + zmortp * zfactp
               prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + zmortp
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zmortp * zfactfe
            END DO
         END DO
      END DO
      !
       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('nano')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_nano')
      !
   END SUBROUTINE p5z_nano


   SUBROUTINE p5z_pico
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_pico  ***
      !!
      !! ** Purpose :   Compute the mortality terms for picophytoplankton
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER  :: ji, jj, jk
      REAL(wp) :: zcompaph
      REAL(wp) :: zfactfe, zfactch, zfactn, zfactp
      REAL(wp) :: ztortp , zrespp , zmortp 
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_pico')
      !
      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi
               zcompaph = MAX( ( trb(ji,jj,jk,jppic) - 1e-9 ), 0.e0 )
               !  Squared mortality of Phyto similar to a sedimentation term during
               !  blooms (Doney et al. 1996)
               !  -----------------------------------------------------------------
               zrespp = wchlp * 1.e6 * xstep * xdiss(ji,jj,jk) * zcompaph * trb(ji,jj,jk,jppic)

               !     Phytoplankton mortality 
               ztortp = mpratp * xstep  * zcompaph
               zmortp = zrespp + ztortp

               !   Update the arrays TRA which contains the biological sources and sinks

               zfactn = trb(ji,jj,jk,jpnpi)/(trb(ji,jj,jk,jppic)+rtrn)
               zfactp = trb(ji,jj,jk,jpppi)/(trb(ji,jj,jk,jppic)+rtrn)
               zfactfe = trb(ji,jj,jk,jppfe)/(trb(ji,jj,jk,jppic)+rtrn)
               zfactch = trb(ji,jj,jk,jppch)/(trb(ji,jj,jk,jppic)+rtrn)
               tra(ji,jj,jk,jppic) = tra(ji,jj,jk,jppic) - zmortp
               tra(ji,jj,jk,jpnpi) = tra(ji,jj,jk,jpnpi) - zmortp * zfactn
               tra(ji,jj,jk,jpppi) = tra(ji,jj,jk,jpppi) - zmortp * zfactp
               tra(ji,jj,jk,jppch) = tra(ji,jj,jk,jppch) - zmortp * zfactch
               tra(ji,jj,jk,jppfe) = tra(ji,jj,jk,jppfe) - zmortp * zfactfe
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + zmortp
               tra(ji,jj,jk,jppon) = tra(ji,jj,jk,jppon) + zmortp * zfactn
               tra(ji,jj,jk,jppop) = tra(ji,jj,jk,jppop) + zmortp * zfactp
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + zmortp * zfactfe
               prodpoc(ji,jj,jk) = prodpoc(ji,jj,jk) + zmortp
            END DO
         END DO
      END DO
      !
       IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('pico')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
       ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_pico')
      !
   END SUBROUTINE p5z_pico


   SUBROUTINE p5z_diat
      !!---------------------------------------------------------------------
      !!                     ***  ROUTINE p5z_diat  ***
      !!
      !! ** Purpose :   Compute the mortality terms for diatoms
      !!
      !! ** Method  : - ???
      !!---------------------------------------------------------------------
      INTEGER  ::  ji, jj, jk
      REAL(wp) ::  zfactfe,zfactsi,zfactch, zfactn, zfactp, zcompadi
      REAL(wp) ::  zrespp2, ztortp2, zmortp2
      REAL(wp) ::  zlim2, zlim1
      CHARACTER (len=25) :: charout
      !!---------------------------------------------------------------------
      !
      IF( ln_timing )   CALL timing_start('p5z_diat')
      !

      DO jk = 1, jpkm1
         DO jj = 1, jpj
            DO ji = 1, jpi

               zcompadi = MAX( ( trb(ji,jj,jk,jpdia) - 1E-9), 0. )

               !   Aggregation term for diatoms is increased in case of nutrient
               !   stress as observed in reality. The stressed cells become more
               !   sticky and coagulate to sink quickly out of the euphotic zone
               !   -------------------------------------------------------------
               !  Phytoplankton squared mortality
               !  -------------------------------
               zlim2   = xlimdia(ji,jj,jk) * xlimdia(ji,jj,jk)
               zlim1   = 0.25 * ( 1. - zlim2 ) / ( 0.25 + zlim2 ) 
               zrespp2 = 1.e6 * xstep * (  wchld + wchldm * zlim1 ) * xdiss(ji,jj,jk) * zcompadi * trb(ji,jj,jk,jpdia)

               !  Phytoplankton linear mortality 
               !  ------------------------------
               ztortp2 = mpratd * xstep  * zcompadi
               zmortp2 = zrespp2 + ztortp2

               !   Update the arrays tra which contains the biological sources and sinks
               !   ---------------------------------------------------------------------
               zfactn  = trb(ji,jj,jk,jpndi) / ( trb(ji,jj,jk,jpdia) + rtrn )
               zfactp  = trb(ji,jj,jk,jppdi) / ( trb(ji,jj,jk,jpdia) + rtrn )
               zfactch = trb(ji,jj,jk,jpdch) / ( trb(ji,jj,jk,jpdia) + rtrn )
               zfactfe = trb(ji,jj,jk,jpdfe) / ( trb(ji,jj,jk,jpdia) + rtrn )
               zfactsi = trb(ji,jj,jk,jpdsi) / ( trb(ji,jj,jk,jpdia) + rtrn )
               tra(ji,jj,jk,jpdia) = tra(ji,jj,jk,jpdia) - zmortp2 
               tra(ji,jj,jk,jpndi) = tra(ji,jj,jk,jpndi) - zmortp2 * zfactn
               tra(ji,jj,jk,jppdi) = tra(ji,jj,jk,jppdi) - zmortp2 * zfactp
               tra(ji,jj,jk,jpdch) = tra(ji,jj,jk,jpdch) - zmortp2 * zfactch
               tra(ji,jj,jk,jpdfe) = tra(ji,jj,jk,jpdfe) - zmortp2 * zfactfe
               tra(ji,jj,jk,jpdsi) = tra(ji,jj,jk,jpdsi) - zmortp2 * zfactsi
               tra(ji,jj,jk,jpgsi) = tra(ji,jj,jk,jpgsi) + zmortp2 * zfactsi
               tra(ji,jj,jk,jpgoc) = tra(ji,jj,jk,jpgoc) + zrespp2 
               tra(ji,jj,jk,jpgon) = tra(ji,jj,jk,jpgon) + zrespp2 * zfactn
               tra(ji,jj,jk,jpgop) = tra(ji,jj,jk,jpgop) + zrespp2 * zfactp
               tra(ji,jj,jk,jpbfe) = tra(ji,jj,jk,jpbfe) + zrespp2 * zfactfe
               tra(ji,jj,jk,jppoc) = tra(ji,jj,jk,jppoc) + ztortp2
               tra(ji,jj,jk,jppon) = tra(ji,jj,jk,jppon) + ztortp2 * zfactn
               tra(ji,jj,jk,jppop) = tra(ji,jj,jk,jppop) + ztortp2 * zfactp
               tra(ji,jj,jk,jpsfe) = tra(ji,jj,jk,jpsfe) + ztortp2 * zfactfe
               prodpoc(ji,jj,jk)   = prodpoc(ji,jj,jk) + ztortp2
               prodgoc(ji,jj,jk)   = prodgoc(ji,jj,jk) + zrespp2
            END DO
         END DO
      END DO
      !
      IF(ln_ctl)   THEN  ! print mean trends (used for debugging)
         WRITE(charout, FMT="('diat')")
         CALL prt_ctl_trc_info(charout)
         CALL prt_ctl_trc(tab4d=tra, mask=tmask, clinfo=ctrcnm)
      ENDIF
      !
      IF( ln_timing )   CALL timing_stop('p5z_diat')
      !
   END SUBROUTINE p5z_diat


   SUBROUTINE p5z_mort_init
      !!----------------------------------------------------------------------
      !!                  ***  ROUTINE p5z_mort_init  ***
      !!
      !! ** Purpose :   Initialization of phytoplankton parameters
      !!
      !! ** Method  :   Read the nampismort namelist and check the parameters
      !!      called at the first timestep
      !!
      !! ** input   :   Namelist nampismort
      !!
      !!----------------------------------------------------------------------
      INTEGER :: ios                 ! Local integer output status for namelist read
      !!
      NAMELIST/namp5zmort/ wchln, wchlp, wchld, wchldm, mpratn, mpratp, mpratd
      !!----------------------------------------------------------------------

      REWIND( numnatp_ref )              ! Namelist nampismort in reference namelist : Pisces phytoplankton
      READ  ( numnatp_ref, namp5zmort, IOSTAT = ios, ERR = 901)
901   IF( ios /= 0 ) CALL ctl_nam ( ios , 'namp5zmort in reference namelist' )

      REWIND( numnatp_cfg )              ! Namelist nampismort in configuration namelist : Pisces phytoplankton
      READ  ( numnatp_cfg, namp5zmort, IOSTAT = ios, ERR = 902 )
902   IF( ios >  0 ) CALL ctl_nam ( ios , 'namp5zmort in configuration namelist' )
      IF(lwm) WRITE ( numonp, namp5zmort )

      IF(lwp) THEN                         ! control print
         WRITE(numout,*) ' '
         WRITE(numout,*) ' Namelist parameters for phytoplankton mortality, namp5zmort'
         WRITE(numout,*) ' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
         WRITE(numout,*) '    quadratic mortality of phytoplankton      wchln     =', wchln
         WRITE(numout,*) '    quadratic mortality of picophyto.         wchlp     =', wchlp
         WRITE(numout,*) '    quadratic mortality of diatoms            wchld     =', wchld
         WRITE(numout,*) '    Additional quadratic mortality of diatoms wchldm    =', wchldm
         WRITE(numout,*) '    nanophyto. mortality rate                 mpratn    =', mpratn
         WRITE(numout,*) '    picophyto. mortality rate                 mpratp    =', mpratp
         WRITE(numout,*) '    Diatoms mortality rate                    mpratd    =', mpratd
      ENDIF

   END SUBROUTINE p5z_mort_init

   !!======================================================================
END MODULE p5zmort
