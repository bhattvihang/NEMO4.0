MODULE obs_inter_h2d
   !!======================================================================
   !!                       ***  MODULE obs_inter_h2d   ***
   !! Observation diagnostics: Perform the horizontal interpolation
   !!                          from model grid to observation location
   !!=====================================================================

   !!----------------------------------------------------------------------
   !!   obs_int_h2d     : Horizontal interpolation to the observation point
   !!   obs_int_h2d_ds1 : Distance-weighted interpolation                 (n2dint=0)
   !!   obs_int_h2d_ds2 : Distance-weighted interpolation (small angle)   (n2dint=1)
   !!   obs_int_h2d_bil : Bilinear interpolation (geographical grid)      (n2dint=2)
   !!   obs_int_h2d_bir : Bilinear remapping interpolation (general grid) (n2dint=3)
   !!   obs_int_h2d_pol : Polynomial interpolation                        (n2dint=4)
   !!   bil_wgt         : Compute weights for bilinear remapping
   !!   lu_invmat       : Invert a matrix using LU decomposition
   !!   lu_decomp       : LU decomposition
   !!   lu_backsb       : LU decomposition - back substitution
   !!----------------------------------------------------------------------
   !! * Modules used
   USE par_kind, ONLY : &  ! Precision variables
      & wp
   USE phycst,   ONLY : &  ! Physical constants
      & rad,  &
      & rpi
   USE in_out_manager
   USE obs_const, ONLY : &
      & obfillflt		! Fillvalue
   USE obs_utils           ! Utility functions
      USE lib_mpp,ONLY : &
      & ctl_warn, ctl_stop

   IMPLICIT NONE

   !! * Routine accessibility
   PRIVATE obs_int_h2d_ds1, & ! Distance-weighted interpolation               
      &    obs_int_h2d_ds2, & ! Distance-weighted interpolation (small angle) 
      &    obs_int_h2d_bil, & ! Bilinear interpolation (geographical grid)    
      &    obs_int_h2d_bir, & ! Bilinear remapping interpolation (general grid)
      &    obs_int_h2d_pol, & ! Polynomial interpolation                       
      &    lu_invmat,       & ! Invert a matrix using LU decomposition
      &    lu_decomp,       & ! LU decomposition
      &    lu_backsb,       & ! LU decomposition - back substitution
      &    bil_wgt            ! Compute weights for bilinear remapping
   PUBLIC obs_int_h2d,      & ! Horizontal interpolation to the observation point
      &   obs_int_h2d_init    ! Set up weights and vertical mask

   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: obs_inter_h2d.F90 10068 2018-08-28 14:09:04Z nicolasmartin $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

CONTAINS
 
   !!----------------------------------------------------------------------
   !! NEMO/OCE 4.0 , NEMO Consortium (2018)
   !! $Id: obsinter_h2d.h90 10353 2018-11-21 16:04:47Z mathiot $
   !! Software governed by the CeCILL license (see ./LICENSE)
   !!----------------------------------------------------------------------

   SUBROUTINE obs_int_h2d_init( kpk,   kpk2,  k2dint, plam,  pphi, &
      &                         pglam, pgphi, pmask,  pweig, pobsmask, &
      &                         iminpoints )
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d  ***
      !!
      !! ** Purpose : Computes weights for horizontal interpolation to the 
      !!              observation point.
      !!
      !! ** Method  : Horizontal interpolation to the observation point using
      !!              model values at the corners of the surrounding grid 
      !!              points.
      !!
      !!    Interpolation Schemes : 
      !!
      !!    1) k2dint = 0: Distance-weighted interpolation scheme 1
      !!
      !!       The interpolation weights are computed as a weighted 
      !!       sum of the distance between the model grid points (A)
      !!       and the observation point (B). Distance (s) is computed 
      !!       using the great-circle distance formula: 
      !!
      !!       s(AB) = arcos(   sin( phiA ) x sin( phiB )
      !!                      + cos( phiA ) x cos( phiB ) 
      !!                                    x cos( lamB - lamA ) )
      !!
      !!    2) k2dint = 1: Distance-weighted interpolation scheme 2
      !!
      !!       As k2dint = 0 but with distance (ds) computed using 
      !!       a small-angle approximation to the great-circle formula:
      !!
      !!       ds(AB) = sqrt(   ( phiB - phiA )^{2} 
      !!                    + ( ( lamB - lamA ) * cos( phiB ) )^{2} )
      !!
      !!    3) k2dint = 2: Bilinear interpolation on a geographical grid
      !!
      !!       The interpolation is split into two 1D interpolations in
      !!       the longitude and latitude directions, respectively.
      !!
      !!    4) k2dint = 3: General bilinear remapping interpolation
      !!
      !!       An iterative scheme that involves first mapping a 
      !!       quadrilateral cell into a cell with coordinates 
      !!       (0,0), (1,0), (0,1) and (1,1).
      !!
      !!    5) k2dint = 4: Polynomial interpolation
      !!
      !!       The interpolation weights are computed by fitting a 
      !!       polynomial function of the form 
      !!              
      !!       P(i) = a1(i) + a2(i) * phi + a3(i) * plam 
      !!                                  + a4(i) * phi * plam
      !!    
      !!       through the model values at the four surrounding grid points.
      !!
      !! ** Action  :
      !!                   
      !! References : Jones, P.: A users guide for SCRIP: A Spherical 
      !!                        Coordinate Remapping and Interpolation Package.
      !!                        Version 1.4. Los Alomos. 
      !!                    
      !!       http://www.acl.lanl.gov/climate/software/SCRIP/SCRIPmain.html
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-03 (A. Vidard) NEMOVAR migration
      !!        ! 06-10 (A. Weaver) Cleanup
      !!        ! 07-08 (K. Mogensen) Split in two routines for easier adj.
      !!-----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kpk,   &             ! Parameter values for automatic arrays
         & kpk2,  &        
         & k2dint               ! Interpolation scheme options
                                ! = 0 distance-weighted (great circle)
                                ! = 1 distance-weighted (small angle)
                                ! = 2 bilinear (geographical grid)
                                ! = 3 bilinear (quadrilateral grid)
                                ! = 4 polynomial (quadrilateral grid)
      REAL(KIND=wp), INTENT(INOUT) :: &
         & plam, &
         & pphi                 ! Geographical (lat,lon) coordinates of
                                ! observation
      REAL(KIND=wp), DIMENSION(2,2), INTENT(IN) :: &
         & pglam, &             ! Model variable lat
         & pgphi                ! Model variable lon
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pmask                ! Model variable mask
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(OUT) ::  &
         & pweig                ! Weights for interpolation
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) ::  &
         & pobsmask             ! Vertical mask for observations
      INTEGER, INTENT(IN), OPTIONAL :: &
         & iminpoints           ! Reject point which is not surrounded
                                ! by at least iminpoints sea points

      !! * Local declarations
      INTEGER :: &
         & jk
      INTEGER :: &
         & ikmax, &
         & iamb1, &
         & iamb2
      REAL(KIND=wp) :: &
         & zphimm,  &
         & zphimp,  &
         & zphipm,  &
         & zphipp,  &
         & zlammm,  &
         & zlammp,  &
         & zlampm,  &
         & zlampp,  &
         & zphimin, &
         & zphimax, &
         & zlammin, &
         & zlammax
      REAL(KIND=wp), DIMENSION(kpk2) :: &
         & z2dmm,  &
         & z2dmp,  &
         & z2dpm,  &
         & z2dpp,  &
         & z2dmmt, &
         & z2dmpt, &
         & z2dpmt, &
         & z2dppt, &
         & zsum
      LOGICAL :: &
         & ll_ds1, &
         & ll_skip, &
         & ll_fail
       
      !------------------------------------------------------------------------
      ! Constants for the 360 degrees ambiguity
      !------------------------------------------------------------------------
      iamb1 = 10   ! dlam < iamb1 * dphi
      iamb2 = 3    ! Special treatment if iamb2 * lam < max(lam) 
       
      !------------------------------------------------------------------------
      ! Initialize number of levels
      !------------------------------------------------------------------------
      IF ( kpk2 == 1 ) THEN
         ikmax = 1
      ELSEIF ( kpk2 == kpk) THEN
         ikmax = kpk-1
      ENDIF
      !------------------------------------------------------------------------
      ! Initialize the cell corners
      !------------------------------------------------------------------------
      zphimm = pgphi(1,1)
      zphimp = pgphi(1,2)
      zphipm = pgphi(2,1)
      zphipp = pgphi(2,2)
      zlammm = pglam(1,1)
      zlammp = pglam(1,2)
      zlampm = pglam(2,1)
      zlampp = pglam(2,2)
       
      !------------------------------------------------------------------------
      ! Treat the 360 degrees ambiguity
      !------------------------------------------------------------------------
      DO WHILE ( ( zlammm < 0.0_wp ).OR.( zlammm > 360.0_wp )  &
         &   .OR.( zlampm < 0.0_wp ).OR.( zlampm > 360.0_wp )  &
         &   .OR.( zlampp < 0.0_wp ).OR.( zlampp > 360.0_wp )  &
         &   .OR.( zlammp < 0.0_wp ).OR.( zlammp > 360.0_wp ) )
       
         IF ( zlammm < 0.0_wp   ) zlammm = zlammm + 360.0_wp
         IF ( zlammm > 360.0_wp ) zlammm = zlammm - 360.0_wp
         IF ( zlammp < 0.0_wp   ) zlammp = zlammp + 360.0_wp
         IF ( zlammp > 360.0_wp ) zlammp = zlammp - 360.0_wp
         IF ( zlampm < 0.0_wp   ) zlampm = zlampm + 360.0_wp
         IF ( zlampm > 360.0_wp ) zlampm = zlampm - 360.0_wp
         IF ( zlampp < 0.0_wp   ) zlampp = zlampp + 360.0_wp
         IF ( zlampp > 360.0_wp ) zlampp = zlampp - 360.0_wp
       
      END DO

      DO WHILE ( ( plam < 0.0_wp ) .OR. ( plam > 360.0_wp ) )
         IF ( plam < 0.0_wp   ) plam = plam + 360.0_wp
         IF ( plam > 360.0_wp ) plam = plam - 360.0_wp
      END DO

      !------------------------------------------------------------------------
      ! Special case for observation on grid points
      !------------------------------------------------------------------------
      ll_skip = .FALSE.
      IF ( ( ABS( zphimm - pphi ) < 1.0e-6_wp ) .AND. &
         & ( ABS( zlammm - plam ) < 1.0e-6_wp ) ) THEN
         z2dmm(:) = 1.0_wp
         z2dpm(:) = 0.0_wp
         z2dmp(:) = 0.0_wp
         z2dpp(:) = 0.0_wp
         ll_skip = .TRUE.
      ENDIF
      IF ( ( ABS( zphipm - pphi ) < 1.0e-6_wp ) .AND. &
         & ( ABS( zlampm - plam ) < 1.0e-6_wp ) ) THEN
         z2dmm(:) = 0.0_wp
         z2dpm(:) = 1.0_wp
         z2dmp(:) = 0.0_wp
         z2dpp(:) = 0.0_wp
         ll_skip = .TRUE.
      ENDIF
      IF ( ( ABS( zphimp - pphi ) < 1.0e-6_wp ) .AND. &
         & ( ABS( zlammp - plam ) < 1.0e-6_wp ) ) THEN
         z2dmm(:) = 0.0_wp
         z2dpm(:) = 0.0_wp
         z2dmp(:) = 1.0_wp
         z2dpp(:) = 0.0_wp
         ll_skip = .TRUE.
      ENDIF
      IF ( ( ABS( zphipp - pphi ) < 1.0e-6_wp ) .AND. &
         & ( ABS( zlampp - plam ) < 1.0e-6_wp ) ) THEN
         z2dmm(:) = 0.0_wp
         z2dpm(:) = 0.0_wp
         z2dmp(:) = 0.0_wp
         z2dpp(:) = 1.0_wp
         ll_skip = .TRUE.
      ENDIF

      IF ( .NOT.ll_skip ) THEN

         zphimin = MIN( zphimm, zphipm, zphipp, zphimp )
         zphimax = MAX( zphimm, zphipm, zphipp, zphimp )
         zlammin = MIN( zlammm, zlampm, zlampp, zlammp )
         zlammax = MAX( zlammm, zlampm, zlampp, zlammp )

         IF ( ( ( zlammax - zlammin ) / ( zphimax - zphimin ) ) > iamb1 ) THEN
            IF ( iamb2 * zlammm < zlammax ) zlammm = zlammm + 360.0_wp
            IF ( iamb2 * zlammp < zlammax ) zlammp = zlammp + 360.0_wp
            IF ( iamb2 * zlampm < zlammax ) zlampm = zlampm + 360.0_wp
            IF ( iamb2 * zlampp < zlammax ) zlampp = zlampp + 360.0_wp
         ENDIF

         zlammin = MIN( zlammm, zlampm, zlampp, zlammp )
         IF ( zlammm > ( zlammin + 180.0_wp ) ) zlammm = zlammm - 360.0_wp
         IF ( zlammp > ( zlammin + 180.0_wp ) ) zlammp = zlammp - 360.0_wp
         IF ( zlampm > ( zlammin + 180.0_wp ) ) zlampm = zlampm - 360.0_wp
         IF ( zlampp > ( zlammin + 180.0_wp ) ) zlampp = zlampp - 360.0_wp
         
         IF ( plam < zlammin ) plam = plam + 360.0_wp
	   z2dmm = 0.0_wp
	   z2dmp = 0.0_wp
	   z2dpm = 0.0_wp
	   z2dpp = 0.0_wp
         SELECT CASE (k2dint)
            
         CASE(0)
            CALL obs_int_h2d_ds1( kpk2, ikmax,                        &
               &                  pphi, plam, pmask,                  &
               &                  zphimm, zlammm, zphimp, zlammp,     & 
               &                  zphipm, zlampm, zphipp, zlampp,     &
               &                  z2dmm, z2dmp, z2dpm, z2dpp )
         CASE(1)
            CALL obs_int_h2d_ds2( kpk2, ikmax,                        &
               &                  pphi, plam, pmask,                  &
               &                  zphimm, zlammm, zphimp, zlammp,     &
               &                  zphipm, zlampm, zphipp, zlampp,     &
               &                  z2dmm, z2dmp, z2dpm, z2dpp )
         CASE(2)
            CALL obs_int_h2d_bil( kpk2, ikmax,                        &
               &                  pphi, plam, pmask,                  &
               &                                          zlammp,     &
               &                  zphipm,         zphipp, zlampp,     &
               &                  z2dmm, z2dmp, z2dpm, z2dpp )
         CASE(3)
            CALL obs_int_h2d_bir( kpk2, ikmax,                        &
               &                  pphi, plam, pmask,                  &
               &                  zphimm, zlammm, zphimp, zlammp,     &
               &                  zphipm, zlampm, zphipp, zlampp,     &
               &                  z2dmm, z2dmp, z2dpm, z2dpp, ll_fail )
            IF (ll_fail) THEN
               IF(lwp) THEN
                  WRITE(numout,*)'Bilinear weight computation failed'
                  WRITE(numout,*)'Switching to great circle distance'
                  WRITE(numout,*)
               ENDIF
               CALL obs_int_h2d_ds1( kpk2, ikmax,                        &
                  &                  pphi, plam, pmask,                  &
                  &                  zphimm, zlammm, zphimp, zlammp,     & 
                  &                  zphipm, zlampm, zphipp, zlampp,     &
                  &                  z2dmm, z2dmp, z2dpm, z2dpp )
            ENDIF
         CASE(4)  
            CALL obs_int_h2d_pol( kpk2, ikmax,                        &
               &                  pphi, plam, pmask,                  &
               &                  zphimm, zlammm, zphimp, zlammp,     &
               &                  zphipm, zlampm, zphipp, zlampp,     & 
               &                  z2dmm, z2dmp, z2dpm, z2dpp )
         END SELECT

      ENDIF
      !------------------------------------------------------------------------
      ! Compute weights for interpolation to the observation point
      !------------------------------------------------------------------------
      pobsmask(:) = 0.0_wp
      pweig(:,:,:) = 0.0_wp
      ! ll_ds1 is used for failed interpolations
      ll_ds1 = .FALSE.
      DO jk = 1, ikmax
         IF (PRESENT(iminpoints)) THEN
            IF (NINT(SUM(pmask(:,:,jk)))<iminpoints) CYCLE
         ENDIF
         zsum(jk) = z2dmm(jk) + z2dmp(jk) + z2dpm(jk) + z2dpp(jk) 
         IF ( zsum(jk) /= 0.0_wp ) THEN
            pweig(1,1,jk) = z2dmm(jk) 
            pweig(1,2,jk) = z2dmp(jk)
            pweig(2,1,jk) = z2dpm(jk)
            pweig(2,2,jk) = z2dpp(jk)
            ! Set the vertical mask
            IF ( ( ( z2dmm(jk) > 0.0_wp ) .AND.       &
               &   ( pmask(1,1,jk) == 1.0_wp ) ) .OR. &
               & ( ( z2dmp(jk) > 0.0_wp ) .AND.       &
               &   ( pmask(1,2,jk) == 1.0_wp ) ) .OR. &
               & ( ( z2dpm(jk) > 0.0_wp ) .AND.       &
               &   ( pmask(2,1,jk) == 1.0_wp ) ) .OR. &
               & ( ( z2dpp(jk) > 0.0_wp ) .AND.       &
               &   ( pmask(2,2,jk) == 1.0_wp ) ) ) pobsmask(jk)=1.0_wp
         ELSE
            ! If the interpolation has failed due to the point
            ! being on the intersect of two land points retry with
            ! k2dint = 0
            IF ( ( pmask(1,1,jk) /= 0.0_wp ).OR. &
               & ( pmask(1,2,jk) /= 0.0_wp ).OR. &
               & ( pmask(2,1,jk) /= 0.0_wp ).OR. &
               & ( pmask(2,2,jk) /= 0.0_wp ) ) THEN
               ! If ll_ds1 is false compute k2dint = 0 weights
               IF ( .NOT.ll_ds1 ) THEN
                  CALL obs_int_h2d_ds1( kpk2, ikmax,                        &
                     &                  pphi, plam, pmask,                  &
                     &                  zphimm, zlammm, zphimp, zlammp,     & 
                     &                  zphipm, zlampm, zphipp, zlampp,     &
                     &                  z2dmmt, z2dmpt, z2dpmt, z2dppt ) 
                  ll_ds1 = .TRUE.
               ENDIF
               zsum(jk) = z2dmmt(jk) + z2dmpt(jk) + z2dpmt(jk) + z2dppt(jk) 
               IF ( zsum(jk) /= 0.0_wp ) THEN
                  pweig(1,1,jk) = z2dmmt(jk) 
                  pweig(1,2,jk) = z2dmpt(jk)
                  pweig(2,1,jk) = z2dpmt(jk)
                  pweig(2,2,jk) = z2dppt(jk)
                  ! Set the vertical mask
                  IF ( ( ( z2dmmt(jk) > 0.0_wp ) .AND.      &
                     &   ( pmask(1,1,jk) == 1.0_wp ) ) .OR. &
                     & ( ( z2dmpt(jk) > 0.0_wp ) .AND.      &
                     &   ( pmask(1,2,jk) == 1.0_wp ) ) .OR. &
                     & ( ( z2dpmt(jk) > 0.0_wp) .AND.      &
                     &   ( pmask(2,1,jk) == 1.0_wp ) ) .OR. &
                     & ( ( z2dppt(jk) > 0.0_wp ) .AND.      &
                     &   ( pmask(2,2,jk) == 1.0_wp ) ) )    &
                     & pobsmask(jk)=1.0_wp
               ENDIF
            ENDIF
         ENDIF
      END DO
  
   END SUBROUTINE obs_int_h2d_init

   SUBROUTINE obs_int_h2d( kpk, kpk2, &
      &                    pweig, pmod, pobsk )
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d  ***
      !!
      !! ** Purpose : Horizontal interpolation to the observation point.
      !!
      !! ** Method  : Horizontal interpolation to the observation point using
      !!              model values at the corners of the surrounding grid 
      !!              points.
      !!
      !! ** Action  :
      !!                   
      !! References : 
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-03 (A. Vidard) NEMOVAR migration
      !!        ! 06-10 (A. Weaver) Cleanup
      !!        ! 07-08 (K. Mogensen) Split in two routines for easier adj.
      !!-----------------------------------------------------------------------
      !! * Modules used
      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kpk,   &             ! Parameter values for automatic arrays
         & kpk2
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pweig                ! Interpolation weights
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pmod                 ! Model variable to interpolate
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) ::  &
         & pobsk                ! Model profile interpolated to obs (i,j) pt

      !! * Local declarations
      INTEGER :: &
         & jk
      INTEGER :: &
         & ikmax
      REAL(KIND=wp) :: &
         & zsum
      !------------------------------------------------------------------------
      ! Initialize number of levels
      !------------------------------------------------------------------------
      IF ( kpk2 == 1 ) THEN
         ikmax = 1
      ELSEIF ( kpk2 == kpk) THEN
         ikmax = kpk-1
      ENDIF
      !------------------------------------------------------------------------
      ! Interpolate to the observation point
      !------------------------------------------------------------------------
      pobsk(:) = obfillflt
      DO jk = 1, ikmax
         zsum = pweig(1,1,jk) + pweig(1,2,jk) + pweig(2,1,jk) +  pweig(2,2,jk) 
         IF ( zsum /= 0.0_wp ) THEN
            pobsk(jk) = (  pweig(1,1,jk) * pmod(1,1,jk)         &
               &         + pweig(1,2,jk) * pmod(1,2,jk)         &
               &         + pweig(2,1,jk) * pmod(2,1,jk)         &
               &         + pweig(2,2,jk) * pmod(2,2,jk)         &
               &                                             ) / zsum
         ENDIF
      END DO

   END SUBROUTINE obs_int_h2d
  
   SUBROUTINE obs_int_h2d_ds1( kpk2, kmax,                        &
      &                        pphi, plam, pmask,                 &
      &                        pphimm, plammm, pphimp, plammp,    & 
      &                        pphipm, plampm, pphipp, plampp,    &
      &                        p2dmm, p2dmp, p2dpm, p2dpp )
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d_ds1  ***
      !!
      !! ** Purpose : Distance-weighted interpolation scheme (k2dint = 0)
      !!
      !! ** Method  : The interpolation weights are computed as a weighted 
      !!              sum of the distance between the model grid points (A) 
      !!              and the observation point (B). 
      !!
      !!    Distance (s) is computed using the great-circle distance formula:
      !!
      !!    s(AB) = arcos(   sin( phiA ) x sin( phiB )
      !!                   + cos( phiA ) x cos( phiB ) x cos( lamB - lamA )
      !!
      !! ** Action  :
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------

      !! * Modules used

      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kpk2, &             ! Parameter values for automatic arrays
         & kmax
      REAL(KIND=wp), INTENT(IN) :: &
         & pphi,   &           ! Geographical location of observation
         & plam,   &
         & pphimm, &           ! Geographical location of surrounding
         & pphimp, &           ! model grid points
         & pphipm, &
         & pphipp, & 
         & plammm, &
         & plammp, &
         & plampm, &
         & plampp
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pmask               ! Model variable mask 
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) :: &
         & p2dmm, &            ! Interpolation weights
         & p2dmp, &
         & p2dpm, &
         & p2dpp

      !! * Local declarations
      INTEGER :: &
         & jk
      REAL(KIND=wp) :: &
         & zphi2,   &
         & zlam2,   &
         & zcola,   &
         & za2,     &
         & zb2,     &
         & zc2,     &
         & zphimm2, &
         & zphimp2, &
         & zphipm2, &
         & zphipp2, &
         & zlammm2, &
         & zlammp2, &
         & zlampm2, &
         & zlampp2, &
         & za1mm,   &
         & za1mp,   &
         & za1pm,   &
         & za1pp,   &
         & zcomm,   &
         & zcomp,   &
         & zcopm,   &
         & zcopp,   &
         & zb1mm,   &
         & zb1mp,   &
         & zb1pm,   &
         & zb1pp,   &
         & zc1mm,   &
         & zc1mp,   &
         & zc1pm,   &
         & zc1pp,   &
         & zsopmpp, &
         & zsommmp, &
         & zsomm,   &
         & zsomp,   &
         & zsopm,   &
         & zsopp
       
      !------------------------------------------------------------------------
      ! Distance-weighted interpolation using the great circle formula
      !------------------------------------------------------------------------
      zphi2 = pphi * rad
      zlam2 = plam * rad
      zcola = COS( zphi2 )
      za2   = SIN( zphi2 )
      zb2   = zcola * COS( zlam2 )
      zc2   = zcola * SIN( zlam2 )
       
      zphimm2 = pphimm * rad
      zphimp2 = pphimp * rad
      zphipm2 = pphipm * rad
      zphipp2 = pphipp * rad
       
      zlammm2 = plammm * rad
      zlammp2 = plammp * rad
      zlampm2 = plampm * rad
      zlampp2 = plampp * rad
       
      za1mm = SIN( zphimm2 )
      za1mp = SIN( zphimp2 )
      za1pm = SIN( zphipm2 )
      za1pp = SIN( zphipp2 )
       
      zcomm = COS( zphimm2 )
      zcomp = COS( zphimp2 )
      zcopm = COS( zphipm2 )
      zcopp = COS( zphipp2 )
       
      zb1mm = zcomm * COS( zlammm2 )
      zb1mp = zcomp * COS( zlammp2 )
      zb1pm = zcopm * COS( zlampm2 )
      zb1pp = zcopp * COS( zlampp2 )
       
      zc1mm = zcomm * SIN( zlammm2 )
      zc1mp = zcomp * SIN( zlammp2 )
      zc1pm = zcopm * SIN( zlampm2 )
      zc1pp = zcopp * SIN( zlampp2 )
       
      ! Function for arcsin(sqrt(1-x^2) version of great-circle formula
      zsomm = grt_cir_dis( za1mm, za2, zb1mm, zb2, zc1mm, zc2 )
      zsomp = grt_cir_dis( za1mp, za2, zb1mp, zb2, zc1mp, zc2 )
      zsopm = grt_cir_dis( za1pm, za2, zb1pm, zb2, zc1pm, zc2 )
      zsopp = grt_cir_dis( za1pp, za2, zb1pp, zb2, zc1pp, zc2 )
       
      zsopmpp = zsopm * zsopp
      zsommmp = zsomm * zsomp
      DO jk = 1, kmax
         p2dmm(jk) = zsomp * zsopmpp * pmask(1,1,jk)
         p2dmp(jk) = zsomm * zsopmpp * pmask(1,2,jk) 
         p2dpm(jk) = zsopp * zsommmp * pmask(2,1,jk)
         p2dpp(jk) = zsopm * zsommmp * pmask(2,2,jk)
      END DO
       
   END SUBROUTINE obs_int_h2d_ds1
  
   SUBROUTINE obs_int_h2d_ds2( kpk2, kmax,                        &
      &                        pphi, plam, pmask,                 &
      &                        pphimm, plammm, pphimp, plammp,    &
      &                        pphipm, plampm, pphipp, plampp,    &
      &                        p2dmm, p2dmp, p2dpm, p2dpp ) 
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d_ds2  ***
      !!
      !! ** Purpose : Distance-weighted interpolation scheme (k2dint = 1)
      !!
      !! ** Method  : As k2dint = 0 but with distance (ds) computed using a 
      !!              small-angle approximation to the great-circle distance 
      !!              formula:
      !!
      !!    ds(AB) = sqrt(   ( phiB - phiA )^{2}
      !!                   + ( ( lamB - lamA ) * cos( phiB ) )^{2} )
      !!
      !! ** Action  :
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------

      !!-----------------------------------------------------------------------
      !! * Modules used
      !!----------------------------------------------------------------------- 
      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kpk2, &             ! Parameter values for automatic arrays
         & kmax
      REAL(KIND=wp), INTENT(IN) ::        &
         & pphi,   &           ! Geographical location of observation
         & plam,   &
         & pphimm, &           ! Geographical location of surrounding
         & pphimp, &           ! model grid points
         & pphipm, &
         & pphipp, & 
         & plammm, &
         & plammp, &
         & plampm, &
         & plampp
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pmask               ! Model variable mask 
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) :: &
         & p2dmm, &            ! Interpolation weights
         & p2dmp, &
         & p2dpm, &
         & p2dpp

      !! * Local declarations
      INTEGER :: &
         & jk
      REAL(KIND=wp) :: &
         & zcosp,   &
         & zdlmm,   &
         & zdlmp,   &
         & zdlpm,   &
         & zdlpp,   &
         & zdpmm,   &
         & zdpmp,   &
         & zdppm,   &
         & zdppp,   &
         & zsomm,   &
         & zsomp,   &
         & zsopm,   &
         & zsopp,   &
         & zsopmpp, &
         & zsommmp
       
      !------------------------------------------------------------------------
      ! Distance-weighted interpolation with a small angle approximation
      !------------------------------------------------------------------------
      zcosp = COS( pphi * rad )
       
      zdlmm = plammm - plam
      zdlmp = plammp - plam
      zdlpm = plampm - plam
      zdlpp = plampp - plam
       
      zdpmm = pphimm - pphi
      zdpmp = pphimp - pphi
      zdppm = pphipm - pphi
      zdppp = pphipp - pphi
       
      zsomm = grt_cir_dis_saa( zdlmm, zdpmm, zcosp )
      zsomp = grt_cir_dis_saa( zdlmp, zdpmp, zcosp )
      zsopm = grt_cir_dis_saa( zdlpm, zdppm, zcosp )
      zsopp = grt_cir_dis_saa( zdlpp, zdppp, zcosp )
       
      zsopmpp = zsopm * zsopp
      zsommmp = zsomm * zsomp
       
      DO jk = 1, kmax
         p2dmm(jk) = zsomp * zsopmpp * pmask(1,1,jk)
         p2dmp(jk) = zsomm * zsopmpp * pmask(1,2,jk)
         p2dpm(jk) = zsopp * zsommmp * pmask(2,1,jk)
         p2dpp(jk) = zsopm * zsommmp * pmask(2,2,jk)
      END DO
       
   END SUBROUTINE obs_int_h2d_ds2
  
   SUBROUTINE obs_int_h2d_bil( kpk2, kmax,                        &
      &                        pphi, plam, pmask,                 &
      &                        plammp, pphipm, pphipp, plampp,    &
      &                        p2dmm, p2dmp, p2dpm, p2dpp)
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d_bil  ***
      !!
      !! ** Purpose : Bilinear interpolation on a geographical grid (k2dint = 2)
      !!
      !! ** Method  : The interpolation is split into two 1D interpolations in 
      !!              the longitude and latitude directions, respectively.
      !!
      !!    An iterative scheme that involves first mapping a quadrilateral
      !!    cell into a cell with coordinates (0,0), (1,0), (0,1) and (1,1).
      !!
      !! ** Action  :
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------

      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kpk2, &             ! Parameter values for automatic arrays
         & kmax
      REAL(KIND=wp), INTENT(IN) :: &
         & pphi,   &           ! Geographical location of observation
         & plam,   &
         & pphipm, &           ! Geographical location of surrounding
         & pphipp, &           ! model grid points
         & plammp, &
         & plampp
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN)  :: &
         & pmask               ! Model variable mask
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) :: &
         & p2dmm, &            ! Interpolation weights
         & p2dmp, &
         & p2dpm, &
         & p2dpp

      !! * Local declarations
      INTEGER :: &
         & jk
      REAL(KIND=wp) :: &
         & zdlmp, &
         & zdppm, &
         & zdlpp, &
         & zdppp
       
      !----------------------------------------------------------------------
      ! Bilinear interpolation for geographical grid
      !----------------------------------------------------------------------
      zdlmp = ABS(plam   - plammp) 
      zdppm = ABS(pphi   - pphipm)
      zdlpp = ABS(plampp - plam)
      zdppp = ABS(pphipp - pphi)
       
      DO jk = 1, kmax
         p2dmm(jk) = zdlpp * zdppp * pmask(1,1,jk)
         p2dmp(jk) = zdlpp * zdppm * pmask(1,2,jk)
         p2dpm(jk) = zdlmp * zdppp * pmask(2,1,jk)
         p2dpp(jk) = zdlmp * zdppm * pmask(2,2,jk)
      END DO
       
   END SUBROUTINE obs_int_h2d_bil
  
   SUBROUTINE obs_int_h2d_bir( kpk2, kmax,                        &
      &                        pphi, plam, pmask,                 &
      &                        pphimm, plammm, pphimp, plammp,    &
      &                        pphipm, plampm, pphipp, plampp,    &
      &                        p2dmm, p2dmp, p2dpm, p2dpp, ldfail )
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d_bir  ***
      !!
      !! ** Purpose : General bilinear remapping interpolation (k2dint = 3)
      !!
      !! ** Method  : An iterative scheme that involves first mapping a 
      !!              quadrilateral cell into a cell with coordinates 
      !!              (0,0), (1,0), (0,1) and (1,1).
      !!
      !! ** Action  :
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------

      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kpk2, &             ! Parameter values for automatic arrays
         & kmax
      REAL(KIND=wp), INTENT(IN) ::        &
         & pphi,   &           ! Geographical location of observation
         & plam,   &
         & pphimm, &           ! Geographical location of surrounding
         & pphimp, &           ! model grid points
         & pphipm, &
         & pphipp, & 
         & plammm, &
         & plammp, &
         & plampm, &
         & plampp
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pmask               ! Model variable mask 
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) :: &
         & p2dmm, &            ! Interpolation weights
         & p2dmp, &
         & p2dpm, &
         & p2dpp
      LOGICAL, INTENT(OUT) :: &
         & ldfail
      !! * Local declarations
      INTEGER :: &
         & jk
      REAL(KIND=wp) :: &
         & zbiwmm, &
         & zbiwmp, &
         & zbiwpm, &
         & zbiwpp
  
      !----------------------------------------------------------------------
      ! Bilinear remapping interpolation for general quadrilateral grid
      !----------------------------------------------------------------------
      CALL bil_wgt( pphimm, pphimp, pphipm, pphipp,  &
         &          plammm, plammp, plampm, plampp,  &
         &          zbiwmm, zbiwmp, zbiwpm, zbiwpp,  &
         &          pphi  , plam,   ldfail           )

      IF ( .NOT.ldfail ) THEN
         DO jk = 1, kmax
            p2dmm(jk) = zbiwmm * pmask(1,1,jk)
            p2dmp(jk) = zbiwmp * pmask(1,2,jk)
            p2dpm(jk) = zbiwpm * pmask(2,1,jk)
            p2dpp(jk) = zbiwpp * pmask(2,2,jk)
         END DO
      ENDIF

   END SUBROUTINE obs_int_h2d_bir

   SUBROUTINE obs_int_h2d_pol( kpk2, kmax,                         &
      &                        pphi, plam, pmask,                  &
      &                        pphimm, plammm, pphimp, plammp,     &
      &                        pphipm, plampm, pphipp, plampp,     &
      &                        p2dmm, p2dmp, p2dpm, p2dpp )
      !!-----------------------------------------------------------------------
      !!
      !!                     ***  ROUTINE obs_int_h2d_pol  ***
      !!
      !! ** Purpose : Polynomial interpolation (k2dint = 4)
      !!
      !! ** Method  : The interpolation weights are computed by fitting a 
      !!              polynomial function of the form
      !!
      !!    P(i) = a1(i) + a2(i) * phi + a3(i) * plam + a4(i) * phi * plam
      !!
      !!    through the model values at four surrounding grid pts (i=1,4).
      !!    As k2dint = 0 but with distance (ds) computed using a small-
      !!    angle approximation to the great-circle distance formula:
      !!
      !!    ds(AB) = sqrt(   ( phiB - phiA )^{2}
      !!                   + ( ( lamB - lamA ) * cos( phiB ) )^{2} )
      !!
      !! ** Action  :
      !!
      !! History :
      !!        ! 97-11 (A. Weaver, N. Daget)
      !!        ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------

      !! * Arguments
      INTEGER, INTENT(IN) :: &  
         & kpk2,   &           ! Parameter values for automatic arrays 
         & kmax
      REAL(KIND=wp), INTENT(IN) :: &
         & pphi,   &           ! Geographical location of observation
         & plam,   &
         & pphimm, &           ! Geographical location of surrounding
         & pphimp, &           ! model grid points
         & pphipm, &
         & pphipp, &
         & plammm, &
         & plammp, &
         & plampm, &
         & plampp
      REAL(KIND=wp), DIMENSION(2,2,kpk2), INTENT(IN) :: &
         & pmask               ! Model variable mask
      REAL(KIND=wp), DIMENSION(kpk2), INTENT(OUT) :: &
         & p2dmm,  &           ! Interpolation weights
         & p2dmp,  &
         & p2dpm,  &
         & p2dpp
  
      !! * Local declarations
      INTEGER :: &
         & jk
      REAL(KIND=wp) :: &
         & zplp
      REAL(KIND=wp), DIMENSION(4,4) :: &
         & zmat,  &
         & zmati
       
      !------------------------------------------------------------------------
      ! Polynomial interpolation
      !------------------------------------------------------------------------       
      zmat(1,1) = 1.0_wp
      zmat(1,2) = 1.0_wp
      zmat(1,3) = 1.0_wp
      zmat(1,4) = 1.0_wp
      zmat(2,1) = plammm
      zmat(2,2) = plammp
      zmat(2,3) = plampm
      zmat(2,4) = plampp
      zmat(3,1) = pphimm
      zmat(3,2) = pphimp
      zmat(3,3) = pphipm
      zmat(3,4) = pphipp
      zmat(4,1) = plammm * pphimm
      zmat(4,2) = plammp * pphimp
      zmat(4,3) = plampm * pphipm
      zmat(4,4) = plampp * pphipp
       
      CALL lu_invmat( zmat, 4, zmati )
       
      zplp = plam * pphi            
      DO jk = 1, kmax
         p2dmm(jk) = ABS(   zmati(1,1)        + zmati(1,2) * plam    &
            &             + zmati(1,3) * pphi + zmati(1,4) * zplp )  &
            &      * pmask(1,1,jk) 
         p2dmp(jk) = ABS(   zmati(2,1)        + zmati(2,2) * plam    &
            &             + zmati(2,3) * pphi + zmati(2,4) * zplp )  &
            &      * pmask(1,2,jk)
         p2dpm(jk) = ABS(   zmati(3,1)        + zmati(3,2) * plam    &
            &             + zmati(3,3) * pphi + zmati(3,4) * zplp )  &
            &      * pmask(2,1,jk)
         p2dpp(jk) = ABS(   zmati(4,1)        + zmati(4,2) * plam    &
            &             + zmati(4,3) * pphi + zmati(4,4) * zplp )  &
            &      * pmask(2,2,jk)
      END DO
  
   END SUBROUTINE obs_int_h2d_pol

   SUBROUTINE bil_wgt( pphimm, pphimp, pphipm, pphipp,   &
      &               plammm, plammp, plampm, plampp,   &
      &               pbiwmm, pbiwmp, pbiwpm, pbiwpp,   &
      &               pphi  , plam,   ldfail            )
      !!-------------------------------------------------------------------
      !!
      !!                   ***  ROUTINE bil_wgt  ***
      !!
      !! ** Purpose : Compute the weights for a bilinear remapping 
      !!              interpolation scheme.
      !!
      !! ** Method  : This scheme is appropriate for bilinear interpolation 
      !!              on a general quadrilateral grid.    
      !!              This scheme is also used in OASIS.
      !!
      !!              This routine is a derivative of the SCRIP software.
      !!              Copyright 1997, 1998 the Regents of the University 
      !!              of California. See SCRIP_Copyright.txt.
      !!     
      !! ** Action  :
      !!
      !! References : Jones, P.: A user's guide for SCRIP: A Spherical 
      !!                Coordinate Remapping and Interpolation Package. 
      !!                Version 1.4. Los Alamos. 
      !!                  
      !!         http://www.acl.lanl.gov/climate/software/SCRIP/SCRIPmain.html
      !!
      !! History
      !!      ! 97-11 (A. Weaver, N. Daget)
      !!      ! 06-03 (A. Vidard)
      !!      ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------
 
      !! * Arguments
      REAL(KIND=wp), INTENT(IN) :: &
         & pphi,   &           ! Geographical location of observation
         & plam,   &
         & pphimm, &           ! Geographical location of surrounding
         & pphimp, &           ! model grid points
         & pphipm, &
         & pphipp, &
         & plammm, &
         & plammp, & 
         & plampm, &
         & plampp
      REAL(KIND=wp), INTENT(OUT) :: &
         & pbiwmm, &           ! Interpolation weights
         & pbiwmp, &
         & pbiwpm, &
         & pbiwpp
      LOGICAL, INTENT(out) :: &
         & ldfail

      !! * Local declarations
      INTEGER :: &
         & jiter
      INTEGER :: &
         & itermax
      REAL(KIND=wp) :: &
         & zphi,    &           ! Geographical location of observation
         & zlam,    &
         & zphimm,  &           ! Geographical location of surrounding
         & zphimp,  &           ! model grid points
         & zphipm,  &
         & zphipp,  &
         & zlammm,  &
         & zlammp,  & 
         & zlampm,  &
         & zlampp,  &
         & zdth1,   &
         & zdth2,   &
         & zdth3,   &
         & zdthp,   &
         & zdph1,   &
         & zdph2,   &
         & zdph3,   &
         & zdphp,   &
         & zmat1,   &
         & zmat2,   &
         & zmat3,   &
         & zmat4,   &
         & zdeli,   &
         & zdelj,   &
         & ziguess, &
         & zjguess, &
         & zeps,    &
         & zdeterm, &
         & z2pi,    &
         & zhpi
      
      ! Initialization

      ! Conversion to radians

      zphi   = pphi   * rad
      zlam   = plam   * rad
      zphimm = pphimm * rad
      zphimp = pphimp * rad
      zphipm = pphipm * rad
      zphipp = pphipp * rad
      zlammm = plammm * rad
      zlammp = plammp * rad
      zlampm = plampm * rad
      zlampp = plampp * rad

      ldfail = .FALSE.

      zdth1 = zphipm - zphimm
      zdth2 = zphimp - zphimm
      zdth3 = zphipp - zphipm - zdth2
       
      zdph1 = zlampm - zlammm
      zdph2 = zlammp - zlammm
      zdph3 = zlampp - zlampm
       
      z2pi = 2.0_wp * rpi
       
      IF ( zdph1 >  3.0_wp * rpi ) zdph1 = zdph1 - z2pi
      IF ( zdph2 >  3.0_wp * rpi ) zdph2 = zdph2 - z2pi
      IF ( zdph3 >  3.0_wp * rpi ) zdph3 = zdph3 - z2pi
      IF ( zdph1 < -3.0_wp * rpi ) zdph1 = zdph1 + z2pi
      IF ( zdph2 < -3.0_wp * rpi ) zdph2 = zdph2 + z2pi
      IF ( zdph3 < -3.0_wp * rpi ) zdph3 = zdph3 + z2pi
       
      zdph3 = zdph3 - zdph2
       
      ziguess = 0.5_wp
      zjguess = 0.5_wp
       
      itermax = 100

      IF ( wp == sp ) THEN
         zeps = 1.0e-6_wp	! Single precision
      ELSE
         zeps = 1.0e-10_wp      ! Double precision
      ENDIF
  
      !------------------------------------------------------------------------
      ! Iterate to determine (i,j) in new coordinate system
      !------------------------------------------------------------------------
      jiter_loop: DO jiter = 1, itermax
         
         zdthp = zphi - zphimm - zdth1 * ziguess - zdth2 * zjguess &
            &  - zdth3 * ziguess * zjguess
         zdphp = zlam - zlammm
         
         zhpi = 0.5_wp * rpi
         IF ( zdphp >  3.0_wp * zhpi ) zdphp = zdphp - z2pi
         IF ( zdphp < -3.0_wp * zhpi ) zdphp = zdphp + z2pi
         
         zdphp = zdphp - zdph1 * ziguess - zdph2 * zjguess &
            &  - zdph3 * ziguess * zjguess
         
         zmat1 = zdth1 + zdth3 * zjguess
         zmat2 = zdth2 + zdth3 * ziguess
         zmat3 = zdph1 + zdph3 * zjguess
         zmat4 = zdph2 + zdph3 * ziguess
         
         ! Matrix determinant
         zdeterm = zmat1 * zmat4 - zmat2 * zmat3
         
         zdeli = ( zdthp * zmat4 - zmat2 * zdphp) / zdeterm
         zdelj = ( zmat1 * zdphp - zdthp * zmat3) / zdeterm
         
         IF ( ABS( zdeli ) < zeps .AND. ABS( zdelj ) < zeps ) EXIT jiter_loop
         
         ziguess = ziguess + zdeli
         zjguess = zjguess + zdelj

         ! DJL prevent ziguess and zjguess from going outside the range
         ! 0 to 1
         ! prevents interpolated value going wrong
         ! for example sea ice concentration gt 1
         
         IF ( ziguess < 0 ) ziguess = 0.0_wp
         IF ( zjguess < 0 ) zjguess = 0.0_wp
         IF ( ziguess > 1 ) ziguess = 1.0_wp
         IF ( zjguess > 1 ) zjguess = 1.0_wp
         
      END DO jiter_loop
       
      IF ( jiter <= itermax ) THEN
           
         ! Successfully found i,j, now compute the weights

         pbiwmm = ( 1.0_wp - ziguess ) * ( 1.0_wp - zjguess )
         pbiwmp = ( 1.0_wp - ziguess ) *            zjguess
         pbiwpm =            ziguess   * ( 1.0_wp - zjguess )
         pbiwpp =            ziguess   *            zjguess
         
      ELSEIF ( jiter > itermax ) THEN
            
         IF(lwp) THEN
            
            WRITE(numout,*)'Obs lat/lon  : ',pphi, plam
            WRITE(numout,*)'Grid lats    : ',pphimm, pphimp, pphipm, pphipp
            WRITE(numout,*)'Grid lons    : ',plammm, plammp, plampm, plampp
            WRITE(numout,*)'Current i,j  : ',ziguess, zjguess
            WRITE(numout,*)'jiter        = ',jiter
            WRITE(numout,*)'zeps         = ',zeps
            WRITE(numout,*)'zdeli, zdelj = ',zdeli, zdelj
            WRITE(numout,*)' Iterations for i,j exceed max iteration count!'
            WRITE(numout,*)
            
            ldfail = .TRUE.

         ENDIF
         
      ENDIF
    
   END SUBROUTINE bil_wgt

   SUBROUTINE lu_invmat( pmatin, kdim, pmatou )
      !!-----------------------------------------------------------------------
      !!
      !!                   ***  ROUTINE lu_invmat  ***
      !!
      !! ** Purpose : Invert a matrix using LU decomposition.
      !!
      !! ** Method  : 
      !! 
      !! ** Action  :
      !!
      !! References : 
      !!
      !! History
      !!      ! 02-11 (A. Weaver, N. Daget)
      !!      ! 06-03 (A. Vidard)
      !!      ! 06-10 (A. Weaver) Cleanup
      !!      ! 06-11 (NEMOVAR task force) Fix declaration of zd.
      !!-----------------------------------------------------------------------

      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kdim             ! Array dimension
      REAL(KIND=wp), DIMENSION(kdim,kdim), INTENT(IN) :: &
         & pmatin 
      REAL(KIND=wp), DIMENSION(kdim,kdim), INTENT(OUT) :: &
         & pmatou 
  
      !! * Local declarations
      INTEGER :: &
         & ji, &
         & jj
      INTEGER, DIMENSION(kdim) :: &
         & indx
      REAL(KIND=wp), DIMENSION(kdim,kdim) :: &
         & zmat
      REAL(KIND=wp) :: &
         & zd

      ! Invert the matrix       
      DO jj = 1, kdim
         DO ji = 1, kdim
            pmatou(ji,jj) = 0.0_wp
            zmat(ji,jj) = pmatin(ji,jj)
         END DO
         pmatou(jj,jj) = 1.0_wp
      END DO
      CALL lu_decomp( zmat, kdim, kdim, indx, zd )
      DO jj = 1, kdim
         CALL lu_backsb( zmat, kdim, kdim, indx, pmatou(1,jj) )
      END DO
      
   END SUBROUTINE lu_invmat

   SUBROUTINE lu_decomp( pmatin, kdim1, kdim2, kindex, pflt )
      !!-----------------------------------------------------------------------
      !!
      !!                   ***  ROUTINE lu_decomp  ***
      !!
      !! ** Purpose : Compute the LU decomposition of a matrix
      !!
      !! ** Method  : 
      !! 
      !! ** Action  :
      !!
      !! References : 
      !!
      !! History
      !!      ! 02-11 (A. Weaver, N. Daget)
      !!      ! 06-03 (A. Vidard)
      !!      ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------
       
      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kdim1, &   ! Array dimensions
         & kdim2
      INTEGER, DIMENSION(kdim1), INTENT(OUT) :: &
         & kindex
      REAL(KIND=wp), INTENT(OUT) :: &
         & pflt
      REAL(KIND=wp), DIMENSION(kdim2,kdim2), INTENT(INOUT) :: &
         & pmatin 
  
      !! * Local declarations
      INTEGER, PARAMETER :: &
         & jpmax = 100
      REAL(KIND=wp), PARAMETER :: &
         & pptiny = 1.0e-20_wp
      REAL(KIND=wp), DIMENSION(jpmax) :: &
         & zvv
      INTEGER :: &
         & ji, &
         & jj, &
         & jk
      INTEGER :: &
         & imax
      REAL(KIND=wp) :: &
         & zsum,  &
         & zdum,  &
         & zaamax
      
      imax = -1 
      ! Main computation
      pflt = 1.0_wp
      DO ji = 1, kdim1
         zaamax = 0.0_wp
         DO jj = 1, kdim1
            IF ( ABS( pmatin(ji,jj) ) > zaamax ) zaamax = ABS( pmatin(ji,jj) )
         END DO
         IF ( zaamax == 0.0_wp ) THEN
            CALL ctl_stop( 'singular matrix' )
         ENDIF 
         zvv(ji) = 1.0_wp / zaamax
      END DO
      DO jj = 1, kdim1
         DO ji = 1, jj-1
            zsum = pmatin(ji,jj)
            DO jk = 1, ji-1
	       zsum = zsum - pmatin(ji,jk) * pmatin(jk,jj)
	    END DO
            pmatin(ji,jj) = zsum
         END DO
         zaamax = 0.0_wp
         DO ji = jj, kdim1
            zsum = pmatin(ji,jj)
            DO jk = 1, jj-1
               zsum = zsum - pmatin(ji,jk) * pmatin(jk,jj)
            END DO
            pmatin(ji,jj) = zsum
            zdum = zvv(ji) * ABS( zsum )
            IF ( zdum >= zaamax ) THEN
               imax = ji
               zaamax = zdum
            ENDIF
	 END DO
         IF ( jj /= imax ) THEN
            DO jk = 1, kdim1
               zdum = pmatin(imax,jk)
               pmatin(imax,jk) = pmatin(jj,jk)
               pmatin(jj,jk) = zdum
	    END DO
            pflt = -pflt
            zvv(imax) = zvv(jj)
         ENDIF
         kindex(jj) = imax
         IF ( pmatin(jj,jj) == 0.0_wp ) pmatin(jj,jj) = pptiny
         IF ( jj /= kdim1 ) THEN
            zdum = 1.0_wp / pmatin(jj,jj)
            DO ji = jj+1, kdim1
               pmatin(ji,jj) = pmatin(ji,jj) * zdum
	    END DO
         ENDIF
      END DO
     
   END SUBROUTINE lu_decomp
     
   SUBROUTINE lu_backsb( pmat, kdim1, kdim2, kindex, pvect )
      !!-----------------------------------------------------------------------
      !!
      !!                   ***  ROUTINE lu_backsb  ***
      !!
      !! ** Purpose : Back substitution
      !!
      !! ** Method  : 
      !! 
      !! ** Action  :
      !!
      !! References : 
      !!
      !! History
      !!      ! 02-11 (A. Weaver, N. Daget)
      !!      ! 06-03 (A. Vidard)
      !!      ! 06-10 (A. Weaver) Cleanup
      !!-----------------------------------------------------------------------
      
      !! * Arguments
      INTEGER, INTENT(IN) :: &
         & kdim1, &     ! Array dimensions
         & kdim2
      INTEGER, DIMENSION(kdim1), INTENT(IN) :: &
         & kindex
      REAL(KIND=wp), DIMENSION(kdim1), INTENT(INOUT) :: &
         & pvect
      REAL(KIND=wp), DIMENSION(kdim2,kdim2), INTENT(IN) :: &
         & pmat
  
      !! * Local declarations
      INTEGER :: &
         & ji,  &
         & jii, &
         & jj,  &
         & jll
      REAL(KIND=wp) :: &
         & zsum

      ! Main computation
      jii = 0
      DO ji = 1, kdim1
         jll = kindex(ji)
         zsum = pvect(jll)
         pvect(jll) = pvect(ji)
         IF ( jii /= 0 ) THEN
            DO jj = jii, ji-1
               zsum = zsum - pmat(ji,jj) * pvect(jj)
            END DO
         ELSEIF ( zsum /= 0.0_wp ) THEN
            jii = ji
         ENDIF
         pvect(ji) = zsum
      END DO
      DO ji = kdim1, 1, -1
         zsum = pvect(ji)
         DO jj = ji+1, kdim1
            zsum = zsum - pmat(ji,jj) * pvect(jj)
         END DO
         pvect(ji) = zsum / pmat(ji,ji)
      END DO
     
   END SUBROUTINE lu_backsb

END MODULE obs_inter_h2d
