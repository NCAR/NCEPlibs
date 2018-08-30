 module interp_utils
!$$$  module documentation block
!
! module:    interp_utils
!   prgmmr: gayno         org: w/np2     date: 2005-05-20
!
! abstract: collection of routines that interpolate
!           data on a lat/lon grid to a regional or global grid.
!
! program history log:
!   2005-05-20  gayno   - initial version
!
! usage: use interp_utils
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  ibm sp
!
!$$$

 integer, parameter, private         :: missing = -9999

 contains

 subroutine interp_aavg_bgrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                   isrc, dx_src, dy_src, lat_11_src, &
                                   lon_11_src, centlat_mdl, &
                                   centlon_mdl, lat_11_mdl, lon_11_mdl, dx_mdl, dy_mdl, &
                                   istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                   imdl, jmdl, nearest_i, nearest_j)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_bgrid_prep
!   prgmmr: gayno          org: w/np2     date: 2007-08-21
!
! abstract:  for all points on a given source data grid, find the 
!            nearest i/j on the rotated lat/lon bgrid.  this routine assumes the
!            source grid is a global lat/lon projection. 
!
! program history log:
! 2007-08-21  gayno    - initial version
!
! usage: call interp_aavg_bgrid_prep(istart_src, iend_src, jstart_src, jend_src, &
!                                    isrc, dx_src, dy_src, lat_11_src, &
!                                    lon_11_src, centlat_mdl, &
!                                    centlon_mdl, lat11_mdl, lon11_mdl, dx_mdl, dy_mdl, &
!                                    istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
!                                    imdl, jmdl, nearest_i, nearest_j)
!
!   input argument list:
!     centlat/lon_mdl - central latitude/longitude of model grid
!     dx_mdl, dy_mdl  - the n/s and e/w resolution of the model grid in degrees
!     dx_src, dy_src  - the n/s and e/w resolution of the source grid in degrees
!     i/jmdl          - i/j dimension of model grid
!     istart/iend_mdl - starting/ending 'i' index on the model grid
!     jstart/jend_mdl - starting/ending 'j' index on the model grid
!     istart/iend_src - starting/ending 'i' index on the source grid
!     jstart/jend_src - starting/ending 'j' index on the source grid
!     isrc            - 'i' dimension of the source grid
!     lat_11_mdl      - latitude of point (1,1) on model grid
!     lon_11_mdl      - longitude of point (1,1) on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!
!   output argument list:
!     nearest_i/j     - holds the nearest model i/j index for all
!                       source data points
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use ll2xy_utils, only            : ll2xy_bgrid_pt

 implicit none

 integer                         :: i, ii, j
 integer, intent(in)             :: imdl, jmdl
 integer, intent(in)             :: istart_mdl, iend_mdl, jstart_mdl, jend_mdl
 integer, intent(in)             :: isrc, istart_src, iend_src, jstart_src, jend_src
 integer                         :: near_i, near_j
 integer*2, intent(out)          :: nearest_i(istart_src:iend_src,jstart_src:jend_src)
 integer*2, intent(out)          :: nearest_j(istart_src:iend_src,jstart_src:jend_src)

 real, intent(in)                :: centlat_mdl, centlon_mdl
 real, intent(in)                :: dx_mdl, dy_mdl
 real, intent(in)                :: dx_src, dy_src
 real, intent(in)                :: lat_11_mdl, lon_11_mdl
 real, intent(in)                :: lat_11_src, lon_11_src
 real                            :: srclat, srclon, x, y

 nearest_i = missing
 nearest_j = missing

 do j = jstart_src, jend_src
   do i = istart_src, iend_src

      ii = i
      if (ii < 1) ii = isrc - i
      if (ii > isrc) ii = i - isrc
      srclat = lat_11_src + (j-1)*dy_src
      srclon = lon_11_src + (ii-1)*dx_src

      call ll2xy_bgrid_pt(centlat_mdl, centlon_mdl, dy_mdl, dx_mdl, &
                          lat_11_mdl, lon_11_mdl, imdl, jmdl, srclat, srclon, near_i, near_j)

      if (near_i >= istart_mdl .and. near_i <= iend_mdl .and. &
          near_j >= jstart_mdl .and. near_j <= jend_mdl) then
        nearest_i(i,j) = near_i
        nearest_j(i,j) = near_j
      end if

   enddo
 enddo

 return

 end subroutine interp_aavg_bgrid_prep

 subroutine interp_aavg_egrid_prep(istart_src, iend_src, jstart_src, jend_src, &
                                   isrc, dx_src, dy_src, lat_11_src, &
                                   lon_11_src, centlat_mdl, &
                                   centlon_mdl, dx_mdl, dy_mdl, &
                                   istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                                   imdl, jmdl, nearest_i, nearest_j)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_egrid_prep
!   prgmmr: gayno          org: w/np2     date: 2007-06-28
!
! abstract:  for all points on a given source data grid, find the 
!            nearest i/j on the egrid.  this routine assumes the
!            source grid is a global lat/lon projection. 
!
! program history log:
! 2007-06-28  gayno    - initial version
!
! usage: call interp_aavg_egrid_prep(istart_src, iend_src, jstart_src, jend_src, &
!                                    isrc, dx_src, dy_src, lat_11_src, &
!                                    lon_11_src, centlat_mdl, &
!                                    centlon_mdl, dx_mdl, dy_mdl, &
!                                    istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
!                                    imdl, jmdl, nearest_i, nearest_j)
!
!   input argument list:
!     centlat/lon_mdl - central latitude/longitude of model grid
!     dx_mdl, dy_mdl  - the n/s and e/w resolution of the model grid in degrees
!     dx_src, dy_src  - the n/s and e/w resolution of the source grid in degrees
!     i/jmdl          - i/j dimension of model grid
!     istart/iend_mdl - starting/ending 'i' index on the model grid
!     jstart/jend_mdl - starting/ending 'j' index on the model grid
!     istart/iend_src - starting/ending 'i' index on the source grid
!     jstart/jend_src - starting/ending 'j' index on the source grid
!     isrc            - 'i' dimension of the source grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!
!   output argument list:
!     nearest_i/j     - holds the nearest model i/j index for all
!                       source data points
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 use ll2xy_utils, only            : ll2xy_egrid_pt

 implicit none

 integer                         :: i, ii, j
 integer, intent(in)             :: imdl, jmdl
 integer, intent(in)             :: istart_mdl, iend_mdl, jstart_mdl, jend_mdl
 integer, intent(in)             :: isrc, istart_src, iend_src, jstart_src, jend_src
 integer                         :: near_i, near_j
 integer*2, intent(out)          :: nearest_i(istart_src:iend_src,jstart_src:jend_src)
 integer*2, intent(out)          :: nearest_j(istart_src:iend_src,jstart_src:jend_src)

 real, intent(in)                :: centlat_mdl, centlon_mdl
 real, intent(in)                :: dx_mdl, dy_mdl
 real, intent(in)                :: dx_src, dy_src
 real, intent(in)                :: lat_11_src, lon_11_src
 real                            :: srclat, srclon

 nearest_i = missing
 nearest_j = missing

 do j = jstart_src, jend_src
   do i = istart_src, iend_src

      ii = i
      if (ii < 1) ii = isrc - i
      if (ii > isrc) ii = i - isrc
      srclat = lat_11_src + (j-1)*dy_src
      srclon = lon_11_src + (ii-1)*dx_src

      call ll2xy_egrid_pt(srclat, srclon, imdl, jmdl, &
                          centlat_mdl, centlon_mdl,   &
                         -(dx_mdl), dy_mdl, near_i, near_j)
                 
      if (near_i >= istart_mdl .and. near_i <= iend_mdl .and. &
          near_j >= jstart_mdl .and. near_j <= jend_mdl) then
        nearest_i(i,j) = near_i
        nearest_j(i,j) = near_j
      end if

   enddo
 enddo

 return

 end subroutine interp_aavg_egrid_prep
 subroutine interp_aavg_egrid(imdl, jmdl, lat_mdl, lon_mdl, &
                        dx_mdl, dy_mdl, lsmask_mdl, &
                        dx_src, dy_src, lat_11_src, lon_11_src, &
                        centlat_mdl, centlon_mdl,   &
                        default_value, undef_value,  &
                        data_mdl, data_src, isrc, jsrc)

 use ll2xy_utils, only              : ll2xy_egrid

 implicit none

 integer                           :: count(imdl,jmdl)
 integer                           :: i, j, ii, jj, iii, jjj
 integer                           :: iend, istart, jend, jstart
 integer                           :: igrid
 integer, intent(in)               :: imdl
 integer, intent(in)               :: isrc
 integer                           :: jgrid
 integer, intent(in)               :: jmdl
 integer, intent(in)               :: jsrc
 integer                           :: jsrc_start, jsrc_end
 integer                           :: krad
 integer                           :: nearest_i(1), nearest_j(1)
 integer                           :: spiral_rad
 integer                           :: test1, test2

 real, intent(in)                  :: centlat_mdl
 real, intent(in)                  :: centlon_mdl
 real, intent(in)                  :: data_src    (isrc,jsrc)
 real, intent(out)                 :: data_mdl    (imdl,jmdl)
 real, intent(in)                  :: default_value
 real, intent(in)                  :: dx_mdl, dy_mdl 
 real, intent(in)                  :: dx_src, dy_src 
 real                              :: gridi
 real                              :: gridj
 real, intent(in)                  :: lat_11_src, lon_11_src
 real, intent(in)                  :: lat_mdl  (imdl, jmdl)
 real, intent(in)                  :: lon_mdl  (imdl, jmdl)
 real, intent(in)                  :: lsmask_mdl  (imdl, jmdl)
 real                              :: srclat(1), srclon(1)
 real                              :: sum(imdl,jmdl)
 real                              :: test
 real, intent(in)                  :: undef_value

!----------------------------------------------------------------------
! routine assumes source data is on a global lat/lon grid.
!
! from the max and min values of latitude on the model grid, 
! determine the corresponding "j" bounds on the source grid.
!---------------------------------------------------------------------- 

 test  = (maxval(lat_mdl) + dy_mdl - lat_11_src) / dy_src + 1.0
 test1 = nint(test)
 if (test1 < 1)    test1 = 1
 if (test1 > jsrc) test1 = jsrc 
 test  = (minval(lat_mdl) - dy_mdl - lat_11_src) / dy_src + 1.0
 test2 = nint(test)
 if (test2 < 1)    test2 = 1
 if (test2 > jsrc) test2 = jsrc 

 jsrc_start = min(test1,test2)
 jsrc_end   = max(test1,test2)

 count = 0
 sum   = 0.0

 do j = jsrc_start, jsrc_end
   do i = 1, isrc

      if (data_src(i,j) /= undef_value) then

        srclat(1) = lat_11_src + (j-1)*dy_src
        srclon(1) = lon_11_src + (i-1)*dx_src

        call ll2xy_egrid(srclat, srclon, imdl, jmdl, &
                         centlat_mdl, centlon_mdl,   &
                         -dx_mdl, dy_mdl, &
                          1, 1, nearest_i, nearest_j)

        if (nearest_i(1) >= 1 .and. nearest_i(1) <= imdl .and. &
            nearest_j(1) >= 1 .and. nearest_j(1) <= jmdl) then
          count(nearest_i(1),nearest_j(1)) = count(nearest_i(1),nearest_j(1)) + 1
          sum(nearest_i(1),nearest_j(1)) = sum(nearest_i(1),nearest_j(1)) + &
                                           data_src(i,j)

        end if

      end if

   enddo
 enddo

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

     LAND_TEST : if (lsmask_mdl(i,j) > 0.0) then

!---------------------------------------------------------------------- 
!      there were valid source data within the model grid box.
!---------------------------------------------------------------------- 

       VALID_COUNT : if (count(i,j) > 0) then
 
         data_mdl(i,j) = sum(i,j) / float(count(i,j))

!---------------------------------------------------------------------- 
!      there were no valid source data within the model grid box,
!      do a spiral search to find a valid value.
!---------------------------------------------------------------------- 

       else

         gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
         gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

         jgrid = nint (gridj)
         igrid = nint (gridi)

         if (jgrid > jsrc) then
           jgrid = jsrc
         elseif (jgrid < 1) then
           jgrid = 1
         end if

         if (igrid > isrc) then
           igrid = igrid - isrc
         else if (igrid < 1) then
           igrid = igrid + isrc
         end if

         spiral_rad = nint(5.0 / dx_src)

         SPIRAL_SEARCH : do krad = 1, spiral_rad

           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jsrc)) then

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc
                 else
                   iii = ii
                 end if

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = data_src(iii,jjj)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo
           enddo

       enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!      the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

       data_mdl(i,j) = default_value
       write (6, 6100) i, j

       end if VALID_COUNT

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg_egrid

 subroutine interp_aavg_nam (istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
                              lat_mdl, lon_mdl, lsmask, &
                              dx_src, dy_src, lat_11_src, lon_11_src, &
                              default_value, undef_value,  &
                              scaling_fac, data_mdl, data_src,  &
                              isrc, istart_src, iend_src, jstart_src, jend_src, &
                              nearest_i, nearest_j)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_nam
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to an nam by taking the
!            area average of the source data.  routine 
!            interp_aavg_egrid_prep or interp_aavg_bgrid_prep 
!            must be called first.
!            assumes source grid is a global lat/lon projection.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-06-28  gayno    - modified to work for mpi
!
! usage: call interp_aavg_nam (istart_mdl, iend_mdl, jstart_mdl, jend_mdl, &
!                               lat_mdl, lon_mdl, lsmask, &
!                               dx_src, dy_src, lat_11_src, lon_11_src, &
!                               default_value, undef_value,  &
!                               scaling_fac, data_mdl, data_src,  &
!                               isrc, istart_src, iend_src, jstart_src, jend_src, &
!                               nearest_i, nearest_j)
!
!   input argument list:
!     data_src        - data on source grid (scaled integer)
!     default_value   - flag value for model points for which source
!                       was not found
!     dx/dy_src       - x/y direction resolution of source data in deg
!     istart/end_mdl  - model grid i-dimension bounds for this task
!     jstart/end_mdl  - model grid j-dimension bounds for this task
!     isrc            - source grid i-dimension
!     istart/end_src  - source grid i-dimension bounds for this task
!     jstart/end_src  - source grid j-dimension bounds for this task
!     lat_mdl         - latitudes on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!     lon_mdl         - longitudes on model grid
!     lsmask          - model land/sea mask
!     nearest_i/j     - holds the i/j index of the nearest model
!                       grid point for all source data points
!     scaling_fac     - source data scaling factor
!     undef_value     - flag value indicating source data point is
!                       non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl        - data interpolated to the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                           :: count(istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 integer*2, intent(in)             :: data_src (isrc,jstart_src:jend_src)
 integer                           :: i, j, ii, jj, iii, jjj
 integer                           :: iend, istart, jend, jstart
 integer                           :: igrid
 integer*4, intent(in)             :: isrc
 integer                           :: jgrid
 integer, intent(in)               :: istart_mdl, iend_mdl, jstart_mdl, jend_mdl
 integer, intent(in)               :: istart_src, iend_src, jstart_src, jend_src
 integer                           :: krad
 integer*2, intent(in)             :: nearest_i(istart_src:iend_src,jstart_src:jend_src)
 integer*2, intent(in)             :: nearest_j(istart_src:iend_src,jstart_src:jend_src)
 integer*4 , intent(in)            :: scaling_fac
 integer                           :: spiral_rad
 integer*8                         :: sum      (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 integer*2, intent(in)             :: undef_value

 real, intent(out)                 :: data_mdl    (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)                  :: default_value
 real*8, intent(in)                :: dx_src, dy_src 
 real                              :: gridi
 real                              :: gridj
 real*8, intent(in)                :: lat_11_src, lon_11_src
 real, intent(in)                  :: lat_mdl  (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)                  :: lon_mdl  (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)                  :: lsmask   (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)

!----------------------------------------------------------------------
! routine assumes source data is on a global lat/lon grid.
!---------------------------------------------------------------------- 

 count = 0
 sum   = 0.0

 do j = jstart_src, jend_src
   do i = istart_src, iend_src
     iii = i
     if (iii < 1) iii = isrc - iii
     if (iii > isrc) iii = iii - isrc
     if (nearest_i(i,j) /= missing .and. nearest_j(i,j) /= missing .and. &
         data_src(iii,j) /= undef_value) then
       count(nearest_i(i,j),nearest_j(i,j)) = count(nearest_i(i,j),nearest_j(i,j)) + 1
       sum(nearest_i(i,j),nearest_j(i,j)) = sum(nearest_i(i,j),nearest_j(i,j)) + &
                                            data_src(iii,j)
     end if
   enddo
 enddo

 data_mdl = 0.0

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl

   if (lsmask(i,j) == 0.0) cycle ILOOP

!---------------------------------------------------------------------- 
!  there were valid source data within the model grid box.
!---------------------------------------------------------------------- 

   VALID_COUNT : if (count(i,j) > 0) then
 
     data_mdl(i,j) = float(sum(i,j)) / (float(count(i,j)) * float(scaling_fac))

!---------------------------------------------------------------------- 
!    there were no valid source data within the model grid box,
!    do a spiral search to find a valid value.
!---------------------------------------------------------------------- 

   else

     gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
     gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

     jgrid = nint (gridj)
     igrid = nint (gridi)

     if (jgrid > jend_src) then
       print*,'widen band: ',i,j,lat_mdl(i,j),lon_mdl(i,j),jgrid
       stop
     elseif (jgrid < jstart_src) then
       print*,'widen band: ',i,j,lat_mdl(i,j),lon_mdl(i,j),jgrid
       stop
     end if

     if (igrid > isrc) then
       igrid = igrid - isrc
     else if (igrid < 1) then
       igrid = igrid + isrc
     end if

     spiral_rad = nint(5.0 / dx_src)

     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = igrid - krad
       iend   = igrid + krad
       jstart = jgrid - krad
       jend   = jgrid + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
             (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!          ensure that point being investigated is within
!          the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

           if ((jj .ge. jstart_src) .and. (jj .le. jend_src)) then

             jjj = jj

!-----------------------------------------------------------------------
!            adjust i-index on source grid when search
!            crosses the date line.
!-----------------------------------------------------------------------

             if (ii .le. 0) then
               iii = isrc + ii
             else if (ii .ge. (isrc+1)) then
               iii = ii - isrc
             else
               iii = ii
             end if

!-----------------------------------------------------------------------
!            a valid value was found.
!-----------------------------------------------------------------------

             if (data_src(iii,jjj) /= undef_value) then
               data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
               write (6, 6000) i,j, krad
               cycle ILOOP
             end if

           end if

         end if

       enddo
       enddo

     enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!    the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

     data_mdl(i,j) = default_value
     write (6, 6100) i, j

   end if VALID_COUNT

 enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg_nam

 subroutine interp_aavg_gaus(istart_mdl, iend_mdl, iend_mdl_4_loops, &
                             jstart_mdl, jend_mdl, &
                             lat_mdl, lon_mdl, lsmask,  &
                             dx_mdl, dy_mdl, &
                             dx_src, dy_src, lat_11_src, lon_11_src,  &
                             default_value, undef_value, scaling_fac,  &
                             data_mdl, data_src, isrc, &
                             jsrc_start, jend_src)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg_gaus
!   prgmmr: gayno          org: w/np2     date: 2007-06-12
!
! abstract:  interpolate data to the GFS gaussian grid by taking
!   an area average of the source data.  
!
! program history log:
! 2007-06-12  gayno    - initial version
!
! usage: call interp_aavg_gaus(istart_mdl, iend_mdl, iend_mdl_4_loops, &
!                              jstart_mdl, jend_mdl, &
!                              lat_mdl, lon_mdl, lsmask,  &
!                              dx_mdl, dy_mdl, &
!                              dx_src, dy_src, lat_11_src, lon_11_src,  &
!                              default_value, undef_value, scaling_fac,  &
!                              data_mdl, data_src, isrc, &
!                              jsrc_start, jend_src)
!
!   input argument list:
!     data_src       - data on source grid (scaled integer)
!     default_value  - flag value for model points for which source
!                      data was not found
!     dx/dy_mdl      - x/y direction resolution of model grid in deg.
!     dx/dy_src      - x/y direction resolution of source data in deg.
!     istart/end_mdl - model grid i-dimension bounds for this task
!     iend_mdl_4_loops - array of upper bound of model grid i-dimension.
!                        used for gfs where i-dim decreases toward poles
!     jstart/end_mdl - model grid j-dimension bounds for this task
!     isrc           - source grid i-dimension
!     jstart/end_src - j dimension bounds of source data
!     lat_mdl        - latitudes on model grid
!     lat_11_src     - latitude of point (1,1) on source grid
!     lon_11_src     - longitude of point (1,1) on source grid
!     lon_mdl        - longitudes on model grid
!     lsmask         - model land/sea mask
!     scaling_fac    - source data scaling factor
!     undef_value    - flag value indicating source data point is
!                      not defined and to be ingored during interpolation
!
!   output argument list:
!     data_mdl       - data interpolated to the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer*2, intent(in)    :: data_src (isrc,jsrc_start:jend_src)
 integer                  :: iend, iend_save, istart, istart_save
 integer                  :: igrid, jgrid
 integer                  :: i, ii, iii, j, jj, jjj
 integer, intent(in)      :: istart_mdl, iend_mdl, iend_mdl_4_loops(jstart_mdl:jend_mdl)
 integer, intent(in)      :: jstart_mdl, jend_mdl
 integer, intent(in)      :: isrc
 integer, intent(in)      :: jsrc_start, jend_src
 integer                  :: jend, jend_save, jstart, jstart_save
 integer                  :: krad
 integer*4, intent(in)    :: scaling_fac
 integer                  :: spiral_rad
 integer*2, intent(in)    :: undef_value

 real                     :: count
 real, intent(in)         :: dx_mdl(jstart_mdl:jend_mdl), dy_mdl
 real, intent(out)        :: data_mdl (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: default_value
 real*8, intent(in)       :: dx_src, dy_src
 real                     :: gridj, gridi
 real, allocatable        :: i_fraction(:), j_fraction(:)
 real                     :: istart_fraction, iend_fraction
 real                     :: jstart_fraction, jend_fraction
 real, intent(in)         :: lat_mdl (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real*8, intent(in)       :: lat_11_src, lon_11_src
 real, intent(in)         :: lon_mdl (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: lsmask  (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real                     :: sum

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 data_mdl = 0.0

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

   if (lsmask(i,j) == 0.0) cycle ILOOP

!-----------------------------------------------------------------------
!  find the i/j bounds of the model grid box with respect to the source
!  grid.  account for the fact that some source grid points may be
!  only partially within the model grid box.  this becomes less
!  important as the resolution of the source data increases with
!  respect to the model resolution.
!-----------------------------------------------------------------------

   gridi           = ( (lon_mdl(i,j)-0.5*dx_mdl(j)) - lon_11_src) / dx_src + 1.0
   istart          = nint(gridi)
   istart_fraction = 0.5 - (gridi - float(istart))

   gridi         = ( (lon_mdl(i,j)+0.5*dx_mdl(j)) - lon_11_src) / dx_src + 1.0
   iend          = nint(gridi)
   iend_fraction = 0.5 + (gridi - float(iend)) 

   allocate (i_fraction(istart:iend))
   i_fraction         = 1.0
   i_fraction(istart) = istart_fraction
   i_fraction(iend)   = iend_fraction

   if (dy_src > 0) then
     gridj = ( (lat_mdl(i,j)-0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   else
     gridj = ( (lat_mdl(i,j)+0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   end if

   jstart          = nint(gridj)
   jstart_fraction = 0.5 - (gridj - float(jstart))

   if (dy_src > 0) then
     gridj = ( (lat_mdl(i,j)+0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   else
     gridj = ( (lat_mdl(i,j)-0.5*dy_mdl) - lat_11_src) / dy_src + 1.0
   endif

   jend          = nint(gridj)
   jend_fraction = 0.5 + (gridj - float(jend))

   allocate (j_fraction(jstart:jend))
   j_fraction         = 1.0
   j_fraction(jstart) = jstart_fraction
   j_fraction(jend)   = jend_fraction

!-----------------------------------------------------------------------
!  this should not occur if you ensure the model points fall within the
!  latitude band of the source grid data.
!-----------------------------------------------------------------------

   if (jend > jend_src) then
     print*,'widen band'
     stop
   elseif (jstart < jsrc_start) then
     print*,'widen band'
     stop
   end if

!-----------------------------------------------------------------------
!  take an area average.
!-----------------------------------------------------------------------

   count = 0.0
   sum   = 0.0

   do ii = istart, iend
       iii = ii
       if (iii .ge. (isrc+1)) iii = iii - isrc
       if (iii .lt. 1)        iii = iii + isrc
     do jj = jstart, jend
       if (data_src(iii,jj) /= undef_value) then
         count = count + (i_fraction(ii) * j_fraction(jj))
         sum = sum + ( float(data_src(iii,jj)) * &
                      i_fraction(ii) * j_fraction(jj) )
       end if
     enddo
   enddo

   deallocate (i_fraction, j_fraction)

   VALID_COUNT : if ( count .gt. 0.0 ) then

     data_mdl(i,j) = sum / (float(scaling_fac)* count)

   else

!-----------------------------------------------------------------------
!    source data is undefined at this point, do a spiral
!    search for valid value.
!-----------------------------------------------------------------------
  
     istart_save = istart
     iend_save   = iend
     jstart_save = jstart
     jend_save   = jend

     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = istart_save - krad
       iend   = iend_save + krad
       jstart = jstart_save - krad
       jend   = jend_save + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
             (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!          ensure that point being investigated is within
!          the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

           if ((jj .ge. jsrc_start) .and. (jj .le. jend_src)) then

             jjj = jj

!-----------------------------------------------------------------------
!            adjust i-index on source grid when search
!            crosses the date line.
!-----------------------------------------------------------------------

             if (ii .le. 0) then
               iii = isrc + ii
             else if (ii .ge. (isrc+1)) then
               iii = ii - isrc
             else
               iii = ii
             end if

!-----------------------------------------------------------------------
!            a valid value was found.
!-----------------------------------------------------------------------

             if (data_src(iii,jjj) /= undef_value) then
               data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
               write (6, 6000) i, j, krad
               cycle ILOOP
             end if

           end if

         end if

       enddo
       enddo

     enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!  the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

     data_mdl(i,j) = default_value
     write (6, 6100) i, j

   end if VALID_COUNT

 enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg_gaus

 subroutine interp_nn(istart_mdl, iend_mdl, iend_mdl_4_loops, &
                      jstart_mdl, jend_mdl, &
                      lat_mdl, lon_mdl, lsmask,  &
                      dx_src, dy_src, lat_11_src, lon_11_src, &
                      default_value, undef_value, scaling_fac,  &
                      data_mdl, data_src, isrc, jstart_src, jend_src) 
!$$$  subprogram documentation block
!
! subprogram:   interp_nn
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by taking the
!   nearest neighbor.
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-06-28  gayno    - modified to work with mpi
!
! usage: call interp_nn(istart_mdl, iend_mdl, iend_mdl_4_loops,  &
!                       jstart_mdl, jend_mdl, &
!                       lat_mdl, lon_mdl, lsmask,  &
!                       dx_src, dy_src, lat_11_src, lon_11_src, &
!                       default_value, undef_value, scaling_fac,  &
!                       data_mdl, data_src, isrc, jstart_src, jend_src) 
!
!   input argument list:
!     data_src        - data on source grid (scaled integer)
!     default_value   - flag value for model points for which source
!                       was not found
!     dx/dy_src       - x/y direction resolution of source data in deg
!     istart/end_mdl  - model grid i-dimension bounds for this task
!     iend_mdl_4_loops - array of upper bound of model grid i-dimension.
!                        used for gfs where i-dim decreases toward poles
!     jstart/end_mdl  - model grid j-dimension bounds for this task
!     isrc            - source grid i-dimension
!     jstart_src/end  - source grid j-dimension bounds
!     lat_mdl         - latitudes on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!     lon_mdl         - longitudes on model grid
!     lsmask          - model land/sea mask 
!     scaling_fac     - source data scaling factor
!                       averaging will occur 
!     undef_value     - flag value indicating source data point is
!                       undefined and to be ingored during interpolation
!
!   output argument list:
!     data_mdl        - data interpolated to the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer*2, intent(in)    :: data_src    (isrc,jstart_src:jend_src)
 integer                  :: iend, istart
 integer, intent(in)      :: istart_mdl, iend_mdl, iend_mdl_4_loops(jstart_mdl:jend_mdl)
 integer, intent(in)      :: jstart_mdl, jend_mdl
 integer                  :: igrid, jgrid
 integer                  :: i,j, ii, iii, jj, jjj
 integer*4, intent(in)    :: isrc
 integer, intent(in)      :: jstart_src, jend_src
 integer                  :: jend, jstart
 integer                  :: krad
 integer*4, intent(in)    :: scaling_fac
 integer                  :: spiral_rad
 integer*2, intent(in)    :: undef_value

 real, intent(out)        :: data_mdl    (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: default_value
 real*8, intent(in)       :: dx_src, dy_src
 real                     :: gridj, gridi
 real, intent(in)         :: lat_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real*8, intent(in)       :: lat_11_src, lon_11_src
 real, intent(in)         :: lsmask      (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: lon_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = jstart_mdl, jend_mdl
 ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

   if (lsmask(i,j) == 0.0) cycle ILOOP

   gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
   gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

   jgrid = nint (gridj)
   igrid = nint (gridi)

   if (jgrid > jend_src) then
     print*,'widen band'
     stop
   elseif (jgrid < jstart_src) then
     print*,'widen band'
     stop
   end if

   if (igrid > isrc) then
     igrid = igrid - isrc
   else if (igrid < 1) then
     igrid = igrid + isrc
   end if

   if (data_src(igrid,jgrid) /= undef_value) then

     data_mdl(i,j) = float(data_src(igrid,jgrid))/float(scaling_fac)

   else

!-----------------------------------------------------------------------
!    source data is undefined at this point, do a spiral
!    search for valid value.
!-----------------------------------------------------------------------

     SPIRAL_SEARCH : do krad = 1, spiral_rad

       istart = igrid - krad
       iend   = igrid + krad
       jstart = jgrid - krad
       jend   = jgrid + krad

       do jj = jstart, jend
       do ii = istart, iend

!-----------------------------------------------------------------------
!        search only along outer square.
!-----------------------------------------------------------------------

         if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
             (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!           ensure that point being investigated is within
!           the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

           if ((jj .ge. jstart_src) .and. (jj .le. jend_src)) then

             jjj = jj

!-----------------------------------------------------------------------
!            adjust i-index on source grid when search
!            crosses the date line.
!-----------------------------------------------------------------------

             if (ii .le. 0) then
               iii = isrc + ii
             else if (ii .ge. (isrc+1)) then
               iii = ii - isrc
             else
               iii = ii
             end if

!-----------------------------------------------------------------------
!            a valid value was found.
!-----------------------------------------------------------------------

             if (data_src(iii,jjj) /= undef_value) then
               data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
               write (6, 6000) i, j, krad
               cycle ILOOP
             end if

           end if

         end if

       enddo
       enddo

     enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!    the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

     data_mdl(i,j) = default_value
     write (6, 6100) i,j

   endif

 enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_nn
 subroutine interp_aavg(imdl, jmdl, lat_mdl, lon_mdl, lsmask, &
                        dx_src, dy_src, lat_11_src, lon_11_src, search_rad, &
                        default_value, undef_value,  &
                        data_mdl, data_src, isrc, jsrc)
!$$$  subprogram documentation block
!
! subprogram:   interp_aavg
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by taking the
!   nearest neighbor or area average of the source data.  the
!   simple area averaging technique only works well for the
!   gaussian grid.  for the e-grid, use routine interp_aavg_egrid.
!
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call interp_aavg(imdl, jmdl, lat_mdl, lon_mdl, lsmask,
!                         dx_src, dy_src, lat_11_src, lon_11_src, search_rad,
!                         default_value, undef_value, 
!                         data_mdl, data_src, isrc, jsrc)
!
!   input argument list:
!     data_src       - data on source grid
!     default_value  - flag value for model points for which source
!                      was not found
!     dx/dy_src      - x/y direction resolution of source data in deg
!     imdl           - model grid i-dimension
!     isrc           - source grid i-dimension
!     jmdl           - model grid j-dimension
!     jsrc           - source grid j-dimension
!     lat_mdl        - latitudes on model grid
!     lat_11_src     - latitude of point (1,1) on source grid
!     lon_11_src     - longitude of point (1,1) on source grid
!     lon_mdl        - longitudes on model grid
!     lsmask         - land mask of model grid (0-nonland;>0 land)
!     search_rad     - radius in grid point over which the area
!                      averaging will occur 
!     undef_value    - flag value indicating source data point is
!                      non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl           - data on the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                  :: count
 integer                  :: iend, istart
 integer, intent(in)      :: imdl
 integer                  :: igrid, jgrid
 integer, intent(in)      :: jmdl
 integer                  :: i, ii, iii, j, jj, jjj
 integer, intent(in)      :: isrc, jsrc
 integer                  :: jend, jstart
 integer                  :: krad
 integer                  :: spiral_rad
 integer, intent(in)      :: search_rad

 real, intent(out)        :: data_mdl    (imdl,jmdl)
 real, intent(in)         :: data_src    (isrc,jsrc)
 real, intent(in)         :: default_value
 real, intent(in)         :: dx_src, dy_src
 real                     :: gridj, gridi
 real, intent(in)         :: lat_mdl     (imdl,jmdl)
 real, intent(in)         :: lat_11_src, lon_11_src
 real, intent(in)         :: lon_mdl     (imdl,jmdl)
 real, intent(in)         :: lsmask      (imdl,jmdl)
 real                     :: sum
 real, intent(in)         :: undef_value

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = 1, jmdl
   ILOOP : do i = 1, imdl

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
       gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jsrc) then
         jgrid = jsrc
       elseif (jgrid < 1) then
         jgrid = 1
       end if

       if (igrid > isrc) then
         igrid = igrid - isrc
       else if (igrid < 1) then
         igrid = igrid + isrc
       end if

!-----------------------------------------------------------------------
!      take an average based on the resolutions of both the model
!      and source grids (incorporated in variable search_rad).
!-----------------------------------------------------------------------

       count = 0
       sum   = 0.0

       do ii = (igrid-search_rad), (igrid+search_rad)
         do jj = (jgrid-search_rad), (jgrid+search_rad)

           if (jj >= 1 .and. jj <= jsrc) then

             iii = ii
             jjj = jj

             if (iii .ge. (isrc+1)) iii = iii - isrc
             if (iii .lt. 1)        iii = iii + isrc

             if (data_src(iii,jjj) /= undef_value) then
               count = count + 1
               sum = sum + data_src(iii,jjj)
             end if

           end if

         enddo
       enddo

       VALID_COUNT : if ( count .gt. 0 ) then

         data_mdl(i,j) = (sum / float(count))

       else

!-----------------------------------------------------------------------
!        source data is undefined at this point, do a spiral
!        search for valid value.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = (search_rad+1), spiral_rad

           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jsrc)) then

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc
                 else
                   iii = ii
                 end if

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = data_src(iii,jjj)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo
           enddo

       enddo SPIRAL_SEARCH

!-----------------------------------------------------------------------
!      the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

       data_mdl(i,j) = default_value
       write (6, 6100) i, j

       end if VALID_COUNT

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3)

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)

 end subroutine interp_aavg

 subroutine find_nn (imdl_input, jmdl_input, lsmask_input, &
                     imdl_output, jmdl_output, lsmask_output, & 
                     flag_value, grid_type, res_input, &
                     xpts, ypts, ipts, jpts)
!$$$  subprogram documentation block
!
! subprogram:   find_nn
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  for all land (water) points on the output grid,
!   find the nearest neighbor land (water) point on the input grid.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call find_nn (imdl_input, jmdl_input, lsmask_input, 
!                      imdl_output, jmdl_output, lsmask_output, 
!                      flag_value, grid_type, res_input, 
!                      xpts, ypts, ipts, jpts)
!
!   input argument list:
!     flag_value     - indicates search for nearest neighbor failed
!     grid_type      - flag indicating global or regional input grid
!     imdl_input     - i-dimension of model input grid
!     imdl_output    - i-dimension of model output grid
!     jmdl_input     - j-dimension of model input grid
!     jmdl_output    - j-dimension of model output grid
!     lsmask_input   - land mask of model input grid (0-nonland;>0-land)
!     lsmask_output  - land mask of model output grid (0-nonland;>0-land)
!     res_input      - resolution of input grid in degrees
!     x/ypnts        - x/y indices of output grid
!
!   output argument list:
!     ipts           - nearest neighbor point (i index) with
!                      respect to the input grid
!     jpts           - nearest neighbor point (j index) with
!                      respect to the input grid
!
! remarks: only used by utility that cold starts nmm from edasx.
!          want to get rid of this.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*6, intent(in)  :: grid_type

 integer, intent(in)      :: flag_value
 integer                  :: i, ii, iii, j, jj, jjj
 integer                  :: iend, istart
 integer, intent(in)      :: imdl_input
 integer, intent(in)      :: imdl_output
 integer                  :: igrid, jgrid
 integer, intent(out)     :: ipts          (imdl_output,jmdl_output)
 integer, intent(in)      :: jmdl_input
 integer, intent(in)      :: jmdl_output
 integer, intent(out)     :: jpts          (imdl_output,jmdl_output)
 integer                  :: jend, jstart
 integer                  :: krad
 integer                  :: spiral_rad

 real                     :: gridi
 real                     :: gridj
 real, intent(in)         :: lsmask_input  (imdl_input,jmdl_input)
 real, intent(in)         :: lsmask_output (imdl_output,jmdl_output)
 real, intent(in)         :: res_input
 real, intent(in)         :: xpts          (imdl_output,jmdl_output)
 real, intent(in)         :: ypts          (imdl_output,jmdl_output)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / res_input)

 JLOOP : do j = 1, jmdl_output
   ILOOP : do i = 1, imdl_output

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridi = xpts(i,j)
       gridj = ypts(i,j)

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jmdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID ", jmdl_input
           stop
         else
           jgrid = jmdl_input
         end if

       elseif (jgrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID "
           stop
         else
           jgrid = 1
         end if

       end if

       if (igrid > imdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE J BOUNDS OF INPUT GRID ", imdl_input
           stop
         else
           igrid = igrid - imdl_input
         end if

       else if (igrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE I BOUNDS OF INPUT GRID "
           stop
         else
           igrid = igrid + imdl_input
         end if

       end if

!-----------------------------------------------------------------------
!      input/output point of the same type.
!-----------------------------------------------------------------------

       if ( (lsmask_input(igrid,jgrid) > 0.0   .and.  &
             lsmask_output(i,j)        > 0.0)   .or.  &
            (lsmask_input(igrid,jgrid) == 0.0  .and.  &
             lsmask_output(i,j)        == 0.0) ) then

         ipts(i,j) = igrid
         jpts(i,j) = jgrid

       else

!-----------------------------------------------------------------------
!        search for point of same type.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           JJ_LOOP : do jj = jstart, jend
           II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jmdl_input)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                for global grids, adjust i-index on source grid
!                when search crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   if (grid_type == "global") then
                     iii = imdl_input + ii
                   else
                     cycle II_LOOP
                   end if
                 else if (ii .ge. (imdl_input+1)) then
                   if (grid_type == "global") then
                     iii = ii - imdl_input                  
                   else
                     cycle II_LOOP
                   end if
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                 a similar type is found.
!-----------------------------------------------------------------------

                  if ( (lsmask_input(iii,jjj) >  0.0    .and.  &
                        lsmask_output(i,j)    >  0.0)    .or.  &
                       (lsmask_input(iii,jjj) == 0.0    .and.  &
                        lsmask_output(i,j)    == 0.0) ) then
                   ipts(i,j) = iii
                   jpts(i,j) = jjj
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo II_LOOP
           enddo JJ_LOOP

       enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!      the sprial search failed.  therefore assign a flag value,
!      so rest of program can handle this point.
!-----------------------------------------------------------------------

       ipts(i,j) = flag_value
       jpts(i,j) = flag_value

       write (6, 6100) i, j
 
     end if

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I4) 
             
 6100 FORMAT (1X, '-- FLAG VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4)     
   
 end subroutine find_nn

 subroutine find_nn_1d (imdl_input, jmdl_input, lsmask_input, &
                        ijmdl_output, lsmask_output,          & 
                        flag_value, grid_type, res_input,     &
                        iindx_output, jindx_output,           &
                        xindx_wrt_input_grid,                 &
                        yindx_wrt_input_grid,                 &
                        nn_iindx_wrt_input_grid,              &
                        nn_jindx_wrt_input_grid)
!$$$  subprogram documentation block
!
! subprogram:   find_nn_1d
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  for all land (water) points on a grid,
!   find the nearest neighbor land (water) point on another grid.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call find_nn_1d(imdl_input, jmdl_input, lsmask_input, 
!                        ijmdl_output, lsmask_output,          
!                        flag_value, grid_type, res_input,     
!                        iindx_output, jindx_output,           
!                        xindx_wrt_input_grid,                 
!                        yindx_wrt_input_grid,                 
!                        nn_iindx_wrt_input_grid,              
!                        nn_jindx_wrt_input_grid)
!
!   input argument list:
!     flag_value             - indicates search for nearest neighbor failed
!     grid_type              - flag indicating global or regional input grid
!     imdl_input             - i-dimension of model input grid
!     ijmdl_output           - dimension of model output grid
!     i/jindx_output         - i/j index of output grid
!     jmdl_input             - j-dimension of model input grid
!     lsmask_input           - land mask of model input grid (0-nonland;>0-land)
!     lsmask_output          - land mask of model output grid (0-nonland;>0-land)
!     res_input              - resolution of input grid in degrees
!     x/yindx_wrt_input_grid - x/y indices with respect to input grid
!
!   output argument list:
!     nn_i/jindx_wrt_input_grid - nearest neighbor point (i/j index) with
!                                 the same land mask value
!
! remarks: want to keep this one.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*6, intent(in)  :: grid_type

 integer, intent(in)      :: flag_value
 integer                  :: ij, ii, iii, jj, jjj
 integer                  :: iend, istart
 integer                  :: igrid, jgrid
 integer, intent(in)      :: iindx_output (ijmdl_output)
 integer, intent(in)      :: ijmdl_output
 integer, intent(in)      :: imdl_input
 integer, intent(in)      :: jindx_output (ijmdl_output)
 integer, intent(in)      :: jmdl_input
 integer                  :: jend, jstart
 integer                  :: krad
 integer, intent(out)     :: nn_iindx_wrt_input_grid   (ijmdl_output)
 integer, intent(out)     :: nn_jindx_wrt_input_grid   (ijmdl_output)
 integer                  :: spiral_rad

 real                     :: gridi
 real                     :: gridj
 real, intent(in)         :: lsmask_input  (imdl_input,jmdl_input)
 real, intent(in)         :: lsmask_output (ijmdl_output)
 real, intent(in)         :: res_input
 real, intent(in)         :: xindx_wrt_input_grid      (ijmdl_output)
 real, intent(in)         :: yindx_wrt_input_grid      (ijmdl_output)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / res_input)

 MAIN : do ij = 1, ijmdl_output

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridi = xindx_wrt_input_grid(ij)
       gridj = yindx_wrt_input_grid(ij)

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jmdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID ", jmdl_input
           stop
         else
           jgrid = jmdl_input
         end if

       elseif (jgrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID "
           stop
         else
           jgrid = 1
         end if

       end if

       if (igrid > imdl_input) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE J BOUNDS OF INPUT GRID ", imdl_input
           stop
         else
           igrid = igrid - imdl_input
         end if

       else if (igrid < 1) then

         if (grid_type /= "global") then
           print*,"- OUTPUT POINT ", igrid, " OUTSIDE I BOUNDS OF INPUT GRID "
           stop
         else
           igrid = igrid + imdl_input
         end if

       end if

!-----------------------------------------------------------------------
!      input/output point of the same type.
!-----------------------------------------------------------------------

       if ( (lsmask_input(igrid,jgrid) > 0.0   .and.  &
             lsmask_output(ij)         > 0.0)   .or.  &
            (lsmask_input(igrid,jgrid) == 0.0  .and.  &
             lsmask_output(ij)         == 0.0) ) then

         nn_iindx_wrt_input_grid(ij) = igrid
         nn_jindx_wrt_input_grid(ij) = jgrid

       else

!-----------------------------------------------------------------------
!        search for point of same type.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           JJ_LOOP : do jj = jstart, jend
           II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jmdl_input)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                for global grids, adjust i-index on source grid
!                when search crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   if (grid_type == "global") then
                     iii = imdl_input + ii
                   else
                     cycle II_LOOP
                   end if
                 else if (ii .ge. (imdl_input+1)) then
                   if (grid_type == "global") then
                     iii = ii - imdl_input                  
                   else
                     cycle II_LOOP
                   end if
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                 a similar type is found.
!-----------------------------------------------------------------------

                  if ( (lsmask_input(iii,jjj) >  0.0    .and.  &
                        lsmask_output(ij)     >  0.0)    .or.  &
                       (lsmask_input(iii,jjj) == 0.0    .and.  &
                        lsmask_output(ij)     == 0.0) ) then
                   nn_iindx_wrt_input_grid(ij) = iii
                   nn_jindx_wrt_input_grid(ij) = jjj
                   write (6, 6000) iindx_output(ij), jindx_output(ij), krad
                   cycle MAIN
                 end if

               end if

             end if

           enddo II_LOOP
           enddo JJ_LOOP

       enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!      the sprial search failed.  therefore assign a flag value,
!      so rest of program can handle this point.
!-----------------------------------------------------------------------

       nn_iindx_wrt_input_grid(ij) = flag_value
       nn_jindx_wrt_input_grid(ij) = flag_value

       write (6, 6100) iindx_output(ij), jindx_output(ij)
 
     end if

 enddo MAIN

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X,'-- CIRCULAR SEARCH AT POINT ',I5,' ',I5,' ITERATIONS ',I4) 
             
 6100 FORMAT (1X,'-- FLAG VALUE ASSIGNED AT POINT ',I5,' ',I5)     
   
 end subroutine find_nn_1d

! new routine

 subroutine find_nn_new (imdl_input, jmdl_input, lsmask_input, &
                        ijmdl_output, lsmask_output,          & 
                        flag_value, grid_type, res_input,     &
                        merge,  &
                        iindx_output, jindx_output,           &
                        xindx_wrt_input_grid,                 &
                        yindx_wrt_input_grid,                 &
                        nn_iindx_wrt_input_grid,              &
                        nn_jindx_wrt_input_grid)
!$$$  subprogram documentation block
!
! subprogram:   find_nn_new
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  find nearest neighbor of same type between
!            two grids.
!   find the nearest neighbor land (water) point on another grid.
!   
! program history log:
! 2005-05-20  gayno    - initial version
!
! usage: call find_nn_new(imdl_input, jmdl_input, lsmask_input, 
!                        ijmdl_output, lsmask_output,          
!                        flag_value, grid_type, res_input, merge,
!                        iindx_output, jindx_output,           
!                        xindx_wrt_input_grid,                 
!                        yindx_wrt_input_grid,                 
!                        nn_iindx_wrt_input_grid,              
!                        nn_jindx_wrt_input_grid)
!
!   input argument list:
!     flag_value             - indicates search for nearest neighbor failed
!     grid_type              - flag indicating global or regional input grid
!     imdl_input             - i-dimension of model input grid
!     ijmdl_output           - dimension of model output grid
!     i/jindx_output         - i/j index of output grid
!     jmdl_input             - j-dimension of model input grid
!     lsmask_input           - land mask of model input grid (0-nonland;>0-land)
!     lsmask_output          - land mask of model output grid (0-nonland;>0-land)
!     merge                  - when true, will ignore points on output grid
!                              that are outside of input grid.  used when
!                              merging input data from two different sources
!                              such as a merger of nam and gfs land fields.
!     res_input              - resolution of input grid in degrees
!     x/yindx_wrt_input_grid - x/y indices with respect to input grid
!
!   output argument list:
!     nn_i/jindx_wrt_input_grid - nearest neighbor point (i/j index) with
!                                 the same land mask value
!
! remarks: want to keep this one.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 character*6, intent(in)  :: grid_type

 integer, intent(in)      :: flag_value
 integer                  :: ij, ii, iii, jj, jjj
 integer                  :: iend, istart
 integer                  :: igrid, jgrid
 integer, intent(in)      :: iindx_output (ijmdl_output)
 integer, intent(in)      :: ijmdl_output
 integer, intent(in)      :: imdl_input
 integer, intent(in)      :: jindx_output (ijmdl_output)
 integer, intent(in)      :: jmdl_input
 integer                  :: jend, jstart
 integer                  :: krad
 integer, intent(out)     :: nn_iindx_wrt_input_grid   (ijmdl_output)
 integer, intent(out)     :: nn_jindx_wrt_input_grid   (ijmdl_output)
 integer                  :: spiral_rad
 integer, intent(in)      :: lsmask_input  (imdl_input,jmdl_input)
 integer, intent(in)      :: lsmask_output (ijmdl_output)

 logical, intent(in)      :: merge

 real                     :: gridi
 real                     :: gridj
 real, intent(in)         :: res_input
 real, intent(in)         :: xindx_wrt_input_grid      (ijmdl_output)
 real, intent(in)         :: yindx_wrt_input_grid      (ijmdl_output)

!-----------------------------------------------------------------------
! set search radius for spiral search to be 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / res_input)

 MAIN : do ij = 1, ijmdl_output

!-----------------------------------------------------------------------
!      ensure we don't go off the source grid.
!-----------------------------------------------------------------------

       gridi = xindx_wrt_input_grid(ij)
       gridj = yindx_wrt_input_grid(ij)

       jgrid = nint (gridj)
       igrid = nint (gridi)

       if (jgrid > jmdl_input) then

         if (grid_type /= "global") then
           if (merge) then
             nn_iindx_wrt_input_grid(ij) = flag_value
             nn_jindx_wrt_input_grid(ij) = flag_value
             cycle MAIN
           else
             print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID ", jmdl_input
             stop
           endif
         else
           jgrid = jmdl_input
         end if

       elseif (jgrid < 1) then

         if (grid_type /= "global") then
           if (merge) then
             nn_iindx_wrt_input_grid(ij) = flag_value
             nn_jindx_wrt_input_grid(ij) = flag_value
             cycle MAIN
           else
             print*,"- OUTPUT POINT ", jgrid, " OUTSIDE J BOUNDS OF INPUT GRID "
             stop
           end if
         else
           jgrid = 1
         end if

       end if

       if (igrid > imdl_input) then

         if (grid_type /= "global") then
           if (merge) then
             nn_iindx_wrt_input_grid(ij) = flag_value
             nn_jindx_wrt_input_grid(ij) = flag_value
             cycle MAIN
           else
             print*,"- OUTPUT POINT ", igrid, " OUTSIDE J BOUNDS OF INPUT GRID ", imdl_input
             stop
           end if
         else
           igrid = igrid - imdl_input
         end if

       else if (igrid < 1) then

         if (grid_type /= "global") then
           if (merge) then
             nn_iindx_wrt_input_grid(ij) = flag_value
             nn_jindx_wrt_input_grid(ij) = flag_value
             cycle MAIN
           else
             print*,"- OUTPUT POINT ", igrid, " OUTSIDE I BOUNDS OF INPUT GRID "
             stop
           endif
         else
           igrid = igrid + imdl_input
         end if

       end if

!-----------------------------------------------------------------------
!      input/output point of the same type.
!-----------------------------------------------------------------------

       if ( lsmask_input(igrid,jgrid) == lsmask_output(ij) ) then

         nn_iindx_wrt_input_grid(ij) = igrid
         nn_jindx_wrt_input_grid(ij) = jgrid

       else

!-----------------------------------------------------------------------
!        search for point of same type.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = igrid - krad
           iend   = igrid + krad
           jstart = jgrid - krad
           jend   = jgrid + krad

           JJ_LOOP : do jj = jstart, jend
           II_LOOP : do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. 1) .and. (jj .le. jmdl_input)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                for global grids, adjust i-index on source grid
!                when search crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   if (grid_type == "global") then
                     iii = imdl_input + ii
                   else
                     cycle II_LOOP
                   end if
                 else if (ii .ge. (imdl_input+1)) then
                   if (grid_type == "global") then
                     iii = ii - imdl_input                  
                   else
                     cycle II_LOOP
                   end if
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                 a similar type is found.
!-----------------------------------------------------------------------

                 if ( lsmask_input(iii,jjj) == lsmask_output(ij) ) then
                   nn_iindx_wrt_input_grid(ij) = iii
                   nn_jindx_wrt_input_grid(ij) = jjj
                   write (6, 6000) iindx_output(ij), jindx_output(ij), krad
                   cycle MAIN
                 end if

               end if

             end if

           enddo II_LOOP
           enddo JJ_LOOP

       enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!      the sprial search failed.  therefore assign a flag value,
!      so rest of program can handle this point.
!-----------------------------------------------------------------------

       nn_iindx_wrt_input_grid(ij) = flag_value
       nn_jindx_wrt_input_grid(ij) = flag_value

       write (6, 6100) iindx_output(ij), jindx_output(ij)
 
     end if

 enddo MAIN

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X,'-- CIRCULAR SEARCH AT POINT ',I5,' ',I5,' ITERATIONS ',I4) 
             
 6100 FORMAT (1X,'-- FLAG VALUE ASSIGNED AT POINT ',I5,' ',I5)     
   
 end subroutine find_nn_new

 subroutine interp_bilinear (istart_mdl, iend_mdl, iend_mdl_4_loops, &
                             jstart_mdl, jend_mdl,  &
                             lat_mdl, lon_mdl, lsmask, & 
                             dx_src, dy_src, lat_11_src, lon_11_src, &
                             default_value, undef_value, scaling_fac, &
                             data_mdl, data_src, isrc, jstart_src, jend_src)
!$$$  subprogram documentation block
!
! subprogram:   interp_bilinear
!   prgmmr: gayno          org: w/np2     date: 2005-05-20
!
! abstract:  interpolate data to the model grid by 
!            bilinear interpolation
!
! program history log:
! 2005-05-20  gayno    - initial version
! 2007-06-28  gayno    - modified to work with mpi
!
! usage: call interp_bilinear (istart_mdl, iend_mdl, iend_mdl_4_loops, &
!                              jstart_mdl, jend_mdl,  &
!                              lat_mdl, lon_mdl, lsmask, & 
!                              dx_src, dy_src, lat_11_src, lon_11_src, &
!                              default_value, undef_value, scaling_fac, &
!                              data_mdl, data_src, isrc, jstart_src, jend_src)
!
!   input argument list:
!     data_src        - data on source grid (scaled integer)
!     default_value   - flag value for model points for which source
!                       was not found
!     dx/dy_src       - x/y direction resolution of source data in deg
!     istart/iend_mdl - model grid i-dimension bounds for this task
!     iend_mdl_4_loops - array of upper bound of model grid i-dimension.
!                        used for gfs where i-dim decreases toward poles
!     isrc            - source grid i-dimension
!     jstart/jend_mdl - model grid j-dimension bounds for this task
!     jstart/jend_src - source grid j-dimension
!     lat_mdl         - latitudes on model grid
!     lat_11_src      - latitude of point (1,1) on source grid
!     lon_11_src      - longitude of point (1,1) on source grid
!     lon_mdl         - longitudes on model grid
!     lsmask          - land mask of model grid (0-nonland;>0 land)
!     scaling_fac     - source data scaling factor
!     undef_value     - flag value indicating source data point is
!                      non-land and to be ingored during interpolation
!
!   output argument list:
!     data_mdl        - data interpolated to the model grid
!
! remarks: none.
!
! attributes:
!   language: fortran 90
!   machine:  IBM SP
!
!$$$

 implicit none

 integer                  :: iend, istart
 integer, intent(in)      :: istart_mdl, iend_mdl, iend_mdl_4_loops(jstart_mdl:jend_mdl)
 integer                  :: igrid, jgrid, nigrid, njgrid
 integer                  :: igridp1, jgridp1
 integer, intent(in)      :: jstart_mdl, jend_mdl
 integer                  :: i, ii, iii, j, jj, jjj
 integer*4, intent(in)    :: isrc, jstart_src, jend_src
 integer                  :: jend, jstart
 integer                  :: krad
 integer                  :: spiral_rad

 real, intent(out)        :: data_mdl    (istart_mdl:iend_mdl,jstart_mdl:jend_mdl) 
 integer*2, intent(in)    :: data_src    (isrc,jstart_src:jend_src)
 real, intent(in)         :: default_value
 real*8, intent(in)       :: dx_src, dy_src
 real, intent(in)         :: lat_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real, intent(in)         :: lon_mdl     (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real                     :: gridj, gridi
 real, intent(in)         :: lat_11_src, lon_11_src
 real, intent(in)         :: lsmask      (istart_mdl:iend_mdl,jstart_mdl:jend_mdl)
 real                     :: term1, term2
 integer*2, intent(in)    :: undef_value
 integer*4, intent(in)    :: scaling_fac
 real                     :: w1, w2

!-----------------------------------------------------------------------
! for spiral searchs, use a generous radius of 5 degrees.
!-----------------------------------------------------------------------

 spiral_rad = nint(5.0 / dx_src)

 JLOOP : do j = jstart_mdl, jend_mdl
   ILOOP : do i = istart_mdl, iend_mdl_4_loops(j)

!-----------------------------------------------------------------------
!    interpolate at land points.
!-----------------------------------------------------------------------

     LAND_TEST : if (lsmask(i,j) > 0.0) then

!-----------------------------------------------------------------------
!      find corresponding "i" and "j" indices on the source grid. 
!      account for cases when you are the poles/dateline.
!-----------------------------------------------------------------------

       gridj = (lat_mdl(i,j) - lat_11_src) / dy_src + 1.0
       jgrid  = int (gridj)

       if (jgrid == jend_src) then
         jgridp1 = jend_src
         njgrid  = jend_src
         w2      = 1
       elseif (jgrid == 0) then
         jgrid   = 1
         jgridp1 = 1
         njgrid  = 1
         w2      = 0
       elseif (jgrid > jend_src) then
         print*,'widen band',lat_mdl(i,j),gridj,jgrid,jend_src
         stop    
       elseif (jgrid < 0) then
         print*,'widen band',lat_mdl(i,j),gridj,jgrid,jstart_src
         stop
       else
         jgridp1 = jgrid + 1
         njgrid  = nint(gridj)
         w2      = mod(gridj,1.0)
       end if

       gridi = (lon_mdl(i,j) - lon_11_src) / dx_src + 1.0
       if (gridi > isrc) gridi = gridi - isrc
       if (gridi < 1.0)  gridi = gridi + isrc 

       w1 = mod(gridi,1.0)

       igrid  = int (gridi)
       if (igrid > isrc) igrid = igrid - isrc
       if (igrid < 1)    igrid = igrid + isrc

       igridp1 = igrid + 1
       if (igridp1 > isrc) igridp1 = igridp1 - isrc

       nigrid = nint(gridi)
       if (nigrid > isrc) nigrid = nigrid - isrc
       if (nigrid < 1)    nigrid = nigrid + isrc

!-----------------------------------------------------------------------
!      the four surrounding points have valid values.  do a bilinear
!      interpolation.
!-----------------------------------------------------------------------

       if ((data_src(igrid,jgrid)     /= undef_value)  .and. &
           (data_src(igridp1,jgrid)   /= undef_value)  .and. &
           (data_src(igrid,jgridp1)   /= undef_value)  .and. &
           (data_src(igridp1,jgridp1) /= undef_value) ) then

         term1 = ( (1.0 - w1) *  float(data_src(igrid,jgrid))/float(scaling_fac) ) +  &
                   w1 * float(data_src(igridp1,jgrid))/float(scaling_fac)
         term2 = ( (1.0 - w1) * float(data_src(igrid,jgridp1))/float(scaling_fac) ) + &
                   w1 * float(data_src(igridp1,jgridp1))/float(scaling_fac) 

         data_mdl(i,j) = ((1.0 - w2) * term1) + &
                           w2 * term2

!-----------------------------------------------------------------------
!      all four surrounding points do not have valid values.  pick
!      the nearest neighbor if valid.
!-----------------------------------------------------------------------

       elseif (data_src(nigrid,njgrid) /= undef_value) then

         data_mdl(i,j) = float(data_src(nigrid,njgrid))/float(scaling_fac)

       else

!-----------------------------------------------------------------------
!        source data is undefined at this point, do a spiral
!        search for valid value.
!-----------------------------------------------------------------------

         SPIRAL_SEARCH : do krad = 1, spiral_rad
                
           istart = nigrid - krad
           iend   = nigrid + krad
           jstart = njgrid - krad
           jend   = njgrid + krad

           do jj = jstart, jend
           do ii = istart, iend

!-----------------------------------------------------------------------
!            search only along outer square.
!-----------------------------------------------------------------------

             if ((jj .eq. jstart) .or. (jj .eq. jend) .or.   &            
                 (ii .eq. istart) .or. (ii .eq. iend))  then

!-----------------------------------------------------------------------
!              ensure that point being investigated is within
!              the northern and southern bounds of the source grid.
!-----------------------------------------------------------------------

               if ((jj .ge. jstart_src) .and. (jj .le. jend_src)) then 

                 jjj = jj

!-----------------------------------------------------------------------
!                adjust i-index on source grid when search
!                crosses the date line.
!-----------------------------------------------------------------------

                 if (ii .le. 0) then
                   iii = isrc + ii
                 else if (ii .ge. (isrc+1)) then
                   iii = ii - isrc                  
                 else
                   iii = ii
                 end if 

!-----------------------------------------------------------------------
!                a valid value was found.
!-----------------------------------------------------------------------

                 if (data_src(iii,jjj) /= undef_value) then
                   data_mdl(i,j) = float(data_src(iii,jjj))/float(scaling_fac)
                   write (6, 6000) i, j, krad
                   cycle ILOOP
                 end if

               end if

             end if

           enddo 
           enddo 

         enddo SPIRAL_SEARCH
 
!-----------------------------------------------------------------------
!        the circular search failed. therefore assign a default value.
!-----------------------------------------------------------------------

         data_mdl(i,j) = default_value
         write (6, 6100) i, j

       end if

     end if LAND_TEST

   enddo ILOOP
 enddo JLOOP

 return

!-----------------------------------------------------------------------
! format statements.
!-----------------------------------------------------------------------

 6000 FORMAT (1X, '-- CIRCULAR SEARCH AT POINT ', I4, 1X, I4, ' ITERATIONS ',I3) 

 6100 FORMAT (1X, '-- DEFAULT VALUE ASSIGNED AT POINT ', 1X, I4, 1X, I4) 
   
 end subroutine interp_bilinear
 end module interp_utils
