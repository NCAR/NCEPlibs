 module read_write_utils

 type date
   sequence
   integer    :: year
   integer    :: month
   integer    :: day
 end type date

 contains

!----------------------------------------------------------------------
! the gfs operates on a thinned or reduced grid. (# grid points in
! x-direction decreases toward poles)  when the gfs outputs its data,
! if fills in the thinned points with nearest neighbor data.  this
! routine mimics that process.
!----------------------------------------------------------------------

 subroutine full_to_thin(dummy_full, imdl, jmdl, lonsperlat)

 implicit none

 integer                         :: dummy(imdl*jmdl)
 integer, intent(in)             :: imdl
 integer                         :: ijmdl
 integer, intent(in)             :: jmdl
 integer, intent(in)             :: lonsperlat(jmdl/2)

 real, intent(inout)             :: dummy_full(imdl*jmdl)
 real, allocatable               :: dummy_thinned(:)

 dummy = 0   ! not used

 ijmdl = sum(lonsperlat) * 2    ! number of points, thinned or reduced grid.

 allocate(dummy_thinned(ijmdl))

 call interpred(1,dummy,dummy_full,dummy_thinned,imdl,jmdl,ijmdl,lonsperlat)

 call uninterpred(1, dummy, dummy_thinned, dummy_full, &
                  imdl, jmdl, ijmdl, lonsperlat)

 deallocate (dummy_thinned)

 return

 end subroutine full_to_thin

!-----------------------------------------------------------------------
! time interpolate to a specific day given an array of
! bounding dates.
!-----------------------------------------------------------------------

 subroutine time_interp(data_climo_in, climo_dates, num_dates, &
                        ijmdl, curr_year, curr_month, curr_day, &
                        curr_hour, curr_minute, data_climo_out)

 implicit none

 integer, intent(in)       :: curr_year
 integer, intent(in)       :: curr_month
 integer, intent(in)       :: curr_day
 integer, intent(in)       :: curr_hour
 integer, intent(in)       :: curr_minute
 integer, intent(in)       :: ijmdl
 integer, intent(in)       :: num_dates
 integer                   :: idat(8)
 integer                   :: jdat(8)
 integer                   :: n
 real                      :: ndays           ! # days between date and
                                              ! 2nd bounding rec
 integer                   :: ndays_btwn_recs ! # days between bounding recs
 integer                   :: rec1
 integer                   :: rec2

 real, intent(in)          :: data_climo_in(ijmdl, num_dates)
 real, intent(out)         :: data_climo_out(ijmdl)
 real                      :: rinc(5)
 real                      :: wght1
 real                      :: wght2

 type (date), intent(in)  :: climo_dates(num_dates)

!-----------------------------------------------------------------------
! current date/time for this time step.
!-----------------------------------------------------------------------

 idat    = 0
 idat(1) = curr_year
 idat(2) = curr_month
 idat(3) = curr_day
 idat(5) = curr_hour
 idat(6) = curr_minute

!-----------------------------------------------------------------------
! find grib records that bound the current date.  routine returns
! the number of days between idat and jdat (rinc(1)).
!-----------------------------------------------------------------------

 SEARCH : do n = 1, num_dates

   jdat    = 0
   jdat(1) = curr_year              ! don't use year from climo data
   jdat(2) = climo_dates(n)%month
   jdat(3) = climo_dates(n)%day
   jdat(5) = 0                      ! assume data valid at 00z

   call w3difdat(jdat, idat, 3, rinc)

   if (rinc(3) > 0) exit SEARCH

 enddo SEARCH

 rec2 = n
 rec1 = n - 1

!-----------------------------------------------------------------------
! handle end of year.
!-----------------------------------------------------------------------

 if (rec1 == 0)           rec1 = num_dates
 if (rec2 == num_dates+1) rec2 = 1

 if (rec2 /= 1) then

!-----------------------------------------------------------------------
! num of minutes between current day and second bounding record was
! calculated above.  convert to decimal number of days.
!-----------------------------------------------------------------------

   ndays = rinc(3) / 1440.0

!-----------------------------------------------------------------------
!  now calculate number of days between bounding records.
!-----------------------------------------------------------------------

   idat    = 0
   idat(1) = curr_year
   idat(2) = climo_dates(rec1)%month
   idat(3) = climo_dates(rec1)%day

   jdat    = 0
   jdat(1) = curr_year
   jdat(2) = climo_dates(rec2)%month
   jdat(3) = climo_dates(rec2)%day

   call w3difdat(jdat, idat, 1, rinc)

   ndays_btwn_recs = rinc(1)

 else  ! rec2 = 1

!-----------------------------------------------------------------------
!  bounding records span two years.
!-----------------------------------------------------------------------

   jdat = 0
   idat = 0

   idat(1) = curr_year
   idat(2) = curr_month
   idat(3) = curr_day
   idat(5) = curr_hour
   idat(6) = curr_minute

!-----------------------------------------------------------------------
!  calc number of days between current day and second bounding
!  record.
!-----------------------------------------------------------------------

   if (curr_month <= climo_dates(rec2)%month) then
     jdat(1) = curr_year
   else
     jdat(1) = curr_year+1
   end if

   jdat(2) = climo_dates(rec2)%month
   jdat(3) = climo_dates(rec2)%day

   call w3difdat(jdat, idat, 3, rinc)

   ndays = rinc(3) / 1440.

!-----------------------------------------------------------------------
!  now calculate number of days between bounding months.
!-----------------------------------------------------------------------

   idat = 0
   jdat = 0

   if (curr_month <= climo_dates(rec2)%month) then
     idat(1) = curr_year-1
   else
     idat(1) = curr_year
   end if

   idat(2) = climo_dates(rec1)%month
   idat(3) = climo_dates(rec1)%day

   if (curr_month <= climo_dates(rec2)%month) then
     jdat(1) = curr_year
   else
     jdat(1) = curr_year+1
   end if
   jdat(2) = climo_dates(rec2)%month
   jdat(3) = climo_dates(rec2)%day

   call w3difdat(jdat, idat, 1, rinc)
   ndays_btwn_recs = rinc(1)

 end if

!-----------------------------------------------------------------------
!  calculate temporal weights for records 1 and 2.
!-----------------------------------------------------------------------

  wght1 = ndays / float(ndays_btwn_recs)
  wght2 = 1.0 - wght1

  data_climo_out = (wght1 * data_climo_in(:,rec1)) + (wght2 * data_climo_in(:,rec2))

  print*,'mon/day ',curr_month,curr_day,curr_hour,curr_minute, &
                    rec1, rec2, ndays, &
                    ndays_btwn_recs, wght1, wght2

 return

 end subroutine time_interp

!-----------------------------------------------------------------------
! given data on the full grid, pick off the points on the subdomain
! (useful for when running in parallel.)
!-----------------------------------------------------------------------

 subroutine thin_data(dummy_in, ipts, jpts, lonsperlat,   &
                      ijmdl, jmdl2, imdl, jmdl, dummy_out)
 implicit none

 integer                   :: ifull
 integer                   :: ij
 integer, intent(in)       :: ijmdl  ! # points on subdomain
 integer, intent(in)       :: imdl   ! # i points on full domain
 integer, intent(in)       :: ipts(ijmdl)
 integer                   :: jfull
 integer                   :: jj
 integer, intent(in)       :: jmdl   ! # j points on full domain
 integer, intent(in)       :: jmdl2
 integer, intent(in)       :: jpts(ijmdl)
 integer, intent(in)       :: lonsperlat(jmdl2)

 real, intent(in)          :: dummy_in(imdl,jmdl)
 real, intent(out)         :: dummy_out(ijmdl)
 real                      :: r
 real                      :: x1

 if (lonsperlat(1) == -9999) then  ! regional grid, where number of
                                   ! 'i' points is constant.

   do ij = 1, ijmdl

     dummy_out(ij) = dummy_in(ipts(ij),jpts(ij))

   enddo

 else ! global grid, where number of points can decrease toward poles.

   do ij = 1, ijmdl

     jfull = jpts(ij)

     jj    = jfull
     if (jfull > (jmdl2)) jj = jmdl - jfull + 1

     r     = float(imdl) / float(lonsperlat(jj))
     x1    = float((ipts(ij) - 1)) * r
     ifull = nint(x1) + 1

     dummy_out(ij) = dummy_in(ifull,jfull)

   enddo

 end if

 return

 end subroutine thin_data

!-----------------------------------------------------------------------
! degrib multiple records of a climo file and pick off the points
! on the subdomain of interest.
!-----------------------------------------------------------------------

 subroutine degrib_climo_thin(data_climo, dates, ijmdl, imdl, jmdl, &
                              jmdl2, ipts, jpts, lonsperlat, param_num, &
                              input_file, tot_num_recs, status, me)

 implicit none

 character*150, intent(in)            :: input_file

 integer, intent(in)                  :: ijmdl
 integer, intent(in)                  :: imdl
 integer, intent(in)                  :: ipts(ijmdl)
 integer                              :: iret
 integer, parameter                   :: iunit_src = 55
 integer                              :: jgds(200)
 integer, intent(in)                  :: jmdl
 integer, intent(in)                  :: jmdl2
 integer                              :: jpds(200)
 integer, intent(in)                  :: jpts(ijmdl)
 integer                              :: kgds(200)
 integer                              :: kpds(200)
 integer, intent(in)                  :: lonsperlat(jmdl2)
 integer                              :: lskip
 integer                              :: lugi
 integer, intent(in), optional        :: me     ! mpi task number
 integer                              :: num_pts
 integer, intent(in)                  :: param_num
 integer                              :: rec_num
 integer, intent(out)                 :: status
 integer, intent(in)                  :: tot_num_recs

 logical*1, allocatable               :: lbms(:,:)

 real, intent(out)                    :: data_climo(ijmdl, tot_num_recs)
 real, allocatable                    :: dummy2d(:,:)

 type (date), intent(out)             :: dates(tot_num_recs)

!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------

 status = 0

 if (present(me)) then
   if (me == 0) print*,"- OPEN INPUT FILE ", trim(input_file)
 else
   print*,"- OPEN INPUT FILE ", trim(input_file)
 end if

 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   status = -1
   return
 end if

 allocate (dummy2d(imdl,jmdl))
 allocate (lbms(imdl,jmdl))

 lskip   = -1
 rec_num = 0
 lugi    = 0

 READ : do

   jgds    = -1
   jpds    = -1
   jpds(5) = param_num
   kgds    = -1
   kpds    = -1

   if (present(me)) then
     if (me == 0) print*,"- DEGRIB DATA "
   else
     print*,"- DEGRIB DATA "
   endif

   call getgb(iunit_src, lugi, (imdl*jmdl), lskip, jpds, jgds, &
              num_pts, lskip, kpds, kgds, lbms, dummy2d, iret)

   if ((iret > 0) .and. (iret < 99)) then
     print*,'- BAD READ OF GRIB DATA. IRET IS ', iret
     status = -2
     return
   end if

   if (iret == 99 .or. rec_num == tot_num_recs) exit READ

   if (param_num == kpds(5)) then

     rec_num = rec_num + 1

     dates(rec_num)%day   = kpds(10)
     dates(rec_num)%month = kpds(9)
     dates(rec_num)%year  = ((kpds(21)-1) * 100) + kpds(8)

!     print*,rec_num,dates(rec_num)%year,dates(rec_num)%month,dates(rec_num)%day

     call thin_data(dummy2d, ipts, jpts, lonsperlat,   &
                    ijmdl, jmdl2, imdl, jmdl, data_climo(1,rec_num))

!     print*,maxval(data_climo(:,rec_num)),minval(data_climo(:,rec_num))

   end if

 enddo READ

 call baclose(iunit_src, iret)

 return

 end subroutine degrib_climo_thin

!-----------------------------------------------------------------------
! degrib multiple records of a climo file.
!-----------------------------------------------------------------------

 subroutine degrib_climo(data_climo, dates, ijmdl,      &
                         param_num, input_file,         &
                         tot_num_recs, status, me)

 implicit none

 character*150, intent(in)            :: input_file

 integer, intent(in)                  :: ijmdl
 integer                              :: iret
 integer, parameter                   :: iunit_src = 55
 integer                              :: jgds(200)
 integer                              :: jpds(200)
 integer                              :: kgds(200)
 integer                              :: kpds(200)
 integer                              :: lskip
 integer                              :: lugi
 integer, intent(in)                  :: me
 integer                              :: num_pts
 integer, intent(in)                  :: param_num
 integer                              :: rec_num
 integer, intent(out)                 :: status
 integer, intent(in)                  :: tot_num_recs
 
 logical*1, allocatable               :: lbms(:)

 real, intent(out)                    :: data_climo(ijmdl, tot_num_recs)
 real, allocatable                    :: dummy(:)

 type (date), intent(out)             :: dates(tot_num_recs)

!-----------------------------------------------------------------------
! read data.
!-----------------------------------------------------------------------

 status = 0

 if (me == 0) print*,"- OPEN INPUT FILE ", trim(input_file)
 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   status = -1
   return
 end if

 allocate (dummy(ijmdl))
 allocate (lbms(ijmdl))

 lskip   = -1
 rec_num = 0
 lugi    = 0

 READ : do

   jgds    = -1
   jpds    = -1
   jpds(5) = param_num
   kgds    = -1
   kpds    = -1

   if (me == 0) print*,"- DEGRIB DATA "

   call getgb(iunit_src, lugi, ijmdl, lskip, jpds, jgds, &
              num_pts, lskip, kpds, kgds, lbms, dummy, iret)

   if ((iret > 0) .and. (iret < 99)) then
     print*,'- BAD READ OF GRIB DATA. IRET IS ', iret
     status = -2
     return
   end if

   if (iret == 99 .or. rec_num == tot_num_recs) exit READ

   if (param_num == kpds(5)) then

     rec_num = rec_num + 1

     dates(rec_num)%day   = kpds(10)
     dates(rec_num)%month = kpds(9)
     dates(rec_num)%year  = ((kpds(21)-1) * 100) + kpds(8)

     data_climo(:,rec_num) = dummy

   end if

 enddo READ

 call baclose(iunit_src, iret)

 deallocate (dummy)
 deallocate (lbms)

 return

 end subroutine degrib_climo

!----------------------------------------------------------------
! inventory a climo grib file.
!----------------------------------------------------------------

 subroutine inventory (input_file, param_num, tot_num_recs, status, me)

 implicit none

 character*150, intent(in)            :: input_file

 integer                              :: iret
 integer, parameter                   :: iunit_src = 55
 integer                              :: jgds(200)
 integer                              :: jpds(200)
 integer                              :: kgds(200)
 integer                              :: kpds(200)
 integer                              :: lskip
 integer                              :: lugi
 integer, intent(in)                  :: me   ! mpi task number
 integer                              :: message_num
 integer                              :: num_bytes
 integer                              :: num_pts
 integer, intent(in)                  :: param_num
 integer, intent(out)                 :: status
 integer, intent(out)                 :: tot_num_recs

!-----------------------------------------------------------------------
! open input and output files.
!-----------------------------------------------------------------------

 status = 0

 if (me == 0) print*,"- OPEN INPUT FILE ", trim(input_file)
 call baopenr (iunit_src, input_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN, IRET IS ', iret
   status = -1
   return
 end if

!-----------------------------------------------------------------------
! inventory the source data.  assumes the following:
!
! 1) data is in grib format
! 2) multiple parameter types are allowed (ex: different types of
!    albedo), but they must be in consecutive records.
! 3) multiple time periods are allowed, but they must be in
!    chronological order.
! 4) each grib record is on the same grid.
!-----------------------------------------------------------------------

 lugi         = 0
 tot_num_recs = 0
 lskip        = -1

 INVENTORY_LOOP : do

   jgds    = -1
   jpds    = -1
   jpds(5) = param_num
   kgds    = -1
   kpds    = -1

   if (me == 0) print*,"- GET GRIB HEADER FOR RECORD ", (max(lskip,0)+1)

   call getgbh(iunit_src, lugi, lskip, jpds, jgds, num_bytes,  &
               num_pts, message_num, kpds, kgds, iret)

!-----------------------------------------------------------------------
!  when the end of file is reached, a status code of 99 is returned.
!  other status codes indicate a bad read.
!-----------------------------------------------------------------------

   if ((iret > 0) .and. (iret < 99)) then
     print*,'- BAD READ OF GRIB HEADER. IRET IS ', iret
     status = -2
     return
   end if

   if (iret == 99) exit INVENTORY_LOOP

!-----------------------------------------------------------------------
!  keep track of the number of records for this field.
!-----------------------------------------------------------------------

   if (param_num == kpds(5)) then
     tot_num_recs = tot_num_recs + 1
   end if

   lskip = message_num

 enddo INVENTORY_LOOP

 if (me == 0) &
 print*,'FILE CONTAINS ', tot_num_recs, ' RECORDS OF PARAMETER ', param_num

 call baclose(iunit_src, iret)

 return

 end subroutine inventory

!-----------------------------------------------------------------------
! generic routine to read grib data.
!-----------------------------------------------------------------------

 subroutine read_grib_data(grib_file, parm_num, dummy, ijmdl, istatus, me)

 implicit none

 character*150, intent(in)     :: grib_file

 integer, intent(in)           :: ijmdl
 integer                       :: iret
 integer, intent(out)          :: istatus
 integer, parameter            :: iunit = 13  ! grib file unit number
 integer                       :: jgds(200)
 integer                       :: jpds(200)
 integer                       :: lgrib
 integer                       :: lskip
 integer, parameter            :: lugi = 0    ! grib index file unit number - not used
 integer                       :: kgds(200)
 integer                       :: kpds(200)
 integer, intent(in), optional :: me          ! used to limit print with mpi
 integer                       :: numbytes
 integer                       :: numpts
 integer, intent(in)           :: parm_num

 logical*1, allocatable        :: lbms(:)

 real, intent(out)             :: dummy(ijmdl)

 istatus = 0

 if (present(me)) then
   if (me == 0)  print*,"- OPEN FILE ", trim(grib_file)
 else
   print*,"- OPEN FILE ", trim(grib_file)
 end if

 call baopenr (iunit, grib_file, iret)

 if (iret /= 0) then
   print*,'- BAD OPEN OF FILE, IRET IS ', iret
   istatus = -1
   return
 end if

!-----------------------------------------------------------------------
! tell degribber what to look for.
!-----------------------------------------------------------------------

 lskip    = -1
 jpds     = -1
 jgds     = -1
 jpds(5)  = parm_num
 kpds     = jpds
 kgds     = jgds

 if (present(me)) then
   if (me == 0) print*,"- DEGRIB DATA."
 else
   print*,"- DEGRIB DATA."
 end if

 allocate(lbms(ijmdl))

 call getgb(iunit, lugi, ijmdl, lskip, jpds, jgds, &
            numpts, lskip, kpds, kgds, lbms, dummy, iret)

 if (iret /= 0) then
   print*,"- BAD DEGRIB OF DATA. IRET IS ", iret
   istatus = -2
   return
 end if

 call baclose(iunit,iret)

 deallocate (lbms)

 return

 end subroutine read_grib_data

!-----------------------------------------------------------------------
! fills out full grid using thinned grid data.  use an iord of
! "1" to use a nearest neighbor approach.
!-----------------------------------------------------------------------

 subroutine uninterpred(iord,kmsk,fi,f,lonl,latd,len,lonsperlat)

 implicit none

 integer, intent(in)               :: len
 integer, intent(in)               :: iord
 integer, intent(in)               :: lonl
 integer, intent(in)               :: latd
 integer, intent(in)               :: lonsperlat(latd/2)
 integer, intent(in)               :: kmsk(lonl*latd)
 integer                           :: j,lons,jj,latd2,ii,i

 real, intent(in)                  :: fi(len)
 real, intent(out)                 :: f(lonl,latd)

 latd2 = latd / 2
 ii    = 1

 do j=1,latd

   jj = j
   if (j .gt. latd2) jj = latd - j + 1
   lons=lonsperlat(jj)

   if(lons.ne.lonl) then
     call intlon(iord,1,1,lons,lonl,kmsk(ii),fi(ii),f(1,j))
   else
     do i=1,lonl
       f(i,j)  = fi(ii+i-1)
     enddo
   endif

   ii = ii + lons

 enddo

 end subroutine uninterpred

 subroutine interpred(iord,kmsk,f,fi,lonl,latd,ijmdl,lonsperlat)

 implicit none

 integer,intent(in)             :: iord
 integer,intent(in)             :: ijmdl
 integer,intent(in)             :: lonl
 integer,intent(in)             :: latd
 integer,intent(in)             :: lonsperlat(latd/2)
 integer,intent(in)             :: kmsk(lonl,latd)
 integer                        :: j,lons,jj,latd2,ii,i

 real,intent(in)                :: f(lonl,latd)
 real,intent(out)               :: fi(ijmdl)

 latd2 = latd / 2
 ii    = 1
 do j=1,latd
   jj = j
   if (j .gt. latd2) jj = latd - j + 1

   lons=lonsperlat(jj)
   if(lons.ne.lonl) then
      call intlon(iord,1,1,lonl,lons,kmsk(1,j),f(1,j),fi(ii))
   else
      do i=1,lonl
        fi(ii+i-1)  = f(i,j)
      enddo
   endif
   ii = ii + lons
 enddo

 end subroutine interpred

 subroutine intlon(iord,imon,imsk,m1,m2,k1,f1,f2)

 implicit none

 integer,intent(in)        :: iord,imon,imsk,m1,m2
 integer,intent(in)        :: k1(m1)
 integer                   :: i2,in,il,ir

 real,intent(in)           :: f1(m1)
 real,intent(out)          :: f2(m2)
 real                      :: r,x1

 r=real(m1)/real(m2)
 do i2=1,m2
   x1=(i2-1)*r
   il=int(x1)+1
   ir=mod(il,m1)+1
   if(iord.eq.2.and.(imsk.eq.0.or.k1(il).eq.k1(ir))) then
     f2(i2)=f1(il)*(il-x1)+f1(ir)*(x1-il+1)
   else
     in=mod(nint(x1),m1)+1
     f2(i2)=f1(in)
   endif
 enddo

 end subroutine intlon

!-----------------------------------------------------------------------
! get time based on model cycle and forecast hour.
!-----------------------------------------------------------------------

 subroutine new_time(cycle_year, cycle_month, &
                     cycle_day,  cycle_hour,  &
                     fcst_hour, curr_year, curr_month, &
                     curr_day, curr_hour, curr_minute, &
                     curr_date)

 implicit none

 character*10, intent(out)  :: curr_date

 integer, intent(out)       :: curr_day
 integer, intent(out)       :: curr_hour
 integer, intent(out)       :: curr_minute
 integer, intent(out)       :: curr_month
 integer, intent(out)       :: curr_year
 integer, intent(in)        :: cycle_day
 integer, intent(in)        :: cycle_hour
 integer, intent(in)        :: cycle_month
 integer, intent(in)        :: cycle_year
 integer                    :: idat(8)
 integer                    :: jdat(8)

 real, intent(in)           :: fcst_hour
 real                       :: fcst_minute
 real                       :: rinc(5)

 rinc = 0
 idat = 0
 jdat = 0

 fcst_minute = nint((fcst_hour - int(fcst_hour)) * 60.0)

 rinc(2) = int(fcst_hour)
 rinc(3) = fcst_minute

 idat(1) = cycle_year
 idat(2) = cycle_month
 idat(3) = cycle_day
 idat(5) = cycle_hour

 call w3movdat(rinc, idat, jdat)

 curr_year  = jdat(1)
 curr_month = jdat(2)
 curr_day   = jdat(3)
 curr_hour  = jdat(5)
 curr_minute= jdat(6)

 write(curr_date(1:4),  "(i4)")   curr_year
 write(curr_date(5:6),  "(i2.2)") curr_month
 write(curr_date(7:8),  "(i2.2)") curr_day
 write(curr_date(9:10), "(i2.2)") curr_hour

 return

 end subroutine new_time

 end module read_write_utils
