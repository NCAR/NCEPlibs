!-------------------------------------------------------------------------------
module nemsio_gfs
!$$$ module document block
! Module : nemsio_gfs: contains data types for gfs nemsio anl file fileheader and
!                      data and subroutines  to read/write data, mainly used by 
!                      chgres and sfccycle
! Abstract
! Program history log
!   2009-07-07 Jun Wang  :  adopt from gfsio_rst(Fanglin Yang), update to nemsio,
!                           add read/wrt subroutines for 8byte real data for 
!                           gaussian grid data and sfc data
!   2010-09-24 Sarah Lu  :  add functions for 4byte real data for sfc file
!   2010-10-13 Jun Wang  :  add functions for 4byte real data for gaussian grid file
!   2011-04-13 Jun Wang  :  general tracers
!   2015-10-14 S Moorthi :  add tke
!   2015-12-31 S Moorthi :  create _slg versions of routines without p, dp and vvel
!   2016-01-12 S Moorthi :  remove _slg versions and add nopdpvv option to exclude dp, p
!   2016-04-25 Jun Wang  :  use meta data header to decide which fields are read/written 
!
! Public Defined Types
!   nemsio_head        nemsio file header information
!   nemsio_headv       second nemsio file header information
!   nemsio_data        nemsio file data information
!
! Public Defined subroutines
!   
!$$$ end module document block
!-------------------------------------------------------------------------------
!
!
  implicit none
!
  private
!
  integer,       parameter :: intkind=4,   realkind=4, dblekind=8
  integer,       parameter :: charkind8=8, charkind=16
  real(intkind), parameter :: intfill=-9999_intkind
  real(realkind),parameter :: realfill=-9999._realkind
  real(dblekind),parameter :: dblefill=-9999._dblekind
!
  public intkind,realkind,dblekind,charkind8,charkind
  public nemsio_gfsgrd_open,   nemsio_gfssfc_open,     &
         nemsio_gfs_wrtgrd,    nemsio_gfs_wrtsfc,      &
         nemsio_gfs_rdgrd,     nemsio_gfs_rdsfc,       &
         nemsio_gfs_axheadv,                           &
         nemsio_gfs_algrd,     nemsio_gfs_axgrd,       &
         nemsio_gfs_alsfc,     nemsio_gfs_axsfc
 
!
  type,public :: nemsio_head
    integer(intkind)     :: version=intfill
    character(charkind8) :: gtype=''
    character(charkind8) :: gdatatype='bin4'
    character(charkind8) :: modelname='GFS'
    integer(intkind)     :: nmeta=12
    integer(intkind)     :: nrec=intfill

    integer(intkind)     :: nfday=intfill
    integer(intkind)     :: nfhour=intfill
    integer(intkind)     :: nfminute=intfill
    integer(intkind)     :: nfsecondn=intfill
    integer(intkind)     :: nfsecondd=intfill
    integer(intkind)     :: idate(7)=intfill

    integer(intkind)     :: dimx=intfill
    integer(intkind)     :: dimy=intfill
    integer(intkind)     :: dimz=intfill
    integer(intkind)     :: ntrac=intfill
    integer(intkind)     :: nsoil=intfill

    integer(intkind)     :: jcap=intfill
    integer(intkind)     :: ncldt=intfill
    integer(intkind)     :: idvc=intfill
    integer(intkind)     :: idsl=intfill
    integer(intkind)     :: idvm=intfill
    integer(intkind)     :: idrt=intfill
    integer(intkind)     :: ntoz=intfill
    integer(intkind)     :: ntcw=intfill
    integer(intkind)     :: ncld=intfill
    integer(intkind)     :: ntke=intfill

    integer(intkind)     :: itrun=intfill
    integer(intkind)     :: iorder=intfill
    integer(intkind)     :: irealf=intfill
    integer(intkind)     :: igen=intfill
    integer(intkind)     :: latb=intfill
    integer(intkind)     :: lonb=intfill
    integer(intkind)     :: levs=intfill
    integer(intkind)     :: latf=intfill
    integer(intkind)     :: lonf=intfill
    integer(intkind)     :: latr=intfill
    integer(intkind)     :: lonr=intfill
    integer(intkind)     :: icen2=intfill
    integer(intkind)     :: iens(2)=intfill
    integer(intkind)     :: idpp=intfill
    integer(intkind)     :: idvt=intfill
    integer(intkind)     :: idrun=intfill
    integer(intkind)     :: idusr=intfill
    real(realkind)       :: pdryini=realfill
    real(realkind)       :: fhour=realfill
    integer(intkind)     :: ixgr=intfill
    integer(intkind)     :: nvcoord=intfill
    integer(intkind)     :: ivssig=intfill
!sfc
    integer(intkind)     :: ivs=intfill
!
    logical              :: extrameta
    integer(intkind)     :: nmetavari=intfill
    integer(intkind)     :: nmetavarr=intfill
    integer(intkind)     :: nmetavarr8=intfill
    integer(intkind)     :: nmetavarl=intfill
    integer(intkind)     :: nmetavarc=intfill
    integer(intkind)     :: nmetaaryi=intfill
    integer(intkind)     :: nmetaaryr=intfill
    integer(intkind)     :: nmetaaryr8=intfill
    integer(intkind)     :: nmetaaryc=intfill
    
  end type nemsio_head

  type,public :: nemsio_headv
    real(realkind),allocatable      :: vcoord(:,:,:)
    character(charkind),allocatable :: recname(:)
    character(charkind),allocatable :: reclevtyp(:)
    integer(intkind),allocatable    :: reclev(:)
    real(realkind),allocatable      :: lat(:)
    real(realkind),allocatable      :: lon(:)
    real(realkind),allocatable      :: dx(:)
    real(realkind),allocatable      :: dy(:)
    real(realkind),allocatable      :: cpi(:)
    real(realkind),allocatable      :: ri(:)
!sfc
    integer(intkind),allocatable    :: lpl(:)
    real(realkind),allocatable      :: zsoil(:)
!
    character(charkind),allocatable :: variname(:)
    character(charkind),allocatable :: varrname(:)
    character(charkind),allocatable :: varr8name(:)
    character(charkind),allocatable :: varlname(:)
    character(charkind),allocatable :: varcname(:)
    character(charkind),allocatable :: aryiname(:)
    character(charkind),allocatable :: aryrname(:)
    character(charkind),allocatable :: aryr8name(:)
    character(charkind),allocatable :: arycname(:)
    integer(intkind),allocatable    :: varival(:)
    integer(intkind),allocatable    :: aryilen(:)
    integer(intkind),allocatable    :: aryival(:,:)
    real(realkind),allocatable      :: varrval(:)
    real(dblekind),allocatable      :: varr8val(:)
    integer(intkind),allocatable    :: aryrlen(:)
    real(realkind),allocatable      :: aryrval(:,:)
    integer(intkind),allocatable    :: aryr8len(:)
    real(dblekind),allocatable      :: aryr8val(:,:)
    logical,allocatable             :: varlval(:)
    character(charkind),allocatable :: varcval(:)
    integer(intkind),allocatable    :: aryclen(:)
    character(charkind),allocatable :: arycval(:)
    
  end type nemsio_headv

  character(charkind),dimension(18) :: aero_tracername=(/      &
      "du001","du002","du003","du004","du005",                 &
      "ss001","ss002","ss003","ss004","ss005",                 &
      "dms",  "so2",  "so4",  "msa",  "bcphobic",              &
      "bcphilic","ocphobic","ocphilic"/)


  type,public:: nemsio_data
!sigma
    real(realkind),allocatable :: zs(:,:)      !surface height, m
    real(realkind),allocatable :: ps(:,:)      !surface pressure, pa
    real(realkind),allocatable :: dp(:,:,:)    !layer pressure thickness, pa
    real(realkind),allocatable :: p(:,:,:)     !layer pressure, pa  
    real(realkind),allocatable :: u(:,:,:)     !layer zonal wind, m/s
    real(realkind),allocatable :: v(:,:,:)     !layer meridional wind, m/s
    real(realkind),allocatable :: t(:,:,:)     !layer temperature, k
    real(realkind),allocatable :: q(:,:,:,:)   !tracers, 1-spfh; 2-O3; 3-CLW , kg/kg
    real(realkind),allocatable :: w(:,:,:)     !layer vertical velocity  pa/s
!sfc
    real(realkind),allocatable :: tsea(:,:)   
    real(realkind),allocatable :: smc(:,:,:)  
    real(realkind),allocatable :: sheleg(:,:) 
    real(realkind),allocatable :: stc(:,:,:)  
    real(realkind),allocatable :: tg3(:,:)   !
    real(realkind),allocatable :: zorl(:,:)  
!   real(realkind),allocatable :: cv(:,:)   
!   real(realkind),allocatable :: cvb(:,:)  
!   real(realkind),allocatable :: cvt(:,:)  
    real(realkind),allocatable :: alvsf(:,:) 
    real(realkind),allocatable :: alvwf(:,:)
    real(realkind),allocatable :: alnsf(:,:)   !
    real(realkind),allocatable :: alnwf(:,:)   
    real(realkind),allocatable :: slmsk(:,:)  
    real(realkind),allocatable :: vfrac(:,:) 
    real(realkind),allocatable :: canopy(:,:)
    real(realkind),allocatable :: f10m(:,:)   
    real(realkind),allocatable :: t2m(:,:)  
    real(realkind),allocatable :: q2m(:,:) 
    real(realkind),allocatable :: vtype(:,:)
    real(realkind),allocatable :: stype(:,:)   !
    real(realkind),allocatable :: facsf(:,:)   
    real(realkind),allocatable :: facwf(:,:)  
    real(realkind),allocatable :: uustar(:,:) 
    real(realkind),allocatable :: ffmm(:,:)
    real(realkind),allocatable :: ffhh(:,:)
    real(realkind),allocatable :: hice(:,:)
    real(realkind),allocatable :: fice(:,:)
    real(realkind),allocatable :: tisfc(:,:)
    real(realkind),allocatable :: tprcp(:,:)
    real(realkind),allocatable :: srflag(:,:)
    real(realkind),allocatable :: snwdph(:,:)
    real(realkind),allocatable :: slc(:,:,:)
    real(realkind),allocatable :: shdmin(:,:)
    real(realkind),allocatable :: shdmax(:,:)
    real(realkind),allocatable :: slope(:,:)
    real(realkind),allocatable :: snoalb(:,:)
    real(realkind),allocatable :: orog(:,:)
  end type nemsio_data

  type,public:: nemsio_dbta
!sigma
    real(dblekind),allocatable :: zs(:,:)      !surface height, m
    real(dblekind),allocatable :: ps(:,:)      !surface pressure, pa
    real(dblekind),allocatable :: dp(:,:,:)    !layer pressure thickness, pa
    real(dblekind),allocatable :: p(:,:,:)     !layer pressure, pa  
    real(dblekind),allocatable :: u(:,:,:)     !layer zonal wind, m/s
    real(dblekind),allocatable :: v(:,:,:)     !layer meridional wind, m/s
    real(dblekind),allocatable :: t(:,:,:)     !layer temperature, k
    real(dblekind),allocatable :: q(:,:,:,:)   !tracers, 1-spfh; 2-O3; 3-CLW , kg/kg
    real(dblekind),allocatable :: w(:,:,:)     !layer vertical velocity  pa/s
!sfc
    real(dblekind),allocatable :: tsea(:,:)   
    real(dblekind),allocatable :: smc(:,:,:)  
    real(dblekind),allocatable :: sheleg(:,:) 
    real(dblekind),allocatable :: stc(:,:,:)  
    real(dblekind),allocatable :: tg3(:,:)   !
    real(dblekind),allocatable :: zorl(:,:)  
!   real(dblekind),allocatable :: cv(:,:)   
!   real(dblekind),allocatable :: cvb(:,:)  
!   real(dblekind),allocatable :: cvt(:,:)  
    real(dblekind),allocatable :: alvsf(:,:) 
    real(dblekind),allocatable :: alvwf(:,:)
    real(dblekind),allocatable :: alnsf(:,:)   !
    real(dblekind),allocatable :: alnwf(:,:)   
    real(dblekind),allocatable :: slmsk(:,:)  
    real(dblekind),allocatable :: vfrac(:,:) 
    real(dblekind),allocatable :: canopy(:,:)
    real(dblekind),allocatable :: f10m(:,:)   
    real(dblekind),allocatable :: t2m(:,:)  
    real(dblekind),allocatable :: q2m(:,:) 
    real(dblekind),allocatable :: vtype(:,:)
    real(dblekind),allocatable :: stype(:,:)   !
    real(dblekind),allocatable :: facsf(:,:)   
    real(dblekind),allocatable :: facwf(:,:)  
    real(dblekind),allocatable :: uustar(:,:) 
    real(dblekind),allocatable :: ffmm(:,:)
    real(dblekind),allocatable :: ffhh(:,:)
    real(dblekind),allocatable :: hice(:,:)
    real(dblekind),allocatable :: fice(:,:)
    real(dblekind),allocatable :: tisfc(:,:)
    real(dblekind),allocatable :: tprcp(:,:)
    real(dblekind),allocatable :: srflag(:,:)
    real(dblekind),allocatable :: snwdph(:,:)
    real(dblekind),allocatable :: slc(:,:,:)
    real(dblekind),allocatable :: shdmin(:,:)
    real(dblekind),allocatable :: shdmax(:,:)
    real(dblekind),allocatable :: slope(:,:)
    real(dblekind),allocatable :: snoalb(:,:)
    real(dblekind),allocatable :: orog(:,:)
!
  end type nemsio_dbta
!
  interface nemsio_gfs_algrd
    module procedure nemsio_gfs_aldbta_grd
    module procedure nemsio_gfs_aldata_grd
  end interface nemsio_gfs_algrd
!
  interface nemsio_gfs_axgrd
    module procedure nemsio_gfs_axdbta_grd
    module procedure nemsio_gfs_axdata_grd
  end interface nemsio_gfs_axgrd

!
  interface nemsio_gfs_alsfc
    module procedure nemsio_gfs_aldbta_sfc
    module procedure nemsio_gfs_aldata_sfc
  end interface nemsio_gfs_alsfc
!
  interface nemsio_gfs_axsfc
    module procedure nemsio_gfs_axdbta_sfc
    module procedure nemsio_gfs_axdata_sfc
  end interface nemsio_gfs_axsfc
!
  interface nemsio_gfs_rdsfc
    module procedure nemsio_gfs_rdsfc4
    module procedure nemsio_gfs_rdsfc8
  end interface nemsio_gfs_rdsfc
!
  interface nemsio_gfs_wrtsfc
    module procedure nemsio_gfs_wrtsfc4
    module procedure nemsio_gfs_wrtsfc8
  end interface nemsio_gfs_wrtsfc
!
  interface nemsio_gfs_rdgrd
    module procedure nemsio_gfs_rdgrd4
    module procedure nemsio_gfs_rdgrd8
  end interface nemsio_gfs_rdgrd
!
  interface nemsio_gfs_wrtgrd
    module procedure nemsio_gfs_wrtgrd4
    module procedure nemsio_gfs_wrtgrd8
  end interface nemsio_gfs_wrtgrd
!
contains

!-----------------------------------------------------------------------   
  subroutine nemsio_gfsgrd_open(gfile, filename, gaction, nopdpvv, gfshead, gfsheadv, iret)
!-----------------------------------------------------------------------   
!
    use nemsio_module, only : nemsio_gfile, nemsio_getfilehead,            &
                              nemsio_open,  nemsio_getheadvar
!
    implicit none
!
    type(nemsio_gfile),intent(inout)   :: gfile
    character*(*),intent(in)           :: filename
    character*(*),intent(in)           :: gaction
    logical,intent(in)                 :: nopdpvv
    type(nemsio_head),intent(inout)    :: gfshead
    type(nemsio_headv),intent(inout)   :: gfsheadv
    integer,intent(out)                :: iret
    integer ios,ios1,i,k,levso,nrec,jrec
    character(8) filetype, modelname
!
    if(trim(gaction) == "read" .or. trim(gaction) == "READ")then
      call nemsio_open(gfile,trim(filename),'read',ios)
      if(ios == 0) then
        call nemsio_getfilehead(gfile,gtype=filetype,modelname=modelname,iret=ios)
        if ((TRIM(modelname) == 'GFS'.or.TRIM(modelname) == 'gfs') .and. ios == 0) then

!  open (read) nemsio grid file headers
        call nemsio_getfilehead(gfile,                                        &
               idate=gfshead%idate, nfhour=gfshead%nfhour, nfminute=gfshead%nfminute, &
               nfsecondn=gfshead%nfsecondn, nfsecondd=gfshead%nfsecondd,      &
               version=gfshead%version, nrec=gfshead%nrec, dimx=gfshead%dimx, &
               dimy=gfshead%dimy, dimz=gfshead%dimz, jcap=gfshead%jcap,       &
               ntrac=gfshead%ntrac, ncldt=gfshead%ncldt, nsoil=gfshead%nsoil, &
               idsl=gfshead%idsl, idvc=gfshead%idvc, idvm=gfshead%idvm,       &
               idrt=gfshead%idrt, extrameta=gfshead%extrameta,                &
               nmetavari=gfshead%nmetavari, nmetavarr=gfshead%nmetavarr,      &
               nmetavarl=gfshead%nmetavarl, nmetavarr8=gfshead%nmetavarr8,    &
               nmetaaryi=gfshead%nmetaaryi, nmetaaryr=gfshead%nmetaaryr,      &
               iret=ios)

        call nemsio_getheadvar(gfile,'fhour', gfshead%fhour,iret=ios)
        if(ios/=0) gfshead%fhour = gfshead%nfhour + gfshead%nfminute/60.          &
     &                           + gfshead%nfsecondn/(3600.*gfshead%nfsecondd)

        call nemsio_getheadvar(gfile,'dimx',    gfshead%latb,iret=ios)
        call nemsio_getheadvar(gfile,'dimy',    gfshead%LONB,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'LEVS',    gfshead%LEVS,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'ITRUN',   gfshead%ITRUN,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IORDER',  gfshead%IORDER,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IREALF',  gfshead%IREALF,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IGEN',    gfshead%IGEN,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'LATF',    gfshead%LATF,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'LONF',    gfshead%LONF,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'LATR',    gfshead%LATR,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'LONR',    gfshead%LONR,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'ICEN2',   gfshead%ICEN2,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IENS',    gfshead%IENS,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IDPP',    gfshead%IDPP,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IDVT',    gfshead%IDVT,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IDRUN',   gfshead%IDRUN,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IDUSR',   gfshead%IDUSR,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'PDRYINI', gfshead%PDRYINI,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'IXGR',    gfshead%IXGR,IRET=ios)
        CALL NEMSIO_GETHEADVAR(GFILE,'NVCOORD', gfshead%NVCOORD,IRET=ios)

        call nemsio_gfs_alheadv(gfshead,gfsheadv)
        
        CALL NEMSIO_GETFILEHEAD(GFILE                                 &
     &,                         RECNAME=gfsheadv%RECNAME              &
     &,                         RECLEVTYP=gfsheadv%RECLEVTYP          &
     &,                         RECLEV=gfsheadv%RECLEV                &
     &,                         VCOORD=gfsheadv%VCOORD                &
     &,                         LAT=gfsheadv%LAT                      &
     &,                         LON=gfsheadv%LON                      &
     &,                         CPI=gfsheadv%CPI                      &
     &,                         RI=gfsheadv%RI                        &
     &,                         variname=gfsheadv%variname            &
     &,                         varrname=gfsheadv%varrname            &
     &,                         varlname=gfsheadv%varlname            &
     &,                         varival=gfsheadv%varival              &
     &,                         varrval=gfsheadv%varrval              &
     &,                         varlval=gfsheadv%varlval              &
     &,                         aryiname=gfsheadv%aryiname            &
     &,                         aryrname=gfsheadv%aryrname            &
     &,                         aryilen=gfsheadv%aryilen              &
     &,                         aryrlen=gfsheadv%aryrlen              &
     &,                         IRET=ios1)
        if(gfshead%nmetaaryi > 0) then
          ALLOCATE(gfsheadv%aryival(maxval(gfsheadv%aryilen),       &
     &             gfshead%nmetaaryi))
          CALL NEMSIO_GETFILEHEAD(gfile,aryival=gfsheadv%aryival)
        endif
        if(gfshead%nmetaaryr > 0) then
          ALLOCATE(gfsheadv%aryrval(maxval(gfsheadv%aryrlen),       &
     &             gfshead%nmetaaryr))
          CALL NEMSIO_GETFILEHEAD(gfile,aryrval=gfsheadv%aryrval)
        endif
!jw        IF(IRET.NE.0 .OR. IRET1.NE.0) THEN
        IF(gfshead%NVCOORD == -9999) THEN
           gfshead%NVCOORD = 3
           if(maxval(gfsheadv%VCOORD(:,3,1))==0..and.                &
     &        minval(gfsheadv%VCOORD(:,3,1))==0. ) then
            gfshead%NVCOORD = 2
!jw for hyb: when no idsl is set
            if(gfshead%IDSL == -9999) gfshead%IDSL = 1
            if(maxval(gfsheadv%VCOORD(:,2,1))==0. .and.              &
     &         minval(gfsheadv%VCOORD(:,2,1))==0.) then
            gfshead%NVCOORD = 1
            endif
          endif
        ENDIF

        endif
      else
        print *,'nemsio read error, iret=',ios
      endif
    else
! for write:
      nrec = gfshead%nrec
      if(allocated(gfsheadv%recname))   deallocate(gfsheadv%recname)
      if(allocated(gfsheadv%reclevtyp)) deallocate(gfsheadv%reclevtyp)
      if(allocated(gfsheadv%reclev))    deallocate(gfsheadv%reclev)
      ALLOCATE(gfsheadv%RECNAME(nrec))
      ALLOCATE(gfsheadv%RECLEVTYP(nrec))
      ALLOCATE(gfsheadv%RECLEV(nrec))

      levso = gfshead%dimz
!-----------------------
      if (nopdpvv) then
!-----------------------
      gfsheadv%RECNAME(1) = 'hgt'
      gfsheadv%RECNAME(2) = 'pres'
      gfsheadv%RECNAME(3:(2+levso)) = 'ugrd'
      gfsheadv%RECNAME((3+levso):(2+2*levso))  = 'vgrd'
      gfsheadv%RECNAME((3+2*levso):(2+3*levso)) = 'tmp'
      gfsheadv%RECNAME((3+3*levso):(2+4*levso)) = 'spfh'
      gfsheadv%RECNAME((3+4*levso):(2+5*levso)) = 'o3mr'
      gfsheadv%RECNAME((3+5*levso):(2+6*levso)) = 'clwmr'
      do i=1,gfshead%ntrac-3
        gfsheadv%RECNAME((3+(i+5)*levso):(2+(i+6)*levso)) = aero_tracername(i)
      enddo
!-----------------------
      else
!-----------------------
      gfsheadv%RECNAME(1) = 'hgt'
      gfsheadv%RECNAME(2) = 'pres'
      gfsheadv%RECNAME(3:(2+levso)) = 'dpres'
      gfsheadv%RECNAME((3+levso):(2+2*levso))  = 'pres'
      gfsheadv%RECNAME((3+2*levso):(2+3*levso)) = 'ugrd'
      gfsheadv%RECNAME((3+3*levso):(2+4*levso)) = 'vgrd'
      gfsheadv%RECNAME((3+4*levso):(2+5*levso)) = 'tmp'
      gfsheadv%RECNAME((3+5*levso):(2+6*levso)) = 'spfh'
      gfsheadv%RECNAME((3+6*levso):(2+7*levso)) = 'o3mr'
      gfsheadv%RECNAME((3+7*levso):(2+8*levso)) = 'clwmr'
      do i=1,gfshead%ntrac-3
        gfsheadv%RECNAME((3+(i+7)*levso):(2+(i+8)*levso)) = aero_tracername(i)
      enddo
      if(gfshead%nrec>(2+(5+gfshead%ntrac)*levso)) then
        gfsheadv%RECNAME((3+(5+gfshead%ntrac)*levso):(2+(6+gfshead%ntrac)*levso)) = 'vvel'
      endif
!-----------------------
      endif
!-----------------------

      gfsheadv%RECLEVTYP(1:2) = 'sfc'
      gfsheadv%RECLEVTYP(3:nrec) = 'mid layer'
      gfsheadv%RECLEV(1:2) = 1
      do i=1,gfshead%nrec/levso
        DO K=1,levso
          jrec = 2 + (i-1)*levso + k
          gfsheadv%RECLEV(2+(i-1)*levso+k) = K
        ENDDO
      ENDDO

      CALL NEMSIO_OPEN(gfile,TRIM(filename),'write'                         &
     &,                MODELNAME="GFS"                                      &
     &,                GDATATYPE=gfshead%gdatatype                          &
     &,                NFHOUR=gfshead%NFHOUR                                &
     &,                NFMINUTE=gfshead%NFMINUTE                            &
     &,                NFSECONDN=gfshead%NFSECONDN                          &
     &,                NFSECONDD=gfshead%NFSECONDD                          &
     &,                IDATE=gfshead%IDATE                                  &
     &,                nrec=gfshead%nrec                                    &
     &,                DIMX=gfshead%DIMX                                    &
     &,                DIMY=gfshead%DIMY                                    &
     &,                DIMZ=gfshead%DIMZ                                    &
     &,                JCAP=gfshead%JCAP                                    &
     &,                NTRAC=gfshead%NTRAC                                  &
     &,                IDSL=gfshead%IDSL                                    &
     &,                IDVC=gfshead%IDVC                                    &
     &,                IDVM=gfshead%IDVM                                    &
     &,                NCLDT=gfshead%NCLDT                                  &
     &,                IDRT=gfshead%IDRT                                    &
     &,                RECNAME=gfsheadv%RECNAME                             &
     &,                RECLEVTYP=gfsheadv%RECLEVTYP                         &
     &,                RECLEV=gfsheadv%RECLEV                               &
     &,                VCOORD=gfsheadv%VCOORD                               &
     &,                LON=gfsheadv%LON                                     &
     &,                LAT=gfsheadv%LAT                                     &
     &,                CPI=gfsheadv%CPI                                     &
     &,                RI=gfsheadv%RI                                       &
     &,                EXTRAMETA=gfshead%EXTRAMETA                          &
     &,                NMETAVARI=gfshead%NMETAVARI                          &
     &,                NMETAVARR=gfshead%NMETAVARR                          &
     &,                NMETAVARL=gfshead%NMETAVARL                          &
     &,                NMETAARYI=gfshead%NMETAARYI                          &
     &,                NMETAARYR=gfshead%NMETAARYR                          &
     &,                VARINAME=gfsheadv%VARINAME                           &
     &,                VARIVAL=gfsheadv%VARIVAL                             &
     &,                VARRNAME=gfsheadv%VARRNAME                           &
     &,                VARRVAL=gfsheadv%VARRVAL                             &
     &,                VARLNAME=gfsheadv%VARLNAME                           &
     &,                VARLVAL=gfsheadv%VARLVAL                             &
     &,                ARYINAME=gfsheadv%ARYINAME                           &
     &,                ARYILEN=gfsheadv%ARYILEN                             &
     &,                ARYIVAL=gfsheadv%ARYIVAL                             &
     &,                ARYRNAME=gfsheadv%ARYRNAME                           &
     &,                ARYRLEN=gfsheadv%ARYRLEN                             &
     &,                ARYRVAL=gfsheadv%ARYRVAL                             &
     &,                IRET=ios)                                     
        iret = ios
        IF(ios.NE.0) THEN
            PRINT*, ' ERROR AT NEMSIO_OPEN ',trim(filename),'iret=',ios
            CALL ERREXIT(4)
        ENDIF
    endif
!
  end subroutine nemsio_gfsgrd_open
!
!-----------------------------------------------------------------------
  subroutine nemsio_gfssfc_open(gfile, filename, gaction, gfshead, gfsheadv, iret)
!-----------------------------------------------------------------------
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,   &
                              nemsio_open,nemsio_getheadvar
!
    implicit none
!
    type(nemsio_gfile),intent(inout)   :: gfile
    character*(*),intent(in)           :: filename
    character*(*),intent(in)           :: gaction
    type(nemsio_head),intent(inout)    :: gfshead
    type(nemsio_headv),intent(inout)   :: gfsheadv
    integer,intent(out)                :: iret
    integer ios,ios1,nrec
!
    if(trim(gaction)=="read" .or. trim(gaction)=="READ")then
      call nemsio_open(gfile,trim(filename),'read',ios)
      if(ios==0) then

       CALL NEMSIO_GETFILEHEAD(gfile                                      &
     &,                        MODELNAME=gfshead%MODELNAME                &
     &,                        NMETA=gfshead%NMETA                        &
     &,                        IDATE=gfshead%IDATE                        &
     &,                        NFHOUR=gfshead%NFHOUR                      &
     &,                        NFMINUTE=gfshead%NFMINUTE                  &
     &,                        NFSECONDN=gfshead%NFSECONDN                &
     &,                        NFSECONDD=gfshead%NFSECONDD                &
     &,                        VERSION=gfshead%VERSION                    &
     &,                        nrec=gfshead%nrec                          &
     &,                        DIMX=gfshead%dimx                          &
     &,                        DIMY=gfshead%dimy                          &
     &,                        DIMZ=gfshead%DIMZ                          &
     &,                        NSOIL=gfshead%NSOIL                        &
     &,                        IDRT=gfshead%IDRT                          &
     &,                        IDVM=gfshead%IDVM                          &
     &,                        NCLDT=gfshead%NCLDT                        &
     &,                        extrameta=gfshead%extrameta                &
     &,                        nmetavari=gfshead%nmetavari                &
     &,                        nmetavarr=gfshead%nmetavarr                &
     &,                        nmetaaryi=gfshead%nmetaaryi                &
     &,                        nmetaaryr=gfshead%nmetaaryr                &
     &,                        IRET=ios)

       
       CALL NEMSIO_GETHEADVAR(gfile,'irealf',gfshead%irealf,IRET)
       ALLOCATE(gfsheadv%LPL((gfshead%LATB+1)/2) )
       ALLOCATE(gfsheadv%ZSOIL(gfshead%NSOIL))
       CALL NEMSIO_GETHEADVAR(gfile,'lpl',gfsheadv%LPL,IRET)
       CALL NEMSIO_GETHEADVAR(gfile,'zsoil',gfsheadv%ZSOIL,IRET)
       CALL NEMSIO_GETHEADVAR(gfile,'IVSSFC',gfshead%IVS,IRET)
       CALL NEMSIO_GETHEADVAR(gfile,'fhour',gfshead%fhour,IRET)
!
       ALLOCATE(gfsheadv%RECNAME(gfshead%nrec))
       ALLOCATE(gfsheadv%RECLEVTYP(gfshead%nrec))
       ALLOCATE(gfsheadv%RECLEV(gfshead%nrec))
       ALLOCATE(gfsheadv%variname(gfshead%nmetavari))
       ALLOCATE(gfsheadv%varrname(gfshead%nmetavarr))
       ALLOCATE(gfsheadv%varival(gfshead%nmetavari))
       ALLOCATE(gfsheadv%varrval(gfshead%nmetavarr))
       ALLOCATE(gfsheadv%aryiname(gfshead%nmetaaryi))
       ALLOCATE(gfsheadv%aryrname(gfshead%nmetaaryr))
       ALLOCATE(gfsheadv%aryilen(gfshead%nmetaaryi))
       ALLOCATE(gfsheadv%aryrlen(gfshead%nmetaaryr))
!
        CALL NEMSIO_GETFILEHEAD(gfile                                      &
     &,                         RECNAME=gfsheadv%RECNAME                   &
     &,                         RECLEVTYP=gfsheadv%RECLEVTYP               &
     &,                         RECLEV=gfsheadv%RECLEV                     &
     &,                         variname=gfsheadv%variname                 &
     &,                         varrname=gfsheadv%varrname                 &
     &,                         varival=gfsheadv%varival                   &
     &,                         varrval=gfsheadv%varrval                   &
     &,                         aryiname=gfsheadv%aryiname                 &
     &,                         aryrname=gfsheadv%aryrname                 &
     &,                         aryilen=gfsheadv%aryilen                   &
     &,                         aryrlen=gfsheadv%aryrlen                   &
     &,                         IRET=ios1)
        if(gfshead%nmetaaryi > 0) then
          ALLOCATE(gfsheadv%aryival(maxval(gfsheadv%aryilen),      &
     &             gfshead%nmetaaryi))
          CALL NEMSIO_GETFILEHEAD(gfile,aryival=gfsheadv%aryival,iret=iret)
        endif
        if(gfshead%nmetaaryr > 0) then
          ALLOCATE(gfsheadv%aryrval(maxval(gfsheadv%aryrlen),      &
     &             gfshead%nmetaaryr))
          CALL NEMSIO_GETFILEHEAD(gfile,aryrval=gfsheadv%aryrval,iret=iret)
        endif

       endif
!
      else
!for write
        CALL NEMSIO_OPEN(gfile,trim(filename),'write'                        &
     &,                  MODELNAME="GFS"                                     &
     &,                  GDATATYPE="bin4"                                    &
     &,                  NFHOUR=gfshead%NFHOUR                               &
     &,                  NFMINUTE=gfshead%NFMINUTE                           &
     &,                  NFSECONDN=gfshead%NFSECONDN                         &
     &,                  NFSECONDD=gfshead%NFSECONDD                         &
     &,                  IDATE=gfshead%IDATE                                 &
     &,                  nrec=gfshead%nrec                                   &
     &,                  DIMX=gfshead%DIMX                                   &
     &,                  DIMY=gfshead%DIMY                                   &
     &,                  DIMZ=gfshead%DIMZ                                   &
     &,                  NSOIL=gfshead%NSOIL                                 &
     &,                  NMETA=gfshead%NMETA                                 &
     &,                  IDRT=gfshead%IDRT                                   &
     &,                  IDVM=gfshead%IDVM                                   &
     &,                  NCLDT=gfshead%NCLDT                                 &
     &,                  RECNAME=gfsheadv%RECNAME                            &
     &,                  RECLEVTYP=gfsheadv%RECLEVTYP                        &
     &,                  RECLEV=gfsheadv%RECLEV                              &
     &,                  EXTRAMETA=gfshead%EXTRAMETA                         &
     &,                  NMETAVARI=gfshead%NMETAVARI                         &  
     &,                  NMETAVARR=gfshead%NMETAVARR                         &
     &,                  NMETAARYI=gfshead%NMETAARYI                         &
     &,                  NMETAARYR=gfshead%NMETAARYR                         &
     &,                  VARINAME=gfsheadv%VARINAME                          &
     &,                  VARIVAL=gfsheadv%VARIVAL                            &
     &,                  VARRNAME=gfsheadv%VARRNAME                          &
     &,                  VARRVAL=gfsheadv%VARRVAL                            &
     &,                  ARYINAME=gfsheadv%ARYINAME                          &
     &,                  ARYILEN=gfsheadv%ARYILEN                            &
     &,                  ARYIVAL=gfsheadv%ARYIVAL                            &
     &,                  ARYRNAME=gfsheadv%ARYRNAME                          &
     &,                  ARYRLEN=gfsheadv%ARYRLEN                            &
     &,                  ARYRVAL=gfsheadv%ARYRVAL                            &
     &,                  IRET=ios)
        IF(ios.NE.0) THEN
          PRINT*, ' ERROR AT NEMSIO_OPEN chgres.out.sfn '
          CALL ERREXIT(4)
        ENDIF

       endif

!
  end subroutine nemsio_gfssfc_open
!
!
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_aldbta_sfc(im,jm,lsoil,nemsiodbta)
!-----------------------------------------------------------------------   
!
    implicit none
!
    integer,          intent(in)    :: im,jm,lsoil
    type(nemsio_dbta),intent(inout) :: nemsiodbta
!
!---allocate nemsio_dbdata with dimension (im,jm)
!
    allocate(nemsiodbta%tsea(im,jm))
    allocate(nemsiodbta%smc(im,jm,lsoil))
    allocate(nemsiodbta%sheleg(im,jm))
    allocate(nemsiodbta%stc(im,jm,lsoil))
    allocate(nemsiodbta%tg3(im,jm))
    allocate(nemsiodbta%zorl(im,jm))
!   allocate(nemsiodbta%cv(im,jm))
!   allocate(nemsiodbta%cvb(im,jm))
!   allocate(nemsiodbta%cvt(im,jm))
    allocate(nemsiodbta%alvsf(im,jm))
    allocate(nemsiodbta%alvwf(im,jm))
    allocate(nemsiodbta%alnsf(im,jm))
    allocate(nemsiodbta%alnwf(im,jm))
    allocate(nemsiodbta%slmsk(im,jm))
    allocate(nemsiodbta%vfrac(im,jm))
    allocate(nemsiodbta%canopy(im,jm))
    allocate(nemsiodbta%f10m(im,jm))
    allocate(nemsiodbta%t2m(im,jm))
    allocate(nemsiodbta%q2m(im,jm))
    allocate(nemsiodbta%vtype(im,jm))
    allocate(nemsiodbta%stype(im,jm))
    allocate(nemsiodbta%facsf(im,jm))
    allocate(nemsiodbta%facwf(im,jm))
    allocate(nemsiodbta%uustar(im,jm))
    allocate(nemsiodbta%ffmm(im,jm))
    allocate(nemsiodbta%ffhh(im,jm))
    allocate(nemsiodbta%hice(im,jm))
    allocate(nemsiodbta%fice(im,jm))
    allocate(nemsiodbta%tisfc(im,jm))
    allocate(nemsiodbta%tprcp(im,jm))
    allocate(nemsiodbta%srflag(im,jm))
    allocate(nemsiodbta%snwdph(im,jm))
    allocate(nemsiodbta%slc(im,jm,lsoil))
    allocate(nemsiodbta%shdmin(im,jm))
    allocate(nemsiodbta%shdmax(im,jm))
    allocate(nemsiodbta%slope(im,jm))
    allocate(nemsiodbta%snoalb(im,jm))
    allocate(nemsiodbta%orog(im,jm))
!
  end subroutine nemsio_gfs_aldbta_sfc
!
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_aldata_sfc(im,jm,lsoil,nemsiodata)
!-----------------------------------------------------------------------   
!
    implicit none
!
    integer,          intent(in)    :: im,jm,lsoil
    type(nemsio_data),intent(inout) :: nemsiodata
!
!---allocate nemsio_data with dimension (im,jm)
!
    allocate(nemsiodata%tsea(im,jm))
    allocate(nemsiodata%smc(im,jm,lsoil))
    allocate(nemsiodata%sheleg(im,jm))
    allocate(nemsiodata%stc(im,jm,lsoil))
    allocate(nemsiodata%tg3(im,jm))
    allocate(nemsiodata%zorl(im,jm))
!   allocate(nemsiodata%cv(im,jm))
!   allocate(nemsiodata%cvb(im,jm))
!   allocate(nemsiodata%cvt(im,jm))
    allocate(nemsiodata%alvsf(im,jm))
    allocate(nemsiodata%alvwf(im,jm))
    allocate(nemsiodata%alnsf(im,jm))
    allocate(nemsiodata%alnwf(im,jm))
    allocate(nemsiodata%slmsk(im,jm))
    allocate(nemsiodata%vfrac(im,jm))
    allocate(nemsiodata%canopy(im,jm))
    allocate(nemsiodata%f10m(im,jm))
    allocate(nemsiodata%t2m(im,jm))
    allocate(nemsiodata%q2m(im,jm))
    allocate(nemsiodata%vtype(im,jm))
    allocate(nemsiodata%stype(im,jm))
    allocate(nemsiodata%facsf(im,jm))
    allocate(nemsiodata%facwf(im,jm))
    allocate(nemsiodata%uustar(im,jm))
    allocate(nemsiodata%ffmm(im,jm))
    allocate(nemsiodata%ffhh(im,jm))
    allocate(nemsiodata%hice(im,jm))
    allocate(nemsiodata%fice(im,jm))
    allocate(nemsiodata%tisfc(im,jm))
    allocate(nemsiodata%tprcp(im,jm))
    allocate(nemsiodata%srflag(im,jm))
    allocate(nemsiodata%snwdph(im,jm))
    allocate(nemsiodata%slc(im,jm,lsoil))
    allocate(nemsiodata%shdmin(im,jm))
    allocate(nemsiodata%shdmax(im,jm))
    allocate(nemsiodata%slope(im,jm))
    allocate(nemsiodata%snoalb(im,jm))
    allocate(nemsiodata%orog(im,jm))

  end subroutine nemsio_gfs_aldata_sfc
!
!
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_axdbta_sfc(nemsiodbta)
!-----------------------------------------------------------------------   
!
    implicit none
!
    type(nemsio_dbta),intent(inout)  :: nemsiodbta
!
!---allocate nemsio_dbdata with dimension (im,jm)
!
    deallocate(nemsiodbta%tsea)
    deallocate(nemsiodbta%smc)
    deallocate(nemsiodbta%sheleg)
    deallocate(nemsiodbta%stc)
    deallocate(nemsiodbta%tg3)
    deallocate(nemsiodbta%zorl)
!   deallocate(nemsiodbta%cv)
!   deallocate(nemsiodbta%cvb)
!   deallocate(nemsiodbta%cvt)
    deallocate(nemsiodbta%alvsf)
    deallocate(nemsiodbta%alvwf)
    deallocate(nemsiodbta%alnsf)
    deallocate(nemsiodbta%alnwf)
    deallocate(nemsiodbta%slmsk)
    deallocate(nemsiodbta%vfrac)
    deallocate(nemsiodbta%canopy)
    deallocate(nemsiodbta%f10m)
    deallocate(nemsiodbta%t2m)
    deallocate(nemsiodbta%q2m)
    deallocate(nemsiodbta%vtype)
    deallocate(nemsiodbta%stype)
    deallocate(nemsiodbta%facsf)
    deallocate(nemsiodbta%facwf)
    deallocate(nemsiodbta%uustar)
    deallocate(nemsiodbta%ffmm)
    deallocate(nemsiodbta%ffhh)
    deallocate(nemsiodbta%hice)
    deallocate(nemsiodbta%fice)
    deallocate(nemsiodbta%tisfc)
    deallocate(nemsiodbta%tprcp)
    deallocate(nemsiodbta%srflag)
    deallocate(nemsiodbta%snwdph)
    deallocate(nemsiodbta%slc)
    deallocate(nemsiodbta%shdmin)
    deallocate(nemsiodbta%shdmax)
    deallocate(nemsiodbta%slope)
    deallocate(nemsiodbta%snoalb)
    deallocate(nemsiodbta%orog)

  end subroutine nemsio_gfs_axdbta_sfc

!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_axdata_sfc(nemsiodata)
!-----------------------------------------------------------------------   
!
    implicit none
!
    type(nemsio_data),intent(inout)  :: nemsiodata
!
!---allocate nemsio_dbdata with dimension (im,jm)
!
    deallocate(nemsiodata%tsea)
    deallocate(nemsiodata%smc)
    deallocate(nemsiodata%sheleg)
    deallocate(nemsiodata%stc)
    deallocate(nemsiodata%tg3)
    deallocate(nemsiodata%zorl)
!   deallocate(nemsiodata%cv)
!   deallocate(nemsiodata%cvb)
!   deallocate(nemsiodata%cvt)
    deallocate(nemsiodata%alvsf)
    deallocate(nemsiodata%alvwf)
    deallocate(nemsiodata%alnsf)
    deallocate(nemsiodata%alnwf)
    deallocate(nemsiodata%slmsk)
    deallocate(nemsiodata%vfrac)
    deallocate(nemsiodata%canopy)
    deallocate(nemsiodata%f10m)
    deallocate(nemsiodata%t2m)
    deallocate(nemsiodata%q2m)
    deallocate(nemsiodata%vtype)
    deallocate(nemsiodata%stype)
    deallocate(nemsiodata%facsf)
    deallocate(nemsiodata%facwf)
    deallocate(nemsiodata%uustar)
    deallocate(nemsiodata%ffmm)
    deallocate(nemsiodata%ffhh)
    deallocate(nemsiodata%hice)
    deallocate(nemsiodata%fice)
    deallocate(nemsiodata%tisfc)
    deallocate(nemsiodata%tprcp)
    deallocate(nemsiodata%srflag)
    deallocate(nemsiodata%snwdph)
    deallocate(nemsiodata%slc)
    deallocate(nemsiodata%shdmin)
    deallocate(nemsiodata%shdmax)
    deallocate(nemsiodata%slope)
    deallocate(nemsiodata%snoalb)
    deallocate(nemsiodata%orog)

  end subroutine nemsio_gfs_axdata_sfc
!
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_alheadv(nemsiohead,nemsioheadv)
!-----------------------------------------------------------------------
!
    implicit none
!
    type(nemsio_head), intent(in)     :: nemsiohead
    type(nemsio_headv),intent(inout)  :: nemsioheadv
    integer dimx,dimy,dimz,fieldsize
!
    allocate(nemsioheadv%vcoord(nemsiohead%dimz+1,3,2))
    dimx = nemsiohead%dimx
    dimy = nemsiohead%dimy
    dimz = nemsiohead%dimz
    fieldsize = dimx*dimy

    allocate(nemsioheadv%lat(fieldsize))
    allocate(nemsioheadv%lon(fieldsize))
    allocate(nemsioheadv%dx(fieldsize))
    allocate(nemsioheadv%dy(fieldsize))
    allocate(nemsioheadv%cpi(nemsiohead%ntrac+1))
    allocate(nemsioheadv%ri(nemsiohead%ntrac+1))
    allocate(nemsioheadv%recname(nemsiohead%nrec))
    allocate(nemsioheadv%reclevtyp(nemsiohead%nrec))
    allocate(nemsioheadv%reclev(nemsiohead%nrec))
!
    if(nemsiohead%nmetavari > 0) allocate(nemsioheadv%variname(nemsiohead%nmetavari))
    if(nemsiohead%nmetavari > 0) allocate(nemsioheadv%varival(nemsiohead%nmetavari))
    if(nemsiohead%nmetavarr > 0) allocate(nemsioheadv%varrname(nemsiohead%nmetavarr))
    if(nemsiohead%nmetavarr > 0) allocate(nemsioheadv%varrval(nemsiohead%nmetavarr))
    if(nemsiohead%nmetavarl > 0) allocate(nemsioheadv%varlname(nemsiohead%nmetavarl))
    if(nemsiohead%nmetavarl > 0) allocate(nemsioheadv%varlval(nemsiohead%nmetavarl))
    if(nemsiohead%nmetaaryi > 0) allocate(nemsioheadv%aryiname(nemsiohead%nmetaaryi))
    if(nemsiohead%nmetaaryi > 0) allocate(nemsioheadv%aryilen(nemsiohead%nmetaaryi))
    if(nemsiohead%nmetaaryr > 0) allocate(nemsioheadv%aryrname(nemsiohead%nmetaaryr))
    if(nemsiohead%nmetaaryr > 0) allocate(nemsioheadv%aryrlen(nemsiohead%nmetaaryr))

  end subroutine nemsio_gfs_alheadv
!
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_axheadv(nemsioheadv)
!-----------------------------------------------------------------------
!
    implicit none
!
    type(nemsio_headv),intent(inout)  :: nemsioheadv
!
    if(allocated(nemsioheadv%vcoord) )   deallocate(nemsioheadv%vcoord)
    if(allocated(nemsioheadv%lat))       deallocate(nemsioheadv%lat)
    if(allocated(nemsioheadv%lon))       deallocate(nemsioheadv%lon)
    if(allocated(nemsioheadv%dx))        deallocate(nemsioheadv%dx)
    if(allocated(nemsioheadv%dy))        deallocate(nemsioheadv%dy)
    if(allocated(nemsioheadv%cpi))       deallocate(nemsioheadv%cpi)
    if(allocated(nemsioheadv%ri))        deallocate(nemsioheadv%ri)
    if(allocated(nemsioheadv%recname))   deallocate(nemsioheadv%recname)
    if(allocated(nemsioheadv%reclevtyp)) deallocate(nemsioheadv%reclevtyp)
    if(allocated(nemsioheadv%reclev))    deallocate(nemsioheadv%reclev)
!
    if(allocated(nemsioheadv%lpl))       deallocate(nemsioheadv%lpl)
    if(allocated(nemsioheadv%zsoil))     deallocate(nemsioheadv%zsoil)
!
    if(allocated(nemsioheadv%variname))  deallocate(nemsioheadv%variname)
    if(allocated(nemsioheadv%varival))   deallocate(nemsioheadv%varival)
    if(allocated(nemsioheadv%varrname))  deallocate(nemsioheadv%varrname)
    if(allocated(nemsioheadv%varrval))   deallocate(nemsioheadv%varrval)
    if(allocated(nemsioheadv%varlname))  deallocate(nemsioheadv%varlname)
    if(allocated(nemsioheadv%varlval))   deallocate(nemsioheadv%varlval)
    if(allocated(nemsioheadv%aryiname))  deallocate(nemsioheadv%aryiname)
    if(allocated(nemsioheadv%aryilen))   deallocate(nemsioheadv%aryilen)
    if(allocated(nemsioheadv%aryival))   deallocate(nemsioheadv%aryival)
    if(allocated(nemsioheadv%aryrname))  deallocate(nemsioheadv%aryrname)
    if(allocated(nemsioheadv%aryrlen))   deallocate(nemsioheadv%aryrlen)
    if(allocated(nemsioheadv%aryrval))   deallocate(nemsioheadv%aryrval)

  end subroutine nemsio_gfs_axheadv
!   
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_rdsfc8(gfile,nemsiodbta,iret)
!-----------------------------------------------------------------------   
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none
!
    type(nemsio_gfile),intent(inout) :: gfile
    type(nemsio_dbta),intent(inout)  :: nemsiodbta
    integer, optional,intent(out)    :: iret
!local
    integer im,jm,nsoil,l,fieldsize,ntrac,ierr
    real(dblekind),allocatable       :: tmp(:)
!
!---read out data from nemsio file
!
    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,nsoil=nsoil,ntrac=ntrac,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
    im = size(nemsiodbta%tsea,1)
    jm = size(nemsiodbta%tsea,2)
    if(im*jm/=fieldsize) then
       print *,'ERROR: dimension not match'
       return
    endif
    allocate(tmp(fieldsize))
    if(present(iret)) iret=0
!tsea
    call nemsio_readrecv(gfile,'tmp','sfc',1,tmp,iret=ierr)
    if(ierr==0) then
       nemsiodbta%tsea=reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (tsea), iret=', ierr
    endif
!smc
    do l=1,nsoil
      call nemsio_readrecv(gfile,'smc','soil layer',l,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodbta%smc(:,:,l) = reshape(tmp,(/im,jm/))
      else
       if(present(iret)) iret = ierr
        print *, 'ERROR in rdsfc (smc), iret=', ierr, ', l=',l
      endif
    enddo
!sheleg
    call nemsio_readrecv(gfile,'weasd','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%sheleg(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (sheleg), iret=', ierr
    endif
!stc
    do l=1,nsoil
      call nemsio_readrecv(gfile,'stc','soil layer',l,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodbta%stc(:,:,l) = reshape(tmp,(/im,jm/))
      else
       if(present(iret)) iret = ierr
        print *, 'ERROR in rdsfc (stc), iret=', ierr, ', l=',l
      endif
    enddo
!tg3
    call nemsio_readrecv(gfile,'tg3','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%tg3(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (tg3), iret=', ierr
    endif
!zorl
    call nemsio_readrecv(gfile,'sfcr','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%zorl(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (zorl), iret=', ierr
    endif
!!cv
!    call nemsio_readrecv(gfile,'tcdc','convect-cld lay',1,tmp,iret=ierr)
!    if(ierr==0) then
!       nemsiodbta%cv(:,:)=reshape(tmp,(/im,jm/) )
!    else
!       if(present(iret)) iret=ierr
!       print *, 'ERROR in rdsfc (cv), iret=', ierr
!    endif
!!cvb
!    call nemsio_readrecv(gfile,'pres','convect-cld bot',1,tmp,iret=ierr)
!    if(ierr==0) then
!       nemsiodbta%cvb(:,:)=reshape(tmp,(/im,jm/) )
!    else
!       if(present(iret)) iret=ierr
!       print *, 'ERROR in rdsfc (cvb), iret=', ierr
!    endif
!!cvt
!    call nemsio_readrecv(gfile,'pres','convect-cld top',1,tmp,iret=ierr)
!    if(ierr==0) then
!       nemsiodbta%cvt(:,:)=reshape(tmp,(/im,jm/) )
!    else
!       if(present(iret)) iret=ierr
!       print *, 'ERROR in rdsfc (cvt), iret=', ierr
!    endif
!alvsf
    call nemsio_readrecv(gfile,'alvsf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%alvsf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (alvsf), iret=', ierr
    endif
!alvwf
    call nemsio_readrecv(gfile,'alvwf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%alvwf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (alvwf), iret=', ierr
    endif
!alnsf
    call nemsio_readrecv(gfile,'alnsf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%alnsf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (alnsf), iret=', ierr
    endif
!alnwf
    call nemsio_readrecv(gfile,'alnwf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%alnwf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (alnwf), iret=', ierr
    endif
!slmsk
    call nemsio_readrecv(gfile,'land','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%slmsk(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (slmsk), iret=', ierr
    endif
!vfrac
    call nemsio_readrecv(gfile,'veg','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%vfrac(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (vfrac), iret=', ierr
    endif
!canopy
    call nemsio_readrecv(gfile,'cnwat','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%canopy(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (canopy), iret=', ierr
    endif
!f10m
    call nemsio_readrecv(gfile,'f10m','10 m above gnd',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%f10m(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (f10m), iret=', ierr
    endif
!t2m
    call nemsio_readrecv(gfile,'tmp','2 m above gnd',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%t2m(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (t2m), iret=', ierr
    endif
!q2m
    call nemsio_readrecv(gfile,'spfh','2 m above gnd',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%q2m(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (q2m), iret=', ierr
    endif
!vtype
    call nemsio_readrecv(gfile,'vtype','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodbta%vtype(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (vtype), iret=', ierr
    endif
!stype
    call nemsio_readrecv(gfile,'sotyp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodbta%stype(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (stype), iret=', ierr
    endif
!facsf
    call nemsio_readrecv(gfile,'facsf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%facsf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (facsf), iret=', ierr
    endif
!facwf
    call nemsio_readrecv(gfile,'facwf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodbta%facwf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (facwf), iret=', ierr
    endif
!uustar
    call nemsio_readrecv(gfile,'fricv','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%uustar(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (uustar), iret=', ierr
    endif
!ffmm
    call nemsio_readrecv(gfile,'ffmm','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%ffmm(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (ffmm), iret=', ierr
    endif
!ffhh
    call nemsio_readrecv(gfile,'ffhh','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%ffhh(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (ffhh), iret=', ierr
    endif
!hice
    call nemsio_readrecv(gfile,'icetk','sfc',1,tmp,iret=ierr)
    if(ierr==0) then
       nemsiodbta%hice(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (hice), iret=', ierr
    endif
!fice
    call nemsio_readrecv(gfile,'icec','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%fice(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (fice), iret=', ierr
    endif
!tisfc
    call nemsio_readrecv(gfile,'tisfc','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodbta%tisfc(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (tisfc), iret=', ierr
    endif
!tprcp
    call nemsio_readrecv(gfile,'tprcp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%tprcp(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (tprcp), iret=', ierr
    endif
!srflag
    call nemsio_readrecv(gfile,'crain','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%srflag(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (srflag), iret=', ierr
    endif
!snwdph
    call nemsio_readrecv(gfile,'snod','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%snwdph(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (snwdph), iret=', ierr
    endif
!slc
    do l=1,nsoil
      call nemsio_readrecv(gfile,'slc','soil layer',l,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodbta%slc(:,:,l) = reshape(tmp,(/im,jm/))
      else
       if(present(iret)) iret = ierr
        print *, 'ERROR in rdsfc (slc), iret=', ierr, ', l=',l
      endif
    enddo
!shdmin
    call nemsio_readrecv(gfile,'shdmin','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%shdmin(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (shdmin), iret=', ierr
    endif
!shdmax
    call nemsio_readrecv(gfile,'shdmax','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%shdmax(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (shdmax), iret=', ierr
    endif
!slope
    call nemsio_readrecv(gfile,'sltyp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%slope(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (slope), iret=', ierr
    endif
!snoalb
    call nemsio_readrecv(gfile,'salbd','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%snoalb(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (snoalb), iret=', ierr
    endif
!orog
    call nemsio_readrecv(gfile,'orog','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodbta%orog(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc (orog), iret=', ierr
    endif
    deallocate(tmp)
!
  end subroutine nemsio_gfs_rdsfc8
!   
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_rdsfc4(gfile,nemsiodata,iret)
!-----------------------------------------------------------------------   
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none
!
    type(nemsio_gfile),intent(inout) :: gfile
    type(nemsio_data), intent(inout) :: nemsiodata
    integer,optional,  intent(out)   :: iret
!local
    integer im,jm,nsoil,l,fieldsize,ierr
    real(realkind),allocatable       :: tmp(:)
!
!---read out data from nemsio file
!
    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,nsoil=nsoil,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
    im = size(nemsiodata%tsea,1)
    jm = size(nemsiodata%tsea,2)
    if(im*jm /= fieldsize) then
       print *,'ERROR: dimension not match'
       return
    endif
    allocate(tmp(fieldsize))
    if(present(iret)) iret = 0
!tsea
    call nemsio_readrecv(gfile,'tmp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%tsea = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (tsea), iret=', ierr
    endif
!smc
    do l=1,nsoil
      call nemsio_readrecv(gfile,'smc','soil layer',l,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodata%smc(:,:,l) = reshape(tmp,(/im,jm/))
      else
       if(present(iret)) iret = ierr
        print *, 'ERROR in rdsfc4 (smc), iret=', ierr, ', l=',l
      endif
    enddo
!sheleg
    call nemsio_readrecv(gfile,'weasd','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%sheleg(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (sheleg), iret=', ierr
    endif
!stc
    do l=1,nsoil
      call nemsio_readrecv(gfile,'stc','soil layer',l,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodata%stc(:,:,l) = reshape(tmp,(/im,jm/))
      else
       if(present(iret)) iret = ierr
        print *, 'ERROR in rdsfc4 (stc), iret=', ierr, ', l=',l
      endif
    enddo
!tg3
    call nemsio_readrecv(gfile,'tg3','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%tg3(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (tg3), iret=', ierr
    endif
!zorl
    call nemsio_readrecv(gfile,'sfcr','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%zorl(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (zorl), iret=', ierr
    endif
!!cv
!    call nemsio_readrecv(gfile,'tcdc','convect-cld lay',1,tmp,iret=ierr)
!    if(ierr==0) then
!       nemsiodata%cv(:,:)=reshape(tmp,(/im,jm/) )
!    else
!       if(present(iret)) iret=ierr
!       print *, 'ERROR in rdsfc4 (cv), iret=', ierr
!    endif
!!cvb
!    call nemsio_readrecv(gfile,'pres','convect-cld bot',1,tmp,iret=ierr)
!    if(ierr==0) then
!       nemsiodata%cvb(:,:)=reshape(tmp,(/im,jm/) )
!    else
!       if(present(iret)) iret=ierr
!       print *, 'ERROR in rdsfc4 (cvb), iret=', ierr
!    endif
!!cvt
!    call nemsio_readrecv(gfile,'pres','convect-cld top',1,tmp,iret=ierr)
!    if(ierr==0) then
!       nemsiodata%cvt(:,:)=reshape(tmp,(/im,jm/) )
!    else
!       if(present(iret)) iret=ierr
!       print *, 'ERROR in rdsfc4 (cvt), iret=', ierr
!    endif
!alvsf
    call nemsio_readrecv(gfile,'alvsf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%alvsf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (alvsf), iret=', ierr
    endif
!alvwf
    call nemsio_readrecv(gfile,'alvwf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%alvwf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (alvwf), iret=', ierr
    endif
!alnsf
    call nemsio_readrecv(gfile,'alnsf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%alnsf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (alnsf), iret=', ierr
    endif
!alnwf
    call nemsio_readrecv(gfile,'alnwf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%alnwf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (alnwf), iret=', ierr
    endif
!slmsk
    call nemsio_readrecv(gfile,'land','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%slmsk(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (slmsk), iret=', ierr
    endif
!vfrac
    call nemsio_readrecv(gfile,'veg','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%vfrac(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (vfrac), iret=', ierr
    endif
!canopy
    call nemsio_readrecv(gfile,'cnwat','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%canopy(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (canopy), iret=', ierr
    endif
!f10m
    call nemsio_readrecv(gfile,'f10m','10 m above gnd',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%f10m(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (f10m), iret=', ierr
    endif
!t2m
    call nemsio_readrecv(gfile,'tmp','2 m above gnd',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%t2m(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (t2m), iret=', ierr
    endif
!q2m
    call nemsio_readrecv(gfile,'spfh','2 m above gnd',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%q2m(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (q2m), iret=', ierr
    endif
!vtype
    call nemsio_readrecv(gfile,'vtype','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodata%vtype(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (vtype), iret=', ierr
    endif
!stype--sotyp
    call nemsio_readrecv(gfile,'sotyp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodata%stype(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (stype), iret=', ierr
    endif
!facsf
    call nemsio_readrecv(gfile,'facsf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%facsf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (facsf), iret=', ierr
    endif
!facwf
    call nemsio_readrecv(gfile,'facwf','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodata%facwf(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (facwf), iret=', ierr
    endif
!uustar-fricv
    call nemsio_readrecv(gfile,'fricv','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%uustar(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (uustar), iret=', ierr
    endif
!ffmm
    call nemsio_readrecv(gfile,'ffmm','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%ffmm(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (ffmm), iret=', ierr
    endif
!ffhh
    call nemsio_readrecv(gfile,'ffhh','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%ffhh(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (ffhh), iret=', ierr
    endif
!hice
    call nemsio_readrecv(gfile,'icetk','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%hice(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (hice), iret=', ierr
    endif
!fice
    call nemsio_readrecv(gfile,'icec','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%fice(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (fice), iret=', ierr
    endif
!tisfc
    call nemsio_readrecv(gfile,'tisfc','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then 
       nemsiodata%tisfc(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (tisfc), iret=', ierr
    endif
!tprcp
    call nemsio_readrecv(gfile,'tprcp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%tprcp(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (tprcp), iret=', ierr
    endif
!srflag--crain
    call nemsio_readrecv(gfile,'crain','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%srflag(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (srflag), iret=', ierr
    endif
!snwdph
    call nemsio_readrecv(gfile,'snod','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%snwdph(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (snwdph), iret=', ierr
    endif
!slc
    do l=1,nsoil
      call nemsio_readrecv(gfile,'slc','soil layer',l,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodata%slc(:,:,l) = reshape(tmp,(/im,jm/))
      else
       if(present(iret)) iret = ierr
        print *, 'ERROR in rdsfc4 (slc), iret=', ierr, ', l=',l
      endif
    enddo
!shdmin
    call nemsio_readrecv(gfile,'shdmin','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%shdmin(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (shdmin), iret=', ierr
    endif
!shdmax
    call nemsio_readrecv(gfile,'shdmax','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%shdmax(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (shdmax), iret=', ierr
    endif
!slope
    call nemsio_readrecv(gfile,'sltyp','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%slope(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (slope), iret=', ierr
    endif
!snoalb
    call nemsio_readrecv(gfile,'salbd','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%snoalb(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (snoalb), iret=', ierr
    endif
!orog
    call nemsio_readrecv(gfile,'orog','sfc',1,tmp,iret=ierr)
    if(ierr == 0) then
       nemsiodata%orog(:,:) = reshape(tmp,(/im,jm/) )
    else
       if(present(iret)) iret = ierr
       print *, 'ERROR in rdsfc4 (orog), iret=', ierr
    endif
    deallocate(tmp)
!
  end subroutine nemsio_gfs_rdsfc4
!   
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_wrtsfc8(gfile,nemsiodbta,iret)
!-----------------------------------------------------------------------   
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,nemsio_writerecv
    implicit none
!
    type(nemsio_gfile),intent(inout) :: gfile
    type(nemsio_dbta),intent(inout)  :: nemsiodbta
    integer,optional, intent(out):: iret
!local
    integer im,jm,nsoil,l,fieldsize,ierr
    real(dblekind),allocatable ::tmp(:)
!
!---read out data from nemsio file
!
    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,nsoil=nsoil,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
!    print *,'in nemsio_gfs_wrtsfc,im=',im,'jm=',jm,fieldsize
    allocate(tmp(fieldsize))
!    print *,'nsoil=',nemsiohead%nsoil
    if(present(iret)) iret = 0
!tsea
    tmp = reshape(nemsiodbta%tsea,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tmp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR in wrtsfc (tsea), iret=', ierr
    endif
!smc
    do l=1,nsoil
      tmp = reshape(nemsiodbta%smc(:,:,l),(/fieldsize/) )
      call nemsio_writerecv(gfile,'smc','soil layer',l,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'ERROR in wrtsfc (smc), iret=', ierr,',l=',l
      endif
    enddo
!sheleg--weasd
    tmp = reshape(nemsiodbta%sheleg,(/fieldsize/) )
    call nemsio_writerecv(gfile,'weasd','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (sheleg), iret=', ierr
    endif
!stc
    do l=1,nsoil
      tmp = reshape(nemsiodbta%stc(:,:,l),(/fieldsize/) )
      call nemsio_writerecv(gfile,'stc','soil layer',l,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'ERROR in wrtsfc (stc), iret=', ierr,',l=',l
      endif
    enddo
!tg3
    tmp = reshape(nemsiodbta%tg3,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tg3','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR in wrtsfc (tg3), iret=', ierr
    endif
!zorl
    tmp = reshape(nemsiodbta%zorl,(/fieldsize/) )
    call nemsio_writerecv(gfile,'sfcr','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (zorl), iret=', ierr
    endif
!cv
!    tmp=reshape(nemsiodbta%cv,(/fieldsize/) )
!    call nemsio_writerecv(gfile,'tcdc','convect-cld lay',1,tmp,iret=ierr)
!    if(ierr/=0) then
!      if(present(iret)) iret=ierr
!      print *,'ERROR in wrtsfc (cv), iret=', ierr
!    endif
!cvb
!    tmp=reshape(nemsiodbta%cvb,(/fieldsize/) )
!    call nemsio_writerecv(gfile,'pres','convect-cld bot',1,tmp,iret=ierr)
!    if(ierr/=0) then
!      if(present(iret)) iret=ierr
!      print *,'ERROR in wrtsfc (cvb), iret=', ierr
!    endif
!cvt
!    tmp=reshape(nemsiodbta%cvt,(/fieldsize/) )
!    call nemsio_writerecv(gfile,'pres','convect-cld top',1,tmp,iret=ierr)
!    if(ierr/=0) then
!      if(present(iret)) iret=ierr
!      print *,'ERROR in wrtsfc (cvt), iret=', ierr
!    endif
!alvsf
    tmp = reshape(nemsiodbta%alvsf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alvsf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (alvsf), iret=', ierr
    endif
!alvwf
    tmp = reshape(nemsiodbta%alvwf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alvwf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (alvwf), iret=', ierr
    endif
!alnsf
    tmp = reshape(nemsiodbta%alnsf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alnsf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (alnsf), iret=', ierr
    endif
!alnwf
    tmp = reshape(nemsiodbta%alnwf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alnwf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (almwf), iret=', ierr
    endif
!slmsk
    tmp = reshape(nemsiodbta%slmsk,(/fieldsize/) )
    call nemsio_writerecv(gfile,'land','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (slmsk), iret=', ierr
    endif
!vfrac
    tmp = reshape(nemsiodbta%vfrac,(/fieldsize/) )
    call nemsio_writerecv(gfile,'veg','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (vfrac), iret=', ierr
    endif
!canopy
    tmp = reshape(nemsiodbta%canopy,(/fieldsize/) )
    call nemsio_writerecv(gfile,'cnwat','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (canopy), iret=', ierr
    endif
!f10m
    tmp = reshape(nemsiodbta%f10m,(/fieldsize/) )
    call nemsio_writerecv(gfile,'f10m','10 m above gnd',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (f10m), iret=', ierr
    endif
!t2m
    tmp = reshape(nemsiodbta%t2m,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tmp','2 m above gnd',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (t2m), iret=', ierr
    endif
!q2m
    tmp = reshape(nemsiodbta%q2m,(/fieldsize/) )
    call nemsio_writerecv(gfile,'spfh','2 m above gnd',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (q2m), iret=', ierr
    endif
!vtype
    tmp = reshape(nemsiodbta%vtype,(/fieldsize/) )
    call nemsio_writerecv(gfile,'vtype','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (vtype), iret=', ierr
    endif
!stype--sotyp
    tmp = reshape(nemsiodbta%stype,(/fieldsize/) )
    call nemsio_writerecv(gfile,'sotyp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (stype), iret=', ierr
    endif
!facsf
    tmp = reshape(nemsiodbta%facsf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'facsf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (facsf), iret=', ierr
    endif
!facwf
    tmp = reshape(nemsiodbta%facwf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'facwf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (facwf), iret=', ierr
    endif
!uustar--fricv
    tmp = reshape(nemsiodbta%uustar,(/fieldsize/) )
    call nemsio_writerecv(gfile,'fricv','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (uustar), iret=', ierr
    endif
!ffmm
    tmp = reshape(nemsiodbta%ffmm,(/fieldsize/) )
    call nemsio_writerecv(gfile,'ffmm','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (ffmm), iret=', ierr
    endif
!ffhh
    tmp = reshape(nemsiodbta%ffhh,(/fieldsize/) )
    call nemsio_writerecv(gfile,'ffhh','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (ffhh), iret=', ierr
    endif
!hice   
    tmp = reshape(nemsiodbta%hice,(/fieldsize/) )
    call nemsio_writerecv(gfile,'icetk','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (hice), iret=', ierr
    endif
!fice
    tmp = reshape(nemsiodbta%fice,(/fieldsize/) )
    call nemsio_writerecv(gfile,'icec','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (fice), iret=', ierr
    endif
!tisfc
    tmp = reshape(nemsiodbta%tisfc,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tisfc','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (tisfc), iret=', ierr
    endif
!tprcp
    tmp = reshape(nemsiodbta%tprcp,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tprcp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (tprcp), iret=', ierr
    endif
!srflag--crain
    tmp = reshape(nemsiodbta%srflag,(/fieldsize/) )
    call nemsio_writerecv(gfile,'crain','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (srflag), iret=', ierr
    endif
!snwdph
    tmp = reshape(nemsiodbta%snwdph,(/fieldsize/) )
    call nemsio_writerecv(gfile,'snod','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (snwdph), iret=', ierr
    endif
!slc
    do l=1,nsoil
      tmp = reshape(nemsiodbta%slc(:,:,l),(/fieldsize/) )
      call nemsio_writerecv(gfile,'slc','soil layer',l,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'ERROR in wrtsfc (slc), iret=', ierr
      endif
    enddo
!shdmin
    tmp = reshape(nemsiodbta%shdmin,(/fieldsize/) )
    call nemsio_writerecv(gfile,'shdmin','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (shdmin), iret=', ierr
    endif
!shdmax
    tmp = reshape(nemsiodbta%shdmax,(/fieldsize/) )
    call nemsio_writerecv(gfile,'shdmax','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (shdmax), iret=', ierr
    endif
!slope
    tmp = reshape(nemsiodbta%slope,(/fieldsize/) )
    call nemsio_writerecv(gfile,'sltyp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (slope), iret=', ierr
    endif
!snoalb
    tmp = reshape(nemsiodbta%snoalb,(/fieldsize/) )
    call nemsio_writerecv(gfile,'salbd','sfc',1,tmp,iret=ierr)
    if(ierr /= 0 ) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (snoalb), iret=', ierr
    endif
!orog
    tmp = reshape(nemsiodbta%orog,(/fieldsize/) )
    call nemsio_writerecv(gfile,'orog','sfc',1,tmp,iret=ierr)
    if(ierr /= 0 ) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc (orog), iret=', ierr
    endif
!
    deallocate(tmp)
!
  end subroutine nemsio_gfs_wrtsfc8
!
!-----------------------------------------------------------------------   
  subroutine nemsio_gfs_wrtsfc4(gfile,nemsiodata,iret)
!-----------------------------------------------------------------------   
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,nemsio_writerecv
    implicit none
!
    type(nemsio_gfile),intent(inout) :: gfile
    type(nemsio_data), intent(inout) :: nemsiodata
    integer,optional,  intent(out)   :: iret
!local
    integer im,jm,nsoil,l,fieldsize,ierr
    real(realkind),allocatable       :: tmp(:)
!
!---read out data from nemsio file
!
    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,nsoil=nsoil,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
!    print *,'in nemsio_gfs_wrtsfc,im=',im,'jm=',jm,fieldsize
    allocate(tmp(fieldsize))
!    print *,'nsoil=',nsoil
    if(present(iret)) iret = 0
!tsea
    tmp = reshape(nemsiodata%tsea,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tmp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (tsea), iret=', ierr
    endif
!smc
    do l=1,nsoil
      tmp = reshape(nemsiodata%smc(:,:,l),(/fieldsize/) )
      call nemsio_writerecv(gfile,'smc','soil layer',l,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'ERROR in wrtsfc4 (smc), iret=', ierr,',l=',l
      endif
    enddo
!sheleg
    tmp = reshape(nemsiodata%sheleg,(/fieldsize/) )
    call nemsio_writerecv(gfile,'weasd','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (sheleg), iret=', ierr
    endif
!stc
    do l=1,nsoil
      tmp = reshape(nemsiodata%stc(:,:,l),(/fieldsize/) )
      call nemsio_writerecv(gfile,'stc','soil layer',l,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'ERROR in wrtsfc4 (stc), iret=', ierr,',l=',l
      endif
    enddo
!tg3
    tmp = reshape(nemsiodata%tg3,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tg3','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (tg3), iret=', ierr
    endif
!zorl
    tmp = reshape(nemsiodata%zorl,(/fieldsize/) )
    call nemsio_writerecv(gfile,'sfcr','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (zorl), iret=', ierr
    endif
!!cv
!    tmp=reshape(nemsiodata%cv,(/fieldsize/) )
!    call nemsio_writerecv(gfile,'tcdc','convect-cld lay',1,tmp,iret=ierr)
!    if(ierr/=0) then
!      if(present(iret)) iret=ierr
!      print *,'ERROR in wrtsfc4 (cv), iret=', ierr
!    endif
!!cvb
!    tmp=reshape(nemsiodata%cvb,(/fieldsize/) )
!    call nemsio_writerecv(gfile,'pres','convect-cld bot',1,tmp,iret=ierr)
!    if(ierr/=0) then
!      if(present(iret)) iret=ierr
!      print *,'ERROR in wrtsfc4 (cvb), iret=', ierr
!    endif
!    if(ierr/=0) print *,'ERROR in wrtsfc4 (cvb), iret=', ierr
!!cvt
!    tmp = reshape(nemsiodata%cvt,(/fieldsize/) )
!    call nemsio_writerecv(gfile,'pres','convect-cld top',1,tmp,iret=ierr)
!    if(ierr /= 0) then
!      if(present(iret)) iret = ierr
!      print *,'ERROR in wrtsfc4 (cvt), iret=', ierr
!    endif
!!alvsf
     tmp = reshape(nemsiodata%alvsf,(/fieldsize/) )
     call nemsio_writerecv(gfile,'alvsf','sfc',1,tmp,iret=ierr)
     if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR in wrtsfc4 (alvsf), iret=', ierr
     endif
!alvwf
    tmp = reshape(nemsiodata%alvwf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alvwf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (alvwf), iret=', ierr
    endif
!alnsf
    tmp = reshape(nemsiodata%alnsf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alnsf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (alnsf), iret=', ierr
    endif
!alnwf
    tmp = reshape(nemsiodata%alnwf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'alnwf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (alnwf), iret=', ierr
    endif
!slmsk
    tmp = reshape(nemsiodata%slmsk,(/fieldsize/) )
    call nemsio_writerecv(gfile,'land','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (land), iret=', ierr
    endif
!vfrac
    tmp = reshape(nemsiodata%vfrac,(/fieldsize/) )
    call nemsio_writerecv(gfile,'veg','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (vfrac), iret=', ierr
    endif
!canopy
    tmp = reshape(nemsiodata%canopy,(/fieldsize/) )
    call nemsio_writerecv(gfile,'cnwat','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (canopy), iret=', ierr
    endif
!f10m
    tmp = reshape(nemsiodata%f10m,(/fieldsize/) )
    call nemsio_writerecv(gfile,'f10m','10 m above gnd',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (f10m), iret=', ierr
    endif
!t2m
    tmp = reshape(nemsiodata%t2m,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tmp','2 m above gnd',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (t2m), iret=', ierr
    endif
!q2m
    tmp = reshape(nemsiodata%q2m,(/fieldsize/) )
    call nemsio_writerecv(gfile,'spfh','2 m above gnd',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (q2m), iret=', ierr
    endif
!vtype
    tmp = reshape(nemsiodata%vtype,(/fieldsize/) )
    call nemsio_writerecv(gfile,'vtype','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (vtype), iret=', ierr
    endif
!stype--sotyp
    tmp = reshape(nemsiodata%stype,(/fieldsize/) )
    call nemsio_writerecv(gfile,'sotyp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (stype), iret=', ierr
    endif
!facsf
    tmp = reshape(nemsiodata%facsf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'facsf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (facsf), iret=', ierr
    endif
!facwf
    tmp = reshape(nemsiodata%facwf,(/fieldsize/) )
    call nemsio_writerecv(gfile,'facwf','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (facwf), iret=', ierr
    endif
!uustar--fricv
    tmp = reshape(nemsiodata%uustar,(/fieldsize/) )
    call nemsio_writerecv(gfile,'fricv','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (uustar), iret=', ierr
    endif
!ffmm
    tmp = reshape(nemsiodata%ffmm,(/fieldsize/) )
    call nemsio_writerecv(gfile,'ffmm','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (ffmm), iret=', ierr
    endif
!ffhh
    tmp = reshape(nemsiodata%ffhh,(/fieldsize/) )
    call nemsio_writerecv(gfile,'ffhh','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (ffhh), iret=', ierr
    endif
!hice   
    tmp = reshape(nemsiodata%hice,(/fieldsize/) )
    call nemsio_writerecv(gfile,'icetk','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (hice), iret=', ierr
    endif
!fice
    tmp = reshape(nemsiodata%fice,(/fieldsize/) )
    call nemsio_writerecv(gfile,'icec','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (fice), iret=', ierr
    endif
!tisfc
    tmp = reshape(nemsiodata%tisfc,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tisfc','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (tisfc), iret=', ierr
    endif
!tprcp
    tmp = reshape(nemsiodata%tprcp,(/fieldsize/) )
    call nemsio_writerecv(gfile,'tprcp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (tprcp), iret=', ierr
    endif
!srflag--crain
    tmp = reshape(nemsiodata%srflag,(/fieldsize/) )
    call nemsio_writerecv(gfile,'crain','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (srflag), iret=', ierr
    endif
!snwdph
    tmp = reshape(nemsiodata%snwdph,(/fieldsize/) )
    call nemsio_writerecv(gfile,'snod','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (snwdph), iret=', ierr
    endif
!slc
    do l=1,nsoil
      tmp = reshape(nemsiodata%slc(:,:,l),(/fieldsize/) )
      call nemsio_writerecv(gfile,'slc','soil layer',l,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'ERROR in wrtsfc4 (slc), iret=', ierr,',l=',l
      endif
    enddo
!shdmin
    tmp = reshape(nemsiodata%shdmin,(/fieldsize/) )
    call nemsio_writerecv(gfile,'shdmin','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (shdmin), iret=', ierr
    endif
!shdmax
    tmp = reshape(nemsiodata%shdmax,(/fieldsize/) )
    call nemsio_writerecv(gfile,'shdmax','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (shdmax), iret=', ierr
    endif
!slope
    tmp=reshape(nemsiodata%slope,(/fieldsize/) )
    call nemsio_writerecv(gfile,'sltyp','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (slope), iret=', ierr
    endif
!snoalb
    tmp = reshape(nemsiodata%snoalb,(/fieldsize/) )
    call nemsio_writerecv(gfile,'salbd','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (snoalb), iret=', ierr
    endif
!orog
    tmp = reshape(nemsiodata%orog,(/fieldsize/) )
    call nemsio_writerecv(gfile,'orog','sfc',1,tmp,iret=ierr)
    if(ierr /= 0) then
      if(present(iret)) iret = ierr
      print *,'ERROR in wrtsfc4 (orog), iret=', ierr
    endif
!
    deallocate(tmp)
!
  end subroutine nemsio_gfs_wrtsfc4
!-----------------------------------------------------------------------
!***********************************************************************
!***********************************************************************
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_aldbta_grd(im,jm,lm,ntrac,nemsiodbta,nopdpvv)
!-----------------------------------------------------------------------
!
    implicit none
!
    integer,          intent(in)    :: im,jm,lm,ntrac
    type(nemsio_dbta),intent(inout) :: nemsiodbta
    logical,          intent(in)    :: nopdpvv
!
!---allocate nemsio_dbdata with dimension (im,jm)
!
    allocate(nemsiodbta%zs(im,jm))
    allocate(nemsiodbta%ps(im,jm))
    if (.not. nopdpvv) then
      allocate(nemsiodbta%p(im,jm,lm))
      allocate(nemsiodbta%dp(im,jm,lm))
    endif
    allocate(nemsiodbta%t(im,jm,lm))
    allocate(nemsiodbta%u(im,jm,lm))
    allocate(nemsiodbta%v(im,jm,lm))
    allocate(nemsiodbta%q(im,jm,lm,ntrac))
    allocate(nemsiodbta%w(im,jm,lm))
!
  end subroutine nemsio_gfs_aldbta_grd
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_aldata_grd(im,jm,lm,ntrac,nemsiodata,nopdpvv)
!-----------------------------------------------------------------------
!
    implicit none
!
    integer,          intent(in)    :: im,jm,lm,ntrac
    type(nemsio_data),intent(inout) :: nemsiodata
    logical,          intent(in)    :: nopdpvv
!
!---allocate nemsio_dbdata with dimension (im,jm)
!
    allocate(nemsiodata%zs(im,jm))
    allocate(nemsiodata%ps(im,jm))
    if (.not. nopdpvv) then
      allocate(nemsiodata%p(im,jm,lm))
      allocate(nemsiodata%dp(im,jm,lm))
    endif
    allocate(nemsiodata%t(im,jm,lm))
    allocate(nemsiodata%u(im,jm,lm))
    allocate(nemsiodata%v(im,jm,lm))
    allocate(nemsiodata%q(im,jm,lm,ntrac))
    allocate(nemsiodata%w(im,jm,lm))
!
  end subroutine nemsio_gfs_aldata_grd
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_axdbta_grd(nemsiodbta)
!-----------------------------------------------------------------------
!
    implicit none
!
    type(nemsio_dbta),intent(inout)  :: nemsiodbta
!
!---deallocate nemsio_dbdata 
!
    if (allocated(nemsiodbta%zs)) deallocate(nemsiodbta%zs)
    if (allocated(nemsiodbta%ps)) deallocate(nemsiodbta%ps)
    if (allocated(nemsiodbta%p))  deallocate(nemsiodbta%p)
    if (allocated(nemsiodbta%dp)) deallocate(nemsiodbta%dp)
    if (allocated(nemsiodbta%t))  deallocate(nemsiodbta%t)
    if (allocated(nemsiodbta%u))  deallocate(nemsiodbta%u)
    if (allocated(nemsiodbta%v))  deallocate(nemsiodbta%v)
    if (allocated(nemsiodbta%q))  deallocate(nemsiodbta%q)
    if (allocated(nemsiodbta%w))  deallocate(nemsiodbta%w)
!
  end subroutine nemsio_gfs_axdbta_grd
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_axdata_grd(nemsiodata)
!-----------------------------------------------------------------------
!
    implicit none
!
    type(nemsio_data),intent(inout)  :: nemsiodata
!
!---deallocate nemsio_dadata
!
    if (allocated(nemsiodata%zs)) deallocate(nemsiodata%zs)
    if (allocated(nemsiodata%ps)) deallocate(nemsiodata%ps)
    if (allocated(nemsiodata%p))  deallocate(nemsiodata%p)
    if (allocated(nemsiodata%dp)) deallocate(nemsiodata%dp)
    if (allocated(nemsiodata%t))  deallocate(nemsiodata%t)
    if (allocated(nemsiodata%u))  deallocate(nemsiodata%u)
    if (allocated(nemsiodata%v))  deallocate(nemsiodata%v)
    if (allocated(nemsiodata%q))  deallocate(nemsiodata%q)
    if (allocated(nemsiodata%w))  deallocate(nemsiodata%w)
  end subroutine nemsio_gfs_axdata_grd
!-----------------------------------------------------------------------

  subroutine nemsio_gfs_rdgrd8(gfile,nemsiodbta,iret)
!-----------------------------------------------------------------------
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,          &
                              nemsio_readrecv,nemsio_readrec,           &
                              nemsio_getrechead,nemsio_getheadvar,      &
                              nemsio_searchrecv
    implicit none
!
    type(nemsio_gfile),intent(inout) :: gfile
    type(nemsio_dbta), intent(inout) :: nemsiodbta
    integer, optional, intent(out)   :: iret
!local
    integer im,jm,lm,n,nrec,l,fieldsize,ierr,jrec,vlev,mtrac,krec,ierr1, &
            lrec,nt,ntrclev,ntoz,ntcw,ntke
    character(16) vname,vlevtyp
    real(dblekind),allocatable ::tmp(:)
!
!---read out data from nemsio file
!
    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,ntrac=mtrac,  &
                            nrec=nrec,iret=ierr)
    if(ierr /= 0) then
       if(present(iret)) iret = ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
    allocate(tmp(fieldsize))
    lrec = 0
!
    ntoz = 2 ; ntcw = 3 ; ntke = 4

    call nemsio_getheadvar(gfile,'ntoz',ntoz,iret=ierr)
    call nemsio_getheadvar(gfile,'ntcw',ntcw,iret=ierr)
    call nemsio_getheadvar(gfile,'ntke',ntke,iret=ierr)
    if(ierr /= 0) ntke = 0
    print *,'in read8,ntoz=',ntoz,' ntcw=',ntcw,' ntke=',ntke,'iret=',ierr
!hgt
    call nemsio_searchrecv(gfile,jrec,'hgt','sfc',1,ierr)
    if(ierr == 0 ) then
      call nemsio_readrecv(gfile,'hgt','sfc',1,tmp,iret=ierr)
      if(ierr == 0) then
        lrec = lrec+1
        nemsiodbta%zs = reshape(tmp,(/im,jm/) )
      else
        if(present(iret)) iret = ierr
        print *, 'ERROR in rdgrd (hgt), iret=', ierr
      endif
    endif
!ps
    call nemsio_searchrecv(gfile,jrec,'pres','sfc',1,ierr)
    if(ierr == 0 ) then
      call nemsio_readrecv(gfile,'pres','sfc',1,tmp,iret=ierr)
      if(ierr == 0) then
        lrec = lrec+1
        nemsiodbta%ps = reshape(tmp,(/im,jm/) )
      else
         if(present(iret)) iret = ierr
         print *, 'ERROR in rdgrd (ps), iret=', ierr
      endif
    endif
!
!dp
    call nemsio_searchrecv(gfile,jrec,'dpres','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'dpres','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%dp(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (dp), iret=', ierr
        endif
      enddo
    endif
!p
    call nemsio_searchrecv(gfile,jrec,'pres','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'pres','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%p(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (p), iret=', ierr
        endif
      enddo
    endif
!u
    call nemsio_searchrecv(gfile,jrec,'ugrd','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'ugrd','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%u(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (u), iret=', ierr
        endif
      enddo
    endif
!v
    call nemsio_searchrecv(gfile,jrec,'vgrd','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'vgrd','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%v(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (v), iret=', ierr
        endif
      enddo
    endif
!t
    call nemsio_searchrecv(gfile,jrec,'tmp','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'tmp','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%t(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (t), iret=', ierr
        endif
      enddo
    endif
!
!spfh
    call nemsio_searchrecv(gfile,jrec,'spfh','mid layer',1,ierr)
    if(ierr == 0 ) then
      ntrclev = 0
      do l=1,lm
        call nemsio_readrecv(gfile,'spfh','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%q(:,:,l,1) = reshape(tmp,(/im,jm/) )
          ntrclev = ntrclev + 1
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (spfh), iret=', ierr
        endif
      enddo
    endif
!
!ozone
    call nemsio_searchrecv(gfile,jrec,'o3mr','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'o3mr','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%q(:,:,l,ntoz) = reshape(tmp,(/im,jm/) )
          ntrclev = ntrclev + 1
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (t), iret=', ierr
        endif
      enddo
    endif
!
!cld
    call nemsio_searchrecv(gfile,jrec,'clwmr','mid layer',1,ierr)
    if(ierr == 0 ) then
      do l=1,lm
        call nemsio_readrecv(gfile,'clwmr','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%q(:,:,l,ntcw) = reshape(tmp,(/im,jm/) )
          ntrclev = ntrclev + 1
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (t), iret=', ierr
        endif
      enddo
    endif
!
!tke
    if (ntke > 0) then
      call nemsio_searchrecv(gfile,jrec,'tke','mid layer',1,ierr)
      if(ierr == 0 ) then
        do l=1,lm
          call nemsio_readrecv(gfile,'tke','mid layer',l,tmp,iret=ierr)
          if(ierr == 0) then
            lrec = lrec+1
            nemsiodbta%q(:,:,l,ntke) = reshape(tmp,(/im,jm/) )
            ntrclev = ntrclev + 1
          else
            if(present(iret)) iret = ierr
           print *, 'ERROR in rdgrd (t), iret=', ierr
          endif
        enddo
      endif
    endif
!
!aerosol tracers
    jrec = 0
    nt = ntrclev/lm
    do n=1,mtrac-max(3,ntke)
      vname = aero_tracername(n)
      call nemsio_searchrecv(gfile,jrec,trim(vname),'mid layer',1,iret=ierr)
      if (ierr == 0) then
         nt = nt + 1
        do l=1,lm
          call nemsio_readrecv(gfile,trim(vname),'mid layer',l,tmp,iret=ierr)
          if(ierr == 0) then
            lrec = lrec+1
            nemsiodbta%q(:,:,l,nt) = reshape(tmp,(/im,jm/) )
          else
            print *,'ERROR in rdgrd (v), ',trim(vname),'level=',l,' iret=', ierr
          endif
        enddo
      else
        print *,'WARNING: no ',trim(vname),' in the file!'
      endif
    enddo
    if(nt < mtrac) then
      print *,'WARNING: ntrac=',mtrac,' but only read out:',nt,' tracers from the file!'
    endif
!
    call nemsio_searchrecv(gfile,jrec,'vvel','mid layer',1,iret=ierr)
    if(jrec > 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'vvel','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          lrec = lrec+1
          nemsiodbta%w(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (w), iret=', ierr
        endif
      enddo
    endif
!
    deallocate(tmp)

!-----------------------------------------------------------------------
  end subroutine nemsio_gfs_rdgrd8
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_rdgrd4(gfile,nemsiodata,iret)
!-----------------------------------------------------------------------
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,nemsio_getheadvar,          &
                              nemsio_readrecv,nemsio_readrec,nemsio_getrechead,           &
                              nemsio_searchrecv
    implicit none
!
    type(nemsio_gfile),intent(inout)  :: gfile
    type(nemsio_data), intent(inout)  :: nemsiodata
    integer,optional,  intent(out)    :: iret
!local
    integer im,jm,lm,n,l,nrec,fieldsize,ierr,jrec,vlev,mtrac,krec,ierr1,nt, &
            ntoz,ntcw,ntrclev,ntke
    character(16) vname,vlevtyp
    real(dblekind),allocatable ::tmp(:)
!
!---read out data from nemsio file
!
    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,ntrac=mtrac,   &
                            nrec=nrec,iret=ierr)
    if(ierr/=0) then
       if(present(iret)) iret=ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
    allocate(tmp(fieldsize))
!
    ntoz = 2 ; ntcw = 3 ; ntke = 4

    call nemsio_getheadvar(gfile,'ntoz',ntoz,iret=ierr)
    call nemsio_getheadvar(gfile,'ntcw',ntcw,iret=ierr)
    call nemsio_getheadvar(gfile,'ntke',ntke,iret=ierr)
    if(ierr /= 0) ntke = 0
!hgt
    call nemsio_searchrecv(gfile,jrec,'hgt','sfc',1,iret=ierr)
    if(ierr == 0) then
      call nemsio_readrecv(gfile,'hgt','sfc',1,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodata%zs = reshape(tmp,(/im,jm/) )
      else
         if(present(iret)) iret = ierr
         print *, 'ERROR in rdgrd (hgt), iret=', ierr
      endif
    endif
!ps
    call nemsio_searchrecv(gfile,jrec,'pres','sfc',1,iret=ierr)
    if(ierr == 0) then
      call nemsio_readrecv(gfile,'pres','sfc',1,tmp,iret=ierr)
      if(ierr == 0) then
        nemsiodata%ps = reshape(tmp,(/im,jm/) )
      else
        if(present(iret)) iret = ierr
        print *, 'ERROR in rdgrd (ps), iret=', ierr
      endif
    endif
!dp
    call nemsio_searchrecv(gfile,jrec,'dpres','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'dpres','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%dp(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret=ierr
          print *, 'ERROR in rdgrd (dp), iret=', ierr
        endif
      enddo
    endif
!p
    call nemsio_searchrecv(gfile,jrec,'pres','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'pres','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%p(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (p), iret=', ierr
        endif
      enddo
    endif
!u
    call nemsio_searchrecv(gfile,jrec,'ugrd','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'ugrd','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%u(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (u), iret=', ierr
        endif
      enddo
    endif
!v
    call nemsio_searchrecv(gfile,jrec,'vgrd','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'vgrd','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%v(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (v), iret=', ierr
        endif
      enddo
    endif
!t
    call nemsio_searchrecv(gfile,jrec,'tmp','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'tmp','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%t(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (t), iret=', ierr
        endif
      enddo
    endif
!
!spfh
    call nemsio_searchrecv(gfile,jrec,'spfh','mid layer',1,iret=ierr)
    if(ierr == 0) then
      ntrclev=0
      do l=1,lm
        call nemsio_readrecv(gfile,'spfh','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%q(:,:,l,1) = reshape(tmp,(/im,jm/) )
          ntrclev = ntrclev + 1
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (spfh), iret=', ierr
        endif
      enddo
    endif
!
!ozone
    call nemsio_searchrecv(gfile,jrec,'o3mr','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'o3mr','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%q(:,:,l,ntoz) = reshape(tmp,(/im,jm/) )
          ntrclev = ntrclev + 1
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (t), iret=', ierr
        endif
      enddo
    endif
!
!cld
    call nemsio_searchrecv(gfile,jrec,'clwmr','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'clwmr','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%q(:,:,l,ntcw) = reshape(tmp,(/im,jm/) )
          ntrclev = ntrclev + 1
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (t), iret=', ierr
        endif
      enddo
    endif
!
!tke
    if (ntke > 0) then
      call nemsio_searchrecv(gfile,jrec,'tke','mid layer',1,iret=ierr)
      if(ierr == 0) then
        do l=1,lm
          call nemsio_readrecv(gfile,'tke','mid layer',l,tmp,iret=ierr)
          if(ierr == 0) then
            nemsiodata%q(:,:,l,ntke) = reshape(tmp,(/im,jm/) )
            ntrclev = ntrclev + 1
          else
            if(present(iret)) iret = ierr
            print *, 'ERROR in rdgrd (t), iret=', ierr
          endif
        enddo
      endif
    endif
!
!aerosol tracers
    jrec = 0
    nt = ntrclev/lm
    do n=1,mtrac-max(3,ntke)
      vname = aero_tracername(n)
      call nemsio_searchrecv(gfile,jrec,trim(vname),'mid layer',1,iret=ierr)
      if (ierr == 0) then
        nt = nt + 1
        do l=1,lm
          call nemsio_readrecv(gfile,trim(vname),'mid layer',l,tmp,iret=ierr)
          if(ierr == 0) then
            nemsiodata%q(:,:,l,nt) = reshape(tmp,(/im,jm/) )
          else
            print *,'ERROR in rdgrd (v), ',trim(vname),'level=',l,' iret=', ierr
          endif
        enddo
      else
        print *,'WARNING: no ',trim(vname),' in the file!'
      endif
    enddo
    if(nt < mtrac) then
      print *,'WARNING: ntrac=',mtrac,' but only read out:',nt,' tracers from the file!'
    endif
!
    call nemsio_searchrecv(gfile,jrec,'vvel','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        call nemsio_readrecv(gfile,'vvel','mid layer',l,tmp,iret=ierr)
        if(ierr == 0) then
          nemsiodata%w(:,:,l) = reshape(tmp,(/im,jm/) )
        else
          if(present(iret)) iret = ierr
          print *, 'ERROR in rdgrd (w), iret=', ierr
        endif
      enddo
    endif
!
    if(present(iret)) iret=0
    deallocate(tmp)

!-----------------------------------------------------------------------
  end subroutine nemsio_gfs_rdgrd4
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_wrtgrd8(gfile,nemsiodbta,iret)
!-----------------------------------------------------------------------
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,           &
                              nemsio_writerecv,nemsio_writerec,          &
                              nemsio_getrechead,nemsio_searchrecv,       &
                              nemsio_getheadvar
    implicit none
!
    type(nemsio_gfile),intent(inout)  :: gfile
    type(nemsio_dbta), intent(in)     :: nemsiodbta
    integer, optional, intent(out)    :: iret
!local
    integer im,jm,lm,n,l,jrec,nrec,fieldsize,ierr,mtrac,krec,vlev,ierr1, &
            nt,ntoz,ntcw,ntke
    character(16) vname,vlevtyp
    real(dblekind),allocatable :: tmp(:)
!
!---read out data from nemsio file
!

    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,     &
                            nrec=nrec,ntrac=mtrac,iret=ierr)
    
    if(ierr/=0) then
       if(present(iret)) iret=ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
    allocate(tmp(fieldsize))
!
    ntoz = 2 ; ntcw = 3 ; ntke = 4

    call nemsio_getheadvar(gfile,'ntoz',ntoz,iret=ierr)
    call nemsio_getheadvar(gfile,'ntcw',ntcw,iret=ierr)
    call nemsio_getheadvar(gfile,'ntke',ntke,iret=ierr)
    if(ierr /= 0) ntke = 0

!hgt
    call nemsio_searchrecv(gfile,jrec,'hgt','sfc',1,iret=ierr)
    if(ierr == 0) then
      tmp = reshape(nemsiodbta%zs,(/fieldsize/) )
      call nemsio_writerecv(gfile,'hgt','sfc',1,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'write hgt,ierr=',ierr
      endif
    endif
!ps
    call nemsio_searchrecv(gfile,jrec,'pres','sfc',1,iret=ierr)
    if(ierr == 0) then
      tmp = reshape(nemsiodbta%ps,(/fieldsize/) )
      call nemsio_writerecv(gfile,'pres','sfc',1,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'write psfc,ierr=',ierr
      endif
    endif
!
!dp
    call nemsio_searchrecv(gfile,jrec,'dpres','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%dp(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'dpres','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'dpres,ierr=',ierr
        endif
      enddo
    endif
!p
    call nemsio_searchrecv(gfile,jrec,'pres','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%p(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'pres','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'pres,ierr=',ierr
        endif
      enddo
    endif
!u
    call nemsio_searchrecv(gfile,jrec,'ugrd','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%u(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'ugrd','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'ugrd,ierr=',ierr
        endif
      enddo
    endif
!v
    call nemsio_searchrecv(gfile,jrec,'vgrd','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%v(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'vgrd','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'vgrd,ierr=',ierr
        endif
      enddo
    endif
!t
    call nemsio_searchrecv(gfile,jrec,'tmp','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%t(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'tmp','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'tmp,ierr=',ierr
        endif
      enddo
    endif
!spfh
    call nemsio_searchrecv(gfile,jrec,'spfh','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%q(:,:,l,1),(/fieldsize/) )
        call nemsio_writerecv(gfile,'spfh','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'spfh,ierr=',ierr
        endif
      enddo
    endif
!ozone
    call nemsio_searchrecv(gfile,jrec,'o3mr','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%q(:,:,l,ntoz),(/fieldsize/) )
        call nemsio_writerecv(gfile,'o3mr','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'o3mr,ierr=',ierr
        endif
      enddo
    endif
!cld
    call nemsio_searchrecv(gfile,jrec,'clwmr','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%q(:,:,l,ntcw),(/fieldsize/) )
        call nemsio_writerecv(gfile,'clwmr','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,' clwmr,ierr=',ierr
        endif
      enddo
    endif
!
!tke
    if (ntke > 0) then
      call nemsio_searchrecv(gfile,jrec,'tke','mid layer',1,iret=ierr)
      if(ierr == 0) then
        do l=1,lm
          tmp = reshape(nemsiodbta%q(:,:,l,ntke),(/fieldsize/) )
          call nemsio_writerecv(gfile,'tke','mid layer',l,tmp,iret=ierr)
          if(ierr /= 0) then
            if(present(iret)) iret = ierr
            print *,'write l=',l,' tke,ierr=',ierr
          endif
        enddo
      endif
    endif
!
!aerosol tracers
    nt=max(3,ntke)
    do n=1,mtrac-max(3,ntke)
      vname = aero_tracername(n)
      call nemsio_searchrecv(gfile,jrec,trim(vname),'mid layer',1,iret=ierr)
      if (ierr == 0) then
        nt = nt + 1
        do l=1,lm
          tmp = reshape(nemsiodbta%q(:,:,l,nt),(/fieldsize/) )
          call nemsio_writerecv(gfile,trim(vname),'mid layer',l,tmp,iret=ierr)
          if(ierr /= 0) then
            if(present(iret)) iret = ierr
            print *,'write rdgrd (v),',trim(vname),'level=',l,' iret=', ierr
          endif
        enddo
      else
        print *,'WARNING: no ',trim(vname),' in the file!'
      endif
    enddo
    if(nt < mtrac) then
      print *,'WARNING: ntrac=',mtrac,' but only read out:',nt,' tracers from the file!'
    endif
    if(present(iret)) iret = ierr

!w
    call nemsio_searchrecv(gfile,jrec,'vvel','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodbta%w(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'vvel','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'vvel,ierr=',ierr
        endif
      enddo
      if(present(iret)) iret = 0
    endif
!
    deallocate(tmp)

!-----------------------------------------------------------------------
  end subroutine nemsio_gfs_wrtgrd8
!-----------------------------------------------------------------------
  subroutine nemsio_gfs_wrtgrd4(gfile,nemsiodata,iret)
!-----------------------------------------------------------------------
!
    use nemsio_module, only : nemsio_gfile,nemsio_getfilehead,           &
                              nemsio_writerecv,nemsio_writerec,          &
                              nemsio_getrechead,nemsio_searchrecv,       &
                              nemsio_getheadvar
    implicit none
!
    type(nemsio_gfile),intent(inout)  :: gfile
    type(nemsio_data), intent(in)     :: nemsiodata
    integer, optional, intent(out)    :: iret
!local
    integer im,jm,lm,n,l,jrec,nrec,fieldsize,mtrac,ierr,vlev,krec,ierr1, &
            nt,ntoz,ntcw,ntke
    character(16) vname,vlevtyp
    real(dblekind),allocatable :: tmp(:)
!
!---read out data from nemsio file
!

    call nemsio_getfilehead(gfile,dimx=im,dimy=jm,dimz=lm,     &
                            nrec=nrec,ntrac=mtrac,iret=ierr)
    if(ierr/=0) then
       if(present(iret)) iret=ierr
       print *,'ERROR: cannot get dimension from gfile'
       return
    endif
    fieldsize = im*jm
    allocate(tmp(fieldsize))
!
    ntoz = 2 ; ntcw = 3 ; ntke = 4

    call nemsio_getheadvar(gfile,'ntoz',ntoz,iret=ierr)
    call nemsio_getheadvar(gfile,'ntcw',ntcw,iret=ierr)
    call nemsio_getheadvar(gfile,'ntke',ntke,iret=ierr)
    if(ierr /= 0) ntke = 0
!hgt
    call nemsio_searchrecv(gfile,jrec,'hgt','sfc',1,iret=ierr)
    if(ierr == 0) then
      tmp = reshape(nemsiodata%zs,(/fieldsize/) )
      call nemsio_writerecv(gfile,'hgt','sfc',1,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'write hgt,ierr=',ierr
      endif
    endif
!ps
    call nemsio_searchrecv(gfile,jrec,'pres','sfc',1,iret=ierr)
    if(ierr == 0) then
      tmp = reshape(nemsiodata%ps,(/fieldsize/) )
      call nemsio_writerecv(gfile,'pres','sfc',1,tmp,iret=ierr)
      if(ierr /= 0) then
        if(present(iret)) iret = ierr
        print *,'write psfc,ierr=',ierr
      endif
    endif
!dp
    call nemsio_searchrecv(gfile,jrec,'dpres','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%dp(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'dpres','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'dpres,ierr=',ierr
        endif
      enddo
    endif
!p
    call nemsio_searchrecv(gfile,jrec,'pres','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%p(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'pres','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'pres,ierr=',ierr
        endif
      enddo
    endif
!u
    call nemsio_searchrecv(gfile,jrec,'ugrd','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%u(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'ugrd','mid layer',l,tmp,iret=ierr)
!      print *,'write l=',l,'ugrd,ierr=',ierr,maxval(tmp),minval(tmp)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'ugrd,ierr=',ierr,maxval(tmp),minval(tmp)
        endif
      enddo
    endif
!v
    call nemsio_searchrecv(gfile,jrec,'vgrd','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%v(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'vgrd','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'vgrd,ierr=',ierr
        endif
      enddo
    endif
!t
    call nemsio_searchrecv(gfile,jrec,'tmp','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%t(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'tmp','mid layer',l,tmp,iret=ierr)
!      print *,'write l=',l,'tmp,ierr=',ierr,maxval(tmp),minval(tmp)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'tmp,ierr=',ierr
        endif
      enddo
    endif
!spfh
    call nemsio_searchrecv(gfile,jrec,'spfh','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%q(:,:,l,1),(/fieldsize/) )
        call nemsio_writerecv(gfile,'spfh','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'spfh,ierr=',ierr
        endif
      enddo
    endif
!ozone
    call nemsio_searchrecv(gfile,jrec,'o3mr','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%q(:,:,l,ntoz),(/fieldsize/) )
        call nemsio_writerecv(gfile,'o3mr','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'o3mr,ierr=',ierr
        endif
      enddo
    endif
!cld
    call nemsio_searchrecv(gfile,jrec,'clwmr','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%q(:,:,l,ntcw),(/fieldsize/) )
        call nemsio_writerecv(gfile,'clwmr','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,' clwmr,ierr=',ierr
        endif
      enddo
    endif
!
!tke
    if (ntke > 0) then
      call nemsio_searchrecv(gfile,jrec,'tke','mid layer',1,iret=ierr)
      if(ierr == 0) then
        do l=1,lm
          tmp = reshape(nemsiodata%q(:,:,l,ntke),(/fieldsize/) )
          call nemsio_writerecv(gfile,'tke','mid layer',l,tmp,iret=ierr)
          if(ierr /= 0) then
            if(present(iret)) iret = ierr
            print *,'write l=',l,' tke,ierr=',ierr
          endif
        enddo
      endif
    endif
!
!aerosol tracers
    nt = max(3,ntke)
    do n=1,mtrac-max(3,ntke)
      vname = aero_tracername(n)
      call nemsio_searchrecv(gfile,jrec,trim(vname),'mid layer',1,iret=ierr)
      if (ierr == 0) then
        nt = nt + 1
        do l=1,lm
          tmp = reshape(nemsiodata%q(:,:,l,nt),(/fieldsize/) )
          call nemsio_writerecv(gfile,trim(vname),'mid layer',l,tmp,iret=ierr)
          if(ierr /= 0) then
            if(present(iret)) iret = ierr
            print *,'write rdgrd (v),',trim(vname),'level=',l,' iret=', ierr
          endif
        enddo
      else
        print *,'WARNING: no ',trim(vname),' in the file!'
      endif
    enddo
    if(nt < mtrac) then
      print *,'WARNING: ntrac=',mtrac,' but only read out:',nt,' tracers from the file!'
    endif
    if(present(iret)) iret=ierr
!w
    call nemsio_searchrecv(gfile,jrec,'vvel','mid layer',1,iret=ierr)
    if(ierr == 0) then
      do l=1,lm
        tmp = reshape(nemsiodata%w(:,:,l),(/fieldsize/) )
        call nemsio_writerecv(gfile,'vvel','mid layer',l,tmp,iret=ierr)
        if(ierr /= 0) then
          if(present(iret)) iret = ierr
          print *,'write l=',l,'vvel,ierr=',ierr
        endif
      enddo
      if(present(iret)) iret=ierr
    endif
!
!
    deallocate(tmp)
!-----------------------------------------------------------------------
  end subroutine nemsio_gfs_wrtgrd4
!-----------------------------------------------------------------------
!
  end module nemsio_gfs

