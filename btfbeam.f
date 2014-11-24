      PROGRAM btfbeam
******************************************************************
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      REAL tim
c      
      Call GZEBRA(NGBANK)       ! Initialize GEANT memory
      Call FFINIT(NFFREA)       ! Initialize FFREAD
      Call HLIMIT(-NHBOOK)      ! Initialize HBOOK
*
      Call UGRINIT
      Call UGINIT               ! Initialize GEANT
c      print *,TIMINT,TIMEND,ITIME,IGDATE,IGTIME
      CALL TIMEST(1E5)
      Call UFILES               ! open user FFREAD and HBOOK files
      CALL GRUN                 ! Start event processing
c      print *,tim
      CALL UGLAST               ! Terminate run
*
      END
*
*
*
      SUBROUTINE UGRINIT
******************************************************************
*     Graphics initialization routine... 
      Implicit none
******************************************************************      
      Call HPLINT(0)
      OPEN(10,FILE='btfbeam.ps',STATUS='UNKNOWN')
      Call IGMETA(10,-111)
*
      RETURN
      END
*
*
*
      SUBROUTINE UGINIT
******************************************************************
*     Initialization routine... (GEANT and USER subroutines)
      Implicit none
      Include 'btfbeam.inc'
******************************************************************      
      HBEA = 1
      HBAR = 1
      HFRW = 1
      HBCK = 1
      OPEN(4, FILE='btfbeam.card',STATUS='OLD')
      Call GINIT                ! initialize GEANT
      OPEN(3,FILE='btfbeam.seed',STATUS='OLD')
      READ(3,*) IDRUN,NRNDM(1),NRNDM(2)
      IDRUN=IDRUN+1
      CLOSE(3)
*
*     Read user keys...
*      
      Call FFKEY('XBEA',XBEAM,  3,'REAL')  
      Call FFKEY('PBEA',PBEAM,  3,'REAL')  
      Call FFKEY('SXBE',SXBEAM, 2,'REAL')  
      Call FFKEY('SPBE',SPBEAM, 1,'REAL')  
      Call FFKEY('EMIT',EMIT,   1,'REAL')  
c
      Call FFKEY('IGEO',IGEOBAR,1, 'INTEGER')  
      Call FFKEY('DRAW',RSCALE,5, 'REAL')  
      Call FFKEY('RBAR',RBAR,1,   'REAL')  
      Call FFKEY('LBAR',LBAR,1,   'REAL')  
      Call FFKEY('WBAR',WBAR,1,   'REAL')  
      Call FFKEY('DCAL',DISTAC,1, 'REAL')  
      Call FFKEY('GCAL',CALOGEO,1,'INTEGER')  
c
      Call FFKEY('QUAX',KX,QUADMAX,  'REAL')  
      Call FFKEY('QUAY',KY,QUADMAX,  'REAL')  
      Call FFKEY('ZMIN',ZMIN,QUADMAX,'REAL')  
      Call FFKEY('ZMAX',ZMAX,QUADMAX,'REAL')  
      CALL FFKEY('RPIP',RPIPE,2,     'REAL')
c
      Call FFKEY('HBEA',HBEA,1,   'INTEGER')  
      Call FFKEY('HBAR',HBAR,1,   'INTEGER')  
      Call FFKEY('HFRW',HFRW,1,   'INTEGER')  
      Call FFKEY('HBCK',HBCK,1,   'INTEGER')  
      Call FFKEY('HQUA',HQUA,1,   'INTEGER')  
      Call FFKEY('HCAL',HCAL,1,   'INTEGER')  
c
      Call FFSET('LINP',4)
      CALL GFFGO
      CLOSE(4)                  ! Close FFREAD file
      print *,'Starting run number: ',IDRUN
      print *,'Starting from seed:  ',NRNDM(1),NRNDM(2)
c
      CALL GZINIT               ! initialize data structures
      CALL GDINIT               ! initialize graphics
      CALL GMATE                ! initialize standard materials
      CALL GPART                ! initialize particle table
      CALL GPIONS               ! initialize ion table    
      CALL UGMATE               ! define user materials and tracking parameter 
      CALL UGEOM_BTF            ! define user geometry
      CALL UGEOM_DRAW           ! draw user geometry
      CALL GPHYSI               ! energy loss and cross-section tables
      CALL UHINIT               ! book user histograms
      CALL UPRINT               ! print user stuff
*      
      RETURN
      END
*
*
*
      SUBROUTINE UFILES
******************************************************************
*     Open all files...
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      INTEGER ISTAT
      CHARACTER*19 FILENAME
*
      WRITE(FILENAME,1111) IDRUN
 1111 FORMAT('/tmp/btf',I5.5,'.hbook')
      print *,'FILE name: ',FILENAME
      CALL HROPEN(21,'BTF',FILENAME,'N',1024,ISTAT)
      print *,'FILE open.'
*
      RETURN
      END
*
*
*
      SUBROUTINE UHINIT
******************************************************************
*     Book ntuple
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      Call HBNT(1,'BTF',' ')
*
      Call HBNAME(1,'INFO',IDRUN,   'NRUN:I')
      Call HBNAME(1,'INFO',NRNDM(1),'SEED1:I')
      Call HBNAME(1,'INFO',NRNDM(2),'SEED2:I')
      Call HBNAME(1,'INFO',CALOGEO, 'CALOGEO:I')
      Call HBNAME(1,'INFO',IGEOBAR, 'BARGEO:I')
      Call HBNAME(1,'INFO',RBAR,'DIM1:R')
      Call HBNAME(1,'INFO',LBAR,'DIM2:R')
      Call HBNAME(1,'INFO',WBAR,'DIM3:R')
      If (HBEA.EQ.1) Then
         Call HBNAME(1,'BEAM',VBEAM,'VBEAM(3):R')
         Call HBNAME(1,'BEAM',PLBEAM,'PLBEAM(3):R')
         Call HBNAME(1,'BEAM',EMIT,'EMIT:R')
      EndIf
      If(HFRW.EQ.1) Then
         Call HBNAME(1,'FRNT',NINP,
     +        'NINP[0,500]:I,XINP(NINP):R,YINP(NINP):R,ZINP(NINP):R,'//
     +        'PXINP(NINP):R,PYINP(NINP):R,PZINP(NINP):R,'//
     +        'PARTINP(NINP):I')
      EndIf
      If(HBCK.EQ.1) Then
         Call HBNAME(1,'BACK',NBCK,
     +        'NBCK[0,500]:I,XBCK(NBCK):R,YBCK(NBCK):R,ZBCK(NBCK):R,'//
     +        'PXBCK(NBCK):R,PYBCK(NBCK):R,PZBCK(NBCK):R,'//
     +        'PARTBCK(NBCK):I')
      EndIf
      If(HQUA.EQ.1) Then
         Call HBNAME(1,'QUA1',NQUA1,
     +        'NQUA1[0,500]:I,XQUA1(NQUA1):R,YQUA1(NQUA1):R,'//
     +        'ZQUA1(NQUA1):R,'//
     +        'PXQUA1(NQUA1):R,PYQUA1(NQUA1):R,PZQUA1(NQUA1):R,'//
     +        'PARTQUA1(NQUA1):I')
         Call HBNAME(1,'QUA2',NQUA2,
     +        'NQUA2[0,500]:I,XQUA2(NQUA2):R,YQUA2(NQUA2):R,'//
     +        'ZQUA2(NQUA2):R,'//
     +        'PXQUA2(NQUA2):R,PYQUA2(NQUA2):R,PZQUA2(NQUA2):R,'//
     +        'PARTQUA2(NQUA2):I')
      EndIf
      If(HBAR.EQ.1) Then
         Call HBNAME(1,'BAR ',NBAR,
     +        'NBAR[0,500]:I,XBAR(NBAR):R,YBAR(NBAR):R,ZBAR(NBAR):R,'//
     +     'PXBAR(NBAR):R,PYBAR(NBAR):R,PZBAR(NBAR):R,PARTBAR(NBAR):I')
         Call HBNAME(1,'BAR ',DEEBAR,'DEEBAR:R')
         Call HBNAME(1,'BAR ',DXXBAR,'DXXBAR:R')
         Call HBNAME(1,'BAR ',NNBAR,'NNBAR:I')
         Call HBNAME(1,'BAR ',TBAR,'TBAR:R')
         Call HBNAME(1,'BAR ',DEBAR,'DEBAR(3):R')
         Call HBNAME(1,'BAR ',DXBAR,'DXBAR(3):R')
         Call HBNAME(1,'BAR ',NPBAR,'NPBAR(3):R')
         Call HBNAME(1,'BAR ',DEDXBAR,'DEDXBAR:R')
      EndIf
      If(HCAL.EQ.1) Then
         Call HBNAME(1,'CALO',DEFE, 'DEFE(3):R')
         Call HBNAME(1,'CALO',DEPB, 'DEPB(3):R')
         Call HBNAME(1,'CALO',DESCI,'DESCI(3):R')
         Call HBNAME(1,'CALO',DXFE, 'DXFE(3):R')
         Call HBNAME(1,'CALO',DXPB, 'DXPB(3):R')
         Call HBNAME(1,'CALO',DXSCI,'DXSCI(3):R')
      Endif
*
      RETURN
      END
*
*
*
      Subroutine UGMATE
******************************************************************
      Implicit None
******************************************************************
      CALL DefineMate
      CALL DefineMedium
*
      RETURN
      END
*
*
*
      Subroutine DefineMate
******************************************************************
*     Define user materials
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      Real    CH_A(2),CH_Z(2),CH_DENS,CH_MAT(2)
      REAL    A(4)
      REAL    Z(4)
      REAL    Density
      REAL    Ratio(4)
      REAL    Weight(4)
*
*     Calorimeter: active medium, C:H = 1.2
*
      CH_A(1)=1.
      CH_A(2)=12.
      CH_Z(1)=1.
      CH_Z(2)=6.
      CH_DENS=1.032
      CH_MAT(1)=1.1
      CH_MAT(2)=0.9
*
      CALL GSMIXT(SCINT,'Scintillator'
     +     ,CH_A,CH_Z,CH_DENS,-2,CH_MAT)
C----------------------------------------------------------------------
C- Lead ( + 6% Antimonium )
C----------------------------------------------------------------------
      A(1)=207.20
      Z(1)=82.0
      A(2)=121.75
      Z(2)=51.0
      Density=11.35                     ! This is the density of Pb!
      Weight(1)=0.94
      Weight(2)=0.06
      CALL gSMixt(LEAD_SB,'Lead',A,Z,Density,2,Weight)
C----------------------------------------------------------------------
C- Plastic scintillator (polystyrene)
C----------------------------------------------------------------------
      A(1)=12.01
      Z(1)=6.0
      A(2)=1.01
      Z(2)=1.0
      A(3)=14.01
      Z(3)=7.0
      A(4)=16.0
      Z(4)=8.0
      Density=1.049
      Weight(1)=.9213
      Weight(2)=.0773
      Weight(3)=8.73E-4
      Weight(4)=5.03E-4
      CALL gSMixt(PLASTIC,'Plastic',A,Z,Density,4,Weight)
C----------------------------------------------------------------------
C- Optical Glue 
C----------------------------------------------------------------------
      Density=1.181
      Weight(1)=.6524
      Weight(2)=.0773
      Weight(3)=.0252
      Weight(4)=.2506
      CALL gSMixt(GLUE,'Glue',A,Z,Density,4,Weight)
*
      RETURN
      END
*
*
*
      Subroutine DefineMedium
******************************************************************
*     Define user materials
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
*
*     Tracking medium parameters:
*
      ISVOL  = 0                ! sensitivity flag
      IFIELD = 1                ! magnetic field flag
      FIELDM = 20.              ! maximum field value (kGauss)
      TMAXFD = 0.               ! max angular deviation due to magnetic in one step
      STEMAX = 0.01             ! max step size permitted (cm)
      DEEMAX = 0.0001           ! max fractional energy loss in one step
      EPSIL  = 0.001            ! boundary crossing precision (cm)
      STMIN  = 0.001            ! min value for the max step imposed by enery loss,
                                ! multiple scattering, Cerenkov, or magnetic
                                ! field effects (cm)
*
*     Define Mother Volume (Vacuum)
*     
      CALL GSTMED(LIMBO,'MOTHTM',VACUUM,ISVOL,IFIELD,FIELDM,TMAXFD,
     +            STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*
*     Bar
*
      CALL GSTMED(THEBAR,'BAR_TM',COPPER
     +     ,ISVOL,IFIELD,FIELDM,TMAXFD
     +     ,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*
*     Forward counter
*
      CALL GSTMED(FORWARD,'FORWTM',VACUUM
     +     ,ISVOL,IFIELD,FIELDM,TMAXFD
     +     ,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*
*     Backward counter
*
      CALL GSTMED(BACKWARD,'BACKTM',VACUUM
     +     ,ISVOL,IFIELD,FIELDM,TMAXFD
     +     ,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*
*     CHECKQ1,2
*
      CALL GSTMED(CHECKQ1,'Q1TM',VACUUM
     +     ,ISVOL,IFIELD,FIELDM,TMAXFD
     +     ,STEMAX,DEEMAX,EPSIL,STMIN,0,0)

      CALL GSTMED(CHECKQ2,'Q2TM',VACUUM
     +     ,ISVOL,IFIELD,FIELDM,TMAXFD
     +     ,STEMAX,DEEMAX,EPSIL,STMIN,0,0)

*
      CALL GSTMED(R_ACTIVE,'ACTV',SCINT
     +     ,ISVOL,IFIELD,FIELDM
     +     ,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*
*     Calorimeter: absorber, Pb
*
      CALL GSTMED(R_LEAD,'ABSB',LEAD,ISVOL,IFIELD,FIELDM
     +     ,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*
*     Calorimeter: iron
*
      CALL GSTMED(SKIN,'IRON',IRON,ISVOL,IFIELD,FIELDM
     +     ,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*     *
*     Pipe: iron
*
      CALL GSTMED(IPIP,'IRON',IRON,ISVOL,IFIELD,FIELDM
     +     ,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
*     
      RETURN
      END
*
*
*
      SUBROUTINE UGEOM_BTF
******************************************************************
*     Define the geometry:
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
*     
*     Local variables
*
      Real MOTH(3)
      Real FRNT(3),BACK(3),BAR(6),DISTA,DISTABACK,PIPEV(5),QUAV(3)
      Real CALO(3),CALODIM(3),CALOEXT(3),NPLA,OFFSET
      Integer IVOL
      Real XSLI
      Integer ISLI
*
      ROTA(1) = 0.
      ROTA(2) = 0.
      ROTA(3) = 90.
      ROTA(4) = 0.
      ROTA(5) = 90.
      ROTA(6) = 90.
c
      ROTB(1) = 0.
      ROTB(2) = 0.
      ROTB(3) = 90.
      ROTB(4) = 90.
      ROTB(5) = 90.
      ROTB(6) = 180.
c
      ROTC(1) = 0.
      ROTC(2) = 0.
      ROTC(3) = 90.
      ROTC(4) = 0.
      ROTC(5) = 0.
      ROTC(6) = 0.
*
*     Define mother volume 
*     
      MOTH(1) = 500.0           ! (half length, cm)
      MOTH(2) = 500.0           ! (half length, cm)
      MOTH(3) = 500.0           ! (half length, cm)   
      CALL GSVOLU('MOTH','BOX ',LIMBO,MOTH,3,IVOL)
*
*     Define BAR
*     
      if(igeobar.eq.1) then
         BAR(1) = 0.            ! (r min, cm)
         BAR(2) = RBAR          ! (r max, cm)
         BAR(3) = 0.5*LBAR      ! (half length z, cm)
         DDBAR  = RBAR
         Call GSVOLU('BAR ','TUBE',THEBAR,BAR,3,IVOL)
      elseif(igeobar.eq.2) then
         BAR(1) = 0.            ! (r min, cm)
         BAR(2) = RBAR          ! (r max, cm)
         BAR(3) = 0.            ! (theta min)
         BAR(4) = 180.          ! (theta max)
         BAR(5) = 0.            ! (phi min)
         BAR(6) = 360.            ! (phi max)
         DDBAR  = RBAR
         Call GSVOLU('BAR ','SPHE',THEBAR,BAR,6,IVOL)
      elseif(igeobar.eq.3) then
         BAR(1) = 0.5*WBAR
         BAR(2) = 0.5*RBAR
         BAR(3) = 0.5*LBAR
         DDBAR  = 0.5*WBAR
         CALL GSVOLU('BAR ','BOX ',THEBAR,BAR,3,IVOL)
      endif
*
*     Define pipe
*
      PIPEV(1)=RPIPE(1)
      PIPEV(2)=RPIPE(2)
      PIPEV(3)=150.0
      PIPEV(4)=0.0
      PIPEV(5)=360.
      CALL GSVOLU('PIPE','TUBS',IPIP,PIPEV,5,IVOL)
*
*     Define volume in front of BAR
*     
      FRNT(1) = 0.1             ! (half length x, cm)
      FRNT(2) = 5.            ! (half length y, cm)
      FRNT(3) = 5.            ! (half length z, cm)
      DISTA = -BAR(2)-1.        ! (distance from origin, cm)
      Call GSVOLU('FRNT','BOX ',LIMBO,FRNT,3,IVOL)
*
*     Define Counting surface (BACK)
*     
      BACK(1) = 5.              ! (half length x, cm)
      BACK(2) = 5.              ! (half length y, cm)
      BACK(3) = .1              ! (half length z, cm)
      Call GSVOLU('BACK','BOX ',BACKWARD,BACK,3,IVOL)
*
*     Define Q1 counting volume
*     
      QUAV(1) = 5.              ! (half length x, cm)
      QUAV(2) = 5.              ! (half length y, cm)
      QUAV(3) = .1              ! (half length z, cm)
      Call GSVOLU('QUA1','BOX ',CHECKQ1,QUAV,3,IVOL)
      Call GSVOLU('QUA2','BOX ',CHECKQ2,QUAV,3,IVOL)
*
*     Define CALOrimeter
*     
      S_SCI  = 0.07
      S_PB   = 0.04
      S_IRON = 0.05
c      S_IRON = 1.
c
c     Pb/SCI planes
c
      If(CALOGEO.EQ.1) Then
         IRONDIM(1) = 24.
         IRONDIM(2) = 14.
         IRONDIM(3) = 40.      
         CALODIM(1) =  IRONDIM(1) - S_IRON
         CALODIM(2) = (IRONDIM(2) - S_IRON)/4
         CALODIM(3) =  IRONDIM(3) - S_IRON
         NPLA = CALODIM(2)/(S_SCI+S_PB) 
         NPLANES = INT(NPLA) + 1 
         OFFSET = S_SCI - (S_SCI + S_PB)/2.
      ElseIf(CALOGEO.EQ.2) Then
         IRONDIM(1) = 14.       ! 24.
         IRONDIM(2) = 40        ! 28.
         IRONDIM(3) = 48        ! 40.      
         CALODIM(1) =  (IRONDIM(3) - S_IRON)/2
         CALODIM(2) =  IRONDIM(1) - S_IRON
         CALODIM(3) =  IRONDIM(2) - S_IRON
         NPLA = CALODIM(1)/(S_SCI+S_PB) 
         NPLANES = INT(NPLA) + 1 
         OFFSET = S_SCI - (S_SCI + S_PB)/2.
      ElseIf(CALOGEO.EQ.3) Then
         IRONDIM(1) = 28.       ! 14.
         IRONDIM(2) = 40        ! 40.
         IRONDIM(3) = 24        ! 48.      
         CALODIM(1) =  IRONDIM(3) - S_IRON
         CALODIM(2) =  (IRONDIM(1) - S_IRON)/2
         CALODIM(3) =  IRONDIM(2) - S_IRON
         NPLA = CALODIM(1)/(S_SCI+S_PB) 
         NPLANES = INT(NPLA) + 1 
         OFFSET = S_SCI - (S_SCI + S_PB)/2.
      EndIf
c
c     Iron
c
      CALOEXT(1) = 0.5*IRONDIM(3) ! (half length x, cm)
      CALOEXT(2) = 0.5*IRONDIM(2) ! (half length y, cm)
      CALOEXT(3) = 0.5*IRONDIM(1) ! (half length z, cm)
      CALL GSVOLU('IRON','BOX ',SKIN,CALOEXT,3,IVOL)
c
      CALO(1) = 0.5*CALODIM(3)  ! (half length x, cm)
      CALO(2) = 0.5*CALODIM(2)  ! (half length y, cm)
      CALO(3) = 0.5*CALODIM(1)  ! (half length z, cm)
c
      CALL GSVOLU('EC2M','BOX ',LIMBO,CALO,3,IVOL)
      CALL GSDVT ('C_PB','EC2M',S_SCI+S_PB,2,R_LEAD,NPLANES)
      CALL GSDVN2('C_SC','C_PB',1,2,OFFSET,R_ACTIVE,6)
*
*     Position volumes...
*
      CALL GSROTM(1,ROTA(1),ROTA(2),ROTA(3),ROTA(4),ROTA(5),ROTA(6))
      CALL GSROTM(2,ROTB(1),ROTB(2),ROTB(3),ROTB(4),ROTB(5),ROTB(6))
      CALL GSROTM(3,ROTC(1),ROTC(2),ROTC(3),ROTC(4),ROTC(5),ROTC(6))
c
      CALL GSPOS('PIPE',1,'MOTH',0.0,0.0,DDBAR+PIPEV(3),0,'ONLY')
      CALL GSPOS('FRNT',1,'MOTH',0.0,0.0,DISTA,1,'ONLY')

      CALL GSPOS('QUA1',1,'MOTH',0.0,0.0,ZMAX(1)+1.,0,'ONLY')      
      CALL GSPOS('QUA2',1,'MOTH',0.0,0.0,ZMAX(2)+1.,0,'ONLY')
c
      if(igeobar.ne.0) CALL GSPOS('BAR ',1,'MOTH',0.0,0.0,0.0,2,'ONLY')
c
      CALL GSPOS('IRON',1,'MOTH',0.0,0.0,DISTAC+CALOEXT(1)
     +     ,0,'ONLY')
      DISTABACK=BAR(2)+1.
      CALL GSPOS('BACK',1,'MOTH',0.0,0.0,DISTABACK,0,'ONLY')
      If(CALOGEO.EQ.1) Then
         CALL GSPOS('EC2M',1,'IRON',0.0,-3.0*CALO(2),0.0,0,'ONLY')
         CALL GSPOS('EC2M',2,'IRON',0.0,-1.0*CALO(2),0.0,0,'ONLY')
         CALL GSPOS('EC2M',3,'IRON',0.0, 1.0*CALO(2),0.0,0,'ONLY')
         CALL GSPOS('EC2M',4,'IRON',0.0, 3.0*CALO(2),0.0,0,'ONLY')
      ElseIf(CALOGEO.EQ.2) Then
         CALL GSPOS('EC2M',1,'IRON',-0.5*CALODIM(1),0.0,0.0,3,'ONLY')
         CALL GSPOS('EC2M',4,'IRON', 0.5*CALODIM(1),0.0,0.0,3,'ONLY')         
      ElseIf(CALOGEO.EQ.3) Then
         CALL GSPOS('EC2M',1,'IRON',0.0,0.0,-0.5*CALODIM(2),3,'ONLY')
         CALL GSPOS('EC2M',4,'IRON',0.0,0.0,0.5*CALODIM(2),3,'ONLY')         
      EndIf
*
*     Close geometry banks
*
      CALL GGCLOS
      RETURN
      END
*
*
*
      SUBROUTINE UGEOM_DRAW
******************************************************************
*     Draw the geometry:
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
*
      CALL GDOPT ('HIDE','OFF ')
      CALL GDOPT ('SHAD','OFF ')
      CALL GDOPT ('PROJ','PERS')
      CALL GSATT ('MOTH','SEEN',0)
      CALL GSATT ('PIPE','SEEN',5)
      CALL GSATT ('PIPE','COLO',1)
      CALL GSATT ('BAR ','COLO',1)
      CALL GSATT ('BACK','COLO',5)
      CALL GSATT ('FRNT','COLO',5)
      CALL GSATT ('QUA1','COLO',5)
      CALL GSATT ('QUA2','COLO',5)
c
      CALL GSATT ('C_PB','COLO',1)
      CALL GSATT ('C_SC','COLO',4)
      CALL GSATT ('IRON','COLO',1)
c
      CALL GSATT ('FRNT','SEEN',1)
      CALL GSATT ('BACK','SEEN',1)
      CALL GSATT ('QUA1','SEEN',1)
      CALL GSATT ('QUA2','SEEN',1)
      CALL GSATT ('IRON','SEEN',1)
      CALL GSATT ('EC2M','SEEN',1)
      CALL GSATT ('C_PB','SEEN',1)
      CALL GSATT ('C_SC','SEEN',1)
*
c      CALL GDRAWC('MOTH',2,0.,10.,10.,RSCALE(1),RSCALE(2))
c      CALL GDAXIS(XBEAM(1),XBEAM(2),XBEAM(3),50.)
      CALL GDRAW ('MOTH',RSCALE(3),RSCALE(4),RSCALE(5),10.,10.
     &     ,RSCALE(1),RSCALE(2))
      CALL GDAXIS(XBEAM(1),XBEAM(2),XBEAM(3),50.)
c
c      CALL GDRAWX('MOTH',0.,0.,0.,RSCALE(3),RSCALE(4),10.,10.
c     &     ,RSCALE(1),RSCALE(2))
*
      RETURN
      END
*
*
*
      SUBROUTINE UPRINT
******************************************************************
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      Print *,
     &     ' **********************************************************'
      Print *,
     &     ' *                                                        *'
      Print *,
     &     ' *                          BTFBeam                       *'
      Print *,
     &     ' *                                                        *'
      Print *,
     &     ' **********************************************************'
      Print *,' '
      Print *,' '
      Print *,'         version 0.5 (19/11/2014)'
      Print *,' '
      Print *,'  P. Valente'
      Print *,' '
      Print *,
     &     ' **********************************************************'
*
*     Print materials, tracking media and volumes
*            
C      CALL GPRINT('MATE',0)
c      CALL GPRINT('TMED,0)
c      CALL GPRINT('VOLU',0)
*
      RETURN
      END
*
*
*
      SUBROUTINE UGLAST
******************************************************************
*     Close everything, save output...
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      INTEGER ICYCLE
      CALL GLAST                ! Terminate GEANT
      Call IGEND                ! Close HIGZ graphics
      CLOSE(10)                 ! Close Postscript
*
      CALL HCDIR('//BTF',' ')
      CALL HROUT(0,ICYCLE,' ')
      CALL HREND('BTF')
      OPEN(3,FILE='btfbeam.seed',STATUS='UNKNOWN')
      WRITE(3,*) IDRUN,NRNDM(1),NRNDM(2)
      CLOSE(3)
      print *,'FILE closed.'
*
      RETURN
      END
*
*
*
      SUBROUTINE GUKINE
******************************************************************
*     Generate kinematics for the primary track
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
*
*     Local variables
*
      REAL THETA,PHI,PTOT
      INTEGER NVERT,NT
c
      CALL BEAM(SXBEAM(1),SXBEAM(2),SPBEAM,EMIT,SPREAD)
*     
*     set beam origin
*
      VBEAM(1) = XBEAM(1)+SPREAD(1)
      VBEAM(2) = XBEAM(2)+SPREAD(2)
      VBEAM(3) = XBEAM(3)
*
      PTOT  = PBEAM(1)+SPREAD(3)
      THETA = PBEAM(2)+SPREAD(4)
      PHI   = PBEAM(3)+SPREAD(5)
C
      PLBEAM(1) = PTOT*SIN(THETA)*COS(PHI)
      PLBEAM(2) = PTOT*SIN(THETA)*SIN(PHI)
      PLBEAM(3) = PTOT*COS(THETA)
      
C     store the first vertex and retrieve the actual 
C     vertex number NVERT from the JVERTX data structure

      CALL GSVERT(VBEAM,0,0,0,0,NVERT)

C     store the particle type IPART with its given momentum 
C     in the particle stack and attach the primary vertex 
C     NVERT to it

      CALL GSKINE(PLBEAM,IKINE,NVERT,0,0,NT)
*
*     print kinematics
*      
c      CALL GPRINT('VERT',0)
c      CALL GPRINT('KINE',0)

      RETURN
      END
*
*
*
      SUBROUTINE GUSTEP
******************************************************************
*     End of tracking...
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      Integer IG,IM,ij
      Character*4 VOLNAM
      Real KBAR,DEDX,SINTHE
*
      If (IEVENT.LE.10) CALL GSXYZ(0)
*
*     NGKINE is > 0 if secondary particle have been generated,
*     store them to the particle stack
*
      If (NGKINE.GT.0) Then
         Do IG=1,NGKINE
            IFLGK(IG) = 1       ! flag to add track to JSTAK and JKINE 
                                ! data structures
                                ! This is needed to get track information
                                ! using KXYZ
         EndDo
         Call GSKING(0)         ! all secondary tracks stored for tracking
      EndIf      
      WRITE(VOLNAM,'(A4)') NAMES(NLEVEL)
      if (ipart.gt.4.or.ipart.lt.1) Then
         print *,volnam,ipart,nrndm(1),nrndm(2)
      endif

      If(VOLNAM.EQ.'BAR') Then
         IBAR = IBAR+1
         If(IBAR.LE.MAXPART) Then
            XBAR(IBAR)=VECT(1)                     ! VECT contiene le variabili della particella in esame:
            YBAR(IBAR)=VECT(2)                     ! VECT(1,2,3) la posizione (x,y,z), VECT(7) l'energia
            ZBAR(IBAR)=VECT(3)                     ! VECT(4,5,6) i coseni direttori
            PXBAR(IBAR)=VECT(4)*VECT(7)
            PYBAR(IBAR)=VECT(5)*VECT(7)
            PZBAR(IBAR)=VECT(6)*VECT(7)
            PARTBAR(IBAR)=IPART
         EndIf
         If(IPART.GE.1.AND.IPART.LE.3) Then
            DEBAR(IPART) = DEBAR(IPART)+DESTEP
            DXBAR(IPART) = DXBAR(IPART)+STEP
            NPBAR(IPART) = NPBAR(IPART)+1
         Endif
c            print *,'NNBAR ',NNBAR,STEP
c            print *,destep,step,nstep
            If(STEP.GT.0) Then
               DEEBAR = DEEBAR+DESTEP   ! DESTEP e' l'energia persa in questo step dalla particella iesima
               DXXBAR = DXXBAR+STEP     ! STEP e' la lunghezza dello step
*
* deebar = somme de, dxxbar= somme dx
*
               THE = ACOS(VECT(4))
               SINTHE = SQRT(1.-VECT(4)**2)
               ZET = 0.5*LBAR+VECT(1)
               NNBAR = NNBAR+1
               DEDX   = DESTEP/STEP ! GeV cm^-1
c               print *,'           ',nmec,dedx  
*
* esiste la possibilita' di sapere per quale processo fisico la particella ha perso energia
* dentro lmec: per esempio muls=multiplo scattering, loss=ionizzazione, eccetera
*
c               do ij=1,nmec                               
c                  if(lmec(ij).eq.1)print *,destep,dedx,ipart,' NEXT'
c                  if(lmec(ij).eq.2)print *,destep,dedx,ipart,' MULS'
c                  if(lmec(ij).eq.3)print *,destep,dedx,ipart,' LOSS'
c                  if(lmec(ij).eq.4)print *,destep,dedx,ipart,' FIEL'
c                  if(lmec(ij).eq.5)print *,destep,dedx,ipart,' DCAY'
c                  if(lmec(ij).eq.6)print *,destep,dedx,ipart,' PAIR'
c                  if(lmec(ij).eq.7)print *,destep,dedx,ipart,' COMP'
c                  if(lmec(ij).eq.8)print *,destep,dedx,ipart,' PHOT'
c                  if(lmec(ij).eq.9)print *,destep,dedx,ipart,' BREM'
c                  if(lmec(ij).eq.10)print *,destep,dedx,ipart,' DRAY'
c                  if(lmec(ij).eq.11)print *,destep,dedx,ipart,' ANNI'
c                  if(lmec(ij).eq.30)print *,destep,dedx,ipart,' STOP'
c               enddo
*
               DEDXBAR = DEDXBAR+DEDX 
*
            EndIf
c         EndIf
      ElseIf(VOLNAM.EQ.'IRON'.AND.IPART.GE.1.AND.IPART.LE.3) Then
         DEFE(IPART) = DEFE(IPART)+DESTEP
         DXFE(IPART) = DXFE(IPART)+STEP
      ElseIf(VOLNAM.EQ.'C_PB'.AND.IPART.GE.1.AND.IPART.LE.3) Then
         DEPB(IPART) = DEPB(IPART)+DESTEP
         DXPB(IPART) = DXPB(IPART)+STEP
      ElseIf(VOLNAM.EQ.'C_SC'.AND.IPART.GE.1.AND.IPART.LE.3) Then
         DESCI(IPART) = DESCI(IPART)+DESTEP
         DXSCI(IPART) = DXSCI(IPART)+STEP
      EndIf
      
      If (INWVOL.EQ.0) Then
c     
      ElseIf (INWVOL.EQ.1) Then
c     
      ElseIf (INWVOL.EQ.2) Then
         If(VOLNAM.EQ.'FRNT') Then
            IINP = IINP+1
            XINP(IINP)=VECT(1)
            YINP(IINP)=VECT(2)
            ZINP(IINP)=VECT(3)
            PXINP(IINP)=VECT(4)*VECT(7)
            PYINP(IINP)=VECT(5)*VECT(7)
            PZINP(IINP)=VECT(6)*VECT(7)
            PARTINP(IINP)=IPART
         ElseIf(VOLNAM.EQ.'BACK') Then
            IBCK = IBCK+1
            XBCK(IBCK)=VECT(1)
            YBCK(IBCK)=VECT(2)
            ZBCK(IBCK)=VECT(3)
            PXBCK(IBCK)=VECT(4)*VECT(7)
            PYBCK(IBCK)=VECT(5)*VECT(7)
            PZBCK(IBCK)=VECT(6)*VECT(7)
            PARTBCK(IBCK)=IPART
         ElseIf(VOLNAM.EQ.'QUA1'.AND.IQUA1.LT.MAXPART) Then
            IQUA1 = IQUA1+1
            XQUA1(IQUA1)=VECT(1)
            YQUA1(IQUA1)=VECT(2)
            ZQUA1(IQUA1)=VECT(3)
            PXQUA1(IQUA1)=VECT(4)*VECT(7)
            PYQUA1(IQUA1)=VECT(5)*VECT(7)
            PZQUA1(IQUA1)=VECT(6)*VECT(7)
            PARTQUA1(IQUA1)=IPART
         ElseIf(VOLNAM.EQ.'QUA2'.AND.IQUA2.LT.MAXPART) Then
            IQUA2 = IQUA2+1
            XQUA2(IQUA2)=VECT(1)
            YQUA2(IQUA2)=VECT(2)
            ZQUA2(IQUA2)=VECT(3)
            PXQUA2(IQUA2)=VECT(4)*VECT(7)
            PYQUA2(IQUA2)=VECT(5)*VECT(7)
            PZQUA2(IQUA2)=VECT(6)*VECT(7)
            PARTQUA2(IQUA2)=IPART
         ElseIf(VOLNAM.EQ.'BAR') Then

         EndIf
      EndIf
*
c      CALL GDCXYZ(0)                
c      CALL GPCXYZ               ! print tracking/physics parameters
c      CALL GPGKIN       
*
      RETURN
      END
*
*
*
      SUBROUTINE GUOUT
******************************************************************
*     End of event...
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      integer i
*
      NINP=IINP
      NBCK=IBCK
      NQUA1=IQUA1
      NQUA2=IQUA2
      NBAR=IBAR
      CALL HFNT(1) ! fill dell'entupla
      CALL GDXYZ(0)
c      CALL GDPART(0,10,.25)
*
      RETURN
      END
*
*
*
      SUBROUTINE BEAM(SIGMAX,SIGMAY,SIGMAE,EMITT,OUT)
******************************************************************
*     Generate the beam
      Implicit None
      Include 'btfbeam.inc'
******************************************************************
      REAL EMITT,OUT(5)
*
      REAL SIGMAX,SIGMAY,SIGMAE,SIGMAT,SIGMAP
      REAL RANDG 
c
      REAL X, Y, E, XP, YP
c
c     Start code
c
      SIGMAT = EMITT/SIGMAX
      SIGMAP = EMITT/SIGMAY
      X  = RANDG(SIGMAX)
      Y  = RANDG(SIGMAY)
      E  = RANDG(SIGMAE)
      XP = RANDG(SIGMAT)
      YP = RANDG(SIGMAP)
      OUT(1) = X
      OUT(2) = Y
      OUT(3) = E
      OUT(4) = XP
      OUT(5) = YP
*      
      RETURN
      END

*
*
*
      SUBROUTINE GUFLD(XX,FF)
******************************************************************
*     PASS magnetic field
      Implicit None
      Include 'btfbeam.inc'
******************************************************************
      REAL XX(3),FF(3),RR2
      INTEGER IQUAD,JQUAD
*
c
c     Start code
c
      FF(1) = 0.
      FF(2) = 0.
      FF(3) = 0.
      JQUAD=0
      DO IQUAD=1,QUADMAX
         IF(XX(3).LE.ZMAX(IQUAD).AND.XX(3).GE.ZMIN(IQUAD)) THEN
            JQUAD=IQUAD
            GOTO 986
         ENDIF
      ENDDO
 986  CONTINUE
      IF (JQUAD.EQ.0) GOTO 987
      RR2=XX(1)*XX(1)+XX(2)*XX(2)
      IF(RR2.GE.RPIPE(1)*RPIPE(1)) GOTO 987
      FF(1)=KX(JQUAD)*XX(2)
      FF(2)=KY(JQUAD)*XX(1)
C      print *,'GUFLD',XX,FF
*     
 987  RETURN
      END
*
*
*    
      REAL FUNCTION RANDG(SIGMA)
******************************************************************
*     Generate gaussian numbers...
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      REAL SIGMA
      INTEGER J
      REAL SUM,RVEC(12)
      SUM = 0.0
      CALL GRNDM(RVEC,12)
      DO J = 1, 12
         SUM = SUM + RVEC(J)
      ENDDO
      RANDG = (SUM - 6.0)*SIGMA 
      RETURN
      END
*
*
*
      SUBROUTINE GRUN
******************************************************************
*     Process one event... 
*     questa e' quella chiamata dal main... e che in realta' viene chiamata n volte
*     all'inizio della generazione di ciascun evento
*
      Implicit none
      Include 'btfbeam.inc'
******************************************************************
      Integer I
*
      Do I=1,NEVENT
c
c     Initialize ...
c
         IEVENT=I
         IINP=0
         IBCK=0
         IBAR=0
         IQUA1=0
         IQUA2=0
         NNBAR=0
         DXXBAR=0
         DEEBAR=0
         DEDXBAR=0
         DEDXTOTAL=0
         CALL VZERO(DEBAR,3)
         CALL VZERO(DXBAR,3)
         CALL VZERO(NPBAR,3)
         CALL VZERO(DEFE,3)
         CALL VZERO(DXFE,3)
         CALL VZERO(DEPB,3)
         CALL VZERO(DXPB,3)
         CALL VZERO(DESCI,3)
         CALL VZERO(DXSCI,3)
c
         CALL VZERO(XINP,MAXPART)
         CALL VZERO(YINP,MAXPART)
         CALL VZERO(ZINP,MAXPART)
         CALL VZERO(PXINP,MAXPART)
         CALL VZERO(PYINP,MAXPART)
         CALL VZERO(PZINP,MAXPART)
         CALL VZERO(PARTINP,MAXPART)
c
         CALL VZERO(XBCK,MAXPART)
         CALL VZERO(YBCK,MAXPART)
         CALL VZERO(ZBCK,MAXPART)
         CALL VZERO(PXBCK,MAXPART)
         CALL VZERO(PYBCK,MAXPART)
         CALL VZERO(PZBCK,MAXPART)
         CALL VZERO(PARTBCK,MAXPART)
c
         CALL VZERO(XQUA1,MAXPART)
         CALL VZERO(YQUA1,MAXPART)
         CALL VZERO(ZQUA1,MAXPART)
         CALL VZERO(PXQUA1,MAXPART)
         CALL VZERO(PYQUA1,MAXPART)
         CALL VZERO(PZQUA1,MAXPART)
         CALL VZERO(PARTQUA1,MAXPART)
c
         CALL VZERO(XQUA2,MAXPART)
         CALL VZERO(YQUA2,MAXPART)
         CALL VZERO(ZQUA2,MAXPART)
         CALL VZERO(PXQUA2,MAXPART)
         CALL VZERO(PYQUA2,MAXPART)
         CALL VZERO(PZQUA2,MAXPART)
         CALL VZERO(PARTQUA2,MAXPART)
c
         If(MOD(IEVENT,100).EQ.0)
     &        Print *,' ************************* EVENT ',
     &        ' ************************* ',I
c
c     Call the tracking now...
c
         Call GTRIGI
         Call GTRIG
         Call GTRIGC
c     
      EndDo
*
      RETURN
      END
*
