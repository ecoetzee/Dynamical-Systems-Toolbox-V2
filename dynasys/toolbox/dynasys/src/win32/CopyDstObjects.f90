#include <fintrf.h>
      SUBROUTINE GETSIMOPTS(PRHS)
!
      USE AUTO_CONSTANTS
!      
      IMPLICIT NONE
      mwPointer PRHS(*)
      mwSize mxIsInt32, mxIsDouble, mxIsChar, mxGetString, STATUS
      mwSize, DIMENSION(1)::D1
      mwSize, DIMENSION(2)::D2
      mwSize, DIMENSION(3)::D3
      mwPointer D1ptr, D2ptr, D3ptr
      mwPointer mxGetProperty, mxGetPr, mxGetDimensions
      mwIndex :: i = 1
      mwSize :: n = 1
      mwSize m,o,numel,mxGetNumberOfElements
      CHARACTER*3  STR
      CHARACTER*63 FCNSTR
!
! Run options
      mwPointer RUNMODEptr1
      mwPointer FUNCFILENAMEptr1
      mwPointer STPNTFILENAMEptr1
      mwPointer BCNDFILENAMEptr1
      mwPointer ICNDFILENAMEptr1
      mwPointer FOPTFILENAMEptr1
      mwPointer PVLSFILENAMEptr1
      mwPointer FORT7ptr1
      mwPointer FORT8ptr1
      mwPointer FORT9ptr1
!
! INPUT OUTPUT SPECIFICATION --------------------------
!
! Assign RunMode values -------------- 
      RUNMODEptr1=mxGetProperty(PRHS(1),i,'RunMode')
!
      IF( RUNMODEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property RunMode not found")
      ENDIF
!
      IF( mxIsChar(RUNMODEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property RunMode is not a character arra
     &y")
      ENDIF
!
      STR=''
      STATUS=mxGetString(RUNMODEptr1, STR, 3); 
      CALL mxDestroyArray(RUNMODEptr1)
!      
      IF (STR.EQ."DST")THEN
        RUNMODEDST=1
        FORT2DST=0
        FORT3DST=0
      ELSE
        RUNMODEDST=0
        FORT2DST=1
        FORT3DST=1
        FORT7DST=1
        FORT8DST=1
        FORT9DST=1
        GOTO 102
      ENDIF    
!      
! Assign Fort7 values -------------- 
      FORT7ptr1=mxGetProperty(PRHS(1),i,'Fort7')
!
      IF( FORT7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Fort7 not found")
      ENDIF
!
      IF( mxIsChar(FORT7ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Fort7 is not a character array"
     &)
      ENDIF
!
      STR=''
      STATUS=mxGetString(FORT7ptr1, STR, 3); 
      CALL mxDestroyArray(FORT7ptr1)
!      
      IF (STR.EQ."off")THEN
        FORT7DST=0
      ELSE
        FORT7DST=1
      ENDIF    
!  
! Assign Fort8 values -------------- 
      FORT8ptr1=mxGetProperty(PRHS(1),i,'Fort8')
!
      IF( FORT8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Fort8 not found")
      ENDIF
!
      IF( mxIsChar(FORT8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Fort8 is not a character array"
     &)
      ENDIF
!
      STR=''
      STATUS=mxGetString(FORT8ptr1, STR, 3); 
      CALL mxDestroyArray(FORT8ptr1)
!      
      IF (STR.EQ."off")THEN
        FORT8DST=0
      ELSE
        FORT8DST=1
      ENDIF    
!                      
! Assign Fort9 values -------------- 
      FORT9ptr1=mxGetProperty(PRHS(1),i,'Fort9')
!
      IF( FORT9ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Fort9 not found")
      ENDIF
!
      IF( mxIsChar(FORT9ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Fort9 is not a character array"
     &)
      ENDIF
!
      STR=''
      STATUS=mxGetString(FORT9ptr1, STR, 3); 
      CALL mxDestroyArray(FORT9ptr1)
!      
      IF (STR.EQ."off")THEN
        FORT9DST=0
      ELSE
        FORT9DST=1
      ENDIF  
!
! Assign FuncFileName values -------------- 
102   FUNCFILENAMEptr1=mxGetProperty(PRHS(1),i,'FuncFileName')
!
      IF( FUNCFILENAMEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property FuncFileName not found")
      ENDIF
!
      IF( mxIsChar(FUNCFILENAMEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property FuncFileName is not a character
     & array")
      ENDIF
!
      m=mxGetNumberOfElements(FUNCFILENAMEptr1)
      FCNSTR=''
      STATUS=mxGetString(FUNCFILENAMEptr1, FCNSTR, m);
      WRITE(FUNCFILENAME,101)TRIM(FCNSTR)
      CALL mxDestroyArray(FUNCFILENAMEptr1)
!      
!
! Assign StpntFileName values -------------- 
      STPNTFILENAMEptr1=mxGetProperty(PRHS(1),i,'StpntFileName')
!
      IF( STPNTFILENAMEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property StpntFileName not found")
      ENDIF
!
      IF( mxIsChar(STPNTFILENAMEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property StpntFileName is not a characte
     &r array")
      ENDIF
!
      m=mxGetNumberOfElements(STPNTFILENAMEptr1)
      FCNSTR=''
      STATUS=mxGetString(STPNTFILENAMEptr1, FCNSTR, m);
      WRITE(STPNTFILENAME,101)TRIM(FCNSTR)
      CALL mxDestroyArray(STPNTFILENAMEptr1)        
! 
! Assign BcndFileName values -------------- 
      BCNDFILENAMEptr1=mxGetProperty(PRHS(1),i,'BcndFileName')
!
      IF( BCNDFILENAMEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property BcndFileName not found")
      ENDIF
!
      IF( mxIsChar(BCNDFILENAMEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property BcndFileName is not a character
     & array")
      ENDIF
!
      m=mxGetNumberOfElements(BCNDFILENAMEptr1)
      FCNSTR=''
      STATUS=mxGetString(BCNDFILENAMEptr1, FCNSTR, m);
      WRITE(BCNDFILENAME,101)TRIM(FCNSTR)
      CALL mxDestroyArray(BCNDFILENAMEptr1)        
! 
! Assign IcndFileName values -------------- 
      ICNDFILENAMEptr1=mxGetProperty(PRHS(1),i,'IcndFileName')
!
      IF( ICNDFILENAMEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property IcndFileName not found")
      ENDIF
!
      IF( mxIsChar(ICNDFILENAMEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property IcndFileName is not a character
     & array")
      ENDIF
!
      m=mxGetNumberOfElements(ICNDFILENAMEptr1)
      FCNSTR=''
      STATUS=mxGetString(ICNDFILENAMEptr1, FCNSTR, m);
      WRITE(ICNDFILENAME,101)TRIM(FCNSTR)
      CALL mxDestroyArray(ICNDFILENAMEptr1)        
! 
! Assign FoptFileName values -------------- 
      FOPTFILENAMEptr1=mxGetProperty(PRHS(1),i,'FoptFileName')
!
      IF( FOPTFILENAMEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property FoptFileName not found")
      ENDIF
!
      IF( mxIsChar(FOPTFILENAMEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property FoptFileName is not a character
     & array")
      ENDIF
!
      m=mxGetNumberOfElements(FOPTFILENAMEptr1)
      FCNSTR=''
      STATUS=mxGetString(FOPTFILENAMEptr1, FCNSTR, m);
      WRITE(FOPTFILENAME,101)TRIM(FCNSTR)
      CALL mxDestroyArray(FOPTFILENAMEptr1)        
! 
! Assign PvlsFileName values -------------- 
      PVLSFILENAMEptr1=mxGetProperty(PRHS(1),i,'PvlsFileName')
!
      IF( PVLSFILENAMEptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property PvlsFileName not found")
      ENDIF
!
      IF( mxIsChar(PVLSFILENAMEptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property PvlsFileName is not a character
     & array")
      ENDIF
!
      m=mxGetNumberOfElements(PVLSFILENAMEptr1)
      FCNSTR=''
      STATUS=mxGetString(PVLSFILENAMEptr1, FCNSTR, m);
      WRITE(PVLSFILENAME,101)TRIM(FCNSTR)
      CALL mxDestroyArray(PVLSFILENAMEptr1)        
! 
101   FORMAT(A)           
      RETURN
      END
!--------------------------------------------------------------------------
      SUBROUTINE COPYDSTOBJECTSTOAUTO(PRHS)
!
      USE AUTO_CONSTANTS
!
      IMPLICIT NONE
      mwPointer PRHS(*)
      mwSize mxIsInt32, mxIsDouble, mxIsChar, mxGetString, STATUS
      mwSize, DIMENSION(1)::D1
      mwSize, DIMENSION(2)::D2
      mwSize, DIMENSION(3)::D3
      mwPointer D1ptr, D2ptr, D3ptr
      mwPointer mxGetProperty, mxGetPr, mxGetDimensions 
      mwIndex :: i = 1
      mwSize :: n = 1
      mwSize mxGetNumberOfDimensions
      mwSize m,o,numel,numdim,mxGetNumberOfElements
!
! Constants
      mwPointer NDIMptr1, NDIMptr2
      mwPointer NOUTXptr1, NOUTXptr2
      mwPointer IPSptr1, IPSptr2
      mwPointer IRSptr1, IRSptr2
      mwPointer ILPptr1, ILPptr2
      mwPointer NICPptr1, NICPptr2
      mwPointer ICPptr1, ICPptr2
      mwPointer NTSTptr1, NTSTptr2
      mwPointer NCOLptr1, NCOLptr2
      mwPointer IADptr1, IADptr2
      mwPointer ISPptr1, ISPptr2
      mwPointer ISWptr1, ISWptr2
      mwPointer IPLTptr1, IPLTptr2
      mwPointer NBCptr1, NBCptr2
      mwPointer NINTptr1, NINTptr2
      mwPointer NMXptr1, NMXptr2
      mwPointer RL0ptr1, RL0ptr2
      mwPointer RL1ptr1, RL1ptr2
      mwPointer A0ptr1, A0ptr2
      mwPointer A1ptr1, A1ptr2
      mwPointer NPRptr1, NPRptr2
      mwPointer MXBFptr1, MXBFptr2
      mwPointer IIDptr1, IIDptr2
      mwPointer ITMXptr1, ITMXptr2
      mwPointer ITNWptr1, ITNWptr2
      mwPointer NWTNptr1, NWTNptr2
      mwPointer JACptr1, JACptr2
      mwPointer EPSLptr1, EPSLptr2
      mwPointer EPSUptr1, EPSUptr2
      mwPointer EPSSptr1, EPSSptr2
      mwPointer DSptr1, DSptr2
      mwPointer DSMINptr1, DSMINptr2
      mwPointer DSMAXptr1, DSMAXptr2
      mwPointer IADSptr1, IADSptr2
      mwPointer NTHLptr1, NTHLptr2
      mwPointer ITHLptr1, ITHLptr2
      mwPointer VTHLptr1, VTHLptr2
      mwPointer NTHUptr1, NTHUptr2
      mwPointer ITHUptr1, ITHUptr2
      mwPointer VTHUptr1, VTHUptr2
      mwPointer NUZRptr1, NUZRptr2
      mwPointer IUZptr1, IUZptr2
      mwPointer VUZptr1, VUZptr2
!
! Initial states
      mwPointer PARDST0ptr1, PARDST0ptr2
      mwPointer UDST0ptr1, UDST0ptr2
      mwPointer OUTDST0ptr1, OUTDST0ptr2 
!
! F8 variables
      mwPointer IBRF8ptr1, IBRF8ptr2
      mwPointer MTOTF8ptr1, MTOTF8ptr2
      mwPointer ITPF8ptr1, ITPF8ptr2
      mwPointer LABF8ptr1, LABF8ptr2
      mwPointer NFPRF8ptr1, NFPRF8ptr2
      mwPointer ISWF8ptr1, ISWF8ptr2
      mwPointer NTPLF8ptr1, NTPLF8ptr2
      mwPointer NARF8ptr1, NARF8ptr2
      mwPointer NROWPRF8ptr1, NROWPRF8ptr2
      mwPointer NTSTF8ptr1, NTSTF8ptr2
      mwPointer NCOLF8ptr1, NCOLF8ptr2
      mwPointer NPARXF8ptr1, NPARXF8ptr2
      mwPointer IFPRF8ptr1, IFPRF8ptr2
      mwPointer TF8ptr1, TF8ptr2
      mwPointer TMF8ptr1, TMF8ptr2
      mwPointer PARF8ptr1, PARF8ptr2
      mwPointer RLDOTF8ptr1, RLDOTF8ptr2
      mwPointer UF8ptr1, UF8ptr2
      mwPointer UPSF8ptr1, UPSF8ptr2
      mwPointer UDOTPSF8ptr1, UDOTPSF8ptr2
!
!
! CONSTANTS PASSED FROM CONSTANTS OBJECT -------------------------------------
!
! Assign Ndim values -------------- 
      NDIMptr1=mxGetProperty(PRHS(2),i,'Ndim')
!
      IF( NDIMptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ndim not found")
      ENDIF
!
      IF( mxIsInt32(NDIMptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ndim is not an integer")
      ENDIF
!
      NDIMptr2=mxGetPr(NDIMptr1)
!
      IF( NDIMptr2 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ndim seems to be undefined")
      ENDIF 
!      
      CALL mxCopyPtrToInteger4(NDIMptr2,NDIMDST,n)
      CALL mxDestroyArray(NDIMptr1)
!      
! Assign Noutx values -------------- 
      NOUTXptr1=mxGetProperty(PRHS(2),i,'Noutx')
!
      IF( NOUTXptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Noutx not found")
      ENDIF
!
      IF( mxIsInt32(NOUTXptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Noutx is not an integer")
      ENDIF
!
      NOUTXptr2=mxGetPr(NOUTXptr1)
!
      IF(NOUTXptr2==0)THEN
        NOUTXDST=0
      ELSE
        CALL mxCopyPtrToInteger4(NOUTXptr2,NOUTXDST,n)
        CALL mxDestroyArray(NOUTXptr1)
      ENDIF  
!      
! Assign Ips values -------------- 
      IPSptr1=mxGetProperty(PRHS(2),i,'Ips')
!
      IF( IPSptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ips not found")
      ENDIF
!
      IF( mxIsInt32(IPSptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ips is not an integer")
      ENDIF
!
      IPSptr2=mxGetPr(IPSptr1)
      CALL mxCopyPtrToInteger4(IPSptr2,IPSDST,n)
      CALL mxDestroyArray(IPSptr1)
!
! Assign Irs values -------------- 
      IRSptr1=mxGetProperty(PRHS(2),i,'Irs')
!
      IF( IRSptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Irs not found")
      ENDIF
!
      IF( mxIsInt32(IRSptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Irs is not an integer")
      ENDIF
!
      IRSptr2=mxGetPr(IRSptr1)
!
      IF( IRSptr2 == 0 ) THEN
        IRSDST=0
      ELSE            
        CALL mxCopyPtrToInteger4(IRSptr2,IRSDST,n)
        CALL mxDestroyArray(IRSptr1)
      ENDIF
!
! Assign Ilp values -------------- 
      ILPptr1=mxGetProperty(PRHS(2),i,'Ilp')
!
      IF( ILPptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ilp not found")
      ENDIF
!
      IF( mxIsInt32(ILPptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ilp is not an integer")
      ENDIF
!
      ILPptr2=mxGetPr(ILPptr1)
!    
      IF( ILPptr2 == 0) THEN
            ILPDST=0
      ELSE
        CALL mxCopyPtrToInteger4(ILPptr2,ILPDST,n)
      ENDIF
      CALL mxDestroyArray(ILPptr1)  
!
! Assign Nicp values -------------- 
      NICPptr1=mxGetProperty(PRHS(2),i,'Nicp')
!
      IF( NICPptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nicp not found")
      ENDIF
!
      IF( mxIsInt32(NICPptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nicp is not an integer")
      ENDIF
!
      NICPptr2=mxGetPr(NICPptr1)      
!      
      IF( NICPptr2 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nicp seems to be undefined")
      ELSE      
        CALL mxCopyPtrToInteger4(NICPptr2,NICPDST,n)
      ENDIF
      CALL mxDestroyArray(NICPptr1)      
!
! Assign Icp values -------------- 
      ICPptr1=mxGetProperty(PRHS(2),i,'Icp')
!
      IF( ICPptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Icp not found")
      ENDIF
!
      IF( mxIsInt32(ICPptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Icp is not an integer")
      ENDIF
!
      ICPptr2=mxGetPr(ICPptr1)
      CALL mxCopyPtrToInteger4(ICPptr2,ICPDST,NICPDST)
      CALL mxDestroyArray(ICPptr1)      
!
! Assign Ntst values -------------- 
      NTSTptr1=mxGetProperty(PRHS(2),i,'Ntst')
!
      IF( NTSTptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntst not found")
      ENDIF
!
      IF( mxIsInt32(NTSTptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntst is not an integer")
      ENDIF
!
      NTSTptr2=mxGetPr(NTSTptr1)
!
      IF( NTSTptr2 == 0) THEN      
        NTSTDST=0
      ELSE  
        CALL mxCopyPtrToInteger4(NTSTptr2,NTSTDST,n)
        CALL mxDestroyArray(NTSTptr1)
      ENDIF
!
! Assign Ncol values -------------- 
      NCOLptr1=mxGetProperty(PRHS(2),i,'Ncol')
!
      IF( NCOLptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ncol not found")
      ENDIF
!
      IF( mxIsInt32(NCOLptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ncol is not an integer")
      ENDIF
!
      NCOLptr2=mxGetPr(NCOLptr1)
!
      IF( NCOLptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Ncol seems to be undefined. Setting
     & to default value of 4')
        NCOLDST=4
      ELSE      
        CALL mxCopyPtrToInteger4(NCOLptr2,NCOLDST,n)
      ENDIF
      CALL mxDestroyArray(NCOLptr1)
!
! Assign Iad values -------------- 
      IADptr1=mxGetProperty(PRHS(2),i,'Iad')
!
      IF( IADptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iad not found")
      ENDIF
!
      IF( mxIsInt32(IADptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iad is not an integer")
      ENDIF
!
      IADptr2=mxGetPr(IADptr1)
!
      IF( IADptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Iad seems to be undefined. Setting 
     &to defualt value of 3')
        IADDST=3
      ELSE        
        CALL mxCopyPtrToInteger4(IADptr2,IADDST,n)
      ENDIF
      CALL mxDestroyArray(IADptr1)
!
! Assign Isp values -------------- 
      ISPptr1=mxGetProperty(PRHS(2),i,'Isp')
!
      IF( ISPptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isp not found")
      ENDIF
!
      IF( mxIsInt32(ISPptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isp is not an integer")
      ENDIF
!
      ISPptr2=mxGetPr(ISPptr1)
!
      IF( ISPptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Isp seems to be undefined. Setting 
     &to defualt value of 0')
        ISPDST=0
      ELSE      
        CALL mxCopyPtrToInteger4(ISPptr2,ISPDST,n)
      ENDIF
      CALL mxDestroyArray(ISPptr1)
!
! Assign Isw values -------------- 
      ISWptr1=mxGetProperty(PRHS(2),i,'Isw')
!
      IF( ISWptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isw not found")
      ENDIF
!
      IF( mxIsInt32(ISWptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isw is not an integer")
      ENDIF
!
      ISWptr2=mxGetPr(ISWptr1)
!
      IF( ISWptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Isw seems to be undefined. Setting 
     &to defualt value of 1')
        ISWDST=1
      ELSE        
        CALL mxCopyPtrToInteger4(ISWptr2,ISWDST,n)
      ENDIF
      CALL mxDestroyArray(ISWptr1)
!
! Assign Iplt values -------------- 
      IPLTptr1=mxGetProperty(PRHS(2),i,'Iplt')
!
      IF( IPLTptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iplt not found")
      ENDIF
!
      IF( mxIsInt32(IPLTptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iplt is not an integer")
      ENDIF
!
      IPLTptr2=mxGetPr(IPLTptr1)
!
      IF( IPLTptr2 == 0) THEN
        IPLTDST = 0
      ELSE      
        CALL mxCopyPtrToInteger4(IPLTptr2,IPLTDST,n)
      ENDIF
      CALL mxDestroyArray(IPLTptr1)
!
! Assign Nbc values -------------- 
      NBCptr1=mxGetProperty(PRHS(2),i,'Nbc')
!
      IF( NBCptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nbc not found")
      ENDIF
!
      IF( mxIsInt32(NBCptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nbc is not an integer")
      ENDIF
!
      NBCptr2=mxGetPr(NBCptr1)
!
      IF( NBCptr2 == 0 ) THEN
        NBCDST = 0
      ELSE   
        CALL mxCopyPtrToInteger4(NBCptr2,NBCDST,n)
      ENDIF
      CALL mxDestroyArray(NBCptr1)
!
! Assign Nint values -------------- 
      NINTptr1=mxGetProperty(PRHS(2),i,'Nint')
!
      IF( NINTptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nint not found")
      ENDIF
!
      IF( mxIsInt32(NINTptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nint is not an integer")
      ENDIF
!
      NINTptr2=mxGetPr(NINTptr1)
!
      IF( NINTptr2 == 0 ) THEN
        NINTDST = 0
      ELSE   
        CALL mxCopyPtrToInteger4(NINTptr2,NINTDST,n)
      ENDIF
      CALL mxDestroyArray(NINTptr1)
!
! Assign Nmx values -------------- 
      NMXptr1=mxGetProperty(PRHS(2),i,'Nmx')
!
      IF( NMXptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nmx not found")
      ENDIF
!
      IF( mxIsInt32(NMXptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nmx is not an integer")
      ENDIF
!
      NMXptr2=mxGetPr(NMXptr1)
!
      IF( NMXptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Nmx seems to be undefined. Setting 
     &to defualt value of 100')
        NMXDST = 100
      ELSE 
        CALL mxCopyPtrToInteger4(NMXptr2,NMXDST,n)
      ENDIF
      CALL mxDestroyArray(NMXptr1)
!
! Assign Rl0 values -------------- 
      RL0ptr1=mxGetProperty(PRHS(2),i,'Rl0')
!
      IF( RL0ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rl0 not found")
      ENDIF
!
      IF( mxIsDouble(RL0ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rl0 is not a double")
      ENDIF
!
      RL0ptr2=mxGetPr(RL0ptr1)
!
      IF( RL0ptr2 == 0)THEN
        CALL mexWarnMsgTxt('Lower bound Rl0 seems to be undefined. Setti
     &ng to defualt value of 0')
        RL0DST = 0
      ELSE       
        CALL mxCopyPtrToReal8(RL0ptr2,RL0DST,n)
      ENDIF
      CALL mxDestroyArray(RL0ptr1)
!
! Assign Rl1 values -------------- 
      RL1ptr1=mxGetProperty(PRHS(2),i,'Rl1')
!
      IF( RL1ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rl1 not found")
      ENDIF
!
      IF( mxIsDouble(RL1ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rl1 is not a double")
      ENDIF
!
      RL1ptr2=mxGetPr(RL1ptr1)
!
      IF( RL1ptr2 == 0)THEN
        CALL mexWarnMsgTxt('Upper bound Rl1 seems to be undefined. Setti
     &ng to defualt value of 1')
        RL1DST = 1
      ELSE       
        CALL mxCopyPtrToReal8(RL1ptr2,RL1DST,n)
      ENDIF
      CALL mxDestroyArray(RL1ptr1)
!
! Assign A0 values -------------- 
      A0ptr1=mxGetProperty(PRHS(2),i,'A0')
!
      IF( A0ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property A0 not found")
      ENDIF
!
      IF( mxIsDouble(A0ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property A0 is not a double")
      ENDIF
!
      A0ptr2=mxGetPr(A0ptr1)
!
      IF( A0ptr2 == 0)THEN
        CALL mexWarnMsgTxt('Lower bound A0 seems to be undefined. Settin
     &g to defualt value of 0')
        A0DST = 0
      ELSE       
        CALL mxCopyPtrToReal8(A0ptr2,A0DST,n)
      ENDIF
      CALL mxDestroyArray(A0ptr1)
!
! Assign A1 values -------------- 
      A1ptr1=mxGetProperty(PRHS(2),i,'A1')
!
      IF( A1ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property A1 not found")
      ENDIF
!
      IF( mxIsDouble(A1ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property A1 is not a double")
      ENDIF
!
      A1ptr2=mxGetPr(A1ptr1)
!
      IF( A1ptr2 == 0)THEN
        CALL mexWarnMsgTxt('Upper bound A1 seems to be undefined. Settin
     &g to defualt value of 1')
        A1DST = 1
      ELSE       
        CALL mxCopyPtrToReal8(A1ptr2,A1DST,n)
      ENDIF
      CALL mxDestroyArray(A1ptr1)
!
! Assign Npr values -------------- 
      NPRptr1=mxGetProperty(PRHS(2),i,'Npr')
!
      IF( NPRptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Npr not found")
      ENDIF
!
      IF( mxIsInt32(NPRptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Npr is not an integer")
      ENDIF
!
      NPRptr2=mxGetPr(NPRptr1)
      IF( NPRptr2 == 0)THEN
        NPRDST = 0
      ELSE       
        CALL mxCopyPtrToInteger4(NPRptr2,NPRDST,n)
      ENDIF
      CALL mxDestroyArray(NPRptr1)
!
! Assign Mxbf values -------------- 
      MXBFptr1=mxGetProperty(PRHS(2),i,'Mxbf')
!
      IF( MXBFptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Mxbf not found")
      ENDIF
!
      IF( mxIsInt32(MXBFptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Mxbf is not an integer")
      ENDIF
!
      MXBFptr2=mxGetPr(MXBFptr1)
!      
      IF( MXBFptr2 == 0)THEN
        MXBFDST = 0
      ELSE       
        CALL mxCopyPtrToInteger4(MXBFptr2,MXBFDST,n)
      ENDIF
      CALL mxDestroyArray(MXBFptr1)
!
! Assign Iid values -------------- 
      IIDptr1=mxGetProperty(PRHS(2),i,'Iid')
!
      IF( IIDptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iid not found")
      ENDIF
!
      IF( mxIsInt32(IIDptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iid is not an integer")
      ENDIF
!
      IIDptr2=mxGetPr(IIDptr1)
!
      IF( IIDptr2 == 0)THEN
        IIDDST = 0
      ELSE       
        CALL mxCopyPtrToInteger4(IIDptr2,IIDDST,n)
      ENDIF
      CALL mxDestroyArray(IIDptr1)
!
! Assign Itmx values -------------- 
      ITMXptr1=mxGetProperty(PRHS(2),i,'Itmx')
!
      IF( ITMXptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itmx not found")
      ENDIF
!
      IF( mxIsInt32(ITMXptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itmx is not an integer")
      ENDIF
!
      ITMXptr2=mxGetPr(ITMXptr1)
!
      IF( ITMXptr2 == 0)THEN
        CALL mexWarnMsgTxt('Maximum number of ierations for Itmx seems t
     &o be undefined. Setting to defualt value of 8.')
        ITMXDST = 8
      ELSE        
        CALL mxCopyPtrToInteger4(ITMXptr2,ITMXDST,n)
      ENDIF
      CALL mxDestroyArray(ITMXptr1)
!
! Assign Itnw values -------------- 
      ITNWptr1=mxGetProperty(PRHS(2),i,'Itnw')
!
      IF( ITNWptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itnw not found")
      ENDIF
!
      IF( mxIsInt32(ITNWptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itnw is not an integer")
      ENDIF
!
      ITNWptr2=mxGetPr(ITNWptr1)
!
      IF( ITNWptr2 == 0)THEN
        CALL mexWarnMsgTxt('Maximum number of Newton-Chord iterations in
     & Itnw seems to be undefined. Setting to defualt value of 5.')
        ITNWDST = 5
      ELSE       
        CALL mxCopyPtrToInteger4(ITNWptr2,ITNWDST,n)
      ENDIF
      CALL mxDestroyArray(ITNWptr1)
!
! Assign Nwtn values -------------- 
      NWTNptr1=mxGetProperty(PRHS(2),i,'Nwtn')
!
      IF( NWTNptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nwtn not found")
      ENDIF
!
      IF( mxIsInt32(NWTNptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nwtn is not an integer")
      ENDIF
!
      NWTNptr2=mxGetPr(NWTNptr1)
!
      IF( NWTNptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Nwtn seems to be undefined. Setting
     & to defualt value of 3.')
        NWTNDST = 3
      ELSE      
        CALL mxCopyPtrToInteger4(NWTNptr2,NWTNDST,n)
      ENDIF
      CALL mxDestroyArray(NWTNptr1)
!
! Assign Jac values -------------- 
      JACptr1=mxGetProperty(PRHS(2),i,'Jac')
!
      IF( JACptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Jac not found")
      ENDIF
!
      IF( mxIsInt32(JACptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Jac is not an integer")
      ENDIF
!
      JACptr2=mxGetPr(JACptr1)
!
      IF( JACptr2 == 0)THEN      
        JACDST = 0
      ELSE
        CALL mxCopyPtrToInteger4(JACptr2,JACDST,n)
      ENDIF
      CALL mxDestroyArray(JACptr1)
!
! Assign Epsl values -------------- 
      EPSLptr1=mxGetProperty(PRHS(2),i,'Epsl')
!
      IF( EPSLptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Epsl not found")
      ENDIF
!
      IF( mxIsDouble(EPSLptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Epsl is not a double")
      ENDIF
!
      EPSLptr2=mxGetPr(EPSLptr1)
!      
      IF( EPSLptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Epsl seems to be undefined. Setting
     & to defualt value of 1E-6.')
        EPSLDST = 0.000001
      ELSE        
        CALL mxCopyPtrToReal8(EPSLptr2,EPSLDST,n)
      ENDIF
      CALL mxDestroyArray(EPSLptr1)
!
! Assign Epsu values -------------- 
      EPSUptr1=mxGetProperty(PRHS(2),i,'Epsu')
!
      IF( EPSUptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Epsu not found")
      ENDIF
!
      IF( mxIsDouble(EPSUptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Epsu is not a double")
      ENDIF
!
      EPSUptr2=mxGetPr(EPSUptr1)
!
      IF( EPSUptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Epsu seems to be undefined. Setting
     & to defualt value of 1E-6.')
        EPSUDST = 0.000001
      ELSE             
        CALL mxCopyPtrToReal8(EPSUptr2,EPSUDST,n)
      ENDIF
      CALL mxDestroyArray(EPSUptr1)
!
! Assign Epss values -------------- 
      EPSSptr1=mxGetProperty(PRHS(2),i,'Epss')
!
      IF( EPSSptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Epss not found")
      ENDIF
!
      IF( mxIsDouble(EPSSptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Epss is not a double")
      ENDIF
!
      EPSSptr2=mxGetPr(EPSSptr1)
!
      IF( EPSSptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Epss seems to be undefined. Setting
     & to defualt value of 1E-4.')
        EPSSDST = 0.0001
      ELSE             
        CALL mxCopyPtrToReal8(EPSSptr2,EPSSDST,n)
      ENDIF
      CALL mxDestroyArray(EPSSptr1)
!
! Assign Ds values -------------- 
      DSptr1=mxGetProperty(PRHS(2),i,'Ds')
!
      IF( DSptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ds not found")
      ENDIF
!
      IF( mxIsDouble(DSptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ds is not a double")
      ENDIF
!
      DSptr2=mxGetPr(DSptr1)     
!      
      IF( DSptr2 == 0)THEN
        CALL AUTOSTOPWITHERROR("Property Ds seems to be undefined.")
      ENDIF
      CALL mxCopyPtrToReal8(DSptr2,DSDST,n)
      CALL mxDestroyArray(DSptr1)
!
! Assign Dsmin values -------------- 
      DSMINptr1=mxGetProperty(PRHS(2),i,'Dsmin')
!
      IF( DSMINptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Dsmin not found")
      ENDIF
!
      IF( mxIsDouble(DSMINptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Dsmin is not a double")
      ENDIF
!
      DSMINptr2=mxGetPr(DSMINptr1)
!
      IF( DSMINptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Dsmin seems to be undefined. Settin
     &g to 0.01*Ds.')
        DSMINDST = ABS(0.01*DS)
      ELSE       
        CALL mxCopyPtrToReal8(DSMINptr2,DSMINDST,n)
      ENDIF
      CALL mxDestroyArray(DSMINptr1)
!
! Assign Dsmax values -------------- 
      DSMAXptr1=mxGetProperty(PRHS(2),i,'Dsmax')
!
      IF( DSMAXptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Dsmax not found")
      ENDIF
!
      IF( mxIsDouble(DSMAXptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Dsmax is not a double")
      ENDIF
!
      DSMAXptr2=mxGetPr(DSMAXptr1)
!
      IF( DSMAXptr2 == 0)THEN
        CALL mexWarnMsgTxt('Property Dsmax seems to be undefined. Settin
     &g to 100*Ds.')
        DSMAXDST = ABS(100*DS)
      ELSE       
        CALL mxCopyPtrToReal8(DSMAXptr2,DSMAXDST,n)
      ENDIF
      CALL mxDestroyArray(DSMAXptr1)
!
! Assign Iads values -------------- 
      IADSptr1=mxGetProperty(PRHS(2),i,'Iads')
!
      IF( IADSptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iads not found")
      ENDIF
!
      IF( mxIsInt32(IADSptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iads is not an integer")
      ENDIF
!
      IADSptr2=mxGetPr(IADSptr1)
!
      IF( IADSptr2 == 0)THEN
        IADSDST = 1
      ELSE            
        CALL mxCopyPtrToInteger4(IADSptr2,IADSDST,n)
      ENDIF
      CALL mxDestroyArray(IADSptr1)
!
! Assign Nthl values -------------- 
      NTHLptr1=mxGetProperty(PRHS(2),i,'Nthl')
!
      IF( NTHLptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nthl not found")
      ENDIF
!
      IF( mxIsInt32(NTHLptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nthl is not an integer")
      ENDIF
!
      NTHLptr2=mxGetPr(NTHLptr1)
!      
      IF( NTHLptr2 == 0 ) THEN
         NTHLDST=0
      ELSE
        CALL mxCopyPtrToInteger4(NTHLptr2,NTHLDST,n)
      ENDIF
      CALL mxDestroyArray(NTHLptr1)
!     
! Assign Ithl values -------------- 
      IF( NTHLDST > 0 ) THEN
      ALLOCATE(ITHLDST(NTHLDST))
      ITHLptr1=mxGetProperty(PRHS(2),i,'Ithl')
!
      IF( ITHLptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ithl not found")
      ENDIF
!
      IF( mxIsInt32(ITHLptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ithl is not an integer")
      ENDIF
!
      ITHLptr2=mxGetPr(ITHLptr1)
      CALL mxCopyPtrToInteger4(ITHLptr2,ITHLDST,NTHLDST)
      CALL mxDestroyArray(ITHLptr1)
      ENDIF
!
! Assign Vthl values -------------- 
      IF( NTHLDST > 0 ) THEN
      ALLOCATE(VTHLDST(NTHLDST))
      VTHLptr1=mxGetProperty(PRHS(2),i,'Vthl')
!
      IF( VTHLptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Vthl not found")
      ENDIF
!
      IF( mxIsDouble(VTHLptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Vthl is not a double")
      ENDIF
!
      VTHLptr2=mxGetPr(VTHLptr1)
      CALL mxCopyPtrToReal8(VTHLptr2,VTHLDST,NTHLDST)
      CALL mxDestroyArray(VTHLptr1)
      ENDIF
!
! Assign Nthu values -------------- 
      NTHUptr1=mxGetProperty(PRHS(2),i,'Nthu')
!
      IF( NTHUptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nthu not found")
      ENDIF
!
      IF( mxIsInt32(NTHUptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nthu is not an integer")
      ENDIF
!
      NTHUptr2=mxGetPr(NTHUptr1)
!
      IF( NTHUptr2 == 0 ) THEN
         NTHUDST=0
      ELSE      
        CALL mxCopyPtrToInteger4(NTHUptr2,NTHUDST,n)
      ENDIF
      CALL mxDestroyArray(NTHUptr1)
!
! Assign Ithu values -------------- 
      IF( NTHUDST > 0 ) THEN
      ALLOCATE(ITHUDST(NTHUDST))
      ITHUptr1=mxGetProperty(PRHS(2),i,'Ithu')
!
      IF( ITHUptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ithu not found")
      ENDIF
!
      IF( mxIsInt32(ITHUptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ithu is not an integer")
      ENDIF
!
      ITHUptr2=mxGetPr(ITHUptr1)
      CALL mxCopyPtrToInteger4(ITHUptr2,ITHUDST,NTHUDST)
      CALL mxDestroyArray(ITHUptr1)
      ENDIF
!
! Assign Vthu values -------------- 
      IF( NTHUDST > 0 ) THEN
      ALLOCATE(VTHUDST(NTHUDST))
      VTHUptr1=mxGetProperty(PRHS(2),i,'Vthu')
!
      IF( VTHUptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Vthu not found")
      ENDIF
!
      IF( mxIsDouble(VTHUptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Vthu is not a double")
      ENDIF
!
      VTHUptr2=mxGetPr(VTHUptr1)
      CALL mxCopyPtrToReal8(VTHUptr2,VTHUDST,NTHUDST)
      CALL mxDestroyArray(VTHUptr1)
      ENDIF
!
! Assign Nuzr values -------------- 
      NUZRptr1=mxGetProperty(PRHS(2),i,'Nuzr')
!
      IF( NUZRptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nuzr not found")
      ENDIF
!
      IF( mxIsInt32(NUZRptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nuzr is not an integer")
      ENDIF
!
      NUZRptr2=mxGetPr(NUZRptr1)
      
      IF( NUZRptr2 == 0 ) THEN
         NUZRDST=0
      ELSE      
        CALL mxCopyPtrToInteger4(NUZRptr2,NUZRDST,n)
      ENDIF
      CALL mxDestroyArray(NUZRptr1)
!
! Assign Iuz values -------------- 
      IF( NUZRDST > 0 ) THEN
      ALLOCATE(IUZDST(NUZRDST))
      IUZptr1=mxGetProperty(PRHS(2),i,'Iuz')
!
      IF( IUZptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iuz not found")
      ENDIF
!
      IF( mxIsInt32(IUZptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Iuz is not an integer")
      ENDIF
!
      IUZptr2=mxGetPr(IUZptr1)
      CALL mxCopyPtrToInteger4(IUZptr2,IUZDST,NUZRDST)
      CALL mxDestroyArray(IUZptr1)
      ENDIF
!
! Assign Vuz values -------------- 
      IF( NUZRDST > 0 ) THEN
      ALLOCATE(VUZDST(NUZRDST))
      VUZptr1=mxGetProperty(PRHS(2),i,'Vuz')
!
      IF( VUZptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Vuz not found")
      ENDIF
!
      IF( mxIsDouble(VUZptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Vuz is not a double")
      ENDIF
!
      VUZptr2=mxGetPr(VUZptr1)
      CALL mxCopyPtrToReal8(VUZptr2,VUZDST,NUZRDST)
      CALL mxDestroyArray(VUZptr1)   
      ENDIF   
!
! INITIAL CONDITIONS PASSED FROM AUTO OBJECT ----------------
!
! Assign Par0 values -------------- 
      PARDST0ptr1=mxGetProperty(PRHS(1),i,'Par0')
!
      IF( PARDST0ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Par0 not found")
      ENDIF
!
      IF( mxIsDouble(PARDST0ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Par0 is not a double")
      ENDIF
!
      PARDST0ptr2=mxGetPr(PARDST0ptr1)
      m=mxGetNumberOfElements(PARDST0ptr1)
      ALLOCATE(PARDST0(m))
      CALL mxCopyPtrToReal8(PARDST0ptr2,PARDST0,m)
      CALL mxDestroyArray(PARDST0ptr1)    
!      
! Assign U0 values -------------- 
      UDST0ptr1=mxGetProperty(PRHS(1),i,'U0')
!
      IF( UDST0ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property U0 not found")
      ENDIF
!
      IF( mxIsDouble(UDST0ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property U0 is not a double")
      ENDIF
!
      UDST0ptr2=mxGetPr(UDST0ptr1)
      m=mxGetNumberOfElements(UDST0ptr1)
      ALLOCATE(UDST0(m))
      CALL mxCopyPtrToReal8(UDST0ptr2,UDST0,m)
      CALL mxDestroyArray(UDST0ptr1)
      
      IF( m /= NDIMDST .AND. IPSDST /= 2) THEN
        CALL AUTOSTOPWITHERROR("Mismatch between Ndim and size of U0 def
     &ined in autoconstants object")       
      ENDIF
!
! Assign Out0 values -------------- 
      OUTDST0ptr1=mxGetProperty(PRHS(1),i,'Out0')
!
      IF( OUTDST0ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Out0 not found")
      ENDIF
!
      IF( mxIsDouble(OUTDST0ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Out0 is not a double")
      ENDIF
!
      OUTDST0ptr2=mxGetPr(OUTDST0ptr1)
      m=mxGetNumberOfElements(OUTDST0ptr1)
      ALLOCATE(OUTDST0(m))
      CALL mxCopyPtrToReal8(OUTDST0ptr2,OUTDST0,m)
      CALL mxDestroyArray(OUTDST0ptr1)    
!  
! Allocate output variables that will be sent back to F7
      CALL ALLOCATEF7()
!
!
! INITIALISE F8 OUTPUTS ------------------------------------------------
!
!
! Assign Ibr values --------------
      IBRF8ptr1=mxGetProperty(PRHS(4),i,'Ibr')
!
      IF( IBRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ibr in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(IBRF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ibr in object F8 is not an inte
     &ger")
      ENDIF
!
      IBRF8ptr2=mxGetPr(IBRF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(IBRF8ptr1)
        D1ptr=mxGetDimensions(IBRF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(IBRF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(IBRF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(IBRF8ptr2,IBRF8,m)
      CALL mxDestroyArray(IBRF8ptr1)
!
! Assign Mtot values --------------
      MTOTF8ptr1=mxGetProperty(PRHS(4),i,'Mtot')
!
      IF( MTOTF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Mtot in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(MTOTF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Mtot in object F8 is not an int
     &eger")
      ENDIF
!
      MTOTF8ptr2=mxGetPr(MTOTF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(MTOTF8ptr1)
        D1ptr=mxGetDimensions(MTOTF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(MTOTF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(MTOTF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(MTOTF8ptr2,MTOTF8,m)
      CALL mxDestroyArray(MTOTF8ptr1)
!
! Assign Itp values --------------
      ITPF8ptr1=mxGetProperty(PRHS(4),i,'Itp')
!
      IF( ITPF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itp in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(ITPF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itp in object F8 is not an inte
     &ger")
      ENDIF
!
      ITPF8ptr2=mxGetPr(ITPF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(ITPF8ptr1)
        D1ptr=mxGetDimensions(ITPF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(ITPF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(ITPF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(ITPF8ptr2,ITPF8,m)
      CALL mxDestroyArray(ITPF8ptr1)
!
! Assign Lab values --------------
      LABF8ptr1=mxGetProperty(PRHS(4),i,'Lab')
!
      IF( LABF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Lab in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(LABF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Lab in object F8 is not an inte
     &ger")
      ENDIF
!
      LABF8ptr2=mxGetPr(LABF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(LABF8ptr1)
        D1ptr=mxGetDimensions(LABF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(LABF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(LABF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(LABF8ptr2,LABF8,m)
      CALL mxDestroyArray(LABF8ptr1)
!
! Assign Nfpr values --------------
      NFPRF8ptr1=mxGetProperty(PRHS(4),i,'Nfpr')
!
      IF( NFPRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nfpr in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NFPRF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nfpr in object F8 is not an int
     &eger")
      ENDIF
!
      NFPRF8ptr2=mxGetPr(NFPRF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NFPRF8ptr1)
        D1ptr=mxGetDimensions(NFPRF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NFPRF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NFPRF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NFPRF8ptr2,NFPRF8,m)
      CALL mxDestroyArray(NFPRF8ptr1)
!
! Assign Isw values --------------
      ISWF8ptr1=mxGetProperty(PRHS(4),i,'Isw')
!
      IF( ISWF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isw in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(ISWF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isw in object F8 is not an inte
     &ger")
      ENDIF
!
      ISWF8ptr2=mxGetPr(ISWF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(ISWF8ptr1)
        D1ptr=mxGetDimensions(ISWF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(ISWF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(ISWF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(ISWF8ptr2,ISWF8,m)
      CALL mxDestroyArray(ISWF8ptr1)
!
! Assign Ntpl values --------------
      NTPLF8ptr1=mxGetProperty(PRHS(4),i,'Ntpl')
!
      IF( NTPLF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntpl in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NTPLF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntpl in object F8 is not an int
     &eger")
      ENDIF
!
      NTPLF8ptr2=mxGetPr(NTPLF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NTPLF8ptr1)
        D1ptr=mxGetDimensions(NTPLF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NTPLF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NTPLF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NTPLF8ptr2,NTPLF8,m)
      CALL mxDestroyArray(NTPLF8ptr1)
!
! Assign Nar values --------------
      NARF8ptr1=mxGetProperty(PRHS(4),i,'Nar')
!
      IF( NARF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nar in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NARF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nar in object F8 is not an inte
     &ger")
      ENDIF
!
      NARF8ptr2=mxGetPr(NARF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NARF8ptr1)
        D1ptr=mxGetDimensions(NARF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NARF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NARF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NARF8ptr2,NARF8,m)
      CALL mxDestroyArray(NARF8ptr1)
!
! Assign Nrowpr values --------------
      NROWPRF8ptr1=mxGetProperty(PRHS(4),i,'Nrowpr')
!
      IF( NROWPRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nrowpr in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NROWPRF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nrowpr in object F8 is not an i
     &nteger")
      ENDIF
!
      NROWPRF8ptr2=mxGetPr(NROWPRF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NROWPRF8ptr1)
        D1ptr=mxGetDimensions(NROWPRF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NROWPRF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NROWPRF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NROWPRF8ptr2,NROWPRF8,m)
      CALL mxDestroyArray(NROWPRF8ptr1)
!
! Assign Ntst values --------------
      NTSTF8ptr1=mxGetProperty(PRHS(4),i,'Ntst')
!
      IF( NTSTF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntst in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NTSTF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntst in object F8 is not an int
     &eger")
      ENDIF
!
      NTSTF8ptr2=mxGetPr(NTSTF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NTSTF8ptr1)
        D1ptr=mxGetDimensions(NTSTF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NTSTF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NTSTF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NTSTF8ptr2,NTSTF8,m)
      CALL mxDestroyArray(NTSTF8ptr1)
!
! Assign Ncol values --------------
      NCOLF8ptr1=mxGetProperty(PRHS(4),i,'Ncol')
!
      IF( NCOLF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ncol in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NCOLF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ncol in object F8 is not an int
     &eger")
      ENDIF
!
      NCOLF8ptr2=mxGetPr(NCOLF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NCOLF8ptr1)
        D1ptr=mxGetDimensions(NCOLF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NCOLF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NCOLF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NCOLF8ptr2,NCOLF8,m)
      CALL mxDestroyArray(NCOLF8ptr1)
!
! Assign Nparx values --------------
      NPARXF8ptr1=mxGetProperty(PRHS(4),i,'Nparx')
!
      IF( NPARXF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nparx in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(NPARXF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nparx in object F8 is not an in
     &teger")
      ENDIF
!
      NPARXF8ptr2=mxGetPr(NPARXF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(NPARXF8ptr1)
        D1ptr=mxGetDimensions(NPARXF8ptr1)
        CALL mxCopyPtrToInteger4(D1ptr,D1,1)
        ALLOCATE(NPARXF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(NPARXF8(0))
      ENDIF
      CALL mxCopyPtrToInteger4(NPARXF8ptr2,NPARXF8,m)
      CALL mxDestroyArray(NPARXF8ptr1)
!
! Assign Ifpr values --------------
      IFPRF8ptr1=mxGetProperty(PRHS(4),i,'Ifpr')
!
      IF( IFPRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ifpr in object F8 not found")
      ENDIF
!
      IF( mxIsInt32(IFPRF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ifpr in object F8 is not an int
     &eger")
      ENDIF
!
      IFPRF8ptr2=mxGetPr(IFPRF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(IFPRF8ptr1)
        D2ptr=mxGetDimensions(IFPRF8ptr1)
        CALL mxCopyPtrToInteger4(D2ptr,D2,2)
        ALLOCATE(IFPRF8(D2(1),D2(2)))
      ELSE
        m=0
        ALLOCATE(IFPRF8(0,0))
      ENDIF
      CALL mxCopyPtrToInteger4(IFPRF8ptr2,IFPRF8,m)
      CALL mxDestroyArray(IFPRF8ptr1)
!
! Assign T values --------------
      TF8ptr1=mxGetProperty(PRHS(4),i,'T')
!
      IF( TF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property T in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(TF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property T in object F8 is not a double"
     &)
      ENDIF
!
      TF8ptr2=mxGetPr(TF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(TF8ptr1)
        D1ptr=mxGetDimensions(TF8ptr1)
        CALL mxCopyPtrToReal8(D1ptr,D1,1)
        ALLOCATE(TF8(D1(1)))
      ELSE
        m=0
        ALLOCATE(TF8(0))
      ENDIF
      CALL mxCopyPtrToReal8(TF8ptr2,TF8,m)
      CALL mxDestroyArray(TF8ptr1)
!
! Assign Tm values --------------
      TMF8ptr1=mxGetProperty(PRHS(4),i,'Tm')
!
      IF( TMF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Tm in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(TMF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Tm in object F8 is not a double
     &")
      ENDIF
!
      TMF8ptr2=mxGetPr(TMF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(TMF8ptr1)
        D2ptr=mxGetDimensions(TMF8ptr1)
        CALL mxCopyPtrToReal8(D2ptr,D2,2)
        ALLOCATE(TMF8(D2(1),D2(2)))
      ELSE
        m=0
        ALLOCATE(TMF8(0,0))
      ENDIF
      CALL mxCopyPtrToReal8(TMF8ptr2,TMF8,m)
      CALL mxDestroyArray(TMF8ptr1)
!
! Assign Par values --------------
      PARF8ptr1=mxGetProperty(PRHS(4),i,'Par')
!
      IF( PARF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Par in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(PARF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Par in object F8 is not a doubl
     &e")
      ENDIF
!
      PARF8ptr2=mxGetPr(PARF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(PARF8ptr1)
        D2ptr=mxGetDimensions(PARF8ptr1)
        CALL mxCopyPtrToReal8(D2ptr,D2,2)
        ALLOCATE(PARF8(D2(1),D2(2)))
      ELSE
        m=0
        ALLOCATE(PARF8(0,0))
      ENDIF
      CALL mxCopyPtrToReal8(PARF8ptr2,PARF8,m)
      CALL mxDestroyArray(PARF8ptr1)
!
! Assign Rldot values --------------
      RLDOTF8ptr1=mxGetProperty(PRHS(4),i,'Rldot')
!
      IF( RLDOTF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rldot in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(RLDOTF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rldot in object F8 is not a dou
     &ble")
      ENDIF
!
      RLDOTF8ptr2=mxGetPr(RLDOTF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(RLDOTF8ptr1)
        D2ptr=mxGetDimensions(RLDOTF8ptr1)
        CALL mxCopyPtrToReal8(D2ptr,D2,2)
        ALLOCATE(RLDOTF8(D2(1),D2(2)))
      ELSE
        m=0
        ALLOCATE(RLDOTF8(0,0))
      ENDIF
      CALL mxCopyPtrToReal8(RLDOTF8ptr2,RLDOTF8,m)
      CALL mxDestroyArray(RLDOTF8ptr1)
!
! Assign U values --------------
      UF8ptr1=mxGetProperty(PRHS(4),i,'U')
!
      IF( UF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property U in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(UF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property U in object F8 is not a double"
     &)
      ENDIF
!
      UF8ptr2=mxGetPr(UF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(UF8ptr1)
        D2ptr=mxGetDimensions(UF8ptr1)
        CALL mxCopyPtrToReal8(D2ptr,D2,2)
        ALLOCATE(UF8(D2(1),D2(2)))
      ELSE
        m=0
        ALLOCATE(UF8(0,0))
      ENDIF
      CALL mxCopyPtrToReal8(UF8ptr2,UF8,m)
      CALL mxDestroyArray(UF8ptr1)
!
! Assign Ups values --------------
      UPSF8ptr1=mxGetProperty(PRHS(4),i,'Ups')
!
      IF( UPSF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ups in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(UPSF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ups in object F8 is not a doubl
     &e")
      ENDIF
!
      UPSF8ptr2=mxGetPr(UPSF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(UPSF8ptr1)
        numdim=mxGetNumberOfDimensions(UPSF8ptr1)
        D3ptr=mxGetDimensions(UPSF8ptr1)
        CALL mxCopyPtrToReal8(D3ptr,D3,3)
        IF(numdim==0)THEN
          ALLOCATE(UPSF8(0,0,0))
        ELSEIF(numdim==1)THEN
          ALLOCATE(UPSF8(D3(1),1,1))
        ELSEIF(numdim==2)THEN
          ALLOCATE(UPSF8(D3(1),D3(2),1))
        ELSEIF(numdim==3)THEN
          ALLOCATE(UPSF8(D3(1),D3(2),D3(3)))
        ENDIF
      ELSE
        m=0
        ALLOCATE(UPSF8(0,0,0))
      ENDIF
      CALL mxCopyPtrToReal8(UPSF8ptr2,UPSF8,m)
      CALL mxDestroyArray(UPSF8ptr1)
!
! Assign Udotps values --------------
      UDOTPSF8ptr1=mxGetProperty(PRHS(4),i,'Udotps')
!
      IF( UDOTPSF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Udotps in object F8 not found")
      ENDIF
!
      IF( mxIsDouble(UDOTPSF8ptr1) == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Udotps in object F8 is not a do
     &uble")
      ENDIF
!
      UDOTPSF8ptr2=mxGetPr(UDOTPSF8ptr1)
      IF( IRSDST > 0) THEN
        m=mxGetNumberOfElements(UDOTPSF8ptr1)
        numdim=mxGetNumberOfDimensions(UDOTPSF8ptr1)
        D3ptr=mxGetDimensions(UDOTPSF8ptr1)
        CALL mxCopyPtrToReal8(D3ptr,D3,3)
        IF(numdim==0)THEN
          ALLOCATE(UDOTPSF8(0,0,0))
        ELSEIF(numdim==1)THEN
          ALLOCATE(UDOTPSF8(D3(1),1,1))
        ELSEIF(numdim==2)THEN
          ALLOCATE(UDOTPSF8(D3(1),D3(2),1))
        ELSEIF(numdim==3)THEN
          ALLOCATE(UDOTPSF8(D3(1),D3(2),D3(3)))
        ENDIF
      ELSE
        m=0
        ALLOCATE(UDOTPSF8(0,0,0))
      ENDIF
      CALL mxCopyPtrToReal8(UDOTPSF8ptr2,UDOTPSF8,m)
      CALL mxDestroyArray(UDOTPSF8ptr1)
!
!
      RETURN      
      END
!
!--------------------------------------------------------------------------
      SUBROUTINE COPYAUTOTODSTOBJECTS(PLHS,PRHS)
!
      USE AUTO_CONSTANTS 
!      USE MatlabAPImex
!      USE MatlabAPImx   
!            
      IMPLICIT NONE
      mwPointer PLHS(*),PRHS(*)
      mwPointer mxGetPr,mxGetProperty,mxCreateNumericArray
      mwSize I,J,K
      mwSize, DIMENSION(1)::D1
      mwSize, DIMENSION(2)::D2
      mwSize, DIMENSION(3)::D3
      real(8), pointer :: Apx1(:)
      real(8), pointer :: Apx2(:,:)
      real(8), pointer :: Apx3(:,:,:)
      common /MatlabAPI_COMA1/ Apx1
      common /MatlabAPI_COMA2/ Apx2
      common /MatlabAPI_COMA3/ Apx3
!      
!      mwPointer mxGetProperty, mxGetPr, mxRealloc, mxCreateDoubleMatrix
       mwSize mxClassIDFromClassName
!      mwIndex :: i = 1
!      mwSize m,n, mxGetNumberOfElements
      mwSize M,N,O
!
! F7 Outputs
      mwPointer IBRF7ptr1, IBRF7ptr2
      mwPointer MTOTF7ptr1, MTOTF7ptr2
      mwPointer ITPF7ptr1, ITPF7ptr2
      mwPointer LABF7ptr1, LABF7ptr2
      mwPointer PARF7ptr1, PARF7ptr2
      mwPointer VAXISF7ptr1, VAXISF7ptr2
      mwPointer UF7ptr1, UF7ptr2
      mwPointer OUTF7ptr1, OUTF7ptr2
      REAL*8, POINTER::IBRF7MWS(:)
      REAL*8, POINTER::MTOTF7MWS(:)
      REAL*8, POINTER::ITPF7MWS(:)
      REAL*8, POINTER::LABF7MWS(:)
      REAL*8, POINTER::PARF7MWS(:,:)
      REAL*8, POINTER::VAXISF7MWS(:)
      REAL*8, POINTER::UF7MWS(:,:)
      REAL*8, POINTER::OUTF7MWS(:,:)
!
! F8 Outputs
      mwPointer IBRF8ptr1, IBRF8ptr2
      mwPointer MTOTF8ptr1, MTOTF8ptr2
      mwPointer ITPF8ptr1, ITPF8ptr2
      mwPointer LABF8ptr1, LABF8ptr2
      mwPointer NFPRF8ptr1, NFPRF8ptr2
      mwPointer ISWF8ptr1, ISWF8ptr2
      mwPointer NTPLF8ptr1, NTPLF8ptr2
      mwPointer NARF8ptr1, NARF8ptr2
      mwPointer NROWPRF8ptr1, NROWPRF8ptr2
      mwPointer NTSTF8ptr1, NTSTF8ptr2
      mwPointer NCOLF8ptr1, NCOLF8ptr2
      mwPointer NPARXF8ptr1, NPARXF8ptr2
      mwPointer IFPRF8ptr1, IFPRF8ptr2
      mwPointer TF8ptr1, TF8ptr2
      mwPointer TMF8ptr1, TMF8ptr2
      mwPointer PARF8ptr1, PARF8ptr2
      mwPointer RLDOTF8ptr1, RLDOTF8ptr2
      mwPointer UF8ptr1, UF8ptr2
      mwPointer UPSF8ptr1, UPSF8ptr2
      mwPointer UDOTPSF8ptr1, UDOTPSF8ptr2
      REAL*8, POINTER::IBRF8MWS(:)
      REAL*8, POINTER::MTOTF8MWS(:)
      REAL*8, POINTER::ITPF8MWS(:)
      REAL*8, POINTER::LABF8MWS(:)
      REAL*8, POINTER::NFPRF8MWS(:)
      REAL*8, POINTER::ISWF8MWS(:)
      REAL*8, POINTER::NTPLF8MWS(:)
      REAL*8, POINTER::NARF8MWS(:)
      REAL*8, POINTER::NROWPRF8MWS(:)
      REAL*8, POINTER::NTSTF8MWS(:)
      REAL*8, POINTER::NCOLF8MWS(:)
      REAL*8, POINTER::NPARXF8MWS(:)
      REAL*8, POINTER::IFPRF8MWS(:,:)
      REAL*8, POINTER::TF8MWS(:)
      REAL*8, POINTER::TMF8MWS(:,:)
      REAL*8, POINTER::PARF8MWS(:,:)
      REAL*8, POINTER::RLDOTF8MWS(:,:)
      REAL*8, POINTER::UF8MWS(:,:)
      REAL*8, POINTER::UPSF8MWS(:,:,:)
      REAL*8, POINTER::UDOTPSF8MWS(:,:,:)
!
! F7 Outputs ------------------------------------------------------------
!
! Assign Ibr values --------------
      IBRF7ptr1=mxGetProperty(PLHS(1),1,'Ibr')
!
      IF( IBRF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ibr not found")
      ENDIF
!
      M=SIZE(IBRF7)
      D1=M
      IBRF7ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(IBRF7ptr2)), 1, D1 )
      IBRF7MWS => Apx1
      IF( .NOT.ASSOCIATED(IBRF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ibr pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        IBRF7MWS(I)=IBRF7(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'Ibr',IBRF7ptr2)
      CALL mxDestroyArray(IBRF7ptr2)
      CALL mxDestroyArray(IBRF7ptr1)
!
! Assign Mtot values --------------
      MTOTF7ptr1=mxGetProperty(PLHS(1),1,'Mtot')
!
      IF( MTOTF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Mtot not found")
      ENDIF
!
      M=SIZE(MTOTF7)
      D1=M
      MTOTF7ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(MTOTF7ptr2)), 1, D1 )
      MTOTF7MWS => Apx1
      IF( .NOT.ASSOCIATED(MTOTF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Mtot pointer 
     &data")
      ENDIF
!
      DO I=1,M
        MTOTF7MWS(I)=MTOTF7(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'Mtot',MTOTF7ptr2)
      CALL mxDestroyArray(MTOTF7ptr2)
      CALL mxDestroyArray(MTOTF7ptr1)
!
! Assign Itp values --------------
      ITPF7ptr1=mxGetProperty(PLHS(1),1,'Itp')
!
      IF( ITPF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itp not found")
      ENDIF
!
      M=SIZE(ITPF7)
      D1=M
      ITPF7ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(ITPF7ptr2)), 1, D1 )
      ITPF7MWS => Apx1
      IF( .NOT.ASSOCIATED(ITPF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Itp pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        ITPF7MWS(I)=ITPF7(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'Itp',ITPF7ptr2)
      CALL mxDestroyArray(ITPF7ptr2)
      CALL mxDestroyArray(ITPF7ptr1)
!
! Assign Lab values --------------
      LABF7ptr1=mxGetProperty(PLHS(1),1,'Lab')
!
      IF( LABF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Lab not found")
      ENDIF
!
      M=SIZE(LABF7)
      D1=M
      LABF7ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(LABF7ptr2)), 1, D1 )
      LABF7MWS => Apx1
      IF( .NOT.ASSOCIATED(LABF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Lab pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        LABF7MWS(I)=LABF7(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'Lab',LABF7ptr2)
      CALL mxDestroyArray(LABF7ptr2)
      CALL mxDestroyArray(LABF7ptr1)
!
! Assign Par values --------------
      PARF7ptr1=mxGetProperty(PLHS(1),1,'Par')
!
      IF( PARF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Par not found")
      ENDIF
!
      M=SIZE(PARF7,1)
      N=SIZE(PARF7,2)
      D2(1)=M
      D2(2)=N
      PARF7ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(PARF7ptr2)), 1, D2 )
      PARF7MWS => Apx2
      IF( .NOT.ASSOCIATED(PARF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Par pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          PARF7MWS(I,J)=PARF7(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'Par',PARF7ptr2)
      CALL mxDestroyArray(PARF7ptr2)
      CALL mxDestroyArray(PARF7ptr1)
!
! Assign L2norm values --------------
      VAXISF7ptr1=mxGetProperty(PLHS(1),1,'L2norm')
!
      IF( VAXISF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property L2norm not found")
      ENDIF
!
      M=SIZE(VAXISF7)
      D1=M
      VAXISF7ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('do
     &uble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(VAXISF7ptr2)), 1, D1 )
      VAXISF7MWS => Apx1
      IF( .NOT.ASSOCIATED(VAXISF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to L2norm pointe
     &r data")
      ENDIF
!
      DO I=1,M
        VAXISF7MWS(I)=VAXISF7(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'L2norm',VAXISF7ptr2)
      CALL mxDestroyArray(VAXISF7ptr2)
      CALL mxDestroyArray(VAXISF7ptr1)
!
! Assign U values --------------
      UF7ptr1=mxGetProperty(PLHS(1),1,'U')
!
      IF( UF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property U not found")
      ENDIF
!
      M=SIZE(UF7,1)
      N=SIZE(UF7,2)
      D2(1)=M
      D2(2)=N
      UF7ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('double
     &'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(UF7ptr2)), 1, D2 )
      UF7MWS => Apx2
      IF( .NOT.ASSOCIATED(UF7MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to U pointer dat
     &a")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          UF7MWS(I,J)=UF7(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(1),1,'U',UF7ptr2)
      CALL mxDestroyArray(UF7ptr2)
      CALL mxDestroyArray(UF7ptr1)
!
! Assign Out values --------------
      OUTF7ptr1=mxGetProperty(PLHS(1),1,'Out')
!
      IF( OUTF7ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Out not found")
      ENDIF
!
      M=SIZE(OUTF7,1)
      N=SIZE(OUTF7,2)
      D2(1)=M
      D2(2)=N
      IF( N > 0 ) THEN
         OUTF7ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('d
     &ouble'),0)
         CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(OUTF7ptr2)), 1, D2 )
         OUTF7MWS => Apx2
         IF( .NOT.ASSOCIATED(OUTF7MWS) ) THEN
            CALL AUTOSTOPWITHERROR("Internal error pointing to Out point
     &er data")
         ENDIF
!
         DO I=1,M
            DO J=1,N
               OUTF7MWS(I,J)=OUTF7(I,J)
            ENDDO
         ENDDO
!
         CALL mxSetProperty(PLHS(1),1,'Out',OUTF7ptr2)
         CALL mxDestroyArray(OUTF7ptr2)
      ENDIF
      CALL mxDestroyArray(OUTF7ptr1)
!
!
! F8 Outputs -----------------------------------------------------------
!
! Assign Ibr values --------------
      IBRF8ptr1=mxGetProperty(PLHS(2),1,'Ibr')
!
      IF( IBRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ibr not found")
      ENDIF
!
      M=SIZE(IBRF8)
      D1=M
      IBRF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(IBRF8ptr2)), 1, D1 )
      IBRF8MWS => Apx1
      IF( .NOT.ASSOCIATED(IBRF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ibr pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        IBRF8MWS(I)=IBRF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Ibr',IBRF8ptr2)
      CALL mxDestroyArray(IBRF8ptr2)
      CALL mxDestroyArray(IBRF8ptr1)
!
! Assign Mtot values --------------
      MTOTF8ptr1=mxGetProperty(PLHS(2),1,'Mtot')
!
      IF( MTOTF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Mtot not found")
      ENDIF
!
      M=SIZE(MTOTF8)
      D1=M
      MTOTF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(MTOTF8ptr2)), 1, D1 )
      MTOTF8MWS => Apx1
      IF( .NOT.ASSOCIATED(MTOTF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Mtot pointer 
     &data")
      ENDIF
!
      DO I=1,M
        MTOTF8MWS(I)=MTOTF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Mtot',MTOTF8ptr2)
      CALL mxDestroyArray(MTOTF8ptr2)
      CALL mxDestroyArray(MTOTF8ptr1)
!
! Assign Itp values --------------
      ITPF8ptr1=mxGetProperty(PLHS(2),1,'Itp')
!
      IF( ITPF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Itp not found")
      ENDIF
!
      M=SIZE(ITPF8)
      D1=M
      ITPF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(ITPF8ptr2)), 1, D1 )
      ITPF8MWS => Apx1
      IF( .NOT.ASSOCIATED(ITPF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Itp pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        ITPF8MWS(I)=ITPF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Itp',ITPF8ptr2)
      CALL mxDestroyArray(ITPF8ptr2)
      CALL mxDestroyArray(ITPF8ptr1)
!
! Assign Lab values --------------
      LABF8ptr1=mxGetProperty(PLHS(2),1,'Lab')
!
      IF( LABF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Lab not found")
      ENDIF
!
      M=SIZE(LABF8)
      D1=M
      LABF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(LABF8ptr2)), 1, D1 )
      LABF8MWS => Apx1
      IF( .NOT.ASSOCIATED(LABF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Lab pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        LABF8MWS(I)=LABF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Lab',LABF8ptr2)
      CALL mxDestroyArray(LABF8ptr2)
      CALL mxDestroyArray(LABF8ptr1)
!
! Assign Nfpr values --------------
      NFPRF8ptr1=mxGetProperty(PLHS(2),1,'Nfpr')
!
      IF( NFPRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nfpr not found")
      ENDIF
!
      M=SIZE(NFPRF8)
      D1=M
      NFPRF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NFPRF8ptr2)), 1, D1 )
      NFPRF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NFPRF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Nfpr pointer 
     &data")
      ENDIF
!
      DO I=1,M
        NFPRF8MWS(I)=NFPRF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Nfpr',NFPRF8ptr2)
      CALL mxDestroyArray(NFPRF8ptr2)
      CALL mxDestroyArray(NFPRF8ptr1)
!
! Assign Isw values --------------
      ISWF8ptr1=mxGetProperty(PLHS(2),1,'Isw')
!
      IF( ISWF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Isw not found")
      ENDIF
!
      M=SIZE(ISWF8)
      D1=M
      ISWF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(ISWF8ptr2)), 1, D1 )
      ISWF8MWS => Apx1
      IF( .NOT.ASSOCIATED(ISWF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Isw pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        ISWF8MWS(I)=ISWF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Isw',ISWF8ptr2)
      CALL mxDestroyArray(ISWF8ptr2)
      CALL mxDestroyArray(ISWF8ptr1)
!
! Assign Ntpl values --------------
      NTPLF8ptr1=mxGetProperty(PLHS(2),1,'Ntpl')
!
      IF( NTPLF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntpl not found")
      ENDIF
!
      M=SIZE(NTPLF8)
      D1=M
      NTPLF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NTPLF8ptr2)), 1, D1 )
      NTPLF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NTPLF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ntpl pointer 
     &data")
      ENDIF
!
      DO I=1,M
        NTPLF8MWS(I)=NTPLF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Ntpl',NTPLF8ptr2)
      CALL mxDestroyArray(NTPLF8ptr2)
      CALL mxDestroyArray(NTPLF8ptr1)
!
! Assign Nar values --------------
      NARF8ptr1=mxGetProperty(PLHS(2),1,'Nar')
!
      IF( NARF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nar not found")
      ENDIF
!
      M=SIZE(NARF8)
      D1=M
      NARF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NARF8ptr2)), 1, D1 )
      NARF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NARF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Nar pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        NARF8MWS(I)=NARF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Nar',NARF8ptr2)
      CALL mxDestroyArray(NARF8ptr2)
      CALL mxDestroyArray(NARF8ptr1)
!
! Assign Nrowpr values --------------
      NROWPRF8ptr1=mxGetProperty(PLHS(2),1,'Nrowpr')
!
      IF( NROWPRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nrowpr not found")
      ENDIF
!
      M=SIZE(NROWPRF8)
      D1=M
      NROWPRF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('d
     &ouble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NROWPRF8ptr2)), 1, D1 )
      NROWPRF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NROWPRF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Nrowpr pointe
     &r data")
      ENDIF
!
      DO I=1,M
        NROWPRF8MWS(I)=NROWPRF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Nrowpr',NROWPRF8ptr2)
      CALL mxDestroyArray(NROWPRF8ptr2)
      CALL mxDestroyArray(NROWPRF8ptr1)
!
! Assign Ntst values --------------
      NTSTF8ptr1=mxGetProperty(PLHS(2),1,'Ntst')
!
      IF( NTSTF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ntst not found")
      ENDIF
!
      M=SIZE(NTSTF8)
      D1=M
      NTSTF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NTSTF8ptr2)), 1, D1 )
      NTSTF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NTSTF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ntst pointer 
     &data")
      ENDIF
!
      DO I=1,M
        NTSTF8MWS(I)=NTSTF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Ntst',NTSTF8ptr2)
      CALL mxDestroyArray(NTSTF8ptr2)
      CALL mxDestroyArray(NTSTF8ptr1)
!
! Assign Ncol values --------------
      NCOLF8ptr1=mxGetProperty(PLHS(2),1,'Ncol')
!
      IF( NCOLF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ncol not found")
      ENDIF
!
      M=SIZE(NCOLF8)
      D1=M
      NCOLF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NCOLF8ptr2)), 1, D1 )
      NCOLF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NCOLF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ncol pointer 
     &data")
      ENDIF
!
      DO I=1,M
        NCOLF8MWS(I)=NCOLF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Ncol',NCOLF8ptr2)
      CALL mxDestroyArray(NCOLF8ptr2)
      CALL mxDestroyArray(NCOLF8ptr1)
!
! Assign Nparx values --------------
      NPARXF8ptr1=mxGetProperty(PLHS(2),1,'Nparx')
!
      IF( NPARXF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Nparx not found")
      ENDIF
!
      M=SIZE(NPARXF8)
      D1=M
      NPARXF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('do
     &uble'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(NPARXF8ptr2)), 1, D1 )
      NPARXF8MWS => Apx1
      IF( .NOT.ASSOCIATED(NPARXF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Nparx pointer
     & data")
      ENDIF
!
      DO I=1,M
        NPARXF8MWS(I)=NPARXF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Nparx',NPARXF8ptr2)
      CALL mxDestroyArray(NPARXF8ptr2)
      CALL mxDestroyArray(NPARXF8ptr1)
!
! Assign Ifpr values --------------
      IFPRF8ptr1=mxGetProperty(PLHS(2),1,'Ifpr')
!
      IF( IFPRF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ifpr not found")
      ENDIF
!
      M=SIZE(IFPRF8,1)
      N=SIZE(IFPRF8,2)
      D2(1)=M
      D2(2)=N
      IFPRF8ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('dou
     &ble'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(IFPRF8ptr2)), 1, D2 )
      IFPRF8MWS => Apx2
      IF( .NOT.ASSOCIATED(IFPRF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ifpr pointer 
     &data")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          IFPRF8MWS(I,J)=IFPRF8(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Ifpr',IFPRF8ptr2)
      CALL mxDestroyArray(IFPRF8ptr2)
      CALL mxDestroyArray(IFPRF8ptr1)
!
! Assign T values --------------
      TF8ptr1=mxGetProperty(PLHS(2),1,'T')
!
      IF( TF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property T not found")
      ENDIF
!
      M=SIZE(TF8)
      D1=M
      TF8ptr2 = mxCreateNumericArray(1,D1,mxClassIDFromClassName('double
     &'),0)
      CALL MatlabAPI_COM_Apx1(%VAL(mxGetPr(TF8ptr2)), 1, D1 )
      TF8MWS => Apx1
      IF( .NOT.ASSOCIATED(TF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to T pointer dat
     &a")
      ENDIF
!
      DO I=1,M
        TF8MWS(I)=TF8(I)
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'T',TF8ptr2)
      CALL mxDestroyArray(TF8ptr2)
      CALL mxDestroyArray(TF8ptr1)
!
! Assign Tm values --------------
      TMF8ptr1=mxGetProperty(PLHS(2),1,'Tm')
!
      IF( TMF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Tm not found")
      ENDIF
!
      M=SIZE(TMF8,1)
      N=SIZE(TMF8,2)
      D2(1)=M
      D2(2)=N
      TMF8ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('doubl
     &e'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(TMF8ptr2)), 1, D2 )
      TMF8MWS => Apx2
      IF( .NOT.ASSOCIATED(TMF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Tm pointer da
     &ta")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          TMF8MWS(I,J)=TMF8(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Tm',TMF8ptr2)
      CALL mxDestroyArray(TMF8ptr2)
      CALL mxDestroyArray(TMF8ptr1)
!
! Assign Par values --------------
      PARF8ptr1=mxGetProperty(PLHS(2),1,'Par')
!
      IF( PARF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Par not found")
      ENDIF
!
      M=SIZE(PARF8,1)
      N=SIZE(PARF8,2)
      D2(1)=M
      D2(2)=N
      PARF8ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(PARF8ptr2)), 1, D2 )
      PARF8MWS => Apx2
      IF( .NOT.ASSOCIATED(PARF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Par pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          PARF8MWS(I,J)=PARF8(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Par',PARF8ptr2)
      CALL mxDestroyArray(PARF8ptr2)
      CALL mxDestroyArray(PARF8ptr1)
!
! Assign Rldot values --------------
      RLDOTF8ptr1=mxGetProperty(PLHS(2),1,'Rldot')
!
      IF( RLDOTF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Rldot not found")
      ENDIF
!
      M=SIZE(RLDOTF8,1)
      N=SIZE(RLDOTF8,2)
      D2(1)=M
      D2(2)=N
      RLDOTF8ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('do
     &uble'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(RLDOTF8ptr2)), 1, D2 )
      RLDOTF8MWS => Apx2
      IF( .NOT.ASSOCIATED(RLDOTF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Rldot pointer
     & data")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          RLDOTF8MWS(I,J)=RLDOTF8(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Rldot',RLDOTF8ptr2)
      CALL mxDestroyArray(RLDOTF8ptr2)
      CALL mxDestroyArray(RLDOTF8ptr1)
!
! Assign U values --------------
      UF8ptr1=mxGetProperty(PLHS(2),1,'U')
!
      IF( UF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property U not found")
      ENDIF
!
      M=SIZE(UF8,1)
      N=SIZE(UF8,2)
      D2(1)=M
      D2(2)=N
      UF8ptr2 = mxCreateNumericArray(2,D2,mxClassIDFromClassName('double
     &'),0)
      CALL MatlabAPI_COM_Apx2(%VAL(mxGetPr(UF8ptr2)), 1, D2 )
      UF8MWS => Apx2
      IF( .NOT.ASSOCIATED(UF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to U pointer dat
     &a")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          UF8MWS(I,J)=UF8(I,J)
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'U',UF8ptr2)
      CALL mxDestroyArray(UF8ptr2)
      CALL mxDestroyArray(UF8ptr1)
!
! Assign Ups values --------------
      UPSF8ptr1=mxGetProperty(PLHS(2),1,'Ups')
!
      IF( UPSF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Ups not found")
      ENDIF
!
      M=SIZE(UPSF8,1)
      N=SIZE(UPSF8,2)
      O=SIZE(UPSF8,3)
      D3(1)=M
      D3(2)=N
      D3(3)=O
      UPSF8ptr2 = mxCreateNumericArray(3,D3,mxClassIDFromClassName('doub
     &le'),0)
      CALL MatlabAPI_COM_Apx3(%VAL(mxGetPr(UPSF8ptr2)), 1, D3 )
      UPSF8MWS => Apx3
      IF( .NOT.ASSOCIATED(UPSF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Ups pointer d
     &ata")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          DO K=1,O
            UPSF8MWS(I,J,K)=UPSF8(I,J,K)
          ENDDO
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Ups',UPSF8ptr2)
      CALL mxDestroyArray(UPSF8ptr2)
      CALL mxDestroyArray(UPSF8ptr1)
!
! Assign Udotps values --------------
      UDOTPSF8ptr1=mxGetProperty(PLHS(2),1,'Udotps')
!
      IF( UDOTPSF8ptr1 == 0 ) THEN
        CALL AUTOSTOPWITHERROR("Property Udotps not found")
      ENDIF
!
      M=SIZE(UDOTPSF8,1)
      N=SIZE(UDOTPSF8,2)
      O=SIZE(UDOTPSF8,3)
      D3(1)=M
      D3(2)=N
      D3(3)=O
      UDOTPSF8ptr2 = mxCreateNumericArray(3,D3,mxClassIDFromClassName('d
     &ouble'),0)
      CALL MatlabAPI_COM_Apx3(%VAL(mxGetPr(UDOTPSF8ptr2)), 1, D3 )
      UDOTPSF8MWS => Apx3
      IF( .NOT.ASSOCIATED(UDOTPSF8MWS) ) THEN
        CALL AUTOSTOPWITHERROR("Internal error pointing to Udotps pointe
     &r data")
      ENDIF
!
      DO I=1,M
        DO J=1,N
          DO K=1,O
            UDOTPSF8MWS(I,J,K)=UDOTPSF8(I,J,K)
          ENDDO
        ENDDO
      ENDDO
!
      CALL mxSetProperty(PLHS(2),1,'Udotps',UDOTPSF8ptr2)
      CALL mxDestroyArray(UDOTPSF8ptr2)
      CALL mxDestroyArray(UDOTPSF8ptr1)
!
!
      RETURN
      END           
!----------------------------------------------------------------------      
! Functions from MatlabAPImx.f supplied by James Tursa on
! Matlab central file exchange
!----------------------------------------------------------------------      
      subroutine MatlabAPI_COM_Apx1( A, stride, N )
      implicit none
!-ARG
      mwSize, intent(in) :: stride, N
      real(8), target, intent(in) :: A(stride, N)
!-COM
      real(8), pointer :: Apx1(:)
      common /MatlabAPI_COMA1/ Apx1
!-----
      Apx1 => A(1,:)
      return
      end subroutine MatlabAPI_COM_Apx1
!----------------------------------------------------------------------      
      subroutine MatlabAPI_COM_Apx2( A, stride, DIMS )
      implicit none
!-ARG
      mwSize, intent(in) :: stride, DIMS(2)
      real(8), target, intent(in) :: A(stride,DIMS(1),DIMS(2))
!-COM
      real(8), pointer :: Apx2(:,:)
      common /MatlabAPI_COMA2/ Apx2
!-----
      Apx2 => A(1,:,:)
      return
      end subroutine MatlabAPI_COM_Apx2
!----------------------------------------------------------------------      
      subroutine MatlabAPI_COM_Apx3( A, stride, DIMS )
      implicit none
!-ARG
      mwSize, intent(in) :: stride, DIMS(3)
      real(8), target, intent(in) :: A(stride,DIMS(1),DIMS(2),DIMS(3))
!-COM
      real(8), pointer :: Apx3(:,:,:)
      common /MatlabAPI_COMA3/ Apx3
!-----
      Apx3 => A(1,:,:,:)
      return
      end subroutine MatlabAPI_COM_Apx3