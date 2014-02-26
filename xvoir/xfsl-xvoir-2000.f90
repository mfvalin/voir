!/* RMNLIB - Library of useful routines for C and FORTRAN programming
! * Copyright (C) 1975-2014  Division de Recherche en Prevision Numerique
! *                          Environnement Canada
! *
! * This library is free software; you can redistribute it and/or
! * modify it under the terms of the GNU Lesser General Public
! * License as published by the Free Software Foundation,
! * version 2.1 of the License.
! *
! * This library is distributed in the hope that it will be useful,
! * but WITHOUT ANY WARRANTY; without even the implied warranty of
! * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! * Lesser General Public License for more details.
! *
! * You should have received a copy of the GNU Lesser General Public
! * License along with this library; if not, write to the
! * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
! * Boston, MA 02111-1307, USA.
! */
integer function xfslvoir2000(nomfich, iun, ttlrecs, winind, typesel, styleflag)
  use convert_ip123
  use ISO_C_BINDING
  implicit none
  interface
    function fstcantranslate(name) result (yesno) &
       BIND(C,name='FstCanTranslateName')
    integer :: yesno
    character(len=1) :: name
    end function
  end interface
  integer ttlrecs,ntmrecs
  character*128 nomfich
  integer iun, winind, typesel
  logical styleflag
  
  include "xfsl-voir.cdk"
  
  character*4 nomvar
  character*2 typvar
  character*1  grtyp, cdatyp
  character*12 etiket
  character*148  titre
  
  integer key, date0, deet, npas, ni, nj, nk, nbits, datyp 
  integer ip1, ip2, ip3, swa, lng, dltf, ubc
  integer ig1, ig2, ig3, ig4, extra1, extra2, extra3
  integer fstinf, fstprm, fstsui, fstrwd
  integer fnom, fstfrm, res
  integer xselouv, xseloup, xselins, xselouf
  real xg1, xg2, xg3, xg4
  integer yyyymmdd,hhmmssss
  
  integer ier
  integer kind, kind1, kind2, kind3
  real p, p1,p2,p3
  character*12 string, string1, string2, string3
  integer i, j, inf, status

  integer, dimension (:), allocatable :: reclist

  integer, parameter :: Max_Kind=31
  character *2 kinds(0:Max_Kind)
  data kinds                                                                  &
  &   / ' m', 'sg', 'mb', '##', ' M', 'hy', 'th', '??',                       &
  &     '??', '??', ' H', '??', '??', '??', '??', '  ',                       &
  &     '??', '[]', '??', '??', '??', 'mp', '??', '??',                       &
  &     '??', '??', '??', '??', '??', '??', '??', '  '/

  if (ttlrecs.gt.0) then
     allocate (reclist(ttlrecs))
  endif
  
  nbdes = 19
  call initidv(idents)                         ! fill idents array with column titles
  call inittabv(tableau, table, ligne)         ! fill table with displacements, each element of tables is ligne chars long
  write(titre, 5) nomfich
  
  res = xseloup(titre, ttlrecs, idents, nbdes, winind, typesel)
  
  if (ttlrecs.eq.0) then                   ! 0 records in linked standard file
     res = xselins(tableau,table,ttlrecs)
     goto 100                              ! the end
  endif
  
  i = 0
  ier = fstrwd(iun)                        ! in case it is a sequential standard file
  key = fstinf(iun, ni, nj, nk,  -1, ' ', -1, -1, -1, ' ', ' ')
  do while (key.ge.0)                      ! loop until end of file
    i = i+1
    
    inf = fstprm(key, date0, deet, npas, ni, nj, nk, nbits,datyp, ip1, ip2, ip3, typvar, & 
        nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3)
    
    call get_cdatyp(cdatyp, mod(datyp,16))
    if(datyp>128) call get_cdatyp(cdatyp, 128+mod(datyp,16))  ! eliminate "missing" flag
    call newdate(date0,yyyymmdd,hhmmssss,-3)
    hhmmssss = hhmmssss / 100
    
    if (.not.styleflag) then
      write(tableau(mod(i-1,64)), 10) NOMVAR, TYPVAR, IP1, IP2, IP3, NI, NJ, NK, ETIKET, &
             yyyymmdd,hhmmssss, deet, npas, grtyp, ig1, ig2, ig3, ig4, cdatyp, nbits
    else
!      call convip_plus( ip1, p1, kind, -1, string1, .true.)
!      call convip_plus( ip2, p2, kind, -1, string2, .true.)
!      call convip_plus( ip3, p3, kind, -1, string3, .true.)
      kind1 = -1 ; kind2 = -1 ; kind3 = -1                       ! in case there is a conversion error
      if(0/=fstcantranslate(NOMVAR//achar(0))) then              ! NOMVAR allows ip decoding
        status = decode_ip(p1,kind1,p2,kind2,p3,kind3,ip1,ip2,ip3)
      endif
      if(kind1>0 .and. kind2>0 .and. kind3>0) then  ! no decoding error
        kind1 = mod(kind1,31)
        kind2 = mod(kind2,31)
        kind3 = mod(kind3,31)
        status = value_to_string(p1 , string1 , len(string1)-2 )
        string1=trim(string1)//kind_to_string(kind1)
        string=""
        string(len(string)-len(trim(string1))+1:len(string))=trim(string1)
        string1=string
        status = value_to_string(p2 , string2 , len(string2)-2 )
        string2=trim(string2)//kind_to_string(kind2)
        string=""
        string(len(string)-len(trim(string2))+1:len(string))=trim(string2)
        string2=string
        status = value_to_string(p3 , string3 , len(string3)-2 )
        string3=trim(string3)//kind_to_string(kind3)
        string=""
        string(len(string)-len(trim(string3))+1:len(string))=trim(string3)
        string3=string
      else  ! ip1/2/3 cannot or must not be decoded
        p1=ip1
        p2=ip2
        p3=ip3
        status = value_to_string(p1 , string1 , len(string1) )
        string=""
        string(len(string)-len(trim(string1))+1:len(string))=trim(string1)
        string1=string
        status = value_to_string(p2 , string2 , len(string2) )
        string=""
        string(len(string)-len(trim(string2))+1:len(string))=trim(string2)
        string2=string
        status = value_to_string(p3 , string3 , len(string3) )
        string=""
        string(len(string)-len(trim(string3))+1:len(string))=trim(string3)
        string3=string        
      endif
!      print *,':'//string1//':'//string2//':'//string3//':'
!
      if (grtyp.ne.'Z'.and.grtyp.ne.'Y'.and.grtyp.ne.'X'.and.grtyp.ne.'#') then
          call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
          write(tableau(mod(i-1,64)), 11) NOMVAR,TYPVAR,string1,string2,string3,NI, NJ, NK, ETIKET, &
            yyyymmdd,hhmmssss, deet, npas,grtyp, xg1, xg2, xg3, xg4, cdatyp, nbits
      else
          write(tableau(mod(i-1,64)), 12) NOMVAR,TYPVAR, string1,string2,string3,NI, NJ, NK, ETIKET, &
            yyyymmdd,hhmmssss, deet, npas,grtyp, ig1, ig2, ig3, ig4, cdatyp, nbits
!          xg1 = ig1
!          xg2 = ig2
!          xg3 = ig3
!          xg4 = ig4
      endif     
    endif
    
    reclist(i) = key
    
    if (ttlrecs.le.1) then                  ! only one record in file
      res = xselins(tableau,table,ttlrecs)
    endif
    
    ntmrecs = mod(i,64)
    if (ntmrecs.eq.0) then
      ntmrecs = 64
    endif
    
    if (0.eq.mod(i,64).or.i.eq.ttlrecs) then ! we have either a block of maxrecs(64) or we have reached the last record
      res = xselins(tableau,table,ntmrecs)   ! insert a block of ntmrecs (up to maxrecs=64) lines into selector
    endif
!   
    key = fstsui(iun, ni, nj, nk)
  enddo  ! (key.ge.0)  loop until end of file
100  res = xselouf(table, ntmrecs)
  xfslvoir2000 = winind
  
2 format(40a)
4 format(3i16)
5 format(128a)
6 format(40a)
10 FORMAT(A4, X, A2, X, I12, X, I12, X, I12, X, I5, X, I5, X, I5, X, A12, X, i8.8,i7.6, X, i6, x, i7, x, a1, x, i9,   x, i9,   x, i9,   x, i9,   x, a1,i2.2)
11 FORMAT(A4, X, A2, X, a12, X, a12, X, a12, X, I5, X, I5, X, I5, X, A12, X, i8.8,i7.6, x, i6, x, i7, x, a1, x, g9.3, x, g9.3, x, g9.3, x, g9.3, x, a1,i2.2)
12 FORMAT(A4, X, A2, X, a12, X, a12, X, a12, X, I5, X, I5, X, I5, X, A12, X, i8.8,i7.6, x, i6, x, i7, x, a1, x, i9,   x, i9,   x, i9,   x, i9,   x, a1,i2.2)
  
  return
end function xfslvoir2000


!c     ****************************************************************
!c     **                                                            **
!c     ****************************************************************

integer function xfslferv(winind)
  implicit none
  integer winind
  
  include "xfsl-voir.cdk"
  
  integer xselfer
  integer i, inf, res
  
  xfslferv = xselfer(winind)
  
  
  return
end function xfslferv

!     ****************************************************************
!     **                                                            **
!     ****************************************************************

integer function xfslactv(slkeys, nslkeys, winind)
  implicit none
  integer nslkeys
  integer slkeys(nslkeys), winind
  
  include "xfsl-voir.cdk"
  
  integer xselact
  integer i, inf, res
  
  xfslactv = xselact(slkeys, nslkeys, winind)
  
!   do 200 i=1, nslkeys
!      slkeys(i) = recliste(slkeys(i)-1)
! 200  continue
!      
     
     return
   end function xfslactv
   
!   c     ****************************************************************
!   c     **                                                            **
!   c     ****************************************************************
   
! NOMV TV IP1       IP2       IP3      NI    NJ    NK    ETIQUETTE    YYYYMMDD HHMMSS DEET  NPAS    G IG1         IG2      IG3      IG4      DTY
! 1234 12 123456789 123456789 1234567891234561234561234561234567890abc12345678 123456 12345 1234567 1 12312345678 12345678 12345678 12345678 123
   
   subroutine initidv(idents)
     character*16 idents(*)
     
     integer i, j, ulng
     integer  getulng
     external getulng
     
     idents(1) =  'NOMV'
     idents(2)  = 'TV'
     idents(3)  = '---- IP1----'
     idents(4)  = '---- IP2----'
     idents(5)  = '---- IP3----'
     idents(6)  = '-NI--'
     idents(7)  = '-NJ--'
     idents(8)  = '-NK--'
     idents(9)  = '---ETIKET---'
     idents(10) = 'YYYYMMDD'
     idents(11) = 'HHMMSS'
     idents(12) = '-DEET-'
     idents(13) = '-NPAS--'
     idents(14) = 'G'
     idents(15) = '---IG1---'
     idents(16) = '---IG2---'
     idents(17) = '---IG3---'
     idents(18) = '---IG4---'
     idents(19) = 'DTY'
     
     return
   end subroutine initidv

   subroutine inittabv(tableau, table, len)
     character*160 tableau(*)
     integer table(3, *)
     integer len
     integer sumlen
     integer i
     
     integer reclen(20)
     data reclen /5,3,13,13,13,6,6,6,13,9,7,7,8,2,10,10,10,10,3,0/
     
     
     sumlen       = 0
     do i=1,20
        table(1,i)   = reclen(i)
        table(2,i)   = len
        table(3,i)   = sumlen
        sumlen       = sumlen + table(1,i)
     enddo
     
     return 
   end subroutine inittabv
   
   
!dompe

! 50 if (key.lt.0) goto 100
!   i = i + 1
!   
!   if (key.lt.0) goto 100
!   inf = fstprm(key, date0, deet, npas, ni, nj, nk, nbits,datyp, ip1, ip2, ip3, typvar, &
!        nomvar, etiket, grtyp, ig1, ig2, ig3, ig4, swa, lng, dltf, ubc, extra1, extra2, extra3)
!   
!   if (datyp.eq.0) then
!      cdatyp = 'X'
!   else
!      if (datyp.eq.1) then
!         cdatyp = 'R'
!      else
!         if (datyp.eq.2) then
!            cdatyp = 'I'
!         else
!            cdatyp = 'C'
!         endif
!      endif
!   endif
!   
!   if (.not.styleflag) then
!      write(tableau(mod(i-1,64)), 10) NOMVAR, TYPVAR, IP1, IP2, IP3,NI, NJ, NK, ETIKET, &
!           DATE0, deet, npas,grtyp, ig1, ig2, ig3, ig4, cdatyp, nbits
!   else
!      if (grtyp.ne.'Z'.and.grtyp.ne.'Y') then
!         call cigaxg(grtyp,xg1,xg2,xg3,xg4,ig1,ig2,ig3,ig4)
!      else
!         xg1 = ig1
!         xg2 = ig2
!         xg3 = ig3
!         xg4 = ig4
!      endif
!      call newdate(date0,yyyymmdd,hhmmssss,-3)
!      hhmmssss = hhmmssss / 100
!      call convip( ip1, p, kind, -1, string, .true.)
!      write(tableau(mod(i-1,64)), 11) NOMVAR,TYPVAR,string,IP2,IP3,NI, NJ, NK, ETIKET, &
!           yyyymmdd,hhmmssss, deet, npas,grtyp, xg1, xg2, xg3, xg4, cdatyp, nbits
!   endif
!   
!   reclist(i) = key
!   

  subroutine get_cdatyp(cdatyp, datyp)
  character*1 cdatyp
  integer datyp
  
    if (datyp.eq.0) then
      cdatyp = 'X'
    else if (datyp.eq.1) then
        cdatyp = 'R'
    else if (datyp.eq.2) then
        cdatyp = 'I'
    else if (datyp.eq.4) then
        cdatyp = 'U'
    else if (datyp.eq.5) then
        cdatyp='E'
    else if (datyp.eq.8) then
        cdatyp='C'
    else if (datyp.eq.6) then
        cdatyp='F'
    else if (datyp.eq.134) then
        cdatyp='f'
    else if (datyp.eq.130) then
        cdatyp='i'
    else if (datyp.eq.132) then
        cdatyp='u'
    else
        cdatyp = 'S'
    endif

  return
  end subroutine get_cdatyp

