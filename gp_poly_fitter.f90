! GENERAL-PURPOSE POLYNOMIAL FITTER   NDD  14/08/2007
! (Adapted from extrapolate_tau utility.)

! This program allows the user to fit a polynomial to a set of data
! held in a file specified on the command line.  The data are assumed
! to be in "x y [error in y]" format in a file.  The program will ask
! the user for the number of terms in the polynomial and the
! exponents, etc.

MODULE utils
  ! Miscellaneous subroutines.
  IMPLICIT NONE
  INTEGER,PARAMETER :: dp=KIND(1.d0) ! Double precision kind.
  PRIVATE
  PUBLIC dp,i2s,real_number,errstop,wordwrap,gammq


CONTAINS


  CHARACTER(12) FUNCTION i2s(n)
    ! Convert integers to left justified strings that can be printed in the
    ! middle of a sentence without introducing large amounts of white space.
    INTEGER,INTENT(in) :: n
    INTEGER :: i,j
    INTEGER,PARAMETER :: ichar0=ICHAR('0')
    i2s=''
    i=ABS(n)
    DO j=LEN(i2s),1,-1
      i2s(j:j)=ACHAR(ichar0+MOD(i,10))
      i=i/10 ; IF(i==0)EXIT
    ENDDO ! j
    IF(n<0)THEN
      i2s='-'//ADJUSTL(i2s)
    ELSE
      i2s=ADJUSTL(i2s)
    ENDIF ! n<0
  END FUNCTION i2s


  CHARACTER(36) FUNCTION real_number(r)
    ! Write out a real number r, stripping trailing zeroes.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: r
    INTEGER :: k
    WRITE(real_number,*)r
    real_number=ADJUSTL(real_number)
    IF(INDEX(real_number,'.')>0)THEN
      DO k=LEN_TRIM(real_number),1,-1
        IF(real_number(k:k)=='0')THEN
          real_number(k:k)=' '
        ELSE
          EXIT
        ENDIF ! k
      ENDDO ! k
    ENDIF ! Decimal place present.
  END FUNCTION real_number


  SUBROUTINE errstop(sub,message)
    ! Report an error and stop.
    IMPLICIT NONE
    CHARACTER(*),INTENT(in) :: sub,message
    WRITE(*,*)
    WRITE(*,*)'ERROR in subroutine '//TRIM(ADJUSTL(sub))//'.'
    WRITE(*,*)
    CALL wordwrap(TRIM(ADJUSTL(message)))
    WRITE(*,*)
    STOP
  END SUBROUTINE errstop


  SUBROUTINE wordwrap(text,unit_in,linelength_in)
    ! This subroutine prints out the contents of the character string 'text',
    ! ensuring that line breaks only occur at space characters.  The output
    ! is written to unit unit_in if this parameter is supplied; otherwise the
    ! output is written to unit o.  The maximum length of each line is given
    ! by linelength_in if this is supplied; otherwise the default line length
    ! is 79 characters.
    IMPLICIT NONE
    INTEGER,INTENT(in),OPTIONAL :: unit_in,linelength_in
    CHARACTER(*),INTENT(in) :: text
    CHARACTER(260) :: temp
    INTEGER :: i,unit,lentext,startpoint,stoppoint,lastpos,linelength
    IF(PRESENT(unit_in))THEN
      unit=unit_in
    ELSE
      unit=6
    ENDIF ! unit supplied.
    lentext=LEN(TRIM(text))
    IF(lentext<1)THEN
      WRITE(unit,*)
      RETURN
    ENDIF ! No text
    IF(PRESENT(linelength_in))THEN
      IF(linelength_in>=2)THEN
        linelength=linelength_in
      ELSE
        linelength=2
      ENDIF ! sensible line-length supplied.
    ELSE
      linelength=79
    ENDIF ! linelength present.
    startpoint=1
    DO i=1,HUGE(1)-1
      stoppoint=startpoint+linelength-1
      IF(stoppoint<=lentext)THEN
        lastpos=INDEX(TRIM(text(startpoint:stoppoint))," ",.TRUE.)
        IF(lastpos>0)stoppoint=startpoint+lastpos-1
      ELSE
        stoppoint=lentext
      ENDIF ! stoppoint <= length of text
      IF(i==1)THEN
        ! Allow the user to indent the first line, if (s)he wishes.
        temp=text(startpoint:stoppoint) ! or else pathscale f90 fails to compile
        WRITE(unit,*)TRIM(temp)
      ELSE
        temp=text(startpoint:stoppoint) ! or else pathscale f90 fails to compile
        WRITE(unit,*)TRIM(ADJUSTL(temp))
      ENDIF ! i=1
      IF(stoppoint==lentext)THEN
        EXIT
      ELSE
        startpoint=stoppoint+1
      ENDIF ! Finished text?
    ENDDO ! Loop over lines.
  END SUBROUTINE wordwrap


  REAL(dp) FUNCTION gammq(a,x)
    ! This function returns Q(a,x)=Gamma_incomplete(a,x)/Gamma(a).
    ! Adapted from Numerical Recipes.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: a,x
    IF(x<0.d0.OR.a<=0.d0)CALL errstop('GAMMQ','Undefined.')
    IF(x<a+1.d0)THEN
      gammq=1.d0-gser(a,x)
    ELSE
      gammq=gcf(a,x)
    ENDIF
  END FUNCTION gammq


  REAL(dp) FUNCTION gser(a,x)
    ! This function returns the Gamma P(a,x) function, evaluated as a series
    ! expansion.  Adapted from Numerical Recipes.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: a,x
    REAL(dp),PARAMETER :: eps=6.d-16
    INTEGER,PARAMETER :: itmax=10000
    INTEGER :: n
    REAL(dp) :: ap,del,sum
    IF(x<=0.d0)THEN
      IF(x<0.d0)CALL errstop('GSER','x<0.')
      gser=0.d0
      RETURN
    ENDIF ! x<=0
    ap=a
    sum=1.d0/a
    del=sum
    DO n=1,itmax
      ap=ap+1.d0
      del=del*x/ap
      sum=sum+del
      IF(ABS(del)<ABS(sum)*eps)THEN
        gser=sum*EXP(-x+a*LOG(x)-gammln(a))
        RETURN
      ENDIF ! Converged
    ENDDO ! n
    CALL errstop('GSER','Failed to converge.')
  END FUNCTION gser


  REAL(dp) FUNCTION gcf(a,x)
    ! This function returns the Gamma Q(a,x) function, evaluated as a ctd
    ! fraction.  Adapted from Numerical Recipes.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: a,x
    REAL(dp),PARAMETER :: eps=6.d-16,fpmin=1.d-300
    INTEGER,PARAMETER :: itmax=10000
    INTEGER :: i
    REAL(dp) :: an,b,c,d,del,h
    b=x+1.d0-a
    c=1.d0/fpmin
    d=1.d0/b
    h=d
    DO i=1,itmax
      an=-DBLE(i)*(DBLE(i)-a)
      b=b+2.d0
      d=an*d+b
      IF(ABS(d)<fpmin)d=fpmin
      c=b+an/c
      IF(ABS(c)<fpmin)c=fpmin
      d=1.d0/d
      del=d*c
      h=h*del
      IF(ABS(del-1.d0)<eps)THEN
        gcf=EXP(-x+a*LOG(x)-gammln(a))*h
        RETURN
      ENDIF ! Converged
    ENDDO ! i
    CALL errstop('GCF','Failed to converge.')
  END FUNCTION gcf


  REAL(dp) FUNCTION gammln(xx)
    ! This function returns the logarithm of the Gamma function.  Adapted from
    ! Numerical Recipes.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: xx
    INTEGER :: j
    REAL(dp) :: ser,tmp,x,y
    REAL(dp),PARAMETER :: cof(6)=(/76.18009172947146d0, &
      &-86.50532032941677d0,24.01409824083091d0,-1.231739572450155d0, &
      &0.1208650973866179d-2,-0.5395239384953d-5/),stp=2.5066282746310005d0
    x=xx
    y=x
    tmp=x+5.5d0
    tmp=(x+0.5d0)*LOG(tmp)-tmp
    ser=1.000000000190015d0
    DO j=1,6
      y=y+1.d0
      ser=ser+cof(j)/y
    ENDDO ! j
    gammln=tmp+LOG(stp*ser/x)
  END FUNCTION gammln


END MODULE utils


MODULE linear_fit
  ! Miscellaneous subroutines.
  USE utils
  IMPLICIT NONE
  ! Numbers of (x,y) data points and parameters.
  INTEGER :: npoints,nparam
  ! x & y data, exponents in fitting function and error bars.
  REAL(dp),ALLOCATABLE :: x(:),y(:),n_exp(:),rec_sigma_y_sq(:)
  PRIVATE
  PUBLIC perform_fit


CONTAINS


  SUBROUTINE calc_parameters(a,Minv,chi2)
    ! Evaluate the parameters using the method given in my notes.  Return
    ! M^(-1), which is needed for calculating error bars on functions of the
    ! fitted parameters.
    IMPLICIT NONE
    REAL(dp),INTENT(out) :: a(nparam),Minv(nparam,nparam),chi2
    REAL(dp) :: M(nparam,nparam),c(nparam),tempr(1)
    REAL(dp),ALLOCATABLE :: work(:)
    INTEGER :: info,i,j,ipiv(nparam),lwork,ialloc
    INTERFACE
      SUBROUTINE DSYTRF(UPLO,N,A,LDA,IPIV,WORK,LWORK,INFO)
        CHARACTER(1),INTENT(in) :: UPLO
        INTEGER,INTENT(in) :: N,LDA,LWORK
        REAL(KIND(1.d0)),INTENT(out) :: A(LDA,*),WORK(*)
        INTEGER,INTENT(out) :: IPIV(*)
        INTEGER,INTENT(out) :: INFO
      END SUBROUTINE DSYTRF
      SUBROUTINE DSYTRI(UPLO,N,A,LDA,IPIV,WORK,INFO)
        CHARACTER(1),INTENT(in) :: UPLO
        INTEGER,INTENT(in) :: N,LDA,IPIV(*)
        REAL(KIND(1.d0)),INTENT(inout) :: A(LDA,*)
        REAL(KIND(1.d0)),INTENT(out) :: WORK(*)
        INTEGER,INTENT(out) :: INFO
      END SUBROUTINE DSYTRI
    END INTERFACE

    ! Construct vector c.
    CALL construct_c(c)

    ! Construct matrix M (lower triangle only).
    CALL construct_M(M)

    ! Invert matrix M using LAPACK routines for inverting symmetric matrices.
    Minv=M
    CALL dsytrf('L',nparam,Minv(1,1),nparam,ipiv(1),tempr(1),-1,info)
    IF(info/=0)CALL errstop('CALC_PARAMETERS','Matrix inversion failed (1).')
    lwork=NINT(tempr(1))
    ALLOCATE(work(lwork),stat=ialloc)
    IF(ialloc/=0)CALL errstop('CALC_PARAMETERS','Allocation error: WORK (1).')
    CALL dsytrf('L',nparam,Minv(1,1),nparam,ipiv(1),work(1),lwork,info)
    IF(info/=0)CALL errstop('CALC_PARAMETERS','Matrix inversion failed (2).')
    DEALLOCATE(work)
    ALLOCATE(work(nparam),stat=ialloc)
    IF(ialloc/=0)CALL errstop('CALC_PARAMETERS','Allocation error: WORK (2).')   
    CALL dsytri('L',nparam,Minv(1,1),nparam,ipiv(1),work(1),info)
    IF(info/=0)CALL errstop('CALC_PARAMETERS','Matrix inversion failed (3).')
    DEALLOCATE(work)

    ! Complete the upper triangle of Minv.
    DO i=1,nparam-1
      DO j=i+1,nparam
        Minv(i,j)=Minv(j,i)
      ENDDO ! j
    ENDDO ! i

    ! Hence evaluate the coefficients of the terms in the polynomial.
    a=MATMUL(Minv,c)

    ! The optimised chi^2 function.
    chi2=chi_squared(a)

  END SUBROUTINE calc_parameters


  REAL(dp) FUNCTION chi_squared(a)
    ! Evaluate the chi-squared value of the fit.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: a(nparam)
    INTEGER :: i,k
    REAL(dp) :: y_fit
    chi_squared=0.d0
    DO i=1,npoints
      y_fit=0.d0
      DO k=1,nparam
        y_fit=y_fit+a(k)*x(i)**n_exp(k)
      ENDDO ! k
      chi_squared=chi_squared+(y(i)-y_fit)**2*rec_sigma_y_sq(i)
    ENDDO ! i
  END FUNCTION chi_squared


  SUBROUTINE construct_c(c)
    ! Construct the vector c (see my notes).
    IMPLICIT NONE
    REAL(dp),INTENT(out) :: c(nparam)
    INTEGER :: i,j
    DO j=1,nparam
      c(j)=0.d0
      DO i=1,npoints
        c(j)=c(j)+y(i)*x(i)**n_exp(j)*rec_sigma_y_sq(i)
      ENDDO ! i
    ENDDO ! j
  END SUBROUTINE construct_c


  SUBROUTINE construct_M(M)
    ! Construct the symmetric matrix M (see my notes).  Only the lower
    ! triangular part is needed.
    IMPLICIT NONE
    REAL(dp),INTENT(out) :: M(nparam,nparam)
    INTEGER :: i,j,k
    M=0.d0
    DO k=1,nparam
      DO j=k,nparam
        DO i=1,npoints
          M(j,k)=M(j,k)+x(i)**(n_exp(j)+n_exp(k))*rec_sigma_y_sq(i)
        ENDDO ! i
      ENDDO ! j
    ENDDO ! k
  END SUBROUTINE construct_M


  SUBROUTINE rescale_errors(chi2,Minv)
    ! Rescale the error bars so that the chi^2 function is equal to
    ! npoints-nparam.  Update chi^2 function and Minv matrix accordingly.
    IMPLICIT NONE
    REAL(dp),INTENT(inout) :: chi2,Minv(nparam,nparam)
    INTEGER :: nu
    nu=npoints-nparam
    IF(nu<0)THEN
      CALL errstop('RESCALE_ERRORS','Fewer fitting points than parameters.')
    ELSEIF(nu==0)THEN
      RETURN ! Cannot rescale error bars.
    ENDIF ! ne<0
    rec_sigma_y_sq(1:npoints)=rec_sigma_y_sq(1:npoints)*DBLE(nu)/chi2
    Minv=Minv*(chi2/DBLE(nu))
    chi2=DBLE(nu)
  END SUBROUTINE rescale_errors


  SUBROUTINE get_exponents
    ! Ask the user to enter the exponents of the terms in the fitting poly.
    IMPLICIT NONE
    INTEGER :: i,j,ierr,ialloc
    REAL(dp) :: temp
    REAL(dp),PARAMETER :: tol_zero=1.d-8
    LOGICAL :: test_flag

    DO
      WRITE(*,*)'Please enter the number of terms in the polynomial to be &
        &fitted.'
      READ(*,*,iostat=ierr)nparam
      IF(ierr/=0)THEN
        WRITE(*,*)'Error - please try again.'
      ELSE
        IF(nparam<1)THEN
          WRITE(*,*)'Interpolating polynomial must have at least one term!'
        ELSE
          IF(nparam>npoints)THEN
            WRITE(*,*)'Insufficient data to determine polynomial coefficients.'
          ELSE
            EXIT
          ENDIF ! nparam>npoints
        ENDIF ! nparam<1
      ENDIF ! ierr/=0
    ENDDO

    ALLOCATE(n_exp(nparam),stat=ialloc)
    IF(ialloc/=0)CALL errstop('GET_EXPONENTS','Allocation error.')

    DO
      WRITE(*,*)'Please enter the '//TRIM(i2s(nparam)) &
        &//' exponents in the fitting polynomial.'
      READ(*,*,iostat=ierr)n_exp(1:nparam)
      IF(ierr/=0)THEN
        WRITE(*,*)'Error - please try again.'
      ELSE
        test_flag=.FALSE.
        ol: DO i=1,nparam
          DO j=1,i-1
            IF(ABS(n_exp(i)-n_exp(j))<=2.d0*tol_zero)THEN
              test_flag=.TRUE.
              EXIT ol
            ENDIF ! Identical exponents
          ENDDO ! j
        ENDDO ol ! i
        IF(test_flag)THEN
          WRITE(*,*)'Two exponents appear to be identical.  Please try again.'
        ELSE
          EXIT
        ENDIF ! Identical exponents
      ENDIF ! Error
    ENDDO

    IF(ANY(n_exp>0.d0))THEN
      ! Sort exponents into ascending order if any are +ve.
      DO i=1,nparam-1
        DO j=i+1,nparam
          IF(n_exp(j)<n_exp(i))THEN
            temp=n_exp(i)
            n_exp(i)=n_exp(j)
            n_exp(j)=temp
          ENDIF ! Swap needed
        ENDDO ! j
      ENDDO ! i
    ELSE
      ! Sort exponents into descending order if all are -ve.
      DO i=1,nparam-1
        DO j=i+1,nparam
          IF(n_exp(j)>n_exp(i))THEN
            temp=n_exp(i)
            n_exp(i)=n_exp(j)
            n_exp(j)=temp
          ENDIF ! Swap needed
        ENDDO ! j
      ENDDO ! i
    ENDIF ! Any +ve exponents

  END SUBROUTINE get_exponents


  SUBROUTINE get_file(in_file)
    ! Find out the filename by one method or another.
    IMPLICIT NONE
    CHARACTER(*),INTENT(out) :: in_file
    INTEGER :: nargs,ierr
    LOGICAL :: file_exists
    nargs=command_argument_count()
    IF(nargs>0)THEN
      CALL get_command_argument(1,in_file)
      in_file=ADJUSTL(in_file)
    ELSE
      DO
        WRITE(*,*)'Please enter name of data file.'
        READ(*,*,iostat=ierr)in_file
        IF(ierr/=0)THEN
          WRITE(*,*)'Error - please try again.'
        ELSE
          in_file=ADJUSTL(in_file)
          INQUIRE(file=TRIM(in_file),exist=file_exists)
          IF(.NOT.file_exists)THEN
            WRITE(*,*)'File does not appear to exist. Please try again.'
          ELSE
            EXIT
          ENDIF ! File nonexistent
        ENDIF ! ierr/=0
      ENDDO
      WRITE(*,*)
    ENDIF ! nargs>0
  END SUBROUTINE get_file


  SUBROUTINE check_file(io,in_file,nlines,errorbars_present)
    ! Count the lines in the input file.
    IMPLICIT NONE
    INTEGER,INTENT(in) :: io
    CHARACTER(*),INTENT(in) :: in_file
    INTEGER,INTENT(out) :: nlines
    LOGICAL,INTENT(out) :: errorbars_present
    INTEGER :: ierr,i
    REAL(dp) :: dummy(3)
    nlines=0
    DO
      READ(io,*,iostat=ierr)dummy(1:2)
      IF(ierr>0)CALL errstop('CHECK_FILE','Error reading '//TRIM(in_file)//'.')
      IF(ierr<0)EXIT
      nlines=nlines+1
    ENDDO
    REWIND(io)
    IF(nlines==0)CALL errstop('CHECK_FILE','File '//TRIM(in_file) &
      &//' doesn''t contain any data.')
    DO i=1,nlines-1
      READ(io,*)dummy(1:2)
    ENDDO ! i
    READ(io,*,iostat=ierr)dummy(1:3)
    errorbars_present=(ierr==0)
    REWIND(io)
  END SUBROUTINE check_file


  SUBROUTINE read_file(io,in_file,errorbars_present)
    ! Read in the data in the input file.
    IMPLICIT NONE
    INTEGER,INTENT(in) :: io
    CHARACTER(*),INTENT(in) :: in_file 
    LOGICAL,INTENT(in) :: errorbars_present
    INTEGER :: i,ierr
    IF(errorbars_present)THEN
      DO i=1,npoints
        READ(io,*,iostat=ierr)x(i),y(i),rec_sigma_y_sq(i) ! x,y,err in y.
        IF(ierr/=0)CALL errstop('READ_FILE','Error reading '//TRIM(in_file)//'.')
        IF(rec_sigma_y_sq(i)<=0.d0)CALL errstop('READ_FILE', &
          &'Non-positive error bar at data line '//TRIM(i2s(i))//'.')
        rec_sigma_y_sq(i)=1.d0/rec_sigma_y_sq(i)**2
      ENDDO ! i
    ELSE
      DO i=1,npoints
        READ(io,*,iostat=ierr)x(i),y(i)
        IF(ierr/=0)CALL errstop('READ_FILE','Error reading '//TRIM(in_file)//'.')
      ENDDO ! i
      rec_sigma_y_sq=1.d0 ! Set error bars to 1, so chi^2 fit = LS fit.
    ENDIF ! errorbars_present
  END SUBROUTINE read_file


  SUBROUTINE construct_graphstr(a,graphstr)
    ! This subroutine returns the fitted polynomial in a suitable format for
    ! pasting into xmgrace.
    IMPLICIT NONE
    REAL(dp),INTENT(in) :: a(nparam)
    CHARACTER(*),INTENT(out) :: graphstr
    INTEGER :: j
    REAL(dp),PARAMETER :: tol_zero=1.d-8
    CHARACTER(1) :: plusstr
    CHARACTER(30) :: coeffstr
    CHARACTER(36) :: pwstr
    graphstr='y='
    DO j=1,nparam
      IF(ABS(ANINT(n_exp(j))-n_exp(j))<tol_zero)THEN
        IF(NINT(n_exp(j))==0)THEN
          pwstr='' ! Exponent = 0
        ELSEIF(NINT(n_exp(j))==1)THEN
          pwstr='*x' ! Exponent = 1
        ELSEIF(NINT(n_exp(j))==-1)THEN
          pwstr='/x' ! Exponent = -1
        ELSEIF(n_exp(j)>1.d0)THEN
          pwstr='*x^'//TRIM(i2s(NINT(n_exp(j))))  ! Exponent > 1 (integer)
        ELSE
          pwstr='/x^'//TRIM(i2s(-NINT(n_exp(j)))) ! Exponent < -1 (int)
        ENDIF ! n_exp=0
      ELSEIF(n_exp(j)>=0.d0)THEN
        WRITE(pwstr,*)n_exp(j)
        pwstr='*x^'//TRIM(real_number(n_exp(j)))   ! Positive real exponent
      ELSE
        WRITE(pwstr,*)-n_exp(j)
        pwstr='/x^'//TRIM(real_number(-n_exp(j)))  ! Negative real exponent
      ENDIF ! Power is an integer, etc.
      IF(j>1.AND.a(j)>=0.d0)THEN
        plusstr='+'
      ELSE
        plusstr=''
      ENDIF ! "+" needed
      WRITE(coeffstr,*)a(j)
      coeffstr=ADJUSTL(coeffstr)
      graphstr=TRIM(graphstr)//TRIM(plusstr)//TRIM(coeffstr)//TRIM(pwstr)
    ENDDO ! j
    ! Change "d" to "e" in graphstr.
    DO j=1,LEN_TRIM(graphstr)
      IF(graphstr(j:j)=='d'.OR.graphstr(j:j)=='D')graphstr(j:j)='E'
    ENDDO ! j
  END SUBROUTINE construct_graphstr


  SUBROUTINE construct_poly(polystr)
    ! This subroutine returns a string holding the form of the fitting
    ! function.
    IMPLICIT NONE
    CHARACTER(*),INTENT(out) :: polystr
    INTEGER :: j
    REAL(dp),PARAMETER :: tol_zero=1.d-8
    CHARACTER(36) :: pwstr
    polystr='y ='
    DO j=1,nparam
      IF(ABS(ANINT(n_exp(j))-n_exp(j))<tol_zero)THEN
        IF(NINT(n_exp(j))==0)THEN
          pwstr=''       ! Exponent=0
        ELSEIF(NINT(n_exp(j))==1)THEN
          pwstr='*x'   ! Exponent=1
        ELSEIF(NINT(n_exp(j))==-1)THEN
          pwstr='/x'   ! Exponent=-1
        ELSEIF(n_exp(j)>0.d0)THEN
          pwstr='*x^'//TRIM(i2s(NINT(n_exp(j))))  ! Pos. int. exponent
        ELSE
          pwstr='/x^'//TRIM(i2s(NINT(-n_exp(j)))) ! Neg. int. exponent
        ENDIF ! n_exp=0
      ELSEIF(n_exp(j)>=0.d0)THEN
        pwstr='*x^'//TRIM(real_number(n_exp(j)))   ! Pos. real exponent
      ELSE
        pwstr='/x^'//TRIM(real_number(-n_exp(j)))  ! Neg. real exponent
      ENDIF ! Power is an integer
      IF(j>1)THEN
        polystr=TRIM(polystr)//' + k_'//TRIM(i2s(j))//TRIM(pwstr)
      ELSE
        polystr=TRIM(polystr)//' k_'//TRIM(i2s(j))//TRIM(pwstr)
      ENDIF ! "+" needed
    ENDDO ! j
  END SUBROUTINE construct_poly


  SUBROUTINE perform_fit
    ! Carry out the fit of the polynomial.
    IMPLICIT NONE
    INTEGER :: j,k,ierr,ialloc,io
    REAL(dp) :: chi2
    REAL(dp),ALLOCATABLE :: a(:),Minv(:,:)
    CHARACTER(3) :: indstr
    CHARACTER(80) :: in_file
    CHARACTER(320) :: tempstr
    LOGICAL :: errorbars_present

    ! Read in data from file specified by user.
    CALL get_file(in_file)
    io=8
    WRITE(*,*)'Reading from "'//TRIM(in_file)//'".'
    OPEN(unit=io,file=TRIM(in_file),status='old',iostat=ierr)
    IF(ierr/=0)CALL errstop('PERFORM_FIT','Error opening "'//TRIM(in_file)//'".')
    CALL check_file(io,in_file,npoints,errorbars_present)
    WRITE(*,*)'Number of data points supplied: '//TRIM(i2s(npoints))
    IF(npoints<1)CALL errstop('PERFORM_FIT','More data needed.')
    IF(errorbars_present)THEN
      WRITE(*,*)'Error bars are supplied.'
    ELSE
      WRITE(*,*)'No error bars are supplied.'
    ENDIF ! errorbars_present
    ALLOCATE(x(npoints),y(npoints),rec_sigma_y_sq(npoints),stat=ialloc)
    IF(ialloc/=0)CALL errstop('PERFORM_FIT','Allocation error.')
    CALL read_file(io,in_file,errorbars_present)
    WRITE(*,*)
    CLOSE(io)

    ! Ask user to enter exponents for fitting polynomial.
    CALL get_exponents
    ALLOCATE(a(nparam),Minv(nparam,nparam),stat=ialloc)
    IF(ialloc/=0)CALL errstop('PERFORM_FIT','Allocation error (2).')
    CALL construct_poly(tempstr)
    WRITE(*,*)'Fitting polynomial is of form:'
    WRITE(*,*)TRIM(tempstr)
    WRITE(*,*)

    ! We solve Ma=c for vector of coefficients a (see my notes).
    IF(errorbars_present)THEN
      WRITE(*,*)'Performing chi-squared fit...'
    ELSE
      WRITE(*,*)'Performing least-squares fit...'
    ENDIF ! errorbars_present
    CALL calc_parameters(a,Minv,chi2)
    WRITE(*,*)'Done.'
    WRITE(*,*)

    ! Report chi^2 and rescale error bars if not present.
    IF(errorbars_present)THEN
      WRITE(*,*)'Chi^2 value: ',chi2
      if(npoints>nparam)then
        WRITE(*,*)
        WRITE(*,*)'Probability chi^2 is this bad or worse: ', &
          &gammq(DBLE(npoints-nparam)*0.5d0,chi2*0.5d0)
      endif ! npoints>nparam
    ELSE
      WRITE(*,*)'Least-squares function    : ',chi2
      WRITE(*,*)
      CALL rescale_errors(chi2,Minv)
      WRITE(*,*)'Assumed error bars on data: ',1.d0/SQRT(rec_sigma_y_sq(1))
    ENDIF ! errorbars_present
    WRITE(*,*)

    ! Write out fitted parameters with error bars.
    DO j=1,nparam
      indstr=ADJUSTL(TRIM(i2s(j)))
      WRITE(*,'(1x,a," = ",es24.16," +/- ",es24.16)')'k_'//indstr,a(j), &
        &SQRT(Minv(j,j))
    ENDDO ! j
    WRITE(*,*)

    ! Write out M^(-1).
    DO j=1,nparam
      DO k=j,nparam
        WRITE(*,*)'M^(-1)_'//TRIM(i2s(j))//','//TRIM(i2s(k))//' = ',Minv(j,k)
      ENDDO ! k
    ENDDO ! j
    WRITE(*,*)

    ! Write out fitted polynomial in a form that can be pasted into xmgrace.
    CALL construct_graphstr(a,tempstr)
    WRITE(*,*)'Fitted polynomial in y(x) form (for pasting into XMGrace):'
    WRITE(*,*)TRIM(tempstr)
    WRITE(*,*)

    DEALLOCATE(x,y,rec_sigma_y_sq,n_exp,a,Minv)

  END SUBROUTINE perform_fit


END MODULE linear_fit


PROGRAM gp_poly_fitter
  ! Main program starts here.
  USE linear_fit,ONLY : perform_fit
  IMPLICIT NONE

  WRITE(*,*)
  WRITE(*,*)'GP polynomial fitter'
  WRITE(*,*)'===================='
  WRITE(*,*)

  CALL perform_fit

  WRITE(*,*)'Program finished.'
  WRITE(*,*)

END PROGRAM gp_poly_fitter
