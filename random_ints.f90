MODULE random
!-------------------------------------------------!
! Routines for generating random numbers.         !
!-------------------------------------------------!
 IMPLICIT NONE
 PUBLIC ranint 
 CONTAINS
  SUBROUTINE ranint(maxint,randomnumber)
!-------------------------------------------------!
! Generate random integers between 0 and maxints. !
!-------------------------------------------------!
  IMPLICIT NONE
  INTEGER :: maxint
  INTEGER :: randomnumber
  REAL :: n
  call ran0to1(n)
  randomnumber=floor(n*maxint)
!  write(*,*) randomnumber  !*! won't work with this line commented.
  END SUBROUTINE ranint 

  SUBROUTINE ran0to1(randomnumber)
!-------------------------------------------------!
! Generate random numbers between 0 and 1.        !
!-------------------------------------------------!
  IMPLICIT NONE
  REAL :: randomnumber
  INTEGER, DIMENSION(20) :: seed
  call random_seed(put=seed)
  call random_number(randomnumber)
  END SUBROUTINE ran0to1
 
END MODULE random

PROGRAM random_ints
 USE random,ONLY: ranint
 IMPLICIT NONE
 INTEGER ::  maxints,randomnumber
 maxints=100 
 call ranint(maxints,randomnumber)
 randomnumber=randomnumber
 write(*,*) randomnumber 
END PROGRAM random_ints
