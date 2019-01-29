      MODULE StringParser

      implicit none
      integer NumberStringParts
      integer, parameter :: MaxStringLength=132
      integer, parameter :: MaxStringTokens=100
      character(LEN=MaxStringLength), allocatable :: StringParts(:)

      END MODULE StringParser
