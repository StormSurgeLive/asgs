        program convert_numbers

C        REAL :: I,J,K
        INTEGER :: I,J,K
        INTEGER :: NumRecords, OutputFreq, RunStartTime
        REAL   :: RUNLENGTH, OutputFreqHr, RunStartTimeHr
       

C          $NumRecords $OutputFreq $RunStartTime
        open(10,file="numberstoconvert",status='old')

         read(10,*) NumRecords,OutputFreq,RunStartTime

        RunStartTimeHr=float(RunStartTime) / 3600.0d0
        OutputFreqHr= 3600.0d0 / float(OutputFreq)
        RUNLENGTH=float(NumRecords) /  OutputFreqHr
C         write(*,*) RunStartTime, OutputFreq, NumRecords, OutputFreqHr
C         write(*,*) 

 
         open(11,file="convertednumbers",status='unknown')

         write(11,8001) RunStartTimeHr,RUNLENGTH
8001     format(2f10.2)
         end program

