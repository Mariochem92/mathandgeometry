!this subroutine takes a number as input and returns its facto
       subroutine fact(o,fo)
        implicit none
        integer :: o,fo
        integer :: i, dif
                
        fo=o
          if (o>1) then
                do i=1,o
                dif=o-i
                !fattoriale di o
                    if (dif>0) then 
                    fo=fo*dif
                    end if
                end do
           else
                 fo=1
           end if
                
         end
