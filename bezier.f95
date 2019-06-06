!given a data set of points stored in the matrix C(nx2), this returns the correspondent bezier fit
!the input g is only there to dynamically write a fortran output file. it shall be omitted if this subroutine is used out of loops 
!where matrixes are recursively fitted

                subroutine bezier(C,n,g)
                implicit none
                double precision :: C(n,2)
                double precision :: tt, a, b
                double precision :: frac
                integer :: n,g,i,k,t,no,o
                integer :: dif,ii, kk,somma
                integer :: fi, fo,fd,j,fin
                
                a=0
                b=0
                !number of fitting points of the polynomial
                fin=1000
                !order of the bezier plynomial
                o=8
                somma=o+1
                t=0
                k=0
                tt=dble(t)
                
                kk=-1
                do j=1,n/somma
                kk=kk+1
                do t=0,fin
                tt=dble(t)/dble(fin)
                k=0
                a=0
                b=0
                do ii=0,o
                k=k+1
                dif=o-ii
                call fact(ii,fi)
                !print *, fi
                call fact(o,fo)
                !print *, fo
                call fact(dif,fd)               
                frac=dble(dble(fo)/(dble(fi)*dble(fd)))
                !write(6,*) dble(fo),dble(fi),dble(fd)
                a=a+frac*tt**(ii)*(1-tt)**(dif)*C(k+kk*somma,1)
                b=b+frac*tt**(ii)*(1-tt)**(dif)*C(k+kk*somma,2)                               
                end do 
                write(100+g,*) a, b
                end do
                end do
                
                
                end
