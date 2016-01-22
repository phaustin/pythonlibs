      subroutine thetawrap(press,temp,outtheta,np)
      integer i,np
      real*8 press(np),temp(np),outtheta(np)
      real*4 theta
cf2py intent(in) np
cf2py intent(in) press,temp
cf2py intent(out)outthetha
cf2py depend(np) press,temp,outtheta
      do i=1,np
         outtheta(i)=theta(real(press(i)),real(temp(i)))
      end do
      end
