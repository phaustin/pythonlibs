subroutine brighttemp(wavelen,radiance,brightness,nvals)
USE type_kinds
USE planck_functions

INTEGER :: error_status
CHARACTER *100 buffer
double precision, INTENT(IN) :: wavelen
INTEGER, INTENT(IN) :: nvals
double precision, INTENT(IN) :: radiance(nvals)
double precision, INTENT(OUT) ::  brightness(nvals)

do i=1,nvals
! Call the function
   error_status = planck_temperature(WAVELENGTH_UNIT, wavelen, radiance(i), brightness(i))
end do

end subroutine brighttemp

subroutine modistemp(channelnum,radiance,brightness,nvals)
USE type_kinds
USE sensor_planck_functions

INTEGER :: error_status
INTEGER, INTENT(IN) :: channelnum
INTEGER, INTENT(IN) :: nvals
double precision, INTENT(IN) :: radiance(nvals)
double precision, INTENT(OUT) ::  brightness(nvals)
INTEGER :: sensorid

!modis id from sensor_planck_functions.f90

sensorid=44

do i=1,nvals
! Call the function
   error_status = sensor_temperature(WAVELENGTH_UNIT, sensorid, channelnum, radiance(i), brightness(i))
end do

end subroutine modistemp

subroutine getchannels(channels,chanlen,chanout)
USE type_kinds
USE sensor_planck_functions

INTEGER :: error_status, sensorid, numchans, i
INTEGER, INTENT(IN) :: CHANLEN
INTEGER, INTENT(OUT) :: chanout
INTEGER, DIMENSION(chanlen), INTENT(OUT) :: channels
INTEGER, DIMENSION(200) ::  DUMBCHAN
!modis id from sensor_planck_functions.f90

print *,"in fortran"
call flush(6)
sensorid=44
error_status = sensor_channels(sensorid, numchans, dumbchan)
chanout=numchans
do i=1,numchans
   channels(i)=dumbchan(i)
end do

end subroutine getchannels


subroutine getnumchans(numchans)
USE type_kinds
USE sensor_planck_functions

INTEGER :: error_status, sensorid
INTEGER, DIMENSION(200) :: channels
INTEGER, INTENT(out) :: numchans
!modis id from sensor_planck_functions.f90

sensorid=44

print *, "here 1"

error_status = sensor_channels(sensorid, numchans, channels)

print *, "here 2"
end subroutine getnumchans
