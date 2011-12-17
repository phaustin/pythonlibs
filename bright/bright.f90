subroutine planckwave(wavelen,temperature,radiance,nvals)
USE planck_functions
real(kind=8), INTENT(IN) :: wavelen(nvals)
real(kind=8), INTENT(IN) :: temperature
INTEGER(kind=4), INTENT(IN) :: nvals
real(kind=8), INTENT(out) :: radiance(nvals)
do i=1,nvals
! Call the function
!   print *,'debug, radiance',i,radiance(i)
   error_status = planck_radiance(WAVELENGTH_UNIT, wavelen(i),temperature,radiance(i))
   !print *,'radiance: ',radiance(i),brightness(i)
end do
end subroutine planckwave

subroutine planckfreq(wavenum,temperature,radiance,nvals)
USE planck_functions
real(kind=8), INTENT(IN) :: wavenum(nvals)
real(kind=8), INTENT(IN) :: temperature
INTEGER(kind=4), INTENT(IN) :: nvals
real(kind=8), INTENT(out) :: radiance(nvals)
do i=1,nvals
! Call the function
!   print *,'debug, radiance',i,radiance(i)
   error_status = planck_radiance(WAVENUMBER_UNIT, wavenum(i),temperature,radiance(i))
   !print *,'radiance: ',radiance(i),brightness(i)
end do
end subroutine planckfreq



subroutine brighttemp(wavelen,radiance,brightness,nvals)
USE type_kinds
USE planck_functions

INTEGER :: error_status
real(kind=8), INTENT(IN) :: wavelen
INTEGER(kind=4), INTENT(IN) :: nvals
real(kind=8), INTENT(IN) :: radiance(nvals)
real(kind=8), INTENT(OUT) ::  brightness(nvals)

print *,'debug in bright.f90: ',wavelen,nvals
do i=1,nvals
! Call the function
!   print *,'debug, radiance',i,radiance(i)
   error_status = planck_temperature(WAVELENGTH_UNIT, wavelen, radiance(i), brightness(i))
   !print *,'radiance: ',radiance(i),brightness(i)
end do

end subroutine brighttemp


subroutine brighttempfreq(wavenum,radiance,brightness,nvals)
USE planck_functions

INTEGER :: error_status
real(kind=8), INTENT(IN) :: wavenum
INTEGER(kind=4), INTENT(IN) :: nvals
real(kind=8), INTENT(IN) :: radiance(nvals)
real(kind=8), INTENT(OUT) ::  brightness(nvals)

do i=1,nvals
! Call the function
!   print *,'debug, radiance',i,radiance(i)
   error_status = planck_temperature(WAVENUMBER_UNIT, wavenum, radiance(i), brightness(i))
   !print *,'radiance: ',radiance(i),brightness(i)
end do

end subroutine brighttempfreq



subroutine modistemp(channelnum,radiance,brightness,nvals)
USE type_kinds
USE sensor_planck_functions

INTEGER(kind=4) :: error_status
INTEGER(kind=4), INTENT(IN) :: channelnum
INTEGER(kind=4), INTENT(IN) :: nvals
real(kind=8), INTENT(IN) :: radiance(nvals)
real(kind=8), INTENT(OUT) ::  brightness(nvals)
INTEGER(kind=4) :: sensorid

!modis id from sensor_planck_functions.f90
!pass negative numbers as missing values

sensorid=44

do i=1,nvals
! Call the function
   if(radiance(i) .lt. 0.) then
      brightness(i)=radiance(i)
   else    
      error_status = sensor_temperature(WAVELENGTH_UNIT, sensorid, channelnum, radiance(i), brightness(i))
   endif
end do

end subroutine modistemp


subroutine modisradiance(channelnum,brightness,radiance,nvals)
USE type_kinds
USE sensor_planck_functions

INTEGER(kind=4) :: error_status
INTEGER(kind=4), INTENT(IN) :: channelnum
INTEGER(kind=4), INTENT(IN) :: nvals
real(kind=8), INTENT(in) ::  brightness(nvals)
real(kind=8), INTENT(out) :: radiance(nvals)
INTEGER(kind=4) :: sensorid

!modis id from sensor_planck_functions.f90

sensorid=44

do i=1,nvals
! Call the function
   error_status = sensor_radiance(WAVELENGTH_UNIT, sensorid, channelnum, brightness(i),radiance(i))
end do

end subroutine modisradiance


subroutine getchannels(channels,numchans,chanlen)
USE type_kinds
USE sensor_planck_functions
!need this for nagf95
!use f90_unix_io,only:flush
integer(kind=4), INTENT(IN) :: chanlen
integer(kind=4), INTENT(OUT) :: numchans
integer(kind=4) :: sensorid
INTEGER(kind=4), DIMENSION(chanlen), INTENT(OUT) :: channels
sensorid=44
error_status = sensor_channels(sensorid, numchans, channels)
end subroutine getchannels


subroutine getnumchans(numchans)
USE type_kinds
USE sensor_planck_functions

INTEGER(kind=4) :: error_status, sensorid
INTEGER(kind=4), DIMENSION(200) :: channels
INTEGER(kind=4), INTENT(out) :: numchans
!modis id from sensor_planck_functions.f90

sensorid=44
error_status = sensor_channels(sensorid, numchans, channels)
end subroutine getnumchans
