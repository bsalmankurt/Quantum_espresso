program cubic
  implicit none
  integer, parameter :: nmax = 10000
  integer :: k, j, toplam, a, b, c
  real    :: fermi
  real, dimension(nmax, nmax) :: freq
  integer, parameter :: unit_in = 5
  integer, parameter :: unit_single = 10
  integer, parameter :: unit_up     = 11
  integer, parameter :: unit_down   = 12

  ! Open the input file and read the data
  open(unit=unit_in, file="fe.bands", status="old", action="read")
  
  read(unit_in, *) fermi
  ! Skip header lines if necessary:
  read(unit_in, *)  ! Unnecessary line
  read(unit_in, *)  ! Unnecessary line
  read(unit_in, *) a, b, c
  toplam = a * b + 1

  ! Write to different output files based on the spin state
  if (b == 1) then
      open(unit=unit_single, file="band.dat", status="replace", action="write")
      do k = 1, c
          read(unit_in, *) (freq(k, j), j = 1, toplam)
          ! Offset according to the Fermi energy
          freq(k, 1) = freq(k, 1) + fermi
          ! Write all the data with reference to the Fermi level
          write(unit_single, '(F7.4, 1000F12.6)') (freq(k, j) - fermi, j = 1, toplam)
      end do
      close(unit_single)
  else
      open(unit=unit_up,   file="band_up.dat",   status="replace", action="write")
      open(unit=unit_down, file="band_down.dat", status="replace", action="write")
      do k = 1, c
          read(unit_in, *) (freq(k, j), j = 1, toplam)
          freq(k, 1) = freq(k, 1) + fermi
          ! For the spin-up part, write the first (a+1) values
          write(unit_up, '(F7.4, 1000F12.6)') (freq(k, j) - fermi, j = 1, a + 1)
          ! For the spin-down part, the value at the end of spin-up is repeated as the first value, and the remaining part is written
          freq(k, a + 1) = freq(k, 1)
          write(unit_down, '(F7.4, 1000F12.6)') (freq(k, j) - fermi, j = a + 1, toplam)
      end do
      close(unit_up)
      close(unit_down)
  end if
  
  close(unit_in)
  stop
end program cubic
