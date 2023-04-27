

program main
    use iso_fortran_env
    use farrayconst
    implicit none

    ! allocatable array
    integer(int32),allocatable :: a(:,:)

    ! iterator
    integer(int32) :: i

    ! create a 2 x 3 matrix 
    ! [[1 2]
    !  [2 3]
    !  [3 4]]
 
    a =( [1, 2] .n. & 
         [2, 3] .n. &
         [3, 4] )

    ! show result
    do i=1,size(a,1)
        print *, a(i,:)
    enddo
    
end program