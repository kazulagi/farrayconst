


program main
    use iso_fortran_env
    use farrayconst
    implicit none

    integer(int32),allocatable :: a(:,:)
    integer(int32) :: a_ans(3,2)

    real(real64),allocatable :: b(:,:)
    real(real64) :: b_ans(3,2)
    

    ! try
    a =( [1, 2] .n. & 
         [2, 3] .n. &
         [3, 4] )
    
    !exact value
    a_ans(1,1:2) = [1,2]
    a_ans(2,1:2) = [2,3]
    a_ans(3,1:2) = [3,4]

    if( sum(abs(a-a_ans))==0)then
        print *, "[OK] int32 x int32 passed."
    else
        print *, "[ERROR] not passed."
    endif

    ! try
    b =( [1.0d0, 2.0d0] .n. & 
         [2.0d0, 3.0d0] .n. &
         [3.0d0, 4.0d0] )
    
    !exact value
    b_ans(1,1:2) = [1.0d0,2.0d0]
    b_ans(2,1:2) = [2.0d0,3.0d0]
    b_ans(3,1:2) = [3.0d0,4.0d0]

    if( sum(abs(b-b_ans))==0)then
        print *, "[OK] real64 x real64 passed."
    else
        print *, "[ERROR] not passed."
    endif


end program main