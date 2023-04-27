module farrayconst
    use iso_fortran_env
    implicit none
    interface operator(.n.)
        module procedure array_const_matvec_int32,array_const_int32,&
            array_const_matvec_real64,array_const_real64
    end interface 
contains

function array_const_int32(vec1, vec2) result(ret)
    integer(int32),intent(in) :: vec1(:), vec2(:)
    integer(int32),allocatable:: ret(:,:)

    allocate(ret(2,maxval( [size(vec1),size(vec2)] ) ) )

    ret(1,1:size(vec1) ) = vec1(:)
    ret(2,1:size(vec2) ) = vec2(:)

end function


function array_const_matvec_int32(mat1, vec2) result(ret)
    integer(int32),intent(in) :: mat1(:,:), vec2(:)
    integer(int32),allocatable:: ret(:,:)

    allocate(ret(size(mat1,1)+1,maxval( [size(mat1,2),size(vec2)] ) ) )

    ret(1:size(mat1,1),1:size(mat1,2) ) = mat1(:,:)
    ret(size(mat1,1)+1,1:size(vec2) ) = vec2(:)

end function



function array_const_real64(vec1, vec2) result(ret)
    real(real64),intent(in) :: vec1(:), vec2(:)
    real(real64),allocatable:: ret(:,:)

    allocate(ret(2,maxval( [size(vec1),size(vec2)] ) ) )

    ret(1,1:size(vec1) ) = vec1(:)
    ret(2,1:size(vec2) ) = vec2(:)

end function


function array_const_matvec_real64(mat1, vec2) result(ret)
    real(real64),intent(in) :: mat1(:,:), vec2(:)
    real(real64),allocatable:: ret(:,:)

    allocate(ret(size(mat1,1)+1,maxval( [size(mat1,2),size(vec2)] ) ) )

    ret(1:size(mat1,1),1:size(mat1,2) ) = mat1(:,:)
    ret(size(mat1,1)+1,1:size(vec2) ) = vec2(:)

end function


end module