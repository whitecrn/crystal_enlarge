module JG
        implicit none
        type :: str_array
                character(len=:),allocatable :: str
        end type str_array
contains

subroutine get_colume(InStr,OutStr)
        implicit none
        character(len=*),intent(in) :: InStr
        type(str_array),allocatable :: OutStr(:)
        character(len=:),allocatable :: temp_Str
        Integer :: pos,length
        Integer :: n=0
        Integer :: i,j,k
        Integer,allocatable :: p(:)
        
        allocate(character(len=len(InStr)) :: temp_Str)
        if (len(trim(adjustl(InStr))) == 0) then
               write(*,*) "Instr is emerty"
               return
        else 
                n=1
        end if

        temp_Str=trim(adjustl(Instr))
        pos=index(adjustl(trim(temp_Str))," ")
        do while (pos /= 0)
                temp_Str=adjustl(temp_Str)
                temp_Str=adjustl(temp_Str(pos:len(temp_Str)))
                !write(*,*) temp_Str
                pos=index(adjustl(trim(temp_Str))," ")
                n=n+1
        end do

        allocate(OutStr(n))
        temp_Str=InStr
        do i=1,n
                pos=index(adjustl(trim(temp_Str))," ")
                allocate(character(len=2) :: OutStr(i)%str)
                temp_Str=adjustl(temp_Str)
                if (pos /= 0) then
                        OutStr(i)%str=temp_Str(1:pos-1)
                else 
                        OutStr(i)%str=temp_Str(1:len(trim(adjustl(temp_Str))))
                end if
                temp_Str=adjustl(trim(temp_Str(pos:len(InStr))))
        end do
        return
end subroutine get_colume
         


end module JG
