program crystal_enlarge
        use JG
        implicit none
        type :: atom
                integer :: a_type
                real :: x(3)
        end type atom
        character(len=1024) :: n_x,n_y,n_z,ELE,sit,file1,file2
        integer :: n(3),m,atom_number
        integer :: i,j,k,l,ii
        integer,allocatable :: atom_n(:)
        type(atom),allocatable :: A(:,:,:,:)
        type(str_array),allocatable :: atom_type(:)
        real :: xx,yy,zz,xy,xz,yx,yz,zx,zy,factor,aa,bb,cc
!-------------------------------------------------------------------------------------
call get_command_argument(1,n_x)
call get_command_argument(2,n_y)
call get_command_argument(3,n_z)
call get_command_argument(4,file1)
call get_command_argument(5,file2)
read(n_x,*) n(1)
read(n_y,*) n(2)
read(n_z,*) n(3)
m=n(1)*n(2)*n(3)
!------------------------------------------------------------------------------------
open(unit=100,file=trim(adjustl(file1)),status='old',action='read')
open(unit=200,file=trim(adjustl(file2)),status='replace')
!------------------------------------------------------------------------------------
read(100,*)
read(100,*) factor
read(100,*) xx,xy,xz
read(100,*) yx,yy,yz
read(100,*) zx,zy,zz
read(100,"(A)") ELE
call get_colume(ELE,atom_type)
atom_number=size(atom_type)
allocate(atom_n(atom_number))
read(100,*) (atom_n(i),i=1,atom_number)
allocate(A(sum(atom_n),n(1),n(2),n(3)))
read(100,*) sit

do i=1,sum(atom_n)
        read(100,*) (A(i,1,1,1)%x(j),j=1,3)
end do
close(100)
!------------------------------------------------------------------------------------
if (trim(adjustl(sit)) == 'Cartesian') then
        do i=1,sum(atom_n)
                A(i,1,1,1)%x(1)=A(i,1,1,1)%x(1)*factor
                A(i,1,1,1)%x(2)=A(i,1,1,1)%x(2)*factor
                A(i,1,1,1)%x(3)=A(i,1,1,1)%x(3)*factor
        end do
else if (trim(adjustl(sit)) == 'Direct') then
        do i=1,sum(atom_n)
                aa=A(i,1,1,1)%x(1)
                bb=A(i,1,1,1)%x(2)
                cc=A(i,1,1,1)%x(3)
                A(i,1,1,1)%x(1)=factor*(aa*xx+bb*yx+cc*zx)
                A(i,1,1,1)%x(2)=factor*(aa*xy+bb*yy+cc*zy)
                A(i,1,1,1)%x(3)=factor*(aa*xz+bb*yz+cc*zz)
        end do
end if
do i=1,n(1)
        do j=1,n(2)
                do k=1,n(3)
                        do l=1,sum(atom_n)
                                A(l,i,j,k)%x(1)=A(l,1,1,1)%x(1)+(i-1)*xx+(j-1)*yx+(k-1)*zx
                                A(l,i,j,k)%x(2)=A(l,1,1,1)%x(2)+(j-1)*yy+(k-1)*zy
                                A(l,i,j,k)%x(3)=A(l,1,1,1)%x(3)+(k-1)*zz
                        end do
                end do
        end do
end do

xx=n(1)*xx*factor
yy=n(2)*yy*factor
zz=n(3)*zz*factor
yx=n(2)*yx*factor
zx=n(3)*zx*factor
zy=n(3)*zy*factor
!-----------------------------------------------------------------------------------
write(200,*) "POSCAR file written by whitecrn"
write(200,"(F4.2)") 1.0
write(200,"(F12.7,2X,F12.7,2X,F12.7)") xx,xy,xz
write(200,"(F12.7,2X,F12.7,2X,F12.7)") yx,yy,yz
write(200,"(F12.7,2X,F12.7,2X,F12.7)") zx,zy,zz
write(200,*) (atom_type(i)%str,"  ",i=1,atom_number)
write(200,*) (atom_n(i)*n(1)*n(2)*n(3),i=1,atom_number)
write(200,"(A9)") "Cartesian"

do l=1,sum(atom_n)
        do i=1,n(1)
                do j=1,n(2)
                        do k=1,n(3)
                                write(200,"(F15.8,1X,F15.8,1X,F15.8)") (A(l,i,j,k)%x(ii),ii=1,3)
                        end do
                end do
        end do
end do
close(200)
stop
end program

