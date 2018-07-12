! towclearsens.dll
!   Dll to determine the distance between blade tip and tower (or any two points in space)
!
!  OBS: Remember to compile by changing Project Properties>Fortran>Libraries>Debug Multithread libs\static
!
!   INPUT HTC:
!  begin type2_dll;
!    name disttowtip ;
!    filename  ./control/towclearsens.dll ;
!    dll_subroutine_init initialize ;
!    dll_subroutine_update update ;
!    arraysizes_init  1 1 ;
!    arraysizes_update  12 4 ;
!	begin init ;
!	  constant  1  2.66  ; Tower radius close to downward blade tip [m] 
!	end init ;
!    begin output;
!	  mbdy state pos tower    3 0.75 global ; [1,2,3]. Tower position: 27.5 m
!	  mbdy state pos blade1  18 1.0 global  ; [4,5,6]
!	  mbdy state pos blade2  18 1.0 global  ; [7,8,9]
!	  mbdy state pos blade3  18 1.0 global  ; [10,11,12]
!    end output;           
!  end type2_dll;
!;
!
!   OUTPUT:
!   1.      Min distance blades tip - tower cylinder          [m]
!   2..4:   For each blade distance tip outside part of tower [m] 
!
    
! ------- Modules -------- !
MODULE ModEchoInit
	implicit none
    real(8)     :: offs_rtow = 0.d0               ! Offset tower radius. Simply subtracted from overall distance 
END MODULE ModEchoInit


! ----------------------------- Initialization SubRoutine ---------------------------------
! Called only once at the beginning. Used to pass constant values to variables in the module (globals...)
subroutine initialize(array1,array2)
	! -- Preliminary Declarations -- !
use ModEchoInit
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'initialize' :: initialize
	! -- Allocate variables -- !
! - i/o variables - !
real(8),dimension(1)	:: array1	! Input array, from hawc2 to dll
real(8),dimension(1)	:: array2	! Array .dll -> hawc2
    ! -- Main part -- !
offs_rtow = array1(1)
    ! . Pass it as output
array2(1) = offs_rtow
end subroutine initialize
! ------------------------------------------------------------------------------ !
! ----------------------------- Update SubRoutine -------------------------------!
! ------------------------------------------------------------------------------ !
subroutine update(array1,array2)
	! -- Preliminary Decalarations -- !
use ModEchoInit
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update' :: update
	! -- Allocate variables -- !
    ! - i/o variables - !
real(8),dimension(12)	:: array1	! Input array, from hawc2 to dll. Tower pos, bl1 pos, bl2 pos, bl3 pos
real(8),dimension(3)	:: array2	! Array .dll -> hawc2   ! Distance
    ! - Internal Variables, from array input - !
real(8), dimension(3)   :: pos_tow          ! Tower position in global coordinates [x,y,z]  [m]
real(8), dimension(9)   :: pos_bltip        ! Blade tip position in global coordinates, for each blade [x,y,z]  [m]    
    ! - Internal variables - !
real(8), dimension(3)   :: delta_pos        ! Difference of positions
real(8), dimension(3)   :: dist_pts         ! Distance tower blade ofr each blade

! --------- Initialize --------------- !
    ! -- Assign inputs from Array --!
pos_tow(1:3)   = array1(1:3)
pos_bltip(1:9) = array1(4:12)
    ! -- Compute distance between points -- !
delta_pos   = pos_bltip(1:3)-pos_tow ! Bl. 1
dist_pts(1) = DSQRT(delta_pos(1)**2+delta_pos(2)**2+delta_pos(3)**2)
delta_pos   = pos_bltip(4:6)-pos_tow ! Bl. 2
dist_pts(2) = DSQRT(delta_pos(1)**2+delta_pos(2)**2+delta_pos(3)**2)
delta_pos   = pos_bltip(7:9)-pos_tow ! Bl. 1
dist_pts(3) = DSQRT(delta_pos(1)**2+delta_pos(2)**2+delta_pos(3)**2)
    ! . Subtract tow offset    
dist_pts = dist_pts-offs_rtow
array2(1) = MIN(dist_pts(1),dist_pts(2),dist_pts(3))
array2(2:4) = dist_pts

! To consider Saftery factor: dsf = (d-d0)*sf + d0 (d0):undeflected distance
end subroutine update