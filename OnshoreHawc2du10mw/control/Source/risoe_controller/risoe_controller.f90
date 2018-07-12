module risoe_controller_mod
use risoe_controller_fcns
implicit none
contains
!**************************************************************************************************
subroutine init_regulation(array1,array2)
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_regulation'::init_regulation
real*8 array1(1000),array2(1)
! Local vars
integer*4 i,ifejl
character text32*32
real*8 minimum_pitch_angle
logical findes
write(6,*) 'Basic DTU Wind Energy Controller ' //trim(adjustl(vertext32))// ' loaded...'
! Input array1 must contain
!       ; Overall parameters
!      constant   1 ; Rated power [kW]                         
!      constant   2 ; Minimum rotor speed [rad/s]
!      constant   3 ; Rated rotor speed [rad/s]
!      constant   4 ; Maximum allowable generator torque [Nm]
!      constant   5 ; Minimum pitch angle, theta_min [deg], 
!                   ; if |theta_min|>90, then a table of <wsp,theta_min> is read ;
!                   ; from a file named 'wptable.n', where n=int(theta_min)
!      constant   6 ; Maximum pitch angle [deg]
!      constant   7 ; Maximum pitch velocity operation [deg/s]
!      constant   8 ; Frequency of generator speed filter [Hz]
!      constant   9 ; Damping ratio of speed filter [-]
!      constant  10 ; Frequency of free-free DT torsion mode [Hz], if zero no notch filter used
!      ; Partial load control parameters
!      constant  11  ; Optimal Cp tracking K factor [Nm/(rad/s)^2], ;
!                    ; Qg=K*Omega^2, K=eta*0.5*rho*A*Cp_opt*R^3/lambda_opt^3                     
!      constant  12  ; Proportional gain of torque controller [Nm/(rad/s)]
!      constant  13  ; Integral gain of torque controller [Nm/rad]
!      constant  14  ; Differential gain of torque controller [Nm/(rad/s^2)]
!;     Full load control parameters
!      constant  15  ; Generator control switch [1=constant power, 2=constant torque]
!      constant  16  ; Proportional gain of pitch controller [rad/(rad/s)]
!      constant  17  ; Integral gain of pitch controller [rad/rad]
!      constant  18  ; Differential gain of pitch controller [rad/(rad/s^2)]
!      constant  19  ; Proportional power error gain [rad/W]
!      constant  20  ; Integral power error gain [rad/(Ws)]
!      constant  21  ; Coefficient of linear term in aerodynamic gain scheduling, KK1 [deg]
!      constant  22  ; Coefficient of quadratic term in aerodynamic gain scheduling, KK2 [deg^2] &
!                    ; (if zero, KK1 = pitch angle at double gain)
!      constant  23  ; Relative speed for double nonlinear gain [-]
!;     Cut-in simulation parameters
!      constant  24  ; Cut-in time [s]
!      constant  25  ; Time delay for soft start of torque [1/1P]
!;     Cut-out simulation parameters
!      constant  26  ; Cut-out time [s]
!      constant  27  ; Time constant for linear torque cut-out [s]
!      constant  28  ; Stop type [1=normal, 2=emergency]
!      constant  29  ; Time delay for pitch stop after shut-down signal [s]
!      constant  30  ; Maximum pitch velocity during initial period of stop [deg/s]
!      constant  31  ; Time period of initial pitch stop phase [s] (maintains pitch speed specified in constant 30)
!      constant  32  ; Maximum pitch velocity during final phase of stop [deg/s]
!;     Expert parameters (keep default values unless otherwise given)
!      constant  33  ; Lower angle above lowest minimum pitch angle for switch [deg]
!      constant  34  ; Upper angle above lowest minimum pitch angle for switch [deg], if equal then hard switch
!      constant  35  ; Ratio between filtered speed and reference speed for fully open torque limits [%]
!      constant  36  ; Time constant of 1st order filter on wind speed used for minimum pitch [1/1P]
!      constant  37  ; Time constant of 1st order filter on pitch angle used for gain scheduling [1/1P]
!;     Drivetrain damper
!      constant  38  ; Proportional gain of active DT damper [Nm/(rad/s)], requires frequency in input 10
!;	   Overspeed
!   	 constant  39  ; Overspeed percentage before initiating turbine controller alarm (shut-down) [%]
!;     Additional non-linear pitch control term (not used when all zero)
!	     constant  40  ; Err0 [rad/s] 
!	     constant  41  ; ErrDot0 [rad/s^2]
!	     constant  42  ; PitNonLin1 [rad/s]
!;     Storm control command
!	     constant 43   ; Wind speed 'Vstorm' above which derating of rotor speed is used [m/s]
!	     constant 44   ; Cut-out wind speed (only used for derating of rotor speed in storm) [m/s]	  
!;     Safety system parameters
!	     constant 45   ; Overspeed percentage before initiating safety system alarm (shut-down) [%]
!	     constant 46   ; Max low-pass filtered tower top acceleration level [m/s^2]
!;     Turbine parameter
!	     constant 47   ; Nominal rotor diameter [m]
!;     Parameters for rotor inertia reduction in variable speed region
!      constant 48   ; Proportional gain on rotor acceleration in variable speed region [Nm/(rad/s^2)] (not used when zero)
!;     Parameters for alternative partial load controller with PI regulated TSR tracking
!      constant 49   ; Optimal tip speed ratio [-] (only used when K=constant 11 = 0 otherwise  Qg=K*Omega^2 is used)
!;     Parameters for adding aerodynamic drivetrain damping on gain scheduling
!      constant 50   ; Proportional gain of aerodynamic DT damping [Nm/(rad/s)]
!      constant 51   ; Coefficient of linear term in aerodynamic DT damping scheduling, KK1 [deg]
!      constant 52   ; Coefficient of quadratic term in aerodynamic DT damping scheduling, KK2 [deg^2]
!
! Output array2 contains nothing for init
!
! Overall parameters
Pe_rated                 =array1( 1)*1.d3
omega_ref_min            =array1( 2)
omega_ref_max            =array1( 3)
max_lss_torque           =array1( 4)
minimum_pitch_angle      =array1( 5)*degrad
pitch_stopang            =array1( 6)*degrad
PID_pit_var.velmax       =array1( 7)*degrad
omega2ordervar.f0        =array1( 8)
omega2ordervar.zeta      =array1( 9)
DT_mode_filt.f0          =array1(10)
! Partial load control parameters
Kopt                     =array1(11)
PID_gen_var.Kpro         =array1(12)
PID_gen_var.Kint         =array1(13)
PID_gen_var.Kdif         =array1(14)
! Full load control parameters
const_power         =(int(array1(15)).eq.1)
PID_pit_var.kpro(1)      =array1(16)
kp_0 = array1(16)
PID_pit_var.kint(1)      =array1(17)
PID_pit_var.kdif(1)      =array1(18)
PID_pit_var.kpro(2)      =array1(19)
PID_pit_var.kint(2)      =array1(20)
PID_pit_var.kdif(2)      =0.d0
invkk1                   =1.d0/(array1(21)*degrad)
if (array1(22).eq.0.d0) then
  invkk2                 =0.d0
else
  invkk2                 =1.d0/(array1(22)*degrad*degrad)
endif
rel_limit                =array1(23)
! Cut-in simulation parameters
t_cutin                  =array1(24)
t_cutin_delay            =array1(25)*2.d0*pi/omega_ref_max
if (t_cutin.gt.0.d0) then
  CtrlStatus=-2
  generator_cutin=.false.
endif
! Cut-out simulation parameters
t_cutout                 =array1(26)
torque_stopdelay         =array1(27)
stoptype                 =int(array1(28))
pitch_stopdelay          =array1(29)
pitch_stopvelmax         =array1(30)*degrad
pitch_stopdelay2         =array1(31)
pitch_stopvelmax2        =array1(32)*degrad
! Expert parameters (keep default values unless otherwise given)
switch1_pitang_lower_org =array1(33)*degrad
switch1_pitang_upper_org =array1(34)*degrad
rel_sp_open_Qg           =array1(35)*1.d-2
wspfirstordervar.tau     =array1(36)*2.d0*pi/omega_ref_max
pitchfirstordervar.tau   =array1(37)*2.d0*pi/omega_ref_max
! Drivetrain damper
DT_damp_gain             =array1(38)
DT_damper_filt.f0        =DT_mode_filt.f0
pwr_DT_mode_filt.f0      =DT_mode_filt.f0
! Overspeed
overspeed_percent        =array1(39)*0.01d0
! Additional non-linear pitch control term
Err0                     = array1(40)
ErrDot0                  = array1(41)
PitNonLin1               = array1(42)
! Default and derived parameters
PID_gen_var.velmax=0.d0 !No limit to generator torque change rate
Qg_rated=Pe_rated/omega_ref_max
switchfirstordervar.tau=2.d0*pi/omega_ref_max
cutinfirstordervar.tau=2.d0*pi/omega_ref_max
rystevagtfirstordervar.tau=2.d0*pi/omega_ref_max
! Wind speed table
if (dabs(minimum_pitch_angle).lt.90.d0*degrad) then 
  Opdatavar.lines=2
  Opdatavar.wpdata(1,1)=0.d0
  Opdatavar.wpdata(2,1)=99.d0
  Opdatavar.wpdata(1,2)=minimum_pitch_angle
  Opdatavar.wpdata(2,2)=minimum_pitch_angle
else
  write(text32,'(i)') int(minimum_pitch_angle*raddeg)
  inquire(file='.\control\wpdata.'//trim(adjustl(text32)),exist=findes)      
  if (findes) then
    open(88,file='.\control\wpdata.'//trim(adjustl(text32)))
    read(88,*,iostat=ifejl) Opdatavar.lines
    if (ifejl.eq.0) then
      do i=1,Opdatavar.lines
        read(88,*,iostat=ifejl) Opdatavar.wpdata(i,1),Opdatavar.wpdata(i,2) 
        if (ifejl.ne.0) then
          write(6,*) ' *** ERROR *** Could not read lines in minimum '&
                   //'pitch table in file wpdata.'//trim(adjustl(text32))
          stop
        endif
        Opdatavar.wpdata(i,2)=Opdatavar.wpdata(i,2)*degrad
      enddo
    else
      write(6,*) ' *** ERROR *** Could not read number of lines '&
               //'in minimum pitch table in file wpdata.'//trim(adjustl(text32))
      stop
    endif
    close(88)
  else
    write(6,*) ' *** ERROR *** File ''wpdata.'//trim(adjustl(text32))&
             //''' does not exist in the .\control\ folder'
    stop
  endif
endif
! Storm controller input
Vstorm  = array1(43) ! [m/s] Vstorm (e.g. 25)
Vcutout = array1(44) ! [m/s] Vcut-out (e.g. 45)
if (Vcutout.gt.Vstorm) then
  write (6,'(a,f4.1,a,f4.1,a)') ' Storm control is active above ',Vstorm,'m/s until cut-out at ',Vcutout,'m/s'
endif
! Overspeed monitor
safety_overspeed_percent=array1(45)*0.01d0
! "Rystevagt" monitor
RysteVagtLevel=array1(46)
R=0.5d0*array1(47)
! Alternative partial load controller
Kopt_dot= array1(48)
TSR_opt = array1(49)
if (array1(11).eq.0.d0) then
  PartialLoadControlMode = 2
endif
! Gain scheduling dQdomega
kp_speed = array1(50)
if (array1(51).gt.0.d0) then
  invkk1_speed=1.d0/(array1(51)*degrad)
else
  invkk1_speed=0.d0
endif
if (array1(52).gt.0.d0) then
  invkk2_speed=1.d0/(array1(52)*degrad*degrad)
else
  invkk2_speed=0.d0
endif
! Initiate the dynamic variables
stepno=0
time_old=0.d0
added_pitch_rate = 0.d0
! No output
array2=0.d0
return
end subroutine init_regulation
!**************************************************************************************************
subroutine update_regulation(array1,array2)
implicit none
!DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_regulation'::update_regulation
real*8 array1(1000),array2(100)
! Input array1 must contain
!
!    1: general time                           ; [s]     
!    2: constraint bearing1 shaft_rot 1 only 2 ; [rad/s] Generator LSS speed 
!    3: constraint bearing2 pitch1 1 only 1    ; [rad]         
!    4: constraint bearing2 pitch2 1 only 1    ; [rad]                               
!    5: constraint bearing2 pitch3 1 only 1    ; [rad]                               
!  6-8: wind free_wind 1 0.0 0.0 hub height    ; [m/s] global coords at hub height
!    9: elec. power  ; [W]
!   10: grid flag  ; [1=no grid,0=grid]
!   11: Tower top x-acceleration  ; [m/s^2]
!   12: Tower top y-acceleration  ; [m/s^2]
!
! Output array2 contains
!
!    1: Generator torque reference            [Nm]
!    2: Pitch angle reference of blade 1      [rad]    
!    3: Pitch angle reference of blade 2      [rad]    
!    4: Pitch angle reference of blade 3      [rad]    
!    5: Power reference                       [W]
!    6: Filtered wind speed                   [m/s]
!    7: Filtered rotor speed                  [rad/s]
!    8: Filtered rotor speed error for torque [rad/s]
!    9: Bandpass filtered rotor speed         [rad/s]
!   10: Proportional term of torque contr.    [Nm]
!   11: Integral term of torque controller    [Nm]
!   12: Minimum limit of torque               [Nm]
!   13: Maximum limit of torque               [Nm]
!   14: Torque limit switch based on pitch    [-]
!   15: Filtered rotor speed error for pitch  [rad/s]
!   16: Power error for pitch                 [W]
!   17: Proportional term of pitch controller [rad]
!   18: Integral term of pitch controller     [rad]
!   19: Minimum limit of pitch                [rad]
!   20: Maximum limit of pitch                [rad]
!   21: Torque reference from DT damper       [Nm]
!   22: Status signal                         [-]
!   23: Total added pitch rate                [rad/s]
!   24: Filtered pitch angle                  [rad]
!   25: Flag for mechnical brake              [0=off/1=on]
!   26: Flag for emergency pitch stop         [0=off/1=on]
!   27: LP filtered acceleration level        [m/s^2]
!
! Local variables
integer*4 GridFlag
real*8 time,omega,omegafilt,domega_dt_filt,wsp,WSPfilt
real*8 omega_err_filt_pitch,omega_err_filt_speed,omega_dtfilt
real*8 added_term
real*8 ommin1,ommin2,ommax1,ommax2
real*8 pitang(3),meanpitang,meanpitangfilt,theta_min,e_pitch(2)
real*8 kgain_pitch(3,2),kgain_torque(3),aero_gain,x,dummy,y(2)
real*8 Qg_min_partial,Qg_max_partial,Qg_min_full,Qg_max_full
real*8 Qgen_ref,theta_col_ref,Qdamp_ref,Pe,AccLevel
real*8 :: active_mech_brake=0.d0
!**************************************************************************************************
! Increment time step (may actually not be necessary in type2 DLLs)
!**************************************************************************************************
time=array1(1)
if (time.gt.time_old) then
  deltat=time-time_old
  if (deltat .eq. 0.d0) deltat = 0.01d0
  time_old=time
  stepno=stepno+1
endif
!**************************************************************************************************
! Inputs and their filtering
!**************************************************************************************************
omega=array1(2)
! Mean pitch angle
pitang(1)=array1(3)
pitang(2)=array1(4)
pitang(3)=array1(5)
meanpitang=(pitang(1)+pitang(2)+pitang(3))/3.d0
! Wind speed as horizontal vector sum
wsp=dsqrt(array1(6)**2+array1(7)**2)
! Elec. power
Pe=array1(9)
! Grid flag
GridFlag=int(array1(10))
! Low-pass filtering of the rotor speed
y=lowpass2orderfilt(deltat,stepno,omega2ordervar,omega)
omegafilt=y(1)
domega_dt_filt=y(2)
! Low-pass filtering of the mean pitch angle for gain scheduling
meanpitangfilt=min(lowpass1orderfilt(deltat,stepno,pitchfirstordervar,meanpitang),30.d0*degrad)
! Low-pass filtering of the nacelle wind speed 
WSPfilt=lowpass1orderfilt(deltat,stepno,wspfirstordervar,wsp)
! Low-pass filtering of the nacelle wind speed 
AccLevel=lowpass1orderfilt(deltat,stepno,rystevagtfirstordervar,sqrt(array1(11)**2+array1(12)**2))
! Minimum pitch angle may vary with filtered wind speed
theta_min=GetOptiPitch(WSPfilt)
switch1_pitang_lower=switch1_pitang_lower_org+theta_min
switch1_pitang_upper=switch1_pitang_upper_org+theta_min
!**************************************************************************************************
! Start-up timer timer monitoring
!**************************************************************************************************
if ((t_cutin.gt.0.d0).and.(time.gt.t_cutin).and.(CtrlStatus.eq.-2)) then
  CtrlStatus=-1
endif
!**************************************************************************************************
! Overspeed monitoring
!**************************************************************************************************
if ((omegafilt.gt.(1.d0+overspeed_percent)*omega_ref_max).and.(CtrlStatus.eq.0)) then
  CtrlStatus = 1
  stoptype = 1
  omega_at_stop=omega
  torque_at_stop=last_Qgen_ref
  time_stop=time
endif
x=switch_spline(time,t_generator_cutin,t_generator_cutin+t_cutin_delay)
if ((omegafilt.gt.(1.d0+safety_overspeed_percent)*omega_ref_max).and.(x.gt.0.999d0)) then
  EmergPitchStop = 1.d0
endif
!**************************************************************************************************
! Grid monitoring
!**************************************************************************************************
if ((GridFlag.gt.0).and.(CtrlStatus.eq.0)) then
  CtrlStatus = 2
  stoptype = 1
  time_stop=time
endif
!**************************************************************************************************
! Acceleration monitoring
!**************************************************************************************************
if ((AccLevel.gt.RysteVagtLevel).and.(CtrlStatus.eq.0)) then
  CtrlStatus = 3
  stoptype = 1
  omega_at_stop=omega
  torque_at_stop=last_Qgen_ref
  time_stop=time
endif
!**************************************************************************************************
! Shut-down timer monitoring
!**************************************************************************************************
if ((t_cutout.gt.0.d0).and.(time.gt.t_cutout).and.(CtrlStatus.eq.0)) then
  if (stoptype.eq.1) CtrlStatus=4
  if (stoptype.eq.2) CtrlStatus=5
  omega_at_stop=omega
  torque_at_stop=last_Qgen_ref
  time_stop=time
endif
!**************************************************************************************************
! Reverse speed monitoring
!**************************************************************************************************
if ((omegafilt.lt.0.d0).and.(CtrlStatus.eq.0)) then
  CtrlStatus = 6
  stoptype = 1
  omega_at_stop=omega
  torque_at_stop=last_Qgen_ref
  time_stop=time
endif
!**************************************************************************************************
! Speed ref. changes max. <-> min. for torque controller and remains at rated for pitch controller
!**************************************************************************************************
select case (PartialLoadControlMode)
case (1)
  if (omegafilt.gt.0.5d0*(omega_ref_max+omega_ref_min)) then
    omega_ref=omega_ref_max
  else
    omega_ref=omega_ref_min
  endif
case (2)
  omega_ref = min(max(WSPfilt*TSR_opt/R,omega_ref_min),omega_ref_max)
end select
! Speed error
omega_err_filt_speed=omegafilt-omega_ref
!**************************************************************************************************
! Normal operation for control status CtrlStatus = 0
!**************************************************************************************************
if (CtrlStatus.eq.0) then
  !**************************************************************************************************
  ! PID regulation of generator torque 
  !**************************************************************************************************
  !-------------------------------------------------------------------------------------------------  
  ! Limit reference speed for storm control
  !-------------------------------------------------------------------------------------------------  
  if (Vcutout.gt.Vstorm) then
     omega_ref_full = omega_ref_max-dmax1(0.d0,(WSPfilt-Vstorm)/(Vcutout-Vstorm)*(omega_ref_max-omega_ref_min))
  else
     omega_ref_full = omega_ref_max
  endif
  omega_ref_full = max(min(omega_ref_full,omega_ref_max),omega_ref_min)
  !-------------------------------------------------------------------------------------------------  
  ! Limits for full load
  !-------------------------------------------------------------------------------------------------  
  if (const_power) then 
    Qg_min_full=min((Qg_rated*omega_ref_full)/max(omega,omega_ref_min),max_lss_torque)
    Qg_max_full=Qg_min_full
  else 
    Qg_min_full=Qg_rated
    Qg_max_full=Qg_min_full
  endif
  !-------------------------------------------------------------------------------------------------  
  ! Limits for partial load that opens in both ends
  !-------------------------------------------------------------------------------------------------  
  select case (PartialLoadControlMode)
    case (1)
      ommin1=omega_ref_min
      ommin2=omega_ref_min/rel_sp_open_Qg
      ommax1=(2.d0*rel_sp_open_Qg-1.d0)*omega_ref_max
      ommax2=rel_sp_open_Qg*omega_ref_max
      x=switch_spline(omegafilt,ommin1,ommin2)
      Qg_min_partial=min((Kopt*omegafilt**2-Kopt_dot*domega_dt_filt)*x,Kopt*ommax1**2)
      x=switch_spline(omegafilt,ommax1,ommax2)
      Qg_max_partial=max((Kopt*omegafilt**2-Kopt_dot*domega_dt_filt)*(1.d0-x)+Qg_max_full*x,Kopt*ommin2**2)
    case (2)
      Qg_min_partial=0
      Qg_max_partial=Qg_max_full
  end select
  ! Switch based on pitch
  switch1=switch_spline(meanpitang,switch1_pitang_lower,switch1_pitang_upper)
  switch1=lowpass1orderfilt(deltat,stepno,switchfirstordervar,switch1)
  ! Interpolation between partial and full load torque limits based on switch 1
  PID_gen_var.outmin=(1.d0-switch1)*Qg_min_partial+switch1*Qg_min_full
  PID_gen_var.outmax=(1.d0-switch1)*Qg_max_partial+switch1*Qg_max_full
  if (PID_gen_var.outmin.gt.PID_gen_var.outmax) PID_gen_var.outmin=PID_gen_var.outmax
  !-------------------------------------------------------------------------------------------------  
  ! Compute PID feedback to generator torque demand
  !-------------------------------------------------------------------------------------------------  
  kgain_torque=1.d0
  Qgen_ref=PID(stepno,deltat,kgain_torque,PID_gen_var,omega_err_filt_speed)
  !-------------------------------------------------------------------------------------------------
  ! Active DT damping based on notch filtered of rotor speed
  !-------------------------------------------------------------------------------------------------
  if ((DT_damp_gain.ne.0.d0).and.(DT_damper_filt.f0.gt.0.d0)) then
    omega_dtfilt=bandpassfilt(deltat,stepno,DT_damper_filt,omega)
    if (t_cutin.gt.0.d0) then
      if (generator_cutin) then
        x=switch_spline(time,t_generator_cutin+t_cutin_delay,t_generator_cutin+2.d0*t_cutin_delay)
        Qdamp_ref=DT_damp_gain*omega_dtfilt*x
        Qgen_ref=min(max(Qgen_ref+Qdamp_ref,0.d0),max_lss_torque)
      endif
    else
      Qdamp_ref=DT_damp_gain*omega_dtfilt
      Qgen_ref=min(max(Qgen_ref+Qdamp_ref,0.d0),max_lss_torque)
    endif
  endif
  !**************************************************************************************************
  ! PID regulation of collective pitch angle  
  !**************************************************************************************************
  ! Reference speed is equal rated speed
  omega_err_filt_pitch=omegafilt-omega_ref_full
  ! Additional non-linear pitch control term
  if ((PitNonLin1.gt.0.d0).and.(Err0.gt.0.d0).and.(ErrDot0.gt.0.d0)) then
    added_term = omega_err_filt_pitch/Err0 + domega_dt_filt/ErrDot0
    if (added_term.gt.1.0d0) then
      added_pitch_rate = PitNonLin1*added_term + added_pitch_rate
    endif
  endif
  ! Limits
  PID_pit_var.outmin=theta_min
  PID_pit_var.outmax=pitch_stopang
  ! Aerodynamic gain scheduling dQ/dtheta
  aero_gain=1.d0+meanpitangfilt*invkk1+meanpitangfilt**2*invkk2
  kgain_pitch = 1.d0/aero_gain
  ! Nonlinear gain to avoid large rotor speed excursion
  if (rel_limit.ne.0.d0) then
    kgain_pitch=kgain_pitch*(omega_err_filt_pitch**2/(omega_ref_full*(rel_limit-1.d0))**2+1.d0)
  endif
  ! Gainscheduling according to dQaero/dOmega
  PID_pit_var.kpro(1) = kp_0
  if (time.gt.5.d0) then
    PID_pit_var.kpro(1) = PID_pit_var.kpro(1) + kp_speed*(1.d0+meanpitangfilt*invkk1_speed+meanpitangfilt**2*invkk2_speed)
  endif
  !-------------------------------------------------------------------------------------------------
  ! Compute PID feedback to pitch demand
  !-------------------------------------------------------------------------------------------------
  if (DT_mode_filt.f0.gt.0.d0) then
    e_pitch(1)=notch2orderfilt(deltat,stepno,DT_mode_filt,omega_err_filt_pitch)
    e_pitch(2)=notch2orderfilt(deltat,stepno,pwr_DT_mode_filt,Pe-Pe_rated)
  else
    e_pitch(1)=omega_err_filt_pitch
    e_pitch(2)=Pe-Pe_rated
  endif
  theta_col_ref=PID2(stepno,deltat,kgain_pitch,PID_pit_var,e_pitch,added_pitch_rate)
endif
!**************************************************************************************************
! Cut-in procedure for control status CtrlStatus = -1
!**************************************************************************************************
if (CtrlStatus.lt.0) then
  if (CtrlStatus.eq.-2) then ! First pitch out
    theta_col_ref=min(pitch_stopang,last_col_pitch_ref+deltat*pitch_stopvelmax)
    Qgen_ref=0.d0
  else
    ! Set point for the pitch feedback is 10% above the minimum speed
    e_pitch(1)=omegafilt-omega_ref_max
    e_pitch(2)=0.d0
    if ((omega+domega_dt_filt*t_cutin_delay.lt.omega_ref_min).and.(.not.generator_cutin)) then
      ! Track AOA with pitch reference
      theta_col_ref=min(last_col_pitch_ref,atan2(WSPfilt,0.75d0*R*omegafilt)-6.d0*degrad)
      ! Remember pitch reference for transition
      theta_ref0=theta_col_ref
      ! Wind-up integral term of PID2 controller
      PID_pit_var.outmin=theta_col_ref
      dummy=PID2(stepno,deltat,kgain_pitch,PID_pit_var,e_pitch,added_pitch_rate)
      ! Generator is still cut-out
      Qgen_ref=0.d0
      ! Remember the generator cut-in time
      t_generator_cutin=time
      ! Set the initial generator torque to K
      Qgen_ref0=min(theta_col_ref*raddeg/10.d0*Kopt*omegafilt**2,Qg_rated)
      ! Windup the integral term of PID controller
      PID_gen_var.outmin=Qgen_ref
      kgain_torque=1.d0
      dummy=PID(stepno,deltat,kgain_torque,PID_gen_var,omega_err_filt_speed)
    elseif (time-t_generator_cutin.lt.t_cutin_delay) then
      ! Generator is cut-in
      generator_cutin=.true.
      ! Gradually set the minimum pitch angle to optimal pitch
      PID_pit_var.outmin=theta_ref0+(theta_min-0.5d0*theta_ref0)*(time-t_generator_cutin)/t_cutin_delay
      ! Let pitch PID2 control the speed while the torque is increased
      theta_col_ref=PID2(stepno,deltat,kgain_pitch,PID_pit_var,e_pitch,added_pitch_rate)
      ! Linearily increase the torque reference
      Qgen_ref=min(Qgen_ref0,Qgen_ref0*(time-t_generator_cutin)/t_cutin_delay)
      ! Start up the switch filter
      switch1=lowpass1orderfilt(deltat,stepno,switchfirstordervar,0.d0)
      ! Windup the integral term of PID controller
      PID_gen_var.outmin=Qgen_ref
      kgain_torque=1.d0
      dummy=PID(stepno,deltat,kgain_torque,PID_gen_var,omega_err_filt_speed)
    else
      CtrlStatus=0
    endif
  endif
endif
!**************************************************************************************************
! Cut-out procedure for control status CtrlStatus > 0
!**************************************************************************************************
if (CtrlStatus.gt.0) then
! Generator settings
  select case(CtrlStatus)
    case(1,3,4)
      if (omega.gt.omega_at_stop*0.8d0) then
        Qgen_ref=torque_at_stop
      else
        if (time_gen_cutout.lt.time_stop) then
          time_gen_cutout=time
        endif
        Qgen_ref=max(0.d0,torque_at_stop*(1.d0-(time-time_gen_cutout)/torque_stopdelay))
      endif
    case(2,5,6)
      Qgen_ref=0.d0
  end select
! Pitch seetings
  select case(CtrlStatus)
    case(1,3,4) ! Two pitch speed stop 
      if  (time.gt.time_stop+pitch_stopdelay+pitch_stopdelay2) then 
        theta_col_ref=min(pitch_stopang,last_col_pitch_ref+deltat*pitch_stopvelmax2)
      elseif (time.gt.time_stop+pitch_stopdelay) then
        theta_col_ref=min(pitch_stopang,last_col_pitch_ref+deltat*pitch_stopvelmax)
      elseif (time.gt.time_stop) then
        theta_col_ref=last_col_pitch_ref
      endif
    case(2,5,6) ! Pitch out a maximum speed
      theta_col_ref=min(pitch_stopang,last_col_pitch_ref+deltat*max(pitch_stopvelmax,pitch_stopvelmax2))
  end select
! Brake settings
  select case(stoptype)
    case(1) ! Normal 
      active_mech_brake=0.d0
    case(2) ! Emergency
      active_mech_brake=1.d0
    case default    
      write(6,'(a,i2,a)') 'ERROR regulation DLL, Stop type ',stoptype,' unknown'
      stop
  end select
endif
!**************************************************************************************************
! Limit changes and save reference signals
!**************************************************************************************************
if ((abs(theta_col_ref-last_col_pitch_ref)/deltat).gt.max(pitch_stopvelmax,pitch_stopvelmax2)) &
      theta_col_ref=last_col_pitch_ref+dsign(max(pitch_stopvelmax,pitch_stopvelmax2)*deltat,theta_col_ref-last_col_pitch_ref)
last_col_pitch_ref=theta_col_ref
last_Qgen_ref=Qgen_ref
!**************************************************************************************************
! Output
!**************************************************************************************************
array2( 1)=Qgen_ref             !    1: Generator torque reference            [Nm]  
array2( 2)=theta_col_ref        !    2: Pitch angle reference of blade 1      [rad]    
array2( 3)=theta_col_ref        !    3: Pitch angle reference of blade 2      [rad]    
array2( 4)=theta_col_ref        !    4: Pitch angle reference of blade 3      [rad]   
array2( 5)=Qgen_ref*omega       !    5: Power reference                       [W] 
array2( 6)=WSPfilt              !    6: Filtered wind speed                   [m/s] 
array2( 7)=omegafilt            !    7: Filtered rotor speed                  [rad/s] 
array2( 8)=omega_err_filt_speed !    8: Filtered rotor speed error for torque [rad/s]   
array2( 9)=omega_dtfilt         !    9: Bandpass filtered rotor speed         [rad/s] 
array2(10)=PID_gen_var.outpro   !   10: Proportional term of torque contr.    [Nm]  
array2(11)=PID_gen_var.outset   !   11: Integral term of torque controller    [Nm]  
array2(12)=PID_gen_var.outmin   !   12: Minimum limit of torque               [Nm]  
array2(13)=PID_gen_var.outmax   !   13: Maximum limit of torque               [Nm]  
array2(14)=switch1              !   14: Torque limit switch based on pitch    [-]  
array2(15)=omega_err_filt_pitch !   15: Filtered rotor speed error for pitch  [rad/s]  
array2(16)=e_pitch(2)           !   16: Power error for pitch                 [W]
array2(17)=PID_pit_var.outpro   !   17: Proportional term of pitch controller [rad]
array2(18)=PID_pit_var.outset   !   18: Integral term of pitch controller     [rad]
array2(19)=PID_pit_var.outmin   !   19: Minimum limit of pitch                [rad]
array2(20)=PID_pit_var.outmax   !   20: Maximum limit of pitch                [rad]
array2(21)=Qdamp_ref            !   21: Torque reference from DT damper       [Nm]
array2(22)=float(CtrlStatus)    !   22: Status signal                         [-]
array2(23)=added_pitch_rate     !   23: Total added pitch rate                [rad/s]
array2(24)=meanpitangfilt       !   24: Filtered pitch angle                  [rad]
array2(25)=active_mech_brake    !   25: Flag for mechnical brake              [0=off/1=on]
array2(26)=EmergPitchStop       !   26: Flag for emergency pitch stop         [0=off/1=on]
array2(27)=AccLevel             !   27: LP filtered acceleration level        [m/s^2]
return
end subroutine update_regulation
!**************************************************************************************************
end module risoe_controller_mod