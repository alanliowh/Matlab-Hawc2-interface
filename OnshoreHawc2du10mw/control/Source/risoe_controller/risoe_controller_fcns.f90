module risoe_controller_fcns
use misc_mod
  ! Constants
integer*4 maxwplines
parameter(maxwplines=100)
! Types
type Tpidvar
  real*8 Kpro,Kdif,Kint,outmin,outmax,velmax,error1,outset1,outres1
  integer*4 stepno1
  real*8 outset,outpro,outdif,error1_old,outset1_old,outres1_old,outres
end type Tpidvar
type Tpid2var
  real*8 Kpro(2),Kdif(2),Kint(2),outmin,outmax,velmax,error1(2),outset1,outres1
  integer*4 stepno1
  real*8 outset,outpro,outdif,error1_old(2),outset1_old,outres1_old,outres
end type Tpid2var
type Twpdata
  real*8 wpdata(maxwplines,2)
  integer*4 lines
end type Twpdata
! Variables
integer*4 stepno
integer*4::nblades=3
logical const_power
real*8 time_old,time_stop,overspeed_percent,overspeed_time,safety_overspeed_percent,RysteVagtLevel
real*8 :: EmergPitchStop=0.d0
real*8 :: RysteVagt=0.d0
integer*4 :: CtrlStatus = 0
real*8 :: deltat = 0.01d0
real*8 added_pitch_rate,Err0,ErrDot0,PitNonLin1
real*8 omega_ref_max,omega_ref_full,omega_ref_min,Pe_rated,Qg_rated,pitch_stopang,max_lss_torque
real*8 omega_ref,Qgen_ref0,theta_ref0
real*8::last_col_pitch_ref=0.d0
real*8::last_Qgen_ref=0.d0
real*8::Vcutout=25.d0
real*8::Vstorm=25.d0
real*8 Kopt,rel_sp_open_Qg
real*8::Kopt_dot=0.0d0
real*8 TSR_opt,R
integer*4::PartialLoadControlMode=1
real*8 Qgen_ref_old,theta_col_ref_old
real*8 invkk1,invkk2,rel_limit
real*8 :: invkk1_speed = 0.d0
real*8 :: invkk2_speed = 0.d0
real*8 :: kp_speed,kp_0 = 0.d0
real*8 switch1_pitang_lower,switch1_pitang_upper,switch1
real*8 switch1_pitang_lower_org,switch1_pitang_upper_org
real*8 DT_damp_gain
logical::generator_cutin=.true.
real*8 t_cutin,t_generator_cutin,t_cutin_delay,cutin_pitch_speed
integer*4 stoptype
real*8 t_cutout,pitch_stopdelay,pitch_stopdelay2,pitch_stopvelmax,pitch_stopvelmax2
real*8 torque_stopdelay,omega_at_stop,torque_at_stop
real*8::time_gen_cutout=0.d0
real*8 C_Tilt, C_yaw, C_Tilt_old, C_yaw_old, C_Tiltfilt, C_Yawfilt
real*8 :: AziAng = 0.d0
! Custom Types
type(Tlowpass2order) omega2ordervar
type(Tnotch2order) DT_mode_filt
type(Tnotch2order) pwr_DT_mode_filt
type(Tbandpassfilt) DT_damper_filt
type(Tpid2var) PID_pit_var
type(Tpidvar) PID_gen_var
type(Twpdata) OPdatavar
type(Tfirstordervar) wspfirstordervar
type(Tfirstordervar) pitchfirstordervar
type(Tfirstordervar) switchfirstordervar
type(Tfirstordervar) cutinfirstordervar
type(Tfirstordervar) rystevagtfirstordervar
!**************************************************************************************************
contains
!**************************************************************************************************
function switch_spline(x,x0,x1)
implicit none
! A function that goes from 0 at x0 to 1 at x1
real*8 switch_spline,x,x0,x1
if (x0.ge.x1) then
  if (x.lt.x0) then
    switch_spline=0.d0
  else
    switch_spline=1.d0
  endif
elseif (x0.gt.x1) then
  switch_spline=0.d0
else
  if (x.lt.x0) then
    switch_spline=0.d0
  elseif (x.gt.x1) then
    switch_spline=1.d0 
  else
    switch_spline=2.d0/(-x1+x0)**3*x**3+(-3.d0*x0-3.d0*x1)/(-x1+x0)**3*x**2&
                 +6.d0*x1*x0/(-x1+x0)**3*x+(x0-3.d0*x1)*x0**2/(-x1+x0)**3
  endif
endif
return
end function switch_spline
!**************************************************************************************************
function interpolate(x,x0,x1,f0,f1)
implicit none
real*8 interpolate,x,x0,x1,f0,f1
if (x0.eq.x1) then 
  interpolate=f0
else 
  interpolate=(x-x1)/(x0-x1)*f0+(x-x0)/(x1-x0)*f1
endif
return
end function interpolate
!**************************************************************************************************
function GetOptiPitch(wsp)
implicit none
real*8 GetOptiPitch,wsp
! local vars
real*8 x,x0,x1,f0,f1,pitch
integer*4 i
i=1
do while((OPdatavar.wpdata(i,1).le.wsp).and.(i.le.OPdatavar.lines))
  i=i+1
enddo
if (i.eq.1) then 
  GetOptiPitch=OPdatavar.wpdata(1,2)
elseif (i.gt.OPdatavar.lines) then
  GetOptiPitch=OPdatavar.wpdata(OPdatavar.lines,2)
else
  x=wsp
  x0=OPdatavar.wpdata(i-1,1)
  x1=OPdatavar.wpdata(i,1)
  f0=OPdatavar.wpdata(i-1,2)
  f1=OPdatavar.wpdata(i,2)
  Pitch=interpolate(x,x0,x1,f0,f1)
  GetOptiPitch=Pitch
endif
return
end function GetOptiPitch
!**************************************************************************************************
function PID(stepno,dt,kgain,PIDvar,error)
implicit none
integer*4 stepno              
real*8 PID,dt,kgain(3),error
type(Tpidvar) PIDvar
! Local vars
real*8 eps
parameter(eps=1.d-6)
! Initiate
if (stepno.eq.1) then
  PIDvar.outset1=0
  PIDvar.outres1=0
  PIDvar.error1=0
  PIDvar.error1_old=0.0
  PIDvar.outset1_old=0.0
  PIDvar.outres1_old=0.0
endif
! Save previous values
if (stepno.gt.PIDvar.stepno1) then
  PIDvar.outset1_old=PIDvar.outset1
  PIDvar.outres1_old=PIDvar.outres1
  PIDvar.error1_old=PIDvar.error1
endif
! Update the integral term
PIDvar.outset=PIDvar.outset1_old+0.5d0*(error+PIDvar.error1)*Kgain(2)*PIDvar.Kint*dt
! Update proportional term
PIDvar.outpro=Kgain(1)*PIDvar.Kpro*0.5d0*(error+PIDvar.error1)
! Update differential term
PIDvar.outdif=Kgain(3)*PIDvar.Kdif*(error-PIDvar.error1_old)/dt
! Sum to up
PIDvar.outres=PIDvar.outset+PIDvar.outpro+PIDvar.outdif
! Satisfy hard limits
if (PIDvar.outres.lt.PIDvar.outmin) then 
  PIDvar.outres=PIDvar.outmin
elseif (PIDvar.outres.gt.PIDvar.outmax) then 
  PIDvar.outres=PIDvar.outmax
endif
! Satisfy max velocity
if (PIDvar.velmax.gt.eps) then
    if ((abs(PIDvar.outres-PIDvar.outres1_old)/dt).gt.PIDvar.velmax) &
      PIDvar.outres=PIDvar.outres1_old+dsign(PIDvar.velmax*dt,PIDvar.outres-PIDvar.outres1_old)
endif
! Anti-windup on integral term and save results
PIDvar.outset1=PIDvar.outres-PIDvar.outpro-PIDvar.outdif
PIDvar.outres1=PIDvar.outres
PIDvar.error1=error
PIDvar.stepno1=stepno
! Set output
if (stepno.eq.0) then 
  PID=0
else 
  PID=PIDvar.outres
endif
return
end function PID
!**************************************************************************************************
function PID2(stepno,dt,kgain,PIDvar,error,added_term)
implicit none
integer*4 stepno              
real*8 PID2,dt,kgain(3,2),error(2),added_term
type(Tpid2var) PIDvar
! Local vars
real*8 eps
parameter(eps=1.d-6)
! Initiate
if (stepno.eq.1) then
  PIDvar.outset1=0
  PIDvar.outres1=0
  PIDvar.error1=0
  PIDvar.error1_old=0.0
  PIDvar.outset1_old=0.0
  PIDvar.outres1_old=0.0
endif
! Save previous values
if (stepno.gt.PIDvar.stepno1) then
  PIDvar.outset1_old=PIDvar.outset1
  PIDvar.outres1_old=PIDvar.outres1
  PIDvar.error1_old=PIDvar.error1
endif
! Update the integral term
PIDvar.outset=PIDvar.outset1_old+0.5d0*dt*(Kgain(2,1)*PIDvar.Kint(1)*(error(1)+PIDvar.error1(1))&
                                          +Kgain(2,2)*PIDvar.Kint(2)*(error(2)+PIDvar.error1(2)))
! Update proportional term
PIDvar.outpro=0.5d0*(Kgain(1,1)*PIDvar.Kpro(1)*(error(1)+PIDvar.error1(1))&
                    +Kgain(1,2)*PIDvar.Kpro(2)*(error(2)+PIDvar.error1(2)))
! Update differential term
PIDvar.outdif=(Kgain(3,1)*PIDvar.Kdif(1)*(error(1)-PIDvar.error1_old(1)))/dt + added_term*dt
! Sum up
PIDvar.outres=PIDvar.outset+PIDvar.outpro+PIDvar.outdif
! Satisfy hard limits
if (PIDvar.outres.lt.PIDvar.outmin) then 
  PIDvar.outres=PIDvar.outmin
elseif (PIDvar.outres.gt.PIDvar.outmax) then 
  PIDvar.outres=PIDvar.outmax
endif
! Write out values if the output is not-a-number
if (isnan(PID_pit_var.outres)) then
  write(*,*)  'PID_pit_var.outres=', PID_pit_var.outres
  write(*,*)  'PID_pit_var.outpro=', PID_pit_var.outpro
  write(*,*)  'PID_pit_var.outdif=', PID_pit_var.outdif
  write(*,*)  'dt=', dt
  write(*,*)  'PIDvar.error1_old(1)=', PIDvar.error1_old(1)
  write(*,*)  'error(1)=', error(1)
  write(*,*)  'PIDvar.Kdif(1)=', PIDvar.Kdif(1)
  write(*,*)  'Padded_term=', added_term
  write(*,*)  'PID_pit_var.outset=', PID_pit_var.outset
  pause
endif
! Satisfy max velocity
if (PIDvar.velmax.gt.eps) then
    if ((abs(PIDvar.outres-PIDvar.outres1_old)/dt).gt.PIDvar.velmax) &
      PIDvar.outres=PIDvar.outres1_old+dsign(PIDvar.velmax*dt,PIDvar.outres-PIDvar.outres1_old)
endif
! Anti-windup on integral term and save results
PIDvar.outset1=PIDvar.outres-PIDvar.outpro-PIDvar.outdif
PIDvar.outres1=PIDvar.outres
PIDvar.error1=error
PIDvar.stepno1=stepno
! Set output
if (stepno.eq.0) then 
  PID2=0
else 
  PID2=PIDvar.outres
endif
return
end function PID2
!**************************************************************************************************
end module risoe_controller_fcns