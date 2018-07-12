module misc_mod
! Version number
character*32::vertext32='(ver. 1.5)'
! log file
! version 1.1 - Added variable generator efficiency as a function of generator speed
! version 1.2 - Added variable generator efficiency as a function of generator speed, torque and power
! version 1.3 - Added Storm control "init_regulation_2"
! version 1.4 - Added new peturbation open-loop procedure
! version 1.5 - Added new cut-in and cut-out procedures
!               United with the storm control, TSR tracking, and aero damping in gain scheduling 
!               Removed new peturbation open-loop procedure (now a separate DLL)
!
! Constants
real*8 pi,degrad,raddeg
parameter(pi=3.14159265358979,degrad=0.0174532925,raddeg=57.2957795131)
! Types
type Tfirstordervar
  real*8 tau,x1,x1_old,y1,y1_old
  integer*4 stepno1
end type Tfirstordervar
type Tlowpass2order
  real*8 zeta,f0,x1,x2,x1_old,x2_old,y1,y2,y1_old,y2_old
  integer*4 stepno1
end type Tlowpass2order
type Tnotch2order
  real*8::zeta1=0.1
  real*8::zeta2=0.001
  real*8 f0,x1,x2,x1_old,x2_old,y1,y2,y1_old,y2_old
  integer*4 stepno1
end type Tnotch2order
type Tbandpassfilt
  real*8::zeta=0.02
  real*8::tau=0.0
  real*8 f0,x1,x2,x1_old,x2_old,y1,y2,y1_old,y2_old
  integer*4 stepno1
end type Tbandpassfilt
contains
!**************************************************************************************************
function lowpass1orderfilt(dt,stepno,filt,x)
implicit none
integer*4 stepno
real*8 lowpass1orderfilt,dt,x,y,a1,b1,b0,tau
type(Tfirstordervar) filt
! Step 
if ((stepno.eq.1).and.(stepno.gt.filt.stepno1)) then
  filt.x1_old=x
  filt.y1_old=x
  y=x
else
  if (stepno.gt.filt.stepno1) then
    filt.x1_old=filt.x1
    filt.y1_old=filt.y1
  endif
  tau=filt.tau
  a1 = (2 * tau - dt) / (2 * tau + dt)
  b0 = dt / (2 * tau + dt)
  b1 = b0
  y=a1*filt.y1_old+b0*x+b1*filt.x1_old
endif
! Save previous values
filt.x1=x
filt.y1=y
filt.stepno1=stepno
! Output
lowpass1orderfilt=y
return
end function lowpass1orderfilt
!**************************************************************************************************
function lowpass2orderfilt(dt,stepno,filt,x)
implicit none
real*8 lowpass2orderfilt(2),dt,x
integer*4 stepno
type(Tlowpass2order) filt
! local vars
real*8 y,f0,zeta,a1,a2,b0,b1,b2,denom
! Step
if ((stepno.eq.1).and.(stepno.gt.filt.stepno1)) then
  filt.x1=x
  filt.x2=x
  filt.x1_old=filt.x1
  filt.x2_old=filt.x2
  filt.y1=x
  filt.y2=x
  filt.y1_old=filt.y1
  filt.y2_old=filt.y2
  y=x
else
  if (stepno.gt.filt.stepno1) then
    filt.x1_old=filt.x1
    filt.x2_old=filt.x2
    filt.y1_old=filt.y1
    filt.y2_old=filt.y2
  endif
  f0=filt.f0
  zeta=filt.zeta
  denom=3.d0+6.d0*zeta*pi*f0*dt+4.d0*pi**2*f0**2*dt**2
  a1=(6.d0-4.d0*pi**2*f0**2*dt**2)/denom
  a2=(-3.d0+6.d0*zeta*pi*f0*dt-4.d0*pi**2*f0**2*dt**2)/denom
  b0=4.d0*pi**2*f0**2*dt**2/denom
  b1=b0
  b2=b0
  y=a1*filt.y1_old+a2*filt.y2_old+b0*x+b1*filt.x1_old+b2*filt.x2_old
endif
! Save previous values
filt.x2=filt.x1
filt.x1=x
filt.y2=filt.y1
filt.y1=y
filt.stepno1=stepno
! Output
lowpass2orderfilt(1)=y
lowpass2orderfilt(2)=(y-filt.y2_old)/dt
return
end function lowpass2orderfilt
!**************************************************************************************************
function notch2orderfilt(dt,stepno,filt,x)
implicit none
real*8 notch2orderfilt,dt,x
integer*4 stepno
type(Tnotch2order) filt
! local vars
real*8 y,f0,zeta1,zeta2,a1,a2,b0,b1,b2,denom
! Step
if ((stepno.eq.1).and.(stepno.gt.filt.stepno1)) then
  filt.x1=x
  filt.x2=x
  filt.x1_old=filt.x1
  filt.x2_old=filt.x2
  filt.y1=x
  filt.y2=x
  filt.y1_old=filt.y1
  filt.y2_old=filt.y2
  y=x
else
  if (stepno.gt.filt.stepno1) then
    filt.x1_old=filt.x1
    filt.x2_old=filt.x2
    filt.y1_old=filt.y1
    filt.y2_old=filt.y2
  endif
  f0=filt.f0
  zeta1=filt.zeta1
  zeta2=filt.zeta2
  denom=3.d0+6.d0*zeta1*pi*f0*dt+4.d0*pi**2*f0**2*dt**2
  a1=(6.d0-4.d0*pi**2*f0**2*dt**2)/denom
  a2=(-3.d0+6.d0*zeta1*pi*f0*dt-4.d0*pi**2*f0**2*dt**2)/denom
  b0=(3.d0+6.d0*zeta2*pi*f0*dt+4.d0*pi**2*f0**2*dt**2)/denom
  b1=(-6.d0+4.d0*pi**2*f0**2*dt**2)/denom
  b2=(3.d0-6.d0*zeta2*pi*f0*dt+4.d0*pi**2*f0**2*dt**2)/denom
  y=a1*filt.y1_old+a2*filt.y2_old+b0*x+b1*filt.x1_old+b2*filt.x2_old
endif
! Save previous values
filt.x2=filt.x1
filt.x1=x
filt.y2=filt.y1
filt.y1=y
filt.stepno1=stepno
! Output
notch2orderfilt=y
return
end function notch2orderfilt
!**************************************************************************************************
function bandpassfilt(dt,stepno,filt,x)
implicit none
real*8 bandpassfilt,dt,x
integer*4 stepno
type(Tbandpassfilt) filt
! local vars
real*8 y,f0,zeta,tau,a1,a2,b0,b1,b2,denom
! Step
if ((stepno.eq.1).and.(stepno.gt.filt.stepno1)) then
  filt.x1=x
  filt.x2=x
  filt.x1_old=filt.x1
  filt.x2_old=filt.x2
  filt.y1=x
  filt.y2=x
  filt.y1_old=filt.y1
  filt.y2_old=filt.y2
  y=x
else
  if (stepno.gt.filt.stepno1) then
    filt.x1_old=filt.x1
    filt.x2_old=filt.x2
    filt.y1_old=filt.y1
    filt.y2_old=filt.y2
  endif
  f0=filt.f0
  zeta=filt.zeta
  tau=filt.tau
  denom=3.d0+6.d0*zeta*pi*f0*dt+4.d0*pi**2*f0**2*dt**2
  a1=-(-6.d0+4.d0*pi**2*f0**2*dt**2)/denom
  a2=-(3.d0-6.d0*zeta*pi*f0*dt+4.d0*pi**2*f0**2*dt**2)/denom
  b0=-(-6.d0*zeta*pi*f0*dt-12.d0*zeta*pi*f0*tau)/denom
  b1=-24.d0*zeta*pi*f0*tau/denom
  b2=-(6.d0*zeta*pi*f0*dt-12.d0*zeta*pi*f0*tau)/denom
  y=a1*filt.y1_old+a2*filt.y2_old+b0*x+b1*filt.x1_old+b2*filt.x2_old
endif
! Save previous values
filt.x2=filt.x1
filt.x1=x
filt.y2=filt.y1
filt.y1=y
filt.stepno1=stepno
! Output
bandpassfilt=y
return
end function bandpassfilt
!**************************************************************************************************
end module misc_mod
