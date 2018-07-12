function Controller_Output = PI_Controller(measurements)
% U - (1) Pitch angle, (2) generator torque
% X - (1) Filtered generator speed, (2) integral error state

global WT1 SimParams Controller;

persistent CtrlStates CtrlInputs;
if isempty(CtrlStates), CtrlStates=[0;0]; end;
if isempty(CtrlInputs), CtrlInputs=[0;0]; end;

GenRot  = WT1.Measrmnt.GenSpeed;%
Ts      = SimParams.Ts;

Ei              = CtrlStates(1);
GenRot_filter   = CtrlStates(2);

% Lowpass filter for generator speed
Alpha           = exp( ( -Ts )*Controller.PiParams.CornerFreq );
GenRot_filter   = ( 1.0 - Alpha )*GenRot + Alpha*GenRot_filter;

% Speed error
Ep = GenRot_filter - Controller.PiParams.GenRot_nom;

% Integrated speed error
Ei = Ei + Ep*Ts;


Kgs = 1/(1+(pi/180*(CtrlInputs(1)-Controller.PiParams.th_min))/Controller.PiParams.Kk)*180/pi;

% Limit of generator speed where optimal tracking ends
GenRot_high = Controller.PiParams.GenRot_nom*.95;

% Generator torque control
Qh      = Controller.PiParams.KII * GenRot_high^2/Controller.PiParams.Ng^3;
Qnom    = Controller.PiParams.Pnom/Controller.PiParams.GenRot_nom;

U(2,1) = Controller.PiParams.KII * GenRot_filter^2/Controller.PiParams.Ng^3;
% 
if CtrlInputs(1) <= Controller.PiParams.th_min+0.1 % Below rated wind speed
    if GenRot_filter <= GenRot_high
        
        U(2,1) = Controller.PiParams.KII * GenRot_filter^2/Controller.PiParams.Ng^3;
    else
        U(2,1) = Qh + (Qnom-Qh)*(GenRot_filter-GenRot_high)/(Controller.PiParams.GenRot_nom-GenRot_high);
    end
else % Above rated wind speed
    U(2,1) =  Controller.PiParams.Pnom/GenRot_filter;%4.1337e+004;%
end

% PI controller for pitch angle
U(1,1) = Kgs*(Controller.PiParams.Kp*Ep + Controller.PiParams.Ki*Ei);

% Saturation limits of control signals
Umax    = [Controller.PiParams.th_max  Controller.PiParams.Qgmax]';
Umin    = [Controller.PiParams.th_min  Controller.PiParams.Qgmin]';
dUmax   = [Controller.PiParams.dthmax  Controller.PiParams.dQgmax]';
dUmin   = [Controller.PiParams.dthmin  Controller.PiParams.dQgmin]';

% Saturate control signal
U = min(max(U,Umin),Umax);

% Saturate control signal rate
dU = min(max((U-CtrlInputs)/Ts,dUmin),dUmax);

% Calc. saturated control signal
U = CtrlInputs+dU*Ts;

% Integrator anti-windup
% This line is important for it prevents the negative error to accumulate in the integrator!
Ei = (U(1) - Kgs*Controller.PiParams.Kp*Ep)/(Kgs*Controller.PiParams.Ki);


X(1,1) = Ei;
X(2,1) = GenRot_filter;

CtrlStates = X;
CtrlInputs = U;


Controller_Output = [U(1); U(2)];

return;



