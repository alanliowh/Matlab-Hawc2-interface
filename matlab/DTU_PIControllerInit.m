% PIControllerInit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Controller parameters
Controller.PiParams.Kk = 1.135E+00;%
Controller.PiParams.Ki = 0.2;%

Controller.PiParams.Kp = 0.5;%
Controller.PiParams.CornerFreq  = 0.25*2*pi*1;%

% Wind turbine operating parameters
Controller.PiParams.Pnom = 10.0e+006;

Controller.PiParams.KII    = 1.0*0.100131E+08;%
Controller.PiParams.Ng     = 1;

Controller.PiParams.GenRot_nom = 1.0053;%
Controller.PiParams.Qgmax      = 1.9960e+05*1.2;%
Controller.PiParams.Qgmin      = 0;
Controller.PiParams.dQgmax     = 15e3;
Controller.PiParams.dQgmin     = -15e3;

Controller.PiParams.th_max = 90;
Controller.PiParams.th_min = -2;%

Controller.PiParams.dthmax = 8;
Controller.PiParams.dthmin = -8;
