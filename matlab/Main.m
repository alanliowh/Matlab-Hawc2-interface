% =========================================================================
% Programmed by:                                                          %
%           Mahmood Mirzaei                                               %
%           mmir@dtu.dk                                                   %
%           Department of Wind Energy                                     %
%           Technical University of Denmark                               %
%                                                                         %
% History:  Ver1.0 @                                                      %
%                                                                         %
% License:  << Open Source Research Collaboration >>                      %
%                                                                         %
%           This version is not Linux compatible!                         %
% =========================================================================

clc; fclose all; clear all; warning off; profile on;beep off;

%%

% -------------------------------------------------------------------------
%                          Initialization
% -------------------------------------------------------------------------

global saved_vars Sys_PPF SimParams Controller;


global out_saved;
out_saved = [];

global time_old;

time_old = 0;

global omega_g_saved ;
omega_g_saved = [];

if ~exist('PI_Controller','file')
    addpath([pwd, '\lib']);
end

if ~exist('DTUConrtollerHAWC2','file')
    addpath([pwd, '\DTUController']);
end

RootDir = pwd;

DTU_PIControllerInit;

% -------------------------------------------------------------------------
%              Initialization of controller dependent variables
% -------------------------------------------------------------------------

OutputName = '1';
LogFileName= OutputName;


% Xpid = [0;0];
% U_old = [0;0];
time_old = 0;
time = 0;

%%


% kill all processes!
system('c:\Windows\System32\taskkill.exe /f /im hawc2mb.exe');
system('c:\Windows\System32\taskkill.exe /f /im cmd.exe');

% -------------------------------------------------------------------------
%                       SIMULATION OPTIONS SETUP
% -------------------------------------------------------------------------

% Simulation time options
Tstart = 0; % From what time results should be saved
Tend = 500; % Simulation length
Ts = 0.025; % Sample time
SimParams.Ts = Ts;

Sys_PPF = c2d(ss(tf(1,[100,1])),Ts);    % Platform pitch filter


ControllerType = 'MATCtrl_Type2Acts';
WTType = 'OnshoreHawc2du10mw';
OutputName = [ControllerType '_' OutputName '_3'];




%%

TempNameValues = {
    num2str(Tend);
    [LogFileName '.log'];
    [ControllerType '.htc'];
    OutputName
    };


TurbineModel_replace;
%%

TCPserverType = 'tcplink_delay';
TCPportnumber = 1239;


%% ************************************************************************
%  * Start HAWC2 and run simulation
%  ************************************************************************
% tic-toc measures computation time of simulation
tic

% Open HAWC2
cd(['../' WTType ]);

if isunix
    system('wine hawc2MB.exe ./htc/main.htc')
else
    !hawc2MB.exe ./htc/main.htc &
end

cd('../matlab');


TcpIpConnected = 0;
for i = 1:100
    if TcpIpConnected
        break,
    end
    pause(.1);
    try
        JTCPOBJ = jtcp('REQUEST','localhost',TCPportnumber);
        TcpIpConnected = 1;
    catch TryTcpIpConnect
        TcpIpConnected = 0;
        fprintf('.')
    end
end



HAWC2matlabScript

jtcp('CLOSE',JTCPOBJ)

toc

% end
