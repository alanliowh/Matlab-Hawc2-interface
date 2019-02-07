% =========================================================================
% Programmed by:                                                          %
%           Wai Hou Lio                                                   %
%           wali@dtu.dk                                                   %
%           Department of Wind Energy                                     %
%           Technical University of Denmark                               %
%                                                                         %
% History:  Ver2.0 @                                                      %
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


if ~exist('PI_Controller','file')
    addpath([pwd, '\lib']);
end

if ~exist('DTUConrtollerHAWC2','file')
    addpath([pwd, '\DTUController']);
end

 DTU_PIControllerInit;

% -------------------------------------------------------------------------
%              Initialization of controller dependent variables
% -------------------------------------------------------------------------

 time = 0;

%%


% kill all processes!
system('c:\Windows\System32\taskkill.exe /f /im hawc2mb.exe');
system('c:\Windows\System32\taskkill.exe /f /im cmd.exe');

% -------------------------------------------------------------------------
%                       SIMULATION OPTIONS SETUP
% ------------------------------------------------------------------------- 
% % Simulation time options
 Tstart = 0; % From what time results should be saved
 Tend = 1000; % Simulation length
 Ts = 0.01; % Sample time
 SimParams.Ts = Ts;

%%

TCPserverType = 'tcplink_delay';
TCPportnumber = 1239;


%% ************************************************************************
%  * Start HAWC2 and run simulation
%  ************************************************************************
% tic-toc measures computation time of simulation
tic

% Open HAWC2
cd(['../DTU_10_MW_Reference_Wind_Turbine_v_9-1' ]);

if isunix
    system('wine hawc2MB.exe ./htc/DTU_10MW_RWT.htc')
else
    !hawc2MB.exe ./htc/DTU_10MW_RWT.htc &
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
