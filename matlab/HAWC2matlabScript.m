% Main simulation loop
while time < Tend-Ts
    %     keyboard;
    
    % THE VARIABLE “mssg” WILL CONTAIN THE OUTPUT VALUES FROM HAWC2
    StringFromHawc=[];
    
    while (isempty(StringFromHawc))
        StringFromHawc = jtcp('READ',JTCPOBJ);
        StringFromHawc = char(StringFromHawc);
    end
    % Replaces NaN with 0 if needed
    StringFromHawc = regexprep(StringFromHawc, 'NAN', '0');
    % Convert string to array
    ArrayFromHawc = str2num(StringFromHawc);
    % Remove first entry from array
    ArrayFromHawc = ArrayFromHawc(2:end);
    
    time = ArrayFromHawc(1);
    clc;
    fprintf('Simulation time: %3.2f',time);
    
    % Pitch actuator sensors
    th = ArrayFromHawc(3)*180/pi;
    %     dth = ArrayFromHawc(3)*180/pi;
    %     d2th = ArrayFromHawc(3)*180/pi;
    
    % Generator sensors
    Qg = ArrayFromHawc(27);
    dQg = ArrayFromHawc(28);
    Pe = ArrayFromHawc(31)*1e3;
    
    % Driveshaft sensors
    omega_r = -ArrayFromHawc(2);
    omega_g = ArrayFromHawc(2);%*ProblemParms.Ng; % Changed the sign here from - to +
    phi_delta = -ArrayFromHawc(6);
    
    % Nacelle fore-aft displacement sensors
    psi_t = ArrayFromHawc(7);
    dpsi_t = ArrayFromHawc(8);
    d2psi_t = ArrayFromHawc(9);
    
    % Wind speed at nacelle sensor
    vy = ArrayFromHawc(37);
    
    % Torque on driveshaft at rotor side
    Qr = -ArrayFromHawc(25)*1000;

    
    measurements.Clock     = time;                           				%    1: general time                            [s]
    measurements.GenSpeed  = omega_g;              				%    2: constraint bearing1 shaft_rot 1 only 2  [rad/s] Generator speed (Default LSS, if HSS insert gear ratio in input #76)
    
    measurements.BldPitch1 = th;            					%    3: constraint bearing2 pitch1 1 only 1     [rad]
    measurements.BldPitch2 = th;               				%    4: constraint bearing2 pitch2 1 only 1     [rad]
    measurements.BldPitch3 = th;               				%    5: constraint bearing2 pitch3 1 only 1     [rad]
    measurements.Wind1VelX =  ArrayFromHawc(7);                       				%    6: wind free_wind 1 0.0 0.0 hub height     [m/s] global coords at hub height
    measurements.Wind1VelY =  ArrayFromHawc(7); % ###TODO: remove this from here!!! Here I am temporarily using 14                         				%    7: Wind_y
    measurements.Wind1VelZ =  ArrayFromHawc(7);                        				%    8: Wind_z
    
    Qr                      = -ArrayFromHawc(11)*1000;
    measurements.pe         =   Qr*omega_g;     %####TODO: remove it from here!!!!			                       %    9: elec. power   [W]
    measurements.YawBrTAxp  = ArrayFromHawc(9);                  					%   11: Tower top x-acceleration   [m/s^2]
    measurements.YawBrTAyp  = ArrayFromHawc(10);                  					%   12: Tower top y-acceleration   [m/s^2]
    
    global WT1;

    if ArrayFromHawc(12)>20
        ArrayFromHawc(12) = ArrayFromHawc(12)-180;
    elseif ArrayFromHawc(12)<-20
        ArrayFromHawc(12) = ArrayFromHawc(12)+180;
    end
    
    
    WT1.Measrmnt.PltfPitch  = ArrayFromHawc(12);                  					%
    
    
    WT1.Measrmnt.GenSpeed = omega_g;
    
    % ### Here you can put your own controller:
    Controller_Output = PI_Controller(measurements);
    
    if ~isreal(Controller_Output)
        Controller_Output
    end
    
    ArrayToHawc     = zeros(40,1);
    
    ArrayToHawc(1)  = Controller_Output(1)*pi/180;
    ArrayToHawc(2)  = Controller_Output(1)*pi/180;
    ArrayToHawc(3)  = Controller_Output(1)*pi/180;
    ArrayToHawc(4)  = Controller_Output(2);
    
    StringToHawc    = [sprintf('%0.12f;',ArrayToHawc) '*'];
    jtcp('WRITE',JTCPOBJ,int8(StringToHawc));
    
end

