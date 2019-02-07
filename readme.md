Matlab-HAWC2 Interface
======================

The Matlab-HAWC2 interfance enables you to build your controller in Matlab and calls HAWC2 in the Matlab environment. The folder also contains a working example of the [DTU10MW Reference Wind Turbine
Model](http://www.hawc2.dk/Download/HAWC2-Model/DTU-10-MW-Reference-Wind-Turbine). 

 

Installation
============

This repository is written for Windows.

[HAWC2 ](http://www.hawc2.dk/) should be installed and the executable HAWC2mb.exe should be in the directory of "DTU_10_MW_Reference_Wind_Turbine_v9-1". The HAWC2mb.exe file and DTU10MW should be checked and updated from time to time. 

To install this package, clone the repository and pip install:

```
git clone https://github.com/alanliowh/Matlab-Hawc2-interface.git
cd Matlab-Hawc2-interface
pip install .
```
Or just simply, download it and unzip it.
Example
=======

To begin, go into the "matlab" directory.

Run "Main.m"

If you want to change the controller, go to "HAWC2matlabScript.m", where you can place your own controller instead of the default PID controller.


Contact
=====
Feel free to contact me if you encounter any problem regarding this interface at wali@dtu.dk.
