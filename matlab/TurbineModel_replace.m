% Change WIND SPEED File:

fin = fopen([cd,'\..\' WTType '\htc\main_template.htc']);
fout = fopen([cd,'\..\' WTType '\htc\main.htc'],'w');

TempNames = {
'$TimeStop$'
'$LogFileName$'
'$ControllerTypeName$'
'$Outputname$'
};
% keyboard;
while ~feof(fin)
            s = fgetl(fin);
    for i=1:length(TempNames)            
            s = strrep(s, TempNames{i},TempNameValues{i});
%             keyboard;
    end
                fprintf(fout,'%s\n',s);
%     disp(s)
end

fclose(fin);
fclose(fout);
% keyboard;