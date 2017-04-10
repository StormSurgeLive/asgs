function fort14=readfort14(fort14file)
% This mfile will read in the fort.14 output from the ADCIRC circulation
% model.  Please refer to the ADCIRC manual for a complete description of
% the contents of the file.

% Created by D. Hill, September 2005
% Modified (heavily) by Taylor Asher, 2013-12-21
% Last updated 2017-03-07

% clear all;close all;clc
% % fort14file='C:\Users\Taylor_Asher\Documents\FEMA FIS Great Lakes\ProdRun\Files\Fort14\Erie_mesh_03192012_modified.grd';
% fort14file='C:\Users\taylor_asher\Documents\Info\ADCIRC,SWAN\Grids\CubaIkeModNOMAD1enoflux.grd'
if nargin==0||exist(fort14file,'file')==0
   [filename,pathname,~]=uigetfile('*.14;*.grd');
   fort14file=fullfile(pathname,filename);
elseif exist(fort14file,'dir')==7
   [filename,pathname,~]=uigetfile([fort14file,'*.14;*.grd']);
   fort14file=fullfile(pathname,filename);
elseif exist(fort14file,'file')==2
   [a,b,c]=fileparts(fort14file);
   filename=[b,c];
   pathname=a;
   clear a b c
end
if filename==0
   error('No valid input file selected')
end


% %Input the file.
% if exist('pathname')
%     [filename,pathname]=uigetfile(strcat(pathname,'*.14;*.grd'), ...
%         'Please select the fort.14 file you wish to input');
% else
%     [filename,pathname]= ...
%         uigetfile('*.14;*.grd','Please select the fort.14 file you wish to input');
% end

% Open the file and read in the header information
disp(['reading in ',fort14file])
fid=fopen(fort14file,'r');
try
   tmp=fgetl(fid);
   fort14.agrid=tmp;%sscanf(tmp,'%s');%I=find(tmp=='!');       AGRID=tmp(1:I-1);
   tmp=fgetl(fid);
   data=sscanf(tmp,'%i %i');
   fort14.ne=data(1);
   fort14.np=data(2);
   
   tmp=fscanf(fid,'%i %f %f %f\n',[4,fort14.np]);
   fort14.jn=uint32(tmp(1,:)');
   fort14.x=tmp(2,:)';
   fort14.y=tmp(3,:)';
   fort14.dp=tmp(4,:)';
   
   tmp=fscanf(fid,'%i %i %i %i %i\n',[5,fort14.ne]);
   fort14.je=uint32(tmp(1,:)');
   fort14.nhy=uint32(tmp(2,:)');
   fort14.nm=uint32(tmp(3:5,:)');
   clear tmp
   
   
   tmp=fgetl(fid);
   
   fort14.nope=sscanf(tmp,'%i');
   tmp=fgetl(fid);
   fort14.neta=sscanf(tmp,'%i');
   
   for i=1:fort14.nope
      tmp=fgetl(fid);
      data=sscanf(tmp,'%d');
      fort14.nvdll(i)=data(1);   %IBTYPEE(i)=data(2);
      for j=1:fort14.nvdll(i);
         tmp=fgetl(fid);
         fort14.nbdv(i,j)=uint32(sscanf(tmp,'%d'));
      end
   end
   
   tmp=fgetl(fid);
   fort14.nbou=sscanf(tmp,'%d');
   tmp=fgetl(fid);
   fort14.nvel=sscanf(tmp,'%d');
   
   fort14.inflows=false;
   for i=1:fort14.nbou
      tmp=fgetl(fid); data=sscanf(tmp,'%d %d');
      fort14.nvell(i)=data(1);
      fort14.ibtype(i)=data(2);
      for j=1:fort14.nvell(i);
         if any(fort14.ibtype(i)==[0,1,2,10,11,12,20,21,22,30])
            tmp=fgetl(fid);
            fort14.nbvv(i,j)=uint32(sscanf(tmp,'%d'));
            if fort14.ibtype(i)==2
               fort14.inflows=true;   %this flag lets fort.15 know that there are inflows...
            end
         elseif any(fort14.ibtype(i)==[3,13,23])
            tmp=fgetl(fid);
            data=sscanf(tmp,'%d %f %f');
            fort14.nbvv(i,j)=data(1);
            fort14.barlanht(i,j)=data(2);
            fort14.barlancfsp(i,j)=data(3);
         elseif any(fort14.ibtype(i)==[4,24])
            tmp=fgetl(fid);
            data=sscanf(tmp,'%d %d %f %f %f');
            fort14.nbvv(i,j)=data(1);
            fort14.ibconn(i,j)=data(2);
            fort14.barinht(i,j)=data(3);
            fort14.barincfsb(i,j)=data(4);
            fort14.barincfsp(i,j)=data(5);
         elseif any(fort14.ibtype(i)==[5,25])
            tmp=fgetl(fid);
            data=sscanf(tmp,'%d %d %f %f %f %f %f %f');
            fort14.nbvv(i,j)=data(1);
            fort14.ibconnr(i,j)=data(2);
            fort14.barinhtr(i,j)=data(3);
            fort14.barincfsbr(i,j)=data(4);
            fort14.barincfspr(i,j)=data(5);
            fort14.pipehtr(i,j)=data(6);
            fort14.pipecoefr(i,j)=data(7);
            fort14.pipediamr(i,j)=data(8);
         end
      end
   end
   fclose(fid);
catch err
   fclose(fid);
   rethrow(err)
end
fort14.filmesh=fort14file;