clear all
close all
cd C:\Users\B2EDHMEA\Documents\Rivergages

%stations={'03780','76025','85700','76010','85670','01340','01400'}

stations={'01160','01220','01260','01275','01280','01300','01320','01320','01340','01380','01390','01400','01440','01480','01515','01545','01575','01670'};
            

for f = 1:length(stations)
wl=rivergages(stations{1,f},datestr(now-15),datestr(now+1),'HG');
plot(wl(:,1),wl(:,2));hold on;
axis([min(wl(:,1)),max(wl(:,1)),0,40])
datetick('x')
end

