function data = procRegIATData(dataFile, dataPath);

if nargin<2
    if nargin<1
        [dataFile, dataPath] = uigetfile('*_imOrd.txt', 'MultiSelect', 'On');
    else
        dataPath=uigetdir;
    end
end

if ~iscell(dataFile) 
    tmp=dataFile;
    clear dataFile;
    dataFile{1}=tmp; 
end

for ind=1:length(dataFile);

    fid=fopen([dataPath filesep dataFile{ind}], 'r');
    data(ind).fname=dataFile{ind};
    str=fgetl(fid);
    ctr=1;
    while str~=-1
        [tmp,rem]=strtok(str,', ');
        data(ind).trialNo(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).cond(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).blockNum(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).block(ctr) = {tmp};
        [tmp,rem]=strtok(rem,', ');
        data(ind).leftLabel(ctr) = {tmp};
        [tmp,rem]=strtok(rem,', ');
        data(ind).rightLabel(ctr) = {tmp};
        [tmp,rem]=strtok(rem,', ');
        data(ind).stim(ctr) = {tmp};
        [tmp,rem]=strtok(rem,', ');
        data(ind).beginScan(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).trialStartTime(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).stimOnsetTime(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).keyPress(ctr) = {tmp};
        [tmp,rem]=strtok(rem,', ');
        data(ind).respTime(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).correct(ctr) = str2num(tmp);
        [tmp,rem]=strtok(rem,', ');
        data(ind).corrRespTime(ctr) = str2num(tmp);
        ctr=ctr+1;
        str=fgetl(fid);
    end
end

