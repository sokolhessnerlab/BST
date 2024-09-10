function data=analyzeRegIATData();


% if nargin<1 lineColor='b'; end

data=procFIIATData;

% preIntData=dataCell(:,:,1:3);
% postIntData=dataCell(:,:,4:6);
for ind=1:length(data)

%     data(ind).trialRespTime=data(ind).respTime-data(ind).trialStartTime-.2;
    data(ind).correct(find(data(ind).corrRespTime<.3))=-1; % throw out respTimes under .3 secs 
    if length(find(data(ind).correct==-1))./length(data(ind).correct) > .1
        warndlg('Bad Subject - too many immediate responses');
        return;
    end
    data(ind).correct(find(data(ind).corrRespTime>10))=-1; % throw out respTimes over 10 secs 
    tmp=strcmp('insectwhite', data(ind).block);
    data(ind).insectWhiteSel = find(tmp);
    tmp=strcmp('insectblack', data(ind).block);
    data(ind).insectBlackSel = find(tmp);
    
    if length(data(ind).insectWhiteSel) < 60 | length(data(ind).insectBlackSel) < 60
        warndlg('there are less than the usual number of trials');
        data(ind).fullDataSet = 0;
    else
        data(ind).fullDataSet = 1;
    end
    
        
    data(ind).iWhiteUseTrials = intersect(find(data(ind).correct>-1),data(ind).insectWhiteSel);
    data(ind).iBlackUseTrials = intersect(find(data(ind).correct>-1),data(ind).insectBlackSel);
%     data(ind).meanBBadResp = mean(data(ind).trialRespTime(data(ind).bBadUseTrials));
%     data(ind).meanBGoodResp = mean(data(ind).trialRespTime(data(ind).bGoodUseTrials));
    
    if min(data(ind).insectWhiteSel)>max(data(ind).insectBlackSel)
        data(ind).iBlackFirst=1;
        data(ind).indIBlackB1=data(ind).iBlackUseTrials(find(data(ind).iBlackUseTrials<61));
        data(ind).indIBlackB2=data(ind).iBlackUseTrials(find(data(ind).iBlackUseTrials>60));
        data(ind).indIWhiteB1=data(ind).iWhiteUseTrials(find(data(ind).iWhiteUseTrials<161));
        data(ind).indIWhiteB2=data(ind).iWhiteUseTrials(find(data(ind).iWhiteUseTrials>160));
    else
        data(ind).iBlackFirst=0;
        data(ind).indIBlackB1=data(ind).iBlackUseTrials(find(data(ind).iBlackUseTrials<161));
        data(ind).indIBlackB2=data(ind).iBlackUseTrials(find(data(ind).iBlackUseTrials>160));
        data(ind).indIWhiteB1=data(ind).iWhiteUseTrials(find(data(ind).iWhiteUseTrials<61));
        data(ind).indIWhiteB2=data(ind).iWhiteUseTrials(find(data(ind).iWhiteUseTrials>60));
    end
    
    data(ind).B1Std=std(data(ind).corrRespTime([data(ind).indIBlackB1 data(ind).indIWhiteB1]));
    data(ind).B2Std=std(data(ind).corrRespTime([data(ind).indIBlackB2 data(ind).indIWhiteB2]));
    
    data(ind).meanIBlackB1=mean(data(ind).corrRespTime(data(ind).indIBlackB1));
    data(ind).meanIBlackB2=mean(data(ind).corrRespTime(data(ind).indIBlackB2));
    data(ind).meanIWhiteB1=mean(data(ind).corrRespTime(data(ind).indIWhiteB1));
    data(ind).meanIWhiteB2=mean(data(ind).corrRespTime(data(ind).indIWhiteB2));

    data(ind).diffScoreB1=data(ind).meanIWhiteB1-data(ind).meanIBlackB1;
    data(ind).diffScoreB2=data(ind).meanIWhiteB2-data(ind).meanIBlackB2;

    data(ind).D=mean([data(ind).diffScoreB1./data(ind).B1Std data(ind).diffScoreB2./data(ind).B2Std]);
end

