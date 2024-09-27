function data=analyzeRegIATData();


% if nargin<1 lineColor='b'; end

data=procRegIATData;

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
    tmp=strcmp('blackbad', data(ind).block);
    data(ind).blackBadSel = find(tmp);
    tmp=strcmp('blackgood', data(ind).block);
    data(ind).blackGoodSel = find(tmp);
    
    if length(data(ind).blackBadSel) < 60 | length(data(ind).blackGoodSel) < 60
        warndlg('there are less than the usual number of trials');
        data(ind).fullDataSet = 0;
    else
        data(ind).fullDataSet = 1;
    end
    
        
    data(ind).bBadUseTrials = intersect(find(data(ind).correct>-1),data(ind).blackBadSel);
    data(ind).bGoodUseTrials = intersect(find(data(ind).correct>-1),data(ind).blackGoodSel);
%     data(ind).meanBBadResp = mean(data(ind).trialRespTime(data(ind).bBadUseTrials));
%     data(ind).meanBGoodResp = mean(data(ind).trialRespTime(data(ind).bGoodUseTrials));
    
    if min(data(ind).blackBadSel)>max(data(ind).blackGoodSel)
        data(ind).bGoodFirst=1;
        data(ind).indBGoodB1=data(ind).bGoodUseTrials(find(data(ind).bGoodUseTrials<61));
        data(ind).indBGoodB2=data(ind).bGoodUseTrials(find(data(ind).bGoodUseTrials>60));
        data(ind).indBBadB1=data(ind).bBadUseTrials(find(data(ind).bBadUseTrials<161));
        data(ind).indBBadB2=data(ind).bBadUseTrials(find(data(ind).bBadUseTrials>160));
    else
        data(ind).bGoodFirst=0;
        data(ind).indBGoodB1=data(ind).bGoodUseTrials(find(data(ind).bGoodUseTrials<161));
        data(ind).indBGoodB2=data(ind).bGoodUseTrials(find(data(ind).bGoodUseTrials>160));
        data(ind).indBBadB1=data(ind).bBadUseTrials(find(data(ind).bBadUseTrials<61));
        data(ind).indBBadB2=data(ind).bBadUseTrials(find(data(ind).bBadUseTrials>60));
    end
    
    data(ind).B1Std=std(data(ind).corrRespTime([data(ind).indBGoodB1 data(ind).indBBadB1]));
    data(ind).B2Std=std(data(ind).corrRespTime([data(ind).indBGoodB2 data(ind).indBBadB2]));
    
    data(ind).meanBGoodB1=mean(data(ind).corrRespTime(data(ind).indBGoodB1));
    data(ind).meanBGoodB2=mean(data(ind).corrRespTime(data(ind).indBGoodB2));
    data(ind).meanBBadB1=mean(data(ind).corrRespTime(data(ind).indBBadB1));
    data(ind).meanBBadB2=mean(data(ind).corrRespTime(data(ind).indBBadB2));

    data(ind).diffScoreB1=data(ind).meanBGoodB1-data(ind).meanBBadB1;
    data(ind).diffScoreB2=data(ind).meanBGoodB2-data(ind).meanBBadB2;

    data(ind).D=mean([data(ind).diffScoreB1./data(ind).B1Std data(ind).diffScoreB2./data(ind).B2Std]);
end

