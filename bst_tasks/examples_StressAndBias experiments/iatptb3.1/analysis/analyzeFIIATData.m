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
    tmp=strcmp('flowersbad', data(ind).block);
    data(ind).flowersBadSel = find(tmp);
    tmp=strcmp('flowersgood', data(ind).block);
    data(ind).flowersGoodSel = find(tmp);
    
    if length(data(ind).flowersBadSel) < 60 | length(data(ind).flowersGoodSel) < 60
        warndlg('there are less than the usual number of trials');
        data(ind).fullDataSet = 0;
    else
        data(ind).fullDataSet = 1;
    end
    
        
    data(ind).fBadUseTrials = intersect(find(data(ind).correct>-1),data(ind).flowersBadSel);
    data(ind).fGoodUseTrials = intersect(find(data(ind).correct>-1),data(ind).flowersGoodSel);
%     data(ind).meanBBadResp = mean(data(ind).trialRespTime(data(ind).bBadUseTrials));
%     data(ind).meanBGoodResp = mean(data(ind).trialRespTime(data(ind).bGoodUseTrials));
    
    if min(data(ind).flowersBadSel)>max(data(ind).flowersGoodSel)
        data(ind).fGoodFirst=1;
        data(ind).indFGoodB1=data(ind).fGoodUseTrials(find(data(ind).fGoodUseTrials<61));
        data(ind).indFGoodB2=data(ind).fGoodUseTrials(find(data(ind).fGoodUseTrials>60));
        data(ind).indFBadB1=data(ind).fBadUseTrials(find(data(ind).fBadUseTrials<161));
        data(ind).indFBadB2=data(ind).fBadUseTrials(find(data(ind).fBadUseTrials>160));
    else
        data(ind).fGoodFirst=0;
        data(ind).indFGoodB1=data(ind).fGoodUseTrials(find(data(ind).fGoodUseTrials<161));
        data(ind).indFGoodB2=data(ind).fGoodUseTrials(find(data(ind).fGoodUseTrials>160));
        data(ind).indFBadB1=data(ind).fBadUseTrials(find(data(ind).fBadUseTrials<61));
        data(ind).indFBadB2=data(ind).fBadUseTrials(find(data(ind).fBadUseTrials>60));
    end
    
    data(ind).B1Std=std(data(ind).corrRespTime([data(ind).indFGoodB1 data(ind).indFBadB1]));
    data(ind).B2Std=std(data(ind).corrRespTime([data(ind).indFGoodB2 data(ind).indFBadB2]));
    
    data(ind).meanFGoodB1=mean(data(ind).corrRespTime(data(ind).indFGoodB1));
    data(ind).meanFGoodB2=mean(data(ind).corrRespTime(data(ind).indFGoodB2));
    data(ind).meanFBadB1=mean(data(ind).corrRespTime(data(ind).indFBadB1));
    data(ind).meanFBadB2=mean(data(ind).corrRespTime(data(ind).indFBadB2));

    data(ind).diffScoreB1=data(ind).meanFBadB1-data(ind).meanFGoodB1;
    data(ind).diffScoreB2=data(ind).meanFBadB2-data(ind).meanFGoodB2;

    data(ind).D=mean([data(ind).diffScoreB1./data(ind).B1Std data(ind).diffScoreB2./data(ind).B2Std]);
end

