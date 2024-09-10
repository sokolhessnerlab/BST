% dataOut = process_DGTGTR1_CSV; %create a 1x1 structure with 8 fields containing all data
% 
% %clean out bad IATs
% dataOut.data=dataOut.data(~isnan(dataOut.IAT));
% dataOut.DGdisp=dataOut.DGdisp(~isnan(dataOut.IAT));
% dataOut.DGdiff=dataOut.DGdiff(~isnan(dataOut.IAT));
% dataOut.TGdisp=dataOut.TGdisp(~isnan(dataOut.IAT));
% dataOut.TGdiff=dataOut.TGdiff(~isnan(dataOut.IAT));
% dataOut.TRdisp=dataOut.TRdisp(~isnan(dataOut.IAT));
% dataOut.TRdiff=dataOut.TRdiff(~isnan(dataOut.IAT));
% dataOut.IAT=dataOut.IAT(~isnan(dataOut.IAT));
load dataOut_IATcln.mat

%Creating lists of TG, DG and TR disparity ratings that do not include NaN
% DGdisparity = dataOut.DGdisp(~isnan(dataOut.DGdisp));
% TGdisparity = dataOut.TGdisp(~isnan(dataOut.TGdisp));
% TRdisparity = dataOut.TRdisp(~isnan(dataOut.TRdisp));

numDG=numel(dataOut.data(1).DG.resp); %Determine how many trials in DG
numTG=numel(dataOut.data(1).TG.resp);
numTR=numel(dataOut.data(1).TR.resp);

%Calculate num consecAns for each participant
consecAnsDG = [];
consecAnsTG = [];
consecAnsTR = [];
for j=1:numel(dataOut.data)
    consecAnsDG(j)=sum(~diff(dataOut.data(j).DG.resp));
    consecAnsTG(j)=sum(~diff(dataOut.data(j).TG.resp));
    consecAnsTR(j)=sum(~diff(dataOut.data(j).TR.resp));
end

% Create a selector to weed out people with too many consecAns
ruleBasedDG_sel = consecAnsDG>round(numDG.*3/4);

hist(dataOut.IAT(ruleBasedDG_sel));

meanDGci = bootci(10000, @mean, dataOut.DGdisp(~ruleBasedDG_sel))
[corrDGIATr,corrDGIATp] = corr(dataOut.IAT(~ruleBasedDG_sel)', dataOut.DGdisp(~ruleBasedDG_sel)');
corrDGIATci = bootci(10000, @corr, dataOut.IAT(~ruleBasedDG_sel)', dataOut.DGdisp(~ruleBasedDG_sel)')
scatter(dataOut.IAT(~ruleBasedDG_sel)', dataOut.DGdisp(~ruleBasedDG_sel)')


TGresponses = [];

TRstd = [];
TGstd = [];
DGstd = [];

b_DGmean = [];
w_DGmean = [];

b_TRmean = [];
w_TRmean = [];

b_TGmean = [];
w_TGmean = [];

IATtype = [];

%populating the above empty lists with data values
for i = 1:numel(dataOut.data)
%     TRstd = [TRstd; dataOut.data(i).TR.std];
    TRstd(i) = dataOut.data(i).TR.std;
    TGstd = [TGstd; dataOut.data(i).TG.std];
    DGstd = [DGstd; dataOut.data(i).DG.std];
    
    b_DGmean = [b_DGmean; dataOut.data(i).DG.bMean];
    w_DGmean = [w_DGmean; dataOut.data(i).DG.wMean];
    
    b_TRmean = [b_TRmean; dataOut.data(i).TR.bMean];
    w_TRmean = [w_TRmean; dataOut.data(i).TR.wMean];
    
    b_TGmean = [b_TGmean; dataOut.data(i).TG.bMean];
    w_TGmean = [w_TGmean; dataOut.data(i).TG.wMean];
    
    IATtype = [IATtype; dataOut.data(i).dataCell(1,15)];
    
    consecAnsTG = [consecAnsTG; sum(~diff(dataOut.data(i).TG.resp))];
    
    TGresponses = [TGresponses; dataOut.data(i).TG.resp];
end

mTRdisparity = mean(TRdisparity)

mDGdisparity = mean(DGdisparity)

mTGdisparity = mean(TGdisparity)

mTRstd = mean(TRstd)

mTGstd = mean(TGstd)

mDGstd = mean(DGstd)

figure
hold on
title('Frequency of consecutive responses in Trust Game', 'FontSize', 16)
consecHist = histogram(consecAnsTG(TG_IATusable),[0:5:150])
hold off

figure (2)
hold on
title('Trust Game Responses n = 184', 'FontSize', 16)
TGresphist = histogram(TGresponses(consecAnsTG <= 106))
hold off

figure (3)
hold on
title('Trust Game Disparity','FontSize',16)
TGdisphist = histogram(TGdisparity)
hold off