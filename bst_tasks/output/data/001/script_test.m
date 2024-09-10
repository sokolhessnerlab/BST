%% Script for pulling out and reformatting data - test on participant 1


%% Set data path / Load data
datapath = '/Volumes/Research/AHSS Psychology/shlab/Projects/BST/task/output/data/001';

cd(datapath)

data = load('tgametask_subj001_day1_737462.6084.mat');


%% Pull out data, reformat into a single spreadsheet

% Pull data parts from the struct
params = struct2table(data.subjData.params);
subjData = data.subjData.data(:,:);
bonusData = struct2table(data.subjData.tOut);

% Combine into single spreadsheet

% Repeat subject ID + condition over an entire column and convert to table
subjectID = repmat(params.ID, length(subjData.trialNum),1);
condition = repmat(params.condition, length(subjData.trialNum), 1);
paramsTable = table(subjectID, condition);

% Concatenate subjectID and behavioral data
allData = [paramsTable subjData];

% Yay, it works for 1 person's TGame data!! Now to do this for all of a
% participant's computer tasks and all of the participants. Yikes...


