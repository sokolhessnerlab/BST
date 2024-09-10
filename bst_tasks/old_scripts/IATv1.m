function [subjData] = IATv1(subjID, testMode)
    %%
    % subjID must be a 3-character string (e.g. '003')
    % testMode must be either 0 (do the full study) or 1 (do an abbreviated study)
    % 
    % DATA:
    % 
    
    %   %%% Have not yet set up a test mode
    % set up defaults
    Screen('Preference', 'SkipSyncTests', 1);   % skips sync tests for monitor relay timing (*ideally don't use this, mostly for use during testing w/ dual monitor setup)
    if nargin < 2
        testMode = 1; % assume test mode (unless otherwise specified)
    end
    if nargin < 1
        subjID = '000'; % assume default subjid 000 (unless otherwise specified)
    end  

    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% DESCRIPTION
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %{
            YYYY.MM.DD - UPDATES
            2018.09.17 - CXP began updating old IAT - things to update: keypresses. Visuals (center text, change font, change background color)
            2018.09.28 - CXP placed everything in "try" for an easy exit, adjusted visuals and keypresses to match other task scripts. Updated "rand" command to be
                         in line with modern MATLAB commands.
                         (To do: Adjust the way that the task is being saved. We'll probably need cleaner output files. Also, change the way the task forces
                          you to correct your answer before moving on? That's a little strange.)
            2018.10.01 - CXP Adjusted the saved output file (location, content, name), fixed minor bugs. (To do: screen initialization happens twice, make task more
                            aesthetically in line with other parts of the study?)
            2018.10.02 - CXP Fixed the issue of screen initializing twice, cleaned up the way that the screen gets drawn. Divided instructions into multiple pages,
                            removed the need to call a separate text document. Changed the font back to standard. Edited presentation of the fixation cross so that
                            it shows up only when words and X marks are not present. Centered the to-be-sorted word so that it aligns with where the fixation cross
                            is located. Added a note of what to do when a word is sorted incorrectly. Minor code formatting changes. (To do: Separate out practice trials
                            and actual trials, add more signposting indicating which is practice and which is the actual task, change color of background? That 
                            could be its own signposting - grey for practice trials, white for actual trials? IDK.)
            2018.10.05 - CXP Created "this concludes practice trial" screen. (To do: More formatting for end of practice trials/start of main task.)
            2018.10.08 - CXP changed save so that it includes what the correct response is for every trial, edited practice trial screen. (To do: Fix saving some more,
                            corrans is currently a single row rather than a single column; change background and word color to better match rest of study?; Add some kind
                            of completion screen to let participants know when the task is over/what they should do next?)
        %}

    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% PREPARATION & GLOBAL VARS
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % Create Experiment Window
    [wind, rect] = Screen('OpenWindow', max(Screen('Screens')));
    
    % Define Experiment Window
    Screen('BlendFunction', wind, GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);   % turn on aliasing to allow line to be smooth
    blk = BlackIndex(wind);
    wht = WhiteIndex(wind);
    gry = GrayIndex(wind, 0.8);
    Screen('TextFont',wind,'default');
    
    % show loading screen
    %DrawFormattedText(wind, 'Setting up...', 'center', 'center', blk);
    %Screen(wind,'Flip');
    
        % set up file paths
        % OF THE PROJECT (E.G. BST)
        % homepath = [filesep 'Volumes' filesep 'research' filesep 'Projects' filesep 'racial bias' filesep 'task' filesep];
        homepath = [filesep 'Volumes' filesep 'Research' filesep 'AHSS Psychology' filesep 'shlab' filesep 'Projects' filesep 'BST' filesep 'task' filesep];
        imgpath = ['images' filesep 'AMP' filesep];
        outputpath = ['output' filesep];


rng('shuffle');
t=clock; 

% trial parameters 
preFixDur = 4;
instruxDur = 8;
postInstruxDur =1;
endFixDur = 10;
iti=0.25;
noRespTime=.2;
correctionOffset=.005;

% basic keyboard prep
KbName('UnifyKeyNames');    % for OS X

% Define Response Keys % Work on updating code to match these response
% keys.
resp_keys = {'e', 'i'};
resp_key_codes = KbName(resp_keys);
space_key_code = KbName('space');
esc_key_code = KbName('ESCAPE');
trig_key_code = KbName('Return');

leftKey='e';
rightKey='i';
keycode=[leftKey, rightKey, 'q'];


%All this high key needs to be redone

% Psychophysical set up parameters
% screens=Screen('Screens');
% monitor=max(screens);
%screenWidthCM=30; %30cm was what it used to be                          % screen width in cm
%initResolution=Screen('Resolution',monitor, 1024, 768);         % get the resolution of the display screen
%screenRect=Screen(monitor, 'Rect');

%screenWidthPix=screenRect(RectRight);       % screen width in pixels

screenWidthPix = rect(RectRight);
screenHeightPix = rect(RectBottom);
%screenHeightPix=screenRect(RectBottom);     % screen height in pixels
xCenter=screenWidthPix/2;
yCenter=screenHeightPix/2;
%pixPerCM=screenWidthPix/screenWidthCM;		% monitor pixels per centimeter
%viewdist=57;								% observer's viewing distance in CM
%pixPerDva = (viewdist*(tan(1*pi/180)))*pixPerCM;
labelVPos1=round(.05*screenHeightPix); %in percentage of screen height
labelVPos2=round(.1*screenHeightPix); %in percentage of screen height
labelVPos3=round(.15*screenHeightPix); %in percentage of screen height
labelHPos=round(.15*screenWidthPix); %in percentage of screen width
wordHPos=yCenter-10;

outfName = 'testFile_01';
[outfName,errmsg] = sprintf('%s_%s_%d.%02.0f.%02.0f_%02.0f.%02.0f.%02.0f', subjID, mfilename, t);
%OPEN OUTPUT FILES FOR IMAGE ORDER AND nBACK RESPONSE

        % set up the path to the file on disk
     %   subjFile = fopen([homepath outputpath 'tmp_iattask_subj' subjectID '_' num2str(now) '.txt'],'w');
     %   fprintf(subjFile,'SUBJECT ID\tCONDITION\tBLOCK\tTRIAL NUM\tCUMULATIVE TRIAL NUM\tSTIM\tINK\tMASK\tRESPONSE\tRT\n');

outId=fopen([outputpath outfName '_imOrd.txt'], 'w'); % opens an output file for saving the image presentation order
if outId==-1
    error('unable to open image order file');
    return;
end

    %% Establish Try
    %{
    the majority of the script is established in this try section. if anything contained within the try statement fails the catch section executes allowing the script to break gracefully rather than throwing an error & locking the user out of the screen
    %} 
    try
%%

% Load Faces 
blackDir='images/blackFaces';
whiteDir='images/whiteFaces';
homeDir=pwd;

cd(blackDir); %load the black faces
fnames=dir('*.jpg');
fnames=struct2cell(fnames);
fnames=fnames(1,:);
for ind=1:length(fnames)
    black(ind).name=fnames{ind};
    black(ind).image=double(imread(black(ind).name));
end
numBlackFaces=length(black);
cd(homeDir);

cd(whiteDir); %load the white faces
fnames=dir('*.jpg');
fnames=struct2cell(fnames);
fnames=fnames(1,:);
for ind=1:length(fnames)
    white(ind).name=fnames{ind};
    white(ind).image=double(imread(white(ind).name));
end    
cd(homeDir);
numWhiteFaces=length(white);


goodWords={'Great', 'Terrific', 'Wonderful', 'Fabulous', 'Superb', 'Nice', 'Fantastic', 'Lovely'};
badWords={'Awful', 'Terrible', 'Horrible', 'Disgusting', 'Horrid', 'Foul', 'Revolting', 'Nasty'};
numGoodWords=length(goodWords);
numBadWords=length(badWords);
flowers={'Carnation', 'Daisy', 'Lily', 'Orchid', 'Poppy', 'Rose', 'Sunflower', 'Tulip'};
insects={'Beetle', 'Centipede', 'Cockroach', 'Flea', 'Mosquito', 'Scorpion', 'Spider', 'Tick'};
numFlowers=length(flowers);
numInsects=length(insects);


% Display parameters
fixHeightPix = 10;
fixWidthPix = 10;
penHeight=2;
penWidth=2;
fixColor=[0];
textcolor=[0];
flowerInsectColor=[255 255 255];
blackWhiteColor=[255 255 255];
goodBadColor=[255 255 0];

% set up trial Order
switchAssoc=round(rand);
% switchAssoc=1;
% Blocks are 1 (pleasant/unpleasant), 2 (black/white), 3 (Black/pleasant & White/unpleasant), 4 (Black/Unpleasant & White/pleasant), 5-7 insects & flowers, 8 is unpleasant/pleasant
if switchAssoc % Sets up blocks so it's bad/good (OR good/bad), b/w, and then incongruent x2, good/bad, and congruentx2 OR congruent x2, bad/good, incongruentx2
            % Also makes block length vary (20, 20, 20, 40, 40, 20, 40)
    blockOrder = [ones(20,1)*8;ones(20,1)*2;ones(20,1)*4;ones(40,1)*4;ones(40,1);ones(20,1)*3;ones(40,1)*3]; % 8 2 4 4 1 3 3
else
    blockOrder = [ones(20,1);ones(20,1)*2;ones(20,1)*3;ones(40,1)*3;ones(40,1)*8;ones(20,1)*4;ones(40,1)*4]; % 1 2 3 3 8 4 4
end
blockNum = [ones(20,1);ones(20,1)*2;ones(20,1)*3;ones(40,1)*4;ones(40,1)*5;ones(20,1)*6;ones(40,1)*7];

trialOrderB1 = (mod(randperm(20),2)+3)'; 
trialOrderB2 = (mod(randperm(20),2)+1)';

trialOrderB3 = zeros(20,1);
tmp1 = (mod(randperm(10),2)+1)';
tmp2 = (mod(randperm(10),2)+3)';
if round(rand)
    trialOrderB3(1:2:end)=tmp1;
    trialOrderB3(2:2:end)=tmp2;
else
    trialOrderB3(1:2:end)=tmp2;
    trialOrderB3(2:2:end)=tmp1;
end

trialOrderB4 = zeros(40,1);
tmp1 = (mod(randperm(20),2)+1)';
tmp2 = (mod(randperm(20),2)+3)';
if round(rand)
    trialOrderB4(1:2:end)=tmp1;
    trialOrderB4(2:2:end)=tmp2;
else
    trialOrderB4(1:2:end)=tmp2;
    trialOrderB4(2:2:end)=tmp1;
end

trialOrderB5 = (mod(randperm(40),2)+3)';

trialOrderB6 = zeros(20,1);
tmp1 = (mod(randperm(10),2)+1)';
tmp2 = (mod(randperm(10),2)+3)';
if round(rand)
    trialOrderB6(1:2:end)=tmp1;
    trialOrderB6(2:2:end)=tmp2;
else
    trialOrderB6(1:2:end)=tmp2;
    trialOrderB6(2:2:end)=tmp1;
end

trialOrderB7 = zeros(40,1);
tmp1 = (mod(randperm(20),2)+1)';
tmp2 = (mod(randperm(20),2)+3)';
if round(rand)
    trialOrderB7(1:2:end)=tmp1;
    trialOrderB7(2:2:end)=tmp2;
else
    trialOrderB7(1:2:end)=tmp2;
    trialOrderB7(2:2:end)=tmp1;
end

trialOrder=[trialOrderB1; trialOrderB2; trialOrderB3; trialOrderB4; trialOrderB5; trialOrderB6; trialOrderB7];

%Set up Blocks
%have a selector for black/white side and blackgood/bad order
switchSide=round(rand);

% Practice Good/Bad
block(1).name = 'goodbad';
block(1).sortLabels = {'Pleasant', 'Unpleasant'};
if switchSide block(1).sortLabels = block(1).sortLabels(:,[2 1]); end
block(1).leftLabel = block(1).sortLabels(:,1);
block(1).rightLabel = block(1).sortLabels(:,2);
block(1).hiTextColor = goodBadColor;

block(2).name = 'blackwhite';
block(2).sortLabels = {'Black', 'White'};
if switchSide block(2).sortLabels = block(2).sortLabels(:,[2 1]); end
block(2).leftLabel = block(2).sortLabels(:,1);
block(2).rightLabel = block(2).sortLabels(:,2);
block(2).hiTextColor = blackWhiteColor;

block(3).name = 'blackgood';
block(3).sortLabels = {'Black', 'White'; ...
                       'Pleasant', 'Unpleasant'};
if switchSide block(3).sortLabels = block(3).sortLabels(:,[2 1]); end
block(3).leftLabel = block(3).sortLabels(:,1);
block(3).rightLabel = block(3).sortLabels(:,2);
block(3).hiTextColor = blackWhiteColor;
block(3).loTextColor = goodBadColor;

block(4).name = 'blackbad';
block(4).sortLabels = {'Black', 'White'; ...
                       'Unpleasant', 'Pleasant'};
if switchSide block(4).sortLabels = block(4).sortLabels(:,[2 1]); end
block(4).leftLabel = block(4).sortLabels(:,1);
block(4).rightLabel = block(4).sortLabels(:,2);
block(4).hiTextColor = blackWhiteColor;
block(4).loTextColor = goodBadColor;

block(5).name = 'flowersinsects';
block(5).sortLabels = {'Flowers', 'Insects'};
if switchSide block(5).sortLabels = block(5).sortLabels(:,[2 1]); end
block(5).leftLabel = block(5).sortLabels(:,1);
block(5).rightLabel = block(5).sortLabels(:,2);
block(5).hiTextColor = flowerInsectColor;

block(6).name = 'flowersgood';
block(6).sortLabels = {'Flowers', 'Insects'; ...
                       'Pleasant', 'Unpleasant'};
if switchSide block(6).sortLabels = block(6).sortLabels(:,[2 1]); end
block(6).leftLabel = block(6).sortLabels(:,1);
block(6).rightLabel = block(6).sortLabels(:,2);
block(6).hiTextColor = flowerInsectColor;
block(6).loTextColor = goodBadColor;

block(7).name = 'flowersbad';
block(7).sortLabels = {'Flowers', 'Insects'; ...
                       'Unpleasant', 'Pleasant'};
if switchSide block(7).sortLabels = block(7).sortLabels(:,[2 1]); end
block(7).leftLabel = block(7).sortLabels(:,1);
block(7).rightLabel = block(7).sortLabels(:,2);
block(7).hiTextColor = flowerInsectColor;
block(7).loTextColor = goodBadColor;

block(8).name = 'badgood';
block(8).sortLabels = {'Unpleasant', 'Pleasant'};
if switchSide block(8).sortLabels = block(8).sortLabels(:,[2 1]); end
block(8).leftLabel = block(8).sortLabels(:,1);
block(8).rightLabel = block(8).sortLabels(:,2);
block(8).hiTextColor = goodBadColor;

% corrLeft=zeros(size(trialOrder));
% corrLeft(intersect([find(trialOrder==1); find(trialOrder==3)], find(blockOrder==3)))=1;
% corrLeft(intersect([find(trialOrder==1); find(trialOrder==4)], find(blockOrder==4)))=1;
% if switchSide corrLeft(find(trialOrder)) = ~corrLeft(find(trialOrder)); end

for ind=1:length(trialOrder)
    switch trialOrder(ind)
        case 1
            stimSel=round(rand*(numBlackFaces-1))+1;
%             blackFaceCtr=blackFaceCtr+1;
            stimName{ind}=black(stimSel).name;
            stimStr{ind} = sprintf('Screen(''PutImage'', wind, black(%d).image);', stimSel);
            corrans{ind}='Black';
        case 2
            stimSel=round(rand*(numWhiteFaces-1))+1;
%             whiteFaceCtr=whiteFaceCtr+1;
            stimName{ind}=white(stimSel).name;
            stimStr{ind} = sprintf('Screen(''PutImage'', wind, white(%d).image);', stimSel);
            corrans{ind}='White';
        case 3
            stimSel=round(rand*(numGoodWords-1))+1;
%             goodWordCtr=goodWordCtr+1;
            stimName{ind}=goodWords{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(wind,goodWords{%d},''center'',''center'',goodBadColor);', stimSel);
 %           stimStr{ind} = sprintf('DrawFormattedText(wind,goodWords{%d},''center'',wordHPos ,goodBadColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(wind,goodWords{%d},''center'',''center'' ,goodBadColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', goodWords{stimSel});
            corrans{ind}='Pleasant';
        case 4
            stimSel=round(rand*(numBadWords-1))+1;
%             badWordCtr=badWordCtr+1;
            stimName{ind}=badWords{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(wind,badWords{%d},''center'',''center'',goodBadColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(wind,badWords{%d},''center'',''center'',goodBadColor);', stimSel);
%            stimStr{ind} = sprintf('DrawFormattedText(wind,badWords{%d},''center'',wordHPos,goodBadColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', badWords{stimSel});
            corrans{ind}='Unpleasant';
        case 5
            stimSel=round(rand*(numFlowers-1))+1;
%             flowerCtr=flowerCtr+1;
            stimName{ind}=flowers{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(wind,flowers{%d},''center'',''center'',flowerInsectColor);', stimSel);
            stimStr{ind} = sprintf('DrawFormattedText(wind,flowers{%d},''center'',wordHPos,flowerInsectColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', flowers{stimSel});
            corrans{ind}='Flowers';
        case 6
            stimSel=round(rand*(numInsects-1))+1;
%             insectCtr=insectCtr+1;
            stimName{ind}=insects{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(wind,insects{%d},''center'',''center'',flowerInsectColor);', stimSel);
            stimStr{ind} = sprintf('DrawFormattedText(wind,insects{%d},''center'',wordHPos,flowerInsectColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', insects{stimSel});
            corrans{ind}='Insects';
        otherwise
            Screen('CloseAll');
            errordlg('oh shit');
    end
end

currBlock=0; % PSH: PUT NEXT TO BEGINNING OF BLOCK
blockCtr=0;
    AssertOpenGL
    KbName('UnifyKeyNames');
	  
    % set priority - also set after Screen init
    priorityLevel=MaxPriority(wind);
    Priority(priorityLevel);
    HideCursor;
    
    for ind=1:length(block)
        for jnd=1:size(block(ind).sortLabels,1)
            bbox=Screen('TextBounds', wind, block(ind).leftLabel{jnd});
            block(ind).leftTextStartX(jnd)=labelHPos-round(bbox(3)/2);
            bbox=Screen('TextBounds', wind, block(ind).rightLabel{jnd});
            block(ind).rightTextStartX(jnd)=screenWidthPix-labelHPos-round(bbox(3)/2);
        end
    end
    orText='or';
    bbox=Screen('TextBounds', wind, orText);
    leftOrStartX=labelHPos-round(bbox(3)/2);
	rightOrStartX=screenWidthPix-labelHPos-round(bbox(3)/2);

    
          %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% 'WAITING FOR EXPERIMENTER' SCREEN
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Display Text
            Screen('FillRect', wind, gry);
            DrawFormattedText(wind, 'Waiting for experimenter...', 'center', 'center', blk);
            Screen(wind,'Flip');
            while 1
                [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    elseif any(keyCode(trig_key_code))
                        break
                    end
                end
            end
%%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% INSTRUCTIONS SCREENS
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %Initialize Text Params
%     if IsLinux==0
%         Screen('TextSize',wind, 24);
%     end;
%     
%     
%     % Read some text file:
%     instruxId = fopen('bwIATinstructions2.txt', 'rt');
%     if instruxId==-1
%         error('Could not open IATinstructions.txt file!');
%     end
%     
%     instruxText = '';
%     tl = fgets(instruxId);
%     lcount = 0;
%     while (tl~=-1)% & (lcount < 48)
%         instruxText = [instruxText tl];
%         tl = fgets(instruxId);
% %         lcount = lcount + 1;
%     end
%     fclose(instruxId);
%     
    
%Create instruction strings
    
    instructStr{1} = ['In this task, you will be presented with a set of words or images to classify into groups. ' ... 
        'This task requires that you classify items as quickly as you can while making as few mistakes as possible. ' ...
        'Going too slow or making too many mistakes will result in an uninterpretable score.'];
    
    instructStr{2} = ['The following is a list of category labels and the items that belong to each of those categories. \n\n' ...
            'Category Items \n\n' ...
            'Good: Great, Terrific, Wonderful, Fabulous, Superb, Nice, Fantastic, Lovely. \n' ...
            'Bad: Awful, Terrible, Horrible, Disgusting, Horrid, Foul, Revolting, Nasty. \n' ...
            'Black American:  Faces of Black American people \n' ...
            'White American:  Faces of White American people'];
    
    instructStr{3} = ['The two labels at the top will tell you which words or images go with each key. ' ...  
        'Every so often the labels at the top will change, switching which words or images go with each key. ' ...
        'At these times there will be a short break and you will be told to look at the categories.'];
    
    instructStr{4} = ['Things to keep in mind: \n\n' ...
        '- Keep your index fingers on the ''e'' and ''i'' keys to enable rapid response.\n' ...
        '- Each word or image has a correct classification. Most of these are easy.\n' ...
        '- The test gives no results if you go slow -- Please try to go as fast as possible.\n' ...
        '- Expect to make a few mistakes because of going fast. That is OK.\n' ...
        '- If you make a mistake, a red X will appear. You should then correct your response as quickly as possible.'];
    
            for loopCnt = 1:length(instructStr)

              %  Screen('TextSize', wind, 30);
                DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.2, blk);
                DrawFormattedText(wind, instructStr{loopCnt}, 'center', 'center', blk, 60, [], [], 1.4);
                Screen('Flip',wind,[],1);
                
                if (testMode == 1)
                    WaitSecs(1);
                else
                    WaitSecs(3);
                end

                DrawFormattedText(wind, 'Press the space bar to continue when ready.', 'center', rect(4)*.9, blk);
                Screen('Flip', wind);

                while 1
                    [keyIsDown,~,keyCode] = KbCheck(-1);
                    if keyIsDown && any(keyCode(space_key_code))
                        break
                    elseif keyIsDown && keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
                
            end     % end for loop
    
               
%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Instructions Check-In
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
            DrawFormattedText(wind, 'This is the end of the instructions. Please tell your experimenter whether or not you have any questions.', 'center', 'center', blk, 45, [], [], 1.4);
            Screen(wind,'Flip');
            while 1
                [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    elseif any(keyCode(trig_key_code))
                        break
                    end
                end
            end
            
%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% PRACTICE
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %NEEDS A LOT OF EDITING/SIGNPOSTING!!
    
    DrawFormattedText(wind, 'Practice', 'center', rect(4)*.1, blk);
    DrawFormattedText(wind, 'Before starting the experiment, you will have two rounds of practice.', 'center', 'center', blk, 45, [], [], 1.4);
    Screen('Flip',wind,[],1);
    
    WaitSecs(1);
    
    DrawFormattedText(wind, 'Press the space bar to continue when ready to start the practice.', 'center', rect(4)*.9, blk);
    Screen('Flip', wind);
    
    while 1
        [keyIsDown,~,keyCode] = KbCheck(-1);
        if keyIsDown
            if keyCode(esc_key_code)
                error('Experiment aborted by user!');
            elseif any(keyCode(space_key_code))
                break
            end
        end
    end
    
    
%%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% START OF TASK
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%     Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
%     Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
    DrawFormattedText(wind,'+','center','center',fixColor);
    [tmp, beginScan] = Screen('Flip', wind); %begins initial fixation
    elapsedTrialTime=0;

%     WaitSecs(preFixDur)%MAKE MORE ACCURATE!!
    
%Used to count the number of loops (trials) the task is going through
loopcount = 0; % PSH: REMOVE LOOPCOUNT, USE IND

    for ind=1:length(trialOrder)
        loopcount = loopcount +1;
        disp(loopcount);
        
        if blockNum(ind) ~= blockCtr % PSH: CHECK THAT THIS IS EVALUATING LIKE YOU WANT IT TO (WHAT DOES THIS TEST?)
            instruxStart=GetSecs;
%             if ind>1
%                 elapsedTrialTime=elapsedTrialTime+trialStartTime(ind-1)+trialDurTime(ind-1);
%             end
            currBlock=(blockOrder(ind));
            blockCtr=blockCtr+1;
%             Screen('FillRect', wind, gry);
%             Screen('TextSize', wind, 40);
            
    Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth); % PSH: REPLACE WITH "+" CODE FROM 560
    Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
    [tmp, beginScan] = Screen('Flip', wind); %begins initial fixation  
    
    %Introduce a screen indicating the end of the practice rounds. Needs a little more formatting
    if loopcount == 41 % PSH: REMOVE THIS LOOP
        Screen('TextSize', wind, 24);
        DrawFormattedText(wind, 'This concludes the practice trials. When you are ready to begin the main task, please press the space bar to continue', 'center','center', blk, 60, [], [], 1.4);
        Screen('Flip', wind);
                    while 1
                [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    if any(keyCode(space_key_code))
                        break
                    elseif keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
                    end
    end
    
    WaitSecs(2);
    
             Screen('FillRect', wind, gry);
             Screen('TextSize', wind, 40);
            
            if length(block(currBlock).leftLabel)>1
                DrawFormattedText(wind,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(wind,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(wind,orText,leftOrStartX,labelVPos2,textcolor);
                DrawFormattedText(wind,orText,rightOrStartX,labelVPos2,textcolor);
                DrawFormattedText(wind,block(currBlock).leftLabel{2},block(currBlock).leftTextStartX(2),labelVPos3,block(currBlock).loTextColor);
                DrawFormattedText(wind,block(currBlock).rightLabel{2},block(currBlock).rightTextStartX(2),labelVPos3,block(currBlock).loTextColor);
            else
                DrawFormattedText(wind,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(wind,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
            end
%             DrawFormattedText(w,block(currBlock).leftLabel,block(currBlock).leftTextStartX,labelVPos,textcolor);
%             DrawFormattedText(w,block(currBlock).rightLabel,block(currBlock).rightTextStartX,labelVPos,textcolor);

            DrawFormattedText(wind,'Look at the categories\n \nPress the space bar to continue','center','center',textcolor);
            Screen('Flip', wind);
            WaitSecs(2);
            while 1
                [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    if any(keyCode(space_key_code))
                        break
                    elseif keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            end
            Screen('FillRect', wind, gry); % PSH: REPLACE THIS WITH A SCREEN FLIP?
            if length(block(currBlock).leftLabel)>1
                DrawFormattedText(wind,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(wind,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(wind,orText,leftOrStartX,labelVPos2,textcolor);
                DrawFormattedText(wind,orText,rightOrStartX,labelVPos2,textcolor);
                DrawFormattedText(wind,block(currBlock).leftLabel{2},block(currBlock).leftTextStartX(2),labelVPos3,block(currBlock).loTextColor);
                DrawFormattedText(wind,block(currBlock).rightLabel{2},block(currBlock).rightTextStartX(2),labelVPos3,block(currBlock).loTextColor);
            else
                DrawFormattedText(wind,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(wind,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
            end
            % Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
           %  Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);

            %screenArray=Screen('GetImage', wind, screenRect, 'backBuffer');
            screenArray=Screen('GetImage', wind, rect, 'backBuffer'); % PSH: HOW DOES 'BACKBUFFER' WORK?
            screenText=Screen('MakeTexture', wind, screenArray);


%             while (GetSecs-instruxStart) < (instruxDur - correctionOffset - 1);
%                 KbCheck;%WaitSecs(.001);
%             end
            KbWait;
            Screen('Flip', wind);
            Screen('DrawTexture', wind, screenText);
            Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth); % PSH: FIXATION
            Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
            WaitSecs(1);
        end
%         test=stimStr{ind}
        % Updates the screen to reflect our changes to the window.

        [tmp, trialStart(ind)]=Screen('Flip', wind);
        
        Screen('DrawTexture', wind, screenText); % PUT UP LABELS AS AN 'IMAGE'
        eval(stimStr{ind}); % EITHER DRAWS A PICTURE OR WRITES TEXT, DEPENDING ON WHAT KIND OF TRIAL THIS IS.
        
     %   Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
     %   Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
          while (GetSecs - trialStart(ind)) < (iti-correctionOffset)
              KbCheck; % PSH: WHAT IS THIS DOING? REPLACE WITH CODE FOR 'ESCAPING' THE STUDY?
          end
          [flipTime(ind), stimOnset(ind), flipEndTime(ind)] = Screen('Flip', wind);
        WaitSecs(noRespTime);
        timecheck(ind)=GetSecs-stimOnset(ind); % PSH: WHAT IS THIS DOING? SHOULD BE IMPROVED TO COLLECT KEYPRESSES FOR THIS LENGTH OF TIME.
        secs=KbWait;
        [keyIsDown, tmp, key] = KbCheck;
        keypress=char(KbName(key));
        keypress=keypress(1);
        if ismember(corrans{ind},block(currBlock).leftLabel) & ismember(keypress,leftKey)
            correct(ind)=1;
            imCorr=1;
        elseif ismember(corrans{ind},block(currBlock).rightLabel) & ismember(keypress,rightKey)
            correct(ind)=1;
            imCorr=1;
        else
            correct(ind)=0;
            imCorr=0;
        end
        respTime(ind)=secs-stimOnset(ind);
        if ~imCorr
           [keyIsDown,~,keyCode] = KbCheck(-1);
                if keyIsDown
                    if keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
            
%             if strcmp(keypress, 'q')
%                 ShowCursor;
%                 Screen('CloseAll');
%                 fclose(outId);
% %                 error('YO');
%                 save([outfName '.mat'], 'blockOrder', 'block', 'trialOrder', 'beginScan', 'flipTime', 'stimOnset', 'flipEndTime', 'trialStart');
%                 return;
%             end
            Screen('DrawTexture', wind, screenText);
            eval(stimStr{ind});
        %    Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
         %   Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
            DrawFormattedText(wind,'X','center','center',[255 0 0]);
            
            %Add some direction for what to do in the event that a mistake has been made.
            DrawFormattedText(wind, 'Please press the correct key in order to proceed through the task.', 'center', rect(4)*.9, blk);
            
            [tmp, errOnset(ind), tmp2] = Screen('Flip', wind);
            while ~imCorr
%                 secs=KbWait;
                [keyIsDown, secs, key] = KbCheck;
                if keyIsDown
                    keypress=char(KbName(key));
                    keypress=keypress(1);
                    if ismember(corrans{ind},block(currBlock).leftLabel) & ismember(keypress,leftKey)
                        imCorr=1;
                    elseif ismember(corrans{ind},block(currBlock).rightLabel) & ismember(keypress,rightKey)
                        imCorr=1;
                    else
                        imCorr=0;
                    end
                end
            end
        end
        corrRespTime(ind)=secs-stimOnset(ind);
        Screen('DrawTexture', wind, screenText);
        Screen('DrawLine', wind, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
        Screen('DrawLine', wind, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
        
        fprintf(outId, '%d, %d, %d, %s, %s, %s, %s, %d, %d, %d, %s, %d, %d, %d\n', ind, trialOrder(ind), blockOrder(ind), block(currBlock).name, block(currBlock).leftLabel{1}, block(currBlock).rightLabel{1}, stimName{ind}, beginScan, trialStart(ind), stimOnset(ind), char(keypress), respTime(ind), correct(ind), corrRespTime(ind));
        if keyCode(esc_key_code)
             error('Experiment aborted by user!');
        end
    end
    
ShowCursor;
Screen('CloseAll');
Screen('Resolution',monitor, initResolution.width, initResolution.height);
fclose(outId);
%save([outfName '.mat'], 'blockOrder', 'block', 'trialOrder', 'beginScan', 'flipTime', 'stimOnset', 'flipEndTime', 'trialStart', 'respTime', 'correct', 'corrRespTime');
save([outputpath 'iatTask_subj_' subjID '_' num2str(now) '.mat'], 'subjID', 'blockOrder', 'block', 'trialOrder', 'beginScan', 'flipTime', 'stimOnset', 'flipEndTime', 'trialStart', 'respTime', 'correct', 'corrRespTime', 'corrans');
    catch ME
        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% Catch
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %{
        "catch" section executes in case of an error in the "try" section above
        importantly, it closes the onscreen window if open
        ME is MATLAB Exception (aka the error code thrown during the try block)
        %}

        try
            fclose(subjFile);
        end

        try
          %  save([outputpath, 'iatTask_subj' subjID '_' num2str(now) '.mat'], 'subjData');
            save([outputpath 'iatTask_subj_' subjID '_' num2str(now) '.mat'], 'subjID', 'blockOrder', 'block', 'trialOrder', 'beginScan', 'flipTime', 'stimOnset', 'flipEndTime', 'trialStart', 'respTime', 'correct', 'corrRespTime', 'corrans');

        end

        ListenChar(0);
        Priority(0);
        sca;
        rethrow(ME)
        
    end % end try
end % end function %INCLUDE SOME KIND OF END SCREEN (PUT BEFORE SCA - 750ISH.)