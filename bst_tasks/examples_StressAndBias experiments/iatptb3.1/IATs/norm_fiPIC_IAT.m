% 
function norm_fiPIC_IAT(subjectID);

KbName('UnifyKeyNames');

rand('state',sum(100*clock));
t=clock; 

% trial parameters 
preFixDur = 4;
instruxDur = 8;
postInstruxDur =1;
endFixDur = 10;
iti=0.25;
noRespTime=.2;
correctionOffset=.005;

% keycode=['1','2', '3', '4', '5', '6', '7', '8', '9', '0', 'q'];
leftKey='e';
rightKey='i';
keycode=[leftKey, rightKey, 'q'];

% Psychophysical set up parameters
screens=Screen('Screens');
monitor=max(screens);
screenWidthCM=30;                           % screen width in cm
initResolution=Screen('Resolution',monitor, 1024, 768);         % get the resolution of the display screen
screenRect=Screen(monitor, 'Rect');
screenWidthPix=screenRect(RectRight);       % screen width in pixels
screenHeightPix=screenRect(RectBottom);     % screen height in pixels
xCenter=screenWidthPix/2;
yCenter=screenHeightPix/2;
pixPerCM=screenWidthPix/screenWidthCM;		% monitor pixels per centimeter
viewdist=57;								% observer's viewing distance in CM
pixPerDva = (viewdist*(tan(1*pi/180)))*pixPerCM;
labelVPos1=round(.05*screenHeightPix); %in percentage of screen height
labelVPos2=round(.1*screenHeightPix); %in percentage of screen height
labelVPos3=round(.15*screenHeightPix); %in percentage of screen height
labelHPos=round(.15*screenWidthPix); %in percentage of screen width
wordHPos='center';


[outfName,errmsg] = sprintf('%s_%s_%d.%02.0f.%02.0f_%02.0f.%02.0f.%02.0f', subjectID, mfilename, t);
%OPEN OUTPUT FILES FOR IMAGE ORDER AND nBACK RESPONSE

outId=fopen([outfName '_imOrd.txt'], 'w'); % opens an output file for saving the image presentation order
if outId==-1
    error('unable to open image order file');
    return;
end

% Load Faces 
insectDir='Insects_sml'; %changed the name of the folder?
flowerDir='Flowers_sml';
homeDir=pwd;

cd(insectDir); %load the insect faces
fnames=dir('*.png');
fnames=struct2cell(fnames);
fnames=fnames(1,:);
for ind=1:length(fnames)
    insect(ind).name=fnames{ind};
    insect(ind).image=double(imread(insect(ind).name));
end
numInsectFaces=length(insect);
cd(homeDir);

cd(flowerDir); %load the flower faces
fnames=dir('*.png');
fnames=struct2cell(fnames);
fnames=fnames(1,:);
for ind=1:length(fnames)
    flower(ind).name=fnames{ind};
    flower(ind).image=double(imread(flower(ind).name));
end    
cd(homeDir);
numFlowerFaces=length(flower);


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
bgcolor = 110;
flowerInsectColor=[255 255 255];
insectFlowerColor=[255 255 255];
goodBadColor=[255 255 0];

% set up trial Order
switchAssoc=round(rand);
% switchAssoc=1;
if switchAssoc
    blockOrder = [ones(20,1)*8;ones(20,1)*2;ones(20,1)*4;ones(40,1)*4;ones(40,1);ones(20,1)*3;ones(40,1)*3];
else
    blockOrder = [ones(20,1);ones(20,1)*2;ones(20,1)*3;ones(40,1)*3;ones(40,1)*8;ones(20,1)*4;ones(40,1)*4];
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
%have a selector for insect/flower side and insectgood/bad order
switchSide=round(rand);

% Practice Good/Bad
block(1).name = 'goodbad';
block(1).sortLabels = {'Pleasant', 'Unpleasant'};
if switchSide block(1).sortLabels = block(1).sortLabels(:,[2 1]); end
block(1).leftLabel = block(1).sortLabels(:,1);
block(1).rightLabel = block(1).sortLabels(:,2);
block(1).hiTextColor = goodBadColor;

block(2).name = 'insectflower';
block(2).sortLabels = {'Insects', 'Flowers'};
if switchSide block(2).sortLabels = block(2).sortLabels(:,[2 1]); end
block(2).leftLabel = block(2).sortLabels(:,1);
block(2).rightLabel = block(2).sortLabels(:,2);
block(2).hiTextColor = insectFlowerColor;

block(3).name = 'insectgood';
block(3).sortLabels = {'Insects', 'Flowers'; ...
                       'Pleasant', 'Unpleasant'};
if switchSide block(3).sortLabels = block(3).sortLabels(:,[2 1]); end
block(3).leftLabel = block(3).sortLabels(:,1);
block(3).rightLabel = block(3).sortLabels(:,2);
block(3).hiTextColor = insectFlowerColor;
block(3).loTextColor = goodBadColor;

block(4).name = 'insectbad';
block(4).sortLabels = {'Insects', 'Flowers'; ...
                       'Unpleasant', 'Pleasant'};
if switchSide block(4).sortLabels = block(4).sortLabels(:,[2 1]); end
block(4).leftLabel = block(4).sortLabels(:,1);
block(4).rightLabel = block(4).sortLabels(:,2);
block(4).hiTextColor = insectFlowerColor;
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
            stimSel=round(rand*(numInsectFaces-1))+1;
%             blackFaceCtr=blackFaceCtr+1;
            stimName{ind}=insect(stimSel).name;
            stimStr{ind} = sprintf('Screen(''PutImage'', w, insect(%d).image);', stimSel);
            corrans{ind}='Insects';
        case 2
            stimSel=round(rand*(numFlowerFaces-1))+1;
%             flowerFaceCtr=flowerFaceCtr+1;
            stimName{ind}=flower(stimSel).name;
            stimStr{ind} = sprintf('Screen(''PutImage'', w, flower(%d).image);', stimSel);
            corrans{ind}='Flowers';
        case 3
            stimSel=round(rand*(numGoodWords-1))+1;
%             goodWordCtr=goodWordCtr+1;
            stimName{ind}=goodWords{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(w,goodWords{%d},''center'',''center'',goodBadColor);', stimSel);
            stimStr{ind} = sprintf('DrawFormattedText(w,goodWords{%d},''center'',wordHPos ,goodBadColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', goodWords{stimSel});
            corrans{ind}='Pleasant';
        case 4
            stimSel=round(rand*(numBadWords-1))+1;
%             badWordCtr=badWordCtr+1;
            stimName{ind}=badWords{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(w,badWords{%d},''center'',''center'',goodBadColor);', stimSel);
            stimStr{ind} = sprintf('DrawFormattedText(w,badWords{%d},''center'',wordHPos,goodBadColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', badWords{stimSel});
            corrans{ind}='Unpleasant';
        case 5
            stimSel=round(rand*(numFlowers-1))+1;
%             flowerCtr=flowerCtr+1;
            stimName{ind}=flowers{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(w,flowers{%d},''center'',''center'',flowerInsectColor);', stimSel);
            stimStr{ind} = sprintf('DrawFormattedText(w,flowers{%d},''center'',wordHPos,flowerInsectColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', flowers{stimSel});
            corrans{ind}='Flowers';
        case 6
            stimSel=round(rand*(numInsects-1))+1;
%             insectCtr=insectCtr+1;
            stimName{ind}=insects{stimSel};
            stimStr{ind} = sprintf('DrawFormattedText(w,insects{%d},''center'',''center'',flowerInsectColor);', stimSel);
            stimStr{ind} = sprintf('DrawFormattedText(w,insects{%d},''center'',wordHPos,flowerInsectColor);', stimSel);
%             stimStr{ind} = sprintf('DrawFormattedText(w,%s,''center'',wordHPos ,goodBadColor);', insects{stimSel});
            corrans{ind}='Insects';
        otherwise
            Screen('CloseAll');
            errordlg('oh shit');
    end
end

currBlock=0;
blockCtr=0;
% try
    AssertOpenGL
    KbName('UnifyKeyNames');
    % initialize On Screen Window
%     HideCursor;
	    
    [w, wRect] = Screen('OpenWindow', monitor, bgcolor, [], 32, 2);
    
    % set priority - also set after Screen init
    priorityLevel=MaxPriority(w);
    Priority(priorityLevel);
    HideCursor;

    if IsLinux==0
        Screen('TextFont',w, 'Arial');
        Screen('TextSize',w, 42);
        Screen('TextStyle', w, 1);
    end;
    
    for ind=1:length(block);
        for jnd=1:size(block(ind).sortLabels,1)
            bbox=Screen('TextBounds', w, block(ind).leftLabel{jnd});
            block(ind).leftTextStartX(jnd)=labelHPos-round(bbox(3)/2);
            bbox=Screen('TextBounds', w, block(ind).rightLabel{jnd});
            block(ind).rightTextStartX(jnd)=screenWidthPix-labelHPos-round(bbox(3)/2);
        end
    end
    orText='or';
    bbox=Screen('TextBounds', w, orText);
    leftOrStartX=labelHPos-round(bbox(3)/2);
	rightOrStartX=screenWidthPix-labelHPos-round(bbox(3)/2);

    %WAIT FOR BACKTICK
    
    %Initialize Text Params
    if IsLinux==0
        Screen('TextSize',w, 20);
    end;


    % Read some text file:
    instruxId = fopen('fiPICIATinstructions2.txt', 'rt');
    if instruxId==-1
        error('Could not open IATinstructions.txt file!');
    end
    
    instruxText = '';
    tl = fgets(instruxId);
    lcount = 0;
    while (tl~=-1)% & (lcount < 48)
        instruxText = [instruxText tl];
        tl = fgets(instruxId);
%         lcount = lcount + 1;
    end
    fclose(instruxId);
    [nx, ny, bbox] = DrawFormattedText(w, instruxText, 15, 20, 0);
    Screen('Flip', w)

    if IsLinux==0
        Screen('TextSize',w, 42);
    end;
    
%     loop=['[tmp, trialStart(ind)]=Screen(''Flip'', w);'...
%         'Screen(''DrawTexture'', w, screenText);'...
%         'eval(stimStr{ind});'...
%         'Screen(''DrawLine'', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);'...
%         'Screen(''DrawLine'', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);'...
%           'while (GetSecs - trialStart(ind)) < (iti-correctionOffset);'...
%               'KbCheck;'...
%           'end;'...
%           '[flipTime(ind), stimOnset(ind), flipEndTime(ind)] = Screen(''Flip'', w);'...
%         'secs=KbWait;'...
%         '[keyIsDown, secs, key] = KbCheck;'...
%         'keypress=KbName(key);'...
%         'keypress=keypress(1);'...
%         'if ismember(corrans{ind},block(blockCtr).leftLabel) & ismember(keypress,leftKey);'...
%             'correct(ind)=1;'...
%             'imCorr=1;'...
%         'elseif ismember(corrans{ind},block(blockCtr).rightLabel) & ismember(keypress,rightKey);'...
%             'correct(ind)=1;'...
%             'imCorr=1;'...
%         'else;'...
%             'correct(ind)=0;'...
%             'imCorr=0;'...
%         'end;'...
%         'respTime(ind)=secs;'...
%         'while ~imCorr;'...
%             'Screen(''DrawTexture'', w, screenText);'...
%             'eval(stimStr{ind});'...
%             'Screen(''DrawLine'', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);'...
%             'Screen(''DrawLine'', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);'...
%             'DrawFormattedText(w,''X'',''center'',''center'',[255 0 0]);'...
%             '[tmp, errOnset(ind), tmp2] = Screen(''Flip'', w);'...
%             'secs=KbWait;'...
%             '[keyIsDown, secs, key] = KbCheck;'...
%             'keypress=KbName(key);'...
%             'keypress=keypress(1);'...
%             'if ismember(corrans{ind},block(blockCtr).leftLabel) & ismember(keypress,leftKey);'...
%                 'imCorr=1;'...
%             'elseif ismember(corrans{ind},block(blockCtr).rightLabel) & ismember(keypress,rightKey);'...
%                 'imCorr=1;'...
%             'else;'...
%                 'imCorr=0;'...
%             'end;'...
%         'end;'...
%         'corrRespTime(ind)=secs;'...
%         'Screen(''DrawTexture'', w, screenText);'...
%         'Screen(''DrawLine'', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);'...
%         'Screen(''DrawLine'', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);'...
% 
%         ];

      fixFlag=0;
%     if ispc backtick=KbName('E'); end
%     if IsOSX backtick=KbName('`~'); end
%     while(1)
%         [keyIsDown, sec, key]=KbCheck;
%         if key(backtick)  %the scanner gives a ` at up trigger
%             break;
%         end     	  
%     end
    resp = getKBResp(Inf, {'b'},[],1);
    
%     DAS_20130325 Screen('DrawLine', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth); 
%     DAS_20130325 Screen('DrawLine', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
    [tmp, beginScan] = Screen('Flip', w); %begins initial fixation
    elapsedTrialTime=0;

%     WaitSecs(preFixDur)%MAKE MORE ACCURATE!!
    
    for ind=1:length(trialOrder)
        if blockNum(ind) ~= blockCtr;
            instruxStart=GetSecs;
%             if ind>1
%                 elapsedTrialTime=elapsedTrialTime+trialStartTime(ind-1)+trialDurTime(ind-1);
%             end
            currBlock=(blockOrder(ind));
            blockCtr=blockCtr+1;
            Screen('FillRect', w, bgcolor);
            if length(block(currBlock).leftLabel)>1
                DrawFormattedText(w,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(w,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(w,orText,leftOrStartX,labelVPos2,textcolor);
                DrawFormattedText(w,orText,rightOrStartX,labelVPos2,textcolor);
                DrawFormattedText(w,block(currBlock).leftLabel{2},block(currBlock).leftTextStartX(2),labelVPos3,block(currBlock).loTextColor);
                DrawFormattedText(w,block(currBlock).rightLabel{2},block(currBlock).rightTextStartX(2),labelVPos3,block(currBlock).loTextColor);
            else
                DrawFormattedText(w,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(w,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
            end
%             DrawFormattedText(w,block(currBlock).leftLabel,block(currBlock).leftTextStartX,labelVPos,textcolor);
%             DrawFormattedText(w,block(currBlock).rightLabel,block(currBlock).rightTextStartX,labelVPos,textcolor);
            DrawFormattedText(w,'Look at the categories\nPress any key to continue','center','center',textcolor);
            Screen('Flip', w);
            WaitSecs(2);
            Screen('FillRect', w, bgcolor);
            if length(block(currBlock).leftLabel)>1
                DrawFormattedText(w,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(w,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(w,orText,leftOrStartX,labelVPos2,textcolor);
                DrawFormattedText(w,orText,rightOrStartX,labelVPos2,textcolor);
                DrawFormattedText(w,block(currBlock).leftLabel{2},block(currBlock).leftTextStartX(2),labelVPos3,block(currBlock).loTextColor);
                DrawFormattedText(w,block(currBlock).rightLabel{2},block(currBlock).rightTextStartX(2),labelVPos3,block(currBlock).loTextColor);
            else
                DrawFormattedText(w,block(currBlock).leftLabel{1},block(currBlock).leftTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
                DrawFormattedText(w,block(currBlock).rightLabel{1},block(currBlock).rightTextStartX(1),labelVPos1,block(currBlock).hiTextColor);
            end
%             DAS_20130325 Screen('DrawLine', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
%             DAS_20130325 Screen('DrawLine', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);

            screenArray=Screen('GetImage', w, screenRect, 'backBuffer');
            screenText=Screen('MakeTexture', w, screenArray);


%             while (GetSecs-instruxStart) < (instruxDur - correctionOffset - 1);
%                 KbCheck;%WaitSecs(.001);
%             end
            KbWait;
            Screen('Flip', w);
            Screen('DrawTexture', w, screenText);
%             DAS_20130325             Screen('DrawLine', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
%             DAS_20130325             Screen('DrawLine', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
            WaitSecs(1);
        end
%         test=stimStr{ind}
        % Updates the screen to reflect our changes to the window.

        [tmp, trialStart(ind)]=Screen('Flip', w);
        Screen('DrawTexture', w, screenText);
        eval(stimStr{ind});
%             DAS_20130325         Screen('DrawLine', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
%             DAS_20130325         Screen('DrawLine', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
          while (GetSecs - trialStart(ind)) < (iti-correctionOffset)
              KbCheck;
          end
          [flipTime(ind), stimOnset(ind), flipEndTime(ind)] = Screen('Flip', w);
        WaitSecs(noRespTime);
        timecheck(ind)=GetSecs-stimOnset(ind);
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
            if strcmp(keypress, 'q')
                ShowCursor;
                Screen('CloseAll');
                fclose(outId);
%                 error('YO');
                save([outfName '.mat'], 'blockOrder', 'block', 'trialOrder', 'beginScan', 'flipTime', 'stimOnset', 'flipEndTime', 'trialStart');
                return;
            end
            Screen('DrawTexture', w, screenText);
            eval(stimStr{ind});
%             DAS_20130325             Screen('DrawLine', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
%             DAS_20130325             Screen('DrawLine', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
            DrawFormattedText(w,'X','center','center',[255 0 0]);
            [tmp, errOnset(ind), tmp2] = Screen('Flip', w);
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
        Screen('DrawTexture', w, screenText);
 %             DAS_20130325        Screen('DrawLine', w, fixColor, xCenter-fixWidthPix/2,yCenter,xCenter+fixWidthPix/2,yCenter,penWidth);
 %             DAS_20130325        Screen('DrawLine', w, fixColor, xCenter,yCenter-fixWidthPix/2,xCenter,yCenter+fixWidthPix/2,penWidth);
        
        fprintf(outId, '%d, %d, %d, %s, %s, %s, %s, %d, %d, %d, %s, %d, %d, %d\n', ind, trialOrder(ind), blockOrder(ind), block(currBlock).name, block(currBlock).leftLabel{1}, block(currBlock).rightLabel{1}, stimName{ind}, beginScan, trialStart(ind), stimOnset(ind), char(keypress), respTime(ind), correct(ind), corrRespTime(ind));
        if strcmp(keypress, 'q')
            return;
        end
     end
    
    
    
ShowCursor;
Screen('CloseAll');
Screen('Resolution',monitor, initResolution.width, initResolution.height);
fclose(outId);
save([outfName '.mat'], 'blockOrder', 'block', 'trialOrder', 'beginScan', 'flipTime', 'stimOnset', 'flipEndTime', 'trialStart');

% catch
%     ShowCursor;
%     Screen('CloseAll');
%     fclose(outId);
%     psychrethrow(psychlasterror);
% end
