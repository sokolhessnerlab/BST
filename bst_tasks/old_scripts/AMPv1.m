function [subjData] = AMPv1(subjID, testMode)
    %
    % subjID must be a 3-character string (e.g. '003')
    % testMode must be either 0 (do the full study) or 1 (do an abbreviated study)
    % 
    % DATA:
    % 'Pleasant' is scored as 0
    % 'Unpleasant' is scored as 1
    % condition '0' is 'Pleasant' on left
    % condition '1' is 'Pleasant' on right
    %
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
            2018.06.22 - ESA created file
            2018.07.18 - PSH commented (look for <% % %>)
            2018.07.23 - ESA made corrections (added image caching, used GetSecs for timing, etc.)
            2018.09.17 - CXP fixed indexing to filepath
        %}

    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% PREPARATION & GLOBAL VARS
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
        % Create Experiment Window
       % [wind, rect] = Screen('OpenWindow', max(Screen('Screens')));
        rect=[1600 0 2400 600];
        [wind, rect] = Screen('OpenWindow', max(Screen('Screens')) , 200, rect);

        % Define Experiment Window
        Screen('BlendFunction', wind, GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);   % turn on aliasing to allow line to be smooth
        blk = BlackIndex(wind);
        wht = WhiteIndex(wind);
        gry = GrayIndex(wind, 0.8);
        Screen('TextFont',wind,'default');

        % show loading screen
        DrawFormattedText(wind, 'Setting up...', 'center', 'center', blk);
        Screen(wind,'Flip');

        %{ 
        rng ('shuffle') seeds the random number generator based on the current time. if the seed is not shuffled, MATLAB will default to the same set of random numbers continuously
        %}
        rng('shuffle');

        % set up file paths % % % THE FOLDER FOR THIS SHOULD BE THE ACRONYM
        % OF THE PROJECT (E.G. BST)
        % homepath = [filesep 'Volumes' filesep 'research' filesep 'Projects' filesep 'racial bias' filesep 'task' filesep];
        homepath = [filesep 'Volumes' filesep 'Research' filesep 'AHSS Psychology' filesep 'shlab' filesep 'Projects' filesep 'BST' filesep 'task' filesep];
        imgpath = ['images' filesep 'AMP' filesep];
        outputpath = ['output' filesep];

        % basic keyboard prep
        KbName('UnifyKeyNames');    % for OS X

        % Define Response Keys
        resp_keys = {'e', 'i'};
        resp_key_codes = KbName(resp_keys);
        space_key_code = KbName('space');
        esc_key_code = KbName('ESCAPE');
        trig_key_code = KbName('Return');
        
        % set up the number of stimuli to choose from
        numTotalStim = 50;
        
        % set up the number of stimuli (per 3 stimuli groups)
        if testMode == 1
            numStim = 2;
            numBlocks = 3;
        else
            % the product of (stim*3)*blocks should always equal 150
            numStim = 10;
            numBlocks = 5;
            HideCursor;
        end

        % assign condition
        if rand < 0.5
            condition = 0;    % Pleasant on left
        else
            condition = 1;    % Pleasant on right
        end
        
        % set up the data struct for the subject
        subjData = struct;
        subjData.ID = subjID;
        subjData.condition = condition;
        subjData.blockNum = []; % % % OFTEN BETTER PRACTICE TO ALLOCATE THE RIGHT SIZE, AND THEN LATER YOU JUST INDEX INTO IT.
        % % % TWO REASONS FOR THIS: 1) IT TAKES A TOUCH LONGER TO ELONGATE
        % A VARIABLE EVERY TIME YOU STORE SOMETHING IF YOU BEGIN WITH A
        % SIZE NULL VARIABLE; IF YOU'VE PRE-ALLOCATED, IT'S FASTER TO STORE
        % INFORMATION. 2) IF YOU PRE-ALLOCATE VARIABLE SPACE, THEN IF YOU
        % MESS UP, YOU HAVE A BETTER CHANCE OF GETTING AN ERROR SAYING
        % YOU'VE MESSED UP, B/C YOU'RE INDEXING SOMETHING THAT DOESN'T
        % EXIST AND/OR YOU HAVE A BUNCH OF NANs THAT YOU NEVER FILLED.
        subjData.trialNum = [];
        subjData.cumTrialNum = [];
        subjData.stimulus = [];
        subjData.inkStimulus = [];
        subjData.maskStimulus = [];
        subjData.response = [];
        subjData.RT = [];
        
        % set up the path to the file on disk
        subjFile = fopen([homepath outputpath 'tmp_amptask_subj' subjID '_' num2str(now) '.txt'],'w');
        fprintf(subjFile,'SUBJECT ID\tCONDITION\tBLOCK\tTRIAL NUM\tCUMULATIVE TRIAL NUM\tSTIM\tINK\tMASK\tRESPONSE\tRT\n');

        % capture keypresses so that they don't affect the editor or console
        % use cmd-c to re-allow keys should execution fail before code sets flag back
        if testMode == 1
            ListenChar(2);
        end

    %% Establish Try
    %{
    the majority of the script is established in this try section. if anything contained within the try statement fails the catch section executes allowing the script to break gracefully rather than throwing an error & locking the user out of the screen
    %} 
    try
        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% LOAD STIMULI PATHS INTO ARRAYS
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            DrawFormattedText(wind, 'Loading face stimuli 01...', 'center', 'center', blk);
            Screen(wind,'Flip');
            % black faces
            images_face_blk = dir([imgpath 'faces/black*.bmp']);

            DrawFormattedText(wind, 'Loading face stimuli 02...', 'center', 'center', blk);
            Screen(wind,'Flip');
            % white faces
            images_face_wht = dir([imgpath 'faces/white*.bmp']);

            DrawFormattedText(wind, 'Loading face stimuli 03...', 'center', 'center', blk);
            Screen(wind,'Flip');
            % other faces
            images_face_oth = dir([imgpath 'faces/other*.bmp']);

            DrawFormattedText(wind, 'Loading inks...', 'center', 'center', blk);
            Screen(wind,'Flip');
            % inkblots
            images_ink = dir([imgpath 'inks/pd_sm_*']);

            DrawFormattedText(wind, 'Loading masks...', 'center', 'center', blk);
            Screen(wind,'Flip');
            % masks/response ('Pleasant' on the left or right)
            images_mask_p_right = dir([imgpath 'masks/pdPR*']);
            images_mask_p_left = dir([imgpath 'masks/pdPL*']);

        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% RANDOMIZE ARRAYS AND SELECT SUBSETS
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            DrawFormattedText(wind, 'Creating stimuli subsets...', 'center', 'center', blk);
            Screen(wind,'Flip');

            % faces (50 (by default) of each)
            images_face_blk = images_face_blk(randperm(length(images_face_blk)));
            stim_blk = images_face_blk(1:numTotalStim);
            images_face_wht = images_face_wht(randperm(length(images_face_wht)));
            stim_wht = images_face_wht(1:numTotalStim);
            images_face_oth = images_face_oth(randperm(length(images_face_oth)));
            stim_oth = images_face_oth(1:numTotalStim);

            % inkblots (50 (by default) to each face subset)
            images_ink = images_ink(randperm(length(images_ink)));
            stim_ink = images_ink(1:numTotalStim);
            stim_ink_blk  = stim_ink(randperm(length(stim_ink)));
            stim_ink_wht  = stim_ink(randperm(length(stim_ink)));
            stim_ink_oth  = stim_ink(randperm(length(stim_ink)));

            % masks (50 (by default) of both left and right to each face subset)
            images_mask_p_right = images_mask_p_right(randperm(length(images_mask_p_right)));
            stim_mask_p_right = images_mask_p_right(1:numTotalStim);
            stim_mask_p_right_blk  = stim_mask_p_right(randperm(length(stim_mask_p_right)));
            stim_mask_p_right_wht  = stim_mask_p_right(randperm(length(stim_mask_p_right)));
            stim_mask_p_right_oth  = stim_mask_p_right(randperm(length(stim_mask_p_right)));

            images_mask_p_left = images_mask_p_left(randperm(length(images_mask_p_left)));
            stim_mask_p_left = images_mask_p_left(1:numTotalStim);
            stim_mask_p_left_blk  = stim_mask_p_left(randperm(length(stim_mask_p_left)));
            stim_mask_p_left_wht  = stim_mask_p_left(randperm(length(stim_mask_p_left)));
            stim_mask_p_left_oth  = stim_mask_p_left(randperm(length(stim_mask_p_left)));

            % practice arrays (faces, masks, inkblots for both right and left)
            prac_stim = {images_face_blk(numTotalStim+1), images_face_wht(numTotalStim+1), images_face_oth(numTotalStim+1)};
            prac_stim_ink = images_ink(numTotalStim+1:numTotalStim+3);
            prac_stim_mask_p_right = images_mask_p_right(numTotalStim+1:numTotalStim+3);
            prac_stim_mask_p_left = images_mask_p_left(numTotalStim+1:numTotalStim+3);
        
        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% CREATE STIMULI LIST FOR EACH BLOCK
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%            
            blk_stim_loc = 0;
            wht_stim_loc = 0;
            oth_stim_loc = 0;
            for blockCount = 1:numBlocks
                startLoc = ((blockCount-1)*numStim)+1;
                %faces
                stim_set_block = [stim_blk(startLoc:numStim*blockCount); stim_wht(startLoc:numStim*blockCount); stim_oth(startLoc:numStim*blockCount)];
                  %paths for inks & masks as well as preload of images for caching
                  for loopCnt = 1:numStim
                    blk_stim_loc = blk_stim_loc+1;
                    % black stim
                    curStimImage = imread([stim_set_block(loopCnt).('folder') filesep stim_set_block(loopCnt).('name')]);
                    stim_set_block(loopCnt).('stim_texture') = Screen('MakeTexture', wind, curStimImage);
                    % black ink
                    stim_set_block(loopCnt).('ink_name') = stim_ink_blk(blk_stim_loc).name;
                    stim_set_block(loopCnt).('ink_folder') = stim_ink_blk(blk_stim_loc).folder;
                    curInkImage = imread([stim_set_block(loopCnt).('ink_folder') filesep stim_set_block(loopCnt).('ink_name')]);
                    stim_set_block(loopCnt).('ink_texture') = Screen('MakeTexture', wind, curInkImage);
                    % black mask
                    if condition == 0
                       stim_set_block(loopCnt).('mask_name') = stim_mask_p_left_blk(blk_stim_loc).name;
                    else
                       stim_set_block(loopCnt).('mask_name') = stim_mask_p_right_blk(blk_stim_loc).name;
                    end
                    stim_set_block(loopCnt).('mask_folder') = stim_mask_p_right_blk(blk_stim_loc).folder;
                    curMaskImage = imread([stim_set_block(loopCnt).('mask_folder') filesep stim_set_block(loopCnt).('mask_name')]);
                    stim_set_block(loopCnt).('mask_texture') = Screen('MakeTexture', wind, curMaskImage);
                  end
                  for loopCnt = 1:numStim
                    wht_stim_loc = wht_stim_loc+1;
                    % white stim
                    curStimImage = imread([stim_set_block(loopCnt+numStim).('folder') filesep stim_set_block(loopCnt+numStim).('name')]);
                    stim_set_block(loopCnt+numStim).('stim_texture') = Screen('MakeTexture', wind, curStimImage);
                    % white ink
                    stim_set_block(loopCnt+numStim).('ink_name') = stim_ink_wht(wht_stim_loc).name;
                    stim_set_block(loopCnt+numStim).('ink_folder') = stim_ink_wht(wht_stim_loc).folder;
                    curInkImage = imread([stim_set_block(loopCnt+numStim).('ink_folder') filesep stim_set_block(loopCnt+numStim).('ink_name')]);
                    stim_set_block(loopCnt+numStim).('ink_texture') = Screen('MakeTexture', wind, curInkImage);
                    % white mask
                    if condition == 0
                    stim_set_block(loopCnt+numStim).('mask_name') = stim_mask_p_left_wht(wht_stim_loc).name;
                    else
                    stim_set_block(loopCnt+numStim).('mask_name') = stim_mask_p_right_wht(wht_stim_loc).name;
                    end
                    stim_set_block(loopCnt+numStim).('mask_folder') = stim_mask_p_right_wht(wht_stim_loc).folder;
                    curMaskImage = imread([stim_set_block(loopCnt+numStim).('mask_folder') filesep stim_set_block(loopCnt+numStim).('mask_name')]);
                    stim_set_block(loopCnt+numStim).('mask_texture') = Screen('MakeTexture', wind, curMaskImage);
                  end
                  for loopCnt = 1:numStim
                    oth_stim_loc = oth_stim_loc+1;
                    % other stim
                    curStimImage = imread([stim_set_block(loopCnt+numStim*2).('folder') filesep stim_set_block(loopCnt+numStim*2).('name')]);
                    stim_set_block(loopCnt+numStim*2).('stim_texture') = Screen('MakeTexture', wind, curStimImage);
                    % other ink
                    stim_set_block(loopCnt+numStim*2).('ink_name') = stim_ink_oth(oth_stim_loc).name;
                    stim_set_block(loopCnt+numStim*2).('ink_folder') = stim_ink_oth(oth_stim_loc).folder;
                    curInkImage = imread([stim_set_block(loopCnt+numStim*2).('ink_folder') filesep stim_set_block(loopCnt+numStim*2).('ink_name')]);
                    stim_set_block(loopCnt+numStim*2).('ink_texture') = Screen('MakeTexture', wind, curInkImage);
                    % other mask
                    if condition == 0
                    stim_set_block(loopCnt+numStim*2).('mask_name') = stim_mask_p_left_oth(oth_stim_loc).name;
                    else
                    stim_set_block(loopCnt+numStim*2).('mask_name') = stim_mask_p_right_oth(oth_stim_loc).name;
                    end
                    stim_set_block(loopCnt+numStim*2).('mask_folder') = stim_mask_p_right_oth(oth_stim_loc).folder;
                    curMaskImage = imread([stim_set_block(loopCnt+numStim*2).('mask_folder') filesep stim_set_block(loopCnt+numStim*2).('mask_name')]);
                    stim_set_block(loopCnt+numStim*2).('mask_texture') = Screen('MakeTexture', wind, curMaskImage);
                  end
                % randomize the block
                stim_set_block = stim_set_block(randperm(length(stim_set_block)));

                %add it to the block cell array
                stimBlocks{blockCount} = stim_set_block;
                
            end % end numBlocks for loop

           
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
        %%% INSTRUCTIONS
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Define Task Instructions  %this is what will be displayed to participants
            instructStr{1} = ['You will make 150 evaluative judgments of images. For each trial, you will first see a REAL-LIFE picture of a face.'...
            ' Then you will see an image of an inkblot drawing, followed by an image of black and white noise.'...
            ' Finally, you will make an evaluative judgment as to whether the inkblot drawing that you saw was "PLEASANT" or "UNPLEASANT".'];

            instructStr{2} = ['It is important to note that the real-life pictures can sometimes bias people''s judgments of the drawings.'...
            ' Because we are interested in how people can avoid being biased, please try your absolute best not to let the real-life picture'...
            ' bias your judgment of the drawings! Give us an honest assessment of the INKBLOT drawings, regardless of the images that precede them.'...
            ' ONLY CATEGORIZE THE INKBLOT IMAGES.'];

            if condition == 0
                instructStr{3} = ['When prompted, press ''E'' with your LEFT index finger if the inkblot was PLEASANT.'...
                ' Press ''I'' with your RIGHT index finger if the inkblot was UNPLEASANT'];
            else
                instructStr{3} = ['When prompted, press ''E'' with your LEFT index finger if the inkblot was UNPLEASANT.'...
                ' Press ''I'' with your RIGHT index finger if the inkblot was PLEASANT'];    
            end

            for loopCnt = 1:length(instructStr)

                DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, blk);
                DrawFormattedText(wind, instructStr{loopCnt}, 'center', 'center', blk, 45, [], [], 1.4);
                Screen('Flip',wind,[],1);

                WaitSecs(3);

                DrawFormattedText(wind, 'Press the space bar to continue when ready.', 'center', rect(4)*.9, blk);
                Screen('Flip', wind);

                while 1
                    [keyIsDown,~,keyCode] = KbCheck(-1);
                    if keyIsDown && any(keyCode(space_key_code))
                        DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, blk);
                        Screen('Flip', wind);
                        break
                    elseif keyIsDown && keyCode(esc_key_code)
                        error('Experiment aborted by user!');
                    end
                end
                
            end     % end for loop
    
            % Pause to check in after instructions
            Screen('FillRect', wind, gry);
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
            DrawFormattedText(wind, 'Practice', 'center', rect(4)*.1, blk);
            DrawFormattedText(wind, 'Before starting the experiment, you will have three practice trials.', 'center', 'center', blk, 45, [], [], 1.4);
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
            
            % change the background to white to match the stimuli
            Screen('FillRect', wind, wht);
            DrawFormattedText(wind, 'Starting...', 'center', 'center', blk); % % % GIVE THEM AN INDICATION OF HOW LONG UNTIL IT STARTS
            Screen('Flip', wind); 
            WaitSecs(1);
            
            for loopCnt = 1:3
                curStimImage = imread([prac_stim{loopCnt}.folder filesep prac_stim{loopCnt}.name]);
                curStimTexture = Screen('MakeTexture', wind, curStimImage);
                curInkImage = imread([prac_stim_ink(loopCnt).folder filesep prac_stim_ink(loopCnt).name]);
                curInkTexture = Screen('MakeTexture', wind, curInkImage);
                if condition == 0
                    % 'Pleasant' on left
                    curMaskImage = imread([prac_stim_mask_p_left(loopCnt).folder filesep prac_stim_mask_p_left(loopCnt).name]);
                else
                    curMaskImage = imread([prac_stim_mask_p_right(loopCnt).folder filesep prac_stim_mask_p_right(loopCnt).name]);
                end
                curMaskTexture = Screen('MakeTexture', wind, curMaskImage);
                
                Screen('DrawTexture', wind, curStimTexture);
                Screen('Flip', wind);
                startTime = GetSecs;
                while (GetSecs-startTime) < (0.075)
                    % do nothing
                end
                
                Screen('DrawTexture', wind, curInkTexture);
                Screen('Flip', wind);
                startTime = GetSecs;
                while (GetSecs-startTime) < (0.125)
                    % do nothing
                end
                
                Screen('DrawTexture', wind, curMaskTexture);
                Screen('Flip', wind);
                % timestamp
                startTime = GetSecs;
                
                while 1
                    [keyIsDown,~,keyCode] = KbCheck(-1);
                    if keyIsDown
                        if any(keyCode(resp_key_codes))
                            theKeyResponse = KbName(keyCode);
                            % pause here until the key is up
                            while 1
                                [keyIsDown] = KbCheck(-1);
                                if keyIsDown == 0
                                    break;
                                end
                            end
                            % timestamp
                            endTime = GetSecs;
                            % block number
                            subjData(loopCnt).blockNum = 'P';
                            % trial number
                            subjData(loopCnt).trialNum = loopCnt;
                            subjData(loopCnt).cumTrialNum = loopCnt; % % % DESPITE HAVING A DIFFERENT STORAGE LOCATION, THESE ARE THE SAME... 
                            % stimulus
                            subjData(loopCnt).stimulus = prac_stim{loopCnt}.name;
                            subjData(loopCnt).inkStimulus = prac_stim_ink(loopCnt).name;
                            if condition == 0
                                subjData(loopCnt).maskStimulus = prac_stim_mask_p_left(loopCnt).name;
                            else
                                subjData(loopCnt).maskStimulus = prac_stim_mask_p_right(loopCnt).name;
                            end
                            % response
                            % 'PLEASANT' is 0, 'UNPLEASANT' is 1
                            if condition == 0
                                % 'PLEASANT' is on the left ('e' response)
                                if theKeyResponse == 'e'
                                    subjData(loopCnt).response = 0;
                                else
                                    subjData(loopCnt).response = 1;
                                end
                            else
                                % 'PLEASANT' is on the right ('i' response)
                                if theKeyResponse == 'e'
                                    subjData(loopCnt).response = 1;
                                else
                                    subjData(loopCnt).response = 0;
                                end
                            end
                            % RT
                            subjData(loopCnt).RT = endTime-startTime;

                            % Save out data for this trial
                            fprintf(subjFile,'%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%i\t%f\n',...
                                subjID, num2str(condition), subjData(loopCnt).blockNum, num2str(subjData(loopCnt).trialNum, '%02i'), num2str(subjData(loopCnt).cumTrialNum, '%02i'), subjData(loopCnt).stimulus, subjData(loopCnt).inkStimulus, subjData(loopCnt).maskStimulus, subjData(loopCnt).response, subjData(loopCnt).RT);

                            break
                        elseif keyCode(esc_key_code)
                            error('Experiment aborted by user!');
                        end   % end keyCode check
                    end   % end 'keyIsDown' check
                end   % end loop that waits for keypress
                
            end   % end loop through all three practice trials
                 

        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% START EXPERIMENT
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % Pause to check in after practice
            Screen('FillRect', wind, gry);
            DrawFormattedText(wind, 'Practice Finished', 'center', rect(4)*.1, blk);
            DrawFormattedText(wind, ['You''ve completed the practice trials. If you have any questions, please discuss them'...
                ' with the experimenter before starting the experiment.'], 'center', 'center', blk, 45, [], [], 1.4);
            Screen('Flip',wind,[],1);
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
            
            % Allow the participant to start the task when they are ready
            DrawFormattedText(wind, 'Press the space bar to start the experiment.', 'center', rect(4)*.9, blk);
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

            for curBlock = 1:length(stimBlocks)
                % change the background to white to match the stimuli
                Screen('FillRect', wind, wht);
                DrawFormattedText(wind, 'Starting...', 'center', 'center', blk);
                Screen('Flip', wind);
                WaitSecs(1);
                
                for loopCnt = 1:numStim*3
                    Screen('DrawTexture', wind, stimBlocks{curBlock}(loopCnt).stim_texture);
                    Screen('Flip', wind);
                    startTime = GetSecs;
                    while (GetSecs-startTime) < (0.075)
                        % do nothing
                    end
                    Screen('DrawTexture', wind, stimBlocks{curBlock}(loopCnt).ink_texture);
                    Screen('Flip', wind);
                    startTime = GetSecs;
                    while (GetSecs-startTime) < (0.125)
                        % do nothing
                    end
                    Screen('DrawTexture', wind, stimBlocks{curBlock}(loopCnt).mask_texture);
                    Screen('Flip', wind);
                    
                    % timestamp
                    startTime = GetSecs;

                    while 1
                        [keyIsDown,~,keyCode] = KbCheck(-1);
                        if keyIsDown && any(keyCode(resp_key_codes))
                            theKeyResponse = KbName(keyCode);
                            % pause here until the key is up
                            while 1
                                [keyIsDown] = KbCheck(-1);
                                if keyIsDown == 0
                                    break;
                                end
                            end
                            % timestamp
                            endTime = GetSecs;
                            % get the location for saving
                            cellSaveLoc = loopCnt+((curBlock-1)*numStim*3);
                            % subject ID and condition
                            subjData(cellSaveLoc).ID = subjID;
                            % % % THE WAY THINGS ARE BEING STORED HERE
                            % MAKES THEM HARD TO USE/ANALYZE. LET'S TALK
                            % ABOUT TAKING A MORE MATRIX-BASED APPROACH TO
                            % STORING THE OUTPUT DATA.
                            subjData(cellSaveLoc).condition = condition;
                            % block number
                            subjData(cellSaveLoc).blockNum = curBlock;
                            % trial number
                            subjData(cellSaveLoc).trialNum = loopCnt;
                            subjData(cellSaveLoc).cumTrialNum = loopCnt+(numStim*3*(curBlock-1));
                            % stimulus
                            subjData(cellSaveLoc).stimulus = stimBlocks{curBlock}(loopCnt).name;
                            subjData(cellSaveLoc).inkStimulus = stimBlocks{curBlock}(loopCnt).ink_name;
                            subjData(cellSaveLoc).maskStimulus = stimBlocks{curBlock}(loopCnt).mask_name;
                            % response
                            % 'PLEASANT' is 0, 'UNPLEASANT' is 1
                            if condition == 0
                                % 'PLEASANT' is on the left ('e' response)
                                if theKeyResponse == 'e'
                                    subjData(cellSaveLoc).response = 0;
                                else
                                    subjData(cellSaveLoc).response = 1;
                                end
                            else
                                % 'PLEASANT' is on the right ('i' response)
                                if theKeyResponse == 'e'
                                    subjData(cellSaveLoc).response = 1;
                                else
                                    subjData(cellSaveLoc).response = 0;
                                end
                            end
                            % RT
                            subjData(cellSaveLoc).RT = endTime-startTime;

                            % Save out data for this trial
                            fprintf(subjFile,'%s\t%s\t%i\t%s\t%s\t%s\t%s\t%s\t%i\t%f\n',...
                                subjID, num2str(condition), subjData(cellSaveLoc).blockNum, num2str(subjData(cellSaveLoc).trialNum, '%02i'), num2str(subjData(cellSaveLoc).cumTrialNum, '%02i'),...
                                subjData(cellSaveLoc).stimulus, subjData(cellSaveLoc).inkStimulus, subjData(cellSaveLoc).maskStimulus, subjData(cellSaveLoc).response, subjData(cellSaveLoc).RT);

                            break
                        elseif keyIsDown && keyCode(esc_key_code)
                            error('Experiment aborted by user!');
                        end
                    end   % end while loop

                end   % end stim 'for' loop
                if curBlock < length(stimBlocks)
                    DrawFormattedText(wind, sprintf('You''ve completed block %i of %i.\nPlease take a moment to rest your eyes.\nThe next block will starting in 5 seconds...', curBlock, length(stimBlocks)), 'center', 'center', blk, [], [], [], 1.4); % % % "WILL STARTING IN"
                    Screen('Flip', wind);
                    WaitSecs(5);
                end
            
            end % end block 'for' loop

            
        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% END SCREEN
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            % show end text
            Screen('FillRect', wind, gry);
            DrawFormattedText(wind, 'You''ve completed the task.\nPlease ring the bell to inform the experimenter!', 'center', 'center', blk, 45, [], [], 1.4);
            Screen('Flip', wind);
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
            
            % save data
            save([outputpath, 'amptask_subj' subjID '_' num2str(now) '.mat'], 'subjData');
            fclose(subjFile);

            % clean-up
            ListenChar(0);
            sca;     % close the screen
            Priority(0);    % resets window priority level (gives control back to user)

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
            save([outputpath, 'amptask_subj' subjID '_' num2str(now) '.mat'], 'subjData');
        end

        ListenChar(0);
        Priority(0);
        sca;
        rethrow(ME)
        
    end % end try
    
end % end function