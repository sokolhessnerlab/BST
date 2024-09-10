function [subjData] = TRv1(subjID, testMode, condition)
    %
    % subjID must be a 3-character string (e.g. '003')
    % testMode must be either 0 (do the full study) or 1 (do an abbreviated study)
    % 
    % DATA:
    %
    % set up defaults
    Screen('Preference', 'SkipSyncTests', 1);   % skips sync tests for monitor relay timing (*ideally don't use this, mostly for use during testing w/ dual monitor setup)
    if nargin < 3
        condition = 2; % assume default condition of second task (unless otherwise specified)
    end
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
            2018.07.31 - ESA created file
        %}

    %%
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% PREPARATION & GLOBAL VARS
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Create Experiment Window
        %[wind, rect] = Screen('OpenWindow', max(Screen('Screens')));
        rect=[1376 0 2400 768];
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

        % set up file paths % % % THE FOLDER FOR THIS SHOULD BE THE ACRONYM OF THE PROJECT (E.G. BST)
        % homepath = [filesep 'Volumes' filesep 'research' filesep 'Projects' filesep 'racial bias' filesep 'task' filesep];
        homepath = [filesep 'Volumes' filesep 'research' filesep 'AHSS Psychology' filesep 'shlab' filesep 'Projects' filesep 'BST' filesep 'task' filesep];
        imgpath = ['images' filesep 'TRTG' filesep];
        outputpath = ['output' filesep];

        % basic keyboard prep
        KbName('UnifyKeyNames');    % for OS X

        % Define Response Keys
        resp_keys = {'1', '2', '3', '4', '5', '6', '7', '8', '9', '1!', '2@', '3#', '4$', '5%', '6^', '7&', '8*', '9('};
        resp_key_codes = KbName(resp_keys);
        space_key_code = KbName('space');
        esc_key_code = KbName('ESCAPE');
        trig_key_code = KbName('Return');

        % set up the number of stimuli to choose from
        numTotalStim = 50;

        % set up the number of stimuli (per 3 stimuli groups)
        if testMode == 1
            numTRStim = 10;
            numTRBlocks = 2;
        else
            % For TR the product of stim*blocks should equal 142
            numTRStim = 71;
            numTRBlocks = 2;
            HideCursor;
        end

        % set up the data struct for the subject
        subjData = struct;
        subjData.ID = subjID;
        subjData.condition = condition;
        subjData.blockNum = [];
        subjData.trialNum = [];
        subjData.cumTrialNum = [];
        subjData.stimulus = [];
        subjData.response = [];
        subjData.RT = [];

        % set up the path to the file on disk
        subjFile = fopen([homepath outputpath 'tmp_trtask_subj' subjID '_' num2str(now) '.txt'],'w');
        fprintf(subjFile,'SUBJECT ID\tCONDITION\tBLOCK\tTRIAL NUM\tCUMULATIVE TRIAL NUM\tSTIM\tRESPONSE\tRT\n');

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
        %%% Trust Rating Task
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        %%% LOAD STIMULI PATHS INTO ARRAYS, RANDOMIZE, AND SELECT SUBSETS
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            DrawFormattedText(wind, 'Loading stimuli...', 'center', 'center', blk);
            Screen(wind,'Flip');
            % black faces
            images_rating_blk = dir([imgpath 'TR/black*.png']);
            % white faces
            images_rating_wht = dir([imgpath 'TR/white*.png']);
            % other faces
            images_rating_oth = dir([imgpath 'TR/other*.png']);
        
            % faces (50 (by default) of each, except 42 of 'other')
            stim_blk = images_rating_blk(randperm(length(images_rating_blk), numTotalStim));
            stim_wht = images_rating_wht(randperm(length(images_rating_wht), numTotalStim));
            stim_oth = images_rating_oth(randperm(length(images_rating_oth), min(numTotalStim, 42)));
            TR_stim = [stim_blk; stim_wht; stim_oth];
            TR_stim = TR_stim(randperm(length(TR_stim)));
            
            % separate the stimuli into blocks
            for blockCount = 1:numTRBlocks
                startLoc = ((blockCount-1)*numTRStim)+1;
                % break up the face stimuli and put it into a temporary block
                stim_set_block = TR_stim(startLoc:(startLoc-1+numTRStim));
                
                % go through each of the stimuli and add in an image
                % texture so that we can load it quickly later on
                for loopCnt = 1:numTRStim
                    curStimImage = imread([stim_set_block(loopCnt).('folder') filesep stim_set_block(loopCnt).('name')]);
                    stim_set_block(loopCnt).('stim_texture') = Screen('MakeTexture', wind, curStimImage);
                end
                
                %add it to the block cell array
                stimBlocks{blockCount} = stim_set_block;
            end
            
           
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
            instructStr{1} = ['In this task, you will be presented with many pictures of male faces and a rating scale.\n\nFor example:'];
            instructStr{2} = ['For each face, please rate how trustworthy the person looks to you on a scale of 1 (not-at-all trustworthy) to 9 (extremely trustworthy)'...
            '\n\nUse the number keys on the keyboard to select your rating from 1 to 9.'];

            for loopCnt = 1:length(instructStr)

                DrawFormattedText(wind, 'Instructions', 'center', rect(4)*.1, blk);
                DrawFormattedText(wind, instructStr{loopCnt}, 'center', rect(4)*.2, blk, 50, [], [], 1.4);
                if loopCnt == 1
                    curStimImage = imread([TR_stim(length(TR_stim)).('folder') filesep TR_stim(length(TR_stim)).('name')]);
                    Screen('DrawTexture', wind, Screen('MakeTexture', wind, curStimImage), [], [((rect(3)-rect(1))/2)-250 rect(4)*.40 ((rect(3)-rect(1))/2)+250 (rect(4)*.40)+300]);
                end
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
        %%% START EXPERIMENT
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        
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
                
                for loopCnt = 1:numTRStim
                    % show the stimulus
                    Screen('DrawTexture', wind, stimBlocks{curBlock}(loopCnt).stim_texture);
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
                            cellSaveLoc = loopCnt+((curBlock-1)*numTRStim);
                            % subject ID and condition
                            subjData(cellSaveLoc).ID = subjID;
                            subjData(cellSaveLoc).condition = condition;
                            % block number
                            subjData(cellSaveLoc).blockNum = curBlock;
                            % trial number
                            subjData(cellSaveLoc).trialNum = loopCnt;
                            subjData(cellSaveLoc).cumTrialNum = loopCnt+(numTRStim*(curBlock-1));
                            % stimulus
                            subjData(cellSaveLoc).stimulus = stimBlocks{curBlock}(loopCnt).name;
                            % response
                            subjData(cellSaveLoc).response = theKeyResponse(1);
                            % RT
                            subjData(cellSaveLoc).RT = endTime-startTime;

                            % Save out data for this trial
                            fprintf(subjFile,'%s\t%s\t%i\t%s\t%s\t%s\t%s\t%f\n',...
                                subjID, num2str(condition), subjData(cellSaveLoc).blockNum, num2str(subjData(cellSaveLoc).trialNum, '%02i'), num2str(subjData(cellSaveLoc).cumTrialNum, '%02i'),...
                                subjData(cellSaveLoc).stimulus, num2str(subjData(cellSaveLoc).response), subjData(cellSaveLoc).RT);

                            % ITI
                            startTime = GetSecs;
                            while (GetSecs-startTime) < (0.100)
                                % do nothing
                            end
                            break;
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
            save([outputpath, 'trtask_subj' subjID '_' num2str(now) '.mat'], 'subjData');
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
            save([outputpath, 'trtask_subj' subjID '_' num2str(now) '.mat'], 'subjData');
        end

        ListenChar(0);
        Priority(0);
        sca;
        rethrow(ME)
        
    end % end try
    
end % end function