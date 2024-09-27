function fixationCross()

oldVisualDebugLevel = Screen('Preference', 'VisualDebugLevel', 3);
oldSupressAllWarnings = Screen('Preference', 'SuppressAllWarnings', 1);

screenNum=0;
[window, rect] = Screen('OpenWindow', screenNum, 1);
[X,Y] = RectCenter(rect);
FixCross = [X-1,Y-40,X+1,Y+40;X-40,Y-1,X+40,Y+1];
Screen('FillRect', window, [255,255,255], FixCross');
Screen('Flip', window);
WaitSecs(2)