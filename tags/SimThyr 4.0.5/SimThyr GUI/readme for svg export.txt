The following steps should be sufficient for enabling SVG export on builds for mac OS:

"Please open your tafonts.pas in the TAChart installation directory, and find the procedure PopulateFontDirList. You will see {$IFDEF WINDOWS} and {$IFDEF LINUX}. If there is no {$IFDEF LCLCarbon} and not {$IFDEF LCLCocoa} you should add the following lines to this procedure, immediately before the "end;":

 {$IFDEF LCLCarbon}
  AList.Add('/Library/Fonts/');
  AList.Add('/System/Library/Fonts/');
  AList.Add('/Network/Library/Fonts/');
  AList.Add('~/Library/Fonts/');
 {$ENDIF}
 {$IFDEF LCLCocoa}
  AList.Add('/Library/Fonts/');
  AList.Add('/System/Library/Fonts/');
  AList.Add('/Network/Library/Fonts/');
  AList.Add('~/Library/Fonts/');
 {$ENDIF}
 
 Suggested by wp at https://forum.lazarus.freepascal.org/index.php/topic,54623