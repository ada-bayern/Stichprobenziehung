[Setup]
AppName=ADA Stichproben
AppVersion=1.0
WizardStyle=modern
DefaultDirName={autopf}\ADA Stichproben
DefaultGroupName=ADA Stichproben
Compression=lzma2
SolidCompression=yes
OutputDir=.
OutputBaseFilename=ADA_Stichproben_Setup

[Languages]
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Files]
Source: "run_app.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "app\*"; DestDir: "{app}\app"; Flags: recursesubdirs ignoreversion
Source: "R-Portable\*"; DestDir: "{app}\R-Portable"; Flags: recursesubdirs ignoreversion

[Icons]
Name: "{group}\ADA Stichproben"; Filename: "{app}\run_app.bat"
Name: "{commondesktop}\ADA Stichproben"; Filename: "{app}\run_app.bat"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Additional icons:"
