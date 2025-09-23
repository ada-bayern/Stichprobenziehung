# Bundling Shiny Apps as a Windows Executable

How to create exe-files to distribute your Shiny app to lazy Windows users

1.  Setup [R-Portable](https://sourceforge.net/projects/rportable/)

2.  Open portable R version at "...\R-Portable\bin\R.exe" and install your dependencies, e.g., with a script like `install_deps.R`.

3.  Install *Inno Setup*

```{r}
install.packages("installr")
library(installr)

install.inno()
```

4.  Create `run_app.bat`

```{batch}
@echo off

set "RAPPDIR=%~dp0"
set "RAPPDIR=%RAPPDIR:\=/%"

echo %RAPPDIR%

cd "%~dp0R-Portable\bin"
Rscript.exe -e "shiny::runApp('%RAPPDIR%app', launch.browser = TRUE)"
```

`~dp0` is a magic command for your current working directory. `RAPPDIR` stores this path with flipped separators for R.

5.  Create `install.iss` for your app

```{pascal}

[Setup]
AppName=MyApp
AppVersion=1.0
WizardStyle=modern
DefaultDirName={autopf}\MyApp
DefaultGroupName=MyApp
Compression=lzma2
SolidCompression=yes
OutputDir=dist
OutputBaseFilename=MyApp_Installer

[Files]
Source: "run_app.bat"; DestDir: "{app}"; Flags: ignoreversion
Source: "app\*"; DestDir: "{app}\app"; Flags: recursesubdirs ignoreversion
Source: "R-Portable\*"; DestDir: "{app}\R-Portable"; Flags: recursesubdirs ignoreversion

[Icons]
Name: "{group}\MyApp"; Filename: "{app}\run_app.bat"
Name: "{commondesktop}\MyApp"; Filename: "{app}\run_app.bat"; Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "Create a desktop icon"; GroupDescription: "Additional icons:"
```

6.  Organize everything in this structure:

```
/
├── app/            # source directory
├── R-Portable
├── install.iss     # inno setup script
└── run_app.bat     # this script is what the exe is going to do
```

7.  Compile `install.iss` in *Inno Setup* (accessible via Window Menu)
