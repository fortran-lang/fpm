; NSIS Installer script for the Fortran Package Manager

; ---------------- Properties ----------------
; Name used in installer GUI
Name "Fortran Package Manager"

; Name for folder location and reg key
!define INSTALL_NAME "fortran-lang"

; Installer icon
!define MUI_ICON "installer-icon.ico"

; Compress installer
SetCompress auto

; Always produce unicode installer
Unicode true

; ---------------- Setup ----------------
; Use EnVar plugin (https://nsis.sourceforge.io/EnVar_plug-in)
!addplugindir ".\EnVar_plugin\Plugins\x86-unicode"

; Use the 'Modern' Installer UI macros
!include "MUI2.nsh"

; Default installation folder (local)
InstallDir "$LOCALAPPDATA\${INSTALL_NAME}"
  
; Get installation folder from registry if available
InstallDirRegKey HKCU "Software\${INSTALL_NAME}" ""

; Request application privileges
RequestExecutionLevel user


; ---------------- Installer Pages ----------------
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES


; ---------------- Uninstaller Pages ----------------
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES

  
; MUI Language
!insertmacro MUI_LANGUAGE "English"


; ---------------- Component: Core Installation ----------------
Section "-Core" SecCore

  SetOutPath "$INSTDIR"

  ; Store installation folder
  WriteRegStr HKCU "Software\${INSTALL_NAME}" "" $INSTDIR
  
  ; Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; Add to path
  EnVar::SetHKCU
  EnVar::AddValue "PATH" "$INSTDIR\fpm"
  EnVar::AddValue "PATH" "$INSTDIR\MinGit\mingw64\bin"

SectionEnd


; ---------------- Component: fpm ----------------
Section "FPM" SecFPM

  SetOutPath "$INSTDIR\fpm"
  
  File "fpm.exe"

SectionEnd


; ---------------- Component: Git ----------------
Section "Git for Windows" SecGit

  SetOutPath "$INSTDIR"
  
  File /r "MinGit"

SectionEnd


; ---------------- Uninstaller ----------------
Section "Uninstall"

  RMDir /r "$INSTDIR"

  DeleteRegKey /ifempty HKCU "Software\${INSTALL_NAME}"

  EnVar::SetHKCU
  EnVar::DeleteValue "PATH" "$INSTDIR\fpm"
  EnVar::DeleteValue "PATH" "$INSTDIR\MinGit\mingw64\bin"

SectionEnd


; ---------------- Component description Strings (EN) ----------------
LangString DESC_SecFPM ${LANG_ENGLISH} "The Fortran Package Manager"
LangString DESC_SecGit ${LANG_ENGLISH} "Git version control (required for FPM)"


!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  !insertmacro MUI_DESCRIPTION_TEXT ${SecFPM} $(DESC_SecFPM)
  !insertmacro MUI_DESCRIPTION_TEXT ${SecGit} $(DESC_SecGit)
!insertmacro MUI_FUNCTION_DESCRIPTION_END
