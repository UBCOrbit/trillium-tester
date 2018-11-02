#define MyAppName "trillium-tester"
#define MyAppVersion "0.1.0.0"
#define MyAppPublisher "UBC Orbit"
#define MyAppURL "https://www.ubcorbit.com/"
#define MyAppExeName "trillium.exe"

[Setup]
AppId={{C50357EA-C85B-4EBF-BF87-3AF6BEAB0322}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
DisableProgramGroupPage=yes
OutputBaseFilename=trillium-tester-installer
Compression=lzma
SolidCompression=yes
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
PrivilegesRequired = admin
MinVersion = 5.5

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "trillium.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "libusb-1.0.dll"; DestDir: "{app}"; Flags: ignoreversion
Source: "wdi-simple.exe"; DestDir: "{app}"; Flags: replacesameversion promptifolder;

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"

[Run]
Filename: "{app}\wdi-simple.exe"; Flags: "runhidden"; Parameters: " --name HSTLink --vid 0x0483 --pid 0x374b -t 1 --progressbar={wizardhwnd} --timeout 120000"; StatusMsg: "Installing USB driver...";