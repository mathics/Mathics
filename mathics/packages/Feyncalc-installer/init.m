

Unprotect[$VersionNumber];
$VersionNumber=10;
Protect[$VersionNumber];

myPath = FileNameJoin[{"/home/mauricio/Projects/mathics/mathics", "packages/Feyncalc-installer"}];
$PathToFCArc = FileNameJoin[{myPath,"feyncalc-hotfix-stable.zip"}];
$PathToFAArc = FileNameJoin[{myPath,"feynarts-mirror-master.zip"}];
Get[FileNameJoin[{myPath,"install.m"}]]


InstallFeynCalc[]
