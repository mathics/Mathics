(* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ *)

(* :Title: install															*)

(*
	This software is covered by the GNU General Public License 3.
	Copyright (C) 1990-2021 Rolf Mertig
	Copyright (C) 1997-2021 Frederik Orellana
	Copyright (C) 2014-2021 Vladyslav Shtabovenko
*)

(* :Summary:  Installs FeynCalc and FeynArts *)

(* ------------------------------------------------------------------------ *)

If[	!FreeQ[$ContextPath,"WolframLanguageForJupyter`"],
			Print["It seems that your are trying to install FeynCalc from a ",
				"Wolfram Language kernel for Jupyter notebooks.",
				"Unfortunately, graphical installation using a Jupyter frontend is currently not possible.",
				"If you only have access to the Free Wolfram Engine, please start the kernel with a text-based interface",
				"and run the installer again.", "\n\nInstallation aborted!"];
			Abort[]
];

BeginPackage["FeynCalcInstaller`"];

InstallFeynCalc::notcomp =
"Your Mathematica version is too old. FeynCalc requires at least Mathematica 8. Installation aborted!";

InstallFeynCalc::failed =
"Download of `1` failed. Installation aborted!";

InstallFeynCalcQuiet::usage="InstallFeynCalcQuiet is the silent mode of installing FeynCalc, where the \
installer does not ask you any questions but silently overwrites any existing FeynCalc installation and \
modifies Mathematica's options accordingly. FeynArts is not installed. The main purpose of this mode is \
to facilitate the installation of FeynCalc on Mathematica Online.";

AutoEnableTraditionalForm::usage="AutoEnableTraditionalForm is an option of InstallFeynCalc. If \
set to True, the format type of new output cells will be set to TraditionalForm. False means that \
the current value will not be changed.";

AutoOverwriteFeynCalcDirectory::usage="AutoOverwriteFeynCalcDirectory is an option of InstallFeynCalc. If \
set to True, the existing FeynCalc directory will be deleted without any further notice. The default \
value None means that the user will be asked by a dialog. False means that the directory will be overwritten.";

AutoDisableInsufficientVersionWarning::usage="AutoDisableInsufficientVersionWarning is an option of InstallFeynCalc. If \
set to True, warning messages for notebooks that were created with a newer Mathematica version will be silently disabled. \
This is needed to use FeynCalc documentation in Mathematica 8 and 9, since otherwise the warning message will appear every \
time one opens a help page for a FeynCalc function. The default value None means that the user will be asked by a dialog. \
False means that the warning will not be disabled.";

FeynArtsMirrorLink::usage="FeynArtsMirrorLink is an option of InstallFeynCalc. It specifies the url \
to the mirror repository of FeynArts. This repository is maintained by FeynCalc developers and tries to follow \
the development of FeynArts using git. It is also used to install FeynArts with InstallFeynCalc.";

AutoInstallFeynArts::usage="AutoInstallFeynArts is an option of InstallFeynCalc. If \
set to True, FeynArts will be installed automatically.";

FeynCalcDevelopmentVersionLink::usage="FeynCalcDevelopmentVersionLink is an option of InstallFeynCalc. It specifies the url \
to the main repository of FeynCalc. This repository is used to install the development version of FeynCalc.";

FeynCalcStableVersionLink::usage="FeynCalcStableVersionLink is an option of InstallFeynCalc. It specifies the url \
to the latest stable release of FeynCalc.";

InstallFeynCalcDevelopmentVersion::usage="InstallFeynCalcDevelopmentVersion is an option of InstallFeynCalc. If \
set to True, the installer will download the latest development version of FeynCalc from the git repository. \
Otherwise it will install the latest stable version.";

InstallFeynCalcTo::usage="InstallFeynCalcTo is an option of InstallFeynCalc. It specifies, the full path \
to the directory where FeynCalc will be installed.";

InstallFeynArtsTo::usage="InstallFeynArtsTo is an option of InstallFeynArts. It specifies, the full path \
to the directory where FeynArts will be installed.";

$PathToFCArc::usage="$PathToFCArc specifies where the installer should look for the zipped FeynCalc version. \
If the value is not empty, the installer will use the specified file instead of downloading it from the official \
website."

$PathToFAArc::usage="$PathToFAArc specifies where the installer should look for the FeynArts tarball. \
If the value is not empty, the installer will use the specified file instead of downloading it from the official \
website."

Begin["`Private`"]

testConnection::usage="";

If[ !ValueQ[$PathToFCArc],
	$PathToFCArc = ""
];

If[ !ValueQ[$PathToFAArc],
	$PathToFAArc = ""
];

If[	$VersionNumber < 8,
	Message[InstallFeynCalc::notcomp];
	Abort[]
];


If[	8.0 <=$VersionNumber < 9.0,
	(*To use FetchURL in MMA8 we need to load URLTools first *)
	Needs["Utilities`URLTools`"];
];

Which[
	(*Mma 8*)
	8.0 <=$VersionNumber < 9.0,
		(*To use FetchURL we need to load URLTools first *)
		FCGetUrl[x_]:= Utilities`URLTools`FetchURL[x],
	(*Mma 9 or 10 *)
	9.0 <=$VersionNumber < 11.0,
		FCGetUrl[x_]:= URLSave[x,CreateTemporary[]],
	(*Mma 11 and above *)
	$VersionNumber >= 11.0,
		FCGetUrl[x_]:= First[URLDownload[x,CreateTemporary[]]]
];

If[ $PathToFCArc==="",
	(*Test that we can access the FeynCalc repository*)
	Quiet[testConnection = FCGetUrl["https://github.com/FeynCalc/feyncalc"];];
	If[	testConnection===$Failed || !FileExistsQ[testConnection],
		WriteString["stdout",
			"It seems that your Mathematica version is unable to ",
			"connect to the FeynCalc repository on GitHub.\n",
			"This might be a network connectivity problem or an issue with Mathematica.\n",
			"Especially some older versions of Mathematica (8, 9 or 10) and known to cause such problems\n",
			"on recent versions of Linux, MacOS and Windows when accessing SSL-encrypted urls.\n\n",
			"Please check the wiki <https://github.com/FeynCalc/feyncalc/wiki/Installation> for ",
			"possible workarounds.\n",
			"Notice that it is also possible to download all the necessary files by hand and install FeynCalc\n",
			"without an existing internet connection. The required steps are described in the wiki.", "\n\nInstallation aborted!"
		];
			Abort[]
	];
];

fancyText[Column[li_List]] :=
	Column[(TextCell[#, "Text"] & /@ li)] /; $Notebooks

fancyText[x_] :=
	x /; !$Notebooks;

choiceDialog2[x__] :=
	ChoiceDialog[x]/; $Notebooks;

choiceDialog2[text_,rest__] :=
	(
	WriteString["stdout","\n\n"];
	MessageDialog[text];
	ChoiceDialog["",rest]
	)/; !$Notebooks;

(*Greeter*)
Print["Welcome to the automatic FeynCalc installer brought to you by the FeynCalc developer team!"];
Print[" \[Bullet] To install the current stable version of FeynCalc (recommended for productive use), please evaluate"];
Print["\t", If[$Notebooks,TextCell["InstallFeynCalc[]", "Code"],"InstallFeynCalc[]"]];
Print[" \[Bullet] To install the development version of FeynCalc (only for experts or beta testers), please evaluate "];
Print["\t", If[$Notebooks,TextCell["InstallFeynCalc[InstallFeynCalcDevelopmentVersion->True]", "Code"],
	"InstallFeynCalc[InstallFeynCalcDevelopmentVersion->True]" ]];

Options[InstallFeynCalc] = {
	AutoDisableInsufficientVersionWarning	-> None,
	AutoEnableTraditionalForm 				-> None,
	AutoInstallFeynArts						-> None,
	AutoOverwriteFeynCalcDirectory			-> None,
	FeynCalcDevelopmentVersionLink			-> "https://github.com/FeynCalc/feyncalc/archive/master.zip",
	FeynCalcStableVersionLink				-> "https://github.com/FeynCalc/feyncalc/archive/hotfix-stable.zip",
	InstallFeynCalcDevelopmentVersion		-> False,
	InstallFeynCalcTo						-> FileNameJoin[{$UserBaseDirectory, "Applications","FeynCalc"}]
};

Options[InstallFeynCalcQuiet]=
	Options[InstallFeynCalc];

Options[InstallFeynArts] = {
	FeynArtsMirrorLink	-> "https://github.com/FeynCalc/feynarts-mirror/archive/master.zip",
	InstallFeynArtsTo	-> FileNameJoin[{$UserBaseDirectory, "Applications","FeynCalc","FeynArts"}]
};




InstallFeynArts[OptionsPattern[]]:=
	Module[{tmpzip,fazip,unzipDir,faDir},
		(* Install FeynArts	*)

		faDir	= OptionValue[InstallFeynArtsTo];
		fazip 	= OptionValue[FeynArtsMirrorLink];

		(* Download FeynArts tarball	*)
		If[ $PathToFAArc=!="",
			tmpzip = $PathToFAArc;
			WriteString["stdout", "Installing FeynArts from ", tmpzip," ..."],
			WriteString["stdout", "Downloading FeynArts from ", fazip," ..."];
			tmpzip=FCGetUrl[fazip];
		];
		unzipDir= tmpzip<>".dir";
		WriteString["stdout", "done! \n"];

		(* Extract to the content	*)
		WriteString["stdout", "FeynArts zip file was saved to ", tmpzip,".\n"];
		WriteString["stdout", "Extracting FeynArts zip file to ", faDir, " ..."];
		ExtractArchive[tmpzip, unzipDir];
		WriteString["stdout", "done! \n"];

		(* Move the files to the final destination	*)
		WriteString["stdout", "Copying FeynArts to ", faDir, " ..."];
		CopyDirectory[FileNameJoin[{unzipDir,"feynarts-mirror-master"}],faDir];
		WriteString["stdout", "done! \n"];

		(* Delete the downloaded file	*)
		If[ $PathToFAArc==="",
			Quiet@DeleteFile[tmpzip];
		];

		(* Delete the extracted archive *)
		Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];

	];

InstallFeynCalcQuiet[]:=
	InstallFeynCalc[
		AutoDisableInsufficientVersionWarning-> True,
		AutoEnableTraditionalForm -> True,
		AutoInstallFeynArts-> False,
		AutoOverwriteFeynCalcDirectory-> True
	];

InstallFeynCalc[OptionsPattern[]]:=
	Module[	{unzipDir, tmpzip, gitzip, packageName, packageDir, fullPath,
			strFeynArts, configFileProlog, strOverwriteFC,
			strDisableWarning, strEnableTraditionalForm, faInstalled, zipDir,
			useTraditionalForm, configFile},

		If[	OptionValue[InstallFeynCalcDevelopmentVersion],
			gitzip = OptionValue[FeynCalcDevelopmentVersionLink],
			gitzip = OptionValue[FeynCalcStableVersionLink]
		];

		faInstalled			=	False;
		useTraditionalForm	=	False;
		packageName 		= "FeynCalc";
		packageDir 			=	OptionValue[InstallFeynCalcTo];

		strDisableWarning =
			Column[{
				StringJoin["To make the documentation work, we need to disable the warning ",
					"that appears when you open a notebook that was created with a newer Mathematica version."],
				" ",
				StringJoin["Otherwise this warning will appear every time you use the Documentation Center ",
					"to check FeynCalc documentation in Mathematica 8 and 9."],
				" ",
				"This setting is harmless and can be always undone via",
				" ",
				"\"SetOptions[$FrontEnd, MessageOptions -> \ {\"InsufficientVersionWarning\" -> True}]\"",
				"",
				"Should we do this now?"}
			];


		strEnableTraditionalForm =
			Column[{
				StringJoin["FeynCalc makes an extensive use of Mathematica's typesetting capabilities ",
					"to format the output in a nice and easily readable manner."],
				" ",
				StringJoin["However, the built-in typesetting is available only if the format type of ",
					"new output cells is set to TraditionalForm. The default value is StandardForm."],
				" ",
				StringJoin["Do you want to allow FeynCalc to change the default output  format to ",
					"TraditionalForm whenever it is loaded?"],
				" ",
				StringJoin["This will only affect the current FeynCalc front end session and will not ",
					"modify the default behavior of Mathematica."]
				}
			];
		strFeynArts =
			Column[{
				"Do you want to install FeynArts from "<> OptionValue[InstallFeynArts,FeynArtsMirrorLink] <> "?",
				"",
				"FeynArts is a Feynman diagram generator developed by Thomas Hahn (www.feynarts.de).",
				"It is not part of FeynCalc but it can be used together with FeynCalc after some adjustments.",
				"",
				"The modified version of FeynArts will be placed into " <> FileNameJoin[{packageDir,"FeynArts"}]<>"."
				}
			];

		strOverwriteFC =
			Column[{
				"Looks like you already have a version of FeynCalc installed in " <> packageDir,
				"",
				"The installer can overwrite the content of this directory with the downloaded version of FeynCalc.",
				"",
				"However, in this case all custom configuration files or add-ons located there will be lost.",
				"",
				"How should we proceed?"
				}
			];

		configFileProlog = StringJoin[
			"(* ",
			"Here you can put some commands and settings to be evaluated on every start of FeynCalc.\n",
			"This can be used to customize your FeynCalc installation. ",
			"*)"
		];


		(* If the package directory already exists, ask the user about overwriting *)
		If[ DirectoryQ[packageDir],

			If[ OptionValue[AutoOverwriteFeynCalcDirectory],

				Quiet@DeleteDirectory[packageDir, DeleteContents -> True],

				Null,
				If[ choiceDialog2[fancyText[strOverwriteFC],{"Yes, overwrite the " <> packageName <>" directory"->True,
					"No, I need to do a backup first. Abort installation."->False}, WindowFloating->True, WindowTitle->"Existing FeynCalc installation detected"],
					Quiet@DeleteDirectory[packageDir, DeleteContents -> True],
					Abort[]
				]
			]
		];

		(* Download FeynCalc zip file	*)
		If[ $PathToFCArc=!="",
			tmpzip = $PathToFCArc;
			WriteString["stdout", "Installing FeynCalc from ", tmpzip," ..."],
			WriteString["stdout", "Downloading FeynCalc from ", gitzip," ..."];
			tmpzip=FCGetUrl[gitzip];
		];

		If[tmpzip===$Failed || !FileExistsQ[tmpzip],
			WriteString["stdout", "\nFailed to download the FeynCalc zip file. Please check your interent connection.\nInstallation aborted!"];
			Abort[],

			unzipDir= tmpzip<>".dir";
			WriteString["stdout", "done! \n"];
		];

		(* Extract the content	*)
		WriteString["stdout", "FeynCalc zip file was saved to ", tmpzip,".\n"];
		WriteString["stdout", "Extracting FeynCalc zip file to ", unzipDir, " ..."];

		If[	ExtractArchive[tmpzip, unzipDir]===$Failed,
			WriteString["stdout", "\nFailed to extract the FeynCalc zip. The file might be corrupted.\nInstallation aborted!"];
			Abort[],
			WriteString["stdout", "done! \n"];
			(* Delete the downloaded file	*)
			If[ $PathToFCArc==="",
				Quiet@DeleteFile[tmpzip];
			]
		];

		WriteString["stdout", "Checking the directory structure..."];
		zipDir = FileNames["FeynCalc.m", unzipDir, Infinity];
		If[ Length[zipDir]===1,
			fullPath = DirectoryName[zipDir[[1]]];
			zipDir = Last[FileNameSplit[DirectoryName[zipDir[[1]]]]];
			WriteString["stdout", "done! \n"],
			WriteString["stdout", "\nFailed to recognize the directory structure of the downloaded zip file. \nInstallation aborted!"];
			Abort[]
		];

		(* Move the files to the final destination	*)
		WriteString["stdout", "Copying "<>packageName<>" to ", packageDir, " ..."];

		If[	CopyDirectory[fullPath,packageDir]===$Failed,
			WriteString["stdout", "\nFailed to copy "  <>fullPath<>" to ", packageDir <>". \nInstallation aborted!"];
			Abort[],
			WriteString["stdout", "done! \n"];
			(* Delete the extracted archive *)
			Quiet@DeleteDirectory[unzipDir, DeleteContents -> True];
		];

		(* Activate the documentation	*)
		WriteString["stdout", "Setting up the help system ... "];
		RenameDirectory[FileNameJoin[{packageDir,"DocOutput"}],FileNameJoin[{packageDir,"Documentation"}]];
		Quiet@DeleteDirectory[FileNameJoin[{packageDir,"DocSource"}], DeleteContents -> True];

		(* Disable InsufficientVersionWarning?*)
		If[ OptionValue[AutoDisableInsufficientVersionWarning] && $Notebooks,

			SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}],

			Null,
			If[ choiceDialog2[fancyText[strDisableWarning], WindowFloating->True, WindowTitle->"Documentation system"] && $Notebooks,
				SetOptions[$FrontEnd, MessageOptions -> {"InsufficientVersionWarning" -> False}]
			]
		];

		(* Activate TraditionalForm? *)
		WriteString["stdout", "Setting up the format type of new output cells ... "];
		If[ OptionValue[AutoEnableTraditionalForm],

			useTraditionalForm = True,
			Null,
			If[ choiceDialog2[fancyText[strEnableTraditionalForm], WindowFloating->True, WindowTitle -> "TraditionalForm typesetting"],
				useTraditionalForm = True
			]
		];

		WriteString["stdout", "done! \n"];

		(* To have the documentation available immediately after installing FeynCalc (following the advice of Szabolcs Horv'at) *)
		If[	$VersionNumber >= 12.1,
			PacletDataRebuild[],
			RebuildPacletData[]
		];

		(* Generate FCConfig.m	*)
		WriteString["stdout", "Creating the configuration file ... "];
		configFile = StringJoin[configFileProlog, "\n\n(* Activate TraditionalForm output for each FeynCalc session *) \n$FCTraditionalFormOutput="<>ToString[useTraditionalForm]<>";"];
		Export[FileNameJoin[{packageDir,"FCConfig.m"}], configFile, "Text"];
		WriteString["stdout", "done! \n"];

		If[ OptionValue[AutoInstallFeynArts],

			faInstalled=True;
			InstallFeynArts[InstallFeynArtsTo->FileNameJoin[{packageDir,"FeynArts"}]],
			Null,
			If[ choiceDialog2[fancyText[strFeynArts], WindowFloating->True, WindowTitle->"Install FeynArts"],
				faInstalled=True;
				InstallFeynArts[InstallFeynArtsTo->FileNameJoin[{packageDir,"FeynArts"}]]
			]
		];

		WriteString["stdout", "\nInstallation complete! Loading FeynCalc ... \n"];

		If[	faInstalled,

			If[	OptionValue[InstallFeynCalcDevelopmentVersion],
				Global`$LoadAddOns={"FeynArts"},
				Global`$LoadFeynArts=True
			]
		];
		Get["FeynCalc`"];

];

End[];

EndPackage[];
