(* Mathematica Package *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)

(* :Title: ShowStepFormatting *)
(* :Author: patrick *)
(* :Date: 2018-06-16 *)
(* :Discussion: This contains only the formatting definitions for the integration steps. *)

$RuleColor = Hue[0, .8, .5];
$ConditionColor = Hue[.39, .8, .5];
$IntermediateResultColor = Hue[.6, .8, .5];

(* TraditionalForm formatting for some Rubi functions *)
Format[HoldPattern[Dist[u_, v_, _]], TraditionalForm] := HoldForm[u * v];
Unprotect[Int]; Format[HoldPattern[Int[expr_, x_]], TraditionalForm] := HoldForm[Integrate[expr, x]];

FormatRubiStep::usage = "FormatRubiStep[step] displays an integration step as formatted box in TraditionalForm";
FormatRubiStep[RubiRule[cond_, HoldComplete[lhs_], HoldComplete[rhs_], rule_ : "General"]] := Module[
  {
    fullRule,
    transformation,
    grid
  },
  grid[expr_, True, opts__] := Grid[expr, opts, AllowScriptLevelChange -> False];
  grid[expr_, False, opts__] := Grid[expr, opts];
  transformation =
      Row[{Style[TraditionalForm@HoldForm[lhs], $RuleColor],
        Style["\[LongRightArrow]", $RuleColor],
        Style[TraditionalForm@HoldForm[rhs], $RuleColor]},
        Spacer[10], Alignment -> Center];
  fullRule =
      grid[{
        {Style["Rubi Rule:", "Label", Gray], Style[rule, "Label"]},
        {Style["Condition:", "Label", Gray], Style[cond, $ConditionColor]},
        {Style["Transformation:", "Label", Gray], transformation}},
        $VersionNumber > 8,
        Alignment -> {{Right, Left}, {Center, Center, Center}},
        Spacings -> {1, 2},
        Dividers -> {False, {2 -> LightGray, 3 -> LightGray}}
      ];
  If[$VersionNumber > 8,
    (* newer versions need the AllowScriptLevelChange option to look nice *)
    Item[DynamicModule[{open = False},
      Deploy@Framed[
        Grid[{{Opener@Dynamic@open,
          PaneSelector[{False -> transformation, True -> fullRule},
            Dynamic@open, ImageSize -> Automatic]}},
          Alignment -> {Left, Center}, AllowScriptLevelChange -> False],
        AllowScriptLevelChange -> False,
        FrameMargins -> 10, FrameStyle -> {AbsoluteThickness[1], LightGray}, RoundingRadius -> 3
      ]],
      Alignment -> Right,
      AllowScriptLevelChange -> False
    ],
    (* For old versions of Mathematica without AllowScriptLevelChange option*)
    Item[DynamicModule[{open = False},
      Deploy@Framed[
        Grid[{{Opener@Dynamic@open,
          PaneSelector[{False -> transformation, True -> fullRule},
            Dynamic@open, ImageSize -> Automatic]}},
          Alignment -> {Left, Center}],
        FrameMargins -> 10, FrameStyle -> {AbsoluteThickness[1], LightGray}, RoundingRadius -> 3
      ]],
      Alignment -> Right
    ]
  ]
];

FormatRubiStep[RubiIntermediateResult[HoldComplete[expr_]]] := (Item[
  Tooltip[
    Button[
      Deploy@Framed[Style[StandardForm@HoldForm[expr], $IntermediateResultColor], FrameMargins -> 10, FrameStyle -> {AbsoluteThickness[1], LightGray}, RoundingRadius -> 3],
      CopyToClipboard[Defer[expr]], Appearance -> None, BaseStyle -> {}],
    "Click to copy as input"
  ],
  Alignment -> Left,
  AllowScriptLevelChange -> False
]);

PrintRubiSteps::usage = "PrintRubiSteps[{steps..}] formats and prints a list of steps and rules from the integration process.";
PrintRubiSteps::err = "Could not print Rubi steps: ``";
PrintRubiSteps[{steps : {__}}] /; $VersionNumber > 8 := Print[
  Column[
    Append[
      FormatRubiStep /@ steps,
      Button["Copy Steps", CopyToClipboard[steps], ImageSize -> Automatic, Tooltip -> "Copy list of steps as plain input", Appearance -> "Palette"]
    ], Background -> GrayLevel@.98, AllowScriptLevelChange -> False
  ]
];
(* Simplified definition for older Mathematica versions *)
PrintRubiSteps[{steps : {__}}] := Print[
  Column[
    FormatRubiStep /@ steps,
    Background -> GrayLevel@.98
  ]
];

PrintRubiSteps[arg___] := Message[PrintRubiSteps::err, {arg}];

RubiStats /: MakeBoxes[RubiStats[arg : {a_, b_, c_, d_, e_, f_}], form : (StandardForm | TraditionalForm)] := Module[{above, below},
  above = { (* example grid *)
    {BoxForm`SummaryItem[{"Number of steps: ", a}]},
    {BoxForm`SummaryItem[{"Number of distinct rules: ", b}]},
    {BoxForm`SummaryItem[{"Leaf-count of input: ", c}]},
    {BoxForm`SummaryItem[{"Leaf-count of antiderivative: ", d}]},
    {BoxForm`SummaryItem[{"Rule-size-ratio: ", e}]}
  };
  below = {};
  BoxForm`ArrangeSummaryBox[
    RubiStats, (* head *)
    arg, (* interpretation *)
    None, (* icon, use None if not needed *)
    (* above and below must be in a format suitable for Grid or Column *)
    above, (* always shown content *)
    below, (* expandable content *)
    form,
    "Interpretable" -> Automatic
  ] /; $VersionNumber > 8
];

RubiStats[{a_, b_, c_, d_, e_, f_}]["Steps"] := a;
RubiStats[{a_, b_, c_, d_, e_, f_}]["NumberOfRules"] := b;
RubiStats[{a_, b_, c_, d_, e_, f_}]["InputLeafCount"] := c;
RubiStats[{a_, b_, c_, d_, e_, f_}]["OutputLeafCount"] := d;
RubiStats[{a_, b_, c_, d_, e_, f_}]["Ratio"] := e;
RubiStats[{a_, b_, c_, d_, e_, f_}]["Rules"] := f;
