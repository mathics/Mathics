(* ::Package:: *)

(* Paclet Info File *)
(* Created by Mathematica Plugin for IntelliJ IDEA, see http://wlplugin.halirutan.de/ *)
(* :Author: patrick *)
(* :Date: 08.06.18 *)

Paclet[
  Name -> "Rubi",
  Version -> "4.16.1.0",
  MathematicaVersion -> "9+",
  Description -> "Package for Rule-based Integration",
  Creator -> "Albert D. Rich, Patrick Scheibe",
  Thumbnail -> "logo.png",
  URL -> "https://rulebasedintegration.org",
  Extensions ->
      {
        {"Kernel", Root -> ".", Context -> "Rubi`"},
        {"PacletServer",
          "Tags" -> {"integration", "rule", "antiderivative"},
          "Categories" -> {"Mathematics"},
          "Description" -> "Rubi is a package for symbolic, rule-based integration that outperforms Mathematica's Integrate in many cases and often finds optimal antiderivatives.",
          "License" -> "MIT"
        }
      }
]
