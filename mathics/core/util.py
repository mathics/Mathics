#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import re
import sys
from itertools import chain

FORMAT_RE = re.compile(r'\`(\d*)\`')

WL_TO_UNICODE = {
    '\uf74b': '𝐷', # \[CapitalDifferentialD] -> MATHEMATICAL ITALIC CAPITAL D
    '\uf76a': '⌘', # \[CommandKey] -> PLACE OF INTEREST SIGN
    '\uf7da': '𝕔', # \[ConstantC] -> MATHEMATICAL DOUBLE-STRUCK SMALL C
    '\uf3b1': '⋱', # \[Continuation] -> DOWN RIGHT DIAGONAL ELLIPSIS
    '\uf4a0': '⨯', # \[Cross] -> VECTOR OR CROSS PRODUCT
    '\uf74c': '𝑑', # \[DifferentialD] -> MATHEMATICAL ITALIC SMALL D
    '\uf3d5': '→', # \[DirectedEdge] -> RIGHTWARDS ARROW
    '\uf4a4': 'ϴ', # \[DiscreteRatio] -> GREEK CAPITAL THETA SYMBOL
    '\uf700': 'ȷ', # \[DotlessJ] -> LATIN SMALL LETTER DOTLESS J
    '\uf751': '⛶', # \[DottedSquare] -> SQUARE FOUR CORNERS
    '\uf74a': 'ℽ', # \[DoubledGamma] -> DOUBLE-STRUCK SMALL GAMMA
    '\uf749': 'ℼ', # \[DoubledPi] -> DOUBLE-STRUCK SMALL PI
    '\uf6e6': '𝕒', # \[DoubleStruckA] -> MATHEMATICAL DOUBLE-STRUCK SMALL A
    '\uf6e7': '𝕓', # \[DoubleStruckB] -> MATHEMATICAL DOUBLE-STRUCK SMALL B
    '\uf6e8': '𝕔', # \[DoubleStruckC] -> MATHEMATICAL DOUBLE-STRUCK SMALL C
    '\uf7a4': '𝔸', # \[DoubleStruckCapitalA] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL A
    '\uf7a5': '𝔹', # \[DoubleStruckCapitalB] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL B
    '\uf7a6': 'ℂ', # \[DoubleStruckCapitalC] -> DOUBLE-STRUCK CAPITAL C
    '\uf7a7': '𝔻', # \[DoubleStruckCapitalD] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL D
    '\uf7a8': '𝔼', # \[DoubleStruckCapitalE] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL E
    '\uf7a9': '𝔽', # \[DoubleStruckCapitalF] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL F
    '\uf7aa': '𝔾', # \[DoubleStruckCapitalG] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL G
    '\uf7ab': 'ℍ', # \[DoubleStruckCapitalH] -> DOUBLE-STRUCK CAPITAL H
    '\uf7ac': '𝕀', # \[DoubleStruckCapitalI] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL I
    '\uf7ad': '𝕁', # \[DoubleStruckCapitalJ] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL J
    '\uf7ae': '𝕂', # \[DoubleStruckCapitalK] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL K
    '\uf7af': '𝕃', # \[DoubleStruckCapitalL] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL L
    '\uf7b0': '𝕄', # \[DoubleStruckCapitalM] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL M
    '\uf7b1': 'ℕ', # \[DoubleStruckCapitalN] -> DOUBLE-STRUCK CAPITAL N
    '\uf7b2': '𝕆', # \[DoubleStruckCapitalO] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL O
    '\uf7b3': 'ℙ', # \[DoubleStruckCapitalP] -> DOUBLE-STRUCK CAPITAL P
    '\uf7b4': 'ℚ', # \[DoubleStruckCapitalQ] -> DOUBLE-STRUCK CAPITAL Q
    '\uf7b5': 'ℝ', # \[DoubleStruckCapitalR] -> DOUBLE-STRUCK CAPITAL R
    '\uf7b6': '𝕊', # \[DoubleStruckCapitalS] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL S
    '\uf7b7': '𝕋', # \[DoubleStruckCapitalT] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL T
    '\uf7b8': '𝕌', # \[DoubleStruckCapitalU] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL U
    '\uf7b9': '𝕍', # \[DoubleStruckCapitalV] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL V
    '\uf7ba': '𝕎', # \[DoubleStruckCapitalW] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL W
    '\uf7bb': '𝕏', # \[DoubleStruckCapitalX] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL X
    '\uf7bc': '𝕐', # \[DoubleStruckCapitalY] -> MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
    '\uf7bd': 'ℤ', # \[DoubleStruckCapitalZ] -> DOUBLE-STRUCK CAPITAL Z
    '\uf6e9': '𝕕', # \[DoubleStruckD] -> MATHEMATICAL DOUBLE-STRUCK SMALL D
    '\uf6ea': '𝕖', # \[DoubleStruckE] -> MATHEMATICAL DOUBLE-STRUCK SMALL E
    '\uf7e3': '𝟠', # \[DoubleStruckEight] -> MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT
    '\uf6eb': '𝕗', # \[DoubleStruckF] -> MATHEMATICAL DOUBLE-STRUCK SMALL F
    '\uf7e0': '𝟝', # \[DoubleStruckFive] -> MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE
    '\uf7df': '𝟜', # \[DoubleStruckFour] -> MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR
    '\uf6ec': '𝕘', # \[DoubleStruckG] -> MATHEMATICAL DOUBLE-STRUCK SMALL G
    '\uf6ed': '𝕙', # \[DoubleStruckH] -> MATHEMATICAL DOUBLE-STRUCK SMALL H
    '\uf6ee': '𝕚', # \[DoubleStruckI] -> MATHEMATICAL DOUBLE-STRUCK SMALL I
    '\uf6ef': '𝕛', # \[DoubleStruckJ] -> MATHEMATICAL DOUBLE-STRUCK SMALL J
    '\uf6f0': '𝕜', # \[DoubleStruckK] -> MATHEMATICAL DOUBLE-STRUCK SMALL K
    '\uf6f1': '𝕝', # \[DoubleStruckL] -> MATHEMATICAL DOUBLE-STRUCK SMALL L
    '\uf6f2': '𝕞', # \[DoubleStruckM] -> MATHEMATICAL DOUBLE-STRUCK SMALL M
    '\uf6f3': '𝕟', # \[DoubleStruckN] -> MATHEMATICAL DOUBLE-STRUCK SMALL N
    '\uf7e4': '𝟡', # \[DoubleStruckNine] -> MATHEMATICAL DOUBLE-STRUCK DIGIT NINE
    '\uf6f4': '𝕠', # \[DoubleStruckO] -> MATHEMATICAL DOUBLE-STRUCK SMALL O
    '\uf7dc': '𝟙', # \[DoubleStruckOne] -> MATHEMATICAL DOUBLE-STRUCK DIGIT ONE
    '\uf6f5': '𝕡', # \[DoubleStruckP] -> MATHEMATICAL DOUBLE-STRUCK SMALL P
    '\uf6f6': '𝕢', # \[DoubleStruckQ] -> MATHEMATICAL DOUBLE-STRUCK SMALL Q
    '\uf6f7': '𝕣', # \[DoubleStruckR] -> MATHEMATICAL DOUBLE-STRUCK SMALL R
    '\uf6f8': '𝕤', # \[DoubleStruckS] -> MATHEMATICAL DOUBLE-STRUCK SMALL S
    '\uf7e2': '𝟟', # \[DoubleStruckSeven] -> MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN
    '\uf7e1': '𝟞', # \[DoubleStruckSix] -> MATHEMATICAL DOUBLE-STRUCK DIGIT SIX
    '\uf6f9': '𝕥', # \[DoubleStruckT] -> MATHEMATICAL DOUBLE-STRUCK SMALL T
    '\uf7de': '𝟛', # \[DoubleStruckThree] -> MATHEMATICAL DOUBLE-STRUCK DIGIT THREE
    '\uf7dd': '𝟚', # \[DoubleStruckTwo] -> MATHEMATICAL DOUBLE-STRUCK DIGIT TWO
    '\uf6fa': '𝕦', # \[DoubleStruckU] -> MATHEMATICAL DOUBLE-STRUCK SMALL U
    '\uf6fb': '𝕧', # \[DoubleStruckV] -> MATHEMATICAL DOUBLE-STRUCK SMALL V
    '\uf6fc': '𝕨', # \[DoubleStruckW] -> MATHEMATICAL DOUBLE-STRUCK SMALL W
    '\uf6fd': '𝕩', # \[DoubleStruckX] -> MATHEMATICAL DOUBLE-STRUCK SMALL X
    '\uf6fe': '𝕪', # \[DoubleStruckY] -> MATHEMATICAL DOUBLE-STRUCK SMALL Y
    '\uf6ff': '𝕫', # \[DoubleStruckZ] -> MATHEMATICAL DOUBLE-STRUCK SMALL Z
    '\uf7db': '𝟘', # \[DoubleStruckZero] -> MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
    '\uf755': ' ̑', # \[DownBreve] -> SPACE + COMBINING INVERTED BREVE
    '\uf431': '⩵', # \[Equal] -> TWO CONSECUTIVE EQUALS SIGNS
    '⧦': '⇔', # \[Equivalent] (GLEICH STARK) -> LEFT RIGHT DOUBLE ARROW
    '\uf74d': 'ⅇ', # \[ExponentialE] -> DOUBLE-STRUCK ITALIC SMALL E
    '\uf750': '•', # \[FilledSmallCircle] -> BULLET
    '\uf800': 'ạ', # \[FormalA] -> LATIN SMALL LETTER A WITH DOT BELOW
    '\uf854': 'α̣', # \[FormalAlpha] -> GREEK SMALL LETTER ALPHA + COMBINING DOT BELOW
    '\uf801': 'ḅ', # \[FormalB] -> LATIN SMALL LETTER B WITH DOT BELOW
    '\uf855': 'β̣', # \[FormalBeta] -> GREEK SMALL LETTER BETA + COMBINING DOT BELOW
    '\uf802': 'c̣', # \[FormalC] -> LATIN SMALL LETTER C + COMBINING DOT BELOW
    '\uf81a': 'Ạ', # \[FormalCapitalA] -> LATIN CAPITAL LETTER A WITH DOT BELOW
    '\uf834': 'Α̣', # \[FormalCapitalAlpha] -> GREEK CAPITAL LETTER ALPHA + COMBINING DOT BELOW
    '\uf81b': 'Ḅ', # \[FormalCapitalB] -> LATIN CAPITAL LETTER B WITH DOT BELOW
    '\uf835': 'Β̣', # \[FormalCapitalBeta] -> GREEK CAPITAL LETTER BETA + COMBINING DOT BELOW
    '\uf81c': 'C̣', # \[FormalCapitalC] -> LATIN CAPITAL LETTER C + COMBINING DOT BELOW
    '\uf84a': 'Χ̣', # \[FormalCapitalChi] -> GREEK CAPITAL LETTER CHI + COMBINING DOT BELOW
    '\uf81d': 'Ḍ', # \[FormalCapitalD] -> LATIN CAPITAL LETTER D WITH DOT BELOW
    '\uf837': 'Δ̣', # \[FormalCapitalDelta] -> GREEK CAPITAL LETTER DELTA + COMBINING DOT BELOW
    '\uf87f': 'Ϝ̣', # \[FormalCapitalDigamma] -> GREEK LETTER DIGAMMA + COMBINING DOT BELOW
    '\uf81e': 'Ẹ', # \[FormalCapitalE] -> LATIN CAPITAL LETTER E WITH DOT BELOW
    '\uf838': 'Ε̣', # \[FormalCapitalEpsilon] -> GREEK CAPITAL LETTER EPSILON + COMBINING DOT BELOW
    '\uf83a': 'Η̣', # \[FormalCapitalEta] -> GREEK CAPITAL LETTER ETA + COMBINING DOT BELOW
    '\uf81f': 'F̣', # \[FormalCapitalF] -> LATIN CAPITAL LETTER F + COMBINING DOT BELOW
    '\uf820': 'G̣', # \[FormalCapitalG] -> LATIN CAPITAL LETTER G + COMBINING DOT BELOW
    '\uf836': 'Γ', # \[FormalCapitalGamma] -> GREEK CAPITAL LETTER GAMMA
    '\uf821': 'Ḥ', # \[FormalCapitalH] -> LATIN CAPITAL LETTER H WITH DOT BELOW
    '\uf822': 'Ị', # \[FormalCapitalI] -> LATIN CAPITAL LETTER I WITH DOT BELOW
    '\uf83c': 'Ι̣', # \[FormalCapitalIota] -> GREEK CAPITAL LETTER IOTA + COMBINING DOT BELOW
    '\uf823': 'J̣', # \[FormalCapitalJ] -> LATIN CAPITAL LETTER J + COMBINING DOT BELOW
    '\uf824': 'Ḳ', # \[FormalCapitalK] -> LATIN CAPITAL LETTER K WITH DOT BELOW
    '\uf83d': 'Κ̣', # \[FormalCapitalKappa] -> GREEK CAPITAL LETTER KAPPA + COMBINING DOT BELOW
    '\uf881': 'Ϟ̣', # \[FormalCapitalKoppa] -> GREEK LETTER KOPPA + COMBINING DOT BELOW
    '\uf825': 'Ḷ', # \[FormalCapitalL] -> LATIN CAPITAL LETTER L WITH DOT BELOW
    '\uf83e': 'Λ̣', # \[FormalCapitalLambda] -> GREEK CAPITAL LETTER LAMDA + COMBINING DOT BELOW
    '\uf826': 'Ṃ', # \[FormalCapitalM] -> LATIN CAPITAL LETTER M WITH DOT BELOW
    '\uf83f': 'Μ̣', # \[FormalCapitalMu] -> GREEK CAPITAL LETTER MU + COMBINING DOT BELOW
    '\uf827': 'Ṇ', # \[FormalCapitalN] -> LATIN CAPITAL LETTER N WITH DOT BELOW
    '\uf840': 'Ν̣', # \[FormalCapitalNu] -> GREEK CAPITAL LETTER NU + COMBINING DOT BELOW
    '\uf828': 'Ọ', # \[FormalCapitalO] -> LATIN CAPITAL LETTER O WITH DOT BELOW
    '\uf84c': 'Ω̣', # \[FormalCapitalOmega] -> GREEK CAPITAL LETTER OMEGA + COMBINING DOT BELOW
    '\uf842': 'Ο̣', # \[FormalCapitalOmicron] -> GREEK CAPITAL LETTER OMICRON + COMBINING DOT BELOW
    '\uf829': 'P̣', # \[FormalCapitalP] -> LATIN CAPITAL LETTER P + COMBINING DOT BELOW
    '\uf849': 'Φ̣', # \[FormalCapitalPhi] -> GREEK CAPITAL LETTER PHI + COMBINING DOT BELOW
    '\uf843': 'Π̣', # \[FormalCapitalPi] -> GREEK CAPITAL LETTER PI + COMBINING DOT BELOW
    '\uf84b': 'Ψ̣', # \[FormalCapitalPsi] -> GREEK CAPITAL LETTER PSI + COMBINING DOT BELOW
    '\uf82a': 'Q̣', # \[FormalCapitalQ] -> LATIN CAPITAL LETTER Q + COMBINING DOT BELOW
    '\uf82b': 'Ṛ', # \[FormalCapitalR] -> LATIN CAPITAL LETTER R WITH DOT BELOW
    '\uf844': 'Ρ̣', # \[FormalCapitalRho] -> GREEK CAPITAL LETTER RHO + COMBINING DOT BELOW
    '\uf82c': 'Ṣ', # \[FormalCapitalS] -> LATIN CAPITAL LETTER S WITH DOT BELOW
    '\uf883': 'Ϡ̣', # \[FormalCapitalSampi] -> GREEK LETTER SAMPI + COMBINING DOT BELOW
    '\uf846': 'Σ̣', # \[FormalCapitalSigma] -> GREEK CAPITAL LETTER SIGMA + COMBINING DOT BELOW
    '\uf87d': 'Ϛ̣', # \[FormalCapitalStigma] -> GREEK LETTER STIGMA + COMBINING DOT BELOW
    '\uf82d': 'Ṭ', # \[FormalCapitalT] -> LATIN CAPITAL LETTER T WITH DOT BELOW
    '\uf847': 'Τ̣', # \[FormalCapitalTau] -> GREEK CAPITAL LETTER TAU + COMBINING DOT BELOW
    '\uf83b': 'Θ', # \[FormalCapitalTheta] -> GREEK CAPITAL LETTER THETA
    '\uf82e': 'Ụ', # \[FormalCapitalU] -> LATIN CAPITAL LETTER U WITH DOT BELOW
    '\uf848': 'Υ̣', # \[FormalCapitalUpsilon] -> GREEK CAPITAL LETTER UPSILON + COMBINING DOT BELOW
    '\uf82f': 'Ṿ', # \[FormalCapitalV] -> LATIN CAPITAL LETTER V WITH DOT BELOW
    '\uf830': 'Ẉ', # \[FormalCapitalW] -> LATIN CAPITAL LETTER W WITH DOT BELOW
    '\uf831': 'X̣', # \[FormalCapitalX] -> LATIN CAPITAL LETTER X + COMBINING DOT BELOW
    '\uf841': 'Ξ̣', # \[FormalCapitalXi] -> GREEK CAPITAL LETTER XI + COMBINING DOT BELOW
    '\uf832': 'Ỵ', # \[FormalCapitalY] -> LATIN CAPITAL LETTER Y WITH DOT BELOW
    '\uf833': 'Ẓ', # \[FormalCapitalZ] -> LATIN CAPITAL LETTER Z WITH DOT BELOW
    '\uf839': 'Ζ̣', # \[FormalCapitalZeta] -> GREEK CAPITAL LETTER ZETA + COMBINING DOT BELOW
    '\uf86a': 'χ̣', # \[FormalChi] -> GREEK SMALL LETTER CHI + COMBINING DOT BELOW
    '\uf875': 'ϒ̣', # \[FormalCurlyCapitalUpsilon] -> GREEK UPSILON WITH HOOK SYMBOL + COMBINING DOT BELOW
    '\uf858': 'ε̣', # \[FormalCurlyEpsilon] -> GREEK SMALL LETTER EPSILON + COMBINING DOT BELOW
    '\uf885': 'ϰ̣', # \[FormalCurlyKappa] -> GREEK KAPPA SYMBOL + COMBINING DOT BELOW
    '\uf869': 'φ̣', # \[FormalCurlyPhi] -> GREEK SMALL LETTER PHI + COMBINING DOT BELOW
    '\uf879': 'ϖ̣', # \[FormalCurlyPi] -> GREEK PI SYMBOL + COMBINING DOT BELOW
    '\uf886': 'ϱ̣', # \[FormalCurlyRho] -> GREEK RHO SYMBOL + COMBINING DOT BELOW
    '\uf874': 'ϑ̣', # \[FormalCurlyTheta] -> GREEK THETA SYMBOL + COMBINING DOT BELOW
    '\uf803': 'ḍ', # \[FormalD] -> LATIN SMALL LETTER D WITH DOT BELOW
    '\uf857': 'δ̣', # \[FormalDelta] -> GREEK SMALL LETTER DELTA + COMBINING DOT BELOW
    '\uf880': 'ϝ', # \[FormalDigamma] -> GREEK SMALL LETTER DIGAMMA
    '\uf804': 'ẹ', # \[FormalE] -> LATIN SMALL LETTER E WITH DOT BELOW
    '\uf88a': 'ϵ̣', # \[FormalEpsilon] -> GREEK LUNATE EPSILON SYMBOL + COMBINING DOT BELOW
    '\uf85a': 'η̣', # \[FormalEta] -> GREEK SMALL LETTER ETA + COMBINING DOT BELOW
    '\uf805': 'f̣', # \[FormalF] -> LATIN SMALL LETTER F + COMBINING DOT BELOW
    '\uf865': 'ς̣', # \[FormalFinalSigma] -> GREEK SMALL LETTER FINAL SIGMA + COMBINING DOT BELOW
    '\uf806': 'g̣', # \[FormalG] -> LATIN SMALL LETTER G + COMBINING DOT BELOW
    '\uf856': 'γ̣', # \[FormalGamma] -> GREEK SMALL LETTER GAMMA + COMBINING DOT BELOW
    '\uf807': 'ḥ', # \[FormalH] -> LATIN SMALL LETTER H WITH DOT BELOW
    '\uf808': 'ị', # \[FormalI] -> LATIN SMALL LETTER I WITH DOT BELOW
    '\uf85c': 'Ι̣', # \[FormalIota] -> GREEK CAPITAL LETTER IOTA + COMBINING DOT BELOW
    '\uf809': 'j̣', # \[FormalJ] -> LATIN SMALL LETTER J + COMBINING DOT BELOW
    '\uf80a': 'ḳ', # \[FormalK] -> LATIN SMALL LETTER K WITH DOT BELOW
    '\uf85d': 'κ̣', # \[FormalKappa] -> GREEK SMALL LETTER KAPPA + COMBINING DOT BELOW
    '\uf882': 'ϟ̣', # \[FormalKoppa] -> GREEK SMALL LETTER KOPPA + COMBINING DOT BELOW
    '\uf80b': 'ḷ', # \[FormalL] -> LATIN SMALL LETTER L WITH DOT BELOW
    '\uf85e': 'λ̣', # \[FormalLambda] -> GREEK SMALL LETTER LAMDA + COMBINING DOT BELOW
    '\uf80c': 'ṃ', # \[FormalM] -> LATIN SMALL LETTER M + COMBINING DOT BELOW
    '\uf85f': 'μ̣', # \[FormalMu] -> GREEK SMALL LETTER MU + COMBINING DOT BELOW
    '\uf80d': 'ṇ', # \[FormalN] -> LATIN SMALL LETTER N + COMBINING DOT BELOW
    '\uf860': 'ν̣', # \[FormalNu] -> GREEK SMALL LETTER NU + COMBINING DOT BELOW
    '\uf80e': 'ọ', # \[FormalO] -> LATIN SMALL LETTER O + COMBINING DOT BELOW
    '\uf86c': 'ω̣', # \[FormalOmega] -> GREEK SMALL LETTER OMEGA + COMBINING DOT BELOW
    '\uf862': 'ο̣', # \[FormalOmicron] -> GREEK SMALL LETTER OMICRON + COMBINING DOT BELOW
    '\uf80f': 'p̣', # \[FormalP] -> LATIN SMALL LETTER P + COMBINING DOT BELOW
    '\uf878': 'ϕ̣', # \[FormalPhi] -> GREEK PHI SYMBOL + COMBINING DOT BELOW
    '\uf863': 'π̣', # \[FormalPi] -> GREEK SMALL LETTER PI + COMBINING DOT BELOW
    '\uf86b': 'ψ̣', # \[FormalPsi] -> GREEK SMALL LETTER PSI + COMBINING DOT BELOW
    '\uf810': 'q̣', # \[FormalQ] -> LATIN SMALL LETTER Q + COMBINING DOT BELOW
    '\uf811': 'ṛ', # \[FormalR] -> LATIN SMALL LETTER R WITH DOT BELOW
    '\uf864': 'ρ̣', # \[FormalRho] -> GREEK SMALL LETTER RHO + COMBINING DOT BELOW
    '\uf812': 'ṣ', # \[FormalS] -> LATIN SMALL LETTER S WITH DOT BELOW
    '\uf884': 'ϡ̣', # \[FormalSampi] -> GREEK SMALL LETTER SAMPI + COMBINING DOT BELOW
    '\uf866': 'σ̣', # \[FormalSigma] -> GREEK SMALL LETTER SIGMA + COMBINING DOT BELOW
    '\uf87e': 'ϛ', # \[FormalStigma] -> GREEK SMALL LETTER STIGMA
    '\uf813': 'ṭ', # \[FormalT] -> LATIN SMALL LETTER T WITH DOT BELOW
    '\uf867': 'τ̣', # \[FormalTau] -> GREEK SMALL LETTER TAU + COMBINING DOT BELOW
    '\uf85b': 'θ̣', # \[FormalTheta] -> GREEK SMALL LETTER THETA + COMBINING DOT BELOW
    '\uf814': 'ụ', # \[FormalU] -> LATIN SMALL LETTER U WITH DOT BELOW
    '\uf868': 'υ̣', # \[FormalUpsilon] -> GREEK SMALL LETTER UPSILON + COMBINING DOT BELOW
    '\uf815': 'ṿ', # \[FormalV] -> LATIN SMALL LETTER V WITH DOT BELOW
    '\uf816': 'ẉ', # \[FormalW] -> LATIN SMALL LETTER W WITH DOT BELOW
    '\uf817': 'x̣', # \[FormalX] -> LATIN SMALL LETTER X + COMBINING DOT BELOW
    '\uf861': 'ξ̣', # \[FormalXi] -> GREEK SMALL LETTER XI + COMBINING DOT BELOW
    '\uf818': 'ỵ', # \[FormalY] -> LATIN SMALL LETTER Y WITH DOT BELOW
    '\uf819': 'ẓ', # \[FormalZ] -> LATIN SMALL LETTER Z WITH DOT BELOW
    '\uf859': 'ζ̣', # \[FormalZeta] -> GREEK SMALL LETTER ZETA + COMBINING DOT BELOW
    '\uf4a1': '↦', # \[Function] -> RIGHTWARDS ARROW FROM BAR
    '\uf6cc': '𝔞', # \[GothicA] -> MATHEMATICAL FRAKTUR SMALL A
    '\uf6cd': '𝔟', # \[GothicB] -> MATHEMATICAL FRAKTUR SMALL B
    '\uf6ce': '𝔠', # \[GothicC] -> MATHEMATICAL FRAKTUR SMALL C
    '\uf78a': '𝔄', # \[GothicCapitalA] -> MATHEMATICAL FRAKTUR CAPITAL A
    '\uf78b': '𝔅', # \[GothicCapitalB] -> MATHEMATICAL FRAKTUR CAPITAL B
    '\uf78d': '𝔇', # \[GothicCapitalD] -> MATHEMATICAL FRAKTUR CAPITAL D
    '\uf78e': '𝔈', # \[GothicCapitalE] -> MATHEMATICAL FRAKTUR CAPITAL E
    '\uf78f': '𝔉', # \[GothicCapitalF] -> MATHEMATICAL FRAKTUR CAPITAL F
    '\uf790': '𝔊', # \[GothicCapitalG] -> MATHEMATICAL FRAKTUR CAPITAL G
    '\uf793': '𝔍', # \[GothicCapitalJ] -> MATHEMATICAL FRAKTUR CAPITAL J
    '\uf794': '𝔎', # \[GothicCapitalK] -> MATHEMATICAL FRAKTUR CAPITAL K
    '\uf795': '𝔏', # \[GothicCapitalL] -> MATHEMATICAL FRAKTUR CAPITAL L
    '\uf796': '𝔐', # \[GothicCapitalM] -> MATHEMATICAL FRAKTUR CAPITAL M
    '\uf797': '𝔑', # \[GothicCapitalN] -> MATHEMATICAL FRAKTUR CAPITAL N
    '\uf798': '𝔒', # \[GothicCapitalO] -> MATHEMATICAL FRAKTUR CAPITAL O
    '\uf799': '𝔓', # \[GothicCapitalP] -> MATHEMATICAL FRAKTUR CAPITAL P
    '\uf79a': '𝔔', # \[GothicCapitalQ] -> MATHEMATICAL FRAKTUR CAPITAL Q
    '\uf79c': '𝔖', # \[GothicCapitalS] -> MATHEMATICAL FRAKTUR CAPITAL S
    '\uf79d': '𝔗', # \[GothicCapitalT] -> MATHEMATICAL FRAKTUR CAPITAL T
    '\uf79e': '𝔘', # \[GothicCapitalU] -> MATHEMATICAL FRAKTUR CAPITAL U
    '\uf79f': '𝔙', # \[GothicCapitalV] -> MATHEMATICAL FRAKTUR CAPITAL V
    '\uf7a0': '𝔚', # \[GothicCapitalW] -> MATHEMATICAL FRAKTUR CAPITAL W
    '\uf7a1': '𝔛', # \[GothicCapitalX] -> MATHEMATICAL FRAKTUR CAPITAL X
    '\uf7a2': '𝔜', # \[GothicCapitalY] -> MATHEMATICAL FRAKTUR CAPITAL Y
    '\uf6cf': '𝔡', # \[GothicD] -> MATHEMATICAL FRAKTUR SMALL D
    '\uf6d0': '𝔢', # \[GothicE] -> MATHEMATICAL FRAKTUR SMALL E
    '\uf6d1': '𝔣', # \[GothicF] -> MATHEMATICAL FRAKTUR SMALL F
    '\uf6d2': '𝔤', # \[GothicG] -> MATHEMATICAL FRAKTUR SMALL G
    '\uf6d3': '𝔥', # \[GothicH] -> MATHEMATICAL FRAKTUR SMALL H
    '\uf6d4': '𝔦', # \[GothicI] -> MATHEMATICAL FRAKTUR SMALL I
    '\uf6d5': '𝔧', # \[GothicJ] -> MATHEMATICAL FRAKTUR SMALL J
    '\uf6d6': '𝔨', # \[GothicK] -> MATHEMATICAL FRAKTUR SMALL K
    '\uf6d7': '𝔩', # \[GothicL] -> MATHEMATICAL FRAKTUR SMALL L
    '\uf6d8': '𝔪', # \[GothicM] -> MATHEMATICAL FRAKTUR SMALL M
    '\uf6d9': '𝔫', # \[GothicN] -> MATHEMATICAL FRAKTUR SMALL N
    '\uf6da': '𝔬', # \[GothicO] -> MATHEMATICAL FRAKTUR SMALL O
    '\uf6db': '𝔭', # \[GothicP] -> MATHEMATICAL FRAKTUR SMALL P
    '\uf6dc': '𝔮', # \[GothicQ] -> MATHEMATICAL FRAKTUR SMALL Q
    '\uf6dd': '𝔯', # \[GothicR] -> MATHEMATICAL FRAKTUR SMALL R
    '\uf6de': '𝔰', # \[GothicS] -> MATHEMATICAL FRAKTUR SMALL S
    '\uf6df': '𝔱', # \[GothicT] -> MATHEMATICAL FRAKTUR SMALL T
    '\uf6e0': '𝔲', # \[GothicU] -> MATHEMATICAL FRAKTUR SMALL U
    '\uf6e1': '𝔳', # \[GothicV] -> MATHEMATICAL FRAKTUR SMALL V
    '\uf6e2': '𝔴', # \[GothicW] -> MATHEMATICAL FRAKTUR SMALL W
    '\uf6e3': '𝔵', # \[GothicX] -> MATHEMATICAL FRAKTUR SMALL X
    '\uf6e4': '𝔶', # \[GothicY] -> MATHEMATICAL FRAKTUR SMALL Y
    '\uf6e5': '𝔷', # \[GothicZ] -> MATHEMATICAL FRAKTUR SMALL Z
    '\uf753': '●', # \[GrayCircle] -> BLACK CIRCLE
    '\uf752': '■', # \[GraySquare] -> BLACK SQUARE
    '\uf74e': 'ⅈ', # \[ImaginaryI] -> DOUBLE-STRUCK ITALIC SMALL I
    '\uf74f': 'ⅉ', # \[ImaginaryJ] -> DOUBLE-STRUCK ITALIC SMALL J
    '\uf523': '⟹', # \[Implies] -> LONG RIGHTWARDS DOUBLE ARROW
    '\uf603': '|', # \[LeftBracketingBar] -> VERTICAL LINE
    '\uf605': '‖', # \[LeftDoubleBracketingBar] -> DOUBLE VERTICAL LINE
    '\uf761': '«', # \[LeftSkeleton] -> LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    '\uf7d9': '=', # \[LongEqual] -> EQUALS SIGN
    '\uf724': '#', # \[NumberSign] -> NUMBER SIGN
    '\uf3de': '⊙', # \[PermutationProduct] -> CIRCLED DOT OPERATOR
    '\uf528': '⎕', # \[Placeholder] -> APL FUNCTIONAL SYMBOL QUAD
    '\uf604': '|', # \[RightBracketingBar] -> VERTICAL LINE
    '\uf606': '‖', # \[RightDoubleBracketingBar] -> DOUBLE VERTICAL LINE
    '\uf762': '»', # \[RightSkeleton] -> RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    '\uf522': '→', # \[Rule] -> RIGHTWARDS ARROW
    '\uf51f': '⧴', # \[RuleDelayed] -> RULE-DELAYED
    # This one is changed because \[CapitalSampi] is mapped to GREEK LETTER SAMPI already
    'Ϡ': 'ϡ', # \[Sampi] (GREEK LETTER SAMPI) -> GREEK SMALL LETTER SAMPI
    '\uf6b2': '𝒶', # \[ScriptA] -> MATHEMATICAL SCRIPT SMALL A
    '\uf6b3': '𝒷', # \[ScriptB] -> MATHEMATICAL SCRIPT SMALL B
    '\uf6b4': '𝒸', # \[ScriptC] -> MATHEMATICAL SCRIPT SMALL C
    '\uf770': '𝒜', # \[ScriptCapitalA] -> MATHEMATICAL SCRIPT CAPITAL A
    '\uf772': '𝒞', # \[ScriptCapitalC] -> MATHEMATICAL SCRIPT CAPITAL C
    '\uf773': '𝒟', # \[ScriptCapitalD] -> MATHEMATICAL SCRIPT CAPITAL D
    '\uf776': '𝒢', # \[ScriptCapitalG] -> MATHEMATICAL SCRIPT CAPITAL G
    '\uf779': '𝒥', # \[ScriptCapitalJ] -> MATHEMATICAL SCRIPT CAPITAL J
    '\uf77a': '𝒦', # \[ScriptCapitalK] -> MATHEMATICAL SCRIPT CAPITAL K
    '\uf77d': '𝒩', # \[ScriptCapitalN] -> MATHEMATICAL SCRIPT CAPITAL N
    '\uf77e': '𝒪', # \[ScriptCapitalO] -> MATHEMATICAL SCRIPT CAPITAL O
    '\uf780': '𝒬', # \[ScriptCapitalQ] -> MATHEMATICAL SCRIPT CAPITAL Q
    '\uf782': '𝒮', # \[ScriptCapitalS] -> MATHEMATICAL SCRIPT CAPITAL S
    '\uf783': '𝒯', # \[ScriptCapitalT] -> MATHEMATICAL SCRIPT CAPITAL T
    '\uf784': '𝒰', # \[ScriptCapitalU] -> MATHEMATICAL SCRIPT CAPITAL U
    '\uf785': '𝒱', # \[ScriptCapitalV] -> MATHEMATICAL SCRIPT CAPITAL V
    '\uf786': '𝒲', # \[ScriptCapitalW] -> MATHEMATICAL SCRIPT CAPITAL W
    '\uf787': '𝒳', # \[ScriptCapitalX] -> MATHEMATICAL SCRIPT CAPITAL X
    '\uf788': '𝒴', # \[ScriptCapitalY] -> MATHEMATICAL SCRIPT CAPITAL Y
    '\uf789': '𝒵', # \[ScriptCapitalZ] -> MATHEMATICAL SCRIPT CAPITAL Z
    '\uf6b5': '𝒹', # \[ScriptD] -> MATHEMATICAL SCRIPT SMALL D
    '\uf730': '𝒾', # \[ScriptDotlessI] -> MATHEMATICAL SCRIPT SMALL I
    '\uf731': '𝒿', # \[ScriptDotlessJ] -> MATHEMATICAL SCRIPT SMALL J
    '\uf6b7': '𝒻', # \[ScriptF] -> MATHEMATICAL SCRIPT SMALL F
    '\uf6b9': '𝒽', # \[ScriptH] -> MATHEMATICAL SCRIPT SMALL H
    '\uf6ba': '𝒾', # \[ScriptI] -> MATHEMATICAL SCRIPT SMALL I
    '\uf6bb': '𝒿', # \[ScriptJ] -> MATHEMATICAL SCRIPT SMALL J
    '\uf6bc': '𝓀', # \[ScriptK] -> MATHEMATICAL SCRIPT SMALL K
    '\uf6be': '𝓂', # \[ScriptM] -> MATHEMATICAL SCRIPT SMALL M
    '\uf6bf': '𝓃', # \[ScriptN] -> MATHEMATICAL SCRIPT SMALL N
    '\uf6c1': '𝓅', # \[ScriptP] -> MATHEMATICAL SCRIPT SMALL P
    '\uf6c2': '𝓆', # \[ScriptQ] -> MATHEMATICAL SCRIPT SMALL Q
    '\uf6c3': '𝓇', # \[ScriptR] -> MATHEMATICAL SCRIPT SMALL R
    '\uf6c4': '𝓈', # \[ScriptS] -> MATHEMATICAL SCRIPT SMALL S
    '\uf6c5': '𝓉', # \[ScriptT] -> MATHEMATICAL SCRIPT SMALL T
    '\uf6c6': '𝓊', # \[ScriptU] -> MATHEMATICAL SCRIPT SMALL U
    '\uf6c7': '𝓋', # \[ScriptV] -> MATHEMATICAL SCRIPT SMALL V
    '\uf6c8': '𝓌', # \[ScriptW] -> MATHEMATICAL SCRIPT SMALL W
    '\uf6c9': '𝓍', # \[ScriptX] -> MATHEMATICAL SCRIPT SMALL X
    '\uf6ca': '𝓎', # \[ScriptY] -> MATHEMATICAL SCRIPT SMALL Y
    '\uf6cb': '𝓏', # \[ScriptZ] -> MATHEMATICAL SCRIPT SMALL Z
    '\uf52b': '↓', # \[ShortDownArrow] -> DOWNWARDS ARROW
    '\uf526': '←', # \[ShortLeftArrow] -> LEFTWARDS ARROW
    '\uf525': '→', # \[ShortRightArrow] -> RIGHTWARDS ARROW
    '\uf52a': '↑', # \[ShortUpArrow] -> UPWARDS ARROW
    '\uf3bb': '⋮', # \[SpanFromAbove] -> VERTICAL ELLIPSIS
    '\uf3bc': '⋱', # \[SpanFromBoth] -> DOWN RIGHT DIAGONAL ELLIPSIS
    '\uf3ba': '⋯', # \[SpanFromLeft] -> MIDLINE HORIZONTAL ELLIPSIS
    '\uf520': '▫', # \[Square] -> WHITE SMALL SQUARE
    '\uf3da': '⊗', # \[TensorProduct] -> CIRCLED TIMES
    '\uf3c7': 'ᵀ', # \[Transpose] -> MODIFIER LETTER CAPITAL T
    '\uf758': '⋯', # \[TripleDot] -> MIDLINE HORIZONTAL ELLIPSIS
    '\uf3d4': '↔', # \[UndirectedEdge] -> LEFT RIGHT ARROW
    '♅': '⛢', # \[Uranus] (URANUS) -> ASTRONOMICAL SYMBOL FOR URANUS
    '\uf3d0': '|', # \[VerticalBar] -> VERTICAL LINE
}

UNICODE_TO_WL = {
    '𝐷': '\uf74b', # MATHEMATICAL ITALIC CAPITAL D -> \[CapitalDifferentialD]
    '⌘': '\uf76a', # PLACE OF INTEREST SIGN -> \[CommandKey]
    '𝕔': '\uf6e8', # MATHEMATICAL DOUBLE-STRUCK SMALL C -> \[DoubleStruckC]
    '⨯': '\uf4a0', # VECTOR OR CROSS PRODUCT -> \[Cross]
    '𝑑': '\uf74c', # MATHEMATICAL ITALIC SMALL D -> \[DifferentialD]
    '→': '\uf525', # RIGHTWARDS ARROW -> \[ShortRightArrow]
    'ϴ': '\uf4a4', # GREEK CAPITAL THETA SYMBOL -> \[DiscreteRatio]
    'ȷ': '\uf700', # LATIN SMALL LETTER DOTLESS J -> \[DotlessJ]
    '⛶': '\uf751', # SQUARE FOUR CORNERS -> \[DottedSquare]
    'ℽ': '\uf74a', # DOUBLE-STRUCK SMALL GAMMA -> \[DoubledGamma]
    'ℼ': '\uf749', # DOUBLE-STRUCK SMALL PI -> \[DoubledPi]
    '𝕒': '\uf6e6', # MATHEMATICAL DOUBLE-STRUCK SMALL A -> \[DoubleStruckA]
    '𝕓': '\uf6e7', # MATHEMATICAL DOUBLE-STRUCK SMALL B -> \[DoubleStruckB]
    '𝔸': '\uf7a4', # MATHEMATICAL DOUBLE-STRUCK CAPITAL A -> \[DoubleStruckCapitalA]
    '𝔹': '\uf7a5', # MATHEMATICAL DOUBLE-STRUCK CAPITAL B -> \[DoubleStruckCapitalB]
    'ℂ': '\uf7a6', # DOUBLE-STRUCK CAPITAL C -> \[DoubleStruckCapitalC]
    '𝔻': '\uf7a7', # MATHEMATICAL DOUBLE-STRUCK CAPITAL D -> \[DoubleStruckCapitalD]
    '𝔼': '\uf7a8', # MATHEMATICAL DOUBLE-STRUCK CAPITAL E -> \[DoubleStruckCapitalE]
    '𝔽': '\uf7a9', # MATHEMATICAL DOUBLE-STRUCK CAPITAL F -> \[DoubleStruckCapitalF]
    '𝔾': '\uf7aa', # MATHEMATICAL DOUBLE-STRUCK CAPITAL G -> \[DoubleStruckCapitalG]
    'ℍ': '\uf7ab', # DOUBLE-STRUCK CAPITAL H -> \[DoubleStruckCapitalH]
    '𝕀': '\uf7ac', # MATHEMATICAL DOUBLE-STRUCK CAPITAL I -> \[DoubleStruckCapitalI]
    '𝕁': '\uf7ad', # MATHEMATICAL DOUBLE-STRUCK CAPITAL J -> \[DoubleStruckCapitalJ]
    '𝕂': '\uf7ae', # MATHEMATICAL DOUBLE-STRUCK CAPITAL K -> \[DoubleStruckCapitalK]
    '𝕃': '\uf7af', # MATHEMATICAL DOUBLE-STRUCK CAPITAL L -> \[DoubleStruckCapitalL]
    '𝕄': '\uf7b0', # MATHEMATICAL DOUBLE-STRUCK CAPITAL M -> \[DoubleStruckCapitalM]
    'ℕ': '\uf7b1', # DOUBLE-STRUCK CAPITAL N -> \[DoubleStruckCapitalN]
    '𝕆': '\uf7b2', # MATHEMATICAL DOUBLE-STRUCK CAPITAL O -> \[DoubleStruckCapitalO]
    'ℙ': '\uf7b3', # DOUBLE-STRUCK CAPITAL P -> \[DoubleStruckCapitalP]
    'ℚ': '\uf7b4', # DOUBLE-STRUCK CAPITAL Q -> \[DoubleStruckCapitalQ]
    'ℝ': '\uf7b5', # DOUBLE-STRUCK CAPITAL R -> \[DoubleStruckCapitalR]
    '𝕊': '\uf7b6', # MATHEMATICAL DOUBLE-STRUCK CAPITAL S -> \[DoubleStruckCapitalS]
    '𝕋': '\uf7b7', # MATHEMATICAL DOUBLE-STRUCK CAPITAL T -> \[DoubleStruckCapitalT]
    '𝕌': '\uf7b8', # MATHEMATICAL DOUBLE-STRUCK CAPITAL U -> \[DoubleStruckCapitalU]
    '𝕍': '\uf7b9', # MATHEMATICAL DOUBLE-STRUCK CAPITAL V -> \[DoubleStruckCapitalV]
    '𝕎': '\uf7ba', # MATHEMATICAL DOUBLE-STRUCK CAPITAL W -> \[DoubleStruckCapitalW]
    '𝕏': '\uf7bb', # MATHEMATICAL DOUBLE-STRUCK CAPITAL X -> \[DoubleStruckCapitalX]
    '𝕐': '\uf7bc', # MATHEMATICAL DOUBLE-STRUCK CAPITAL Y -> \[DoubleStruckCapitalY]
    'ℤ': '\uf7bd', # DOUBLE-STRUCK CAPITAL Z -> \[DoubleStruckCapitalZ]
    '𝕕': '\uf6e9', # MATHEMATICAL DOUBLE-STRUCK SMALL D -> \[DoubleStruckD]
    '𝕖': '\uf6ea', # MATHEMATICAL DOUBLE-STRUCK SMALL E -> \[DoubleStruckE]
    '𝟠': '\uf7e3', # MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT -> \[DoubleStruckEight]
    '𝕗': '\uf6eb', # MATHEMATICAL DOUBLE-STRUCK SMALL F -> \[DoubleStruckF]
    '𝟝': '\uf7e0', # MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE -> \[DoubleStruckFive]
    '𝟜': '\uf7df', # MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR -> \[DoubleStruckFour]
    '𝕘': '\uf6ec', # MATHEMATICAL DOUBLE-STRUCK SMALL G -> \[DoubleStruckG]
    '𝕙': '\uf6ed', # MATHEMATICAL DOUBLE-STRUCK SMALL H -> \[DoubleStruckH]
    '𝕚': '\uf6ee', # MATHEMATICAL DOUBLE-STRUCK SMALL I -> \[DoubleStruckI]
    '𝕛': '\uf6ef', # MATHEMATICAL DOUBLE-STRUCK SMALL J -> \[DoubleStruckJ]
    '𝕜': '\uf6f0', # MATHEMATICAL DOUBLE-STRUCK SMALL K -> \[DoubleStruckK]
    '𝕝': '\uf6f1', # MATHEMATICAL DOUBLE-STRUCK SMALL L -> \[DoubleStruckL]
    '𝕞': '\uf6f2', # MATHEMATICAL DOUBLE-STRUCK SMALL M -> \[DoubleStruckM]
    '𝕟': '\uf6f3', # MATHEMATICAL DOUBLE-STRUCK SMALL N -> \[DoubleStruckN]
    '𝟡': '\uf7e4', # MATHEMATICAL DOUBLE-STRUCK DIGIT NINE -> \[DoubleStruckNine]
    '𝕠': '\uf6f4', # MATHEMATICAL DOUBLE-STRUCK SMALL O -> \[DoubleStruckO]
    '𝟙': '\uf7dc', # MATHEMATICAL DOUBLE-STRUCK DIGIT ONE -> \[DoubleStruckOne]
    '𝕡': '\uf6f5', # MATHEMATICAL DOUBLE-STRUCK SMALL P -> \[DoubleStruckP]
    '𝕢': '\uf6f6', # MATHEMATICAL DOUBLE-STRUCK SMALL Q -> \[DoubleStruckQ]
    '𝕣': '\uf6f7', # MATHEMATICAL DOUBLE-STRUCK SMALL R -> \[DoubleStruckR]
    '𝕤': '\uf6f8', # MATHEMATICAL DOUBLE-STRUCK SMALL S -> \[DoubleStruckS]
    '𝟟': '\uf7e2', # MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN -> \[DoubleStruckSeven]
    '𝟞': '\uf7e1', # MATHEMATICAL DOUBLE-STRUCK DIGIT SIX -> \[DoubleStruckSix]
    '𝕥': '\uf6f9', # MATHEMATICAL DOUBLE-STRUCK SMALL T -> \[DoubleStruckT]
    '𝟛': '\uf7de', # MATHEMATICAL DOUBLE-STRUCK DIGIT THREE -> \[DoubleStruckThree]
    '𝟚': '\uf7dd', # MATHEMATICAL DOUBLE-STRUCK DIGIT TWO -> \[DoubleStruckTwo]
    '𝕦': '\uf6fa', # MATHEMATICAL DOUBLE-STRUCK SMALL U -> \[DoubleStruckU]
    '𝕧': '\uf6fb', # MATHEMATICAL DOUBLE-STRUCK SMALL V -> \[DoubleStruckV]
    '𝕨': '\uf6fc', # MATHEMATICAL DOUBLE-STRUCK SMALL W -> \[DoubleStruckW]
    '𝕩': '\uf6fd', # MATHEMATICAL DOUBLE-STRUCK SMALL X -> \[DoubleStruckX]
    '𝕪': '\uf6fe', # MATHEMATICAL DOUBLE-STRUCK SMALL Y -> \[DoubleStruckY]
    '𝕫': '\uf6ff', # MATHEMATICAL DOUBLE-STRUCK SMALL Z -> \[DoubleStruckZ]
    '𝟘': '\uf7db', # MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO -> \[DoubleStruckZero]
    '⩵': '\uf431', # TWO CONSECUTIVE EQUALS SIGNS -> \[Equal]
    '⇔': '⧦', # LEFT RIGHT DOUBLE ARROW -> \[Equivalent]
    'ⅇ': '\uf74d', # DOUBLE-STRUCK ITALIC SMALL E -> \[ExponentialE]
    '•': '\uf750', # BULLET -> \[FilledSmallCircle]
    'ạ': '\uf800', # LATIN SMALL LETTER A WITH DOT BELOW -> \[FormalA]
    'α̣': '\uf854', # GREEK SMALL LETTER ALPHA + COMBINING DOT BELOW -> \[FormalAlpha]
    'ḅ': '\uf801', # LATIN SMALL LETTER B WITH DOT BELOW -> \[FormalB]
    'β̣': '\uf855', # GREEK SMALL LETTER BETA + COMBINING DOT BELOW -> \[FormalBeta]
    'c̣': '\uf802', # LATIN SMALL LETTER C + COMBINING DOT BELOW -> \[FormalC]
    'Ạ': '\uf81a', # LATIN CAPITAL LETTER A WITH DOT BELOW -> \[FormalCapitalA]
    'Α̣': '\uf834', # GREEK CAPITAL LETTER ALPHA + COMBINING DOT BELOW -> \[FormalCapitalAlpha]
    'Ḅ': '\uf81b', # LATIN CAPITAL LETTER B WITH DOT BELOW -> \[FormalCapitalB]
    'Β̣': '\uf835', # GREEK CAPITAL LETTER BETA + COMBINING DOT BELOW -> \[FormalCapitalBeta]
    'C̣': '\uf81c', # LATIN CAPITAL LETTER C + COMBINING DOT BELOW -> \[FormalCapitalC]
    'Χ̣': '\uf84a', # GREEK CAPITAL LETTER CHI + COMBINING DOT BELOW -> \[FormalCapitalChi]
    'Ḍ': '\uf81d', # LATIN CAPITAL LETTER D WITH DOT BELOW -> \[FormalCapitalD]
    'Δ̣': '\uf837', # GREEK CAPITAL LETTER DELTA + COMBINING DOT BELOW -> \[FormalCapitalDelta]
    'Ϝ̣': '\uf87f', # GREEK LETTER DIGAMMA + COMBINING DOT BELOW -> \[FormalCapitalDigamma]
    'Ẹ': '\uf81e', # LATIN CAPITAL LETTER E WITH DOT BELOW -> \[FormalCapitalE]
    'Ε̣': '\uf838', # GREEK CAPITAL LETTER EPSILON + COMBINING DOT BELOW -> \[FormalCapitalEpsilon]
    'Η̣': '\uf83a', # GREEK CAPITAL LETTER ETA + COMBINING DOT BELOW -> \[FormalCapitalEta]
    'F̣': '\uf81f', # LATIN CAPITAL LETTER F + COMBINING DOT BELOW -> \[FormalCapitalF]
    'G̣': '\uf820', # LATIN CAPITAL LETTER G + COMBINING DOT BELOW -> \[FormalCapitalG]
    'Γ': '\uf836', # GREEK CAPITAL LETTER GAMMA -> \[FormalCapitalGamma]
    'Ḥ': '\uf821', # LATIN CAPITAL LETTER H WITH DOT BELOW -> \[FormalCapitalH]
    'Ị': '\uf822', # LATIN CAPITAL LETTER I WITH DOT BELOW -> \[FormalCapitalI]
    'Ι̣': '\uf85c', # GREEK CAPITAL LETTER IOTA + COMBINING DOT BELOW -> \[FormalIota]
    'J̣': '\uf823', # LATIN CAPITAL LETTER J + COMBINING DOT BELOW -> \[FormalCapitalJ]
    'Ḳ': '\uf824', # LATIN CAPITAL LETTER K WITH DOT BELOW -> \[FormalCapitalK]
    'Κ̣': '\uf83d', # GREEK CAPITAL LETTER KAPPA + COMBINING DOT BELOW -> \[FormalCapitalKappa]
    'Ϟ̣': '\uf881', # GREEK LETTER KOPPA + COMBINING DOT BELOW -> \[FormalCapitalKoppa]
    'Ḷ': '\uf825', # LATIN CAPITAL LETTER L WITH DOT BELOW -> \[FormalCapitalL]
    'Λ̣': '\uf83e', # GREEK CAPITAL LETTER LAMDA + COMBINING DOT BELOW -> \[FormalCapitalLambda]
    'Ṃ': '\uf826', # LATIN CAPITAL LETTER M WITH DOT BELOW -> \[FormalCapitalM]
    'Μ̣': '\uf83f', # GREEK CAPITAL LETTER MU + COMBINING DOT BELOW -> \[FormalCapitalMu]
    'Ṇ': '\uf827', # LATIN CAPITAL LETTER N WITH DOT BELOW -> \[FormalCapitalN]
    'Ν̣': '\uf840', # GREEK CAPITAL LETTER NU + COMBINING DOT BELOW -> \[FormalCapitalNu]
    'Ọ': '\uf828', # LATIN CAPITAL LETTER O WITH DOT BELOW -> \[FormalCapitalO]
    'Ω̣': '\uf84c', # GREEK CAPITAL LETTER OMEGA + COMBINING DOT BELOW -> \[FormalCapitalOmega]
    'Ο̣': '\uf842', # GREEK CAPITAL LETTER OMICRON + COMBINING DOT BELOW -> \[FormalCapitalOmicron]
    'P̣': '\uf829', # LATIN CAPITAL LETTER P + COMBINING DOT BELOW -> \[FormalCapitalP]
    'Φ̣': '\uf849', # GREEK CAPITAL LETTER PHI + COMBINING DOT BELOW -> \[FormalCapitalPhi]
    'Π̣': '\uf843', # GREEK CAPITAL LETTER PI + COMBINING DOT BELOW -> \[FormalCapitalPi]
    'Ψ̣': '\uf84b', # GREEK CAPITAL LETTER PSI + COMBINING DOT BELOW -> \[FormalCapitalPsi]
    'Q̣': '\uf82a', # LATIN CAPITAL LETTER Q + COMBINING DOT BELOW -> \[FormalCapitalQ]
    'Ṛ': '\uf82b', # LATIN CAPITAL LETTER R WITH DOT BELOW -> \[FormalCapitalR]
    'Ρ̣': '\uf844', # GREEK CAPITAL LETTER RHO + COMBINING DOT BELOW -> \[FormalCapitalRho]
    'Ṣ': '\uf82c', # LATIN CAPITAL LETTER S WITH DOT BELOW -> \[FormalCapitalS]
    'Ϡ̣': '\uf883', # GREEK LETTER SAMPI + COMBINING DOT BELOW -> \[FormalCapitalSampi]
    'Σ̣': '\uf846', # GREEK CAPITAL LETTER SIGMA + COMBINING DOT BELOW -> \[FormalCapitalSigma]
    'Ϛ̣': '\uf87d', # GREEK LETTER STIGMA + COMBINING DOT BELOW -> \[FormalCapitalStigma]
    'Ṭ': '\uf82d', # LATIN CAPITAL LETTER T WITH DOT BELOW -> \[FormalCapitalT]
    'Τ̣': '\uf847', # GREEK CAPITAL LETTER TAU + COMBINING DOT BELOW -> \[FormalCapitalTau]
    'Θ': '\uf83b', # GREEK CAPITAL LETTER THETA -> \[FormalCapitalTheta]
    'Ụ': '\uf82e', # LATIN CAPITAL LETTER U WITH DOT BELOW -> \[FormalCapitalU]
    'Υ̣': '\uf848', # GREEK CAPITAL LETTER UPSILON + COMBINING DOT BELOW -> \[FormalCapitalUpsilon]
    'Ṿ': '\uf82f', # LATIN CAPITAL LETTER V WITH DOT BELOW -> \[FormalCapitalV]
    'Ẉ': '\uf830', # LATIN CAPITAL LETTER W WITH DOT BELOW -> \[FormalCapitalW]
    'X̣': '\uf831', # LATIN CAPITAL LETTER X + COMBINING DOT BELOW -> \[FormalCapitalX]
    'Ξ̣': '\uf841', # GREEK CAPITAL LETTER XI + COMBINING DOT BELOW -> \[FormalCapitalXi]
    'Ỵ': '\uf832', # LATIN CAPITAL LETTER Y WITH DOT BELOW -> \[FormalCapitalY]
    'Ẓ': '\uf833', # LATIN CAPITAL LETTER Z WITH DOT BELOW -> \[FormalCapitalZ]
    'Ζ̣': '\uf839', # GREEK CAPITAL LETTER ZETA + COMBINING DOT BELOW -> \[FormalCapitalZeta]
    'χ̣': '\uf86a', # GREEK SMALL LETTER CHI + COMBINING DOT BELOW -> \[FormalChi]
    'ϒ̣': '\uf875', # GREEK UPSILON WITH HOOK SYMBOL + COMBINING DOT BELOW -> \[FormalCurlyCapitalUpsilon]
    'ε̣': '\uf858', # GREEK SMALL LETTER EPSILON + COMBINING DOT BELOW -> \[FormalCurlyEpsilon]
    'ϰ̣': '\uf885', # GREEK KAPPA SYMBOL + COMBINING DOT BELOW -> \[FormalCurlyKappa]
    'φ̣': '\uf869', # GREEK SMALL LETTER PHI + COMBINING DOT BELOW -> \[FormalCurlyPhi]
    'ϖ̣': '\uf879', # GREEK PI SYMBOL + COMBINING DOT BELOW -> \[FormalCurlyPi]
    'ϱ̣': '\uf886', # GREEK RHO SYMBOL + COMBINING DOT BELOW -> \[FormalCurlyRho]
    'ϑ̣': '\uf874', # GREEK THETA SYMBOL + COMBINING DOT BELOW -> \[FormalCurlyTheta]
    'ḍ': '\uf803', # LATIN SMALL LETTER D WITH DOT BELOW -> \[FormalD]
    'δ̣': '\uf857', # GREEK SMALL LETTER DELTA + COMBINING DOT BELOW -> \[FormalDelta]
    'ϝ': '\uf880', # GREEK SMALL LETTER DIGAMMA -> \[FormalDigamma]
    'ẹ': '\uf804', # LATIN SMALL LETTER E WITH DOT BELOW -> \[FormalE]
    'ϵ̣': '\uf88a', # GREEK LUNATE EPSILON SYMBOL + COMBINING DOT BELOW -> \[FormalEpsilon]
    'η̣': '\uf85a', # GREEK SMALL LETTER ETA + COMBINING DOT BELOW -> \[FormalEta]
    'f̣': '\uf805', # LATIN SMALL LETTER F + COMBINING DOT BELOW -> \[FormalF]
    'ς̣': '\uf865', # GREEK SMALL LETTER FINAL SIGMA + COMBINING DOT BELOW -> \[FormalFinalSigma]
    'g̣': '\uf806', # LATIN SMALL LETTER G + COMBINING DOT BELOW -> \[FormalG]
    'γ̣': '\uf856', # GREEK SMALL LETTER GAMMA + COMBINING DOT BELOW -> \[FormalGamma]
    'ḥ': '\uf807', # LATIN SMALL LETTER H WITH DOT BELOW -> \[FormalH]
    'ị': '\uf808', # LATIN SMALL LETTER I WITH DOT BELOW -> \[FormalI]
    'j̣': '\uf809', # LATIN SMALL LETTER J + COMBINING DOT BELOW -> \[FormalJ]
    'ḳ': '\uf80a', # LATIN SMALL LETTER K WITH DOT BELOW -> \[FormalK]
    'κ̣': '\uf85d', # GREEK SMALL LETTER KAPPA + COMBINING DOT BELOW -> \[FormalKappa]
    'ϟ̣': '\uf882', # GREEK SMALL LETTER KOPPA + COMBINING DOT BELOW -> \[FormalKoppa]
    'ḷ': '\uf80b', # LATIN SMALL LETTER L WITH DOT BELOW -> \[FormalL]
    'λ̣': '\uf85e', # GREEK SMALL LETTER LAMDA + COMBINING DOT BELOW -> \[FormalLambda]
    'ṃ': '\uf80c', # LATIN SMALL LETTER M + COMBINING DOT BELOW -> \[FormalM]
    'μ̣': '\uf85f', # GREEK SMALL LETTER MU + COMBINING DOT BELOW -> \[FormalMu]
    'ṇ': '\uf80d', # LATIN SMALL LETTER N + COMBINING DOT BELOW -> \[FormalN]
    'ν̣': '\uf860', # GREEK SMALL LETTER NU + COMBINING DOT BELOW -> \[FormalNu]
    'ọ': '\uf80e', # LATIN SMALL LETTER O + COMBINING DOT BELOW -> \[FormalO]
    'ω̣': '\uf86c', # GREEK SMALL LETTER OMEGA + COMBINING DOT BELOW -> \[FormalOmega]
    'ο̣': '\uf862', # GREEK SMALL LETTER OMICRON + COMBINING DOT BELOW -> \[FormalOmicron]
    'p̣': '\uf80f', # LATIN SMALL LETTER P + COMBINING DOT BELOW -> \[FormalP]
    'ϕ̣': '\uf878', # GREEK PHI SYMBOL + COMBINING DOT BELOW -> \[FormalPhi]
    'π̣': '\uf863', # GREEK SMALL LETTER PI + COMBINING DOT BELOW -> \[FormalPi]
    'ψ̣': '\uf86b', # GREEK SMALL LETTER PSI + COMBINING DOT BELOW -> \[FormalPsi]
    'q̣': '\uf810', # LATIN SMALL LETTER Q + COMBINING DOT BELOW -> \[FormalQ]
    'ṛ': '\uf811', # LATIN SMALL LETTER R WITH DOT BELOW -> \[FormalR]
    'ρ̣': '\uf864', # GREEK SMALL LETTER RHO + COMBINING DOT BELOW -> \[FormalRho]
    'ṣ': '\uf812', # LATIN SMALL LETTER S WITH DOT BELOW -> \[FormalS]
    # This one is changed because GREEK LETTER SAMPI is mapped to \[CapitalSampi] already
    'ϡ̣': '\uf884', # GREEK SMALL LETTER SAMPI + COMBINING DOT BELOW -> \[FormalSampi]
    'σ̣': '\uf866', # GREEK SMALL LETTER SIGMA + COMBINING DOT BELOW -> \[FormalSigma]
    'ϛ': '\uf87e', # GREEK SMALL LETTER STIGMA -> \[FormalStigma]
    'ṭ': '\uf813', # LATIN SMALL LETTER T WITH DOT BELOW -> \[FormalT]
    'τ̣': '\uf867', # GREEK SMALL LETTER TAU + COMBINING DOT BELOW -> \[FormalTau]
    'θ̣': '\uf85b', # GREEK SMALL LETTER THETA + COMBINING DOT BELOW -> \[FormalTheta]
    'ụ': '\uf814', # LATIN SMALL LETTER U WITH DOT BELOW -> \[FormalU]
    'υ̣': '\uf868', # GREEK SMALL LETTER UPSILON + COMBINING DOT BELOW -> \[FormalUpsilon]
    'ṿ': '\uf815', # LATIN SMALL LETTER V WITH DOT BELOW -> \[FormalV]
    'ẉ': '\uf816', # LATIN SMALL LETTER W WITH DOT BELOW -> \[FormalW]
    'x̣': '\uf817', # LATIN SMALL LETTER X + COMBINING DOT BELOW -> \[FormalX]
    'ξ̣': '\uf861', # GREEK SMALL LETTER XI + COMBINING DOT BELOW -> \[FormalXi]
    'ỵ': '\uf818', # LATIN SMALL LETTER Y WITH DOT BELOW -> \[FormalY]
    'ẓ': '\uf819', # LATIN SMALL LETTER Z WITH DOT BELOW -> \[FormalZ]
    'ζ̣': '\uf859', # GREEK SMALL LETTER ZETA + COMBINING DOT BELOW -> \[FormalZeta]
    '↦': '\uf4a1', # RIGHTWARDS ARROW FROM BAR -> \[Function]
    '𝔞': '\uf6cc', # MATHEMATICAL FRAKTUR SMALL A -> \[GothicA]
    '𝔟': '\uf6cd', # MATHEMATICAL FRAKTUR SMALL B -> \[GothicB]
    '𝔠': '\uf6ce', # MATHEMATICAL FRAKTUR SMALL C -> \[GothicC]
    '𝔄': '\uf78a', # MATHEMATICAL FRAKTUR CAPITAL A -> \[GothicCapitalA]
    '𝔅': '\uf78b', # MATHEMATICAL FRAKTUR CAPITAL B -> \[GothicCapitalB]
    '𝔇': '\uf78d', # MATHEMATICAL FRAKTUR CAPITAL D -> \[GothicCapitalD]
    '𝔈': '\uf78e', # MATHEMATICAL FRAKTUR CAPITAL E -> \[GothicCapitalE]
    '𝔉': '\uf78f', # MATHEMATICAL FRAKTUR CAPITAL F -> \[GothicCapitalF]
    '𝔊': '\uf790', # MATHEMATICAL FRAKTUR CAPITAL G -> \[GothicCapitalG]
    '𝔍': '\uf793', # MATHEMATICAL FRAKTUR CAPITAL J -> \[GothicCapitalJ]
    '𝔎': '\uf794', # MATHEMATICAL FRAKTUR CAPITAL K -> \[GothicCapitalK]
    '𝔏': '\uf795', # MATHEMATICAL FRAKTUR CAPITAL L -> \[GothicCapitalL]
    '𝔐': '\uf796', # MATHEMATICAL FRAKTUR CAPITAL M -> \[GothicCapitalM]
    '𝔑': '\uf797', # MATHEMATICAL FRAKTUR CAPITAL N -> \[GothicCapitalN]
    '𝔒': '\uf798', # MATHEMATICAL FRAKTUR CAPITAL O -> \[GothicCapitalO]
    '𝔓': '\uf799', # MATHEMATICAL FRAKTUR CAPITAL P -> \[GothicCapitalP]
    '𝔔': '\uf79a', # MATHEMATICAL FRAKTUR CAPITAL Q -> \[GothicCapitalQ]
    '𝔖': '\uf79c', # MATHEMATICAL FRAKTUR CAPITAL S -> \[GothicCapitalS]
    '𝔗': '\uf79d', # MATHEMATICAL FRAKTUR CAPITAL T -> \[GothicCapitalT]
    '𝔘': '\uf79e', # MATHEMATICAL FRAKTUR CAPITAL U -> \[GothicCapitalU]
    '𝔙': '\uf79f', # MATHEMATICAL FRAKTUR CAPITAL V -> \[GothicCapitalV]
    '𝔚': '\uf7a0', # MATHEMATICAL FRAKTUR CAPITAL W -> \[GothicCapitalW]
    '𝔛': '\uf7a1', # MATHEMATICAL FRAKTUR CAPITAL X -> \[GothicCapitalX]
    '𝔜': '\uf7a2', # MATHEMATICAL FRAKTUR CAPITAL Y -> \[GothicCapitalY]
    '𝔡': '\uf6cf', # MATHEMATICAL FRAKTUR SMALL D -> \[GothicD]
    '𝔢': '\uf6d0', # MATHEMATICAL FRAKTUR SMALL E -> \[GothicE]
    '𝔣': '\uf6d1', # MATHEMATICAL FRAKTUR SMALL F -> \[GothicF]
    '𝔤': '\uf6d2', # MATHEMATICAL FRAKTUR SMALL G -> \[GothicG]
    '𝔥': '\uf6d3', # MATHEMATICAL FRAKTUR SMALL H -> \[GothicH]
    '𝔦': '\uf6d4', # MATHEMATICAL FRAKTUR SMALL I -> \[GothicI]
    '𝔧': '\uf6d5', # MATHEMATICAL FRAKTUR SMALL J -> \[GothicJ]
    '𝔨': '\uf6d6', # MATHEMATICAL FRAKTUR SMALL K -> \[GothicK]
    '𝔩': '\uf6d7', # MATHEMATICAL FRAKTUR SMALL L -> \[GothicL]
    '𝔪': '\uf6d8', # MATHEMATICAL FRAKTUR SMALL M -> \[GothicM]
    '𝔫': '\uf6d9', # MATHEMATICAL FRAKTUR SMALL N -> \[GothicN]
    '𝔬': '\uf6da', # MATHEMATICAL FRAKTUR SMALL O -> \[GothicO]
    '𝔭': '\uf6db', # MATHEMATICAL FRAKTUR SMALL P -> \[GothicP]
    '𝔮': '\uf6dc', # MATHEMATICAL FRAKTUR SMALL Q -> \[GothicQ]
    '𝔯': '\uf6dd', # MATHEMATICAL FRAKTUR SMALL R -> \[GothicR]
    '𝔰': '\uf6de', # MATHEMATICAL FRAKTUR SMALL S -> \[GothicS]
    '𝔱': '\uf6df', # MATHEMATICAL FRAKTUR SMALL T -> \[GothicT]
    '𝔲': '\uf6e0', # MATHEMATICAL FRAKTUR SMALL U -> \[GothicU]
    '𝔳': '\uf6e1', # MATHEMATICAL FRAKTUR SMALL V -> \[GothicV]
    '𝔴': '\uf6e2', # MATHEMATICAL FRAKTUR SMALL W -> \[GothicW]
    '𝔵': '\uf6e3', # MATHEMATICAL FRAKTUR SMALL X -> \[GothicX]
    '𝔶': '\uf6e4', # MATHEMATICAL FRAKTUR SMALL Y -> \[GothicY]
    '𝔷': '\uf6e5', # MATHEMATICAL FRAKTUR SMALL Z -> \[GothicZ]
    '●': '\uf753', # BLACK CIRCLE -> \[GrayCircle]
    'ⅈ': '\uf74e', # DOUBLE-STRUCK ITALIC SMALL I -> \[ImaginaryI]
    'ⅉ': '\uf74f', # DOUBLE-STRUCK ITALIC SMALL J -> \[ImaginaryJ]
    '⟹': '\uf523', # LONG RIGHTWARDS DOUBLE ARROW -> \[Implies]
    '|': '\uf3d0', # VERTICAL LINE -> 
    '«': '\uf761', # LEFT-POINTING DOUBLE ANGLE QUOTATION MARK -> \[LeftSkeleton]
    # The following is ommited so that `a := b` or `a = b` don't get converted to `a :\uf7d9 b` or `a \uf7d9 b`
    # '=': '\uf7d9', # EQUALS SIGN -> \[LongEqual]
    '#': '\uf724', # NUMBER SIGN -> \[NumberSign]
    '⊙': '\uf3de', # CIRCLED DOT OPERATOR -> \[PermutationProduct]
    '⎕': '\uf528', # APL FUNCTIONAL SYMBOL QUAD -> \[Placeholder]
    '»': '\uf762', # RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK -> \[RightSkeleton]
    '⧴': '\uf51f', # RULE-DELAYED -> \[RuleDelayed]
    'ϡ': 'Ϡ', # GREEK SMALL LETTER SAMPI -> \[Sampi]
    '𝒶': '\uf6b2', # MATHEMATICAL SCRIPT SMALL A -> \[ScriptA]
    '𝒷': '\uf6b3', # MATHEMATICAL SCRIPT SMALL B -> \[ScriptB]
    '𝒸': '\uf6b4', # MATHEMATICAL SCRIPT SMALL C -> \[ScriptC]
    '𝒜': '\uf770', # MATHEMATICAL SCRIPT CAPITAL A -> \[ScriptCapitalA]
    '𝒞': '\uf772', # MATHEMATICAL SCRIPT CAPITAL C -> \[ScriptCapitalC]
    '𝒟': '\uf773', # MATHEMATICAL SCRIPT CAPITAL D -> \[ScriptCapitalD]
    '𝒢': '\uf776', # MATHEMATICAL SCRIPT CAPITAL G -> \[ScriptCapitalG]
    '𝒥': '\uf779', # MATHEMATICAL SCRIPT CAPITAL J -> \[ScriptCapitalJ]
    '𝒦': '\uf77a', # MATHEMATICAL SCRIPT CAPITAL K -> \[ScriptCapitalK]
    '𝒩': '\uf77d', # MATHEMATICAL SCRIPT CAPITAL N -> \[ScriptCapitalN]
    '𝒪': '\uf77e', # MATHEMATICAL SCRIPT CAPITAL O -> \[ScriptCapitalO]
    '𝒬': '\uf780', # MATHEMATICAL SCRIPT CAPITAL Q -> \[ScriptCapitalQ]
    '𝒮': '\uf782', # MATHEMATICAL SCRIPT CAPITAL S -> \[ScriptCapitalS]
    '𝒯': '\uf783', # MATHEMATICAL SCRIPT CAPITAL T -> \[ScriptCapitalT]
    '𝒰': '\uf784', # MATHEMATICAL SCRIPT CAPITAL U -> \[ScriptCapitalU]
    '𝒱': '\uf785', # MATHEMATICAL SCRIPT CAPITAL V -> \[ScriptCapitalV]
    '𝒲': '\uf786', # MATHEMATICAL SCRIPT CAPITAL W -> \[ScriptCapitalW]
    '𝒳': '\uf787', # MATHEMATICAL SCRIPT CAPITAL X -> \[ScriptCapitalX]
    '𝒴': '\uf788', # MATHEMATICAL SCRIPT CAPITAL Y -> \[ScriptCapitalY]
    '𝒵': '\uf789', # MATHEMATICAL SCRIPT CAPITAL Z -> \[ScriptCapitalZ]
    '𝒹': '\uf6b5', # MATHEMATICAL SCRIPT SMALL D -> \[ScriptD]
    '𝒾': '\uf6ba', # MATHEMATICAL SCRIPT SMALL I -> \[ScriptI]
    '𝒿': '\uf6bb', # MATHEMATICAL SCRIPT SMALL J -> \[ScriptJ]
    '𝒻': '\uf6b7', # MATHEMATICAL SCRIPT SMALL F -> \[ScriptF]
    '𝒽': '\uf6b9', # MATHEMATICAL SCRIPT SMALL H -> \[ScriptH]
    '𝓀': '\uf6bc', # MATHEMATICAL SCRIPT SMALL K -> \[ScriptK]
    '𝓂': '\uf6be', # MATHEMATICAL SCRIPT SMALL M -> \[ScriptM]
    '𝓃': '\uf6bf', # MATHEMATICAL SCRIPT SMALL N -> \[ScriptN]
    '𝓅': '\uf6c1', # MATHEMATICAL SCRIPT SMALL P -> \[ScriptP]
    '𝓆': '\uf6c2', # MATHEMATICAL SCRIPT SMALL Q -> \[ScriptQ]
    '𝓇': '\uf6c3', # MATHEMATICAL SCRIPT SMALL R -> \[ScriptR]
    '𝓈': '\uf6c4', # MATHEMATICAL SCRIPT SMALL S -> \[ScriptS]
    '𝓉': '\uf6c5', # MATHEMATICAL SCRIPT SMALL T -> \[ScriptT]
    '𝓊': '\uf6c6', # MATHEMATICAL SCRIPT SMALL U -> \[ScriptU]
    '𝓋': '\uf6c7', # MATHEMATICAL SCRIPT SMALL V -> \[ScriptV]
    '𝓌': '\uf6c8', # MATHEMATICAL SCRIPT SMALL W -> \[ScriptW]
    '𝓍': '\uf6c9', # MATHEMATICAL SCRIPT SMALL X -> \[ScriptX]
    '𝓎': '\uf6ca', # MATHEMATICAL SCRIPT SMALL Y -> \[ScriptY]
    '𝓏': '\uf6cb', # MATHEMATICAL SCRIPT SMALL Z -> \[ScriptZ]
    '↓': '\uf52b', # DOWNWARDS ARROW -> \[ShortDownArrow]
    '←': '\uf526', # LEFTWARDS ARROW -> \[ShortLeftArrow]
    '↑': '\uf52a', # UPWARDS ARROW -> \[ShortUpArrow]
    '▫': '\uf520', # WHITE SMALL SQUARE -> \[Square]
    '⊗': '\uf3da', # CIRCLED TIMES -> \[TensorProduct]
    'ᵀ': '\uf3c7', # MODIFIER LETTER CAPITAL T -> \[Transpose]
    '↔': '\uf3d4', # LEFT RIGHT ARROW -> \[UndirectedEdge]
    '⛢': '♅', # ASTRONOMICAL SYMBOL FOR URANUS -> \[Uranus]
}

WL_TO_NAMED = {
    'á': '\\[AAcute]',
    'ā': '\\[ABar]',
    'ă': '\\[ACup]',
    'ä': '\\[ADoubleDot]',
    'æ': '\\[AE]',
    'à': '\\[AGrave]',
    'â': '\\[AHat]',
    'ℵ': '\\[Aleph]',
    '\uf764': '\\[AliasDelimiter]',
    '\uf768': '\\[AliasIndicator]',
    '\uf760': '\\[AlignmentMarker]',
    'α': '\\[Alpha]',
    '\uf7d1': '\\[AltKey]',
    '∧': '\\[And]',
    '∠': '\\[Angle]',
    'Å': '\\[Angstrom]',
    '♒': '\\[AquariusSign]',
    '♈': '\\[AriesSign]',
    'å': '\\[ARing]',
    '⋰': '\\[AscendingEllipsis]',
    'ã': '\\[ATilde]',
    '\uf3a8': '\\[AutoLeftMatch]',
    '\uf3ae': '\\[AutoOperand]',
    '\uf3a4': '\\[AutoPlaceholder]',
    '\uf3a9': '\\[AutoRightMatch]',
    '\uf3ad': '\\[AutoSpace]',
    '∖': '\\[Backslash]',
    '♫': '\\[BeamedEighthNote]',
    '♬': '\\[BeamedSixteenthNote]',
    '∵': '\\[Because]',
    'ℶ': '\\[Bet]',
    'β': '\\[Beta]',
    '♝': '\\[BlackBishop]',
    '♚': '\\[BlackKing]',
    '♞': '\\[BlackKnight]',
    '♟': '\\[BlackPawn]',
    '♛': '\\[BlackQueen]',
    '♜': '\\[BlackRook]',
    '˘': '\\[Breve]',
    '•': '\\[Bullet]',
    'ć': '\\[CAcute]',
    '♋': '\\[CancerSign]',
    '⌢': '\\[Cap]',
    'Á': '\\[CapitalAAcute]',
    'Ā': '\\[CapitalABar]',
    'Ă': '\\[CapitalACup]',
    'Ä': '\\[CapitalADoubleDot]',
    'Æ': '\\[CapitalAE]',
    'À': '\\[CapitalAGrave]',
    'Â': '\\[CapitalAHat]',
    'Α': '\\[CapitalAlpha]',
    'Å': '\\[CapitalARing]',
    'Ã': '\\[CapitalATilde]',
    'Β': '\\[CapitalBeta]',
    'Ć': '\\[CapitalCAcute]',
    'Ç': '\\[CapitalCCedilla]',
    'Č': '\\[CapitalCHacek]',
    'Χ': '\\[CapitalChi]',
    'Δ': '\\[CapitalDelta]',
    'Ď': '\\[CapitalDHacek]',
    '\uf74b': '\\[CapitalDifferentialD]',
    'Ϝ': '\\[CapitalDigamma]',
    'É': '\\[CapitalEAcute]',
    'Ē': '\\[CapitalEBar]',
    'Ĕ': '\\[CapitalECup]',
    'Ë': '\\[CapitalEDoubleDot]',
    'È': '\\[CapitalEGrave]',
    'Ě': '\\[CapitalEHacek]',
    'Ê': '\\[CapitalEHat]',
    'Ε': '\\[CapitalEpsilon]',
    'Η': '\\[CapitalEta]',
    'Ð': '\\[CapitalEth]',
    'Γ': '\\[CapitalGamma]',
    'Í': '\\[CapitalIAcute]',
    'Ĭ': '\\[CapitalICup]',
    'Ï': '\\[CapitalIDoubleDot]',
    'Ì': '\\[CapitalIGrave]',
    'Î': '\\[CapitalIHat]',
    'Ι': '\\[CapitalIota]',
    'Κ': '\\[CapitalKappa]',
    'Ϟ': '\\[CapitalKoppa]',
    'Λ': '\\[CapitalLambda]',
    'Ł': '\\[CapitalLSlash]',
    'Μ': '\\[CapitalMu]',
    'Ň': '\\[CapitalNHacek]',
    'Ñ': '\\[CapitalNTilde]',
    'Ν': '\\[CapitalNu]',
    'Ó': '\\[CapitalOAcute]',
    'Ő': '\\[CapitalODoubleAcute]',
    'Ö': '\\[CapitalODoubleDot]',
    'Œ': '\\[CapitalOE]',
    'Ò': '\\[CapitalOGrave]',
    'Ô': '\\[CapitalOHat]',
    'Ω': '\\[CapitalOmega]',
    'Ο': '\\[CapitalOmicron]',
    'Ø': '\\[CapitalOSlash]',
    'Õ': '\\[CapitalOTilde]',
    'Φ': '\\[CapitalPhi]',
    'Π': '\\[CapitalPi]',
    'Ψ': '\\[CapitalPsi]',
    'Ř': '\\[CapitalRHacek]',
    'Ρ': '\\[CapitalRho]',
    'Ϡ': '\\[CapitalSampi]',
    'Š': '\\[CapitalSHacek]',
    'Σ': '\\[CapitalSigma]',
    'Ϛ': '\\[CapitalStigma]',
    'Τ': '\\[CapitalTau]',
    'Ť': '\\[CapitalTHacek]',
    'Θ': '\\[CapitalTheta]',
    'Þ': '\\[CapitalThorn]',
    'Ú': '\\[CapitalUAcute]',
    'Ű': '\\[CapitalUDoubleAcute]',
    'Ü': '\\[CapitalUDoubleDot]',
    'Ù': '\\[CapitalUGrave]',
    'Û': '\\[CapitalUHat]',
    'Υ': '\\[CapitalUpsilon]',
    'Ů': '\\[CapitalURing]',
    'Ξ': '\\[CapitalXi]',
    'Ý': '\\[CapitalYAcute]',
    'Ζ': '\\[CapitalZeta]',
    'Ž': '\\[CapitalZHacek]',
    '♑': '\\[CapricornSign]',
    'ç': '\\[CCedilla]',
    '¸': '\\[Cedilla]',
    '·': '\\[CenterDot]',
    '⋯': '\\[CenterEllipsis]',
    '¢': '\\[Cent]',
    'č': '\\[CHacek]',
    '☒': '\\[CheckedBox]',
    '✓': '\\[Checkmark]',
    'χ': '\\[Chi]',
    '⊙': '\\[CircleDot]',
    '⊖': '\\[CircleMinus]',
    '⊕': '\\[CirclePlus]',
    '⊗': '\\[CircleTimes]',
    '∲': '\\[ClockwiseContourIntegral]',
    '”': '\\[CloseCurlyDoubleQuote]',
    '’': '\\[CloseCurlyQuote]',
    '⌘': '\\[CloverLeaf]',
    '♣': '\\[ClubSuit]',
    '∶': '\\[Colon]',
    '\uf76a': '\\[CommandKey]',
    '\uf3d3': '\\[Conditioned]',
    '≡': '\\[Congruent]',
    '\uf3c8': '\\[Conjugate]',
    '\uf3c9': '\\[ConjugateTranspose]',
    '\uf7da': '\\[ConstantC]',
    '\uf3b1': '\\[Continuation]',
    '∮': '\\[ContourIntegral]',
    '\uf763': '\\[ControlKey]',
    '∐': '\\[Coproduct]',
    '©': '\\[Copyright]',
    '∳': '\\[CounterClockwiseContourIntegral]',
    '\uf4a0': '\\[Cross]',
    '≍': '\\[CupCap]',
    '⌣': '\\[Cup]',
    'ϒ': '\\[CurlyCapitalUpsilon]',
    'ε': '\\[CurlyEpsilon]',
    'ϰ': '\\[CurlyKappa]',
    'φ': '\\[CurlyPhi]',
    'ϖ': '\\[CurlyPi]',
    'ϱ': '\\[CurlyRho]',
    'ϑ': '\\[CurlyTheta]',
    '¤': '\\[Currency]',
    '†': '\\[Dagger]',
    'ℸ': '\\[Dalet]',
    '–': '\\[Dash]',
    '°': '\\[Degree]',
    '\uf7d0': '\\[DeleteKey]',
    '∇': '\\[Del]',
    'δ': '\\[Delta]',
    '⋱': '\\[DescendingEllipsis]',
    'ď': '\\[DHacek]',
    '⌀': '\\[Diameter]',
    '⋄': '\\[Diamond]',
    '♢': '\\[DiamondSuit]',
    '∆': '\\[DifferenceDelta]',
    '\uf74c': '\\[DifferentialD]',
    'ϝ': '\\[Digamma]',
    '\uf3d5': '\\[DirectedEdge]',
    '\uf4a4': '\\[DiscreteRatio]',
    '\uf4a3': '\\[DiscreteShift]',
    '\xad': '\\[DiscretionaryHyphen]',
    '\uf76e': '\\[DiscretionaryLineSeparator]',
    '\uf3bf': '\\[DiscretionaryPageBreakAbove]',
    '\uf3c6': '\\[DiscretionaryPageBreakBelow]',
    '\uf76f': '\\[DiscretionaryParagraphSeparator]',
    '\uf3d2': '\\[Distributed]',
    '∣': '\\[Divides]',
    '÷': '\\[Divide]',
    '≐': '\\[DotEqual]',
    'ı': '\\[DotlessI]',
    '\uf700': '\\[DotlessJ]',
    '\uf751': '\\[DottedSquare]',
    '∯': '\\[DoubleContourIntegral]',
    '‡': '\\[DoubleDagger]',
    '\uf74a': '\\[DoubledGamma]',
    '¨': '\\[DoubleDot]',
    '⇓': '\\[DoubleDownArrow]',
    '\uf749': '\\[DoubledPi]',
    '⇐': '\\[DoubleLeftArrow]',
    '⇔': '\\[DoubleLeftRightArrow]',
    '⫤': '\\[DoubleLeftTee]',
    '⟸': '\\[DoubleLongLeftArrow]',
    '⟺': '\\[DoubleLongLeftRightArrow]',
    '⟹': '\\[DoubleLongRightArrow]',
    '″': '\\[DoublePrime]',
    '⇒': '\\[DoubleRightArrow]',
    '⊨': '\\[DoubleRightTee]',
    '\uf6e6': '\\[DoubleStruckA]',
    '\uf6e7': '\\[DoubleStruckB]',
    '\uf6e8': '\\[DoubleStruckC]',
    '\uf7a4': '\\[DoubleStruckCapitalA]',
    '\uf7a5': '\\[DoubleStruckCapitalB]',
    '\uf7a6': '\\[DoubleStruckCapitalC]',
    '\uf7a7': '\\[DoubleStruckCapitalD]',
    '\uf7a8': '\\[DoubleStruckCapitalE]',
    '\uf7a9': '\\[DoubleStruckCapitalF]',
    '\uf7aa': '\\[DoubleStruckCapitalG]',
    '\uf7ab': '\\[DoubleStruckCapitalH]',
    '\uf7ac': '\\[DoubleStruckCapitalI]',
    '\uf7ad': '\\[DoubleStruckCapitalJ]',
    '\uf7ae': '\\[DoubleStruckCapitalK]',
    '\uf7af': '\\[DoubleStruckCapitalL]',
    '\uf7b0': '\\[DoubleStruckCapitalM]',
    '\uf7b1': '\\[DoubleStruckCapitalN]',
    '\uf7b2': '\\[DoubleStruckCapitalO]',
    '\uf7b3': '\\[DoubleStruckCapitalP]',
    '\uf7b4': '\\[DoubleStruckCapitalQ]',
    '\uf7b5': '\\[DoubleStruckCapitalR]',
    '\uf7b6': '\\[DoubleStruckCapitalS]',
    '\uf7b7': '\\[DoubleStruckCapitalT]',
    '\uf7b8': '\\[DoubleStruckCapitalU]',
    '\uf7b9': '\\[DoubleStruckCapitalV]',
    '\uf7ba': '\\[DoubleStruckCapitalW]',
    '\uf7bb': '\\[DoubleStruckCapitalX]',
    '\uf7bc': '\\[DoubleStruckCapitalY]',
    '\uf7bd': '\\[DoubleStruckCapitalZ]',
    '\uf6e9': '\\[DoubleStruckD]',
    '\uf6ea': '\\[DoubleStruckE]',
    '\uf7e3': '\\[DoubleStruckEight]',
    '\uf6eb': '\\[DoubleStruckF]',
    '\uf7e0': '\\[DoubleStruckFive]',
    '\uf7df': '\\[DoubleStruckFour]',
    '\uf6ec': '\\[DoubleStruckG]',
    '\uf6ed': '\\[DoubleStruckH]',
    '\uf6ee': '\\[DoubleStruckI]',
    '\uf6ef': '\\[DoubleStruckJ]',
    '\uf6f0': '\\[DoubleStruckK]',
    '\uf6f1': '\\[DoubleStruckL]',
    '\uf6f2': '\\[DoubleStruckM]',
    '\uf6f3': '\\[DoubleStruckN]',
    '\uf7e4': '\\[DoubleStruckNine]',
    '\uf6f4': '\\[DoubleStruckO]',
    '\uf7dc': '\\[DoubleStruckOne]',
    '\uf6f5': '\\[DoubleStruckP]',
    '\uf6f6': '\\[DoubleStruckQ]',
    '\uf6f7': '\\[DoubleStruckR]',
    '\uf6f8': '\\[DoubleStruckS]',
    '\uf7e2': '\\[DoubleStruckSeven]',
    '\uf7e1': '\\[DoubleStruckSix]',
    '\uf6f9': '\\[DoubleStruckT]',
    '\uf7de': '\\[DoubleStruckThree]',
    '\uf7dd': '\\[DoubleStruckTwo]',
    '\uf6fa': '\\[DoubleStruckU]',
    '\uf6fb': '\\[DoubleStruckV]',
    '\uf6fc': '\\[DoubleStruckW]',
    '\uf6fd': '\\[DoubleStruckX]',
    '\uf6fe': '\\[DoubleStruckY]',
    '\uf6ff': '\\[DoubleStruckZ]',
    '\uf7db': '\\[DoubleStruckZero]',
    '⇑': '\\[DoubleUpArrow]',
    '⇕': '\\[DoubleUpDownArrow]',
    '∥': '\\[DoubleVerticalBar]',
    '⤓': '\\[DownArrowBar]',
    '↓': '\\[DownArrow]',
    '⇵': '\\[DownArrowUpArrow]',
    '\uf755': '\\[DownBreve]',
    '¡': '\\[DownExclamation]',
    '⥐': '\\[DownLeftRightVector]',
    '⥞': '\\[DownLeftTeeVector]',
    '↽': '\\[DownLeftVector]',
    '⥖': '\\[DownLeftVectorBar]',
    '▾': '\\[DownPointer]',
    '¿': '\\[DownQuestion]',
    '⥟': '\\[DownRightTeeVector]',
    '⇁': '\\[DownRightVector]',
    '⥗': '\\[DownRightVectorBar]',
    '↧': '\\[DownTeeArrow]',
    '⊤': '\\[DownTee]',
    'é': '\\[EAcute]',
    '♁': '\\[Earth]',
    'ē': '\\[EBar]',
    'ĕ': '\\[ECup]',
    'ë': '\\[EDoubleDot]',
    'è': '\\[EGrave]',
    'ě': '\\[EHacek]',
    'ê': '\\[EHat]',
    '♪': '\\[EighthNote]',
    '∈': '\\[Element]',
    '…': '\\[Ellipsis]',
    '○': '\\[EmptyCircle]',
    '◇': '\\[EmptyDiamond]',
    '▽': '\\[EmptyDownTriangle]',
    '▯': '\\[EmptyRectangle]',
    '∅': '\\[EmptySet]',
    '◦': '\\[EmptySmallCircle]',
    '◻': '\\[EmptySmallSquare]',
    '□': '\\[EmptySquare]',
    '△': '\\[EmptyUpTriangle]',
    '▫': '\\[EmptyVerySmallSquare]',
    '\uf7d4': '\\[EnterKey]',
    '\uf3b9': '\\[EntityEnd]',
    '\uf3b8': '\\[EntityStart]',
    'ϵ': '\\[Epsilon]',
    '\uf431': '\\[Equal]',
    '≂': '\\[EqualTilde]',
    '⇌': '\\[Equilibrium]',
    '⧦': '\\[Equivalent]',
    '\uf767': '\\[ErrorIndicator]',
    '\uf769': '\\[EscapeKey]',
    'η': '\\[Eta]',
    'ð': '\\[Eth]',
    '€': '\\[Euro]',
    '∃': '\\[Exists]',
    '\uf74d': '\\[ExponentialE]',
    'ﬁ': '\\[FiLigature]',
    '●': '\\[FilledCircle]',
    '◆': '\\[FilledDiamond]',
    '▼': '\\[FilledDownTriangle]',
    '◀': '\\[FilledLeftTriangle]',
    '▮': '\\[FilledRectangle]',
    '▶': '\\[FilledRightTriangle]',
    '\uf750': '\\[FilledSmallCircle]',
    '◼': '\\[FilledSmallSquare]',
    '■': '\\[FilledSquare]',
    '▲': '\\[FilledUpTriangle]',
    '▪': '\\[FilledVerySmallSquare]',
    'ς': '\\[FinalSigma]',
    '\uf7fa': '\\[FirstPage]',
    '★': '\\[FivePointedStar]',
    '♭': '\\[Flat]',
    'ﬂ': '\\[FlLigature]',
    'ƒ': '\\[Florin]',
    '∀': '\\[ForAll]',
    '\uf800': '\\[FormalA]',
    '\uf854': '\\[FormalAlpha]',
    '\uf801': '\\[FormalB]',
    '\uf855': '\\[FormalBeta]',
    '\uf802': '\\[FormalC]',
    '\uf81a': '\\[FormalCapitalA]',
    '\uf834': '\\[FormalCapitalAlpha]',
    '\uf81b': '\\[FormalCapitalB]',
    '\uf835': '\\[FormalCapitalBeta]',
    '\uf81c': '\\[FormalCapitalC]',
    '\uf84a': '\\[FormalCapitalChi]',
    '\uf81d': '\\[FormalCapitalD]',
    '\uf837': '\\[FormalCapitalDelta]',
    '\uf87f': '\\[FormalCapitalDigamma]',
    '\uf81e': '\\[FormalCapitalE]',
    '\uf838': '\\[FormalCapitalEpsilon]',
    '\uf83a': '\\[FormalCapitalEta]',
    '\uf81f': '\\[FormalCapitalF]',
    '\uf820': '\\[FormalCapitalG]',
    '\uf836': '\\[FormalCapitalGamma]',
    '\uf821': '\\[FormalCapitalH]',
    '\uf822': '\\[FormalCapitalI]',
    '\uf83c': '\\[FormalCapitalIota]',
    '\uf823': '\\[FormalCapitalJ]',
    '\uf824': '\\[FormalCapitalK]',
    '\uf83d': '\\[FormalCapitalKappa]',
    '\uf881': '\\[FormalCapitalKoppa]',
    '\uf825': '\\[FormalCapitalL]',
    '\uf83e': '\\[FormalCapitalLambda]',
    '\uf826': '\\[FormalCapitalM]',
    '\uf83f': '\\[FormalCapitalMu]',
    '\uf827': '\\[FormalCapitalN]',
    '\uf840': '\\[FormalCapitalNu]',
    '\uf828': '\\[FormalCapitalO]',
    '\uf84c': '\\[FormalCapitalOmega]',
    '\uf842': '\\[FormalCapitalOmicron]',
    '\uf829': '\\[FormalCapitalP]',
    '\uf849': '\\[FormalCapitalPhi]',
    '\uf843': '\\[FormalCapitalPi]',
    '\uf84b': '\\[FormalCapitalPsi]',
    '\uf82a': '\\[FormalCapitalQ]',
    '\uf82b': '\\[FormalCapitalR]',
    '\uf844': '\\[FormalCapitalRho]',
    '\uf82c': '\\[FormalCapitalS]',
    '\uf883': '\\[FormalCapitalSampi]',
    '\uf846': '\\[FormalCapitalSigma]',
    '\uf87d': '\\[FormalCapitalStigma]',
    '\uf82d': '\\[FormalCapitalT]',
    '\uf847': '\\[FormalCapitalTau]',
    '\uf83b': '\\[FormalCapitalTheta]',
    '\uf82e': '\\[FormalCapitalU]',
    '\uf848': '\\[FormalCapitalUpsilon]',
    '\uf82f': '\\[FormalCapitalV]',
    '\uf830': '\\[FormalCapitalW]',
    '\uf831': '\\[FormalCapitalX]',
    '\uf841': '\\[FormalCapitalXi]',
    '\uf832': '\\[FormalCapitalY]',
    '\uf833': '\\[FormalCapitalZ]',
    '\uf839': '\\[FormalCapitalZeta]',
    '\uf86a': '\\[FormalChi]',
    '\uf875': '\\[FormalCurlyCapitalUpsilon]',
    '\uf858': '\\[FormalCurlyEpsilon]',
    '\uf885': '\\[FormalCurlyKappa]',
    '\uf869': '\\[FormalCurlyPhi]',
    '\uf879': '\\[FormalCurlyPi]',
    '\uf886': '\\[FormalCurlyRho]',
    '\uf874': '\\[FormalCurlyTheta]',
    '\uf803': '\\[FormalD]',
    '\uf857': '\\[FormalDelta]',
    '\uf880': '\\[FormalDigamma]',
    '\uf804': '\\[FormalE]',
    '\uf88a': '\\[FormalEpsilon]',
    '\uf85a': '\\[FormalEta]',
    '\uf805': '\\[FormalF]',
    '\uf865': '\\[FormalFinalSigma]',
    '\uf806': '\\[FormalG]',
    '\uf856': '\\[FormalGamma]',
    '\uf807': '\\[FormalH]',
    '\uf808': '\\[FormalI]',
    '\uf85c': '\\[FormalIota]',
    '\uf809': '\\[FormalJ]',
    '\uf80a': '\\[FormalK]',
    '\uf85d': '\\[FormalKappa]',
    '\uf882': '\\[FormalKoppa]',
    '\uf80b': '\\[FormalL]',
    '\uf85e': '\\[FormalLambda]',
    '\uf80c': '\\[FormalM]',
    '\uf85f': '\\[FormalMu]',
    '\uf80d': '\\[FormalN]',
    '\uf860': '\\[FormalNu]',
    '\uf80e': '\\[FormalO]',
    '\uf86c': '\\[FormalOmega]',
    '\uf862': '\\[FormalOmicron]',
    '\uf80f': '\\[FormalP]',
    '\uf878': '\\[FormalPhi]',
    '\uf863': '\\[FormalPi]',
    '\uf86b': '\\[FormalPsi]',
    '\uf810': '\\[FormalQ]',
    '\uf811': '\\[FormalR]',
    '\uf864': '\\[FormalRho]',
    '\uf812': '\\[FormalS]',
    '\uf884': '\\[FormalSampi]',
    '\uf866': '\\[FormalSigma]',
    '\uf87e': '\\[FormalStigma]',
    '\uf813': '\\[FormalT]',
    '\uf867': '\\[FormalTau]',
    '\uf85b': '\\[FormalTheta]',
    '\uf814': '\\[FormalU]',
    '\uf868': '\\[FormalUpsilon]',
    '\uf815': '\\[FormalV]',
    '\uf816': '\\[FormalW]',
    '\uf817': '\\[FormalX]',
    '\uf861': '\\[FormalXi]',
    '\uf818': '\\[FormalY]',
    '\uf819': '\\[FormalZ]',
    '\uf859': '\\[FormalZeta]',
    '\uf721': '\\[FreakedSmiley]',
    '\uf4a1': '\\[Function]',
    'γ': '\\[Gamma]',
    '♊': '\\[GeminiSign]',
    'ℷ': '\\[Gimel]',
    '\uf6cc': '\\[GothicA]',
    '\uf6cd': '\\[GothicB]',
    '\uf6ce': '\\[GothicC]',
    '\uf78a': '\\[GothicCapitalA]',
    '\uf78b': '\\[GothicCapitalB]',
    'ℭ': '\\[GothicCapitalC]',
    '\uf78d': '\\[GothicCapitalD]',
    '\uf78e': '\\[GothicCapitalE]',
    '\uf78f': '\\[GothicCapitalF]',
    '\uf790': '\\[GothicCapitalG]',
    'ℌ': '\\[GothicCapitalH]',
    'ℑ': '\\[GothicCapitalI]',
    '\uf793': '\\[GothicCapitalJ]',
    '\uf794': '\\[GothicCapitalK]',
    '\uf795': '\\[GothicCapitalL]',
    '\uf796': '\\[GothicCapitalM]',
    '\uf797': '\\[GothicCapitalN]',
    '\uf798': '\\[GothicCapitalO]',
    '\uf799': '\\[GothicCapitalP]',
    '\uf79a': '\\[GothicCapitalQ]',
    'ℜ': '\\[GothicCapitalR]',
    '\uf79c': '\\[GothicCapitalS]',
    '\uf79d': '\\[GothicCapitalT]',
    '\uf79e': '\\[GothicCapitalU]',
    '\uf79f': '\\[GothicCapitalV]',
    '\uf7a0': '\\[GothicCapitalW]',
    '\uf7a1': '\\[GothicCapitalX]',
    '\uf7a2': '\\[GothicCapitalY]',
    'ℨ': '\\[GothicCapitalZ]',
    '\uf6cf': '\\[GothicD]',
    '\uf6d0': '\\[GothicE]',
    '\uf7ed': '\\[GothicEight]',
    '\uf6d1': '\\[GothicF]',
    '\uf7ea': '\\[GothicFive]',
    '\uf7e9': '\\[GothicFour]',
    '\uf6d2': '\\[GothicG]',
    '\uf6d3': '\\[GothicH]',
    '\uf6d4': '\\[GothicI]',
    '\uf6d5': '\\[GothicJ]',
    '\uf6d6': '\\[GothicK]',
    '\uf6d7': '\\[GothicL]',
    '\uf6d8': '\\[GothicM]',
    '\uf6d9': '\\[GothicN]',
    '\uf7ef': '\\[GothicNine]',
    '\uf6da': '\\[GothicO]',
    '\uf7e6': '\\[GothicOne]',
    '\uf6db': '\\[GothicP]',
    '\uf6dc': '\\[GothicQ]',
    '\uf6dd': '\\[GothicR]',
    '\uf6de': '\\[GothicS]',
    '\uf7ec': '\\[GothicSeven]',
    '\uf7eb': '\\[GothicSix]',
    '\uf6df': '\\[GothicT]',
    '\uf7e8': '\\[GothicThree]',
    '\uf7e7': '\\[GothicTwo]',
    '\uf6e0': '\\[GothicU]',
    '\uf6e1': '\\[GothicV]',
    '\uf6e2': '\\[GothicW]',
    '\uf6e3': '\\[GothicX]',
    '\uf6e4': '\\[GothicY]',
    '\uf6e5': '\\[GothicZ]',
    '\uf7e5': '\\[GothicZero]',
    '\uf753': '\\[GrayCircle]',
    '\uf752': '\\[GraySquare]',
    '⋛': '\\[GreaterEqualLess]',
    '≥': '\\[GreaterEqual]',
    '≧': '\\[GreaterFullEqual]',
    '≫': '\\[GreaterGreater]',
    '≷': '\\[GreaterLess]',
    '⩾': '\\[GreaterSlantEqual]',
    '≳': '\\[GreaterTilde]',
    'ˇ': '\\[Hacek]',
    '☺': '\\[HappySmiley]',
    'ℏ': '\\[HBar]',
    '♡': '\\[HeartSuit]',
    '\uf3ce': '\\[HermitianConjugate]',
    '─': '\\[HorizontalLine]',
    '≎': '\\[HumpDownHump]',
    '≏': '\\[HumpEqual]',
    '‐': '\\[Hyphen]',
    'í': '\\[IAcute]',
    'ĭ': '\\[ICup]',
    'ï': '\\[IDoubleDot]',
    'ì': '\\[IGrave]',
    'î': '\\[IHat]',
    '\uf74e': '\\[ImaginaryI]',
    '\uf74f': '\\[ImaginaryJ]',
    '\uf39e': '\\[ImplicitPlus]',
    '\uf523': '\\[Implies]',
    '∞': '\\[Infinity]',
    '∫': '\\[Integral]',
    '⋂': '\\[Intersection]',
    '\uf76d': '\\[InvisibleApplication]',
    '\uf765': '\\[InvisibleComma]',
    '\uf3b4': '\\[InvisiblePostfixScriptBase]',
    '\uf3b3': '\\[InvisiblePrefixScriptBase]',
    '\uf360': '\\[InvisibleSpace]',
    '\u2062': '\\[InvisibleTimes]',
    'ι': '\\[Iota]',
    '♃': '\\[Jupiter]',
    'κ': '\\[Kappa]',
    '\uf756': '\\[KernelIcon]',
    'ϟ': '\\[Koppa]',
    'λ': '\\[Lambda]',
    '\uf7fb': '\\[LastPage]',
    '〈': '\\[LeftAngleBracket]',
    '⇤': '\\[LeftArrowBar]',
    '←': '\\[LeftArrow]',
    '⇆': '\\[LeftArrowRightArrow]',
    '\uf113': '\\[LeftAssociation]',
    '\uf603': '\\[LeftBracketingBar]',
    '⌈': '\\[LeftCeiling]',
    '〚': '\\[LeftDoubleBracket]',
    '\uf605': '\\[LeftDoubleBracketingBar]',
    '⥡': '\\[LeftDownTeeVector]',
    '⥙': '\\[LeftDownVectorBar]',
    '⇃': '\\[LeftDownVector]',
    '⌊': '\\[LeftFloor]',
    '«': '\\[LeftGuillemet]',
    '\uf76b': '\\[LeftModified]',
    '◂': '\\[LeftPointer]',
    '↔': '\\[LeftRightArrow]',
    '⥎': '\\[LeftRightVector]',
    '\uf761': '\\[LeftSkeleton]',
    '⊣': '\\[LeftTee]',
    '↤': '\\[LeftTeeArrow]',
    '⥚': '\\[LeftTeeVector]',
    '⊲': '\\[LeftTriangle]',
    '⧏': '\\[LeftTriangleBar]',
    '⊴': '\\[LeftTriangleEqual]',
    '⥑': '\\[LeftUpDownVector]',
    '⥠': '\\[LeftUpTeeVector]',
    '↿': '\\[LeftUpVector]',
    '⥘': '\\[LeftUpVectorBar]',
    '↼': '\\[LeftVector]',
    '⥒': '\\[LeftVectorBar]',
    '♌': '\\[LeoSign]',
    '≤': '\\[LessEqual]',
    '⋚': '\\[LessEqualGreater]',
    '≦': '\\[LessFullEqual]',
    '≶': '\\[LessGreater]',
    '≪': '\\[LessLess]',
    '⩽': '\\[LessSlantEqual]',
    '≲': '\\[LessTilde]',
    '\uf754': '\\[LetterSpace]',
    '♎': '\\[LibraSign]',
    '\uf723': '\\[LightBulb]',
    '—': '\\[LongDash]',
    '\uf7d9': '\\[LongEqual]',
    '⟵': '\\[LongLeftArrow]',
    '⟷': '\\[LongLeftRightArrow]',
    '⟶': '\\[LongRightArrow]',
    '↙': '\\[LowerLeftArrow]',
    '↘': '\\[LowerRightArrow]',
    'ł': '\\[LSlash]',
    '♂': '\\[Mars]',
    '\uf757': '\\[MathematicaIcon]',
    '∡': '\\[MeasuredAngle]',
    '\u205f': '\\[MediumSpace]',
    '☿': '\\[Mercury]',
    '℧': '\\[Mho]',
    'µ': '\\[Micro]',
    '∓': '\\[MinusPlus]',
    'μ': '\\[Mu]',
    '⊼': '\\[Nand]',
    '♮': '\\[Natural]',
    '\uf383': '\\[NegativeMediumSpace]',
    '\uf384': '\\[NegativeThickSpace]',
    '\uf382': '\\[NegativeThinSpace]',
    '\uf380': '\\[NegativeVeryThinSpace]',
    '♆': '\\[Neptune]',
    '⪢': '\\[NestedGreaterGreater]',
    '⪡': '\\[NestedLessLess]',
    '\uf722': '\\[NeutralSmiley]',
    'ň': '\\[NHacek]',
    '\u2060': '\\[NoBreak]',
    '\xa0': '\\[NonBreakingSpace]',
    '⊽': '\\[Nor]',
    '≢': '\\[NotCongruent]',
    '≭': '\\[NotCupCap]',
    '∦': '\\[NotDoubleVerticalBar]',
    '∉': '\\[NotElement]',
    '≠': '\\[NotEqual]',
    '\uf400': '\\[NotEqualTilde]',
    '∄': '\\[NotExists]',
    '≯': '\\[NotGreater]',
    '≱': '\\[NotGreaterEqual]',
    '≩': '\\[NotGreaterFullEqual]',
    '\uf427': '\\[NotGreaterGreater]',
    '≹': '\\[NotGreaterLess]',
    '\uf429': '\\[NotGreaterSlantEqual]',
    '≵': '\\[NotGreaterTilde]',
    '\uf402': '\\[NotHumpDownHump]',
    '\uf401': '\\[NotHumpEqual]',
    '⋪': '\\[NotLeftTriangle]',
    '\uf412': '\\[NotLeftTriangleBar]',
    '⋬': '\\[NotLeftTriangleEqual]',
    '≰': '\\[NotLessEqual]',
    '≨': '\\[NotLessFullEqual]',
    '≸': '\\[NotLessGreater]',
    '≮': '\\[NotLess]',
    '\uf422': '\\[NotLessLess]',
    '\uf424': '\\[NotLessSlantEqual]',
    '≴': '\\[NotLessTilde]',
    '¬': '\\[Not]',
    '\uf428': '\\[NotNestedGreaterGreater]',
    '\uf423': '\\[NotNestedLessLess]',
    '⊀': '\\[NotPrecedes]',
    '\uf42b': '\\[NotPrecedesEqual]',
    '⋠': '\\[NotPrecedesSlantEqual]',
    '⋨': '\\[NotPrecedesTilde]',
    '∌': '\\[NotReverseElement]',
    '⋫': '\\[NotRightTriangle]',
    '\uf413': '\\[NotRightTriangleBar]',
    '⋭': '\\[NotRightTriangleEqual]',
    '\uf42e': '\\[NotSquareSubset]',
    '⋢': '\\[NotSquareSubsetEqual]',
    '\uf42f': '\\[NotSquareSuperset]',
    '⋣': '\\[NotSquareSupersetEqual]',
    '⊄': '\\[NotSubset]',
    '⊈': '\\[NotSubsetEqual]',
    '⊁': '\\[NotSucceeds]',
    '\uf42d': '\\[NotSucceedsEqual]',
    '⋡': '\\[NotSucceedsSlantEqual]',
    '⋩': '\\[NotSucceedsTilde]',
    '⊅': '\\[NotSuperset]',
    '⊉': '\\[NotSupersetEqual]',
    '≁': '\\[NotTilde]',
    '≄': '\\[NotTildeEqual]',
    '≇': '\\[NotTildeFullEqual]',
    '≉': '\\[NotTildeTilde]',
    '\uf3d1': '\\[NotVerticalBar]',
    'ñ': '\\[NTilde]',
    'ν': '\\[Nu]',
    '\uf3a0': '\\[Null]',
    '\uf724': '\\[NumberSign]',
    'ó': '\\[OAcute]',
    'ő': '\\[ODoubleAcute]',
    'ö': '\\[ODoubleDot]',
    'œ': '\\[OE]',
    'ò': '\\[OGrave]',
    'ô': '\\[OHat]',
    'ω': '\\[Omega]',
    'ο': '\\[Omicron]',
    '“': '\\[OpenCurlyDoubleQuote]',
    '‘': '\\[OpenCurlyQuote]',
    '\uf7d2': '\\[OptionKey]',
    '∨': '\\[Or]',
    'ø': '\\[OSlash]',
    'õ': '\\[OTilde]',
    '︷': '\\[OverBrace]',
    '⎴': '\\[OverBracket]',
    '︵': '\\[OverParenthesis]',
    '¶': '\\[Paragraph]',
    '∂': '\\[PartialD]',
    '\uf3de': '\\[PermutationProduct]',
    '⟂': '\\[Perpendicular]',
    'ϕ': '\\[Phi]',
    'π': '\\[Pi]',
    '\uf361': '\\[Piecewise]',
    '♓': '\\[PiscesSign]',
    '\uf528': '\\[Placeholder]',
    '±': '\\[PlusMinus]',
    '♇': '\\[Pluto]',
    '≺': '\\[Precedes]',
    '⪯': '\\[PrecedesEqual]',
    '≼': '\\[PrecedesSlantEqual]',
    '≾': '\\[PrecedesTilde]',
    '′': '\\[Prime]',
    '∏': '\\[Product]',
    '∷': '\\[Proportion]',
    '∝': '\\[Proportional]',
    'ψ': '\\[Psi]',
    '♩': '\\[QuarterNote]',
    '®': '\\[RegisteredTrademark]',
    '↵': '\\[ReturnIndicator]',
    '\uf766': '\\[ReturnKey]',
    '‶': '\\[ReverseDoublePrime]',
    '∋': '\\[ReverseElement]',
    '⇋': '\\[ReverseEquilibrium]',
    '‵': '\\[ReversePrime]',
    '⥯': '\\[ReverseUpEquilibrium]',
    'ř': '\\[RHacek]',
    'ρ': '\\[Rho]',
    '∟': '\\[RightAngle]',
    '〉': '\\[RightAngleBracket]',
    '→': '\\[RightArrow]',
    '⇥': '\\[RightArrowBar]',
    '⇄': '\\[RightArrowLeftArrow]',
    '\uf114': '\\[RightAssociation]',
    '\uf604': '\\[RightBracketingBar]',
    '⌉': '\\[RightCeiling]',
    '〛': '\\[RightDoubleBracket]',
    '\uf606': '\\[RightDoubleBracketingBar]',
    '⥝': '\\[RightDownTeeVector]',
    '⇂': '\\[RightDownVector]',
    '⥕': '\\[RightDownVectorBar]',
    '⌋': '\\[RightFloor]',
    '»': '\\[RightGuillemet]',
    '\uf76c': '\\[RightModified]',
    '▸': '\\[RightPointer]',
    '\uf762': '\\[RightSkeleton]',
    '⊢': '\\[RightTee]',
    '↦': '\\[RightTeeArrow]',
    '⥛': '\\[RightTeeVector]',
    '⊳': '\\[RightTriangle]',
    '⧐': '\\[RightTriangleBar]',
    '⊵': '\\[RightTriangleEqual]',
    '⥏': '\\[RightUpDownVector]',
    '⥜': '\\[RightUpTeeVector]',
    '↾': '\\[RightUpVector]',
    '⥔': '\\[RightUpVectorBar]',
    '⇀': '\\[RightVector]',
    '⥓': '\\[RightVectorBar]',
    '⥰': '\\[RoundImplies]',
    '\uf3b2': '\\[RoundSpaceIndicator]',
    '\uf522': '\\[Rule]',
    '\uf51f': '\\[RuleDelayed]',
    '☹': '\\[SadSmiley]',
    '♐': '\\[SagittariusSign]',
    'Ϡ': '\\[Sampi]',
    '♄': '\\[Saturn]',
    '♏': '\\[ScorpioSign]',
    '\uf6b2': '\\[ScriptA]',
    '\uf6b3': '\\[ScriptB]',
    '\uf6b4': '\\[ScriptC]',
    '\uf770': '\\[ScriptCapitalA]',
    'ℬ': '\\[ScriptCapitalB]',
    '\uf772': '\\[ScriptCapitalC]',
    '\uf773': '\\[ScriptCapitalD]',
    'ℰ': '\\[ScriptCapitalE]',
    'ℱ': '\\[ScriptCapitalF]',
    '\uf776': '\\[ScriptCapitalG]',
    'ℋ': '\\[ScriptCapitalH]',
    'ℐ': '\\[ScriptCapitalI]',
    '\uf779': '\\[ScriptCapitalJ]',
    '\uf77a': '\\[ScriptCapitalK]',
    'ℒ': '\\[ScriptCapitalL]',
    'ℳ': '\\[ScriptCapitalM]',
    '\uf77d': '\\[ScriptCapitalN]',
    '\uf77e': '\\[ScriptCapitalO]',
    '℘': '\\[ScriptCapitalP]',
    '\uf780': '\\[ScriptCapitalQ]',
    'ℛ': '\\[ScriptCapitalR]',
    '\uf782': '\\[ScriptCapitalS]',
    '\uf783': '\\[ScriptCapitalT]',
    '\uf784': '\\[ScriptCapitalU]',
    '\uf785': '\\[ScriptCapitalV]',
    '\uf786': '\\[ScriptCapitalW]',
    '\uf787': '\\[ScriptCapitalX]',
    '\uf788': '\\[ScriptCapitalY]',
    '\uf789': '\\[ScriptCapitalZ]',
    '\uf6b5': '\\[ScriptD]',
    '\uf730': '\\[ScriptDotlessI]',
    '\uf731': '\\[ScriptDotlessJ]',
    'ℯ': '\\[ScriptE]',
    '\uf7f8': '\\[ScriptEight]',
    '\uf6b7': '\\[ScriptF]',
    '\uf7f5': '\\[ScriptFive]',
    '\uf7f4': '\\[ScriptFour]',
    'ℊ': '\\[ScriptG]',
    '\uf6b9': '\\[ScriptH]',
    '\uf6ba': '\\[ScriptI]',
    '\uf6bb': '\\[ScriptJ]',
    '\uf6bc': '\\[ScriptK]',
    'ℓ': '\\[ScriptL]',
    '\uf6be': '\\[ScriptM]',
    '\uf6bf': '\\[ScriptN]',
    '\uf7f9': '\\[ScriptNine]',
    'ℴ': '\\[ScriptO]',
    '\uf7f1': '\\[ScriptOne]',
    '\uf6c1': '\\[ScriptP]',
    '\uf6c2': '\\[ScriptQ]',
    '\uf6c3': '\\[ScriptR]',
    '\uf6c4': '\\[ScriptS]',
    '\uf7f7': '\\[ScriptSeven]',
    '\uf7f6': '\\[ScriptSix]',
    '\uf6c5': '\\[ScriptT]',
    '\uf7f3': '\\[ScriptThree]',
    '\uf7f2': '\\[ScriptTwo]',
    '\uf6c6': '\\[ScriptU]',
    '\uf6c7': '\\[ScriptV]',
    '\uf6c8': '\\[ScriptW]',
    '\uf6c9': '\\[ScriptX]',
    '\uf6ca': '\\[ScriptY]',
    '\uf6cb': '\\[ScriptZ]',
    '\uf7f0': '\\[ScriptZero]',
    '§': '\\[Section]',
    '\uf527': '\\[SelectionPlaceholder]',
    'š': '\\[SHacek]',
    '♯': '\\[Sharp]',
    '\uf52b': '\\[ShortDownArrow]',
    '\uf526': '\\[ShortLeftArrow]',
    '\uf525': '\\[ShortRightArrow]',
    '\uf52a': '\\[ShortUpArrow]',
    'σ': '\\[Sigma]',
    '✶': '\\[SixPointedStar]',
    '⁃': '\\[SkeletonIndicator]',
    '∘': '\\[SmallCircle]',
    '␣': '\\[SpaceIndicator]',
    '\uf7bf': '\\[SpaceKey]',
    '♠': '\\[SpadeSuit]',
    '\uf3bb': '\\[SpanFromAbove]',
    '\uf3bc': '\\[SpanFromBoth]',
    '\uf3ba': '\\[SpanFromLeft]',
    '∢': '\\[SphericalAngle]',
    '√': '\\[Sqrt]',
    '\uf520': '\\[Square]',
    '⊓': '\\[SquareIntersection]',
    '⊏': '\\[SquareSubset]',
    '⊑': '\\[SquareSubsetEqual]',
    '⊐': '\\[SquareSuperset]',
    '⊒': '\\[SquareSupersetEqual]',
    '⊔': '\\[SquareUnion]',
    '⋆': '\\[Star]',
    '£': '\\[Sterling]',
    'ϛ': '\\[Stigma]',
    '⊂': '\\[Subset]',
    '⊆': '\\[SubsetEqual]',
    '≻': '\\[Succeeds]',
    '⪰': '\\[SucceedsEqual]',
    '≽': '\\[SucceedsSlantEqual]',
    '≿': '\\[SucceedsTilde]',
    '∍': '\\[SuchThat]',
    '∑': '\\[Sum]',
    '⊃': '\\[Superset]',
    '⊇': '\\[SupersetEqual]',
    '\uf75f': '\\[SystemEnterKey]',
    '\uf3af': '\\[SystemsModelDelay]',
    'ß': '\\[SZ]',
    '\uf7be': '\\[TabKey]',
    'τ': '\\[Tau]',
    '♉': '\\[TaurusSign]',
    '\uf3da': '\\[TensorProduct]',
    '\uf3db': '\\[TensorWedge]',
    'ť': '\\[THacek]',
    '∴': '\\[Therefore]',
    'θ': '\\[Theta]',
    '\u2005': '\\[ThickSpace]',
    '\u2009': '\\[ThinSpace]',
    'þ': '\\[Thorn]',
    '∼': '\\[Tilde]',
    '≃': '\\[TildeEqual]',
    '≅': '\\[TildeFullEqual]',
    '≈': '\\[TildeTilde]',
    '×': '\\[Times]',
    '™': '\\[Trademark]',
    '\uf3c7': '\\[Transpose]',
    '\uf758': '\\[TripleDot]',
    'ú': '\\[UAcute]',
    'ű': '\\[UDoubleAcute]',
    'ü': '\\[UDoubleDot]',
    'ù': '\\[UGrave]',
    'û': '\\[UHat]',
    '︸': '\\[UnderBrace]',
    '⎵': '\\[UnderBracket]',
    '︶': '\\[UnderParenthesis]',
    '\uf3d4': '\\[UndirectedEdge]',
    '⋃': '\\[Union]',
    '⊎': '\\[UnionPlus]',
    '↑': '\\[UpArrow]',
    '⤒': '\\[UpArrowBar]',
    '⇅': '\\[UpArrowDownArrow]',
    '↕': '\\[UpDownArrow]',
    '⥮': '\\[UpEquilibrium]',
    '↖': '\\[UpperLeftArrow]',
    '↗': '\\[UpperRightArrow]',
    '▴': '\\[UpPointer]',
    'υ': '\\[Upsilon]',
    '⊥': '\\[UpTee]',
    '↥': '\\[UpTeeArrow]',
    '♅': '\\[Uranus]',
    'ů': '\\[URing]',
    '⋁': '\\[Vee]',
    '♀': '\\[Venus]',
    '\uf3d0': '\\[VerticalBar]',
    '⋮': '\\[VerticalEllipsis]',
    '│': '\\[VerticalLine]',
    '\uf432': '\\[VerticalSeparator]',
    '≀': '\\[VerticalTilde]',
    '\u200a': '\\[VeryThinSpace]',
    '♍': '\\[VirgoSign]',
    '\uf725': '\\[WarningSign]',
    '⌚': '\\[WatchIcon]',
    '⋀': '\\[Wedge]',
    '℘': '\\[WeierstrassP]',
    '♗': '\\[WhiteBishop]',
    '♔': '\\[WhiteKing]',
    '♘': '\\[WhiteKnight]',
    '♙': '\\[WhitePawn]',
    '♕': '\\[WhiteQueen]',
    '♖': '\\[WhiteRook]',
    '\uf720': '\\[Wolf]',
    '\uf11e': '\\[WolframLanguageLogo]',
    '\uf11f': '\\[WolframLanguageLogoCircle]',
    'ξ': '\\[Xi]',
    '\uf4a2': '\\[Xnor]',
    '⊻': '\\[Xor]',
    'ý': '\\[YAcute]',
    'ÿ': '\\[YDoubleDot]',
    '¥': '\\[Yen]',
    'ζ': '\\[Zeta]',
    'ž': '\\[ZHacek]',
}

WL_TO_UNICODE_DICT = {re.escape(k): v 
                     for k, v in {**WL_TO_NAMED, **WL_TO_UNICODE}.items()}
WL_TO_UNICODE_RE = re.compile(
    "|".join(sorted(WL_TO_UNICODE_DICT.keys(), key=lambda k: (-len(k), k)))
)

WL_TO_PLAIN_DICT = {re.escape(k): v for k, v in WL_TO_NAMED.items()}
WL_TO_PLAIN_RE = re.compile(
    "|".join(
        sorted(WL_TO_PLAIN_DICT.keys(), key=lambda k: (-len(k), k))
    )
)

UNICODE_REPLACE_DICT = {re.escape(k): v for k, v in UNICODE_TO_WL.items()}
UNICODE_REPLACE_RE = re.compile(
    "|".join(sorted(UNICODE_REPLACE_DICT.keys(), key=lambda k: (-len(k), k)))
)

def interpolate_string(text, get_param) -> str:
    index = [1]

    def get_item(index):
        if 1 <= index <= len(args):
            return args[index - 1]
        else:
            return ''

    if isinstance(get_param, list):
        args = get_param
        get_param = get_item

    def repl(match):
        arg = match.group(1)
        if arg == '' or arg == '0':
            arg = index[0]
        else:
            arg = int(arg)
        index[0] += 1
        param = get_param(arg)
        return param
    return FORMAT_RE.sub(repl, text)

"""
NOTE: Maybe see
http://www.cosc.canterbury.ac.nz/tad.takaoka/isaac.pdf
resp.
http://www.cosc.canterbury.ac.nz/tad.takaoka/perm.p
for a permutation generating algorithm for multisets.
"""


def permutations(items, without_duplicates=True):
    if not items:
        yield []
    # already_taken = set()
    # first yield identical permutation without recursion
    yield items
    for index in range(len(items)):
        item = items[index]
        # if item not in already_taken:
        for sub in permutations(items[:index] + items[index + 1:]):
            yield [item] + sub
            # already_taken.add(item)


def subsets(items, min, max, included=None, less_first=False):
    if max is None:
        max = len(items)
    lengths = list(range(min, max + 1))
    if not less_first:
        lengths = reversed(lengths)
    lengths = list(lengths)
    if lengths and lengths[0] == 0:
        lengths = lengths[1:] + [0]

    def decide(chosen, not_chosen, rest, count):
        if count < 0 or len(rest) < count:
            return
        if count == 0:
            yield chosen, list(chain(not_chosen, rest))
        elif len(rest) == count:
            if included is None or all(item in included for item in rest):
                yield list(chain(chosen, rest)), not_chosen
        elif rest:
            item = rest[0]
            if included is None or item in included:
                for set in decide(chosen + [item], not_chosen, rest[1:],
                                  count - 1):
                    yield set
            for set in decide(chosen, not_chosen + [item], rest[1:], count):
                yield set

    for length in lengths:
        for chosen, not_chosen in decide([], [], items, length):
            yield chosen, ([], not_chosen)


def subsets_2(items, min, max, without_duplicates=True):
    """ max may only be 1 or None (= infinity).
    Respects include property of items
    """

    if min <= max == 1:
        for index in range(len(items)):
            if items[index].include:
                yield [items[index]], ([], items[:index] + items[index + 1:])
        if min == 0:
            yield [], ([], items)
    else:
        counts = {}
        for item in items:
            if item.include:
                if item in counts:
                    counts[item] += 1
                else:
                    counts[item] = 1
        already = set()

        def decide(chosen, not_chosen, rest):
            if not rest:
                if len(chosen) >= min:
                    """if False and len(chosen) > 1 and (
                            permutate_until is None or
                            len(chosen) <= permutate_until):
                        for perm in permutations(chosen):
                            yield perm, ([], not_chosen)
                    else:"""
                    yield chosen, ([], not_chosen)
            else:
                if rest[0].include:
                    for set in decide(chosen + [rest[0]], not_chosen,
                                      rest[1:]):
                        yield set
                for set in decide(chosen, not_chosen + [rest[0]], rest[1:]):
                    yield set
        for subset in decide([], [], list(counts.keys())):
            t = tuple(subset[0])
            if t not in already:
                yield subset
                already.add(t)
            else:
                print('already taken')


def subranges(items, min_count, max, flexible_start=False, included=None,
              less_first=False):
    # TODO: take into account included

    if max is None:
        max = len(items)
    max = min(max, len(items))
    if flexible_start:
        starts = list(range(len(items) - max + 1))
    else:
        starts = (0,)
    for start in starts:
        lengths = list(range(min_count, max + 1))
        if not less_first:
            lengths = reversed(lengths)
        lengths = list(lengths)
        if lengths == [0, 1]:
            lengths = [1, 0]
        for length in lengths:
            yield (items[start:start + length],
                   (items[:start], items[start + length:]))


def unicode_superscript(value) -> str:
    def repl_char(c):
        if c == '1':
            value = 185
        elif c == '2':
            value = 178
        elif c == '3':
            value = 179
        elif '0' <= c <= '9':
            value = 8304 + (ord(c) - ord('0'))
        elif c == '-':
            value = 8315
        elif c == '(':
            value = 8317
        elif c == ')':
            value = 8318
        else:
            value = ord(c)
        return chr(value)
    return ''.join(repl_char(c) for c in value)


try:
    from inspect import signature

    def _python_function_arguments(f):
        return signature(f).parameters.keys()
except ImportError:  # py2, pypy
    from inspect import getargspec

    def _python_function_arguments(f):
        return getargspec(f).args

if sys.version_info >= (3, 4, 0):
    _cython_function_arguments = _python_function_arguments
elif sys.version_info[0] >= 3:  # py3.3
    def _cython_function_arguments(f):
        return f.__code__.co_varnames
else:  # py2
    def _cython_function_arguments(f):
        return f.func_code.co_varnames


def function_arguments(f):
    try:
        return _python_function_arguments(f)
    except (TypeError, ValueError):
        return _cython_function_arguments(f)

def robust_min(iterable):
    minimum = None
    for i in iterable:
        if minimum is None or i < minimum:
            minimum = i
    return minimum

def replace_wl_with_plain_text(wl_input: str, use_unicode=True) -> str:
    """WL uses some non-unicode character for various things.
    Replace them with the unicode equivalent.
    """
    r = WL_TO_UNICODE_RE if use_unicode else WL_TO_PLAIN_RE
    d = WL_TO_UNICODE_DICT if use_unicode else WL_TO_PLAIN_DICT

    return r.sub(lambda m: d[re.escape(m.group(0))], wl_input)

def replace_unicode_with_wl(unicode_input: str) -> str:
    """WL uses some non-unicode character for various things.
    Replace their unicode equivalent with them.
    """
    return UNICODE_REPLACE_RE.sub(
        lambda m: UNICODE_REPLACE_DICT[re.escape(m.group(0))], unicode_input
    )


